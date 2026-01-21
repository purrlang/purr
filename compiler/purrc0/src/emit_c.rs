use crate::ast::*;
use crate::error::Error;
use crate::span::Span;
use std::collections::{BTreeSet, HashMap};

pub fn emit_c(program: &Program) -> Result<String, Error> {
    let mut out = String::new();
    out.push_str("#include \"purr_runtime.h\"\n");
    out.push_str("#include <stdint.h>\n\n");

    let mut string_lits = Vec::new();
    collect_strings(program, &mut string_lits);
    let mut unique = BTreeSet::new();
    for s in &string_lits {
        unique.insert(s.clone());
    }
    let mut lit_map = HashMap::new();
    for (idx, s) in unique.iter().enumerate() {
        let escaped = escape_c_string(s);
        let len = s.as_bytes().len();
        out.push_str(&format!("static const char _purr_str_{idx}[] = \"{escaped}\";\n"));
        out.push_str(&format!("static const purr_string _purr_strval_{idx} = {{ _purr_str_{idx}, {len} }};\n\n"));
        lit_map.insert(s.clone(), idx);
    }

    for decl in &program.declarations {
        match decl {
            Declaration::Struct(s) => emit_struct(&mut out, s),
            Declaration::Enum(e) => emit_enum(&mut out, e)?,
            Declaration::Func(f) => {
                emit_func_sig(&mut out, f, &lit_map);
                out.push_str(";\n");
            }
        }
    }

    out.push_str("\n");

    for decl in &program.declarations {
        if let Declaration::Func(f) = decl {
            emit_func_body(&mut out, f, &lit_map)?;
            out.push_str("\n");
        }
    }

    Ok(out)
}

fn emit_struct(out: &mut String, decl: &StructDecl) {
    out.push_str(&format!("typedef struct {} {{\n", decl.name));
    for field in &decl.fields {
        out.push_str("    ");
        out.push_str(&format!("{} {};", type_to_c(&field.ty), field.name));
        out.push_str("\n");
    }
    out.push_str(&format!("}} {};\n\n", decl.name));
}

fn emit_enum(out: &mut String, decl: &EnumDecl) -> Result<(), Error> {
    for case in &decl.cases {
        if !case.fields.is_empty() {
            return Err(Error::new("enum payloads not supported in purrc0 emitter", Some(case.span)));
        }
    }
    out.push_str(&format!("typedef enum {} {{\n", decl.name));
    for case in &decl.cases {
        out.push_str(&format!("    {}_{},\n", decl.name, case.name));
    }
    out.push_str(&format!("}} {};\n\n", decl.name));
    Ok(())
}

fn emit_func_sig(out: &mut String, decl: &FuncDecl, _lit_map: &HashMap<String, usize>) {
    let (name, _) = func_name(decl);
    let ret = decl.ret.as_ref().map(type_to_c).unwrap_or_else(|| "void".to_string());
    out.push_str(&format!("{} {}(", ret, name));
    for (i, p) in decl.params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("{} {}", type_to_c(&p.ty), p.name));
    }
    out.push_str(")");
}

fn emit_func_body(out: &mut String, decl: &FuncDecl, lit_map: &HashMap<String, usize>) -> Result<(), Error> {
    emit_func_sig(out, decl, lit_map);
    out.push_str(" {\n");
    for stmt in &decl.body.statements {
        emit_stmt(out, stmt, lit_map)?;
    }
    out.push_str("}\n");
    Ok(())
}

fn emit_stmt(out: &mut String, stmt: &Stmt, lit_map: &HashMap<String, usize>) -> Result<(), Error> {
    match stmt {
        Stmt::Var(v) => {
            let cty = v.ty.as_ref().map(type_to_c).unwrap_or_else(|| "auto".to_string());
            out.push_str(&format!("    {} {} = ", cty, v.name));
            emit_expr(out, &v.init, lit_map)?;
            out.push_str(";\n");
        }
        Stmt::Assign(a) => {
            out.push_str("    ");
            emit_lvalue(out, &a.target)?;
            out.push_str(" = ");
            emit_expr(out, &a.value, lit_map)?;
            out.push_str(";\n");
        }
        Stmt::Return(r) => {
            out.push_str("    return");
            if let Some(expr) = &r.value {
                out.push(' ');
                emit_expr(out, expr, lit_map)?;
            }
            out.push_str(";\n");
        }
        Stmt::Expr(e) => {
            out.push_str("    ");
            emit_expr(out, &e.expr, lit_map)?;
            out.push_str(";\n");
        }
        Stmt::Block(b) => {
            out.push_str("    {\n");
            for st in &b.statements {
                emit_stmt(out, st, lit_map)?;
            }
            out.push_str("    }\n");
        }
    }
    Ok(())
}

fn emit_lvalue(out: &mut String, lv: &LValue) -> Result<(), Error> {
    match lv {
        LValue::Name(name, _) => out.push_str(name),
        LValue::Field { base, name, .. } => {
            emit_lvalue(out, base)?;
            out.push('.');
            out.push_str(name);
        }
    }
    Ok(())
}

fn emit_expr(out: &mut String, expr: &Expr, lit_map: &HashMap<String, usize>) -> Result<(), Error> {
    match expr {
        Expr::Int(v, _) => out.push_str(v),
        Expr::Float(v, _) => out.push_str(v),
        Expr::String(v, _) => {
            let idx = lit_map.get(v).copied().unwrap_or(0);
            out.push_str(&format!("_purr_strval_{idx}"));
        }
        Expr::Bool(b, _) => out.push_str(if *b { "true" } else { "false" }),
        Expr::Nil(_) => out.push_str("((void*)0)"),
        Expr::Name(n, _) => out.push_str(n),
        Expr::StructLit { name, fields, .. } => {
            out.push_str(&format!("({}){{", name));
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!(".{} = ", f.name));
                emit_expr(out, &f.value, lit_map)?;
            }
            out.push_str("}");
        }
        Expr::Call { callee, args, .. } => {
            let qname = expr_to_qualified_name(callee);
            if let Some(name) = qname {
                if name == "core.Print" {
                    out.push_str("purr_core_print");
                } else {
                    out.push_str(&name);
                }
            } else {
                return Err(Error::new("unsupported call target", None));
            }
            out.push('(');
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_expr(out, a, lit_map)?;
            }
            out.push(')');
        }
        Expr::Field { base, name, .. } => {
            emit_expr(out, base, lit_map)?;
            out.push('.');
            out.push_str(name);
        }
        Expr::Unary { op, expr, .. } => {
            out.push_str(match op { UnaryOp::Neg => "-", UnaryOp::Not => "!" });
            emit_expr(out, expr, lit_map)?;
        }
        Expr::Binary { op, left, right, .. } => {
            emit_expr(out, left, lit_map)?;
            out.push_str(match op {
                BinaryOp::Add => " + ",
                BinaryOp::Sub => " - ",
                BinaryOp::Mul => " * ",
                BinaryOp::Div => " / ",
                BinaryOp::Eq => " == ",
                BinaryOp::Neq => " != ",
                BinaryOp::Lt => " < ",
                BinaryOp::Lte => " <= ",
                BinaryOp::Gt => " > ",
                BinaryOp::Gte => " >= ",
                BinaryOp::And => " && ",
                BinaryOp::Or => " || ",
            });
            emit_expr(out, right, lit_map)?;
        }
        Expr::Group { expr, .. } => {
            out.push('(');
            emit_expr(out, expr, lit_map)?;
            out.push(')');
        }
    }
    Ok(())
}

fn type_to_c(ty: &Type) -> String {
    match ty {
        Type::Name(name, _) => match name.as_str() {
            "bool" => "int".to_string(),
            "i32" => "int32_t".to_string(),
            "i64" => "int64_t".to_string(),
            "f64" => "double".to_string(),
            "string" => "purr_string".to_string(),
            "bytes" => "purr_string".to_string(),
            "void" => "void".to_string(),
            _ => name.clone(),
        },
        Type::Ctor { name, .. } => name.clone(),
        Type::Optional { .. } => "void*".to_string(),
        Type::Func { .. } => "void*".to_string(),
    }
}

fn func_name(decl: &FuncDecl) -> (String, Span) {
    match &decl.name {
        FunctionName::Free(n) => (n.clone(), decl.span),
        FunctionName::Method { type_name, method } => (format!("{}_{}", type_name, method), decl.span),
    }
}

fn collect_strings(program: &Program, out: &mut Vec<String>) {
    for decl in &program.declarations {
        if let Declaration::Func(f) = decl {
            for stmt in &f.body.statements {
                collect_strings_stmt(stmt, out);
            }
        }
    }
}

fn collect_strings_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Var(v) => collect_strings_expr(&v.init, out),
        Stmt::Assign(a) => collect_strings_expr(&a.value, out),
        Stmt::Return(r) => {
            if let Some(e) = &r.value {
                collect_strings_expr(e, out)
            }
        }
        Stmt::Expr(e) => collect_strings_expr(&e.expr, out),
        Stmt::Block(b) => {
            for st in &b.statements {
                collect_strings_stmt(st, out)
            }
        }
    }
}

fn collect_strings_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::String(s, _) => out.push(s.clone()),
        Expr::StructLit { fields, .. } => {
            for f in fields {
                collect_strings_expr(&f.value, out);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                collect_strings_expr(a, out);
            }
        }
        Expr::Field { base, .. } => collect_strings_expr(base, out),
        Expr::Unary { expr, .. } => collect_strings_expr(expr, out),
        Expr::Binary { left, right, .. } => {
            collect_strings_expr(left, out);
            collect_strings_expr(right, out);
        }
        Expr::Group { expr, .. } => collect_strings_expr(expr, out),
        Expr::Int(_, _) | Expr::Float(_, _) | Expr::Bool(_, _) | Expr::Nil(_) | Expr::Name(_, _) => {}
    }
}

fn expr_to_qualified_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Name(n, _) => Some(n.clone()),
        Expr::Field { base, name, .. } => match &**base {
            Expr::Name(t, _) => Some(format!("{t}.{name}")),
            _ => None,
        },
        _ => None,
    }
}

fn escape_c_string(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}
