use crate::ast::*;
use crate::error::Error;
use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Bool,
    I32,
    I64,
    F64,
    String,
    Bytes,
    Void,
    Optional(Box<Ty>),
    Ctor(String, Vec<Ty>),
    Func(Vec<Ty>, Box<Ty>),
    Struct(String),
    Enum(String),
}

pub fn typecheck(program: &Program) -> Result<(), Error> {
    let mut tc = TypeChecker::new();
    tc.collect_decls(program)?;
    tc.check_program(program)
}

struct TypeChecker {
    structs: HashMap<String, StructDecl>,
    enums: HashMap<String, EnumDecl>,
    funcs: HashMap<String, FuncDecl>,
}

impl TypeChecker {
    fn new() -> Self {
        Self { structs: HashMap::new(), enums: HashMap::new(), funcs: HashMap::new() }
    }

    fn collect_decls(&mut self, program: &Program) -> Result<(), Error> {
        for decl in &program.declarations {
            match decl {
                Declaration::Struct(s) => {
                    if self.structs.contains_key(&s.name) {
                        return Err(Error::new("duplicate struct", Some(s.span)));
                    }
                    self.structs.insert(s.name.clone(), s.clone());
                }
                Declaration::Enum(e) => {
                    if self.enums.contains_key(&e.name) {
                        return Err(Error::new("duplicate enum", Some(e.span)));
                    }
                    self.enums.insert(e.name.clone(), e.clone());
                }
                Declaration::Func(f) => {
                    let name = self.func_name_key(&f.name);
                    if self.funcs.contains_key(&name) {
                        return Err(Error::new("duplicate function", Some(f.span)));
                    }
                    self.funcs.insert(name, f.clone());
                }
            }
        }
        Ok(())
    }

    fn check_program(&self, program: &Program) -> Result<(), Error> {
        for decl in &program.declarations {
            if let Declaration::Func(f) = decl {
                self.check_func(f)?;
            }
        }
        Ok(())
    }

    fn check_func(&self, func: &FuncDecl) -> Result<(), Error> {
        let mut env = Env::new();
        for param in &func.params {
            let ty = self.resolve_type(&param.ty)?;
            env.insert(&param.name, ty, param.span)?;
        }
        let ret = match &func.ret {
            Some(t) => self.resolve_type(t)?,
            None => Ty::Void,
        };
        let mut found_return = false;
        self.check_block(&func.body, &mut env, &ret, &mut found_return)?;
        if ret != Ty::Void && !found_return {
            return Err(Error::new("missing return in non-void function", Some(func.span)));
        }
        Ok(())
    }

    fn check_block(&self, block: &Block, env: &mut Env, ret: &Ty, found_return: &mut bool) -> Result<(), Error> {
        env.push_scope();
        for stmt in &block.statements {
            self.check_stmt(stmt, env, ret, found_return)?;
        }
        env.pop_scope();
        Ok(())
    }

    fn check_stmt(&self, stmt: &Stmt, env: &mut Env, ret: &Ty, found_return: &mut bool) -> Result<(), Error> {
        match stmt {
            Stmt::Var(v) => {
                let init_ty = self.check_expr(&v.init, env, None)?;
                let ty = if let Some(t) = &v.ty {
                    let declared = self.resolve_type(t)?;
                    self.expect_type(&init_ty, &declared, v.span)?;
                    declared
                } else {
                    if matches!(v.init, Expr::Nil(_)) {
                        return Err(Error::new("cannot infer type from nil", Some(v.span)));
                    }
                    init_ty
                };
                env.insert(&v.name, ty, v.span)?;
            }
            Stmt::Assign(a) => {
                let target_ty = self.check_lvalue(&a.target, env)?;
                let value_ty = self.check_expr(&a.value, env, Some(&target_ty))?;
                self.expect_type(&value_ty, &target_ty, a.span)?;
            }
            Stmt::Return(r) => {
                *found_return = true;
                match (&r.value, ret) {
                    (None, Ty::Void) => {}
                    (None, _) => return Err(Error::new("return value required", Some(r.span))),
                    (Some(_), Ty::Void) => return Err(Error::new("void function cannot return a value", Some(r.span))),
                    (Some(expr), _) => {
                        let ty = self.check_expr(expr, env, Some(ret))?;
                        self.expect_type(&ty, ret, r.span)?;
                    }
                }
            }
            Stmt::Expr(e) => {
                self.check_expr(&e.expr, env, None)?;
            }
            Stmt::Block(b) => {
                self.check_block(b, env, ret, found_return)?;
            }
        }
        Ok(())
    }

    fn check_expr(&self, expr: &Expr, env: &mut Env, expected: Option<&Ty>) -> Result<Ty, Error> {
        match expr {
            Expr::Int(_, span) => {
                if expected.is_none() {
                    return Err(Error::new("integer literal requires type context", Some(*span)));
                }
                Ok(expected.unwrap().clone())
            }
            Expr::Float(_, _) => Ok(Ty::F64),
            Expr::String(_, _) => Ok(Ty::String),
            Expr::Bool(_, _) => Ok(Ty::Bool),
            Expr::Nil(span) => {
                if let Some(t) = expected {
                    if let Ty::Optional(_) = t {
                        return Ok(t.clone());
                    }
                }
                Err(Error::new("nil requires optional type context", Some(*span)))
            }
            Expr::Name(name, span) => env.lookup(name).ok_or_else(|| Error::new("unknown name", Some(*span))),
            Expr::StructLit { name, fields, span } => {
                let decl = self.structs.get(name).ok_or_else(|| Error::new("unknown struct", Some(*span)))?;
                let mut used = HashMap::new();
                for init in fields {
                    if used.insert(&init.name, ()).is_some() {
                        return Err(Error::new("duplicate field", Some(init.span)));
                    }
                    let field = decl.fields.iter().find(|f| f.name == init.name)
                        .ok_or_else(|| Error::new("unknown field", Some(init.span)))?;
                    let fty = self.resolve_type(&field.ty)?;
                    let vty = self.check_expr(&init.value, env, Some(&fty))?;
                    self.expect_type(&vty, &fty, init.span)?;
                }
                for field in &decl.fields {
                    if !used.contains_key(&field.name) {
                        return Err(Error::new("missing field in struct literal", Some(*span)));
                    }
                }
                Ok(Ty::Struct(name.clone()))
            }
            Expr::Call { callee, args, span } => {
                let qname = self.expr_to_qualified_name(callee)
                    .ok_or_else(|| Error::new("invalid call target", Some(*span)))?;
                if qname == "core.Print" {
                    if args.len() != 1 {
                        return Err(Error::new("core.Print expects 1 argument", Some(*span)));
                    }
                    let aty = self.check_expr(&args[0], env, Some(&Ty::String))?;
                    self.expect_type(&aty, &Ty::String, *span)?;
                    return Ok(Ty::Void);
                }

                let func = self.funcs.get(&qname).ok_or_else(|| Error::new("unknown function", Some(*span)))?;
                if func.params.len() != args.len() {
                    return Err(Error::new("argument count mismatch", Some(*span)));
                }
                for (arg, param) in args.iter().zip(func.params.iter()) {
                    let pty = self.resolve_type(&param.ty)?;
                    let aty = self.check_expr(arg, env, Some(&pty))?;
                    self.expect_type(&aty, &pty, *span)?;
                }
                let ret = match &func.ret {
                    Some(t) => self.resolve_type(t)?,
                    None => Ty::Void,
                };
                Ok(ret)
            }
            Expr::Field { base, name, span } => {
                let bty = self.check_expr(base, env, None)?;
                match bty {
                    Ty::Struct(sname) => {
                        let decl = self.structs.get(&sname).ok_or_else(|| Error::new("unknown struct", Some(*span)))?;
                        let field = decl.fields.iter().find(|f| f.name == *name)
                            .ok_or_else(|| Error::new("unknown field", Some(*span)))?;
                        self.resolve_type(&field.ty)
                    }
                    _ => Err(Error::new("field access on non-struct", Some(*span))),
                }
            }
            Expr::Unary { op, expr, span } => {
                let ty = self.check_expr(expr, env, None)?;
                match op {
                    UnaryOp::Not => self.expect_type(&ty, &Ty::Bool, *span).map(|_| Ty::Bool),
                    UnaryOp::Neg => {
                        if matches!(ty, Ty::I32 | Ty::I64 | Ty::F64) {
                            Ok(ty)
                        } else {
                            Err(Error::new("invalid unary '-'", Some(*span)))
                        }
                    }
                }
            }
            Expr::Binary { op, left, right, span } => {
                let lty = self.check_expr(left, env, None)?;
                let rty = self.check_expr(right, env, Some(&lty))?;
                self.expect_type(&rty, &lty, *span)?;
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        if matches!(lty, Ty::I32 | Ty::I64 | Ty::F64) {
                            Ok(lty)
                        } else {
                            Err(Error::new("invalid arithmetic types", Some(*span)))
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Neq => Ok(Ty::Bool),
                    BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => Ok(Ty::Bool),
                    BinaryOp::And | BinaryOp::Or => {
                        self.expect_type(&lty, &Ty::Bool, *span)?;
                        Ok(Ty::Bool)
                    }
                }
            }
            Expr::Group { expr, .. } => self.check_expr(expr, env, expected),
        }
    }

    fn check_lvalue(&self, lv: &LValue, env: &mut Env) -> Result<Ty, Error> {
        match lv {
            LValue::Name(name, span) => env.lookup(name).ok_or_else(|| Error::new("unknown name", Some(*span))),
            LValue::Field { base, name, span } => {
                let bty = self.check_lvalue(base, env)?;
                match bty {
                    Ty::Struct(sname) => {
                        let decl = self.structs.get(&sname).ok_or_else(|| Error::new("unknown struct", Some(*span)))?;
                        let field = decl.fields.iter().find(|f| f.name == *name)
                            .ok_or_else(|| Error::new("unknown field", Some(*span)))?;
                        self.resolve_type(&field.ty)
                    }
                    _ => Err(Error::new("field access on non-struct", Some(*span))),
                }
            }
        }
    }

    fn resolve_type(&self, ty: &Type) -> Result<Ty, Error> {
        match ty {
            Type::Name(name, _) => Ok(match name.as_str() {
                "bool" => Ty::Bool,
                "i32" => Ty::I32,
                "i64" => Ty::I64,
                "f64" => Ty::F64,
                "string" => Ty::String,
                "bytes" => Ty::Bytes,
                "void" => Ty::Void,
                _ => {
                    if self.structs.contains_key(name) {
                        Ty::Struct(name.clone())
                    } else if self.enums.contains_key(name) {
                        Ty::Enum(name.clone())
                    } else {
                        return Err(Error::new("unknown type", Some(self.type_span(ty))));
                    }
                }
            }),
            Type::Ctor { name, args, span: _ } => {
                let mut resolved = Vec::new();
                for arg in args {
                    resolved.push(self.resolve_type(arg)?);
                }
                match name.as_str() {
                    "option" => {
                        if resolved.len() != 1 {
                            return Err(Error::new("option requires 1 type argument", Some(self.type_span(ty))));
                        }
                        Ok(Ty::Optional(Box::new(resolved[0].clone())))
                    }
                    "result" | "mailbox" | "reply" => Ok(Ty::Ctor(name.clone(), resolved)),
                    _ => Err(Error::new("unknown type constructor", Some(self.type_span(ty)))),
                }
            }
            Type::Optional { inner, .. } => Ok(Ty::Optional(Box::new(self.resolve_type(inner)?))),
            Type::Func { params, ret, .. } => {
                let mut p = Vec::new();
                for t in params {
                    p.push(self.resolve_type(t)?);
                }
                let r = self.resolve_type(ret)?;
                Ok(Ty::Func(p, Box::new(r)))
            }
        }
    }

    fn type_span(&self, ty: &Type) -> Span {
        match ty {
            Type::Name(_, span) => *span,
            Type::Ctor { span, .. } => *span,
            Type::Optional { span, .. } => *span,
            Type::Func { span, .. } => *span,
        }
    }

    fn expect_type(&self, got: &Ty, expected: &Ty, span: Span) -> Result<(), Error> {
        if got == expected {
            Ok(())
        } else {
            Err(Error::new("type mismatch", Some(span)))
        }
    }

    fn func_name_key(&self, name: &FunctionName) -> String {
        match name {
            FunctionName::Free(n) => n.clone(),
            FunctionName::Method { type_name, method } => format!("{type_name}.{method}"),
        }
    }

    fn expr_to_qualified_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Name(n, _) => Some(n.clone()),
            Expr::Field { base, name, .. } => match &**base {
                Expr::Name(t, _) => Some(format!("{t}.{name}")),
                _ => None,
            },
            _ => None,
        }
    }
}

struct Env {
    scopes: Vec<HashMap<String, Ty>>,
}

impl Env {
    fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: &str, ty: Ty, span: Span) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(name) {
            return Err(Error::new("duplicate name", Some(span)));
        }
        scope.insert(name.to_string(), ty);
        Ok(())
    }

    fn lookup(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }
}
