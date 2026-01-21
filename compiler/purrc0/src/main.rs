mod ast;
mod error;
mod ir;
mod lexer;
mod parser;
mod span;
mod token;
mod typecheck;
mod emit_c;

use crate::lexer::lex;
use crate::parser::parse_program;
use crate::typecheck::typecheck;
use crate::emit_c::emit_c;
use std::env;
use std::fs;

fn main() {
    let path = match env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("purrc0: expected a source file path");
            std::process::exit(1);
        }
    };

    let source = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("purrc0: failed to read file: {e}");
            std::process::exit(1);
        }
    };

    let tokens = match lex(&source) {
        Ok(t) => t,
        Err(e) => {
            report_error(&source, &path, &e);
            std::process::exit(1);
        }
    };

    match parse_program(&tokens) {
        Ok(program) => {
            if let Err(e) = typecheck(&program) {
                report_error(&source, &path, &e);
                std::process::exit(1);
            }
            let c = match emit_c(&program) {
                Ok(c) => c,
                Err(e) => {
                    report_error(&source, &path, &e);
                    std::process::exit(1);
                }
            };
            let out_path = format!("{path}.c");
            if let Err(e) = fs::write(&out_path, c) {
                eprintln!("purrc0: failed to write output: {e}");
                std::process::exit(1);
            }
            println!("purrc0: wrote {out_path}");
        }
        Err(e) => {
            report_error(&source, &path, &e);
            std::process::exit(1);
        }
    }
}

fn report_error(source: &str, path: &str, err: &crate::error::Error) {
    eprintln!("{path}: {}", err.message);
    if let Some(span) = err.span {
        let (line, col) = line_col(source, span.start);
        eprintln!("  at {line}:{col}");
    }
}

fn line_col(source: &str, byte_index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    let mut i = 0;
    for ch in source.chars() {
        let len = ch.len_utf8();
        if i >= byte_index {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        i += len;
    }
    (line, col)
}
