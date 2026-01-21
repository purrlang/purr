#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: String,
    pub instrs: Vec<Instr>,
    pub term: Terminator,
}

#[derive(Debug, Clone)]
pub enum Instr {
    ConstI64 { dst: String, value: i64 },
    AddI64 { dst: String, lhs: String, rhs: String },
    Load { dst: String, addr: String },
    Store { addr: String, src: String },
    Call { dst: Option<String>, func: String, args: Vec<String> },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Ret { value: Option<String> },
    Br { target: String },
    BrIf { cond: String, then_t: String, else_t: String },
}
