#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    LoadConst {
        dst: u8,
        idx: u16,
    },
    LoadNull {
        dst: u8,
    },
    LoadTrue {
        dst: u8,
    },
    LoadFalse {
        dst: u8,
    },
    Move {
        dst: u8,
        src: u8,
    },

    Add {
        dst: u8,
        a: u8,
        b: u8,
    },
    Sub {
        dst: u8,
        a: u8,
        b: u8,
    },
    Mul {
        dst: u8,
        a: u8,
        b: u8,
    },
    Div {
        dst: u8,
        a: u8,
        b: u8,
    },
    Mod {
        dst: u8,
        a: u8,
        b: u8,
    },
    Pow {
        dst: u8,
        a: u8,
        b: u8,
    },
    Neg {
        dst: u8,
        src: u8,
    },

    BitAnd {
        dst: u8,
        a: u8,
        b: u8,
    },
    BitOr {
        dst: u8,
        a: u8,
        b: u8,
    },
    BitXor {
        dst: u8,
        a: u8,
        b: u8,
    },
    Shl {
        dst: u8,
        a: u8,
        b: u8,
    },
    Shr {
        dst: u8,
        a: u8,
        b: u8,
    },
    BitNot {
        dst: u8,
        src: u8,
    },

    Eq {
        dst: u8,
        a: u8,
        b: u8,
    },
    Ne {
        dst: u8,
        a: u8,
        b: u8,
    },
    Lt {
        dst: u8,
        a: u8,
        b: u8,
    },
    Gt {
        dst: u8,
        a: u8,
        b: u8,
    },
    Le {
        dst: u8,
        a: u8,
        b: u8,
    },
    Ge {
        dst: u8,
        a: u8,
        b: u8,
    },

    Not {
        dst: u8,
        src: u8,
    },

    Inc {
        dst: u8,
    },
    Dec {
        dst: u8,
    },

    Jump {
        offset: i32,
    },
    JumpIfFalse {
        src: u8,
        offset: i32,
    },
    JumpIfTrue {
        src: u8,
        offset: i32,
    },

    GetGlobal {
        dst: u8,
        name_idx: u16,
    },
    SetGlobal {
        name_idx: u16,
        src: u8,
    },
    GetLocal {
        dst: u8,
        slot: u8,
    },
    SetLocal {
        slot: u8,
        src: u8,
    },

    Call {
        dst: u8,
        func_reg: u8,
        arg_start: u8,
        arg_count: u8,
    },
    Return {
        src: u8,
    },
    ReturnNull,

    NewArray {
        dst: u8,
        start: u8,
        count: u8,
    },
    GetIndex {
        dst: u8,
        obj: u8,
        idx: u8,
    },
    SetIndex {
        obj: u8,
        idx: u8,
        val: u8,
    },

    GetMember {
        dst: u8,
        obj: u8,
        name_idx: u16,
    },
    SetMember {
        obj: u8,
        name_idx: u16,
        val: u8,
    },

    Concat {
        dst: u8,
        start: u8,
        count: u8,
    },
    ToString {
        dst: u8,
        src: u8,
    },
}
#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub constants: Vec<crate::value::Value>,
    pub names: Vec<String>,
    pub local_count: u8,
    pub param_count: u8,
}
