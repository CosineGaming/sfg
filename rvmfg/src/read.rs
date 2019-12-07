/// Includes all the code for reading from a binary and turning it into
/// instructions / types / etc
use crate::thread::Fn;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Str,
    Int,
    Bool,
    Float,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Deser {
    Add,
    BAnd,
    Decl,
    DeVars,
    Div,
    Dup,
    FAdd,
    FDiv,
    FLess,
    FMul,
    FSub,
    ExternFnCall,
    ExternFnHeader,
    FnCall,
    FnHeader,
    Panic,
    JumpZero,
    Load,
    Locals,
    Mul,
    BNot,
    Pop,
    Push,
    Return,
    StringLit,
    Sub,
    Store,
    Type(Type),
    Void,
    Xor,
}

pub fn deser(what: u8) -> Option<Deser> {
    use Deser as D;
    use Type::*;
    Some(match what {
        // Sections
        0x01 => D::StringLit,
        0x02 => D::FnHeader,
        0x03 => D::ExternFnHeader,
        // Types 1x
        0x10 => D::Void,
        0x11 => D::Type(Int),
        0x12 => D::Type(Str),
        0x13 => D::Type(Bool),
        0x14 => D::Type(Float),
        // Data and Flow 2x
        0x20 => D::Push,
        0x21 => D::Pop,
        0x22 => D::FnCall,
        0x23 => D::ExternFnCall,
        0x24 => D::Return,
        0x25 => D::JumpZero,
        0x26 => D::Panic,
        0x27 => D::Locals,
        0x28 => D::DeVars,
        0x29 => D::Dup,
        0x2a => D::Decl,
        0x2b => D::Store,
        0x2c => D::Load,
        // Float/?? 4x
        0x40 => D::FAdd,
        0x41 => D::FSub,
        0x42 => D::FMul,
        0x43 => D::FDiv,
        0x44 => D::FLess,
        // Int 5x
        0x50 => D::Add,
        0x51 => D::Sub,
        0x52 => D::Mul,
        0x53 => D::Div,
        0x54 => D::BAnd,
        0x55 => D::Xor,
        0x56 => D::BNot,        // Types 1x
        _ => return None,
    })
}
pub fn deser_strong(what: u8) -> Deser {
    match deser(what) {
        Some(res) => res,
        None => panic!("tried to match invalid u8 0x{:X}", what),
    }
}

pub fn read_u8(code: &[u8], ip: &mut usize) -> u8 {
    let rv = code[*ip];
    *ip += 1;
    rv
}

pub fn read_u32(code: &[u8], ip: &mut usize) -> u32 {
    use std::mem::transmute;
    let mut four: [u8; 4] = Default::default();
    four.copy_from_slice(&code[*ip..*ip + 4]);
    let rv = u32::from_le(unsafe { transmute(four) });
    *ip += 4;
    rv
}
pub fn read_i32(code: &[u8], ip: &mut usize) -> i32 {
    use std::mem::transmute;
    let mut four: [u8; 4] = Default::default();
    four.copy_from_slice(&code[*ip..*ip + 4]);
    let rv = i32::from_le(unsafe { transmute(four) });
    *ip += 4;
    rv
}

pub fn read_to_zero(code: &[u8], mut ip: &mut usize) -> Vec<u8> {
    let mut rv = Vec::new();
    loop {
        let b = read_u8(code, &mut ip);
        if b == 0 {
            break;
        }
        rv.push(b);
    }
    rv
}

pub fn read_string(code: &[u8], mut ip: &mut usize) -> String {
    let bytes = read_to_zero(&code, &mut ip);
    match String::from_utf8(bytes) {
        Ok(string) => string,
        Err(e) => panic!("invalid string {}", e),
    }
}

/// Returns (name, function)
pub fn read_fn_header(code: &[u8], mut ip: &mut usize, is_extern: bool) -> (String, Fn) {
    let return_type_u8 = read_u8(code, &mut ip);
    let return_type = match deser_strong(return_type_u8) {
        Deser::Type(t) => Some(t),
        Deser::Void => None,
        _ => panic!("expected type or void, got {}", return_type_u8),
    };
    let param_count = read_u8(code, &mut ip);
    let mut parameters = vec![];
    for _ in 0..param_count {
        let type_u8 = read_u8(code, &mut ip);
        let param = match deser_strong(type_u8) {
            Deser::Type(t) => t,
            _ => panic!("expected type, got {}", type_u8),
        };
        parameters.push(param);
    }
    let name = read_string(&code, &mut ip);
    let codeloc = if is_extern {
        0
    } else {
        read_u32(code, &mut ip) as usize
    };
    let func = Fn::new(codeloc, return_type, parameters);
    (name, func)
}
