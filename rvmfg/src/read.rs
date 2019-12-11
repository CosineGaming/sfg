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
#[repr(u8)]
pub enum Deser {
    Add,
    Decl,
    DeclLit,
    DeVars,
    Div,
    Dup,
    FAdd,
    FDiv,
    FLess,
    FMul,
    FSub,
    ExternFnCall,
    FnCall,
    Panic,
    Jump,
    JumpZero,
    Less,
    Load,
    Locals,
    Mod,
    Mul,
    Not,
    Pop,
    Push,
    Return,
    Sub,
    Store,
    StoreLit,
    Xor,
}

/// much less perf heavy, able to sub type
#[derive(PartialEq, Eq, Debug)]
pub enum DeserHeader {
    Type(Type),
    Void,
    StringLit,
    FnHeader,
    ExternFnHeader,
}

// deser is a really tightly called function and inlining it shows 25% speedups
#[inline(always)]
pub fn deser(what: u8) -> Deser {
    use Deser::*;
    match what {
        // Data and Flow 2x
        0x20 => Push,
        0x21 => Pop,
        0x22 => FnCall,
        0x23 => ExternFnCall,
        0x24 => Return,
        0x25 => JumpZero,
        0x26 => Panic,
        0x27 => Locals,
        0x28 => DeVars,
        0x29 => Dup,
        0x2a => Decl,
        0x2b => Store,
        0x2c => Load,
        0x2d => DeclLit,
        0x2e => StoreLit,
        0x2f => Jump,
        // Float/?? 4x
        0x40 => FAdd,
        0x41 => FSub,
        0x42 => FMul,
        0x43 => FDiv,
        0x44 => FLess,
        // Int 5x
        0x50 => Add,
        0x51 => Sub,
        0x52 => Mul,
        0x53 => Div,
        0x54 => Less,
        0x55 => Xor,
        0x56 => Mod,
        // Bool/?? 6x
        0x60 => Not,
        fail => panic!("tried to match invalid u8 0x{:X}", fail),
    }
}
pub fn deser_header(what: u8) -> Option<DeserHeader> {
    use DeserHeader::*;
    use self::Type::*;
    Some(match what {
        // Sections
        0x01 => StringLit,
        0x02 => FnHeader,
        0x03 => ExternFnHeader,
        // Types 1x
        0x10 => Void,
        0x11 => Type(Int),
        0x12 => Type(Str),
        0x13 => Type(Bool),
        0x14 => Type(Float),
        _ => return None,
    })
}
pub fn deser_header_strong(what: u8) -> DeserHeader {
    match deser_header(what) {
        Some(res) => res,
        None => panic!("tried to match invalid u8 0x{:X} in header", what),
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
pub fn read_u16(code: &[u8], ip: &mut usize) -> u16 {
    use std::mem::transmute;
    let mut region: [u8; 2] = Default::default();
    region.copy_from_slice(&code[*ip..*ip + 2]);
    let rv = u16::from_le(unsafe { transmute(region) });
    *ip += 2;
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
    let return_type = match deser_header_strong(return_type_u8) {
        DeserHeader::Type(t) => Some(t),
        DeserHeader::Void => None,
        _ => panic!("expected type or void, got {}", return_type_u8),
    };
    let param_count = read_u8(code, &mut ip);
    let mut parameters = vec![];
    for _ in 0..param_count {
        let type_u8 = read_u8(code, &mut ip);
        let param = match deser_header_strong(type_u8) {
            DeserHeader::Type(t) => t,
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
