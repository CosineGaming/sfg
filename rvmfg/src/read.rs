/// Includes all the code for reading from a binary and turning it into
/// instructions / types / etc
use crate::thread::Fn;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Str,
    Int,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Deser {
    Add,
    Dup,
    Equals,
    ExternFnCall,
    ExternFnHeader,
    FnCall,
    FnHeader,
    Panic,
    JumpZero,
    Pop,
    Push,
    Return,
    StringLit,
    Sub,
    Swap,
    Type(Type),
    Void,
}

pub fn deser(what: u8) -> Option<Deser> {
    use Deser as D;
    use Type::*;
    match what {
        // Types 1x
        0x10 => Some(D::Type(Int)),
        0x11 => Some(D::Type(Str)),
        // Other 2x
        0x21 => Some(D::Void),
        // Instructions 3x
        0x30 => Some(D::Push),
        0x31 => Some(D::ExternFnCall),
        0x32 => Some(D::StringLit),
        0x33 => Some(D::FnHeader),
        0x34 => Some(D::ExternFnHeader),
        0x35 => Some(D::Return),
        0x36 => Some(D::FnCall),
        0x37 => Some(D::Pop),
        0x38 => Some(D::Equals),
        0x39 => Some(D::JumpZero),
        0x3a => Some(D::Dup),
        0x3b => Some(D::Panic),
        0x3c => Some(D::Add),
        0x3d => Some(D::Sub),
        0x3e => Some(D::Swap),
        _ => None,
    }
}
pub fn deser_strong(what: u8) -> Deser {
    match deser(what) {
        Some(res) => res,
        None => panic!("tried to match invalid u8 0x{:X}", what),
    }
}

pub fn next(code: &Vec<u8>, ip: &mut usize) -> u8 {
    let rv = code[*ip];
    *ip += 1;
    rv
}

pub fn read_u32(code: &Vec<u8>, ip: &mut usize) -> u32 {
    use std::mem::transmute;
    let mut four: [u8; 4] = Default::default();
    four.copy_from_slice(&code[*ip..*ip + 4]);
    let rv = u32::from_le(unsafe { transmute(four) });
    *ip += 4;
    rv
}
pub fn read_i32(code: &Vec<u8>, ip: &mut usize) -> i32 {
    use std::mem::transmute;
    let mut four: [u8; 4] = Default::default();
    four.copy_from_slice(&code[*ip..*ip + 4]);
    let rv = i32::from_le(unsafe { transmute(four) });
    *ip += 4;
    rv
}
pub fn read_i8(code: &Vec<u8>, ip: &mut usize) -> i8 {
    use std::mem::transmute;
    let as_u8 = next(code, ip);
    unsafe { transmute(as_u8) }
}

pub fn read_to_zero(code: &Vec<u8>, mut ip: &mut usize) -> Vec<u8> {
    let mut rv = Vec::new();
    loop {
        let b = next(code, &mut ip);
        if b == 0 {
            break;
        }
        rv.push(b);
    }
    rv
}

pub fn read_string(code: &Vec<u8>, mut ip: &mut usize) -> String {
    let bytes = read_to_zero(&code, &mut ip);
    match String::from_utf8(bytes) {
        Ok(string) => string,
        Err(e) => panic!("invalid string {}", e),
    }
}

/// Returns (name, function)
pub fn read_fn_header(code: &Vec<u8>, mut ip: &mut usize, is_extern: bool) -> (String, Fn) {
    let return_type_u8 = next(code, &mut ip);
    let return_type = match deser_strong(return_type_u8) {
        Deser::Type(t) => Some(t),
        Deser::Void => None,
        _ => panic!("expected type or void, got {}", return_type_u8),
    };
    let param_count = next(code, &mut ip);
    let mut parameters = vec![];
    for _ in 0..param_count {
        let type_u8 = next(code, &mut ip);
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
        read_u32(code, &mut ip)
    };
    let func = Fn::new(codeloc, return_type, parameters);
    (name, func)
}
