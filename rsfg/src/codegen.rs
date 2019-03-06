use crate::ast;
use crate::Type;

enum Byte {
	Sep = 0,
}

fn type_size(id_type: Type) -> u8 {
	use Type::*;
	match id_type {
		Int => 4,
		Infer => panic!("type not yet inferred by size check"),
	}
}

fn type_to_byte(id_type: Type) -> u8 {
	use Type::*;
	match id_type {
		Int => 'i' as u8,
		Infer => panic!("type not yet inferred by printing"),
	}
}

fn gen_fn_header(func: ast::Function) -> Vec<u8> {
	// We require the code location but until generation of all
    // the code we can't know where that is
	let mut no_code_loc = Vec::new();
	let mut size = 0;
	for param in &func.signature.parameters {
		size += type_size(param.id_type);
	}
	// stack size
	no_code_loc.push(size);
	// return type
	no_code_loc.push(match func.signature.return_type {
		Some(rt) => type_to_byte(rt),
		None => 'v' as u8,
	});
	// number of parameters
	no_code_loc.push(func.signature.parameters.len() as u8);
	// the type of each parameter
	for param in func.signature.parameters {
		no_code_loc.push(type_to_byte(param.id_type));
	}
	// name
	no_code_loc.append(&mut func.name.into_bytes());
	// sep to finish name
	no_code_loc.push(Byte::Sep as u8);
	no_code_loc
}

fn gen_extern_fn_header(func: ast::ExternFn) -> Vec<u8> {
	panic!("extern fn codegen unimplemented!");
}

pub fn gen(ast: ast::AST) -> Vec<u8> {
	println!("WARNING: codegen is incomplete!");
	let mut code = Vec::new();
	let mut fn_headers = Vec::new();
	for node in ast {
		fn_headers.push(match node {
			ast::ASTNode::Function(func) => gen_fn_header(func),
			ast::ASTNode::ExternFn(func) => gen_extern_fn_header(func),
		})
	}
	// TODO: actually we need to add the code_loc so this isn't done
	for mut header in fn_headers {
		code.append(&mut header);
	}
	code
}

