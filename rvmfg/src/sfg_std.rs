// As much as possible of the standard library is compiled to bcfg to keep
// the VM minimal
// That which cannot be is supposed to be implemented by the order of
// one or two parent-app functions, and is here

pub fn log(what: &str) {
	println!("{}", what);
}

