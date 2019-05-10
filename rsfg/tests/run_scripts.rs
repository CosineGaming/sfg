extern crate rsfg;
use rsfg::compile;

fn compile_file(filename: &str) -> Vec<u8> {
    let script_string = std::fs::read_to_string(filename).expect("could not load given file");
    compile(&script_string, "")
}

fn assert_hex(a: Vec<u8>, b: Vec<u8>) {
    assert_eq!(a, b, "result:\n{:x?}\nexpected:\n{:x?}", a, b);
}

#[test]
fn decompile() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug);
    let result = compile_file("tests/scripts/decompile.sfg");
    assert_hex(
        result,
        vec![
            // bcfg
            0x62, 0x63, 0x66, 0x67, // header for main:
            // fn header
            0x33, // return type: void
            0x21, // parameter count
            0, // "main"
            0x6d, 0x61, 0x69, 0x6e, // "\0"
            0, // codeloc (4b)
            0x1c, 0x00, 0x00, 0x00, // header for log:
            // extern fn header
            0x34, // return type: void
            0x21, // parameter count
            1, // parameters:
            // type: string
            0x11, // "log\0"
            0x6c, 0x6f, 0x67, 0, // string lit
            0x32, // "hi\0"
            0x68, 0x69, 0, // push
            0x30, // 8
            0x08, 0x00, 0x00, 0x00, // push 8 again
            0x30, 0x08, 0x00, 0x00, 0x00, // equals
            0x38, // jump zero
            0x39, // by one instruction (just the return) to AFTER:
            0x01, // return
            0x35, // AFTER:
            // call log:
            // push string lit
            0x30, 0x00, 0x00, 0x00, // string index
            0, // call extern log
            0x31, // function index (4b)
            // 1 (0:main, 1:log)
            0x01, 0x00, 0x00, 0x00, // Return
            0x35,
        ],
    );
}
