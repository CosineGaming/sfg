extern crate docopt;
extern crate rsfg;
use docopt::Docopt;
use rsfg::STDLIB;
use rsfg::{compile, CompileError};
use rvmfg::{call, Thread};
use std::path::Path;

const USAGE: &str = "
rsfg command line interface

Usage: rsfg <source> [<dest>]
       rsfg --run <source>
       rsfg --update-tests

Options:
    -r, --run       [DEBUG ONLY] Use rvmfg virtual machine to immediately run code
    --update-tests  [DEBUG ONLY] Spawn interactive prompt to update stdout of tests if correct
    [<dest>]        can be directory (in which case it will be named source.bcfg) or filename
";

fn compile_file(filename: &Path) -> Result<Vec<u8>, CompileError> {
    let script_string = std::fs::read_to_string(filename).expect("could not load given file");
    compile(&script_string, STDLIB)
}

fn main() {
    env_logger::init();
    let args = Docopt::new(USAGE)
        .and_then(|d| d.parse())
        .unwrap_or_else(|e| e.exit());
    let script_filename = args.get_str("<source>");
    #[cfg(debug_assertions)]
    {
        if args.get_bool("--update-tests") {
            update_tests();
            std::process::exit(0);
        }
    }
    let script_path = Path::new(&script_filename);
    let result = compile_file(script_path);
    let compiled = match result {
        Ok(c) => c,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    if args.get_bool("--run") {
        let mut thread = Thread::new(compiled);
        call![thread.main()];
    } else {
        let out_path = args.get_str("<dest>");
        let out = if out_path == "" {
            script_path.with_extension("bcfg")
        } else {
            let out_path = Path::new(out_path);
            if out_path.is_file() {
                Path::new(&out_path).to_path_buf()
            } else {
                out_path
                    .join(script_path.file_name().unwrap())
                    .with_extension("bcfg")
            }
        };
        std::fs::write(out, compiled).expect("couldn't output compiled file");
    }
}

#[cfg(debug_assertions)]
fn update_tests() {
    // TODO: Actually write the tests this tests (and deduplicate)
    use log::debug;
    use std::io::*;
    let mut failed = 0;
    for entry in std::fs::read_dir("tests/scripts/error").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension() == Some(&std::ffi::OsString::from("sfg")) {
            let pathstr = path.to_string_lossy();
            let out_path = path.with_extension("stderr");
            debug!("COMPILING {}:", pathstr);
            let err = match compile_file(&path) {
                Ok(_) => {
                    println!("{}:\nCOMPILED OK. MUST FIX.", pathstr);
                    failed += 1;
                    continue;
                }
                Err(e) => e,
            };
            let out = format!("{}", err);
            if out_path.is_file() {
                let old = std::fs::read_to_string(out_path.clone()).unwrap();
                if old == out {
                    debug!("SAVED OUTPUT IS THE SAME. CONTINUING.");
                    continue;
                } else {
                    println!(
                        "{}:
OUTPUT:
{}
OLD OUTPUT:
{}
save Y/n?",
                        pathstr, out, old
                    );
                }
            } else {
                println!(
                    "{}:
OUTPUT:
{}
TEST OUTPUT NOT YET SAVED
save Y/n?",
                    pathstr, out
                );
            }
            let do_write = loop {
                let mut input = String::new();
                stdin().read_line(&mut input).unwrap();
                match input.trim() {
                    "Y" | "y" | "" => break true,
                    "N" | "n" => break false,
                    _ => continue,
                };
            };
            if do_write {
                println!("SAVING.");
                let mut file = std::fs::File::create(out_path).unwrap();
                file.write_all(out.as_bytes()).unwrap();
            } else {
                println!("PRESERVING OLD.");
                failed += 1;
            }
        }
    }
    if failed != 0 {
        println!("{} TESTS FAILED AND WEREN'T UPDATED.", failed);
        std::process::exit(failed);
    } else {
        println!("ALL TESTS SUCCEEDED OR WERE UPDATED.");
        std::process::exit(0);
    }
}
