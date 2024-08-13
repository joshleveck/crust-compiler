use std::fs::File;
use std::io::Read;

pub fn read_file(path: &String) -> String {
    let mut f = File::open(path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    s
}

pub fn roundup(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}