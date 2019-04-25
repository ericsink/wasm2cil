
use std::env;

pub fn foo(next_in : u32, input: &[u8]) -> u64 {
    let v = 0 as u64;
    let w = v | (input[next_in as usize] as u64) << 56;
    w
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let a = args.iter().map(|x| x.parse::<u8>().unwrap()).collect::<Vec<u8>>();
    let x = foo(0, &a);
    println!("{:x}", x);
}

