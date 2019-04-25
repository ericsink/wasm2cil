
fn Log2FloorNonZero(mut n: u64) -> u32 {
  let mut result: u32 = 0u32;
  'loop1: loop {
    if {
         n = n >> 1i32;
         n
       } != 0 {
      result = result.wrapping_add(1 as (u32));
      continue 'loop1;
    } else {
      break 'loop1;
    }
  }
  result
}

fn foo(n: u64) {
  if n == 0 {
  println!("d1");
  } else {
  println!("d2a : n = {}", n);
    let nbits: u8 = Log2FloorNonZero(n) as (u8);
  println!("d3 : nbits = {}", nbits);
  println!("d4");
  println!("d5");
    let q1 = 1u64 << nbits;
  println!("q1 {}", q1);
    let q2 = n.wrapping_sub(q1);
  println!("q2 {}", q2);
  println!("d6");
  }
}

fn main() {
    foo(1 as (u64));
    println!("Hello, world!");
}

