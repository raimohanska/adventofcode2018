#[macro_use] extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::fmt;
#[macro_use]
extern crate nom;

#[derive(Debug,PartialEq)]
pub struct Rect {
  pub id: i32,
  pub x: i32,
  pub y: i32,
  pub width: i32,
  pub height: i32
}

impl fmt::Display for Rect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Use `self.number` to refer to each positional data point.
        write!(f, "(#{} @ {}, {}, {}, {})", self.id, self.x, self.y, self.width, self.height)
    }
}

named!(int<&str, i32>,
  map_res!(take_while_m_n!(1, 5, |c: char| { println!("check {}", c); c.is_digit(10)}), parse_int)
);

named!(rect<&str, Rect>,
  do_parse!(
      tag!("#") >>
      id: int >>
      tag!(" @ ") >>
      x: int >>
      tag!(",") >>
      y: int >>
      tag!(": ") >>
      width: int >>
      tag!("x") >>
      height : int >>
      (Rect { id, x, y, width, height })
  )
);

fn parse_int(input: &str) -> Result<i32, std::num::ParseIntError> {
  i32::from_str_radix(input, 10)
}

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())

}

fn main() {
   let strings: Vec<String> = lines_from_file("2.txt")
        .unwrap()
        .map(|line| line.unwrap() + "\n") // add \n to facilitate parsing
        .collect();
   match rect("#1 @ 661,227: 29x11\n").unwrap() {
       (_, r) => println!("{}", r)
   }
}
