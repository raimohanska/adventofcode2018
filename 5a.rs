extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use itertools::Itertools;
use std::fs::File;
use std::io::prelude::*;

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}

fn main() {
    let mut file = File::open("5.txt").expect("file not found");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("read to string fail");
}

fn react(chars: &[char]) {
}
