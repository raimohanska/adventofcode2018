use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}

fn handle_lines<I: Iterator<Item = String>>(lines: I) {
    let result = lines
      .map(|line| line.parse::<i32>().unwrap())
      .fold(0, |acc, x| acc + x); 
    println!("{}", result);
}

fn main() {
    let lines = lines_from_file("1.txt");
    handle_lines(lines.unwrap().map(|line| line.unwrap()));
}
