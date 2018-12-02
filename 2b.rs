#[macro_use] extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::collections::HashSet;
use itertools::Itertools;

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}

fn count_diff(x: &String, y: &String) -> usize {
    izip!(x.chars(), y.chars()).filter(|&(x, y)| x != y).count()
}

fn common_letters(x: &String, y: &String) -> String {
    izip!(x.chars(), y.chars()).filter(|&(x, y)| x == y).map(|(x, y)| x).collect()
}

fn main() {
   let strings: Vec<String> = lines_from_file("2.txt")
        .unwrap()
        .map(|line| line.unwrap())
        .collect();
   let pairs = iproduct!(strings.iter(), strings.iter());
   let with_one_difference = pairs.filter(|&(x, y)| count_diff(x, y) == 1);
   let common_part = with_one_difference.map(|(x,y)| common_letters(x, y));
   for s in common_part {
       println!("{}", s);
   }
}
