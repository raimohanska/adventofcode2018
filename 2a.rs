extern crate itertools;
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

fn has_count(count: usize, str: &String) -> bool {
    let mut vec = str.chars().collect::<Vec<char>>();
    vec.sort();
    let grouped = vec.into_iter().group_by(|&c| c);
    let mut counts = grouped.into_iter().map(|(k, group)| group.count());
    counts.any(|c| c == (count as usize))
}

fn calc_counts(count: usize, lines: &Vec<String>) -> usize {
    lines.iter().filter(|line| has_count(count, *line)).count()
}

fn main() {
   let strings: Vec<String> = lines_from_file("2.txt")
        .unwrap()
        .map(|line| line.unwrap())
        .collect();

   let result = calc_counts(2, &strings) * calc_counts(3, &strings);
   println!("{}", result);
}
