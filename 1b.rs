use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::collections::HashSet;

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}

fn find_first_repeated<I: Iterator<Item = i32>>(mut directions: I) -> i32{
  let mut history = HashSet :: new();
  let mut next = 0;
  loop {
      next += directions.next().unwrap();
      if history.contains(&next) {
          return next
      }
      history.insert(next);
  }
}

fn main() {
   let numbers: Vec<i32> = lines_from_file("1.txt")
        .unwrap()
        .map(|line| line.unwrap().parse::<i32>().unwrap())
        .collect();

   let directions = numbers
     .iter().cloned().cycle();

   let result = find_first_repeated(directions); 
   println!("{}", result);
}
