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
   let pairs = pair_iter(strings.iter());
   let with_one_difference = pairs.filter(|&(x, y)| count_diff(x, y) == 1);
   let common_part = with_one_difference.map(|(x,y)| common_letters(x, y));
   for s in common_part {
       println!("{}", s);
   }
}

fn pair_iter<A, I: Clone + Iterator<Item = A>>(i: I) -> PairIter<A, I> {
    PairIter { i, a: None, i2: None }
}

struct PairIter<A, I: Iterator<Item = A>> {
    i: I,
    a: Option<A>,
    i2: Option<I>
}

impl<A : Clone, I: Clone + Iterator<Item = A>> Iterator for PairIter<A, I> {
    type Item = (A, A);

    fn next(&mut self) -> Option<(A, A)> {
        let mut need_new = false;
        let mut b: Option<A> = None;
        let mut new_pair = None;
        match self.i2.as_mut() {
            None => {
                match self.i.next() {
                    None => return None,
                    Some(a) => {
                        new_pair = Some((a, self.i.clone()));
                    }
                }
            }
            Some(i2) => { 
                b = i2.next(); 
            }
        }
        match new_pair {
            None => {},
            Some((a, mut i2)) => {
                self.a = Some(a);
                b = i2.next();
                self.i2 = Some(i2);
            }
        }
        match b {
            None => {
                self.i2 = None;
                return self.next()
            },
            Some(b) => {
                match self.a.as_mut() {
                    None => panic!("should never happen: 'a' missing."),
                    Some(a) => { return Some((a.clone(), b)) }
                }
            }
        }
        None
    }
}

