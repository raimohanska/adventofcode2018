extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::collections::HashSet;
use std::fmt;
#[macro_use]
extern crate nom;

const rule_width: usize = 5;
const rule_margin: usize = (rule_width - 1) / 2;

type Rule = (Vec<bool>, bool);

#[derive(Debug)]
struct State {
  start_index: i32,
  plants: Vec<bool>
}

impl State {
    fn ensure_capacity(&mut self) {
        if !self.has_capacity() {
            let margin = 10;
            self.start_index = self.start_index - margin;
            let mut new_plants = vec!(false; margin as usize);
            new_plants.append(&mut self.plants);
            self.plants = new_plants;
        }
    }
    fn has_capacity(&self) -> bool {
        for i in 0..rule_margin {
            if self.plants[i] { 
                return false;
            }
        }
        true
    }
    fn evolve(self, rules: &Vec<Rule>) -> State {
    }
}

fn main() {
   let lines: Vec<String> = lines_from_file("12.txt") .unwrap() .map(|x| x.unwrap()).collect();
   let mut state = initial_state(&lines[0]).unwrap().1;
   let instructions = lines.iter().skip(2).map(|line| rule(line).unwrap().1).collect::<Vec<_>>();
   state.ensure_capacity();
   println!("{:?}", state);
}

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())

}

named!(a_bool<&str, bool>,
  map!(alt!(tag!("#") | tag!(".")), |c| c == "#")
);

named!(initial_state<&str, State>,
  do_parse!(
      tag!("initial state: ") >>
      plants: many0!(a_bool) >>
      (State { plants, start_index: 0 })
  )
);

named!(rule<&str, Rule>,
  do_parse!(
      xs: many0!(a_bool) >>
      tag!(" => ") >>
      b: a_bool >>
      (xs, b)
  )
);

