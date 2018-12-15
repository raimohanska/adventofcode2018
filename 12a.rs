extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::collections::HashSet;
use std::fmt;
#[macro_use]
extern crate nom;

const RULE_WIDTH: usize = 5;
const RULE_MARGIN: usize = (RULE_WIDTH - 1) / 2;

type Rule = (Vec<bool>, bool);

#[derive(Debug)]
struct State {
  start_index: i32,
  plants: Vec<bool>
}

impl State {
    fn add_margin(&mut self) {
        let margin = 4;
        self.start_index = self.start_index - margin;

        let mut new_plants = vec!(false; margin as usize);
        new_plants.append(&mut self.plants);
        new_plants.append(&mut vec!(false; margin as usize));
        self.plants = new_plants;
    }
    fn evolve(&mut self, rules: &Vec<Rule>) -> State {
        self.add_margin();
        let plants: Vec<bool> = self.plants.windows(RULE_WIDTH).map(|slice| {
           let rule: &Rule = rules.iter().find(|rule| match_rule(rule, slice)).unwrap(); 
           rule.1
        }).collect();
        State { start_index: self.start_index + 2, plants }
    }
}

fn match_rule(r: &Rule, slice: &[bool]) -> bool {
    let pattern: &[bool] = &r.0;
    slice == pattern
}

fn print_state(s: &State) {
    print!("{:?} ", s.start_index);
    for b in &s.plants {
       print!("{}", if *b { "#" } else { "." }) 
    }
    println!();
}

fn main() {
   let lines: Vec<String> = lines_from_file("12.txt") .unwrap() .map(|x| x.unwrap()).collect();
   let mut state = initial_state(&lines[0]).unwrap().1;
   let rules = lines.iter().skip(2).map(|line| rule(line).unwrap().1).collect::<Vec<_>>();
   print_state(&state);
   for i in 1..=20 {
     state = state.evolve(&rules);
     print_state(&state);
   }
   let sum: i32 = state.plants.iter().enumerate().filter(|(i, s)| **s).map(|(i, _)| (i as i32) + state.start_index).sum();
   println!("{:?}", sum);
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

