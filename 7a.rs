extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
#[macro_use]
extern crate nom;
use std::collections::HashMap;
use std::collections::HashSet;

type Dependency = (char, char);
struct Task {
    before: HashSet<char>,
    after: HashSet<char>
}

type TaskMap = HashMap<char, Task>;

named!(dep<&str, Dependency>,
  do_parse!(
      tag!("Step ") >>
      x: up_char >>
      tag!(" must be finished before step ") >>
      y: up_char >>
      ((x, y))
  )
);

named!(up_char<&str, char>,
  one_of!("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
);

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}

fn main() {
   let deps: Vec<Dependency> = lines_from_file("7.txt")
        .unwrap()
        .map(|line| line.unwrap())
        .map(|line:String| {
            dep(&(line)).unwrap().1
          })
        .collect();
    println!("{} dep lines", deps.len());

    let mut tasks: TaskMap = HashMap :: new();


    for (before, after) in deps.into_iter() {
        add_dep(&mut tasks, before, after);
    }

    println!("{} tasks", tasks.len());

    let mut sorted_roots = find_roots(&tasks);
    let mut result: Vec<char> = Vec :: new();

    while (!sorted_roots.is_empty()) {
      let root: char = sorted_roots.remove(0);

      result.push(root);
      for new_root in iterate_next_roots(&mut tasks, root) {
          sorted_roots.push(new_root);
      }
      sorted_roots.sort();
    }

    println!("Result size {}", result.len());
    for r in result {
        print!("{}", r);
    }
    println!();
}

fn iterate_next_roots(tasks: &mut TaskMap, root: char) -> Vec<char> {
  let to_check: HashSet<char> = tasks.get(&root).unwrap().after
    .iter().map(|c| *c)
    .collect();
  let mut next_roots = Vec :: new();
  for key in to_check {
      let deps: &mut Task = tasks.get_mut(&key).unwrap();
      deps.before.remove(&root);
      if (deps.before.is_empty()) {
          next_roots.push(key);
      }
  }
  next_roots.sort();
  next_roots
}

fn find_roots(tasks: &TaskMap) -> Vec<char> {
    let mut roots : Vec<char> = tasks.iter()
        .filter(|(task, deps)| deps.before.is_empty())
        .map(|(task, _)| *task)
        .collect();
    roots.sort();
    roots
}

fn add_dep(tasks: &mut TaskMap, before: char, after: char) {
    ensure_key(tasks, before);
    ensure_key(tasks, after);
    {
        let before_entry = tasks.get_mut(&before).unwrap();
        before_entry.after.insert(after);
    }
    {
        let after_entry = tasks.get_mut(&after).unwrap();
        after_entry.before.insert(before);
    }
}

fn ensure_key(tasks: &mut TaskMap, key: char) {
    if !tasks.contains_key(&key) {
        tasks.insert(key, Task { before: HashSet :: new(), after: HashSet :: new()});
    }
}
