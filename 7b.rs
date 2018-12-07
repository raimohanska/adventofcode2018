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
struct Worker {
    task: char, 
    left: i32
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
    let mut result: String = "".to_string();
    let mut in_progress: Vec<Worker> = Vec::new();
    let mut free_workers: i32 = 5;
    let mut second = 0;

    while !(sorted_roots.is_empty() && in_progress.is_empty()) {
      while free_workers > 0 && !sorted_roots.is_empty() {
        let root: char = sorted_roots.remove(0);
        println!("{} Start work on {}", second, root);
        free_workers = free_workers - 1;
        in_progress.push(Worker { task: root, left: calc_time(root) });
      }

      second = second + 1;
      for worker in in_progress.iter_mut() {
          worker.left = worker.left - 1;
      }

      in_progress.retain(|worker| {
          if worker.left == 0 {
              free_workers = free_workers + 1;
              let root = worker.task;
              println!("{} Finish work on {}", second, root);
              result.push(root);
              for new_root in iterate_next_roots(&mut tasks, root) {
                  sorted_roots.push(new_root);
              }
              sorted_roots.sort();
              return false
          }
          true
      });
    }

    println!("Result is {}", result);
    println!("Took {} seconds", second);
}

fn calc_time(task: char) -> i32 {
    60 + (task as i32) - ('A' as i32) + 1
}

fn iterate_next_roots(tasks: &mut TaskMap, root: char) -> Vec<char> {
  let to_check: HashSet<char> = tasks.get(&root).unwrap().after
    .iter().map(|c| *c)
    .collect();
  let mut next_roots = Vec :: new();
  for key in to_check {
      let deps: &mut Task = tasks.get_mut(&key).unwrap();
      deps.before.remove(&root);
      if deps.before.is_empty() {
          next_roots.push(key);
      }
  }
  next_roots.sort();
  next_roots
}

fn find_roots(tasks: &TaskMap) -> Vec<char> {
    let mut roots : Vec<char> = tasks.iter()
        .filter(|(_, deps)| deps.before.is_empty())
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
