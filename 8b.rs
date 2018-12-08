extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::iter;

struct Node {
    children: Vec<Node>,
    metadata: Vec<i32>
}

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())
}


fn main() {
   let lines: Vec<String> = lines_from_file("8.txt")
        .unwrap()
        .map(|line| line.unwrap())
        .collect();
   let nums: Vec<i32> = lines
        .iter()
        .flat_map(|line : &String| line.split(" "))
        .map(|num| {
            num.parse::<i32>().unwrap()
         })
        .collect();
    let root = read_node(&mut nums.into_iter());
    println!("{}", node_value(&root));
}

fn node_value(n: &Node) -> i32 {
  if n.children.len() == 0 {
     return n.metadata.iter().sum();
  } else {
     let child_sum: i32 = n.metadata.iter()
         .flat_map(|i| {
             let res = n.children.get((*i - 1) as usize);
             res
         })
         .map(node_value).sum();
     return child_sum
  }
}

fn read_node<I: Iterator<Item = i32>>(i: &mut I) -> Node {
    let num_children = i.next().expect("expected num_children");
    let num_metadata = i.next().expect("expected num_metadata");
    let children: Vec<Node> = iter::repeat(0).take(num_children as usize).map(|_| read_node(i)).collect();
    let metadata: Vec<i32> = i.take(num_metadata as usize).collect();
    Node { children, metadata }
}
