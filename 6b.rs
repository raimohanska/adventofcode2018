extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
#[macro_use]
extern crate nom;

type Coord = i32;
type Coords = (Coord, Coord);
type AreaId = usize;
type Distance = i32;
type Cell = Distance;

named!(int<&str, i32>,
  map_res!(take_while_m_n!(1, 5, |c: char| c.is_digit(10)), parse_int)
);

named!(coords<&str, Coords>,
  do_parse!(
      x: int >>
      tag!(", ") >>
      y: int >>
      ((x, y))
  )
);

fn parse_int(input: &str) -> Result<i32, std::num::ParseIntError> {
  i32::from_str_radix(input, 10)
}

fn lines_from_file<P>(filename: P) -> Result<io::Lines<io::BufReader<File>>, io::Error>
  where P: AsRef<Path>,
{
    let file = try!(File::open(filename));
    Ok(io::BufReader::new(file).lines())

}

fn main() {
   let coordinates: Vec<Coords> = lines_from_file("6.txt")
        .unwrap()
        .map(|line| coords(&(line.unwrap() + "\n")).unwrap().1) // add \n to facilitate parsing
        .collect();

   let (width, height) = find_bounds(&coordinates);
   let area_size = iter_coords(width, height)
       .filter(|(x, y)| calc_distance_sum((*x, *y), &coordinates) < 10000)
       .count();
   println!("{}", area_size);

}

fn calc_distance_sum(pos: Coords, coordinates: &Vec<Coords>) -> i32 {
    coordinates.iter().map(|(x, y)| distance((*x, *y), pos)).sum()
}

fn find_bounds(coords: &Vec<Coords>) -> (Coord, Coord) {
    let mut width = 0;
    let mut height = 0;
    for (x, y) in coords {
        if (x + 1) > width { width = *x + 1 };
        if (y + 1) > height { height = *y + 1 };
    }
    (width, height)
}

struct Grid<A : Sized> {
    width: Coord,
    height: Coord,
    cells: Vec<Option<A>>
}

fn iter_coords(width: i32, height: i32) -> impl Iterator<Item = ( Coord,  Coord)> {
    (0..height).flat_map(move |y: Coord| { 
        (0..width).map(move |x| (x, y))
    })
}

fn distance(a: Coords, b: Coords) -> i32 {
    let (x1, y1) = a;
    let (x2, y2) = b;
    return (x1 - x2).abs() + (y1 -y2).abs();
}

impl<A : Clone> Grid<A> {
    fn new(width: Coord, height: Coord) -> Grid<A> {
        let mut cells : Vec<Option<A>> = vec![None; (width * height) as usize];
        for i in 0 .. (width*height) {
            cells[i as usize] = None
        }
        Grid {
            width,
            height,
            cells
        }
    }
    fn get(&self, x: Coord, y: Coord) -> Option<&A> {
        self.index_for(x, y)
          .and_then(|index| self.cells[index].as_ref())
    }
    fn index_for(&self, x:  Coord, y:  Coord) -> Option<usize> {
        if x < 0 || y < 0 || x >= self.width || y >= self.height {
            return None
        }
        let index = (x + (y * self.width)) as usize;
        return Some(index)
    }
    fn set(&mut self, x:  Coord, y:  Coord, value: Option<A>) {
        let index = self.index_for(x, y)
            .expect(&format!("Out of bounds: {}, {}", x, y));
        self.cells[index] = value
    }
    fn iter<'a>(&'a self) -> impl Iterator<Item = ( Coord,  Coord)> + 'a {
        iter_coords(self.width, self.height)
    }
    fn neighbors<'a>(&'a self, x:  Coord, y:  Coord) -> impl Iterator<Item = (Coord, Coord)> + 'a {
      vec!((x+1, y), (x-1, y), (x, y+1), (x, y-1)).into_iter()
          .filter(move |(x, y)| self.index_for(*x, *y).is_some())
    }
    fn is_border(&self, x: Coord, y: Coord) -> bool {
        x == 0 || y == 0 || x == self.width - 1 || y == self.height - 1
    }
}
