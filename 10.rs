extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
#[macro_use]
extern crate nom;

type Coord = i32;
type V2 = (Coord, Coord);
type Velocity = V2;
type Position = V2;
type Size = V2;

named!(uint<&str, i32>,
  map_res!(take_while_m_n!(1, 10, |c: char| c.is_digit(10)), parse_int)
);

named!(int<&str, i32>,
  do_parse!(
      opt!(tag!(" ")) >>
      sign: opt!(tag!("-")) >>
      x: uint >>
      (if sign.is_some() { -x } else { x })
  )
);

named!(coord_pair<&str, (V2, V2)>,
  do_parse!(
      tag!("position=<") >>
      x: int >>
      tag!(", ") >>
      y: int >>
      tag!("> velocity=<") >>
      vx: int >>
      tag!(", ") >>
      vy: int >>
      ((x, y), (vx, vy))
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
    let mut population: Vec<((Position, Velocity))> = lines_from_file("10.txt")
        .unwrap()
        .map(|line| line.unwrap())
        .map(|line:String| {
            coord_pair(&(line + "\n")).unwrap().1
          }) // add \n to facilitate parsing
        .collect();

    for n in 1..100000 {
        advance_population(&mut population);
        {
          let positions = population.iter().map(|(pos, _)|pos.clone());
          let bounds = population_bounds_size(positions.clone());
          if bounds.1 <= 10 {
            print_population(positions.clone());
            println!("Took {} seconds.", n);
            break;
          }
        }
    }
}

fn advance_population(population: &mut Vec<(Position, Velocity)>) {
    for elem in population {
      elem.0 = ((elem.0).0 + (elem.1).0, (elem.0).1 + (elem.1).1);  
    }
}

fn population_bounds_size<I : Iterator<Item = Position> + Clone>(coordinates: I) -> Size {
    let (max_x, max_y) = bottom_right_corner(normalize_positions(coordinates));
    (max_x + 1, max_y + 1)
}

fn normalize_positions<I : Iterator<Item = Position> + Clone>(coordinates: I) -> impl Iterator < Item = Position > + Clone {
    let (x0, y0): V2 = (
        coordinates.clone().map(|(x, _)| x).min().unwrap(), 
        coordinates.clone().map(|(_,y)| y).min().unwrap()
    );
    let translated = coordinates.clone().map(move |(x, y)| (x - x0, y - y0));
    translated
}

fn bottom_right_corner<I : Iterator<Item = Position> + Clone>(coordinates: I) -> Position {
    (
        coordinates.clone().map(|(x, _)| x).max().unwrap(),
        coordinates.clone().map(|(_, y)| y).max().unwrap()
    )
}

fn print_population<I : Iterator<Item = Position> + Clone>(coordinates: I) {
    let translated = normalize_positions(coordinates);
    let (x_max, y_max): Position = bottom_right_corner(translated.clone());
    let mut grid : Grid<bool> = Grid :: new(x_max + 1, y_max + 1, false);
    for (x, y) in translated.clone() {
        grid.set(x, y, true);
    }
    print_grid(&grid, |x| if *x { '*' } else { ' ' })
}

fn print_grid<T : Clone>(grid: &Grid<T>, f: fn(&T) -> char) {
    for (x,y) in grid.iter() {
        if x == 0 && y != 0 {
            println!();
        }
        let c = f(grid.get(x, y).unwrap());
        print!("{}", c);
    }
    println!();
}

struct Grid<A : Sized> {
    width: Coord,
    height: Coord,
    cells: Vec<A>
}

fn iter_coords(width: i32, height: i32) -> impl Iterator<Item = ( Coord,  Coord)> {
    (0..height).flat_map(move |y: Coord| { 
        (0..width).map(move |x| (x, y))
    })
}

impl<A : Clone> Grid<A> {
    fn new(width: Coord, height: Coord, init_value: A) -> Grid<A> {
        let cells : Vec<A> = vec![init_value; (width * height) as usize];
        Grid {
            width,
            height,
            cells
        }
    }
    fn get(&self, x: Coord, y: Coord) -> Option<&A> {
        self.index_for(x, y)
          .map(|index| &self.cells[index])
    }
    fn index_for(&self, x:  Coord, y:  Coord) -> Option<usize> {
        if x < 0 || y < 0 || x >= self.width || y >= self.height {
            return None
        }
        let index = (x + (y * self.width)) as usize;
        return Some(index)
    }
    fn set(&mut self, x:  Coord, y:  Coord, value: A) {
        let index = self.index_for(x, y)
            .expect(&format!("Out of bounds: {}, {}", x, y));
        self.cells[index] = value
    }
    fn iter<'a>(&'a self) -> impl Iterator<Item = (Coord,  Coord)> + 'a {
        iter_coords(self.width, self.height)
    }
}
