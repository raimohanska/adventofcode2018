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
type Cell = (Vec<AreaId>, Distance);

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
        .map(|line| line.unwrap())
        .map(|line:String| {
            coords(&(line + "\n")).unwrap().1
          }) // add \n to facilitate parsing
        .collect();

   let (width, height) = find_bounds(&coordinates);
   //                   id,    distance
   let mut grid : Grid<Cell> = Grid :: new(width, height);
   println!("Created grid size {}x{}", width, height);

   for (id, (x, y)) in coordinates.iter().enumerate() {
       grid.set(*x, *y, Some((vec!(id), 0)))
   }
   for _ in 0..10000 { 
       //print_grid(&grid);
       //println!();
       let progress = expand_areas(&mut grid);
       if !progress {
           break;
       }
   }

   let mut sizes: Vec<usize> = coordinates.iter().enumerate()
       .map(|(id,_)| area_size(&grid, id))
       .collect();

   sizes.sort();
   let largest = sizes[sizes.len() - 1];

   println!("{}", largest);
}

// returns zero for infinite areas
fn area_size(grid: &Grid<Cell>, area_id: AreaId) -> usize {
    let mut count = 0;
    for (x, y) in grid.iter() {
        for (ids, _) in grid.get(x, y) {
          if ids.len() == 1 && ids[0] == area_id {
            if grid.is_border(x, y) {
                return 0;
            }
            count = count + 1;
          }
        }
    }
    count
}

fn print_grid(grid: &Grid<Cell>) {
    for (x,y) in grid.iter() {
        if x == 0 && y != 0 {
            println!();
        }
        let c = match grid.get(x,y) {
            Some((ids, distance)) if ids.len() == 1 => distance.to_string(),
            _ => '.'.to_string(),
        };
        print!("{}", c);
    }
    println!();
}

fn expand_areas(grid: &mut Grid<Cell>) -> bool {
    let mut found = false;
    let mut changes = Vec :: new();
    for (x, y) in grid.iter() {
        if grid.get(x,y).is_none() { // empty slot
            let mut neighbors:Vec<&Cell> = grid.neighbors(x,y)
                .flat_map(|(x,y)| grid.get(x,y))
                .collect();
            neighbors.sort();
            let count = neighbors.len();
            if count > 0 {
                // TODO
                let mut all_neighbors: Vec<(AreaId, Distance)> = neighbors.iter()
                    .flat_map(|(ids, d)| { 
                        ids.iter().map(move |id| { 
                            (*id, *d) 
                        })
                    })
                    .collect();
                all_neighbors.sort_by(|(_, d), (_, d2)| d.cmp(d2));
                let min_d = all_neighbors[0].1;
                let mut nearest_neighbors: Vec<AreaId> = all_neighbors.iter()
                    .filter(|(_, d)| *d == min_d)
                    .map(|(id, _)| *id)
                    .collect();
                nearest_neighbors.sort();
                nearest_neighbors.dedup();

                let new_value = (nearest_neighbors, min_d + 1);

                changes.push((x, y, new_value));
                found = true;
            }
        }
    }
    for (x, y, v) in changes {
        grid.set(x, y, Some(v))
    }
    return found;
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
