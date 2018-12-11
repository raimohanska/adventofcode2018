extern crate itertools;
use std::cmp;

type Coord = i32;
type V2 = (Coord, Coord);
type Position = V2;
type Width = Coord;
type Power = i32;

fn main() {
    let serial = 9798;
    let grid_size = 300;

    println!("{:?}", max_power_pos_any_size(grid_size, serial));
}

fn max_power_pos_any_size(grid_size: Width, serial: i32) -> (Position, Width, Power) {
   let (pos, (size, power)) = iter_coords(1, grid_size, grid_size)
     .map(|pos| (pos, max_power_at_pos(pos, grid_size, serial)))
     .inspect(|(pos, (size, power))| println!("{:?} size {} power {}", pos, size, power))
     .max_by_key(|(_, (_, power))| *power).unwrap();
   (pos, size, power)
}

fn max_power_at_pos(pos: Position, grid_size: Width, serial: i32) -> (Width, Power) {
  let (x0, y0) = pos;
  let max_size_here = grid_size - cmp::max(x0, y0);
  let state = (0, 0, 0);
  let (power, size, _) = (1..max_size_here).fold(state, |(max_power, max_size, prev_power), size| {
      let along_x: Power = (0..size).map(|x| get_power_level(x0 + x, y0 + size - 1, serial)).sum();
      let along_y: Power = (0..size-1).map(|y| get_power_level(x0 + size - 1, y0 + y, serial)).sum();
      let power = prev_power + along_x + along_y;
      //println!("size {} power {}", size, power);
      let (new_max_power, new_size) = if power > max_power { (power, size) } else { (max_power, max_size)};
      (new_max_power, new_size, power)
  });
  (size, power) 
}

fn get_power_level(x: Coord, y: Coord, serial: i32) -> Power {
    let rack_id = x + 10;
    ((((rack_id * y) + serial) * rack_id) / 100) % 10 - 5
}

fn iter_coords(base: Coord, width: Coord, height: Coord) -> impl Iterator<Item = (Coord,  Coord)> {
    (0..height).flat_map(move |y: Coord| { 
        (0..width).map(move |x| (x+base, y+base))
    })
}
