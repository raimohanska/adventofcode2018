extern crate itertools;

type Coord = i32;
type Power = i32;
type Position = (Coord, Coord);

fn main() {
    let serial = 9798;
    let max_x = 300;
    let max_y = 300;

    println!("{:?}", max_power_pos(max_x, max_y, serial));
}

fn max_power_pos(max_x: Coord, max_y: Coord, serial: i32) -> Position {
    iter_coords(max_x - 2, max_y - 2)
        .map(|(x, y)| (x + 1, y + 1)) // from 0-based to 1-based coords
        .max_by_key(|(x, y)| total_power_at(*x, *y, serial))
        .unwrap()
}

fn total_power_at(x0: Coord, y0: Coord, serial: i32) -> Power {
    let mut total = 0;
    for (x, y) in iter_coords(3, 3) {
      total = total + get_power_level(x + x0, y + y0, serial);
    }
    total
}

fn get_power_level(x: Coord, y: Coord, serial: i32) -> Power {
    let rack_id = x + 10;
    ((((rack_id * y) + serial) * rack_id) / 100) % 10 - 5
}

fn iter_coords(width: Coord, height: Coord) -> impl Iterator<Item = ( Coord,  Coord)> {
    (0..height).flat_map(move |y: Coord| { 
        (0..width).map(move |x| (x, y))
    })
}
