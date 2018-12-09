extern crate itertools;
use std::cmp;
use std::collections::LinkedList;

#[derive(Debug,PartialEq,Clone)]
struct Player {
    score: i64
}

// Here, a mutable Array just doesn't perform even when allocating all the space
// upfront. A LinkedList works fine though. Also, 32bit signed int overflows for score!
fn main() {
    // 424 players; last marble is worth 71144 points
    let (num_players, last_value) = (424, 7114400);
    let mut players = vec![Player { score: 0 }; num_players];
    let mut circle: LinkedList<i64> = LinkedList :: new();
    let mut current_player_i: usize = 0;
    let reporting_interval = cmp::max(last_value / 100, 100);
    for marble in 0..=last_value {
        if marble % reporting_interval == 0 {
            println!("{}%", (marble * 100 / last_value));
        }
        //println!("turn for {} marble value {}", current_player_i, marble);
        if marble > 0 && marble % 23 == 0 {
            //println!("score!");
            let current_player = players.get_mut(current_player_i).unwrap();
            rot_back(&mut circle, 7);
            let removed_marble = circle.pop_front().unwrap();
            let new_score = current_player.score + marble + removed_marble;
            current_player.score = new_score;
        } else {
            rot_fwd(&mut circle, 2);
            circle.push_front(marble);
        }
        current_player_i = (current_player_i + 1) % num_players;
        //print_circle(&circle, current_marble_i);
    }
    println!("Winner score {}", players.iter().map(|p| p.score).max().unwrap());
}

fn rot_fwd<T>(list: &mut LinkedList<T>, steps: usize) {
    if list.is_empty() { return };
    let mut left = steps;
    while left > 0 {
      left = left - 1;
      let el = list.pop_front().unwrap();
      list.push_back(el);
    }
}

fn rot_back<T>(list: &mut LinkedList<T>, steps: usize) {
    if list.is_empty() { return };
    let mut left = steps;
    while left > 0 {
      left = left - 1;
      let el = list.pop_back().unwrap();
      list.push_front(el);
    }
}

fn print_circle(circle: &Vec<i64>, current_marble_i: usize) {
    println!("{:?}", circle.iter().enumerate().map(|(i, v)| 
        if i == current_marble_i { format!("({})", v) } else { format!("{}", v) }
    ).collect::<Vec<String>>());
}
