extern crate itertools;
use std::io::{self, BufRead};
use std::path::Path;
use std::fs::File;
use std::iter;

#[derive(Debug,PartialEq,Clone)]
struct Player {
    score: i32
}

fn main() {
    // 424 players; last marble is worth 71144 points
    let (num_players, last_value) = (424, 71144);
    let mut players = vec![Player { score: 0 }; num_players];
    let mut circle: Vec<i32> = Vec :: new();
    let mut current_marble_i: usize = 0;
    let mut current_player_i: usize = 0;
    for marble in 0..=last_value {
        //println!("turn for {} marble value {}", current_player_i, marble);
        if marble > 0 && marble % 23 == 0 {
            //println!("score!");
            let current_player = players.get_mut(current_player_i).unwrap();
            let new_index = (current_marble_i + circle.len() - 7) % circle.len();
            let removed_marble = circle.remove(new_index);
            let new_score = current_player.score + marble + removed_marble;
            current_player.score = new_score;
            current_marble_i = new_index;
        } else {
            if (circle.len() > 0) {
                current_marble_i = ((current_marble_i + 1) % circle.len()) + 1;
            }
            circle.insert(current_marble_i, marble);
        }
        current_player_i = (current_player_i + 1) % num_players;
        //print_circle(&circle, current_marble_i);
    }
    println!("Winner score {}", players.iter().map(|p| p.score).max().unwrap());
}

fn print_circle(circle: &Vec<i32>, current_marble_i: usize) {
    println!("{:?}", circle.iter().enumerate().map(|(i, v)| 
        if (i == current_marble_i) { format!("({})", v) } else { format!("{}", v) }
    ).collect::<Vec<String>>());
}
