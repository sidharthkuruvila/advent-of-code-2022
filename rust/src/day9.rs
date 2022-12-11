use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs;

const SMALL_INPUT: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

fn parse(s: &str) -> Vec<(i32, i32)> {
    return s.split("\n").flat_map(|line| {
        let parts: Vec <&str> =line.split(" ").collect();
        let distance:usize = parts[1].parse().unwrap();
        let diff = match parts[0] {
            "U" => (0, 1),
            "L" => (-1, 0),
            "D" => (0, -1),
            "R" => (1, 0),
            _ => panic!("Don't have any more commands")
        };
        return vec![diff; distance].into_iter();
    }).collect()
}


//Draw board is buggy I didn't get time to fix it.
fn draw_board(head_position: (i32, i32), knots: &Vec<(i32, i32)>) {
    let mut knots = knots.clone();

    let min_x = min(0 ,min(*knots.iter().map(|(x, _)| x).min().unwrap(), head_position.0));
    let max_x = max(0, max(*knots.iter().map(|(x, _)| x).min().unwrap(), head_position.0));
    let min_y = min(0, min(*knots.iter().map(|(_, y)| y).min().unwrap(), head_position.1));
    let max_y = max(0, max(*knots.iter().map(|(_, y)| y).min().unwrap(), head_position.1));

    let height = ((max_y - min_y) as usize) + 3;
    let width = ((max_x - min_x) as usize) + 3;
    let mut grid = vec![vec!['.'; width]; height];

    let mut add_to_grid = |(x, y): (i32, i32), ch: char| {
        let grid_x = ((x - min_x) as usize) + 1;
        let grid_y = ((max_y - y) as usize) + 1;
        // It appears that grid_x and grid_y can be greater than the width or the height.
        assert!(grid_x < width);
        assert!(grid_y < height);
        grid[grid_y][grid_x] = ch
    };
    add_to_grid((0, 0), 's');

    for i in (0..knots.len()).rev() {
        add_to_grid(knots[i], char::from_u32((49 + i) as u32).unwrap());
    }
    add_to_grid(head_position, 'H');

    for row in grid {
        for ch in row {
            print!("{}", ch);
        }
        println!();
    }
}

fn add((p1x, p1y): (i32, i32), (p2x, p2y): (i32, i32)) -> (i32, i32) {
    return (p1x + p2x, p1y + p2y);
}

fn subtract((p1x, p1y): (i32, i32), (p2x, p2y): (i32, i32)) -> (i32, i32) {
    return (p1x - p2x, p1y - p2y);
}

fn scalar_div((px, py): (i32, i32), scalar: i32) -> (i32, i32) {
    return (px / scalar, py / scalar)
}

fn is_adjacent((p1x, p1y): (i32, i32), (p2x, p2y): (i32, i32)) -> bool {
    return (p1x - p2x).abs() <= 1 && (p1y - p2y).abs() <= 1;
}

fn move_knot(knot: (i32, i32), head_position: (i32, i32)) -> (i32, i32) {
    let res = if is_adjacent(knot, head_position) {
        knot
    } else {
        let difference = subtract(head_position, knot);
        let (difference_x, difference_y) = difference;
        if difference_x.abs() == difference_y.abs() {
            let scalar = difference_x.abs();
            let magnitude = scalar_div(difference, scalar);
            let knot_position_difference = subtract(difference, magnitude);
            let updated_knot_position = add(knot, knot_position_difference);
            // println!("difference: {:?},  scalar: {:?}, magnitude: {:?}, knot_position_difference: {:?}, updated_knot_position: {:?}",
            //        difference, scalar, magnitude, knot_position_difference, updated_knot_position
            // );
            updated_knot_position
        } else if difference_x.abs() > difference_y.abs() {
            let a = (difference_x, 0);
            let scalar = difference_x.abs();
            let magnitude = scalar_div(a, scalar);
            let res = subtract(head_position, magnitude);

            // println!("x --> a: {:?},  scalar: {:?}, magnitude: {:?}, res: {:?}",
            //        a, scalar, magnitude, res
            // );
            res
        } else {
            let a = (0, difference_y);
            let scalar = difference_y.abs();
            let magnitude = scalar_div(a, scalar);
            let res = subtract(head_position, magnitude);

            // println!("x --> a: {:?},  scalar: {:?}, magnitude: {:?}, res: {:?}",
            //          a, scalar, magnitude, res
            // );
            res
        }
    };
    assert!(is_adjacent(res, head_position));
    return res;
}

fn run(commands: Vec<(i32, i32)>, rope_length: usize) -> usize {
    let mut rope = vec![(0, 0); rope_length];
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut head_position = (0, 0);
    visited.insert((0, 0));
    for command in commands {
        let mut new_rope = vec!();
        head_position = add(head_position, command);
        let mut previous_position = head_position;
        for knot in rope {
            previous_position = move_knot(knot, previous_position);
            new_rope.push(previous_position);
        }

        visited.insert(previous_position);
        rope = new_rope;
        // draw_board(head_position, &rope);
    }
    return visited.len()
}

fn part1(s: &str) {
    let commands = parse(s);
    let res = run(commands, 1);
    println!("Res: {}", res);
}

fn part2(s: &str) {
    let commands = parse(s);
    let res = run(commands, 9);
    println!("Res: {}", res);
}

pub fn day9() {
    let contents = fs::read_to_string("data/day9_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    part1(SMALL_INPUT);
    println!("Big Input");
    part1(big_input.as_str().trim());
    println!("Part 2");
    println!("Small Input");
    part2(SMALL_INPUT);
    println!("Big Input");
    part2(big_input.as_str().trim());
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "U 4
L 4
D 3
R 1";

    #[test]
    fn test_parse() {
        let expected_res = vec!(
            (0, 1), (0, 1), (0, 1), (0, 1), (-1, 0), (-1, 0), (-1, 0), (-1, 0), (0, -1), (0, -1), (0, -1), (1, 0)
        );
        let res = parse(TEST_INPUT);
        assert_eq!(res, expected_res);
    }

    fn move_a_knot(knot: (i32, i32), head_position: (i32, i32), expected: (i32, i32)) {
        let res = move_knot(knot, head_position);
        assert_eq!(res, expected);
    }

    #[test]
    fn test_move_knot() {

        // Adjacent positions
        move_a_knot((0, 0), (0, 0), (0, 0));
        move_a_knot((0, 0), (1, 1), (0, 0));
        move_a_knot((0, 0), (1, -1), (0, 0));
        move_a_knot((0, 0), (-1, -1), (0, 0));
        move_a_knot((0, 0), (-1, -1), (0, 0));
        move_a_knot((0, 0), (0, 1), (0, 0));
        move_a_knot((0, 0), (1, 0), (0, 0));
        move_a_knot((0, 0), (-1, 0), (0, 0));
        move_a_knot((0, 0), (0, -1), (0, 0));

        move_a_knot((0, 0), (2, 2), (1, 1));
        move_a_knot((0, 0), (2, -2), (1, -1));
        move_a_knot((0, 0), (-2, -2), (-1, -1));
        move_a_knot((0, 0), (2, -2), (1, -1));

        move_a_knot((0, 0), (1, 3), (1, 2));

        move_a_knot((1, 0), (3, 0), (2, 0));
    }

}