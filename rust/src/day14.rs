use std::borrow::BorrowMut;
use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs;

const SMALL_INPUT: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";


fn connect((x1, y1): (usize, usize), (x2, y2): (usize, usize)) -> Vec<(usize, usize)> {
    assert!(x1 == x2 || y1 == y2);
    return if x1 == x2 {
        let start: usize = min(y1, y2);
        let end: usize = (max(y1, y2) + 1);
        (start .. end).map(|y| (x1, y)).collect()
    } else {
        let start: usize = min(x1, x2);
        let end: usize = (max(x1, x2) + 1);
        (start .. end).map(|x| (x, y1)).collect()
    }
}

fn parse_pair(s: &str) -> (usize, usize) {
    let mut it = s.split(",");
    let x: usize = it.next().unwrap().parse().unwrap();
    let y: usize = it.next().unwrap().parse().unwrap();
    (x, y)
}

fn parse_line(line: &str) -> Vec<(usize, usize)> {
    let parts: Vec<(usize, usize)> = line.split(" -> ")
        .map(parse_pair).collect();
    let l1 = parts.clone();
    let l2 = parts.clone();
    let it1 = l1.iter();
    let mut it2 = l2.iter();
    it2.next();
    it1.zip(it2).flat_map(|(p1, p2)| {
        connect(*p1, *p2)
    }).collect::<Vec<(usize, usize)>>()
}

fn parse(s: &str) -> HashSet<(usize, usize)> {
    s.trim().split("\n").flat_map(|line| {
        parse_line(line)
    }).collect()
}


fn can_place((x, y): (usize, usize), bottom: usize, occupied: &HashSet<(usize, usize)>) -> bool {
    if y == bottom {
        return false
    }
    if occupied.contains(&(x, y)) {
        return false;
    }
    return true
}


fn drop_1((x, y): (usize, usize), bottom: usize, occupieds: &HashSet<(usize, usize)>) ->  Option<(usize, usize)> {
    let possible_locations = vec!(0, -1, 1);
    let new_y = y + 1;
    for dx in possible_locations {
        let new_x = x.checked_add_signed(dx).unwrap();
        if can_place((new_x, new_y), bottom, occupieds) {
            return Some((new_x, new_y))
        }
    }
    return None
}

fn drop(bottom: usize, occupieds: &HashSet<(usize, usize)>) -> (usize, usize) {
    let mut position = (500, 0);
    loop {
        match drop_1(position, bottom, occupieds) {
            None => return position,
            Some(new_position) => position = new_position
        }
    }
}

fn fill(bottom: usize, mut occupieds: HashSet<(usize, usize)>) -> usize {
    let mut count = 0;

    loop {
        let position = drop(bottom, &occupieds);
        let (x, y) = position;
        if y == (bottom - 1) {
            return count;
        } else {
            occupieds.insert(position);
            count +=1
        }
    }
}

fn part_1(s: &str) {
    let mut occupieds = parse(s);
    let bottom = occupieds.iter().map(|(x, y)| y).max().unwrap() + 2;
    let ov: Vec<(usize, usize)> = occupieds.clone().into_iter().collect();
    let count = fill(bottom, occupieds);
    println!("Res: {}", count);
}

pub fn day14() {
    let contents = fs::read_to_string("data/day14_input.txt");
    let big_input = contents.unwrap();
    println!("Part 2");
    println!("Small Input");
    part_1(SMALL_INPUT);
    println!("Big Input");
    part_1(big_input.trim());
}

#[cfg(test)]
mod tests {
    use super::*;

    const LINE: &str = "498,4 -> 498,6 -> 496,6";

    #[test]
    fn test_connect_horizontal() {
        let expected = vec!((1, 1), (2, 1), (3, 1));
        let result = connect((1, 1), (3, 1));
        assert_eq!(expected, result);
    }

    #[test]
    fn test_connect_vertical() {
        let expected = vec!((1, 1), (1, 2), (1, 3));
        let result = connect((1, 1), (1, 3));
        assert_eq!(expected, result);
    }

    #[test]
    fn test_parse_line() {
        let result = parse_line(LINE);
        let expected = vec!((498, 4), (498, 5), (498, 6), (496, 6), (497, 6), (498, 6));
        assert_eq!(expected, result);
    }

    #[test]
    fn test_can_place() {
        let occupieds: HashSet<(usize, usize)> = vec!((0, 0)).into_iter().collect();
        assert!(!can_place((0, 0), 2, &occupieds));
        assert!(can_place((1, 1), 2, &occupieds));
        assert!(!can_place((1, 2), 2, &occupieds));
    }

    #[test]
    fn test_drop_1() {
        let occupieds: HashSet<(usize, usize)> = vec!().into_iter().collect();
        assert_eq!(Some((1, 1)), drop_1((1, 0), 2, &occupieds));
        let occupieds: HashSet<(usize, usize)> = vec!((1, 1)).into_iter().collect();
        assert_eq!(Some((0, 1)), drop_1((1, 0), 2, &occupieds));
        let occupieds: HashSet<(usize, usize)> = vec!((0, 1), (1, 1)).into_iter().collect();
        assert_eq!(Some((2, 1)), drop_1((1, 0), 2, &occupieds));
        let occupieds: HashSet<(usize, usize)> = vec!().into_iter().collect();
        assert_eq!(None, drop_1((1, 1), 2, &occupieds));
    }

    #[test]
    fn test_drop() {
        let occupieds: HashSet<(usize, usize)> = vec!().into_iter().collect();
        assert_eq!((500, 9), drop( 10, &occupieds));
    }
}
