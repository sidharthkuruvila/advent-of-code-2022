use std::fs;
use regex::Regex;

const SMALL_INPUT: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

fn parse(s : &str) -> Vec<(i32, i32, i32, i32)> {
    let re = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)$").unwrap();
    s.trim().split("\n").map(|x| {
        let g = re.captures(x).unwrap();
        let read_int = |index: usize| -> i32 {
            return g[index].parse().unwrap()
        };
        (read_int(1), read_int(2), read_int(3), read_int(4))
    }).collect()
}

fn contains(a1: &i32, b1: &i32, a2: &i32, b2: &i32) -> bool {
    (a1 <= a2 && b2 <= b1) || (a2 <= a1 && b1 <= b2)
}

fn overlaps(a1: &i32, b1: &i32, a2: &i32, b2: &i32) -> bool {
    (a1 <= a2 && a2 <= b1) || (a1 <= b2 && b2 <= b1) || contains(a1, b1, a2, b2)
}

fn part1(s: &str) {
    let l = parse(s);
    let res: i32 = l.iter().map(|(a1, b1, a2, b2)| {
        if contains(a1, b1, a2, b2) { 1 } else { 0 }
    }).sum();
    println!("Res: {}", res)
}

fn part2(s: &str) {
    let l = parse(s);
    let res: i32 = l.iter().map(|(a1, b1, a2, b2)| {
        if overlaps(a1, b1, a2, b2) { 1 } else { 0 }
    }).sum();
    println!("Res: {}", res)
}

pub fn day4() {
    let contents = fs::read_to_string("data/day4_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    part1(SMALL_INPUT);
    println!("Big Input");
    part1(big_input.as_str());
    println!("Part 2");
    println!("Small Input");
    part2(SMALL_INPUT);
    println!("Big Input");
    part2(big_input.as_str());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {

        let input = "2-4,6-8\n2-3,4-5\n";
        let expected = vec!(
            (2, 4, 6, 8),
            (2, 3, 4, 5)
        );
        assert_eq!(parse(input), expected);
    }

    #[test]
    fn test_contains() {
        assert!(!contains(&1, &1, &2, &2));
    }
}
