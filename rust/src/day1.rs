use std::fs;

static SMALL_INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

fn parse(s: &str) -> Vec<Vec<i32>> {
    let trimmed = s.trim();
    let v: Vec<_> = trimmed.split("\n\n")
        .map(|x| x.split("\n").map(|x| x.parse::<i32>().unwrap()).collect())
        .collect();
    return v;
}

fn find_largest_score(s: Vec<Vec<i32>>) -> i32 {
    return s.iter().map(|x| x.iter().sum()).max().unwrap_or(-1);
}

fn part1(input: &str) {
    let p = parse(input);
    let v = find_largest_score(p);
    println!("Res: {}", v);
}

fn find_top_3_scores(s: Vec<Vec<i32>>) -> i32 {
    let mut scores: Vec<i32> = s.iter().map(|x| x.iter().sum()).collect();
    scores.sort_by(|a , b| b.cmp(a));
    return scores.iter().take(3).sum();
}

fn part2(input: &str) {
    let p = parse(input);
    let v = find_top_3_scores(p);
    println!("Res: {}", v);
}

pub fn day1() {
    let contents = fs::read_to_string("data/day1_input.txt");
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