use std::fs;
use std::process::exit;

const SMALL_INPUT: &str = "A Y
B X
C Z";

#[derive(Debug)]
enum AType {
    A, B, C
}

#[derive(Debug)]
enum XType {
    X, Y, Z
}

fn read_x(input: &str) -> XType{
    match input {
        "X" => XType::X,
        "Y" => XType::Y,
        "Z" => XType::Z,
        _ => {
            print!("Not valid X Type: {}", input);
            exit(0)
        }
    }
}

fn read_a(input: &str) -> AType{
    match input {
        "A" => AType::A,
        "B" => AType::B,
        "C" => AType::C,
        _ => {
            print!("Not valid X Type: {}", input);
            exit(0)
        }
    }
}

fn parse(input: &str) -> Vec<(AType, XType)> {
    input.trim().split("\n").map(|x| {
        let parts:Vec<&str> = x.split(" ").collect();
        let a = read_a(parts[0]);
        let x = read_x(parts[1]);
        (a, x)
    }).collect()
}

fn get_play_score(a: &AType, x: &XType) -> i32 {
    match (a, x) {
        (AType::A, XType::X) | (AType::B, XType::Y) | (AType::C, XType::Z) => 3,
        (AType::A, XType::Y) | (AType::B, XType::Z) | (AType::C, XType::X) => 6,
        (AType::A, XType::Z) | (AType::B, XType::X) | (AType::C, XType::Y) => 0,
    }
}

fn get_move_score(x: &XType) -> i32 {
    match x {
        XType::X => 1,
        XType::Y => 2,
        XType::Z => 3
    }
}

fn get_score(a: &AType, x: &XType) -> i32 {
    let play_score = get_play_score(a, x);
    let move_score = get_move_score(x);
    play_score + move_score
}

fn get_scores(s: &Vec<(AType, XType)>) -> i32 {
    s.iter().map(|(a, x)| {
        get_score(a, x)
    }).sum()
}

fn part1(input: &str) {
    let p = parse(input);
    let scores = get_scores(&p);
    println!("Res: {}", scores);
}

fn a_to_int(a: &AType) -> i32 {
    match a {
        AType::A => 0,
        AType::B => 1,
        AType::C => 2
    }
}


fn a_from_int(a: i32) -> AType {
    match a {
        0 => AType::A,
        1 => AType::B,
        2 => AType::C,
        _ => {
            eprintln!("The input needs to be between 0 and 2");
            exit(1)
        }
    }
}

fn a_ref_to_value(a: &AType) -> AType {
    match a {
        AType::A => AType::A,
        AType::B => AType::B,
        AType::C => AType::C
    }
}

fn winning_move(their_move: &AType) -> AType {
    let i = a_to_int(their_move);
    a_from_int((i + 1) % 3)
}

fn losing_move(their_move: &AType) -> AType {
    let i = a_to_int(their_move);
    a_from_int((i + 2) % 3)
}

fn get_my_move(their_move: &AType, expected_result: &XType) -> AType {
    match expected_result {
        XType::X => losing_move(their_move),
        XType::Y => a_ref_to_value(their_move),
        XType::Z => winning_move(their_move),
    }
}

fn get_play_score2(result: &XType) -> i32 {
    match result {
        XType::X => 0,
        XType::Y => 3,
        XType::Z => 6,
    }
}

fn get_move_score2(x: &AType) -> i32 {
    match x {
        AType::A => 1,
        AType::B => 2,
        AType::C => 3
    }
}


fn get_scores2(s: &Vec<(AType, XType)>) -> i32 {
    s.iter().map(|(their_move, expected_result)| {
        let my_move = get_my_move(their_move, expected_result);
        get_move_score2(&my_move) + get_play_score2(expected_result)
    }).sum()
}


fn part2(input: &str) {
    let p = parse(input);
    let scores = get_scores2(&p);
    println!("Res: {}", scores);
}

pub fn day2() {
    let contents = fs::read_to_string("data/day2_input.txt");
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