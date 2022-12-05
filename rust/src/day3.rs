use std::fs;
use std::process::exit;

const SMALL_INPUT: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

fn get_priority(char: char) -> i32 {
    let n = char as i32;
    if 'a' <= char && char <= 'z' {
        return  n - 96
    } else if 'A' <= char && char <= 'Z' {
        return n - 38
    } else {
        eprintln!("Char not in range");
        exit(1);
    }
}

fn as_int(s: &str) -> u64 {
    let mut n: u64 = 0;
    for i in 0..s.len() {
        let c: u64 = 1 << (get_priority(s.as_bytes()[i] as char) - 1);
        n = n | c;

    }
    return n
}

fn parse1(s: &str) ->   Vec<(u64, u64)> {
    let v: Vec<(u64, u64)> =  s.trim().split("\n").into_iter().map(|x| {
        let (a, b) = x.split_at(x.len() / 2);
        (as_int(a), as_int(b))
    }).collect();
    return v;
}

fn bit_pos(n: u64) -> i32 {
    let mut m = n;
    if n == 0 {
        eprintln!("Number should have at least 1 bit");
        exit(1);
    }
    let mut i = 0;
    while m != 1 {
        m = m >> 1;
        i+=1;
    }
    return i
}

fn part1(s: &str) {
    let p = parse1(s);
    let res: i32 = p.iter().map(|(a, b)| {
        let n = a & b;
        bit_pos(n) + 1
    }).sum();
    println!("Res: {}", res)
}

fn parse2(s: &str) -> Vec<u64>{
    return s.trim().split("\n").into_iter().map(|x| as_int(x)).collect()
}

fn part2(s: &str) {
    let p = parse2(s);
    let mut res: i32 = 0;
    for i in 0..(p.len() / 3) {
        let mut acc: u64 = u64::MAX;
        for j in 0..3 {
            acc = acc & p[i*3 + j];
        }
        let pr = bit_pos(acc) + 1;
        res = res + pr;
    }
    println!("Res: {}", res)
}

pub fn day3() {
    let contents = fs::read_to_string("data/day3_input.txt");
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