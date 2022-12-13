use std::fs;
use crate::day10::Instr::{Addx, Noop};

#[derive(PartialEq, Debug)]
enum Instr {
    Noop,
    Addx(i32)
}

fn parse_instr(s: &str) -> Instr {
    let parts: Vec<&str> = s.split(" ").collect();
    return match parts.as_slice() {
        ["noop"] => Noop,
        ["addx", ns] => Addx(ns.parse().unwrap()),
        _ => panic!()
    }
}

fn parse(s: &str) -> Vec<Instr> {
    s.trim().split("\n")
        .map(|line| parse_instr(line))
        .collect()
}

fn run(instrs: &Vec<Instr>) -> Vec<i32> {
    let mut acc = 1;
    let mut cycles = vec!();
    cycles.push(1);
    for instr in instrs {
        match instr {
            Noop => {
                cycles.push(acc);
            },
            Addx(n) => {
                cycles.push(acc);
                acc = acc + n;
                cycles.push(acc);
            }
        }
    }
    return cycles;
}

fn part1(s: &str) {
    let instrs = parse(s.trim());
    let cycles = run(&instrs);
    let res: i32 = (0..6).map(|n| {
        let cycle = n*40 + 20;
        let strength = cycles[cycle - 1];
        (cycle as i32) * strength

    }).sum();
    println!("Res: {}", res);
}

fn part2(s: &str) {
    let instrs = parse(s.trim());
    let cycles = run(&instrs);
    println!("\nRes:");
    for row in 0..6 {
        for col in 0..40 {
            let cycle = row * 40 + col;
            let strength = cycles[cycle];
            let n = cycle as i32 % 40;
            if n-1 <= strength && strength <= n+1  {
                print!("#");
            }  else {
                print!(".");
            }
        }
        println!();
    }
    println!("\n")
}

pub fn day10() {
    let small_contents = fs::read_to_string("data/day10_small_input.txt");
    let small_input = small_contents.unwrap();
    let big_contents = fs::read_to_string("data/day10_input.txt");
    let big_input = big_contents.unwrap();
    println!("Part 1");
    part1(small_input.as_str());
    part1(big_input.as_str());
    println!("Part 2");
    part2(small_input.as_str());
    part2(big_input.as_str());
}


#[cfg(test)]
mod tests {
    use super::*;

    const test_input: &str = "noop\naddx 10\naddx -1\n";
    #[test]
    fn test_parse() {
        let result = parse(test_input);
        let expected = vec!(
            Noop,
            Addx(10),
            Addx(-1)
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn test_run() {
        let input1 = vec!(Noop);
        let result = run(&input1);
        let expected = vec!(1);

        let input2 = vec!(Addx(3));
        let result = run(&input2);
        let expected = vec!(1, 4);
    }
}