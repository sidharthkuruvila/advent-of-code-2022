use std::fs;

const SMALL_INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";



fn test(s: &str, l: usize) -> usize {
    let b = |i: usize| -> usize {
        s.as_bytes()[i] as usize
    };
    let mut board: [u8; 256] = [0; 256];
    for i in 0..(l - 1) as usize {
        board[b(i)] = board[b(i)] + 1;
    }
    let mut count= board.iter().filter (|a| **a > (0 as u8)).count();
    for i in (l - 1) as usize .. {
        if board[b(i)] == 0 {
            count = count + 1;
        }
        if count == l {
            return i;
        }
        board[b(i)]+=1;
        board[b(i-(l-1))]-=1;
        if board[b(i-(l-1))] == 0 {
            count = count - 1;
        }
    }
    panic!("Should never come here!");
}

fn part1(s: &str) {
    let res = test(s, 4);
    println!("Res: {}", res + 1);
}

fn part2(s: &str) {
    let res = test(s, 14);
    println!("Res: {}", res + 1);
}

pub fn day6() {
    let contents = fs::read_to_string("data/day6_input.txt");
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