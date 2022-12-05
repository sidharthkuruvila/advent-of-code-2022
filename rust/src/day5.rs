use std::fs;
use regex::Regex;

const SMALL_INPUT: &str =
"    [D]    \n\
[N] [C]    \n\
[Z] [M] [P]\n\
 1   2   3\n\
\n\
move 1 from 2 to 1\n\
move 3 from 1 to 3\n\
move 2 from 2 to 1\n\
move 1 from 1 to 2\n";

fn parse(s: &str) -> (Vec<Vec<char>>, Vec<(i32, i32, i32)>) {
    let (first, second) = s.split_once("\n\n").unwrap();
    let mut box_lines: Vec<&str> = first.split("\n").collect();
    box_lines.pop();
    let stack_count = (box_lines[0].len() + 1) / 4;

    let mut stacks = vec!();
    for _i in 0..stack_count {
        stacks.push(vec!())
    }

    for line in box_lines {
        for i in (0..stack_count).rev() {
            let ch = line.as_bytes()[i * 4 + 1];
            if ch != 32 {
                stacks[i].push(ch as char)
            }
        }
    }
    let re = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
    let mut steps = vec!();
    for line in second.split("\n").take_while(|x| !x.is_empty()) {
        let g = re.captures(line).unwrap();
        let read_int = |index: usize| -> i32 {
            return g[index].parse().unwrap()
        };
        steps.push((read_int(1), read_int(2) - 1, read_int(3) - 1));
    }

    return (stacks, steps)
}



fn move1(stack: i32, depth: i32, src: i32, dest: i32, count: i32) -> (i32, i32) {
    return if src == stack && count <= depth {
        (stack, depth - count)
    } else if src == stack {
        (dest, count - depth - 1)
    } else if dest == stack {
        (stack, depth + count)
    } else {
        (stack, depth)
    }
}



fn move2(stack: i32, depth: i32, src: i32, dest: i32, count: i32) -> (i32, i32) {
    return if src == stack && count <= depth {
        (stack, depth - count)
    } else if src == stack {
        (dest, depth)
    } else if dest == stack {
        (stack, depth + count)
    } else {
        (stack, depth)
    }
}

fn part1(s: &str) {
    let (stacks, steps) = parse(s);

    let chars: String = (0..(stacks.len() as i32)).map(|mut stack| {
        let mut depth = 0;
        for (count, dest, src) in steps.iter().rev() {
            (stack, depth) = move1(stack, depth, *src, *dest, *count);
        }

        stacks[stack as usize][depth as usize]
    }).collect();
    println!("Res: {}", chars);
}

fn part2(s: &str) {
    let (stacks, steps) = parse(s);

    let chars: String = (0..(stacks.len() as i32)).map(|mut stack| {
        let mut depth = 0;
        for (count, dest, src) in steps.iter().rev() {
            (stack, depth) = move2(stack, depth, *src, *dest, *count);
        }
        stacks[stack as usize][depth as usize]
    }).collect();
    println!("Res: {}", chars);
}

pub fn day5() {
    let contents = fs::read_to_string("data/day5_input.txt");
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
        let (stacks, steps) = parse(SMALL_INPUT);
        let expected_steps = vec!(
            (1, 1, 0),
            (3, 0, 2),
            (2, 1, 0),
            (1, 0, 1)
        );

        let expected_stacks = vec!(
            vec!('N', 'Z'),
            vec!('D', 'C', 'M'),
            vec!('P')
        );

        assert_eq!(steps, expected_steps);
        assert_eq!(stacks, expected_stacks);
    }

}