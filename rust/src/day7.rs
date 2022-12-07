use std::fs;
use regex::Regex;

static SMALL_INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

#[derive(PartialEq, Debug)]
enum Command {
    Up(String),
    Down,
    Add(i32)
}

struct BottomlessStack<T: 'static>
{
    vec: Vec<T>,
    make_zero: Box<dyn Fn() -> T>,
}

impl<T: 'static> BottomlessStack<T>
{
    fn new(make_zero: impl Fn() -> T + 'static) -> Self {
        Self {
            vec: Vec::new(),
            make_zero: Box::new(make_zero),
        }
    }

    fn push(&mut self, v: T) {
        self.vec.push(v);
    }

    fn pop(&mut self) -> T {
        if self.vec.is_empty() {
            (self.make_zero)()
        } else {
            self.vec.pop().unwrap()
        }
    }

}

fn parse(s: &str) -> Vec<Command> {
    let re = Regex::new(r"^(\d+) .+$|^([$] cd [.][.])$|^[$] cd (.+)$").unwrap();
    let lines: Vec<&str> = s.split("\n").collect();
    let mut res: Vec<Command> = lines.iter().filter_map(|line|{
        return if let Some(capture) = re.captures(line) {
            if let Some(digits) = capture.get(1) {
                Some(Command::Add(digits.as_str().parse().unwrap()))
            } else if let Some(_) = capture.get(2) {
                Some(Command::Down)
            } else if let Some(name) = capture.get(3) {
                Some(Command::Up(name.as_str().to_string()))
            } else {
                None
            }
        } else {
            None
        }
    }).collect();
    res.reverse();
    res
}

fn walk(commands: &Vec<Command>, mut cb: impl FnMut(&str, i32)) -> i32 {
    let mut stack: BottomlessStack<i32> = BottomlessStack::new(|| 0);
    for command in commands {
        match command {
            Command::Add(file_size) => {
                let accumulated_current_dir_size = stack.pop();
                stack.push(accumulated_current_dir_size + file_size)
            },
            Command::Up(s) => {
                let current_dir_size = stack.pop();
                let accumulated_parent_dir_size = stack.pop();
                cb(s.as_str(), current_dir_size);
                stack.push(current_dir_size + accumulated_parent_dir_size);
            }
            Command::Down => {
                let initial_current_dir_size= 0;
                stack.push(initial_current_dir_size)
            }
        }
    }
    stack.pop()
}

fn part1(s: &str) {
    let commands = parse(s);
    let mut sum: i32 = 0;
    walk(&commands,  |_, n | {
        if n < 100000 {
            sum += n;
        }
    });
    println!("Res: {}", sum);
}

fn part2(s: &str) {
    let commands = parse(s);
    let mut candidate: i32 = i32::MAX;
    let total_size = walk(&commands,  |_, _ | ());
    let required_size = total_size - 40000000;
    walk(&commands, |_, n| {
        if n > required_size && n < candidate{
            candidate = n
        }
    });
    println!("Res: {}", candidate);
}

pub fn day7() {
    let contents = fs::read_to_string("data/day7_input.txt");
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
    const TEST_INPUT: &str = "$ ls
dir a
14848514 b.txt
$ cd a.b
$ cd ..";
    #[test]
    fn test_parse() {
        let expected_res = vec!(
            Command::Add(14848514),
            Command::Up("a.b".to_string()),
            Command::Down
        );
        let res = parse(TEST_INPUT);
        assert_eq!(res, expected_res);
    }


    #[test]
    fn test_bottomless_stack() {
        let mut stack = BottomlessStack::new(|| 0);
        stack.push(1);
        let v1 = stack.pop();
        assert_eq!(v1, 1);
        let v2 = stack.pop();
        assert_eq!(v2, 0);
    }
}