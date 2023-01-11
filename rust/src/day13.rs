use std::cmp::Ordering;
use std::cmp::Ordering::Equal;
use std::fs;
use std::io::BufRead;
use regex::Regex;
use crate::day13::Tree::Num;

const SMALL_INPUT: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

#[derive(PartialEq, Debug, Clone)]
enum Tree {
    List(Vec<Tree>),
    Num(i32),
}

fn parse_line(s: &str) -> Tree {
    let re = Regex::new(r"(\[)|(])|(\d+)").unwrap();
    let mut stack = vec!(vec!());
    for captures in re.captures_iter(s) {
        if let Some(_) = captures.get(1) {
            stack.push(vec!());
        } else if let Some(_) = captures.get(2) {
            let mut l = stack.pop().unwrap();
            let len = stack.len();
            let mut last = stack.last_mut().unwrap();
            last.push(Tree::List(l));
        } else if let Some(ns) = captures.get(3) {
            let len = stack.len();
            let l = stack.last_mut().unwrap();
            l.push(Tree::Num(ns.as_str().parse().unwrap()));
        } else {
            panic!("Shouldn't come here");
        }
    }
    assert_eq!(stack.len(), 1);
    stack.pop().unwrap().pop().unwrap()
}

fn parse(s: &str) -> Vec<(Tree, Tree)> {
    s.split("\n\n").map(|l| {
        let mut it = l.split("\n").map(|line| parse_line(line));
        let s1 = it.next().unwrap();
        let s2 = it.next().unwrap();
        (s1, s2)
    }).collect()
}

fn compare(t1: &Tree, t2: &Tree) -> Ordering {
    match (t1, t2) {
        (Tree::Num(n1), Tree::Num(n2)) => n1.cmp(n2),
        (Num(n1), _) => compare(&Tree::List(vec!(Num(*n1))), t2),
        (_, Num(n2)) => compare(t1, &Tree::List(vec!(Num(*n2)))),
        (Tree::List(v1), Tree::List(v2)) => {
            v1.iter().zip(v2.iter()).find_map(|(a, b)| {
                let cmp = compare(a, b);
                if cmp == Ordering::Equal {
                    None
                } else {
                    Some(cmp)
                }
            }).unwrap_or(
                if v1.len() > v2.len() {
                    Ordering::Greater
                } else if v1.len() < v2.len() {
                    Ordering::Less
                } else{Ordering::Equal})
        }
    }
}

fn test_1(s: &str) {
    let l = parse(s);
    let res: usize = l.iter().zip(0..(l.len()))
        .filter_map(|((a, b), i)|
            if compare(a, b) == Ordering::Less { Some(i + 1) } else { None }
        )
        .sum();
    println!("Res: {:?}", res);
}

fn test_2(s: &str) {
    let sep_1 = parse_line("[[2]]");
    let sep_2 = parse_line("[[6]]");
    let mut l: Vec<Tree> = parse(s)
        .into_iter()
        .flat_map(|(a, b)| vec!(a, b).into_iter())
        .collect();
    l.push(sep_1.clone());
    l.push(sep_2.clone());
    l.sort_by(compare);

    let idx_1 = l.binary_search_by(|x| compare(&x, &sep_1)).unwrap() + 1;
    let idx_2 = l.binary_search_by(|x| compare(&x, &sep_2)).unwrap() + 1;
    let res = idx_1 * idx_2;
    println!("Res: {:?}", res);
}

pub fn day13() {
    let contents = fs::read_to_string("data/day13_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    test_1(SMALL_INPUT);
    println!("Big Input");
    test_1(big_input.as_str().trim());
    println!("Part 2");
    println!("Small Input");
    test_2(SMALL_INPUT);
    println!("Big Input");
    test_2(big_input.as_str().trim());
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT_LINE: &str = "[123,123]";

    #[test]
    fn test_parse_line() {
        let result = parse_line(INPUT_LINE);
        let expected = Tree::List(vec!(Tree::Num(123), Tree::Num(123)));
        assert_eq!(result, expected);
    }


    #[test]
    fn test_compare() {
        let compare_cases: Vec<(Tree, Tree, Ordering)> = vec!(
            (Tree::List(vec!()), Tree::List(vec!()), Ordering::Equal),
            (Tree::List(vec!(Tree::Num(1))), Tree::List(vec!()), Ordering::Greater),
        );

        for (t1, t2, expected_ordering) in compare_cases {
            println!("left: {:?}, right: {:?}, expected ordering: {:?}", t1, t2, expected_ordering);
            let result = compare(&t1, &t2);
            assert_eq!(result, expected_ordering);
        }
    }
}