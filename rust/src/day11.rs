use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::num::ParseIntError;
use regex::{Captures, Regex};

#[derive(PartialEq, Debug)]
enum Operation {
    Add(i64),
    Mul(i64),
    Square,
}

#[derive(PartialEq, Debug)]
struct Monkey {
    id: usize,
    operation: Operation,
    starting_items: Vec<i64>,
    divisibility: i64,
    if_true: usize,
    if_false: usize,
}


fn make_op(op_s: &str, v_s: &str) -> Operation {
    return match (op_s, v_s) {
        ("*", "old") => Operation::Square,
        ("+", n) => Operation::Add(n.parse().unwrap()),
        ("*", n) => Operation::Mul(n.parse().unwrap()),
        _ => panic!()
    };
}

fn make_monkey(captures: &Captures) -> Result<Monkey, Box<dyn Error>> {
    let id = captures.get(1).ok_or("Id not found")?.as_str().parse()?;
    let starting_items = captures.get(2).ok_or("Starting items not found")?.as_str().split(", ")
        .map(|x| x.parse()).collect::<Result<Vec<i64>, ParseIntError>>()?;
    let operation =  make_op(
        captures.get(3).ok_or("operator symbol not found")?.as_str(),
        captures.get(4).ok_or("operation value not found")?.as_str());
    let divisibility  = captures.get(5).ok_or("divisibility not found")?.as_str().parse()?;
    let if_true = captures.get(6).ok_or("if_true not found")?.as_str().parse()?;
    let if_false = captures.get(7).ok_or("if_false not found")?.as_str().parse()?;

    let monkey = Monkey {
        id,
        starting_items,
        operation,
        divisibility,
        if_true,
        if_false,
    };

    Ok(monkey)
}

// fn make_monkey(captures: &Captures) -> Monkey {
//     Monkey {
//         id: captures.get(1).unwrap().as_str().parse().unwrap(),
//         starting_items: captures.get(2).unwrap().as_str().split(", ")
//             .map(|x| x.parse().unwrap()).collect(),
//         operation: make_op(
//             captures.get(3).unwrap().as_str(),
//             captures.get(4).unwrap().as_str()),
//         divisibility: captures.get(5).unwrap().as_str().parse().unwrap(),
//         if_true: captures.get(6).unwrap().as_str().parse().unwrap(),
//         if_false: captures.get(7).unwrap().as_str().parse().unwrap(),
//     }
// }

fn parse(s: &str) -> Vec<Monkey> {
    let re = Regex::new(r"Monkey (\d):
  Starting items: ([\d, ]+)
  Operation: new = old ([*+]) (old|\d+)
  Test: divisible by (\d+)
    If true: throw to monkey (\d)
    If false: throw to monkey (\d)").unwrap();

    return re.captures_iter(s)
        .map(|captures| {
            make_monkey(&captures).unwrap()
        }).collect();
}

fn update_item(monkey: &Monkey, item: i64, reduce: impl Fn(i64) -> i64) -> (usize, i64) {
    let incremented = apply_operation(&monkey.operation, item);
    let reduced: i64 = reduce(incremented);
    if reduced % monkey.divisibility == 0 {
        (monkey.if_true, reduced)
    } else {
        (monkey.if_false, reduced)
    }
}

fn apply_operation(operation: &Operation, item: i64) -> i64 {
    match operation {
        Operation::Add(n) => { item + n }
        Operation::Mul(n) => { item * n}
        Operation::Square => { item * item }
    }
}

fn test(monkeys: Vec<Monkey>, rounds: usize, reduce: impl Fn(i64) -> i64) -> i64 {
    let mut inspection_counts: HashMap<usize, i64> =
        monkeys.iter().map(|monkey| (monkey.id, 0)).collect();
    let mut items:Vec<(usize, i64)> = monkeys.iter()
        .flat_map(|monkey| monkey.starting_items.iter().map(|start_item| (monkey.id, *start_item)))
        .collect();
    for _ in 0..rounds {
        for monkey in &monkeys {
            let (current_items, mut remaining_items): (Vec<(usize, i64)>, Vec<(usize, i64)>) = items.iter().partition(|(monkey_id, _)| *monkey_id == monkey.id);
            inspection_counts.insert(monkey.id, inspection_counts[&monkey.id] + current_items.len() as i64);
            let mut updated_items: Vec<(usize, i64)> = current_items.iter().map(|(_, item)| update_item(&monkey, *item, &reduce)).collect();
            updated_items.append(&mut remaining_items);
            items = updated_items;
        }
    }
    let mut final_counts: Vec<(&usize, &i64)> = inspection_counts.iter().collect();
    final_counts.sort_by(|(_, a), (_, b)| (**b).cmp(*a));

    return final_counts[0..2].iter().map(|(_, a)| **a).reduce(|a, b| a * b).unwrap();
}

fn part1(s: &str) {
    let monkeys = parse(s);
    let res = test(monkeys, 20, |n| n / 3);
    println!("Res: {}", res);
}

fn part2(s: &str) {
    let monkeys = parse(s);
    let ceiling: i64 = monkeys.iter().map(|monkey| monkey.divisibility).reduce(|a, b| a*b).unwrap();
    let res = test(monkeys, 10000, |n| n % ceiling);
    println!("Res: {}", res);

}

pub fn day11() {
    let contents = fs::read_to_string("data/day11_small_input.txt");
    let small_input = contents.unwrap();
    let contents = fs::read_to_string("data/day11_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    part1(small_input.as_str());
    println!("Big Input");
    part1(big_input.as_str());
    println!("Part 2");
    println!("Small Input");
    part2(small_input.as_str());
    println!("Big Input");
    part2(big_input.as_str());
}


#[cfg(test)]
mod tests {
    use super::*;

    const parse_input: &str = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3";

    #[test]
    fn test_parse() {
        let result = parse(parse_input);
        let expected = vec!(
            Monkey {
                id: 0,
                starting_items: vec!(79, 98),
                operation: Operation::Mul(19),
                divisibility: 23,
                if_true: 2,
                if_false: 3,
            }
        );
        assert_eq!(expected, result);
    }
}