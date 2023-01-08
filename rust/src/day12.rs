use std::collections::{HashMap, HashSet};
use std::fs;

const SMALL_INPUT: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";


fn index (width: usize, (x, y): (usize, usize)) -> usize {
    return  y * (width + 1) + x;
}

fn get(s: &str, i: usize) -> usize {
    let a = s.as_bytes()[i] as usize;
    return if a == 'S' as usize  {
        'a' as usize - 97
    } else if a == 'E' as usize {
        'z' as usize - 97
    } else {
        a - 97
    }
}

fn make_adj_list(s: &str) -> (usize, usize, Vec<(usize, usize)>) {
    let width = s.find("\n").unwrap();
    let height = (s.len() + 1) / (width + 1);
    let index = |x: usize, y: usize| -> usize {
        index(width, (x, y))
    };
    let get = |i: usize| -> usize {
        get(s, i)
    };

    let start_i = s.find("S").unwrap();
    let end_i = s.find("E").unwrap();

    let mut adj_list: Vec<(usize, usize)> = vec!();
    for i in 0..width {
        for j in 0..height {
            let idx = index(i, j);
            let a = get(idx);
            let neighbours:Vec<(isize, isize)> = vec!((1, 0), (-1, 0), (0, -1), (0, 1));
            neighbours.iter().for_each(|(di, dj)| {
                if let Some(x) = i.checked_add_signed(*di)
                    .filter(|x| *x < width){
                    if let Some(y) = j.checked_add_signed(*dj)
                        .filter(|y| *y < height){
                        let next_idx = index(x, y);
                        let b = get(next_idx);
                        if a + 1 == b || a >= b {
                            adj_list.push ((idx, next_idx))
                        }
                    }
                }
            })
        }
    }
    return (start_i, end_i, adj_list);
}

fn find_shortest_path(start: usize, end:usize, adj_list: Vec<(usize, usize)>) -> Option<usize> {
    let mut adj_map: HashMap<usize, Vec<usize>> = HashMap::new();
    for (a, b) in adj_list {
        if !adj_map.contains_key(&a) {
            adj_map.insert(a, vec!());
        }
        adj_map.get_mut(&a).unwrap().push(b);
    }
    let mut frontier: HashSet<usize> = HashSet::from_iter(vec!(start));
    let mut visited: HashSet<usize> = HashSet::from_iter(frontier.clone());
    let mut depth = 0;
    while !(frontier.iter().any(|a| *a == end)) {
        if frontier.len() == 0 {
            return None;
        }
        frontier = frontier.iter()
            .flat_map(|a| adj_map.get(a).map(|x| x.clone()).unwrap_or(vec![]).into_iter())
            .filter(|a| !visited.contains(a))
            .collect();
        visited.extend(frontier.clone());
        depth+=1;
    }
    return Some(depth)
}

fn test_1(s: &str) {
    let (start, end, adj_list) = make_adj_list(s);
    let res = find_shortest_path(start, end, adj_list).unwrap();
    println!("Res: {}", res);
}

fn test_2(s: &str) {
    let (_, end, adj_list) = make_adj_list(s);
    let candidates: Vec<usize> = s.as_bytes().iter().zip(0..s.len())
        .filter_map(|(a, b)| if *a == ('a' as u8) { Some (b.clone())} else { None }).collect();
    let res = candidates.iter().flat_map(|start| find_shortest_path(*start, end, adj_list.clone()))
        .min().unwrap();
    println!("Res: {}", res);
}

pub fn day12() {
    let contents = fs::read_to_string("data/day12_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small input");
    test_1(SMALL_INPUT);
    println!("Big input");
    test_1(big_input.as_str());
    println!("Part 2");
    println!("Small input");
    test_2(SMALL_INPUT);
    println!("Big input");
    test_2(big_input.as_str());
}



#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "Sabcdef
mlkjihg
nopqrst
Ezyxwvu";

    #[test]
    fn test_make_adj_list() {
        let result = make_adj_list(INPUT);
        let expected = (0, 24,
                        vec!((12, 11), (12, 0), (12, 13), (13, 14), (13, 12),
                             (0, 1), (11, 10), (11, 12), (11, 0), (14, 15),
                             (14, 13), (14, 11), (1, 2), (1, 0), (10, 9),
                             (10, 11), (10, 1), (15, 16), (15, 14),
                             (15, 10), (2, 3), (2, 1), (9, 8), (9, 10),
                             (9, 2), (16, 17), (16, 15), (16, 9), (3, 4),
                             (3, 2), (8, 7), (8, 9), (8, 3), (17, 18),
                             (17, 16), (17, 8), (4, 5), (4, 3), (7, 6),
                             (7, 8), (7, 4), (18, 19), (18, 17), (18, 7),
                             (5, 4), (5, 6), (6, 7), (6, 5), (19, 18), (19, 6)));
        assert_eq!(expected, result);
    }

}