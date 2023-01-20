use std::cmp::max;
use std::collections::HashSet;
use std::fs;
use regex::Regex;

const SMALL_INPUT: &str = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

fn parse(s: &str) -> Vec<((isize, isize), (isize, isize))> {
    let mut l: Vec<((isize, isize), (isize, isize))> = vec!();
    let re = Regex::new(r"Sensor at x=([\d-]+), y=([\d-]+): closest beacon is at x=([\d-]+), y=([\d-]+)").unwrap();
    for captures in re.captures_iter(s) {
        let sx = captures.get(1).unwrap().as_str().parse().unwrap();
        let sy = captures.get(2).unwrap().as_str().parse().unwrap();
        let bx = captures.get(3).unwrap().as_str().parse().unwrap();
        let by = captures.get(4).unwrap().as_str().parse().unwrap();
        l.push(((sx, sy), (bx, by)));
    }
    return l;
}

fn manhattan_distance((x1, y1): (isize, isize), (x2, y2): (isize, isize)) -> usize {
    return ((x1 - x2).abs() + (y1 - y2).abs()) as usize;
}

fn range_in_row(row: isize, (bx, by): (isize, isize), distance: usize) -> Option<(isize, isize)> {
    let beacon_distance = (row - by).abs() as usize;
    if beacon_distance > distance {
        return None;
    }
    let available_distance = (distance - beacon_distance) as isize;
    return Some ((bx - available_distance, bx + available_distance));
}

fn scan_row(row: isize, mut beacons: Vec<((isize, isize), (isize, isize))>) -> usize {
    let mut count: usize = 0;
    let mut visited_beacons = HashSet::new();
    beacons.sort_by_key(|(x, _) | *x);
    let mut right = isize::MIN;
    for (sensor, beacon) in beacons {
        let distance= manhattan_distance(sensor, beacon);
        if let Some ((start, end)) = range_in_row(row, sensor, distance)
        {
            if end > right {
                count = count + ((end - max(right + 1, start) + 1) as usize);
                right = end;
            }
            let (bx, by) = beacon;
            if by == row && !visited_beacons.contains(&bx){
                visited_beacons.insert(bx);
                count -= 1;
            }
        }
    }
    return count;
}


fn part_1(s: &str, y: isize) {
    let beacons = parse(s);
    let res = scan_row(y, beacons);
    println!("Res: {}", res);
}

fn find_beacon(mut beacons: Vec<((isize, isize), (isize, isize))>, size: isize) -> (isize, isize) {
    beacons.sort_by_key(|(x, _) | *x);
    for row in 0..(size + 1) {
        let mut right = beacons[0].0.0;
        let mut gap = None;
        for (sensor, beacon) in &beacons {
            let distance= manhattan_distance(*sensor, *beacon);
            if let Some ((start, end)) = range_in_row(row, *sensor, distance) {
                if gap == None && start > right + 1 {
                    gap = Some(right + 1);
                }
                if gap != None && start <= gap.unwrap() {
                    gap = None;
                }
                right = max(end, right);
            }

        }
        if gap.is_some() {
            return (gap.unwrap(), row);
        }
    }
    panic!();
}



fn part_2(s: &str, size: isize) {
    let beacons = parse(s);
    let (x, y) = find_beacon(beacons, size);
    println!("Res: {}", x*4000000 + y);
}

pub fn day15() {

    let contents = fs::read_to_string("data/day15_input.txt");
    let big_input = contents.unwrap();

    println!("Part 1");
    println!("Small Input");
    part_1(SMALL_INPUT, 10);
    println!("Big Input");
    part_1(big_input.trim(), 2000000);
    println!("Part 2");
    println!("Small Input");
    part_2(SMALL_INPUT, 20);
    println!("Big Input");
    part_2(big_input.trim(), 4000000);
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "Sensor at x=9, y=16: closest beacon is at x=10, y=-16";
        let result = parse(input);
        let expected = vec!(((9, 16), (10, -16)));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_manhattan_distance() {
        let a = (10, 15);
        let b = (20, 23);
        let result = manhattan_distance(a, b);
        let expected: usize = 18;
        assert_eq!(result, expected);
    }

    #[test]
    fn test_range_in_row() {
        let beacon = (10, 10);
        let distance = 15;
        let y = 30;
        let expected = None;
        let result = range_in_row(y, beacon, distance);
        assert_eq!(expected, result);

        let y = 25;
        let expected = Some((10, 10));
        let result = range_in_row(y, beacon, distance);
        assert_eq!(expected, result);

        let y = 15;
        let expected = Some((0, 20));
        let result = range_in_row(y, beacon, distance);
        assert_eq!(expected, result)
    }

    #[test]
    fn test_scan_row() {
        let beacons = vec!(
            ((0, 0), 5),
            ((3, 0), 3),
            ((6, 0), 7),
            ((8, 0), 7)
        );
        let y = 3;
        let expected = 15;
        let result = scan_row(y, beacons);
        assert_eq!(expected, result);

    }
}