use std::fs;

static SMALL_INPUT: &str = "30373
25512
65332
33549
3539";

fn parse(s: &str) -> Vec<Vec<i32>> {
    s.split("\n")
        .map(|line|
            line
            .as_bytes()
                .iter()
                .map(|b| *b as i32).collect())
        .collect()
}


const DIRECTIONS: [(i32, i32); 4] = [
    (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0)
];

fn dimensions(forrest: &Vec<Vec<i32>>) -> (usize, usize) {
    let width = forrest[0].len();
    let height = forrest.len();
    return (width, height);
}

fn within_range(forrest: &Vec<Vec<i32>>, x:i32, y:i32) -> bool {
    let (width, height) = dimensions(forrest);
    return 0 <= x && x < (width as i32) &&  0 <= y && y < (height as i32)
}

fn get(forrest: &Vec<Vec<i32>>, x:usize, y:usize) -> i32 {
    return forrest[y][x];
}

fn is_visible(forrest: &Vec<Vec<i32>>, mut x:usize, mut y: usize) -> bool {
    println!("Aboud to candidate it x:{}, y:{}", x, y);
    let height = get(forrest, x, y);
    for (dx, dy) in DIRECTIONS {
        while within_range(forrest, tx, ty) {
            println!("dx:{}, dy:{}", dx, dy);
            let tx = (x as i32) + dx;
            let ty = (y as i32) + dy;
            x = tx as usize;
            y = ty as usize;
            if get(forrest, x, y) >= height {
                return false
            }
        }
    }
    return true
}

fn get_candidates(forrest: &Vec<Vec<i32>>) -> Vec<(usize, usize)> {
    let (width, height) = dimensions(forrest);
    return (1..(height - 1)).flat_map(|y| (1..(width - 1)).map(move |x| (x, y))).collect()
}

fn part1(s: &str) {
    let forrest = parse(s);
    let (width, height) =dimensions(&forrest);
    let candidates = get_candidates(&forrest);
    let inside_visibles = candidates.iter().filter(|(x, y)| is_visible(&forrest, *x, *y)).count();

    let total_visibles = inside_visibles + 2*(width + height) - 4;
    println!("Res: {}", total_visibles);
}

pub fn day8(){
    let contents = fs::read_to_string("data/day8_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    part1(SMALL_INPUT);
    println!("Big Input");
    part1(big_input.as_str());
    // println!("Part 2");
    // println!("Small Input");
    // part2(SMALL_INPUT);
    // println!("Big Input");
    // part2(big_input.as_str());
}