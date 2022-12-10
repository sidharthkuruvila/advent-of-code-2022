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

fn dimensions(forrest: &Vec<Vec<i32>>) -> (usize, usize) {
    let width = forrest[0].len();
    let height = forrest.len();
    return (width, height);
}

fn get(forrest: &Vec<Vec<i32>>, x:usize, y:usize) -> i32 {
    return forrest[y][x];
}

fn make_directions(forrest: &Vec<Vec<i32>>, x:usize, y: usize) -> Vec<Vec<i32>> {
    let forrest_length = forrest.len();
    let forrest_breadth = forrest[0].len();

    let directions: Vec<Vec<i32>> = vec!(
        (0..x).rev().map(|i| get(forrest,i, y)).collect(),
        (0..y).rev().map(|i| get(forrest, x, i)).collect(),
        ((y+1)..forrest_length).map(|i| get(forrest, x, i)).collect(),
        ((x+1)..forrest_breadth).map(|i| get(forrest, i, y)).collect(),
    );
    return directions;
}

fn is_visible(forrest: &Vec<Vec<i32>>, x:usize, y: usize) -> bool {
    let tree_height = get(forrest, x, y);
    let directions = make_directions(forrest, x, y);
    return directions.iter().map(|direction| direction.iter()
        .all(|height| *height < tree_height ))
        .any(|b| b);
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

fn calculate_score(forrest: &Vec<Vec<i32>>, x: usize, y: usize) -> i32 {
    let tree_height = get(forrest, x, y);
    let directions = make_directions(forrest, x, y);
    return directions.iter()
        .map(|direction| direction[0..direction.len() - 1].iter()
            .take_while(|height| **height < tree_height).count() as i32 + 1)
        .fold(1, |acc, v|  (acc  * v))

}

fn part2(s: &str) {
    let forrest = parse(s);
    let candidates = get_candidates(&forrest);

    let max_score = candidates.iter().map(|(x, y)| calculate_score(&forrest, *x, *y)).max();
    println!("Res: {}", max_score.unwrap());
}

pub fn day8(){
    let contents = fs::read_to_string("data/day8_input.txt");
    let big_input = contents.unwrap();
    println!("Part 1");
    println!("Small Input");
    part1(SMALL_INPUT);
    println!("Big Input");
    part1(big_input.as_str().trim());
    println!("Part 2");
    println!("Small Input");
    part2(SMALL_INPUT);
    println!("Big Input");
    part2(big_input.as_str().trim());
}