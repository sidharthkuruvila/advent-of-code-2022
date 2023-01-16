extern crate core;

use std::env;

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;

fn main() {
    let _args: Vec<String> = env::args().collect();
    match "14" {
        "1" => {
            println!("\nDay1:");
            day1::day1();
        }
        "2" => {
            println!("\nDay2:");
            day2::day2();
        }
        "3" => {
            println!("\nDay3:");
            day3::day3();
        }
        "4" => {
            println!("\nDay4:");
            day4::day4();
        }
        "5" => {
            println!("\nDay5:");
            day5::day5();
        }

        "6" => {
            println!("\nDay6:");
            day6::day6();
        }
        "7" => {
            println!("\nDay7:");
            day7::day7();
        }
        "8" => {
            println!("\nDay8:");
            day8::day8();
        }
        "9" => {
            println!("\nDay9:");
            day9::day9();
        }
        "10" => {
            println!("\nDay10:");
            day10::day10();
        }
        "11" => {
            println!("\nDay11:");
            day11::day11();
        }
        "12" => {
            println!("\nDay12:");
            day12::day12();
        }
        "13" => {
            println!("\nDay13:");
            day13::day13();
        }
        "14" => {
            println!("\nDay14:");
            day14::day14();
        }
        _ => panic!()
    }
}
