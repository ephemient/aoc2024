use aoc2024::{day1, day2};
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io;
use std::path::Path;

fn get_day_input(day: u8) -> io::Result<String> {
    let datadir = env::var("AOC2024_DATADIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| ".".to_string());
    fs::read_to_string(Path::new(&datadir).join(format!("day{}.txt", day)))
}

fn main() -> io::Result<()> {
    let args = env::args().skip(1).collect::<HashSet<_>>();

    if args.is_empty() || args.contains("1") {
        println!("Day 1");
        let data = get_day_input(1)?;
        println!("{:?}", day1::part1(&data));
        println!("{:?}", day1::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("2") {
        println!("Day 2");
        let data = get_day_input(2)?;
        println!("{:?}", day2::part1(&data));
        println!("{:?}", day2::part2(&data));
        println!();
    }

    Ok(())
}
