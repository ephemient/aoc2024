use anyhow::anyhow;
use aoc2024::{day1, day10, day11, day2, day3, day4, day5, day6, day7, day8, day9};
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

fn main() -> anyhow::Result<()> {
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

    if args.is_empty() || args.contains("3") {
        println!("Day 3");
        let data = get_day_input(3)?;
        println!("{:?}", day3::part1(&data));
        println!("{:?}", day3::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("4") {
        println!("Day 4");
        let data = get_day_input(4)?;
        println!("{:?}", day4::part1(&data));
        println!("{:?}", day4::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("5") {
        println!("Day 5");
        let data = get_day_input(5)?;
        println!("{:?}", day5::part1(&data)?);
        println!("{:?}", day5::part2(&data)?);
        println!();
    }

    if args.is_empty() || args.contains("6") {
        println!("Day 6");
        let data = get_day_input(6)?;
        println!("{:?}", day6::part1(&data).ok_or(anyhow!("None"))?);
        println!("{:?}", day6::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("7") {
        println!("Day 7");
        let data = get_day_input(7)?;
        println!("{:?}", day7::part1(&data));
        println!("{:?}", day7::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("8") {
        println!("Day 8");
        let data = get_day_input(8)?;
        println!("{:?}", day8::part1(&data));
        println!("{:?}", day8::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("9") {
        println!("Day 9");
        let data = get_day_input(9)?;
        println!("{:?}", day9::part1(&data));
        println!("{:?}", day9::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("10") {
        println!("Day 10");
        let data = get_day_input(10)?;
        println!("{:?}", day10::part1(&data));
        println!("{:?}", day10::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("11") {
        println!("Day 11");
        let data = get_day_input(11)?;
        println!("{:?}", day11::part1(&data)?);
        println!("{:?}", day11::part2(&data)?);
        println!();
    }

    Ok(())
}
