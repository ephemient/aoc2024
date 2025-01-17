use anyhow::anyhow;
use aoc2024::{
    day1, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day2, day20, day21,
    day22, day23, day24, day25, day3, day4, day5, day6, day7, day8, day9,
};
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

    if args.is_empty() || args.contains("12") {
        println!("Day 12");
        let data = get_day_input(12)?;
        println!("{:?}", day12::part1(&data));
        println!("{:?}", day12::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("13") {
        println!("Day 13");
        let data = get_day_input(13)?;
        println!("{:?}", day13::part1(&data));
        println!("{:?}", day13::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("14") {
        println!("Day 14");
        let data = get_day_input(14)?;
        println!("{:?}", day14::part1(&data));
        println!("{:?}", day14::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("15") {
        println!("Day 15");
        let data = get_day_input(15)?;
        println!("{:?}", day15::part1(&data).ok_or(anyhow!("None"))?);
        println!("{:?}", day15::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("16") {
        println!("Day 16");
        let data = get_day_input(16)?;
        println!("{:?}", day16::part1(&data).ok_or(anyhow!("None"))?);
        println!("{:?}", day16::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("17") {
        println!("Day 17");
        let data = get_day_input(17)?;
        println!("{}", day17::part1(&data).ok_or(anyhow!("None"))?);
        println!("{:?}", day17::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("18") {
        println!("Day 18");
        let data = get_day_input(18)?;
        println!("{:?}", day18::part1(&data).ok_or(anyhow!("None"))?);
        println!("{}", day18::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("19") {
        println!("Day 19");
        let data = get_day_input(19)?;
        let (part1, part2) = day19::solve(&data).ok_or(anyhow!("None"))?;
        println!("{:?}", part1);
        println!("{:?}", part2);
        println!();
    }

    if args.is_empty() || args.contains("20") {
        println!("Day 20");
        let data = get_day_input(20)?;
        println!("{:?}", day20::part1(&data).ok_or(anyhow!("None"))?);
        println!("{}", day20::part2(&data).ok_or(anyhow!("None"))?);
        println!();
    }

    if args.is_empty() || args.contains("21") {
        println!("Day 21");
        let data = get_day_input(21)?;
        println!("{:?}", day21::part1(&data));
        println!("{:?}", day21::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("22") {
        println!("Day 22");
        let data = get_day_input(22)?;
        println!("{:?}", day22::part1(&data));
        println!("{:?}", day22::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("23") {
        println!("Day 23");
        let data = get_day_input(23)?;
        println!("{:?}", day23::part1(&data));
        println!("{}", day23::part2(&data));
        println!();
    }

    if args.is_empty() || args.contains("24") {
        println!("Day 24");
        let data = get_day_input(24)?;
        println!("{:?}", day24::part1(&data)?);
        println!("{}", day24::part2(&data)?);
        println!();
    }

    if args.is_empty() || args.contains("25") {
        println!("Day 25");
        let data = get_day_input(25)?;
        println!("{:?}", day25::part1(&data));
        println!();
    }

    Ok(())
}
