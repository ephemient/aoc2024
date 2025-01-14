use aoc2024::{
    day1, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day2, day20, day21,
    day22, day23, day24, day25, day3, day4, day5, day6, day7, day8, day9,
};
use criterion::{black_box, Criterion};
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

fn aoc2024_bench(c: &mut Criterion) -> io::Result<()> {
    let data = get_day_input(1)?;
    let mut g = c.benchmark_group("day 1");
    g.bench_function("part 1", |b| b.iter(|| day1::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day1::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(2)?;
    let mut g = c.benchmark_group("day 2");
    g.bench_function("part 1", |b| b.iter(|| day2::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day2::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(3)?;
    let mut g = c.benchmark_group("day 3");
    g.bench_function("part 1", |b| b.iter(|| day3::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day3::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(4)?;
    let mut g = c.benchmark_group("day 4");
    g.bench_function("part 1", |b| b.iter(|| day4::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day4::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(5)?;
    let mut g = c.benchmark_group("day 5");
    g.bench_function("part 1", |b| b.iter(|| day5::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day5::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(6)?;
    let mut g = c.benchmark_group("day 6");
    g.bench_function("part 1", |b| b.iter(|| day6::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day6::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(7)?;
    let mut g = c.benchmark_group("day 7");
    g.bench_function("part 1", |b| b.iter(|| day7::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day7::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(8)?;
    let mut g = c.benchmark_group("day 8");
    g.bench_function("part 1", |b| b.iter(|| day8::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day8::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(9)?;
    let mut g = c.benchmark_group("day 9");
    g.bench_function("part 1", |b| b.iter(|| day9::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day9::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(10)?;
    let mut g = c.benchmark_group("day 10");
    g.bench_function("part 1", |b| b.iter(|| day10::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day10::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(11)?;
    let mut g = c.benchmark_group("day 11");
    g.bench_function("part 1", |b| b.iter(|| day11::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day11::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(12)?;
    let mut g = c.benchmark_group("day 12");
    g.bench_function("part 1", |b| b.iter(|| day12::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day12::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(13)?;
    let mut g = c.benchmark_group("day 13");
    g.bench_function("part 1", |b| b.iter(|| day13::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day13::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(14)?;
    let mut g = c.benchmark_group("day 14");
    g.bench_function("part 1", |b| b.iter(|| day14::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day14::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(15)?;
    let mut g = c.benchmark_group("day 15");
    g.bench_function("part 1", |b| b.iter(|| day15::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day15::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(16)?;
    let mut g = c.benchmark_group("day 16");
    g.bench_function("part 1", |b| b.iter(|| day16::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day16::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(17)?;
    let mut g = c.benchmark_group("day 17");
    g.bench_function("part 1", |b| b.iter(|| day17::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day17::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(18)?;
    let mut g = c.benchmark_group("day 18");
    g.bench_function("part 1", |b| b.iter(|| day18::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day18::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(19)?;
    let mut g = c.benchmark_group("day 19");
    g.bench_function("solve", |b| b.iter(|| day19::solve(black_box(&data))));
    g.finish();

    let data = get_day_input(20)?;
    let mut g = c.benchmark_group("day 20");
    g.bench_function("part 1", |b| b.iter(|| day20::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day20::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(21)?;
    let mut g = c.benchmark_group("day 21");
    g.bench_function("part 1", |b| b.iter(|| day21::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day21::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(22)?;
    let mut g = c.benchmark_group("day 22");
    g.bench_function("part 1", |b| b.iter(|| day22::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day22::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(23)?;
    let mut g = c.benchmark_group("day 23");
    g.bench_function("part 1", |b| b.iter(|| day23::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day23::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(24)?;
    let mut g = c.benchmark_group("day 24");
    g.bench_function("part 1", |b| b.iter(|| day24::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day24::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(25)?;
    let mut g = c.benchmark_group("day 25");
    g.bench_function("part 1", |b| b.iter(|| day25::part1(black_box(&data))));
    g.finish();

    Ok(())
}

fn aoc2024() -> io::Result<()> {
    let mut criterion = Criterion::default().configure_from_args();
    aoc2024_bench(&mut criterion)?;
    Ok(())
}

fn main() -> io::Result<()> {
    aoc2024()?;
    Criterion::default().configure_from_args().final_summary();
    Ok(())
}
