use std::cmp::Ordering;

use itertools::Itertools;

const WIDTH: isize = 101;
const HEIGHT: isize = 103;

#[derive(Clone, Copy, Debug)]
struct Robot {
    x0: isize,
    y0: isize,
    vx: isize,
    vy: isize,
}

fn parse(data: &str) -> impl Iterator<Item = Robot> + use<'_> {
    data.lines().filter_map(|line| {
        let (p, v) = line.split_once(' ')?;
        let (x0, y0) = p.strip_prefix("p=")?.split_once(',')?;
        let (vx, vy) = v.strip_prefix("v=")?.split_once(',')?;
        Some(Robot {
            x0: x0.parse().ok()?,
            y0: y0.parse().ok()?,
            vx: vx.parse().ok()?,
            vy: vy.parse().ok()?,
        })
    })
}

pub fn part1(data: &str) -> usize {
    part1_(data, WIDTH, HEIGHT)
}

fn part1_(data: &str, width: isize, height: isize) -> usize {
    let (mut q1, mut q2, mut q3, mut q4) = (0, 0, 0, 0);
    for Robot { x0, y0, vx, vy } in parse(data) {
        let x = (x0 + vx * 100).rem_euclid(width);
        let y = (y0 + vy * 100).rem_euclid(height);
        match ((width / 2).cmp(&x), (height / 2).cmp(&y)) {
            (Ordering::Less, Ordering::Less) => q1 += 1,
            (Ordering::Less, Ordering::Greater) => q2 += 1,
            (Ordering::Greater, Ordering::Less) => q3 += 1,
            (Ordering::Greater, Ordering::Greater) => q4 += 1,
            _ => {}
        }
    }
    q1 * q2 * q3 * q4
}

pub fn part2(data: &str) -> Option<usize> {
    let robots = parse(data).collect::<Vec<_>>();
    (0..WIDTH * HEIGHT)
        .max_by_key(|t| {
            robots
                .iter()
                .map(|Robot { x0, y0, vx, vy }| {
                    (
                        (x0 + vx * t).rem_euclid(WIDTH),
                        (y0 + vy * t).rem_euclid(HEIGHT),
                    )
                })
                .sorted()
                .tuple_windows()
                .filter(|((x, y), next)| (*x, y + 1) == *next)
                .count()
        })
        .and_then(|t| t.try_into().ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        p=0,4 v=3,-3
        p=6,3 v=-1,-3
        p=10,3 v=-1,2
        p=2,0 v=2,-1
        p=0,0 v=1,3
        p=3,0 v=-2,-2
        p=7,6 v=-1,-3
        p=3,0 v=-1,-2
        p=9,3 v=2,3
        p=7,3 v=-1,2
        p=2,4 v=2,-3
        p=9,5 v=-3,-3
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(12, part1_(EXAMPLE, 11, 7));
    }
}
