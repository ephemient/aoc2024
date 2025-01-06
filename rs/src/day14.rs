use std::cmp::Ordering;

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
    part1_::<WIDTH, HEIGHT>(data)
}

fn part1_<const WIDTH: isize, const HEIGHT: isize>(data: &str) -> usize {
    let (mut q1, mut q2, mut q3, mut q4) = (0, 0, 0, 0);
    for Robot { x0, y0, vx, vy } in parse(data) {
        let x = (x0 + vx * 100).rem_euclid(WIDTH);
        let y = (y0 + vy * 100).rem_euclid(HEIGHT);
        match ((WIDTH / 2).cmp(&x), (HEIGHT / 2).cmp(&y)) {
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
    const INVERSE: isize = 51;
    const _: () = assert!(WIDTH * INVERSE % HEIGHT == 1);

    let robots = parse(data).collect::<Vec<_>>();
    let x = (0..WIDTH).max_by_key(|t| {
        let (mut less, mut greater) = (0usize, 0usize);
        for Robot { x0, vx, .. } in &robots {
            match (x0 + vx * t).cmp(&(WIDTH / 2)) {
                Ordering::Less => less += 1,
                Ordering::Greater => greater += 1,
                _ => {}
            }
        }
        less.max(greater)
    })?;
    let y = (0..HEIGHT).max_by_key(|t| {
        let (mut less, mut greater) = (0usize, 0usize);
        for Robot { y0, vy, .. } in &robots {
            match (y0 + vy * t).cmp(&(HEIGHT / 2)) {
                Ordering::Less => less += 1,
                Ordering::Greater => greater += 1,
                _ => {}
            }
        }
        less.max(greater)
    })?;
    Some(((x + (y - x) * INVERSE * WIDTH) % (WIDTH * HEIGHT)) as usize)
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
        assert_eq!(12, part1_::<11, 7>(EXAMPLE));
    }
}
