use std::array;
use std::cmp::Ordering;

#[allow(clippy::erasing_op, clippy::identity_op)]
const fn key_pos(key: u8) -> u8 {
    match key {
        b'0' => 3 * 1 + 1,
        b'1' => 3 * 2 + 0,
        b'2' => 3 * 2 + 1,
        b'3' => 3 * 2 + 2,
        b'4' => 3 * 3 + 0,
        b'5' => 3 * 3 + 1,
        b'6' => 3 * 3 + 2,
        b'7' => 3 * 4 + 0,
        b'8' => 3 * 4 + 1,
        b'9' => 3 * 4 + 2,
        _ => 3 * 1 + 2,
    }
}
#[allow(clippy::erasing_op, clippy::identity_op)]
const BLANK: u8 = 3 * 1 + 0;
#[allow(clippy::erasing_op, clippy::identity_op)]
const A: u8 = 3 * 1 + 2;
#[allow(clippy::erasing_op, clippy::identity_op)]
const UP: u8 = 3 * 1 + 1;
#[allow(clippy::erasing_op, clippy::identity_op)]
const LEFT: u8 = 3 * 0 + 0;
#[allow(clippy::erasing_op, clippy::identity_op)]
const DOWN: u8 = 3 * 0 + 1;
#[allow(clippy::erasing_op, clippy::identity_op)]
const RIGHT: u8 = 3 * 0 + 2;

fn search(table: &[Option<usize>; 225], p1: u8, p2: u8, pos: u8) -> Option<usize> {
    if p1 == BLANK || p2 == BLANK {
        None
    } else if p1 == p2 {
        table[(15 * pos + A) as usize]
    } else {
        let (y1, x1) = (p1 / 3, p1 % 3);
        let (y2, x2) = (p2 / 3, p2 % 3);
        [
            match x1.cmp(&x2) {
                Ordering::Less => Some(((x1 + 1, y1), RIGHT)),
                Ordering::Greater => Some(((x1 - 1, y1), LEFT)),
                _ => None,
            },
            match y1.cmp(&y2) {
                Ordering::Less => Some(((x1, y1 + 1), UP)),
                Ordering::Greater => Some(((x1, y1 - 1), DOWN)),
                _ => None,
            },
        ]
        .into_iter()
        .filter_map(|choice| {
            let ((x, y), pos2) = choice?;
            Some(table[(15 * pos + pos2) as usize]? + search(table, 3 * y + x, p2, pos2)?)
        })
        .min()
    }
}

fn solve(data: &str, depth: usize) -> usize {
    let mut table = array::from_fn::<_, 225, _>(|i| {
        let (p1, p2) = (i as u8 / 15, i as u8 % 15);
        if p1 == BLANK || p2 == BLANK {
            None
        } else {
            let (x1, y1) = (p1 / 3, p1 % 3);
            let (x2, y2) = (p2 / 3, p2 % 3);
            Some((x1.abs_diff(x2) + y1.abs_diff(y2) + 1) as usize)
        }
    });
    for _ in 0..depth {
        table = array::from_fn(|i| {
            let (p1, p2) = (i as u8 / 15, i as u8 % 15);
            if p1 == BLANK || p2 == BLANK {
                None
            } else {
                search(&table, p1, p2, A)
            }
        });
    }
    data.lines()
        .filter_map(|line| {
            let (mut num, mut len, mut pos) = (0, 0, A);
            for b in line.bytes() {
                if b.is_ascii_digit() {
                    num = 10 * num + (b - b'0') as usize;
                }
                let pos2 = key_pos(b);
                len += table[(15 * pos + pos2) as usize]?;
                pos = pos2;
            }
            Some(num * len)
        })
        .sum()
}

pub fn part1(data: &str) -> usize {
    solve(data, 2)
}

pub fn part2(data: &str) -> usize {
    solve(data, 25)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        029A
        980A
        179A
        456A
        379A
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(126384, part1(EXAMPLE));
    }
}
