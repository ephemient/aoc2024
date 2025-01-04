use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};

pub fn part1(data: &str) -> Option<usize> {
    solve(data, 2, 100)
}

pub fn part2(data: &str) -> Option<usize> {
    solve(data, 20, 100)
}

fn solve(data: &str, cheats: usize, time: usize) -> Option<usize> {
    let lines = data.lines().collect::<Vec<_>>();
    let start = lines
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes()
                .enumerate()
                .filter_map(move |(x, b)| Some((y, x)).filter(|_| b == b'S'))
        })
        .exactly_one()
        .ok()?;
    let mut path = vec![(start, 0)];
    loop {
        let prev = path.iter().nth_back(1).map(|(pos, _)| pos);
        let (pos, b) = path
            .last()
            .iter()
            .flat_map(|((y, x), _)| {
                [
                    (y.wrapping_sub(1), *x),
                    (*y, x.wrapping_sub(1)),
                    (*y, x.wrapping_add(1)),
                    (y.wrapping_add(1), *x),
                ]
            })
            .map(|pos @ (y, x)| (pos, lines.get(y).map(|line| line.as_bytes().get(x))))
            .filter(|(pos, b)| Some(pos) != prev && b != &Some(Some(&b'#')))
            .exactly_one()
            .ok()?;
        path.push((pos, path.len()));
        if b == Some(Some(&b'E')) {
            break;
        }
    }
    path.sort_unstable();
    Some(
        path.par_iter()
            .enumerate()
            .map(|(i, ((y1, x1), t1))| {
                path[i + 1..]
                    .iter()
                    .take_while(|(pos, _)| *pos <= (y1 + cheats, *x1))
                    .filter(|((y2, x2), t2)| {
                        let d = y1.abs_diff(*y2) + x1.abs_diff(*x2);
                        d <= cheats && d + time <= t1.abs_diff(*t2)
                    })
                    .count()
            })
            .sum(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ###############
        #...#...#.....#
        #.#.#.#.#.###.#
        #S#...#.#.#...#
        #######.#.#.###
        #######.#.#...#
        #######.#.###.#
        ###..E#...#...#
        ###.#######.###
        #...###...#...#
        #.#####.#.###.#
        #.#...#.#.#...#
        #.#.#.#.#.#.###
        #...#...#...###
        ###############
    "};

    #[test]
    fn part1_examples() {
        [
            (14, 2),
            (14, 4),
            (2, 6),
            (4, 8),
            (2, 10),
            (3, 12),
            (1, 20),
            (1, 36),
            (1, 38),
            (1, 40),
            (1, 64),
        ]
        .into_iter()
        .rev()
        .fold(0, |acc, (count, time)| {
            assert_eq!(Some(acc + count), solve(EXAMPLE, 2, time));
            acc + count
        });
    }

    #[test]
    fn part2_examples() {
        [
            (32, 50),
            (31, 52),
            (29, 54),
            (39, 56),
            (25, 58),
            (23, 60),
            (20, 62),
            (19, 64),
            (12, 66),
            (14, 68),
            (12, 70),
            (22, 72),
            (4, 74),
            (3, 76),
        ]
        .into_iter()
        .rev()
        .fold(0, |acc, (count, time)| {
            assert_eq!(Some(acc + count), solve(EXAMPLE, 20, time));
            acc + count
        });
    }
}
