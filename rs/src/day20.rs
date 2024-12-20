use std::collections::BTreeMap;
use std::iter;

use if_chain::if_chain;

pub fn part1(data: &str) -> usize {
    solve(data, 2, 100)
}

pub fn part2(data: &str) -> usize {
    solve(data, 20, 100)
}

fn solve(data: &str, cheats: usize, time: usize) -> usize {
    let lines = data.lines().collect::<Vec<_>>();
    lines
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes()
                .enumerate()
                .filter_map(move |(x, b)| Some((y, x)).filter(|_| b == b'S'))
        })
        .flat_map(|start| {
            let lines = lines.clone();
            let mut queue: Vec<(_, BTreeMap<_, _>)> = vec![(start, [(start, 0)].into())];
            iter::from_fn(move || {
                while let Some(((y, x), path)) = queue.pop() {
                    match lines[y].as_bytes()[x] {
                        b'E' => {
                            return Some(
                                path.iter()
                                    .map(|(key, value)| (*key, *value))
                                    .collect::<Vec<_>>(),
                            )
                        }
                        _ => {
                            for pos @ (y, x) in [
                                (y.wrapping_sub(1), x),
                                (y, x.wrapping_sub(1)),
                                (y, x + 1),
                                (y + 1, x),
                            ] {
                                if_chain! {
                                    if !path.contains_key(&pos);
                                    if let Some(line) = lines.get(y);
                                    if let Some(b) = line.as_bytes().get(x);
                                    if *b != b'#';
                                    then {
                                        let mut path = path.clone();
                                        path.insert(pos, path.len());
                                        queue.push((pos, path));
                                    }
                                }
                            }
                        }
                    }
                }
                None
            })
        })
        .map(|path| {
            path.iter()
                .enumerate()
                .map(|(i, ((y1, x1), t1))| {
                    path[i + 1..]
                        .iter()
                        .take_while(|(pos, _)| *pos <= (y1 + cheats, *x1))
                        .filter(|((y2, x2), t2)| {
                            let distance = y1.abs_diff(*y2) + x1.abs_diff(*x2);
                            distance <= cheats && distance + time <= t1.abs_diff(*t2)
                        })
                        .count()
                })
                .sum::<usize>()
        })
        .sum()
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
            assert_eq!(acc + count, solve(EXAMPLE, 2, time));
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
            assert_eq!(acc + count, solve(EXAMPLE, 20, time));
            acc + count
        });
    }
}
