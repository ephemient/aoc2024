use if_chain::if_chain;
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap};

type YX = (usize, usize);

fn parse(data: &str) -> Option<(BTreeSet<YX>, YX, YX)> {
    let mut maze = BTreeSet::new();
    let mut start = None;
    let mut end = None;
    for (y, line) in data.lines().enumerate() {
        for (x, b) in line.bytes().enumerate() {
            match b {
                b'#' => {
                    maze.insert((y, x));
                }
                b'S' => {
                    if start.replace((y, x)).is_some() {
                        return None;
                    }
                }
                b'E' => {
                    if end.replace((y, x)).is_some() {
                        return None;
                    }
                }
                _ => {}
            }
        }
    }
    Some((maze, start?, end?))
}

pub fn part1(data: &str) -> Option<usize> {
    let (maze, start, end) = parse(data)?;
    let mut queue: BinaryHeap<_> = [(Reverse(0), start, (0, 1))].into();
    let mut visited = BTreeSet::new();
    while let Some((Reverse(score), (y, x), (dy, dx))) = queue.pop() {
        if !visited.insert(((y, x), (dy, dx))) {
            continue;
        }
        if (y, x) == end {
            return Some(score);
        }
        for (score, (dy, dx)) in [
            (score + 1, (dy, dx)),
            (score + 1001, (-dx, dy)),
            (score + 1001, (dx, -dy)),
        ] {
            if_chain! {
                if let Some(y) = y.checked_add_signed(dy);
                if let Some(x) = x.checked_add_signed(dx);
                if !maze.contains(&(y, x));
                then {
                    queue.push((Reverse(score), (y, x), (dy, dx)));
                }
            }
        }
    }
    None
}

pub fn part2(data: &str) -> Option<usize> {
    let (maze, start, end) = parse(data)?;
    let mut best = None;
    let mut acc = BTreeSet::new();
    let mut queue: BinaryHeap<(_, _, _, BTreeSet<_>)> =
        [(Reverse(0), start, (0, 1), [start].into())].into();
    let mut visited = BTreeMap::new();
    while let Some((Reverse(score), (y, x), (dy, dx), mut path)) = queue.pop() {
        if best.filter(|best| *best < score).is_some() {
            break;
        }
        if *visited.entry(((y, x), (dy, dx))).or_insert(score) < score {
            continue;
        }
        if (y, x) == end {
            best = Some(score);
            acc.append(&mut path);
            continue;
        }
        for (score, (dy, dx)) in [
            (score + 1, (dy, dx)),
            (score + 1001, (-dx, dy)),
            (score + 1001, (dx, -dy)),
        ] {
            if_chain! {
                if let Some(y) = y.checked_add_signed(dy);
                if let Some(x) = x.checked_add_signed(dx);
                if !maze.contains(&(y, x));
                then {
                    let mut path = path.clone();
                    if path.insert((y, x)) {
                        queue.push((Reverse(score), (y, x), (dy, dx), path));
                    }
                }
            }
        }
    }
    Some(acc.len())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        ###############
        #.......#....E#
        #.#.###.#.###.#
        #.....#.#...#.#
        #.###.#####.#.#
        #.#.#.......#.#
        #.#.#####.###.#
        #...........#.#
        ###.#.#####.#.#
        #...#.....#.#.#
        #.#.#.###.#.#.#
        #.....#...#.#.#
        #.###.#.#.#.#.#
        #S..#.....#...#
        ###############
    "};

    static EXAMPLE_2: &str = indoc! {"
        #################
        #...#...#...#..E#
        #.#.#.#.#.#.#.#.#
        #.#.#.#...#...#.#
        #.#.#.#.###.#.#.#
        #...#.#.#.....#.#
        #.#.#.#.#.#####.#
        #.#...#.#.#.....#
        #.#.#####.#.###.#
        #.#.#.......#...#
        #.#.###.#####.###
        #.#.#...#.....#.#
        #.#.#.#####.###.#
        #.#.#.........#.#
        #.#.#.#########.#
        #S#.............#
        #################
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(7036), part1(EXAMPLE_1));
        assert_eq!(Some(11048), part1(EXAMPLE_2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(45), part2(EXAMPLE_1));
        assert_eq!(Some(64), part2(EXAMPLE_2));
    }
}
