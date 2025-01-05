use std::cmp::Reverse;
use std::collections::{btree_map, BTreeMap, BTreeSet, BinaryHeap};

type Point = (usize, usize);
type Direction = (isize, isize);

fn parse(data: &str) -> Option<(BTreeSet<Point>, Point, Point)> {
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

struct Explore<F> {
    maze: F,
    queue: BinaryHeap<(Reverse<usize>, (Point, Direction), u8)>,
    visited: BTreeMap<(Point, Direction), usize>,
}

impl<F> Explore<F> {
    fn new(maze: F, start: Point) -> Self {
        Self {
            maze,
            queue: [(Reverse(0), (start, (0, 1)), 0)].into(),
            visited: BTreeMap::new(),
        }
    }
}

impl<F> Iterator for Explore<F>
where
    F: Fn(Point) -> bool,
{
    type Item = (usize, (Point, Direction), u8);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (Reverse(score), state @ ((y, x), (dy, dx)), way) = self.queue.pop()?;
            match self.visited.entry(state) {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(score);
                }
                btree_map::Entry::Occupied(entry) => {
                    let last_score = *entry.get();
                    assert!(last_score <= score);
                    if last_score < score {
                        continue;
                    }
                    break Some((score, state, way));
                }
            }
            for (score, (dy, dx), way) in [
                (score + 1, (dy, dx), 1),
                (score + 1001, (-dx, dy), 2),
                (score + 1001, (dx, -dy), 4),
            ] {
                let pos = (y.wrapping_add_signed(dy), x.wrapping_add_signed(dx));
                if !(self.maze)(pos) {
                    self.queue.push((Reverse(score), (pos, (dy, dx)), way));
                }
            }
            break Some((score, state, way));
        }
    }
}

pub fn part1(data: &str) -> Option<usize> {
    let (maze, start, end) = parse(data)?;
    Some(
        Explore::new(|pos| maze.contains(&pos), start)
            .find(|(_, (pos, _), _)| *pos == end)?
            .0,
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let (maze, start, end) = parse(data)?;
    let mut best = None;
    let mut paths = BTreeMap::new();
    for (score, state @ (pos, _), way) in Explore::new(|pos| maze.contains(&pos), start) {
        if best.is_some_and(|best| best < score) {
            break;
        }
        paths.entry(state).and_modify(|e| *e |= way).or_insert(way);
        if pos == end {
            best = Some(score);
        }
    }
    let mut stack = vec![(end, (0, 1)), (end, (1, 0)), (end, (0, -1)), (end, (-1, 0))];
    let mut visited = stack.iter().cloned().collect::<BTreeSet<_>>();
    while let Some(key @ ((y, x), dir @ (dy, dx))) = stack.pop() {
        let Some(ways) = paths.get(&key) else {
            continue;
        };
        let pos = (y.wrapping_add_signed(-dy), x.wrapping_add_signed(-dx));
        for (way, dir) in [(1, dir), (2, (dx, -dy)), (4, (-dx, dy))] {
            if ways & way != 0 && visited.insert((pos, dir)) {
                stack.push((pos, dir));
            }
        }
    }
    let mut last = None;
    Some(
        visited
            .iter()
            .filter(|(pos, _)| Some(*pos) != last.replace(*pos))
            .count(),
    )
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
