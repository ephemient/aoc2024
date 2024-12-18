use std::collections::BTreeSet;
use std::ops::IndexMut;

type YX = (usize, usize);

fn parse(data: &str) -> Option<(Vec<Vec<u8>>, YX, &str)> {
    let (map, moves) = data.split_once("\n\n").unwrap_or((data, ""));
    let map = map
        .lines()
        .map(|line| line.bytes().collect())
        .collect::<Vec<Vec<_>>>();
    let robot = map.iter().enumerate().find_map(|(y, line)| {
        line.iter()
            .enumerate()
            .find(|(_, b)| **b == b'@')
            .map(|(x, _)| (y, x))
    });
    Some((map, robot?, moves))
}

fn move_robot<Line>(map: &mut [Line], robot: YX, c: char) -> Option<YX>
where
    Line: IndexMut<usize, Output = u8>,
{
    let (dy, dx) = match c {
        '^' => (usize::MAX, 0),
        'v' => (1, 0),
        '<' => (0, usize::MAX),
        '>' => (0, 1),
        _ => return None,
    };
    let mut levels = vec![];
    let mut front: BTreeSet<_> = [robot].into();
    while !front.is_empty() {
        let back = front;
        front = BTreeSet::new();
        for (y, x) in &back {
            let (y, x) = (y.wrapping_add(dy), x.wrapping_add(dx));
            match map[y][x] {
                b'.' => continue,
                b'O' => {
                    front.insert((y, x));
                }
                b'[' => {
                    front.insert((y, x));
                    if dy != 0 {
                        front.insert((y, x + 1));
                    }
                }
                b']' => {
                    front.insert((y, x));
                    if dy != 0 {
                        front.insert((y, x - 1));
                    }
                }
                _ => return None,
            }
        }
        levels.push(back);
    }
    for front in levels.into_iter().rev() {
        for (y, x) in front {
            map[y.wrapping_add(dy)][x.wrapping_add(dx)] = map[y][x];
            map[y][x] = b'.';
        }
    }
    Some((robot.0.wrapping_add(dy), robot.1.wrapping_add(dx)))
}

pub fn part1(data: &str) -> Option<usize> {
    let (mut map, mut robot, moves) = parse(data)?;
    for c in moves.chars() {
        robot = move_robot(&mut map, robot, c).unwrap_or(robot);
    }
    Some(
        map.into_iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.into_iter().enumerate().filter_map(move |(x, b)| {
                    if b == b'O' {
                        Some(100 * y + x)
                    } else {
                        None
                    }
                })
            })
            .sum(),
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let (mut map, mut robot, moves) = parse(data)?;
    for line in &mut map {
        *line = line
            .iter()
            .flat_map(|&b| match b {
                b'@' => *b"@.",
                b'O' => *b"[]",
                _ => [b, b],
            })
            .collect();
    }
    robot.1 *= 2;
    for c in moves.chars() {
        robot = move_robot(&mut map, robot, c).unwrap_or(robot);
    }
    Some(
        map.into_iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.into_iter().enumerate().filter_map(move |(x, b)| {
                    if b == b'[' {
                        Some(100 * y + x)
                    } else {
                        None
                    }
                })
            })
            .sum(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        ##########
        #..O..O.O#
        #......O.#
        #.OO..O.O#
        #..O@..O.#
        #O#..O...#
        #O..O..O.#
        #.OO.O.OO#
        #....O...#
        ##########

        <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    "};

    static EXAMPLE_2: &str = indoc! {"
        ########
        #..O.O.#
        ##@.O..#
        #...O..#
        #.#.O..#
        #...O..#
        #......#
        ########

        <^^>>>vv<v>>v<<
    "};

    static EXAMPLE_3: &str = indoc! {"
        #######
        #...#.#
        #.....#
        #..OO@#
        #..O..#
        #.....#
        #######

        <vv<<^^<<^^
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(2028), part1(EXAMPLE_2));
        assert_eq!(Some(10092), part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(618), part2(EXAMPLE_3));
        assert_eq!(Some(9021), part2(EXAMPLE_1));
    }
}
