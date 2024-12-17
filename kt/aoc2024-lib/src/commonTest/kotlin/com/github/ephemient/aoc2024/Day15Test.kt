package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun part1() {
        assertEquals(2028, Day15(example2).part1())
        assertEquals(10092, Day15(example1).part1())
    }

    @Test
    fun part2() {
        assertEquals(618, Day15(example3).part2())
        assertEquals(9021, Day15(example1).part2())
    }

    companion object {
        val example1 =
            """
            |##########
            |#..O..O.O#
            |#......O.#
            |#.OO..O.O#
            |#..O@..O.#
            |#O#..O...#
            |#O..O..O.#
            |#.OO.O.OO#
            |#....O...#
            |##########
            |
            |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
            |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
            |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
            |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
            |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
            |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
            |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
            |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
            |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
            |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
            |""".trimMargin()
        val example2 =
            """
            |########
            |#..O.O.#
            |##@.O..#
            |#...O..#
            |#.#.O..#
            |#...O..#
            |#......#
            |########
            |
            |<^^>>>vv<v>>v<<
            |""".trimMargin()
        val example3 =
            """
            |#######
            |#...#.#
            |#.....#
            |#..OO@#
            |#..O..#
            |#.....#
            |#######
            |
            |<vv<<^^<<^^                
            |""".trimMargin()
    }
}
