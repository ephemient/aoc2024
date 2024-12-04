package com.github.ephemient.aoc2024

class Day4(input: String) {
    private val lines = input.lines()

    fun part1() = lines.indices.sumOf { y ->
        lines[y].indices.sumOf { x ->
            Direction.entries.count { (dx, dy) ->
                XMAS.withIndex().all { (i, c) ->
                    lines.getOrNull(y + i * dy)?.getOrNull(x + i * dx) == c
                }
            }
        }
    }

    fun part2() = lines.indices.sumOf { y ->
        lines[y].indices.count { x ->
            if (lines[y][x] != 'A') return@count false
            val n = lines.getOrNull(y - 1) ?: return@count false
            val s = lines.getOrNull(y + 1) ?: return@count false
            setOf(n.getOrNull(x - 1), s.getOrNull(x + 1)) == MS &&
                setOf(n.getOrNull(x + 1), s.getOrNull(x - 1)) == MS
        }
    }

    private enum class Direction(val dx: Int, val dy: Int) {
        EAST(1, 0),
        NORTHEAST(1, -1),
        NORTH(0, -1),
        NORTHWEST(-1, -1),
        WEST(-1, 0),
        SOUTHWEST(-1, 1),
        SOUTH(0, 1),
        SOUTHEAST(1, 1),
        ;

        operator fun component1() = dx
        operator fun component2() = dy
    }

    companion object {
        private const val XMAS = "XMAS"
        private val MS = "MS".toSet()
    }
}
