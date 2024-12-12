package com.github.ephemient.aoc2024

import kotlin.math.abs

class Day12(input: String) {
    private val groups = buildList {
        val visited = mutableSetOf<IntPair>()
        val lines = input.lines()
        for ((y, line) in lines.withIndex()) {
            line.forEachIndexed { x, char ->
                add(
                    buildSet {
                        val stack = mutableListOf(y to x)
                        while (stack.isNotEmpty()) {
                            val pos = stack.removeLast()
                            if (lines[pos] != char || !visited.add(pos)) continue
                            add(pos)
                            stack.add(pos.first - 1 to pos.second)
                            stack.add(pos.first to pos.second - 1)
                            stack.add(pos.first to pos.second + 1)
                            stack.add(pos.first + 1 to pos.second)
                        }
                    }.ifEmpty { return@forEachIndexed }
                )
            }
        }
    }

    fun part1() = groups.sumOf { group ->
        group.size * group.flatMap { (y, x) ->
            listOf(2 * y - 1 to 2 * x, 2 * y to 2 * x - 1, 2 * y to 2 * x + 1, 2 * y + 1 to 2 * x)
        }.groupingBy { it }.eachCount().count { it.value == 1 }
    }

    fun part2() = groups.sumOf { group ->
        group.size * group.flatMap { (y, x) ->
            listOf(
                2 * y - 1 to 2 * x to true,
                2 * y to 2 * x - 1 to false,
                2 * y to 2 * x + 1 to true,
                2 * y + 1 to 2 * x to false,
            )
        }
            .groupBy { it.first }
            .flatMap { (_, value) -> if (value.size == 1) value else emptyList() }
            .groupBy { (pos, _) ->
                val (y, x) = pos
                if (y % 2 == 0) x else y + 1
            }
            .values
            .sumOf { edges ->
                val line = buildList {
                    edges.mapTo(this) { (pos, dir) -> pos.first + pos.second to dir }
                    sortBy(Pair<Int, Boolean>::first)
                }
                line.indices.count {
                    line[it].second != line.getOrNull(it + 1)?.second ||
                        abs(line[it].first - line[it + 1].first) > 2
                }
            }
    }

    companion object {
        private operator fun List<String>.get(pos: IntPair) = getOrNull(pos.first)?.getOrNull(pos.second)
    }
}
