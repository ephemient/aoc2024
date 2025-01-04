package com.github.ephemient.aoc2024

import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.count
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.flatMapMerge
import kotlinx.coroutines.flow.flowOf

class Day6(input: String) {
    private val lines = input.lines()
    private val initialPosition = lines.withIndex().firstNotNullOf { (y, line) ->
        val x = line.indexOf('^')
        if (x >= 0) y to x else null
    }
    private val initialWalk: Set<IntPair> by lazy(LazyThreadSafetyMode.PUBLICATION) {
        lines.walk(initialPosition).mapTo(mutableSetOf()) { it.first }
    }

    fun part1() = initialWalk.size

    @OptIn(ExperimentalCoroutinesApi::class)
    suspend fun part2() = initialWalk.asFlow().drop(1).flatMapMerge { (y, x) ->
        val lines = lines.toMutableList()
        lines[y] = StringBuilder(lines[y]).apply { set(x, '#') }.toString()
        flowOf(Unit).filter {
            var last = 0
            val visited = mutableSetOf<IntPair>()
            !lines.walk(initialPosition).all { (pos, dir) ->
                (last == -1 || dir.first != -1 || visited.add(pos)).also { last = dir.first }
            }
        }
    }.count()

    companion object {
        private fun List<String>.walk(position: IntPair) = sequence {
            var (y, x) = position
            var dy = -1
            var dx = 0
            while (true) {
                yield(Pair(y to x, dy to dx))
                val nextY = y + dy
                val nextX = x + dx
                when (getOrNull(nextY)?.getOrNull(nextX)) {
                    null -> break
                    '#' -> dy = dx.also { dx = -dy }
                    else -> {
                        y = nextY
                        x = nextX
                    }
                }
            }
        }
    }
}
