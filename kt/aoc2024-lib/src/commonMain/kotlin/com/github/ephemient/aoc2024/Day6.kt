package com.github.ephemient.aoc2024

import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.count
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.flatMapMerge
import kotlinx.coroutines.flow.flowOf

class Day6(input: String) {
    private val lines = input.lines()
    private val initialPosition = lines.withIndex().firstNotNullOf { (y, line) ->
        val x = line.indexOf('^')
        if (x >= 0) y to x else null
    }

    fun part1() = lines.walk(initialPosition).mapTo(mutableSetOf()) { it.first }.size

    @OptIn(ExperimentalCoroutinesApi::class)
    suspend fun part2() = lines.walk(initialPosition)
        .mapTo(mutableSetOf()) { it.first }
        .apply { remove(initialPosition) }
        .asFlow()
        .flatMapMerge { (y, x) ->
            val lines = lines.toMutableList()
            lines[y] = StringBuilder(lines[y]).apply { set(x, '#') }.toString()
            flowOf(Unit).filter { !lines.walk(initialPosition).all(mutableSetOf<Any?>()::add) }
        }
        .count()

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
