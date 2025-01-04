package com.github.ephemient.aoc2024

import kotlin.math.abs
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.flatMapMerge
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.fold

class Day20(input: String) {
    private val path = buildSet {
        val lines = input.lines()
        var position = lines.withIndex().firstNotNullOf { (y, line) ->
            val x = line.indexOf('S')
            if (x < 0) null else y to x
        }
        add(position)
        do {
            position = position.adjacencies().single { next ->
                val (y, x) = next
                y in lines.indices && x in lines[y].indices && lines[y][x] != '#' && add(next)
            }
        } while (lines[position.first][position.second] != 'E')
    }.withIndex().sortedWith(compareBy(comparator) { it.value })

    suspend fun part1(time: Int = 100) = solve(2, time)

    suspend fun part2(time: Int = 100) = solve(20, time)

    @OptIn(ExperimentalCoroutinesApi::class)
    private suspend fun solve(cheats: Int, time: Int) = path.indices.asFlow().flatMapMerge { i ->
        flow {
            var count = 0
            val (t1, pos1) = path[i]
            val limit = pos1.first + cheats to pos1.second
            for (j in i + 1..<path.size) {
                val (t2, pos2) = path[j]
                if (comparator.compare(pos2, limit) > 0) break
                val distance = abs(pos2.first - pos1.first) + abs(pos2.second - pos1.second)
                if (distance <= cheats && distance + time <= abs(t2 - t1)) count++
            }
            emit(count)
        }
    }.fold(0, Int::plus)

    companion object {
        private val comparator = compareBy(IntPair::first, IntPair::second)
        private fun IntPair.adjacencies() =
            arrayOf(first - 1 to second, first to second - 1, first to second + 1, first + 1 to second)
    }
}
