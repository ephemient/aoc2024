package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day12
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day12Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(12)
    }

    @Benchmark
    fun part1() = Day12(input).part1()

    @Benchmark
    fun part2() = Day12(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day12 = Day12(input)
        bh.consume(day12.part1())
        bh.consume(day12.part2())
    }
}
