package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day9
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day9Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(9)
    }

    @Benchmark
    fun part1() = Day9(input).part1()

    @Benchmark
    fun part2() = Day9(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day9 = Day9(input)
        bh.consume(day9.part1())
        bh.consume(day9.part2())
    }
}
