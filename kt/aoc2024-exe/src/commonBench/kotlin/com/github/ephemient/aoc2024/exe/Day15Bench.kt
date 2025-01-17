package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day15
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day15Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(15)
    }

    @Benchmark
    fun part1() = Day15(input).part1()

    @Benchmark
    fun part2() = Day15(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day15 = Day15(input)
        bh.consume(day15.part1())
        bh.consume(day15.part2())
    }
}
