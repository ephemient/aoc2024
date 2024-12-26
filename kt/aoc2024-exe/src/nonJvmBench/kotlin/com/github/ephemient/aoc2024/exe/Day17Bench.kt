package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day17
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day17Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(17)
    }

    @Benchmark
    fun part1() = Day17(input).part1()

    @Benchmark
    fun part2() = Day17(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day17 = Day17(input)
        bh.consume(day17.part1())
        bh.consume(day17.part2())
    }
}
