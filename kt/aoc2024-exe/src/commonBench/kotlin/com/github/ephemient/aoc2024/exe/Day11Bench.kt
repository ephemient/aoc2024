package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day11
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day11Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(11)
    }

    @Benchmark
    fun part1() = Day11(input).part1()

    @Benchmark
    fun part2() = Day11(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day11 = Day11(input)
        bh.consume(day11.part1())
        bh.consume(day11.part2())
    }
}
