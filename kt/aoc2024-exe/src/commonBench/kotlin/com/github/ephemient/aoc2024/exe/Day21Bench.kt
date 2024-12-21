package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day21
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day21Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(21)
    }

    @Benchmark
    fun part1() = Day21(input).part1()

    @Benchmark
    fun part2() = Day21(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day21 = Day21(input)
        bh.consume(day21.part1())
        bh.consume(day21.part2())
    }
}