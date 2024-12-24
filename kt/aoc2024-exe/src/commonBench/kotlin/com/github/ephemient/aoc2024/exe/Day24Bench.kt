package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day24
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day24Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(24)
    }

    @Benchmark
    fun part1() = Day24(input).part1()

    @Benchmark
    fun part2() = Day24(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day24 = Day24(input)
        bh.consume(day24.part1())
        bh.consume(day24.part2())
    }
}
