package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day8
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day8Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(8)
    }

    @Benchmark
    fun part1() = Day8(input).part1()

    @Benchmark
    fun part2() = Day8(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day8 = Day8(input)
        bh.consume(day8.part1())
        bh.consume(day8.part2())
    }
}
