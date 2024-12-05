package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day5
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day5Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(5)
    }

    @Benchmark
    fun part1() = Day5(input).part1()

    @Benchmark
    fun part2() = Day5(input).part2()

    @Benchmark
    fun both(bh: Blackhole) {
        val day5 = Day5(input)
        bh.consume(day5.part1())
        bh.consume(day5.part2())
    }
}
