package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day3
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day3Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(3)
    }

    @Benchmark
    fun part1() = Day3(input).part1()

    @Benchmark
    fun part2() = Day3(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day3 = Day3(input)
        bh.consume(day3.part1())
        bh.consume(day3.part2())
    }
}
