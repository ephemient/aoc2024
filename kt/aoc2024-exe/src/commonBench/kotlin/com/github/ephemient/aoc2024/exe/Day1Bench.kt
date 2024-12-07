package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day1
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day1Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(1)
    }

    @Benchmark
    fun part1() = Day1(input).part1()

    @Benchmark
    fun part2() = Day1(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day1 = Day1(input)
        bh.consume(day1.part1())
        bh.consume(day1.part2())
    }
}
