package com.github.ephemient.aoc2024.codegen

import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.LIST
import com.squareup.kotlinpoet.LONG
import com.squareup.kotlinpoet.LONG_ARRAY
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.MemberName.Companion.member
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.STRING
import com.squareup.kotlinpoet.TypeSpec
import com.squareup.kotlinpoet.joinToCode
import java.nio.file.Path
import kotlin.math.abs

fun day21(outputDir: Path, className: ClassName = ClassName("com.github.ephemient.aoc2024", "Day21")) {
    FileSpec.builder(className)
        .addType(
            TypeSpec.classBuilder(className)
                .primaryConstructor(
                    FunSpec.constructorBuilder()
                        .addParameter("input", STRING)
                        .build()
                )
                .addProperty(
                    PropertySpec.builder("lines", LIST.parameterizedBy(STRING))
                        .addModifiers(KModifier.PRIVATE)
                        .initializer("%N.%M()", "input", MemberName("kotlin.text", "lines"))
                        .build()
                )
                .addFunctions(
                    arrayOf("part1", "part2").map { name ->
                        FunSpec.builder(name)
                            .returns(LONG)
                            .beginControlFlow("return %N.%M", "lines", MemberName("kotlin.collections", "sumOf"))
                            .addStatement("var %N = 10", "i")
                            .addStatement("var %N = 0", "numeric")
                            .addStatement("var %N = 0L", "length")
                            .beginControlFlow("for (%N in %N)", "char", "it")
                            .beginControlFlow("val %N = when (%N)", "j", "char")
                            .beginControlFlow("in '0'..'9' ->")
                            .addStatement("%1N = 10 * %1N + (%2N - '0')", "numeric", "char")
                            .addStatement("%N - '0'", "char")
                            .endControlFlow()
                            .addStatement("else -> 10")
                            .endControlFlow()
                            .addStatement(
                                "%N += %M[11 * %N + %N]",
                                "length",
                                className.nestedClass("Companion").member(name),
                                "i",
                                "j",
                            )
                            .addStatement("%N = %N", "i", "j")
                            .endControlFlow()
                            .addStatement("%N * %N", "numeric", "length")
                            .endControlFlow()
                            .build()
                    }
                )
                .addType(
                    TypeSpec.companionObjectBuilder()
                        .addProperty(
                            PropertySpec.builder("part1", LONG_ARRAY)
                                .addModifiers(KModifier.PRIVATE)
                                .initializer(
                                    "%M(%L)",
                                    MemberName("kotlin", "longArrayOf"),
                                    lut(2).joinToCode(",♢") { CodeBlock.of("%L", it) },
                                )
                                .build()
                        )
                        .addProperty(
                            PropertySpec.builder("part2", LONG_ARRAY)
                                .addModifiers(KModifier.PRIVATE)
                                .initializer(
                                    "%M(%L)",
                                    MemberName("kotlin", "longArrayOf"),
                                    lut(25).joinToCode(",♢") { CodeBlock.of("%L", it) },
                                )
                                .build()
                        )
                        .build()
                )
                .build()
        )
        .build()
        .writeTo(outputDir)
}

private val keypad = intArrayOf(
    3 * 1 + 1, // 0
    3 * 2 + 0, // 1
    3 * 2 + 1, // 2
    3 * 2 + 2, // 3
    3 * 3 + 0, // 4
    3 * 3 + 1, // 5
    3 * 3 + 2, // 6
    3 * 4 + 0, // 7
    3 * 4 + 1, // 8
    3 * 4 + 2, // 9
    3 * 1 + 2, // A
)
private const val BLANK = 3 * 1 + 0
private const val UP = 3 * 1 + 1
private const val A = 3 * 1 + 2
private const val LEFT = 3 * 0 + 0
private const val DOWN = 3 * 0 + 1
private const val RIGHT = 3 * 0 + 2

private fun lut(depth: Int): List<Long> {
    var lut = Array(15 * 15) {
        val p1 = it / 15
        val p2 = it % 15
        if (p1 == BLANK || p2 == BLANK) null else abs(p2 / 3 - p1 / 3) + abs(p2 % 3 - p1 % 3) + 1L
    }
    fun best(p1: Int, p2: Int, pos: Int): Long? {
        if (p1 == BLANK || p2 == BLANK) return null
        if (p1 == p2) return lut[15 * pos + A]
        val v = when {
            p1 / 3 < p2 / 3 -> lut[15 * pos + UP]?.let { best(p1 + 3, p2, UP)?.let(it::plus) }
            p1 / 3 > p2 / 3 -> lut[15 * pos + DOWN]?.let { best(p1 - 3, p2, DOWN)?.let(it::plus) }
            else -> null
        }
        val h = when {
            p1 % 3 < p2 % 3 -> lut[15 * pos + RIGHT]?.let { best(p1 + 1, p2, RIGHT)?.let(it::plus) }
            p1 % 3 > p2 % 3 -> lut[15 * pos + LEFT]?.let { best(p1 - 1, p2, LEFT)?.let(it::plus) }
            else -> null
        }
        return if (v != null && h != null) minOf(v, h) else v ?: h
    }
    repeat(depth) { lut = Array(15 * 15) { best(it / 15, it % 15, A) } }
    return List(11 * 11) { lut[15 * keypad[it / 11] + keypad[it % 11]]!! }.also {
        println(it.chunked(11).joinToString(",\n", "Day21[$depth] = [\n", "]") { it.joinToString() })
    }
}
