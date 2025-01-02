package com.github.ephemient.aoc2024.codegen

import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.INT_ARRAY
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.TypeSpec
import com.squareup.kotlinpoet.joinToCode
import java.nio.file.Path

fun day22(outputDir: Path) {
    FileSpec.builder("com.github.ephemient.aoc2024", "Day22Constants")
        .addType(
            TypeSpec.objectBuilder("Day22Constants")
                .addModifiers(KModifier.INTERNAL)
                .addProperty(
                    PropertySpec.builder("part1", INT_ARRAY)
                        .getter(
                            FunSpec.getterBuilder()
                                .addCode(
                                    "return %M(%L)",
                                    MemberName("kotlin", "intArrayOf"),
                                    List(24) {
                                        var secret = 1 shl it
                                        repeat(2000) {
                                            val one = secret shl 6 xor secret and 16777215
                                            val two = one shr 5 xor one and 16777215
                                            secret = two shl 11 xor two and 16777215
                                        }
                                        println(
                                            "Day22.part1(1 << ${it.toString().padStart(2)}) = 0b${
                                                secret.toString(2).padStart(24, '0')
                                            }"
                                        )
                                        secret
                                    }.joinToCode(",♢") { CodeBlock.of("%L", "0x%06x".format(it)) }
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
