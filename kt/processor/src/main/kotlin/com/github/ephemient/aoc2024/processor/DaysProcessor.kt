package com.github.ephemient.aoc2024.processor

import com.google.devtools.ksp.isAbstract
import com.google.devtools.ksp.isPublic
import com.google.devtools.ksp.processing.CodeGenerator
import com.google.devtools.ksp.processing.Dependencies
import com.google.devtools.ksp.processing.JvmPlatformInfo
import com.google.devtools.ksp.processing.PlatformInfo
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.symbol.ClassKind
import com.google.devtools.ksp.symbol.KSAnnotated
import com.google.devtools.ksp.symbol.KSClassDeclaration
import com.google.devtools.ksp.symbol.KSFile
import com.google.devtools.ksp.symbol.KSFunctionDeclaration
import com.squareup.kotlinpoet.AnnotationSpec
import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.LIST
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.TypeName
import com.squareup.kotlinpoet.joinToCode
import com.squareup.kotlinpoet.ksp.toClassName
import com.squareup.kotlinpoet.ksp.writeTo

class DaysProcessor(
    private val platforms: List<PlatformInfo>,
    private val codeGenerator: CodeGenerator,
) : SymbolProcessor {
    private val days = mutableListOf<Day>()

    override fun process(resolver: Resolver): List<KSAnnotated> {
        resolver.getNewFiles()
            .flatMap { it.declarations.map(it::to) }
            .mapNotNullTo(days) { (source, declaration) ->
                if (
                    declaration !is KSClassDeclaration ||
                    !declaration.isPublic() ||
                    declaration.isAbstract() ||
                    declaration.classKind != ClassKind.CLASS
                ) return@mapNotNullTo null
                classNamePattern.matchEntire(declaration.simpleName.asString())?.let { match ->
                    val (name, day) = match.destructured
                    val typeName = declaration.toClassName()
                    Day(
                        source = source,
                        day = day.toInt(),
                        name = name,
                        typeName = typeName,
                        parts = declaration.declarations
                            .mapNotNull { declaration ->
                                if (
                                    declaration !is KSFunctionDeclaration ||
                                    !declaration.isPublic()
                                ) return@mapNotNull null
                                val memberName = declaration.simpleName.asString()
                                if (memberName == "solve") return@mapNotNull Part(
                                    part = Int.MAX_VALUE,
                                    name = memberName,
                                    memberName = memberName
                                )
                                functionNamePattern.matchEntire(memberName)?.let { match ->
                                    val (name, part) = match.destructured
                                    Part(
                                        part = part.toInt(),
                                        name = name,
                                        memberName = memberName,
                                    )
                                }
                            }
                            .toList()
                            .sortedWith(compareBy(Part::part, Part::name))
                    )
                }
            }
        return emptyList()
    }

    override fun finish() {
        FileSpec.builder(PACKAGE, "Days")
            .addAnnotations(
                if (platforms.any { it is JvmPlatformInfo }) listOf(
                    AnnotationSpec.builder(ClassName("kotlin.jvm", "JvmName"))
                        .addMember("%S", "JvmDays")
                        .build()
                ) else emptyList()
            )
            .addProperty(
                PropertySpec.builder("days", LIST.parameterizedBy(ClassName(PACKAGE, "Day")))
                    .addModifiers(KModifier.ACTUAL)
                    .initializer(
                        "%M(\n%L)",
                        MemberName("kotlin.collections", "listOf"),
                        days.sortedWith(compareBy(Day::day, Day::name)).joinToCode(",\n") { day ->
                            CodeBlock.of(
                                "%T(%L)",
                                ClassName(PACKAGE, "Day"),
                                listOfNotNull(
                                    CodeBlock.of("%N = %L", "day", day.day),
                                    CodeBlock.of("::%T", day.typeName),
                                    *Array(day.parts.size) {
                                        CodeBlock.of("%T::%N", day.typeName, day.parts[it].memberName)
                                    },
                                    CodeBlock.of("%N = %S", "name", day.name),
                                ).joinToCode(),
                            )
                        }
                    )
                    .build()
            )
            .build()
            .writeTo(codeGenerator, Dependencies(aggregating = true, sources = Array(days.size) { days[it].source }))
    }

    private data class Day(
        val source: KSFile,
        val day: Int,
        val name: String,
        val typeName: TypeName,
        val parts: List<Part>,
    )

    private data class Part(
        val part: Int,
        val name: String,
        val memberName: String,
    )

    companion object {
        private const val PACKAGE = "com.github.ephemient.aoc2024"
        private val classNamePattern = """Day((\d+).*)""".toRegex()
        private val functionNamePattern = """part((\d+).*)""".toRegex()
    }
}
