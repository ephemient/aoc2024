import com.google.devtools.ksp.gradle.KspAATask
import com.google.devtools.ksp.gradle.KspTaskNative
import org.gradle.internal.extensions.stdlib.capitalized
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinMetadataTarget

plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.ksp)
    alias(libs.plugins.detekt)
}

kotlin {
    jvm {
        val codegen by compilations.creating
        val runCodegen by tasks.registering(JavaExec::class) {
            mainClass = "com.github.ephemient.aoc2024.codegen.Main"
            classpath(codegen.output.allOutputs, codegen.runtimeDependencyFiles)
            outputs.dir(layout.buildDirectory.dir("build/sources/codegen"))
            argumentProviders.add { listOf(outputs.files.singleFile.path) }
        }
        kotlin.sourceSets.getByName("commonMain").kotlin.srcDirs(runCodegen)
    }
    wasmJs {
        browser()
        nodejs()
    }
    wasmWasi {
        nodejs()
    }
    js {
        browser()
        nodejs()
    }
    linuxArm64()
    linuxX64()
    macosArm64()
    macosX64()
    mingwX64()

    applyDefaultHierarchyTemplate {
        common {
            group("nonJvm") {
                withWasmJs()
                withWasmWasi()
                withJs()
                withNative()
            }
        }
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(libs.kotlinx.coroutines)
            }
        }

        commonTest {
            dependencies {
                implementation(kotlin("test"))
                implementation(libs.kotlinx.coroutines.test)
            }
        }

        jvmTest {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }

        getByName("jvmCodegen") {
            dependencies {
                implementation(libs.kotlinpoet)
            }
        }
    }

    val detektTaskNames = targets.flatMap { target ->
        target.compilations.map { compilation ->
            "detekt${target.targetName.capitalized()}${compilation.compilationName.capitalized()}"
        }
    }
    tasks.check {
        dependsOn(detektTaskNames)
    }
}

val jvmTestCompilation = kotlin.jvm().compilations.getByName("test")
val jvmTestJar by tasks.registering(Jar::class) {
    group = BasePlugin.BUILD_GROUP
    description = "Assembles an archive containing the test classes."
    archiveClassifier = "test"
    from(jvmTestCompilation.output.allOutputs)
}
for ((name, base, usage) in listOf(
    Triple("jvmTestApiElements", "jvmTestApi", Usage.JAVA_API),
    Triple("jvmTestRuntimeElements", "jvmTestRuntimeClasspath", Usage.JAVA_RUNTIME),
)) {
    configurations.consumable(name) {
        extendsFrom(configurations.getByName(base))
        attributes {
            attribute(Category.CATEGORY_ATTRIBUTE, objects.named(Category.LIBRARY))
            attribute(
                TargetJvmEnvironment.TARGET_JVM_ENVIRONMENT_ATTRIBUTE,
                objects.named(TargetJvmEnvironment.STANDARD_JVM),
            )
            attribute(
                LibraryElements.LIBRARY_ELEMENTS_ATTRIBUTE,
                objects.named(LibraryElements.JAR),
            )
            attribute(Usage.USAGE_ATTRIBUTE, objects.named(usage))
        }
        outgoing {
            capability("com.github.ephemient.aoc2024:aoc2024-test:1.0")
            artifact(jvmTestJar) {
                type = ArtifactTypeDefinition.JAR_TYPE
            }
            variants.create("classes") {
                attributes {
                    attribute(
                        LibraryElements.LIBRARY_ELEMENTS_ATTRIBUTE,
                        objects.named(LibraryElements.CLASSES),
                    )
                }
                for (classesDir in jvmTestCompilation.output.classesDirs) {
                    artifact(classesDir) {
                        type = ArtifactTypeDefinition.JVM_CLASS_DIRECTORY
                        builtBy(jvmTestCompilation.compileTaskProvider)
                    }
                }
            }
            variants.create("resources") {
                attributes {
                    attribute(
                        LibraryElements.LIBRARY_ELEMENTS_ATTRIBUTE,
                        objects.named(LibraryElements.RESOURCES),
                    )
                }
                artifact(jvmTestCompilation.output.resourcesDir) {
                    type = ArtifactTypeDefinition.JVM_RESOURCES_DIRECTORY
                    builtBy(jvmTestCompilation.processResourcesTaskName)
                }
            }
        }
    }
}

dependencies {
    kotlin.targets.all { if (this !is KotlinMetadataTarget) "ksp${name.capitalized()}"(projects.processor) }
    detektPlugins(libs.bundles.detekt.plugins)
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}

afterEvaluate {
    // Workaround for https://github.com/google/ksp/issues/2267
    tasks.withType<KspAATask>().configureEach { setOnlyIf(Specs.satisfyAll()) }
    tasks.withType<KspTaskNative>().configureEach { setOnlyIf(Specs.satisfyAll()) }
}
