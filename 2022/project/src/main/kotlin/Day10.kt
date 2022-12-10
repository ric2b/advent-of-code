package day10

fun part1(input: String): Int {
    val ops: List<Int?> = input.trimEnd().lines().map { line ->
        Regex("addx (-?\\d+)").find(line)?.groupValues?.last()?.toInt()
    }

    val xValues: MutableList<Int> = mutableListOf(1)
    ops.forEach { value ->
        xValues.add(xValues.last())
        if (value !== null) { xValues.add(xValues.last() + value) }
    }

    return listOf(20, 60, 100, 140, 180, 220).sumOf { i -> i * xValues[i - 1] }
}

fun part2(input: String): String {
    val ops: List<Int?> = input.trimEnd().lines().map { line ->
        Regex("addx (-?\\d+)").find(line)?.groupValues?.last()?.toInt()
    }

    val xValues: MutableList<Int> = mutableListOf(1)
    ops.forEach { value ->
        xValues.add(xValues.last())
        if (value !== null) { xValues.add(xValues.last() + value) }
    }

    val output: MutableList<Char> = mutableListOf()
    for (i in 0 until 40*6) {
        if (i % 40 in xValues[i] - 1..xValues[i] + 1) output.add('#') else output.add(' ')
        if (i > 0 && (i+1) % 40 == 0) output.add('\n')
    }

    return output.dropLast(1).joinToString("")
}