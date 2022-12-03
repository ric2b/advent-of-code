package day01

fun part1(input: String): Long {
    val elfCaloriesCarried: List<List<String>> = input.split("\n\n").map { it.lines() }
    val elfTotals = elfCaloriesCarried.map { elf -> elf.sumOf { it.toLong() } }
    return elfTotals.max()
}

fun part2(input: String): Long {
    val elfCaloriesCarried: List<List<String>> = input.split("\n\n").map { it.lines() }
    val elfTotals = elfCaloriesCarried.map { elf -> elf.sumOf { it.toLong() } }
    return elfTotals.sortedDescending().take(3).sum()
}