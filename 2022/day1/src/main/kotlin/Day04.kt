package day04

fun part1(input: String): Int {
    val pairs: List<Pair<IntRange, IntRange>> = input.trimEnd().lines().map { rawPair ->
        val (elfA, elfB) = rawPair.split(',').map { rawRange ->
            val (a, b) = rawRange.split('-')
            a.toInt()..b.toInt()
        }
        Pair(elfA, elfB)
    }

    return pairs.count { (elfA, elfB) -> elfA.all { elfB.contains(it) } || elfB.all { elfA.contains(it) } }
}

fun part2(input: String): Int {
    val pairs: List<Pair<IntRange, IntRange>> = input.trimEnd().lines().map { rawPair ->
        val (elfA, elfB) = rawPair.split(',').map { rawRange ->
            val (a, b) = rawRange.split('-').map { it.toInt() }
            IntRange(a, b)
        }
        Pair(elfA, elfB)
    }

    return pairs.count { (elfA, elfB) -> elfA.any { elfB.contains(it) } || elfB.any { elfA.contains(it) } }
}