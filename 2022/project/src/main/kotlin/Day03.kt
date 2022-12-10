package day03

fun part1(input: String): Int {
    val rucksacks: List<Pair<List<Char>, List<Char>>> = input.trimEnd().lines().map { rawRucksack ->
        val firstHalf: CharSequence = rawRucksack.subSequence(0, rawRucksack.length / 2)
        val secondHalf: CharSequence = rawRucksack.subSequence( rawRucksack.length / 2, rawRucksack.length)
        Pair(
            rawRucksack.subSequence(0, rawRucksack.length / 2).toList(),
            rawRucksack.subSequence( rawRucksack.length / 2, rawRucksack.length).toList()
        )
    }

    return rucksacks.sumOf { rucksack ->
        val commonItem = rucksack.first.intersect(rucksack.second).first()
        if (commonItem.code > 91) commonItem.code - 'a'.code + 1 else commonItem.code - 'A'.code + 27
    }
}

fun part2(input: String): Int {
    val rucksackGroups: List<List<String>> = input.trimEnd().lines().chunked(3)
    return rucksackGroups.sumOf { (a, b, c) ->
        val commonItem = a.toList().intersect(b.toList()).intersect(c.toList()).first()
        if (commonItem.code > 91) commonItem.code - 'a'.code + 1 else commonItem.code - 'A'.code + 27
    }
}