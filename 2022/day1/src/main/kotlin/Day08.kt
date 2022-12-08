package day08

fun part1(input: String): Int {
    val treeHeights: List<List<Int>> = input.trimEnd().lines().map { treeLine -> treeLine.map { it.digitToInt() } }
    var visibleTrees = 0

    for (row in treeHeights.indices) {
        for (column in treeHeights[row].indices) {
            val treeHeight = treeHeights[row][column]

            if (
                treeHeights[row].slice(0 until column).all { it < treeHeight }
                || treeHeights[row].slice(column+1 until treeHeights[row].size).all { it < treeHeight }
                || treeHeights.slice(0 until row).all { it[column] < treeHeight }
                || treeHeights.slice(row+1 until treeHeights.size).all { it[column] < treeHeight }
            ) {
                visibleTrees += 1
            }
        }
    }

    return visibleTrees
}

fun part2(input: String): Int {
    val treeHeights: List<List<Int>> = input.trimEnd().lines().map { treeLine -> treeLine.map { it.digitToInt() } }
    var maxScore = 0

    for (row in treeHeights.indices) {
        for (column in treeHeights[row].indices) {
            val treeHeight = treeHeights[row][column]

            var toLeft = 0
            for (height in treeHeights[row].slice((0 until column).reversed())) {
                toLeft += 1
                if (height >= treeHeight) break
            }
            var toRight = 0
            for (height in treeHeights[row].slice(column+1 until treeHeights[row].size)) {
                toRight += 1
                if (height >= treeHeight) break
            }
            var toAbove = 0
            for (trees in treeHeights.slice((0 until row).reversed())) {
                toAbove += 1
                if (trees[column] >= treeHeight) break
            }
            var toBelow = 0
            for (trees in treeHeights.slice(row+1 until treeHeights.size)) {
                toBelow += 1
                if (trees[column] >= treeHeight) break
            }

            val treeScore = toLeft * toRight * toAbove * toBelow
            maxScore = listOf(maxScore, treeScore).max()
        }
    }

    return maxScore
}