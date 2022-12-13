package day13

import org.json.JSONArray
import java.lang.Integer.min

fun orderedPackets(leftPacket: JSONArray, rightPacket: JSONArray): Boolean? {
    for (i in 0 until min(leftPacket.length(), rightPacket.length())) {
        val left = try { leftPacket.getInt(i) } catch (e: org.json.JSONException) { leftPacket.getJSONArray(i) }
        val right = try { rightPacket.getInt(i) } catch (e: org.json.JSONException) { rightPacket.getJSONArray(i) }

        if(left is Int && right is Int) { // If both values are integers, the lower integer should come first.
            // If the left integer is lower than the right integer, the inputs are in the right order.
            // If the left integer is higher than the right integer, the inputs are not in the right order.
            if (left != right) return left < right
            // Otherwise, the inputs are the same integer; continue checking the next part of the input.
        } else if (left is Int && right is JSONArray) { // If exactly one value is an integer,
            orderedPackets(JSONArray(listOf(left)), right)?.let { return it } // convert the integer to a list which contains that integer as its only value, then retry the comparison.
        } else if (left is JSONArray && right is Int) { // If exactly one value is an integer,
            orderedPackets(left, JSONArray(listOf(right)))?.let { return it } // convert the integer to a list which contains that integer as its only value, then retry the comparison.
        } else if (left is JSONArray && right is JSONArray) { // If both values are lists, compare the first value of each list, then the second value, and so on.
            orderedPackets(left, right)?.let { return it }

        }
    }

    if (leftPacket.length() < rightPacket.length()) return true // If the left list runs out of items first, the inputs are in the right order.
    if (leftPacket.length() > rightPacket.length()) return false // If the right list runs out of items first, the inputs are not in the right order.
    // If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
    return null
}

fun part1(input: String): Int {
    val pairs: List<Pair<JSONArray, JSONArray>> = input.trimEnd().split("\n\n").map {
        val (packetA, packetB) = it.lines()
        Pair(JSONArray(packetA), JSONArray(packetB))
    }

    val validIndexes = pairs.mapIndexedNotNull { i, pair -> if (orderedPackets(pair.first, pair.second)!!) i + 1 else null }

    return validIndexes.sum()
}

fun part2(input: String): Int {
    val pairs: List<Pair<JSONArray, JSONArray>> = input.trimEnd().split("\n\n").map {
        val (packetA, packetB) = it.lines()
        Pair(JSONArray(packetA), JSONArray(packetB))
    }

    val packets = pairs.flatMap { listOf(it.first, it.second) }
    val dividerPackets = listOf(JSONArray("[[2]]"), JSONArray("[[6]]"))

    val sortedPackets = (packets + dividerPackets).sortedWith { left, right ->
        when (orderedPackets(left, right)) {
            true -> -1
            null -> 0
            false -> 1
        }
    }

    return dividerPackets.map { sortedPackets.indexOf(it) + 1 }.reduce { a, b -> a * b}
}