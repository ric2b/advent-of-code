package day16

import kotlin.math.max
import kotlin.math.min

typealias ValveId = String
data class Valve(val id: ValveId, val flowRate: Int, val connectedTo: List<ValveId>)

fun part1(input: String): Int {
    val graph: Map<ValveId, Valve> = input.trimEnd().lines().associate { line ->
        Regex("Valve (?<valve>\\w+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? ([\\w,\\s]+)+")
            .findAll(line).first().let {
                val (_, valveId, rawFlowRate, rawConnectedValves) = it.groupValues
                valveId to Valve(valveId, rawFlowRate.toInt(), rawConnectedValves.split(", "))
            }
    }

    val minutesLeft = 30
    val distances = calculateDistances(graph)

    val start = distances.keys.find { it.id == "AA" }!!
    val closedValves = distances.keys.filter { it.flowRate > 0 }.toSet()

    val possibleOpeningOrders = validOpeningOrders(distances, start, closedValves, minutesLeft).distinct()
    return possibleOpeningOrders.maxOf { openingOrder -> scoreOpeningOrder(distances, start, openingOrder, minutesLeft) }
}

fun part2(input: String): Int {
    val graph: Map<ValveId, Valve> = input.trimEnd().lines().associate { line ->
        Regex("Valve (?<valve>\\w+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? ([\\w,\\s]+)+")
            .findAll(line).first().let {
                val (_, valveId, rawFlowRate, rawConnectedValves) = it.groupValues
                valveId to Valve(valveId, rawFlowRate.toInt(), rawConnectedValves.split(", "))
            }
    }

    val minutesLeft = 26
    val distances = calculateDistances(graph)

    val start = distances.keys.find { it.id == "AA" }!!
    val closedValves = distances.keys.filter { it.flowRate > 0 }.toSet()

    val possibleOpeningOrders = validOpeningOrders(distances, start, closedValves, minutesLeft).distinct()

    val bestScores: MutableMap<Set<Valve>, Int> = mutableMapOf()
    possibleOpeningOrders.forEach {
        val asSet = it.toSet()
        bestScores[asSet] = max(bestScores.getOrDefault(asSet, 0), scoreOpeningOrder(distances, start, it, minutesLeft))
    }

    val bestScoresList = bestScores.toList().sortedByDescending { it.second }
    var bestCombinedScore = 0
    bestScoresList.forEachIndexed { humanIndex, human ->
        val elephantScores = bestScoresList.drop(humanIndex)
        if (human.second + elephantScores.first().second > bestCombinedScore) {
            elephantScores.forEach { elephant ->
                if (human.first.intersect(elephant.first).isEmpty()) {
                    bestCombinedScore = max(bestCombinedScore, human.second + elephant.second)
                }
            }
        }
    }
    return bestCombinedScore
}

// aka Floyd-Warshall
fun calculateDistances(graph: Map<ValveId, Valve>): Map<Valve, Map<Valve, Int>> {
    val matrix: Map<Valve, MutableMap<Valve, Int>> = graph.values.associateWith { source ->
        graph.values.associateWith { destination ->
            when (destination.id) {
                source.id -> 0
                in source.connectedTo -> 1
                else -> 2 * graph.size // "infinity" with no overflow risk
            }
        }.toMutableMap()
    }

    matrix.keys.forEach { b ->
        matrix.forEach { (a, distances) ->
            distances.forEach { (c, distance) ->
                distances[c] = min(matrix.getValue(a).getValue(b) + matrix.getValue(b).getValue(c), distance)
            }
        }
    }

    return matrix
}

fun validOpeningOrders(distances: Map<Valve, Map<Valve, Int>>, location: Valve, closedValves: Set<Valve>, minutesLeft: Int): Collection<List<Valve>> {
    return when {
        closedValves.isEmpty() || minutesLeft < 0 -> listOf(emptyList())
        else -> closedValves.flatMap { nextValve ->
            val minutesSpent = 1 + distances.getValue(location).getValue(nextValve)
            if (minutesSpent > minutesLeft) {
                listOf(emptyList())
            } else {
                listOf(emptyList<Valve>()) +
                validOpeningOrders(distances, nextValve, closedValves - nextValve, minutesLeft - minutesSpent)
                    .map { listOf(nextValve) + it }.distinct()
            }
        }
    }
}

fun scoreOpeningOrder(distances: Map<Valve, Map<Valve, Int>>, start: Valve, openingOrder: List<Valve>, totalMinutes: Int): Int {
    var minutesLeft = totalMinutes
    var pressureReleased = 0
    var location = start

    for (nextValve in openingOrder) {
        minutesLeft -= 1 + distances.getValue(location).getValue(nextValve)
        pressureReleased += nextValve.flowRate * minutesLeft
        location = nextValve
    }

    return pressureReleased
}
