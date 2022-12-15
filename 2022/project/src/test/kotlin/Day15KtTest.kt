import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day15KtTest {
    private val testInput =
        """
            Sensor at x=2, y=18: closest beacon is at x=-2, y=15
            Sensor at x=9, y=16: closest beacon is at x=10, y=16
            Sensor at x=13, y=2: closest beacon is at x=15, y=3
            Sensor at x=12, y=14: closest beacon is at x=10, y=16
            Sensor at x=10, y=20: closest beacon is at x=10, y=16
            Sensor at x=14, y=17: closest beacon is at x=10, y=16
            Sensor at x=8, y=7: closest beacon is at x=2, y=10
            Sensor at x=2, y=0: closest beacon is at x=2, y=10
            Sensor at x=0, y=11: closest beacon is at x=2, y=10
            Sensor at x=20, y=14: closest beacon is at x=25, y=17
            Sensor at x=17, y=20: closest beacon is at x=21, y=22
            Sensor at x=16, y=7: closest beacon is at x=15, y=3
            Sensor at x=14, y=3: closest beacon is at x=15, y=3
            Sensor at x=20, y=1: closest beacon is at x=15, y=3
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day15.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(26, day15.part1(testInput, 10))

    @Test
    fun `test part 1 with my input`() = assertEquals(5040643, day15.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(56000011, day15.part2(testInput, 0..20))

    @Test
    fun `test part 2 with my input`() = assertEquals(11016575214126, day15.part2(myInput))

    @Test
    fun rangeIntersection() {
        val ranges = listOf(IntRange(1, 5), IntRange(3, 7), IntRange(6, 9))


        val sortedRanges = ranges.sortedBy { it.start }
        val reducedRanges: MutableList<IntRange> = mutableListOf(sortedRanges.first())

        sortedRanges.drop(1).forEach {
            val last = reducedRanges.removeLast()
            if (it.start <= last.endInclusive && last.endInclusive <= it.endInclusive) {
                reducedRanges.add(last.start..it.endInclusive)
            } else {
                reducedRanges.add(last)
                reducedRanges.add(it)
            }
        }

        assertEquals(listOf(1..9), reducedRanges)
    }
}
