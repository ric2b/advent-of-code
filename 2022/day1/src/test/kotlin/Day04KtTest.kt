import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day04KtTest {
    private val testInput =
        """
            2-4,6-8
            2-3,4-5
            5-7,7-9
            2-8,3-7
            6-6,4-6
            2-6,4-8
        """.trimIndent()
    private val myInput = this::class.java.classLoader.getResource("day04.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(2, day04.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(644, day04.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(4, day04.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(926, day04.part2(myInput))
}