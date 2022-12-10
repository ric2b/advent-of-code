import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day01KtTest {
    private val testInput =
        """
            1000
            2000
            3000

            4000

            5000
            6000

            7000
            8000
            9000

            10000
        """.trimIndent()
    private val myInput = this::class.java.classLoader.getResource("day01.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(24000, day01.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(69836, day01.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(45000, day01.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(207968, day01.part2(myInput))
}