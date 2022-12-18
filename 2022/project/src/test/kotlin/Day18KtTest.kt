import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day18KtTest {
    private val testInput =
        """
            2,2,2
            1,2,2
            3,2,2
            2,1,2
            2,3,2
            2,2,1
            2,2,3
            2,2,4
            2,2,6
            1,2,5
            3,2,5
            2,1,5
            2,3,5
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day18.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(64, day18.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(4450, day18.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(58, day18.part2(testInput))

    @Test
    fun `test part 2 with custom example`() {
        val testInput =
            """
            2,2,2
            1,2,2
            3,2,2
            2,1,2
            2,3,2
            2,2,1
            2,2,3
            2,2,6
            1,2,5
            3,2,5
            2,1,5
            2,3,5
            1,2,4
            3,2,4
            2,1,4
            2,3,4
        """.trimIndent()
        assertEquals(66, day18.part2(testInput))
    }

    @Test
    fun `test part 2 with my input`() = assertEquals(2564, day18.part2(myInput))
}
