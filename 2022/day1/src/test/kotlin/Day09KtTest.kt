import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day09KtTest {
    private val testInput =
        """
            R 4
            U 4
            L 3
            D 1
            R 4
            D 1
            L 5
            R 2
        """.trimIndent()

    @Test
    fun testPart1() = assertEquals(13, day09.part1(testInput))

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day09.txt")!!.readText()
        assertEquals(6209, day09.part1(myInput))
    }

    @Test
    fun testPart2() {
        assertEquals(1, day09.part2(testInput))

        val largerInput =
            """
                R 5
                U 8
                L 8
                D 3
                R 17
                D 10
                L 25
                U 20
        """.trimIndent()

        assertEquals(36, day09.part2(largerInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day09.txt")!!.readText()
        assertEquals(2460, day09.part2(myInput))
    }
}
