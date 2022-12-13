import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day13KtTest {
    private val testInput =
        """
            [1,1,3,1,1]
            [1,1,5,1,1]

            [[1],[2,3,4]]
            [[1],4]

            [9]
            [[8,7,6]]

            [[4,4],4,4]
            [[4,4],4,4,4]

            [7,7,7,7]
            [7,7,7]

            []
            [3]

            [[[]]]
            [[]]

            [1,[2,[3,[4,[5,6,7]]]],8,9]
            [1,[2,[3,[4,[5,6,0]]]],8,9]
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day13.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(13, day13.part1(testInput))

    @Test
    fun `test part 1 with custom examples`() {
        val testInput =
            """
                [[0, 7],[6]]
                [[7],[4]]
                
                [[0,5,[[]],[10,1,[1],[],7]],[6,10,7,10]]
                [[7],[4]]
                
                [[],[[[]],9,[],1],[1,0]]
                [[[10,8,0,[4,3],[1,4]],4,[7,5,[9,10,7,10],8]],[8,[[1,5,9,1],6,[10,5],5,[7,1]]],[[0,7,3],[[5],[8,3,0],4]],[4,2]]
                
                [[1],[5]]
                [[2],4]
                
                [[],[10,6,3,[[2,2],4,2,[7,5,10,5]],[9,3]],[[],[7,7,6,[10,6,2,0],[7,10,5,2,7]]]]
                [[],[3,[10,[3,3,8,2,4],6,[10,3,10,10],3]]]
            """.trimIndent()
        assertEquals(10, day13.part1(testInput))
    }

    @Test
    fun `test part 1 with my input`() = assertEquals(5252, day13.part1(myInput)) // 150 pairs

    @Test
    fun `test part 2 with example`() = assertEquals(140, day13.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(20592, day13.part2(myInput))
}
