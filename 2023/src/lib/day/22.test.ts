import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2} from './22';

describe('part 1', () => {
	it('calculates the right value for example', () => {
		const raw_input = `
			1,0,1~1,2,1
			0,0,2~2,0,2
			0,2,3~2,2,3
			0,0,4~0,2,4
			2,0,5~2,2,5
			0,1,6~2,1,6
			1,1,8~1,1,9
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(5);
	});

	it('calculates the right value for example 1', () => {
		// just Ken
		const raw_input = `
			0,0,1~0,0,1
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 2', () => {
		// B on top of A
		const raw_input = `
			0,0,1~0,0,1
			0,0,2~0,0,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 3', () => {
		// C on top of B on top of A
		const raw_input = `
			0,0,1~0,0,1
			0,0,2~0,0,2
			0,0,2~0,0,3
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 4', () => {
		// B, C, D and E on top of A
		const raw_input = `
			0,0,1~1,1,1
			
			0,0,2~0,0,2
			0,1,2~0,1,2
			1,0,2~1,0,2
			1,1,2~1,1,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example "owl-bear from the top rope"', () => {
		// owl-bear on top of B, C, D and E on top of A
		// only A can't be disintegrated
		const raw_input = `
			0,0,1~1,1,1
			
			0,0,2~0,0,2
			0,1,2~0,1,2
			1,0,2~1,0,2
			1,1,2~1,1,2
			
			0,0,9001~1,1,9001
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(5);
	});

	it('calculates the right value for example 6', () => {
		// C on top of B on top of A
		// C is offset of A but B is a wider square that connects them
		const raw_input = `
			0,0,1~0,0,1
			0,0,10~1,1,10
			1,1,20~1,1,20
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 7', () => {
		// D on top of B on top of A
		// C is just hanging out on top of A because B is taller and holds C by itself
		const raw_input = `
			0,0,1~1,1,1
			0,0,2~0,0,3
			1,1,2~1,1,2
			0,0,4~1,1,4
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 8', () => {
		// C on top of B and A
		const raw_input = `
			0,0,1~1,1,1
			2,2,1~3,3,1
			0,0,2~3,3,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for example 9', () => {
		// C on top of A
		// B is just hanging out because A is taller
		const raw_input = `
			0,0,1~1,1,5
			2,2,1~3,3,1
			0,0,6~3,3,8
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 10', () => {
		// C on top of A
		// B is just hanging out because A is taller
		const raw_input = `
			0,0,1~1,1,5
			2,2,1~3,3,1
			0,0,6~3,3,8
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 11', () => {
		//   DE(FG)
		//   CC
		//  BB
		// AA
		const raw_input = `
			0,0,2~1,1,2
			1,1,4~2,2,4
			2,2,6~3,3,6
			2,2,7~2,2,7
			2,3,7~2,3,7
			3,2,7~3,2,7
			3,3,7~3,3,7
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example 12a', () => {
		// level 1:
		//  A..B
		//  .EF.
		//  .GH.
		//  C..D
		// level 2:
		//  IIII
		//  IIII
		//  IIII
		//  IIII

		const raw_input = `
			0,0,2~0,0,2
			0,3,3~0,3,3
			3,0,4~3,0,4
			3,3,5~3,3,5
			
			1,1,7~1,1,7
			1,2,7~1,2,7
			2,1,7~2,1,7
			2,2,7~2,2,7
			
			0,0,9~3,3,9
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(9);
	});

	it('calculates the right value for example 12b', () => {
		// level 1:
		//  A.B
		//  ...
		//  C.D
		// level 2:
		//  III
		//  III
		//  III

		const raw_input = `
			0,0,2~0,0,2
			0,2,3~0,2,3
			2,0,4~2,0,4
			2,2,5~2,2,5
			
			0,0,9~2,2,9
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(5);
	});

	it('calculates the right value for example 12c', () => {
		// level 1:
		//  A..B
		//  ....
		//  ....
		//  C..D
		// level 2:
		//  JJJJ
		//  JJJJ
		//  JJJJ
		//  JJJJ
		// level 3:
		//  ....
		//  .EF.
		//  .GH.
		//  ....
		// level 4:
		//  IIII
		//  IIII
		//  IIII
		//  IIII

		const raw_input = `
			0,0,2~0,0,2
			0,3,3~0,3,3
			3,0,4~3,0,4
			3,3,5~3,3,5
			
			0,0,6~3,3,6
			
			1,1,7~1,1,7
			1,2,7~1,2,7
			2,1,7~2,1,7
			2,2,7~2,2,7
			
			1,1,9~2,2,9
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(9);
	});

	it('calculates the right value for example 13', () => {
		// A B
		const raw_input = `
			0,0,2~0,0,2
			1,1,2~1,1,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 13b', () => {
		// A B C
		const raw_input = `
			0,0,2~0,0,2
			1,1,2~1,1,2
			2,2,2~2,2,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for example 14', () => {
		// Layer 1 and 2
		// 012 x
		// AA  y0
		// AA  y1
		// .BB y2
		// .BB y3
		// Layer 3
		// 012 x
		// .CC y0
		// .CC y1
		// ... y2
		// ... y3
		const raw_input = `
			0,0,1~1,1,20
			1,2,1~2,3,20
			
			1,0,30~2,1,30
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 15', () => {
		const raw_input = `
			0,0,1~1,1,1
			2,2,2~3,3,2
			0,0,2~3,3,3
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for example 16', () => {
		//x012
		// A.. y0
		// AB. y1
		//x012
		// C. y0
		// DD y1

		// A is required to support C
		const raw_input = `
			0,0,1~0,1,1
			1,1,1~1,1,1
			0,0,2~0,0,2
			0,1,2~1,1,2
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/22.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 1393 is too high
		// 1391 is too high
		// 526 is too high

		expect(part1(raw_input)).toBe(457);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		// A supports B and C
		// B and C support D and E
		// D and E support F
		// F supports G
		const raw_input = `
			1,0,1~1,2,1
			0,0,2~2,0,2
			0,2,3~2,2,3
			0,0,4~0,2,4
			2,0,5~2,2,5
			0,1,6~2,1,6
			1,1,8~1,1,9
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(7);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '22.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(79122);
	});
});
