import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2} from './22';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
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

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/22.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 1393 is too high

		expect(part1(raw_input)).toBe(3768);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			1,0,1~1,2,1
			0,0,2~2,0,2
			0,2,3~2,2,3
			0,0,4~0,2,4
			2,0,5~2,2,5
			0,1,6~2,1,6
			1,1,8~1,1,9
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(391791381003720);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '22.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(627960775905777);
	});
});
