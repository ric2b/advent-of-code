import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './12';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			???.### 1,1,3
			.??..??...?##. 1,1,3
			?#?#?#?#?#?#?#? 1,3,1,6
			????.#...#... 4,1,1
			????.######..#####. 1,6,5
			?###???????? 3,2,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(21);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/12.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(7916);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			...#......
			.......#..
			#.........
			..........
			......#...
			.#........
			.........#
			..........
			.......#..
			#...#.....
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input, 10)).toBe(1030);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			...#......
			.......#..
			#.........
			..........
			......#...
			.#........
			.........#
			..........
			.......#..
			#...#.....
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input, 100)).toBe(8410);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '12.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(678728808158);
	});
});
