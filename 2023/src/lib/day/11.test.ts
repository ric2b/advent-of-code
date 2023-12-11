import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './11';

describe('part 1', () => {
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

		expect(part1(raw_input)).toBe(374);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/11.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(10165598);
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
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '11.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(678728808158);
	});
});
