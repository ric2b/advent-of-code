import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './14';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			OOOO.#.O..
			OO..#....#
			OO..O##..O
			O..#.OO...
			........#.
			..#....#.#
			..O..#.O.O
			..O.......
			#....###..
			#....#....
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(136);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/14.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(109665);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			OOOO.#.O..
			OO..#....#
			OO..O##..O
			O..#.OO...
			........#.
			..#....#.#
			..O..#.O.O
			..O.......
			#....###..
			#....#....
        `.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(64);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '14.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(2);
	});
});
