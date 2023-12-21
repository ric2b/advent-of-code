import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2} from './21';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			...........
			.....###.#.
			.###.##..#.
			..#.#...#..
			....#.#....
			.##..S####.
			.##..#...#.
			.......##..
			.##.#.####.
			.##..##.##.
			...........
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input, 6)).toBe(16);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/21.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(3768);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `

		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '21.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(1);
	});
});
