import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2, part2b} from './21';

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

		expect(part2(raw_input)).toBe(391791381003720);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '21.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 21628988986748133000 too high!
		// 9635477433421572000 too high!
		// 151104306597970460 not right
		// 151102812745422370 not right
		// 78636785046373 too low

		expect(part2(raw_input)).toBe(627960775905777);
	});
});
