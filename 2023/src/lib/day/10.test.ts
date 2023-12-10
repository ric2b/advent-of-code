import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './10';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			.....
			.S-7.
			.|.|.
			.L-J.
			.....
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			..F7.
			.FJ|.
			SJ.L7
			|F--J
			LJ...
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(8);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			L-J
			7S7
			LJL
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/10.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(6864);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `

        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(2);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '10.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(1012);
	});
});
