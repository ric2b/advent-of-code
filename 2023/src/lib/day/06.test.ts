import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './06';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            Time:      7  15   30
            Distance:  9  40  200
        `.replace(/^ +/gm, '');

		expect(part1(raw_input)).toBe(288);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/06.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(293046);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            Time:      7  15   30
            Distance:  9  40  200
        `.replace(/^ +/gm, '');

		expect(part2(raw_input)).toBe(71503);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '06.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(35150181);
	});
});
