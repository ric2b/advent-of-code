import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './09';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			0 3 6 9 12 15
			1 3 6 10 15 21
			10 13 16 21 30 45
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(114);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/09.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(1992273652);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			0 3 6 9 12 15
			1 3 6 10 15 21
			10 13 16 21 30 45
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(2);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '09.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(1012);
	});
});
