import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './01';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet
        `.replace(/^\s+/gm, '');

		expect(part1(raw_input)).toBe(142);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/01.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(55172);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            two1nine
            eightwothree
            abcone2threexyz
            xtwone3four
            4nineeightseven2
            zoneight234
            7pqrstsixteen
        `.replace(/^\s+/gm, '');

		expect(part2(raw_input)).toBe(281);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '01.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(54925);
	});
});
