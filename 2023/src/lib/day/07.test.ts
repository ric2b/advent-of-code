import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './07';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			32T3K 765
			T55J5 684
			KK677 28
			KTJJT 220
			QQQJA 483
        `.replace(/^ +/gm, '');

		expect(part1(raw_input)).toBe(6440);
	});

	it('calculates the right value for custom example 1', () => {
		const raw_input = `
			25673 1
			74562 2
			42836 3
        `.replace(/^ +/gm, '');

		expect(part1(raw_input)).toBe(1 + 6 + 6);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/07.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(250898830);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			32T3K 765
			T55J5 684
			KK677 28
			KTJJT 220
			QQQJA 483
        `.replace(/^ +/gm, '');

		expect(part2(raw_input)).toBe(5905);
	});

	it('calculates the right value for custom example 2', () => {
		const raw_input = `
			T723K 1
			33848 2
			47767 3
			66366 4
			KQKJK 5
			AJ44J 6
        `.replace(/^ +/gm, '');

		expect(part2(raw_input)).toBe(1 * 1 + 2 * 2 + 3 * 3 + 4 * 4 + 5 * 5 + 6 * 6);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '07.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(252127335);
	});
});
