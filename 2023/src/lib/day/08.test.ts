import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './08';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			RL
			
			AAA = (BBB, CCC)
			BBB = (DDD, EEE)
			CCC = (ZZZ, GGG)
			DDD = (DDD, DDD)
			EEE = (EEE, EEE)
			GGG = (GGG, GGG)
			ZZZ = (ZZZ, ZZZ)
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			LLR
			
			AAA = (BBB, BBB)
			BBB = (AAA, ZZZ)
			ZZZ = (ZZZ, ZZZ)
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(6);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/08.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(17621);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			LR
			
			11A = (11B, XXX)
			11B = (XXX, 11Z)
			11Z = (11B, XXX)
			22A = (22B, XXX)
			22B = (22C, 22C)
			22C = (22Z, 22Z)
			22Z = (22B, 22B)
			XXX = (XXX, XXX)
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(6);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '08.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(20685524831999);
	});
});
