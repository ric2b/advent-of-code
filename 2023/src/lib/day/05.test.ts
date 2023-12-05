import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './05';

describe('part 1', () => {
    it('calculates the right value for the example', () => {
        const raw_input = `
            seeds: 79 14 55 13
            
            seed-to-soil map:
            50 98 2
            52 50 48
            
            soil-to-fertilizer map:
            0 15 37
            37 52 2
            39 0 15
            
            fertilizer-to-water map:
            49 53 8
            0 11 42
            42 0 7
            57 7 4
            
            water-to-light map:
            88 18 7
            18 25 70
            
            light-to-temperature map:
            45 77 23
            81 45 19
            68 64 13
            
            temperature-to-humidity map:
            0 69 1
            1 0 69
            
            humidity-to-location map:
            60 56 37
            56 93 4
        `
            .replace(/^ +/gm, '');

        expect(part1(raw_input)).toBe(35);
    });

    it('calculates the right value for the input', () => {
        const filePath = path.resolve(process.cwd(), 'static/inputs/05.txt');
        const raw_input = readFileSync(filePath, 'utf8');

        expect(part1(raw_input)).toBe(388071289);
    });
});

describe('part 2', () => {
    it('calculates the right value for the example', () => {
        const raw_input = `

        `
            .replace(/^\s+/gm, '');

        expect(part2(raw_input)).toBe(0);
    });

    it('calculates the right value for the input', () => {
        const filePath = path.resolve(process.cwd(), 'static', 'inputs', '05.txt');
        const raw_input = readFileSync(filePath, 'utf8');

        expect(part2(raw_input)).toBe(0);
    });
});
