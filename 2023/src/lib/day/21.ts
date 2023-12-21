import {vi} from "vitest";
import * as wasi from "wasi";
import type {Loc} from "eslint-plugin-svelte/lib/shared/svelte-compile-warns";

export function part1(raw_input: string, step_limit: number = 64): number {
	const grid: number[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	let start;
	for(let i = 0; i < grid.length; i++) {
		for(let j = 0; j < grid[0].length; j++) {
			if(grid[i][j] == 'S') {
				start = new Location(j, i);
				break;
			}
		}
	}

	const graph = build_graph(grid, start);

	const reachable_at_step_limit = new Set(bfs(graph, start, step_limit).map(n => n.key));

	return reachable_at_step_limit.size;
}

export function part2(raw_input: string): number {
	raw_input.trim().split('\n')

	return 2;
}

class Location {
	public readonly x: number;
	public readonly y: number;
	public readonly key: string;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
		this.key = Location.build_key(x, y);
	}

	neighbors(grid: string[][]): Location[] {
		const grid_height = grid.length;
		const grid_width = grid[0].length;

		return [
			new Location(this.x, this.y - 1),
			new Location(this.x, this.y + 1),
			new Location(this.x + 1, this.y),
			new Location(this.x - 1, this.y),
		].filter(l => {
			return l.y >= 0 && l.y < grid_height && l.x >= 0 && l.x < grid_width && grid[l.y][l.x] != '#';
		})
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number)
		return new Location(x, y);
	}
}

function bfs(graph: Map<string, Location[]>, start: Location, step_limit: number, visited: Set<string> = new Set(), steps: number = 1): Location[] {
	if (steps > step_limit || visited.has(`${start.key};${steps}`)) {
		return [];
	}

	visited.add(`${start.key};${steps}`);
	const neighbors: Location[] = graph.get(start.key);

	if (steps == step_limit) {
		return neighbors;
	}

	return neighbors.flatMap(n => bfs(graph, n, step_limit, visited,steps + 1));
}

function build_graph(grid: number[][], start: Location): Map<string, Location[]> {
	const graph: Map<string, Location[]> = new Map();

	const queue = [start];
	let steps = 0;

	while (queue.length > 0) {
		const location = queue.shift();

		const neighbors = location.neighbors(grid);
		if (!graph.has(location.key)) {
			graph.set(location.key, neighbors);
			queue.push(...neighbors)
		}
	}

	return graph;
}
