import { Heap } from 'heap-js'

export function part1(raw_input: string): number {
	const grid: number[][] = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split('').map(Number));

	return search(grid, 1, 3);
}

export function part2(raw_input: string): number {
	const grid: number[][] = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split('').map(Number));

	return search(grid, 4, 10);
}

function search(grid: number[][], min_steps: number, max_steps: number): number {
	const start = new Node(new Location(0, 0), 'E', 0, 0);
	const goal_location: Location = new Location(grid[0].length - 1, grid.length -1);

	const visited: Set<NodeId> = new Set();
	const costs: Heap<number> = new Heap();
	costs.push(0);
	const nodes_by_cost: Map<number, Node[]> = new Map([[0, [start]]]);

	while (costs.length > 0) {
		const lowest_cost: number = costs.pop();
		const lowest_cost_nodes: Node[] = nodes_by_cost.get(lowest_cost);
		nodes_by_cost.delete(lowest_cost);

		for (const node of lowest_cost_nodes) {
			if (visited.has(node.key)) { continue; }

			visited.add(node.key);
			for (const neighbor of node.neighbors(grid, min_steps, max_steps)) {
				const neighbor_cost = lowest_cost + neighbor.heat_loss;

				if (neighbor.location.key == goal_location.key) {
					return neighbor_cost;
				}

				if (nodes_by_cost.has(neighbor_cost)) {
					nodes_by_cost.get(neighbor_cost).push(neighbor);
				} else {
					costs.push(neighbor_cost)
					nodes_by_cost.set(neighbor_cost, [neighbor]);
				}
			}
		}
	}
}

type Direction = 'N' | 'S' | 'W' | 'E';
type NodeId = string;

const directions: Map<Direction, [number, number]> = new Map([['N', [0, -1]], ['S', [0, 1]], ['W', [-1, 0]], ['E', [1, 0]]])

class Node {
	public readonly location: Location;
	public readonly last_direction?: Direction;
	public readonly steps: number;
	public readonly heat_loss: number;
	public readonly key: NodeId;

	constructor(location: Location, last_direction: Direction, steps: number, heat_loss: number) {
		this.location = location;
		this.last_direction = last_direction;
		this.steps = steps;
		this.heat_loss = heat_loss;
		this.key = this.build_key();
	}

	neighbors(grid: number[][], min_steps: number, max_steps: number): Node[] {
		const neighbors: Node[] = [];
		for (const direction of this.possible_directions()) {
			const same_direction = direction === this.last_direction;
			const current_steps = same_direction ? this.steps : 0;

			if (current_steps >= max_steps) { continue; }

			let direction_heat_loss = 0;

			for (let extra_steps = 1; extra_steps <= min_steps; extra_steps++) {
				const new_location = this.location.move(directions.get(direction).map(n => n * extra_steps));

				if (!new_location.inside_grid(grid)) { break; }

				direction_heat_loss += new_location.grid_value(grid);

				const total_steps = current_steps + extra_steps;
				if (total_steps >= min_steps && total_steps <= max_steps) {
					neighbors.push(new Node(new_location, direction, total_steps, direction_heat_loss));
				}
			}
		}
		return neighbors;
	}

	possible_directions(): Direction[] {
		const directions: Direction[] = [this.last_direction];
		switch (this.last_direction) {
			case 'N':
			case 'S':
				directions.push('E', 'W');
				break;
			case 'W':
			case 'E':
				directions.push('N', 'S');
				break;
		}
		return directions;
	}

	build_key() {
		return `${this.location.key}:${this.last_direction}:${this.steps}`;
	}
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

	move([x, y]: [number, number]): Location {
		return new Location(this.x + x, this.y + y);
	}

	inside_grid(grid: number[][]): boolean {
		return this.x >= 0 && this.x < grid[0].length && this.y >= 0 && this.y < grid.length;
	}

	grid_value(grid: number[][]): number {
		return grid[this.y][this.x];
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}
}
