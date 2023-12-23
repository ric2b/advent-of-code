export function part1(raw_input: string): number {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	const graph = new Graph(grid, true);

	const start: Node = new Node(new Location(grid.at(0).findIndex(char => char == '.'), 0));
	const end: Node = new Node(new Location(grid.at(-1).findIndex(char => char == '.'), grid.length - 1));

	return longest_path(graph, start, end);
}

export function part2(raw_input: string): number {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));
	const graph = new Graph(grid);

	const start: Node = new Node(new Location(grid.at(0).findIndex(char => char == '.'), 0));
	const end: Node = new Node(new Location(grid.at(-1).findIndex(char => char == '.'), grid.length - 1));

	return longest_path(graph, start, end);
}

export function visualization(raw_input: string): string[] {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));
	const graph = new Graph(grid);

	const mermaid = [];
	// mermaid.push('https://mermaid.live/edit')

	mermaid.push('flowchart TD');
	mermaid.push('start --> 1,0');

	for (const [node_key, edges] of graph.edges.entries()) {
		for (const edge of edges) {
			const node = graph.nodes.get(node_key);
			const neighbor = edge.node;
			const node_order = node.location.x * grid.length + node.location.y;
			const neighbor_order = neighbor.location.x * grid.length + neighbor.location.y;

			if (node_order < neighbor_order) {
				mermaid.push(`${node.key} ---|${edge.cost}| ${neighbor.key}([${neighbor.key}])`);
			}
		}
	}
	mermaid.push('139,140 --> finish[finish]');


	return mermaid;
}

function longest_path(graph: Graph, start: Node, end: Node, visited: Set<NodeId> = new Set()): number {
	const costs_to_end: number[] = graph.edges.get(start.key)
		.filter(edge => !visited.has(edge.node.key))
		.map(edge => {
			if (edge.node.key == end.key) {
				return edge.cost;
			}
			return edge.cost + longest_path(graph, edge.node, end, new Set([...visited, edge.node.key]));
		});

	return Math.max(...costs_to_end);
}

type Edge = { node: Node, cost: number };
type NodeId = string;
class Node {
	public readonly location: Location;
	public readonly value: unknown;
	public readonly key: NodeId;

	constructor(location: Location, value: unknown) {
		this.location = location;
		this.value = value;
		this.key = location.key;
	}
}

class Graph {
	public readonly nodes: Map<NodeId, Node>;
	public readonly edges: Map<NodeId, Edge[]>;

	constructor(grid: string[][], slippery: boolean = false) {
		this.nodes = new Map();
		this.edges = new Map();
		grid.forEach((row, y) => {
			row.forEach((cell, x) => {
				if (cell != '#') {
					const node = new Node(new Location(x, y), cell);
					this.nodes.set(node.key, node);
				}
			})
		});

		for (const node of this.nodes.values()) {
			const neighbors: Node[] = node.location.neighbors(grid, slippery).map(l => this.nodes.get(l.key));
			this.edges.set(node.key, neighbors.map(n => ({ node: n, cost: 1 })));
		}
		this.compact_straight_runs();
		this.remove_extra_edges_from_last_node(grid);
	}

	compact_straight_runs() {
		for (const node_key of this.edges.keys()) {
			const node_edges: Edge[] = this.edges.get(node_key);
			if (node_edges.length === 2) {
				const [to_a, to_b] = node_edges;
				const [from_a, from_b] = node_edges.map(to_edge => {
					return this.edges.get(to_edge.node.key).find(from_edge => from_edge.node.key === node_key)
				});
				if (from_a) {
					from_a.node = to_b.node;
					from_a.cost += to_b.cost;
				}
				if (from_b) {
					from_b.node = to_a.node;
					from_b.cost += to_a.cost;
				}

				this.nodes.delete(node_key);
				this.edges.delete(node_key);
			}
		}
	}

        remove_extra_edges_from_last_node(grid: number[][]) {
            const end_key: string = `${grid[0].length-2},${grid.length-1}`;
            const last_node: Node = this.edges.get(end_key).at(0).node;
            const last_node_edges = this.edges.get(last_node.key);
            this.edges.set(last_node.key, last_node_edges.filter(e => e.node.key == end_key));
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

	neighbors(grid: string[][], slippery: boolean = false): Location[] {
		const grid_height = grid.length;
		const grid_width = grid[0].length;

		if (slippery) {
			switch (grid[this.y][this.x]) {
				case '^': return [new Location(this.x, this.y - 1)];
				case 'v': return [new Location(this.x, this.y + 1)];
				case '>': return [new Location(this.x + 1, this.y)];
				case '<': return [new Location(this.x - 1, this.y)];
			}
		}

		return [
			new Location(this.x, this.y - 1),
			new Location(this.x, this.y + 1),
			new Location(this.x + 1, this.y),
			new Location(this.x - 1, this.y),
		].filter(l => {
			return l.y >= 0 && l.y < grid_height && l.x >= 0 && l.x < grid_width && grid[l.y][l.x] != '#';
		});
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number);
		return new Location(x, y);
	}
}
