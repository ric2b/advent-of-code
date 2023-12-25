import { init } from 'z3-solver';

export function part1(raw_input: string): number {
	const sources = ['kfr', 'vkp']
	// https://www.dagitty.net/dags.html
	const graph = []
	graph.push('graph {');
	raw_input.trim().split('\n').map((line, i) => {
		const [src, dests] = line.split(':');
		dests.match(/(\w+)/g).forEach(dest => {
			graph.push(`${dest} -- ${src}`);
		})
	});
	graph.push('}');
	return graph.join('\n');
}

export function part2(raw_input: string): number {
	const edges: Map<string, string[]> = new Map();
	raw_input.trim().split('\n').map((line, i) => {
		const [src, raw_dests] = line.split(':');
		const dests = raw_dests.match(/(\w+)/g)
		if (!edges.has(src)) {
			edges.set(src, []);
		}
		edges.get(src).push(...dests);

		dests.forEach(dest => {
			if (!edges.has(dest)) {
				edges.set(dest, []);
			}
			edges.get(dest).push(src);
		})
	});

	return [count_connected(edges, 'kfr'), count_connected(edges, 'vkp')];
}

function count_connected(edges: Map<string, string[]>, start: string): number {
	const unique_nodes: Set<string> = new Set();
	const queue = [start];

	while (queue.length > 0) {
		const node = queue.shift();
		unique_nodes.add(node);
		if (edges.has(node)) {
			queue.push(...edges.get(node).filter(dest => !unique_nodes.has(dest)));
		}
	}

	return unique_nodes.size;
}
