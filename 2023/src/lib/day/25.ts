type NodeId = string;
type Edge = [NodeId, NodeId];

export function part1(raw_input: string): number {
	let edges: Edge[];
	let score = -1;

	do {
		edges = raw_input.trim().split('\n').flatMap(line => {
			const [node_a, raw_neighbors] = line.split(':');
			return raw_neighbors.match(/(\w+)/g)?.map(node_b => {
				return [node_a, node_b];
			});
		});

		[score, edges] = karger(edges);
	} while (edges.length != 3);

	return score;
}

function karger(edges: Edge[]) {
	const node_ids: Set<NodeId> = new Set(edges.flat());
	let supernode_index = 0;
	const supernode_children: Map<NodeId, NodeId[]> = new Map();

	while (node_ids.size > 2) {
		const contracted_edge_index = Math.floor(Math.random() * edges.length);

		const contracted_edge: Edge = edges.at(contracted_edge_index);
		const [contracted_node_a, contracted_node_b] = contracted_edge;
		edges.splice(contracted_edge_index, 1);

		const supernode_id = `supernode_${supernode_index}`;
		node_ids.add(supernode_id);
		contracted_edge.forEach(node_id => node_ids.delete(node_id));
		supernode_children.set(supernode_id, [contracted_node_a, contracted_node_b]);
		supernode_index++;

		edges.forEach(edge => {
			contracted_edge.forEach(contracted_node => {
				const index = edge.indexOf(contracted_node);
				if (index != -1) { edge[index] = supernode_id; }
			})
		})

		edges = edges.filter(([a, b]) => a != b);
	}

	const [supernode_a, supernode_b] = edges[0];

	const a_children = children(supernode_children, supernode_a);
	const b_children = children(supernode_children, supernode_b);
	const score = a_children.length * b_children.length;
	return [score, edges];
}

function children(supernode_children: Map<NodeId, NodeId[]>, supernode_id: NodeId): NodeId[] {
	if (!supernode_children.has(supernode_id)) {
		return [supernode_id];
	}

	return supernode_children.get(supernode_id).flatMap(node_id => {
		return node_id.startsWith('supernode_') ? children(supernode_children, node_id) : [node_id];
	});
}
