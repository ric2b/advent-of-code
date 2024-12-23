from itertools import combinations

example = '''\
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
'''

input = example.splitlines()

with open('inputs/day_23.txt', 'r') as input_file:
    input = input_file.read().splitlines()

nodes = set()
connections = {}
for line in input:
    a, b = line.strip().split('-')

    nodes.update({a, b})
    connections.setdefault(a, set()).add(b)
    connections.setdefault(b, set()).add(a)

t_groups = set()
for a, b, c in combinations(nodes, 3):
    if b in connections[a] and c in connections[a] and c in connections[b]:
        if any(node.startswith('t') for node in (a, b, c)):
            t_groups.add(frozenset({a, b, c}))

# pt1: 926
print(len(t_groups))

# pt2: az,ed,hz,it,ld,nh,pc,td,ty,ux,wc,yg,zz
for candidate_size in reversed(range(max(len(node_connections) + 1 for node_connections in connections.values()))):
    found = False
    for node, node_connections in connections.items():
        if len(node_connections) + 1 >= candidate_size:
            connected = {node, *node_connections}
            for a, b in combinations(node_connections, 2):
                if b not in connections[a]:
                    connected.discard(b)

            if len(connected) == candidate_size and any(node.startswith('t') for node in connected):
                print(','.join(sorted(connected)))
                found = True
                break

    if found:
        break
