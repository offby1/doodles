# <r37r0uv33> as we know if a know b, b know c, eventually a will know c. And a, b and c will form a
#     group which is [a, b, c], you have to create a function to check how many group in total.for
#     example: For total_number_of people = 10 and group = [[0,9], [3,7], [7,9], [5,6], [1,5], [4,2]],
#     the output should be totalgroups(total_number_of people, group) = 4. There will be 4
#     totalgroups: [0, 9, 7, 3], [5, 6, 1], [4, 2] and [8].

import collections

edges = [[0,9], [3,7], [7,9], [5,6], [1,5], [4,2]]

def populate_neighbor_dict(edges):
    rv = collections.defaultdict(set)

    for e in edges:
        rv[e[0]].add(e[1])
        rv[e[1]].add(e[0])

    return rv

neighbors_by_node = populate_neighbor_dict(edges)
graph_by_node = {}

class GraphNameGenerator:
    def __init__(self):
        self.counter = 0

    def __call__(self):
        self.counter += 1
        return 'Graph Number {}'.format(self.counter)


def visit_graph(node, node_func, seen=None):
    if seen is None:
        seen = set([node])

    node_func(node)

    for n in neighbors_by_node[node]:
        if n not in seen:
            seen.add(n)
            visit_graph(n, node_func, seen=seen)

generate_unique_graph_name = GraphNameGenerator()

for e in edges:
    for node in e:
        if node not in graph_by_node:
            graph_name = generate_unique_graph_name()
            # traverse all neighbors, mark them with this graph
            visit_graph(node, lambda node: graph_by_node.update({node: graph_name}))

import pprint
pprint.pprint(dict(graph_by_node))

# invert graph_by_node, since that's easier to read
nodes_by_graph = collections.defaultdict(set)
for node, graph in graph_by_node.items():
    nodes_by_graph[graph].add(node)

pprint.pprint(dict(nodes_by_graph))
