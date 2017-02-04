# <r37r0uv33> as we know if a know b, b know c, eventually a will know c. And a, b and c will form a
#     group which is [a, b, c], you have to create a function to check how many group in total.for
#     example: For total_number_of people = 10 and group = [[0,9], [3,7], [7,9], [5,6], [1,5], [4,2]],
#     the output should be totalgroups(total_number_of people, group) = 4. There will be 4
#     totalgroups: [0, 9, 7, 3], [5, 6, 1], [4, 2] and [8].

import collections


class Graph:

    def __init__(self, edges):
        self.edges = edges
        self.neighbors_by_node = collections.defaultdict(set)

        for e in self.edges:
            n1, n2 = e
            self.neighbors_by_node[n1].add(n2)
            self.neighbors_by_node[n2].add(n1)

    def traverse_starting_at(self, node, node_func, seen=None):
        """For each node in the graph, invoke node_func for side effects.

        """
        if seen is None:
            seen = set()

        node_func(node)
        seen.add(node)

        for n in self.neighbors_by_node[node]:
            if n not in seen:
                self.traverse_starting_at(n, node_func, seen=seen)


the_graph = Graph([[0, 9], [3, 7], [7, 9], [5, 6], [1, 5], [4, 2], [8, 8]])

graph_by_node = {}


class GraphNameGenerator:

    def __init__(self):
        self.counter = 0

    def __call__(self):
        self.counter += 1
        return 'Graph Number {}'.format(self.counter)


generate_unique_graph_name = GraphNameGenerator()

for e in the_graph.edges:
    for node in e:
        if node not in graph_by_node:
            graph_name = generate_unique_graph_name()
            # traverse all neighbors, mark them with this graph
            the_graph.traverse_starting_at(node, lambda node: graph_by_node.update({node: graph_name}))

# invert graph_by_node, since that's easier to read
nodes_by_graph = collections.defaultdict(set)
for node, graph in graph_by_node.items():
    nodes_by_graph[graph].add(node)

print([list(s) for s in nodes_by_graph.values()])
