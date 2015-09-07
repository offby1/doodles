# Core
import ast
import collections
import pprint

# 3rd party
import progress.bar                 # pip install progress


class Node:
    def __init__(self, datum):
        self.datum = datum
        self.neighbors = set()

    def __hash__(self):
        return hash(self.datum)

    def __eq__(self, other):
        return self.datum == other.datum


class Graph:
    def __init__(self):
        self.nodes_by_datum = {}
        self.neighbors_by_node = collections.defaultdict(set)

    def add_node(self, datum):
        self.nodes_by_datum[datum] = Node(datum)

    def add_vertex(self, _from, to):
        self.neighbors_by_node[_from].add(to)
        self.neighbors_by_node[to].add(_from)

    def __str__(self):
        return pprint.pformat(dict(self.neighbors_by_node))

    @classmethod
    def from_python_literal(klass, string):
        vertices = ast.literal_eval(string)
        rv = klass()
        for source_node, target_nodes in vertices.items():
            rv.add_node(source_node)
            for t in target_nodes:
                rv.add_vertex(source_node, t)
        return rv

    @classmethod
    def from_wordlist(klass, wordlist_file_name, word_length):
        rv = klass()
        words = set(n_letter_words(wordlist_file_name, word_length))
        for left in progress.bar.Bar("Processing {}-letter words from {}".format(word_length,
                                                                                 wordlist_file_name),
                                     suffix='%(index)d/%(max)d (%(eta_td)s remaining)').iter(words):
            rv.add_node(left)
            # In theory, you could generate all possible one-letter
            # variants of "left", and then add them; but that would take
            # roughly 25^word_length steps, which is much larger than the
            # n ^ 2 steps we're doing here (n is the number of words of
            # length word_length ... afaict, it doesn't much matter what
            # word_length is; this is always true).
            for right in words:
                if differ_by_one_letter(left, right):
                    rv.add_vertex(left, right)

        return rv


def n_letter_words(dict_file_name, n):
    with open(dict_file_name) as inf:
        for line in inf:
            line = line.lower().rstrip()
            if len(line) == n:
                yield (line)


def differ_by_one_letter(left, right):
    differing_letters = 0
    for l, r in zip(left, right):
        if l != r:
            differing_letters += 1
            if differing_letters > 2:
                return False
    return differing_letters == 1


def main():
    word_length = 5
    cache_file_name = 'graph.cache.{}'.format(word_length)
    try:
        with open(cache_file_name) as inf:
            graph = Graph.from_python_literal(inf.read())
    except FileNotFoundError:
        graph = Graph.from_wordlist('/usr/share/dict/words', word_length)
        with open(cache_file_name, 'w') as outf:
            outf.write(str(graph))

    print(graph)

if __name__ == "__main__":
    main()
