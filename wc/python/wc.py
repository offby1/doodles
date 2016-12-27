# Core
import argparse
import ast
import collections
import pprint
import random

# 3rd party
import progressbar                 # pip install progress2

class Graph:
    """
    This is an "Adjacency List" graph (https://en.wikipedia.org/wiki/Adjacency_list).
    """

    def __init__(self):
        self.neighbors_by_node = collections.defaultdict(set)

    def add_vertex(self, _from, to):
        self.neighbors_by_node[_from].add(to)
        self.neighbors_by_node[to].add(_from)

    @property
    def stats(self):
        v = len(self.neighbors_by_node.keys())
        n = sum([len(s) for s in self.neighbors_by_node.values()])
        ratio = v / n
        return {'vertices': v,
                'nodes': n,
                'ratio': ratio}

    def bfs(self, _from):
        seen = set([_from])
        queue = [(_from, [_from])]

        while len(queue):
            node, trail = queue.pop(0)

            for n in self.neighbors_by_node[node]:
                if n not in seen:
                    queue.append((n, trail + [n]))
                    seen.add(n)
        return trail

    def __str__(self):
        return pprint.pformat(dict(self.neighbors_by_node))

    @classmethod
    def from_python_literal(klass, string):
        vertices = ast.literal_eval(string)
        rv = klass()
        for source_node, target_nodes in vertices.items():
            for t in target_nodes:
                rv.add_vertex(source_node, t)
        return rv

    @classmethod
    def from_wordlist(klass, wordlist_file_name, word_length):
        rv = klass()
        words = set(n_letter_words(wordlist_file_name, word_length))
        for left in progressbar.ProgressBar(widgets=["Processing {}-letter words from {}".format(word_length,
                                                                                                 wordlist_file_name),
                                                     progressbar.Percentage(),
                                                     ' ', progressbar.Bar(),
                                                     ' ', progressbar.ETA()])(words):

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
    def at_least_three(string):
        value = int(string)
        if value < 3:
            msg = "%r is not at least three" % string
            raise argparse.ArgumentTypeError(msg)
        return value
    parser = argparse.ArgumentParser()
    parser.add_argument("--word-length",
                        type=at_least_three,
                        default=3)
    args = parser.parse_args()

    cache_file_name = 'graph.cache.{}'.format(args.word_length)
    try:
        with open(cache_file_name) as inf:
            graph = Graph.from_python_literal(inf.read())
    except FileNotFoundError:
        graph = Graph.from_wordlist('/usr/share/dict/words', args.word_length)
        with open(cache_file_name, 'w') as outf:
            outf.write(str(graph))

    pprint.pprint(graph.stats)

    spinner = progressbar.ProgressBar(max_value=progressbar.UnknownLength)
    all_words = list(graph.neighbors_by_node.keys())

    longest_chain = []

    while True:
        start = random.choice(all_words)

        chain = graph.bfs(start)

        if len(chain) > len(longest_chain):
            longest_chain = chain
            if chain[0] > chain[-1]:
                chain = list(reversed(chain))
            print("\n{}: {}".format(len(chain), chain))

        spinner.update(spinner.value + 1)


if __name__ == "__main__":
    main()
