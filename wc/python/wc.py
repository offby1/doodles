"""Rather than find a chain from one word to another, we will search
our dictionary for (say) five-letter words, and find the longest
chains we can.  We will run more or less forever, displaying a chain
as soon as we find one that's longer than any others we've found so
far.

"""

# Core
import collections
import json
import random

# 3rd party
import click  # pip install click
import tqdm  # pip install tqdm


class Graph:
    """
    This is an "Adjacency List" graph (https://en.wikipedia.org/wiki/Adjacency_list).
    """

    def __init__(self):
        self.neighbors_by_node = collections.defaultdict(set)

    def add_vertex(self, _from, to):
        self.neighbors_by_node[_from].add(to)
        self.neighbors_by_node[to].add(_from)

    def breadth_first_traverse(self, _from):
        """Traverse the graph starting at _FROM, visiting every reachable node in breadth-first order.

        Return the path to the last node we visited (which is as "far"
        as one can get from _FROM).

        """
        seen = set([_from])
        queue = [(_from, [_from])]

        while len(queue):
            node, trail = queue.pop(0)

            for n in self.neighbors_by_node[node]:
                if n not in seen:
                    queue.append((n, trail + [n]))
                    seen.add(n)
        return trail

    @classmethod
    def from_JSON_filehandle(klass, inf):
        vertices = json.load(inf)
        rv = klass()
        for source_node, target_nodes in vertices.items():
            for t in target_nodes:
                rv.add_vertex(source_node, t)
        return rv

    def to_JSON(self, outf):
        with_list_values = {k: sorted(v) for k, v in self.neighbors_by_node.items()}
        return json.dump(with_list_values, outf, indent=2, sort_keys=True)

    @classmethod
    def from_wordlist(klass, wordlist_file_name, word_length):
        rv = klass()
        words = set(n_letter_words(wordlist_file_name, word_length))
        for left in tqdm.tqdm(
            words,
            unit="word",
            desc="Processing {}-letter words from {}".format(
                word_length, wordlist_file_name
            ),
        ):

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
    for l_, r_ in zip(left, right):
        if l_ != r_:
            differing_letters += 1
            if differing_letters > 1:
                return False
    return differing_letters == 1


@click.command()
@click.option("-w", "--word-length", default=5, type=click.IntRange(3, 10, clamp=True))
def main(word_length=5):
    cache_file_name = "graph.cache.{}.json".format(word_length)
    try:
        with open(cache_file_name) as inf:
            graph = Graph.from_JSON_filehandle(inf)
    except FileNotFoundError:
        graph = Graph.from_wordlist("/usr/share/dict/words", word_length)
        with open(cache_file_name, "w") as outf:
            graph.to_JSON(outf)

    spinner = tqdm.tqdm(desc="chains", unit="")
    all_words = list(graph.neighbors_by_node.keys())

    longest_chain = []

    while True:
        start = random.choice(all_words)

        chain = graph.breadth_first_traverse(start)

        if len(chain) > len(longest_chain):
            longest_chain = chain
            if chain[0] > chain[-1]:
                chain = list(reversed(chain))
            print("\n{}: {}".format(len(chain), chain))

        spinner.update(1)


if __name__ == "__main__":
    main()
