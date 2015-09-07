import progress.bar                 # pip install progress


class Graph:
    def __init__(self):
        self.nodes = set()
        self.vertices = set()

    def add_node(self, node):
        self.nodes.add(node)

    def add_vertex(self, _from, to):
        self.nodes.add(_from)
        self.nodes.add(to)
        self.vertices.add((_from, to))
        self.vertices.add((to, _from))


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
    graph = Graph()
    words = set(n_letter_words('/usr/share/dict/words', 5))
    bar = progress.bar.Bar("Chug chug", max=len(words), suffix='%(eta_td)s')
    for left in words:
        graph.add_node(left)
        # In theory, you could generate all possible one-letter
        # variants of "left", and then add them; but that would take
        # roughly 25^5 steps, which is much larger than the 10,000 ^ 2
        # steps we're doing here (10,000 is the number of five-letter
        # words).
        for right in words:
            if differ_by_one_letter(left, right):
                graph.add_vertex(left, right)
        bar.next()
    bar.finish()

if __name__ == "__main__":
    main()
