"""
Test me like this

    python3 -m pytest tree.py

"""

import random


class Tree:
    def __init__(self):
        self.item = None
        self.left = None
        self.right = None

    def add_item(self, item):
        if self.item is None:
            self.item = [item]
        else:
            target = self.find_parent_for(item)
            new = Tree()
            new.item = [item]
            if item > target.item[0]:
                target.right = new
            else:
                target.left = new

    def find_parent_for(self, item):
        if self.item is None:
            return self
        if item > self.item[0]:
            if not self.right:
                return self
            return self.right.find_parent_for(item)
        else:
            if not self.left:
                return self
            return self.left.find_parent_for(item)

    def traverse_depth_first(self, path_to_here=None):
        """
        Generator that emits one node at a time
        """
        if path_to_here is None:
            path_to_here = []

        path_to_here = path_to_here + [self]

        if self.item is None:
            return
        if self.left:
            for child, left_path_to_here in self.left.traverse_depth_first(path_to_here):
                yield (child, left_path_to_here)
        yield (self, path_to_here)
        if self.right:
            for child, right_path_to_here in self.right.traverse_depth_first(path_to_here):
                yield (child, right_path_to_here)


def test_it():
    t = Tree()
    for i in ['cat', 'dog', 'aardvark']:
        t.add_item(i)

    assert [n.item[0] for (n, _) in t.traverse_depth_first()] == ['aardvark', 'cat', 'dog']


def test_it_some_more():
    shuffled = list(range(100))
    random.shuffle(shuffled)

    t = Tree()
    for i in shuffled:
        t.add_item(i)

    ordered = [n.item[0] for (n, _) in t.traverse_depth_first()]
    assert ordered == list(range(100))


def test_returned_paths():
    """
     cat
     /   \
aardvark  \ dog
           /     \
      clydesdale   elephant
    """

    t = Tree()
    for i in ['cat', 'aardvark', 'dog', 'clydesdale', 'elephant']:
        t.add_item(i)

    paths_by_node = {}
    for (node, path) in t.traverse_depth_first():
        [key] = node.item
        value = [n.item for n in path]
        paths_by_node[key] = value

    assert paths_by_node['aardvark']   == [['cat'], ['aardvark']]
    assert paths_by_node['cat']        == [['cat']]
    assert paths_by_node['clydesdale'] == [['cat'], ['dog'], ['clydesdale']]
    assert paths_by_node['dog']        == [['cat'], ['dog']]
    assert paths_by_node['elephant']   == [['cat'], ['dog'], ['elephant']]
