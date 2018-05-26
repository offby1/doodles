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

    def traverse_depth_first(self):
        if self.item is None:
            return
        if self.left:
            for item in self.left.traverse_depth_first():
                yield item
        yield self.item[0]
        if self.right:
            for item in self.right.traverse_depth_first():
                yield item

def test_it():
    t = Tree()
    for i in ['cat', 'dog', 'aardvark']:
        t.add_item(i)

    assert list(t.traverse_depth_first()) == ['aardvark', 'cat', 'dog']

def test_it_some_more():
    shuffled = list(range(100))
    random.shuffle(shuffled)
    print(shuffled)

    t = Tree()
    for i in shuffled:
        t.add_item(i)

    ordered = list(t.traverse_depth_first())
    assert ordered == list(range(100))
