class CircularList(list):
    """
    >>> l = CircularList([1, 2, 3])
    >>> l
    [1, 2, 3]
    >>> l[2]
    3
    >>> l[3]
    1
    >>> l[100]
    2
    >>>
    """
    def __getitem__(self, index):
        index = index % self.__len__()
        return super().__getitem__(index)
