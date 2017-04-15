import random


def insert_into_sorted_list(new_element, list_):
    for index, elt in enumerate(list_):
        if elt >= new_element:
            list_.insert(index, new_element)
            return list_

    # all the existing elements are strictly < new_element, so stick
    # new_element at the end.
    list_.append(new_element)
    return list_


def insertion_sort(list_):
    sorted_ = []
    for elt in list_:
        insert_into_sorted_list(elt, sorted_)
    return sorted_


def test_insert():
    assert insert_into_sorted_list(3, []) == [3]
    assert insert_into_sorted_list(3, [1]) == [1, 3]
    assert insert_into_sorted_list(1, [3]) == [1, 3]


def test_sort():
    assert insertion_sort([]) == []
    assert insertion_sort([1]) == [1]
    assert insertion_sort([1, 1, 1]) == [1, 1, 1]
    assert insertion_sort([1, 2, 3]) == [1, 2, 3]
    assert insertion_sort([3, 2, 1]) == [1, 2, 3]


r = [random.randrange(0, 100) for _ in range(100)]
print(insertion_sort(r))
