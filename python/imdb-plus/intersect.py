import collections
import pprint
import itertools

roles_by_name = collections.defaultdict(set)

for name in ('laura', 'nicole', 'reese'):
    with open(name) as inf:
        for line in inf:
            roles_by_name[name].add(line.rstrip())

pprint.pprint(roles_by_name)

for the_one, the_other in itertools.combinations(roles_by_name.items(), 2):
    print("Stuff that {} and {} are doing together:".format(the_one[0], the_other[0]))
    print(the_one[1].intersection(the_other[1]))
