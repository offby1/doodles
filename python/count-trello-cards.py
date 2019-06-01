import json

exported_trello_board = "/Users/erichanchrow/Desktop/exported.json"

with open(exported_trello_board) as inf:
    board = json.load(inf)

open_lists_by_id = {}
for l in board["lists"]:
    if not l["closed"]:
        open_lists_by_id[l["id"]] = l

open_cards_with_list_name = []
for c in board["cards"]:
    if c["closed"]:
        continue

    list_ = open_lists_by_id.get(c["idList"])

    if not list_:
        continue

    if list_["name"].lower() not in ("straps", "movements"):
        open_cards_with_list_name.append(f"{list_['name']!r}: {c['name']!r}")

for thing in sorted(open_cards_with_list_name, key=lambda s: s.lower()):
    print(thing)
