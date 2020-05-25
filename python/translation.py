import re

input = """
Where is the Treasure?
We wants our MONEY!!!
"""

xlations = {
    'treasure': 'boo-tay',
    'money': 'swag',
    'the': 'de',
    'where': 'waar',
    'we': "wheee!!!",
    'our': 'arr',
    'wants': 'lusts for',
    'is': 'z',
}


def lookup(match):
    word = match.group(0)
    return xlations.get(word.lower(), word)


output = re.sub(r'(\w+)', lookup, input)

print(output)

# python3 translation.py

# waar z de boo-tay?
# wheee!!! lusts for arr swag!!!
