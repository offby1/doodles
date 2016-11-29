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

# ~/venv3.Linux/bin/python wat.py

# Where is the boo-tay?
# We wants our swag!!!
