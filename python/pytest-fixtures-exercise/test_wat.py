"""
WTF _are_ fixtures?  When do they run?  What side effects do they have?  Etc.

http://doc.pytest.org/en/latest/fixture.html
"""


def test_yieldy_thing(func_yieldy_thing, module_yieldy_thing, session_yieldy_thing):
    assert func_yieldy_thing == 'wat'


def test_yieldy_thing_again(func_yieldy_thing, module_yieldy_thing, session_yieldy_thing):
    assert func_yieldy_thing == 'wat'
