import pytest


@pytest.yield_fixture
def func_yieldy_thing():
    print("Func_Yieldy_Thing running")
    yield 'wat'
    print("Func_Yieldy_Thing resuming")


@pytest.yield_fixture(scope='module')
def module_yieldy_thing():
    print("module_Yieldy_Thing running")
    yield 'wat'
    print("module_Yieldy_Thing resuming")


@pytest.yield_fixture(scope='session')
def session_yieldy_thing():
    print("session_Yieldy_Thing running")
    yield 'wat'
    print("session_Yieldy_Thing resuming")
