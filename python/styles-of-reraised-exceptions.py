from enum import Enum
import logging

logging.basicConfig()

_log = logging.getLogger()


class RaiseStyle(Enum):
    BARE    = 0
    FROM    = 1
    NEW     = 2
    RERAISE = 3


def top():
    print("Hi I'm a top-level function")
    middle()


def middle():
    print("MIddle guy here")
    inner()


def inner():
    print("Inner")
    raise Exception("Good golly miss MOlly")


def caller(raise_style):
    try:
        top()
    except Exception as e:
        if raise_style == RaiseStyle.RERAISE:
            print("Raise e")
            raise e
        elif raise_style == RaiseStyle.BARE:
            print("Bare raise")
            raise
        elif raise_style == RaiseStyle.NEW:
            print("Totally new exception")
            raise Exception("Unrelated to what I just caught")
        # elif raise_style == RaiseStyle.FROM:
        #     print("`From' style")
        #     raise Exception("Unrelated to what I just caught") from e


if __name__ == "__main__":
    for style in RaiseStyle:
        try:
            caller(raise_style=style)
        except Exception:
            _log.exception("That was {}".format(style))
