import functools


def verbosely(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        """
        I am a wrapper.  I am not interesting.
        """
        print(f"Entering {func}")
        rv = func(*args, **kwargs)
        print(f"Returning {rv} from {func}")
        return rv

    return wrapper


@verbosely
def add_three(x):
    """
    Add three to argument, and return the sum.
    """
    return x + 3


help(add_three)

# Help on function add_three in module __main__:

# add_three(x)
#     Add three to argument, and return the sum.
