"""hslowlevel has functionality for wrapping Python functions so that
they can be used as Haskell functions. This functionality is
low-level; in particular, if we wrap a function f in this way then
when f is called from Haskell it will be called with all its arguments
being pure HsObjRaw objects (i.e., not marshalled to python in any
way, and not 'upgraded' to the more convenient HsObj class defined in
our high-level library). Moreover, it must return an HsObjRaw (or
something derived therefrom, like an HsObj); no marshalling is done on
the return value. This can be a little annoying!

It would be more convenient if we could write our python function f in
a way that expects to recieve marshalled arguments and which can count
on having its return value marshalled back to Haskell. To achieve
this, this module defines a function wrap_pyfn which can be applied to
python function f which does no marshalling, and transforms it into a
python function which does do marshalling at both ends, which can then
be wrapped into a Haskell function using the features in hslowlevel.

"""

from __future__           import absolute_import
import hyphen

next_expected_return_hstype = None

def check_hs_return(to_return):
    """Convenience function, which can be called from the body of a python
    function which we intend to wrap and expose as a Haskell
    function. Instead of ending the function body:

    return <expression>

    we end it

    return check_hs_return(<expression>)

    The effect of doing so is that we check that the value returned by
    <expression> can be Marshalled to a Haskell object of the right
    type *before* our function (f say) returns, rather than only
    making this check afterwards. This means that if the check fails,
    a postmortem debugger will have access to the frame from our
    function f, which may make debugging substantially easier.

    """
    global next_expected_return_hstype
    return hyphen.marshall_obj_to_hs.py_to_hs(to_return, next_expected_return_hstype)

def wrap_pyfn(pyfn, expected_return_hstype):
    """Given pyfn, a python function, wrap it into another python
    function wrapped_pyfn. wrapped_pyfn expects to recieve HsObjRaws
    as parameters; wrapped_pyfn first marshalls them into nice python
    objects (or at least, to HsObjs), then calls the user-provided
    pyfn on the marshalled values, and marshalls the return value from
    pyfn back to being a Haskell object before finally returning this
    marshalled object.
    """
    to_py = hyphen.marshall_obj_to_py.hs_to_py
    to_hs = hyphen.marshall_obj_to_hs.py_to_hs
    def inner(*args):
        global next_expected_return_hstype
        next_expected_return_hstype = expected_return_hstype
        result = pyfn(*map(to_py, args))
        return to_hs(result, expected_return_hstype)
    return inner
