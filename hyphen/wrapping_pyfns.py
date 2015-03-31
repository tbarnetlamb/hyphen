from __future__           import absolute_import

import hyphen

next_expected_return_hstype = None

def check_hs_return(to_return):
    global next_expected_return_hstype
    return hyphen.marshall_obj_to_hs.py_to_hs(to_return, next_expected_return_hstype)

def wrap_pyfn(pyfn, expected_return_hstype):
    to_py = hyphen.marshall_obj_to_py.hs_to_py
    to_hs = hyphen.marshall_obj_to_hs.py_to_hs
    def inner(*args):
        global next_expected_return_hstype
        next_expected_return_hstype = expected_return_hstype
        result = pyfn(*map(to_py, args))
        return to_hs(result, expected_return_hstype)
    return inner
