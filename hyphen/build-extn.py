#!/usr/bin/env python3

import sys, distutils.sysconfig, os.path, subprocess

def pylib_template():
    if sys.platform == 'darwin':
        return 'python%d.%d'
    else:
        assert False, "%r unsupported" % sys.platform

work_dir   = sys.path[0]
ghc_ver    = subprocess.check_output(["ghc", "--numeric-version"]).decode('ascii')
HSrts_lib  = "HSrts-ghc" + ghc_ver
py_include = distutils.sysconfig.get_python_inc()
py_libdir  = os.path.join(sys.exec_prefix, 'lib')
pythonlib = (pylib_template() % (sys.hexversion >> 24, (sys.hexversion >> 16) & 0xff))

subprocess.check_call([
    "ghc", "-dynamic", "-shared", "-fPIC", "-no-hs-main", "-fwarn-unused-imports",

    # Input files (both named, and path to find dependencies)
    os.path.join(work_dir, "lowlevel_src", "Hyphen.hs"),
    os.path.join(work_dir, "lowlevel_src", "hyphen_c.c"),
    "-i" +    os.path.join(work_dir, "lowlevel_src"),

    # Output/intermediate files
    "-o",     os.path.join(work_dir, "hslowlevel.so"),
    "-hidir", os.path.join(work_dir, "lowlevel_inter"),
    "-odir",  os.path.join(work_dir, "lowlevel_inter"),

    # Where to find C include files
    "-I" + py_include,

    # Linker libraries
    "-l" + HSrts_lib, "-l" + pythonlib,

    # Linker options
    "-optl", "-Wl,-exported_symbol,_PyInit_hslowlevel,-L" + py_libdir
])
