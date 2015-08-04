#!/usr/bin/env python3

import sys, distutils.sysconfig, os.path, subprocess

def pylib():
    items = {'major'    : sys.hexversion >> 24,
             'minor'    : (sys.hexversion >> 16) & 0xff,
             'abiflags' : sys.abiflags}
    if sys.platform == 'darwin':
        return 'python{major}.{minor}'.format(**items)
    elif sys.platform == 'linux':
        return 'python{major}.{minor}{abiflags}'.format(**items)

    else:
        assert False, '%r unsupported' % sys.platform

def linker_opts():
    if sys.platform == 'darwin':
        return ['-Wl', '-exported_symbol', '_PyInit_hslowlevel', '-L' + py_libdir]
    elif sys.platform == 'linux':
        return ['-Wl', '--version-script=' + work_dir + '/hslowlevel.version',
                '-L' + py_libdir]
    else:
        assert False, '%r unsupported' % sys.platform

work_dir   = sys.path[0]
ghc_ver    = subprocess.check_output(['ghc', '--numeric-version']).decode('ascii')
HSrts_lib  = 'HSrts-ghc' + ghc_ver
py_include = distutils.sysconfig.get_python_inc()
py_libdir  = distutils.sysconfig.get_config_var('LIBDIR')
suffix     = distutils.sysconfig.get_config_var('EXT_SUFFIX')

subprocess.check_call([
    'ghc', '-dynamic', '-shared', '-fPIC', '-no-hs-main', '-fwarn-unused-imports',

    # Input files (both named, and path to find dependencies)
    os.path.join(work_dir, 'lowlevel_src', 'Hyphen.hs'),
    os.path.join(work_dir, 'lowlevel_src', 'hyphen_c.c'),
    '-i' +    os.path.join(work_dir, 'lowlevel_src'),

    # Output/intermediate files
    '-o',     os.path.join(work_dir, 'hslowlevel' + suffix),
    '-hidir', os.path.join(work_dir, 'lowlevel_inter'),
    '-odir',  os.path.join(work_dir, 'lowlevel_inter'),

    # Where to find C include files
    '-I' + py_include,

    # Packages needed but not (by default) exposed
    '-package', 'ghc',

    # Linker libraries
    '-l' + HSrts_lib, '-l' + pylib(),

    # Linker options
    '-optl', ','.join(linker_opts())
])
