#!/usr/bin/env python3

import sys, distutils.sysconfig, os, os.path, subprocess, optparse

def flatten(args):
    if isinstance(args, str):
        return [args]
    return sum(map(flatten, args), [])

def getopts():
    parser = optparse.OptionParser()
    parser.add_option("-d", "--dynamic", action="store_const", const="dynamic", dest="dyn_or_static")
    parser.add_option("-s", "--static",  action="store_const", const="static",  dest="dyn_or_static")

    opts, args = parser.parse_args()
    assert not args

    if opts.dyn_or_static is None:
        if sys.platform == 'win32' or sys.platform[:6] == 'cygwin':
            opts.dyn_or_static = 'static'
        else:
            opts.dyn_or_static = 'dynamic'

    return opts

def pylib():
    items = {'major'    : sys.hexversion >> 24,
             'minor'    : (sys.hexversion >> 16) & 0xff,
             'abiflags' : sys.__dict__.get('abiflags', '')}
    if sys.platform == 'darwin':
        return 'python{major}.{minor}'.format(**items)
    elif sys.platform == 'linux':
        return 'python{major}.{minor}{abiflags}'.format(**items)
    elif sys.platform[:6] == 'cygwin':
        return 'python{major}.{minor}{abiflags}'.format(**items)
    elif sys.platform == 'win32':
        return 'python{major}{minor}'.format(**items)
    else:
        assert False, '%r unsupported' % sys.platform

def linker_opts():
    if sys.platform == 'darwin':
        return ['-Wl', '-exported_symbol', '_PyInit_hslowlevel', '-L' + py_libdir]
    elif sys.platform == 'linux':
        return ['-Wl', '--version-script=' + work_dir + '/hslowlevel.version',
                '-L' + py_libdir]
    elif sys.platform[:6] == 'cygwin':
        return ['-Wl', '-L' + cygpreppath(os.path.join(work_dir, 'lowlevel_inter')),
                '--version-script=' + cygpreppath(work_dir + '/hslowlevel.version')]
    elif os.name == 'nt':
        libdirs = set(os.path.join(prf, 'libs') for prf in [
            sys.exec_prefix, sys.base_exec_prefix])
        return ['-Wl', '--version-script=' + work_dir + '/hslowlevel.version'] + [
            '-L' + ldir for ldir in libdirs]
    else:
        assert False, '%r unsupported' % sys.platform

def cygpreppath(path):
    if sys.platform[:6] == 'cygwin':
        return (subprocess.check_output(['cygpath', '-w', path])
                .decode(sys.getdefaultencoding()).rstrip())
    else:
        return path

opts       = getopts()
work_dir   = sys.path[0]
ghc_ver    = subprocess.check_output(['ghc', '--numeric-version']).decode('ascii')
HSrts_lib  = {'dynamic' : 'HSrts-ghc' + ghc_ver,
              'static'  : 'HSrts'              }[opts.dyn_or_static]
py_include = distutils.sysconfig.get_python_inc()
py_libdir  = distutils.sysconfig.get_config_var('LIBDIR')
suffix     = distutils.sysconfig.get_config_var('EXT_SUFFIX')

hyphen_c   = 'hyphen_c.' + ('c' if sys.platform[:6] != 'cygwin' else 'o')
if sys.platform[:6] == 'cygwin':
    # comment here
    ghc_path = subprocess.check_output(['which', 'ghc']).decode(sys.getdefaultencoding())
    haskell_include_path = os.path.realpath(os.path.join(
        os.path.dirname(ghc_path), '..', 'lib', 'include'))
    subprocess.check_call(flatten([
        'gcc', '-c', os.path.join(work_dir, 'lowlevel_src', 'hyphen_c.c'),
        '-o' + os.path.join(work_dir, 'lowlevel_src', 'hyphen_c.o'),
        '-I' + py_include, '-I' + haskell_include_path,
        '-D__GHCAUTOCONF_H__']))

invocation = flatten([
    'ghc', '-' + opts.dyn_or_static, '-shared', '-fPIC', '-no-hs-main',
    '-fwarn-unused-imports', '-cpp',

    # Have to supply this DEFINE on windows 64 bit
    ['-optc', '-DMS_WIN64'] if (
        sys.maxsize > 2**32 and (
            sys.platform == 'win32' or sys.platform[:6] == 'cygwin')
        ) else [],

    # Input files (both named, and path to find dependencies)
    cygpreppath(os.path.join(work_dir, 'lowlevel_src', 'Hyphen.hs')),
    cygpreppath(os.path.join(work_dir, 'lowlevel_src', hyphen_c)),
    '-i' + cygpreppath(os.path.join(work_dir, 'lowlevel_src')),

    # Output/intermediate files
    '-o',     cygpreppath(os.path.join(work_dir, 'hslowlevel' + suffix)),
    '-hidir', cygpreppath(os.path.join(work_dir, 'lowlevel_inter')),
    '-odir',  cygpreppath(os.path.join(work_dir, 'lowlevel_inter')),

    # Where to find C include files
    '-I' + py_include,
 
    # Packages needed but not (by default) exposed
    '-package', 'ghc',

    # Linker libraries
    '-l' + HSrts_lib, '-l' + pylib(),
])

subprocess.check_call(invocation + ['-no-link'])
subprocess.check_call(invocation + [
    # Linker options
    '-optl', ','.join(linker_opts())
])
