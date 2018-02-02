# Copyright (c) 2017 Christoph Landgraf. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

# TODO declare_module function ask for loading modules
#      on startup if functions are not defined

import cui
import os
import subprocess

from cui.util.file_mapping import FileMapping
from cui_emacs.parser import parse
from cui_emacs.util import LispException

FUNCTION_TEMPLATE = "(defun %(name)s (%(args)s) %(body)s)"
FUNCTION_LIST = []
PACKAGE_LIST = []
DEFERRED_FN_INFO = []

cui.def_variable(['emacs', 'emacsclient'], 'emacsclient')
cui.def_variable(['emacs', 'file-mapping'], FileMapping())
cui.def_variable(['logging', 'emacs-calls'], False)


def _convert_arg(arg):
    if isinstance(arg, str):
        return ("\"%s\"" % arg).encode('unicode_escape').decode('utf-8')
    elif isinstance(arg, int):
        return repr(arg)

    raise LispException("Unsupported arg-type: %s" % type(arg))


def evaluate(string, handle_result=True):
    log_calls = cui.get_variable(['logging', 'emacs-calls'])
    if log_calls:
        cui.message('emacs-call: %s' % string)

    proc = subprocess.run([cui.get_variable(['emacs', 'emacsclient']), "-e", string],
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE)
    if (proc.returncode != 0):
        raise LispException(proc.stderr.decode('utf-8'))
    result = proc.stdout.decode('utf-8').strip()
    if log_calls:
        cui.message('emacs-result: %s' % result)
    return parse(result) if handle_result else None


def _set_function_info(fn, info):
    fn.__doc__ = info


def _retrieve_function_info(symbol, info_fn='#\'documentation'):
    return evaluate("(funcall %s #'%s)" % (info_fn, symbol))


def _retrieve_function_infos(symbols, info_fn='#\'documentation'):
    return evaluate("(mapcar %s (list %s))"
                    % (info_fn,
                       ' '.join(('#\'%s' % sym for sym in symbols))))


def declare_function(name, handle_result=True):
    """Declare an existing function in emacs lisp."""
    def _fn(*args):
        return evaluate("(%s %s)"
                        % (name,
                           ' '.join(map(_convert_arg, args))),
                        handle_result=handle_result)
    if (cui.has_run(initialize)):
        _set_function_info(_fn, _retrieve_function_info(name))
    else:
        DEFERRED_FN_INFO.append((name, _fn))
    return _fn


class Package(object):
    def __init__(self, name, path):
        self.name = name
        self.path = path

    def load(self):
        path = _convert_arg(cui.get_variable(['emacs', 'file-mapping']).to_other(self.path))
        evaluate("(if (not (member '%s features)) (load %s))" % (self.name, path))


def declare_package(name, path):
    pkg = Package(name, path)
    if cui.has_run(initialize):
        pkg.load()
    else:
        PACKAGE_LIST.append(pkg)
    return pkg


def defun(e_name, e_args, body=None, path_to_body=None):
    """Define a function from python code."""
    argstring = ' '.join(e_args)
    if not body:
        if not path_to_body:
            raise LispException('No definition for %s(%s)' % (ename, argstring))
        with open(path_to_body, 'r') as f:
            body = f.read()

    lisp = FUNCTION_TEMPLATE % {'name': e_name,
                                'args': argstring,
                                'body': body}
    if cui.has_run(initialize):
        out = evaluate(lisp)
    else:
        FUNCTION_LIST.append(lisp)
    return declare_function(e_name)


@cui.post_init_func
def initialize():
    proc = subprocess.run([cui.get_variable(['emacs', 'emacsclient']), '--version'],
                          universal_newlines=True,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE)
    if (proc.returncode != 0):
        raise LispException(proc.stderr)
    cui.message(proc.stdout)
    # define functions from python
    while len(FUNCTION_LIST):
        evaluate(FUNCTION_LIST.pop(0))

    while len(PACKAGE_LIST):
        PACKAGE_LIST.pop(0).load()

    # retrieve doc for declared functions
    global DEFERRED_FN_INFO
    while DEFERRED_FN_INFO:
        fns = DEFERRED_FN_INFO[:10]
        for fn, info in zip((fn for _, fn in fns),
                            _retrieve_function_infos((sym for sym, _ in fns))):
            _set_function_info(fn, info)
        DEFERRED_FN_INFO = DEFERRED_FN_INFO[10:]
