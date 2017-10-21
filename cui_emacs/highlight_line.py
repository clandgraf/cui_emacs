# Copyright (c) 2017 Christoph Landgraf. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

import cui_emacs

from cui.util import local_file

__el_pkg__ = cui_emacs.declare_package('cui/highlight',
                                       local_file(__file__,
                                                  'el/highlight-line.el'))

highlight_line = cui_emacs.declare_function('cui/highlight-line',
                                            handle_result=False)
unhighlight_line = cui_emacs.declare_function('cui/unhighlight-line')
remove_overlay = cui_emacs.declare_function('cui/remove-overlay')
remove_overlays = cui_emacs.declare_function('cui/remove-overlays')
