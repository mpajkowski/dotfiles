#!/usr/bin/env python3

from mako.template import Template

import os
import sys

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def render(filename, args):
    i3_template = Template(filename=filename)

    output = i3_template.render(**args)

    print(output)

    with open(os.path.join(SCRIPT_DIR, 'config'), 'w') as f:
        f.write(output)

if __name__ == '__main__':
    args = sys.argv[1:]

    parsed_args = dict()
    for arg in args:
        splitted = arg.split(':')
        k, v = splitted[0], splitted[1]
        parsed_args[k] = v

    config_file = os.path.join(SCRIPT_DIR, "config.mako")
    render(config_file, parsed_args)
