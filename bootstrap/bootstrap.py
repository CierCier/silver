#! /usr/bin/env python3

import argparse
import pathlib
import os
import sys

from silver import add_module_path


class Config:
    def __init__(self, **kwargs):
        pass


def main():
    parser = argparse.ArgumentParser(add_help=True)

    parser.add_argument("-o", nargs=1, help="Place the output into <file>")
    parser.add_argument("-c", action="store_true")
    parser.add_argument("-emit-llvm", action="store_true")
    parser.add_argument("source_files", nargs="?")
    parser.add_argument(
        "-I",
        "--include-path",
        action="append",
        help="Add <path> to the list of directories to be searched for header files.",
    )

    args = parser.parse_args()
    if args.source_files is None:
        print(f"{sys.argv[0]}: error: no input files")
        sys.exit(1)

    if args.include_path:
        for path in args.include_path:
            path = pathlib.Path(path).resolve()
            if not path.exists():
                print(f"{sys.argv[0]}: error: {path} does not exist")
                sys.exit(1)
            if not path.is_dir():
                print(f"{sys.argv[0]}: error: {path} is not a directory")
                sys.exit(1)
            add_module_path(str(path))

    if args.o:
        output_file = args.o[0]
    else:
        output_file = "a.out"


if __name__ == "__main__":
    main()
