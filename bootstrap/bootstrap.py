#! /usr/bin/env python3

import argparse
import pathlib
import os
import sys


class Config:
    def __init__(self, **kwargs):
        pass


def print_help():
    print(
        f"""Usage: {sys.argv[0]} <source_file>
Options:
    -h, --help          Show this help message
    -o, --output <file>     -c, --compile-only  Compile and assemble, but do not link
    -emit-llvm          Use the LLVM representation for assembler and object files"""
    )
    sys.exit(0)


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-o", nargs=1, help="Place the output into <file>")
    parser.add_argument("-c", action="store_true")
    parser.add_argument("-emit-llvm", action="store_true")
    parser.add_argument("source_files", nargs="?")

    args = parser.parse_args()

    if args.help:
        print_help()


if __name__ == "__main__":
    main()
