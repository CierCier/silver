#! /usr/bin/env python3

import argparse
import pathlib
import os
import subprocess
import sys

from silver import add_module_path
from silver.parser import Parser, Program, pretty_print
from silver.tokenizer import Tokenizer
from silver.preprocess import Preprocessor
from silver.ir_generator import IRGenerator

import platform


def get_machine_arch():
    machine = platform.machine()
    if machine == "x86_64":
        return "x86_64"
    elif machine == "aarch64":
        return "aarch64"
    else:
        raise Exception(f"Unsupported machine: {machine}")


def get_machine_triple():
    arch = get_machine_arch()
    system = platform.system()
    if system == "Linux":
        return f"{arch}-pc-linux-gnu"
    elif system == "Darwin":
        return f"{arch}-apple-macosx"
    else:
        raise Exception(f"Unsupported system: {system}")


def main():
    parser = argparse.ArgumentParser(add_help=True)
    parser.add_argument(
        "--output", "-o", type=pathlib.Path, help="The output file to write"
    )
    parser.add_argument("--debug", "-d", action="store_true", help="Enable debug mode")
    parser.add_argument(
        "--emit-llvm", "-emit-llvm", action="store_true", help="Emit LLVM IR"
    )
    parser.add_argument(
        "-I",
        "--include",
        action="append",
        type=pathlib.Path,
        help="Include directory",
    )

    parser.add_argument(
        "-c",
        action="store_true",
        help="Compile and assemble but do not link",
    )

    parser.add_argument(
        "-S",
        action="store_true",
        help="Compile but do not assemble or link",
    )

    parser.add_argument(
        "input_file",
        type=pathlib.Path,
        help="The input file to compile",
        nargs="+",
    )

    args = parser.parse_args()

    if not args.input_file:
        print("Error: No input file provided")
        parser.print_help()
        sys.exit(1)

    for include in args.include or []:
        add_module_path(include)  # Add the include directory to the module path

    if args.output:
        output_file = pathlib.Path(args.output)
    else:
        output_file = pathlib.Path("a.out")

    if args.c:
        output_file = output_file.with_suffix(".o")
    elif args.S:
        output_file = output_file.with_suffix(".S")

    input_files = []
    for file in args.input_file:
        input_files.append(pathlib.Path(file))

    if len(input_files) == 0:
        print("Error: No input files provided")
        parser.print_help()
        sys.exit(1)

    preprocessor = Preprocessor(*input_files)
    preprocessed_code = preprocessor.preprocess()

    tokenizer = Tokenizer(preprocessed_code)
    tokens = tokenizer.get_all()

    parser = Parser(tokens)
    program = parser.parse()

    ir_generator = IRGenerator(get_machine_triple())

    ir_code = ir_generator.generate(program)

    ir_file = output_file.with_suffix(".ll")

    with open(ir_file, "w") as f:
        f.write(ir_code)

    # compile the IR to a binary
    command = [
        "clang",
        "-o",
        output_file,
        ir_file,
    ]

    if args.debug:
        command.append("-g")

    if args.emit_llvm:
        command.append("-emit-llvm")

    if args.c:
        command.append("-c")
    elif args.S:
        command.append("-S")

    subprocess.run(command)

    if not args.emit_llvm:
        os.remove(output_file.with_suffix(".ll"))


if __name__ == "__main__":
    main()
