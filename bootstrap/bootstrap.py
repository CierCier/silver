#! /usr/bin/env python3

import argparse
import pathlib
import os
import sys

from silver import add_module_path
from silver.parser import Parser, Program
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


class Config:
    def __init__(self, **kwargs):
        self.output = kwargs.get("output")
        self.debug = kwargs.get("debug", False)
        self.emit_llvm = kwargs.get("emit_llvm", False)
        self.emit_llvm_bc = kwargs.get("emit_llvm_bc", False)
        self.emit_llvm_ir = kwargs.get("emit_llvm_ir", False)
        self.emit_llvm_asm = kwargs.get("emit_llvm_asm", False)
        self.include_dirs = kwargs.get("include_dirs", [])
        self.input_files = kwargs.get("input_files", [])

        if not self.output:
            self.output = self.input_files[0].with_suffix(".ll")

        if (
            not self.emit_llvm
            and not self.emit_llvm_bc
            and not self.emit_llvm_ir
            and not self.emit_llvm_asm
        ):
            self.emit_llvm = True


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
        "--emit-llvm-bc",
        "-emit-llvm-bc",
        action="store_true",
        help="Emit LLVM IR in binary format",
    )
    parser.add_argument(
        "--emit-llvm-ir",
        "-emit-llvm-ir",
        action="store_true",
        help="Emit LLVM IR in text format",
    )
    parser.add_argument(
        "--emit-llvm-asm",
        "-emit-llvm-asm",
        action="store_true",
        help="Emit LLVM IR in assembly format",
    )
    parser.add_argument(
        "-I",
        "--include",
        action="append",
        type=pathlib.Path,
        help="Include directory",
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

    config = Config(
        output=args.output,
        debug=args.debug,
        emit_llvm=args.emit_llvm,
        emit_llvm_bc=args.emit_llvm_bc,
        emit_llvm_ir=args.emit_llvm_ir,
        emit_llvm_asm=args.emit_llvm_asm,
        include_dirs=args.include or [],
        input_files=args.input_file,
    )

    for include in config.include_dirs:
        add_module_path(include)  # Add the include directory to the module path

    if config.output:
        output_file = config.output
    else:
        output_file = "a"

    preprocessor = Preprocessor(*config.input_files)
    preprocessed_code = preprocessor.preprocess()

    tokenizer = Tokenizer(preprocessed_code)
    tokens = tokenizer.get_all()

    parser = Parser(tokens)
    program = parser.parse()

    ir_generator = IRGenerator(get_machine_triple())

    ir_code = ir_generator.generate(program)

    with open(output_file.with_suffix(".ll"), "w") as f:
        f.write(ir_code)


if __name__ == "__main__":
    main()
