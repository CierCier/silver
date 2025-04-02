from typing import List
from generator import Generator
from lexer import Lexer
from parser import Parser

from pathlib import Path

import subprocess


import os
import sys


class Bootstrap:
    def __init__(
        self,
        inputs=List[Path],
        output: Path = Path("a.out"),
        verbose: bool = False,
        debug: bool = False,
        emit_llvm: bool = False,
        emit_asm: bool = False,
        emit_obj: bool = False,
    ):
        self.inputs = inputs
        self.output = output
        self.verbose = verbose
        self.debug = debug

        self.emit_llvm = emit_llvm
        self.emit_asm = emit_asm
        self.emit_obj = emit_obj

        self.start()

    def start(self):
        lexer = Lexer()
        tokens = []
        for input_file in self.inputs:
            lexer.set_input(input_file)
            tokens.extend(lexer.tokenize())

        if self.verbose:
            print("Tokens:")
            for token in tokens:
                print(token, end=" ")
            print()

        parser = Parser(tokens)
        ast = parser.parse()

        if self.verbose:
            print("AST:")
            print(ast)
            print()

        generator = Generator(ast)

        llvm_ir = generator.generate_start()
        ir_file = Path(self.output).with_suffix(".ll")

        ir_file.write_bytes(llvm_ir.encode("utf-8"))

        asm_file = Path(self.output).with_suffix(".s")

        subprocess.run(
            ["llc", str(ir_file), "-o", str(asm_file)],
            check=True,
        )

        output_file = Path(self.output)
        subprocess.run(
            ["clang", str(asm_file), "-o", str(output_file), "-static"],
            check=True,
        )

        if not self.emit_llvm:
            ir_file.unlink()
        if not self.emit_asm:
            asm_file.unlink()


def parse_args(args: List[str]):
    config = {
        "input_files": [],
        "output_file": None,
        "verbose": False,
        "debug": False,
        "show_help": False,
    }

    i = 0
    while i < len(args):
        if args[i] == "-o" or args[i] == "--output":
            config["output_file"] = Path(args[i + 1])
            i += 1
        elif args[i] == "-v" or args[i] == "--verbose":
            config["verbose"] = True
        elif args[i] == "-d" or args[i] == "--debug":
            config["debug"] = True
        elif args[i] == "--help" or args[i] == "-h":
            config["show_help"] = True
        elif args[i] == "-emit-llvm":
            config["emit_llvm"] = True
        elif args[i] == "-emit-asm":
            config["emit_asm"] = True
        elif args[i] == "-emit-obj":
            config["emit_obj"] = True
        else:
            config["input_files"].append(Path(args[i]))

        if config["show_help"]:
            print("Usage: bootstrap.py [options] <input_files>")
            print("Options:")
            print("  -o --output <output_file> \tSpecify output file")
            print("  -v --verbose \t\t\tEnable verbose mode")
            print("  -d --debug \t\t\tEnable debug mode")
            print("  -h, --help \t\t\tShow this help message")
            print("  -emit-llvm \t\t\tEmit LLVM IR")
            print("  -emit-asm \t\t\tEmit assembly code")
            print("  -emit-obj \t\t\tEmit object code")
            sys.exit(0)
        i += 1

    if not config["input_files"]:
        print("No input files specified.")
        print("Usage: bootstrap.py [options] <input_files>")
        sys.exit(1)

    if config["output_file"] is None:
        config["output_file"] = "a.out"

    return config


def main():
    if len(sys.argv) < 2:
        print("Usage: python bootstrap.py <input_files>")
        sys.exit(1)

    args = sys.argv[1:]
    config = parse_args(args)

    inputs = config["input_files"]
    output = config["output_file"]
    verbose = config["verbose"]
    debug = config["debug"]
    emit_llvm = config.get("emit_llvm", False)
    emit_asm = config.get("emit_asm", False)
    emit_obj = config.get("emit_obj", False)

    Bootstrap(
        inputs=inputs,
        output=output,
        verbose=verbose,
        debug=debug,
        emit_llvm=emit_llvm,
        emit_asm=emit_asm,
        emit_obj=emit_obj,
    )


if __name__ == "__main__":
    main()
