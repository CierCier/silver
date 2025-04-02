#! /usr/bin/env python3

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

        if self.emit_llvm:
            self.__emit_llvm(llvm_ir, self.output)
        if self.emit_asm:
            self.__emit_asm(llvm_ir, self.output)
        if self.emit_obj:
            self.__emit_obj(llvm_ir, self.output)

        self.__compile(llvm_ir, self.output)

    def __emit_llvm(self, llvm_ir: str, output: Path):
        ir_file = Path(output).with_suffix(".ll")
        ir_file.write_text(llvm_ir)

    def __emit_asm(self, llvm_ir: str, output: Path):
        from subprocess import PIPE

        asm_file = Path(output).with_suffix(".s")

        # pipe the llvm ir to llc

        process = subprocess.Popen(
            ["llc"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        stdout, stderr = process.communicate(input=llvm_ir.encode())
        if process.returncode != 0:
            print("Error: ", stderr.decode())
            sys.exit(1)
        asm_file.write_text(stdout.decode())

    def __emit_obj(self, llvm_ir: str, output: Path):
        from subprocess import PIPE

        obj_file = Path(output).with_suffix(".o")

        # pipe the llvm ir to llc
        process = subprocess.Popen(
            ["llc", "-filetype=obj"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        stdout, stderr = process.communicate(input=llvm_ir.encode())
        if process.returncode != 0:
            print("Error: ", stderr.decode())
            sys.exit(1)
        obj_file.write_bytes(stdout)

    def __compile(self, llvm_ir: str, output: Path):
        output = Path(output)
        self.__emit_llvm(llvm_ir, output)
        command = [
            "clang",
            "-fPIE",
            "-pie",
            "-o",
            output,  # output file
            Path(output).with_suffix(".ll"),  # input file
            "-lm",
        ]

        subprocess.run(command, check=True)


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
