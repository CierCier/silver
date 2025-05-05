from pathlib import Path
import re
from silver import add_module


class Preprocessor:
    def __init__(self, *sourceFiles):
        self.sourceFiles = sourceFiles
        self.data = []
        for file in sourceFiles:
            f = Path(file)
            if not f.exists():
                raise FileNotFoundError(f"File {file} not found")
            with open(f, "r") as f:
                self.data.append(f"//FILE {f.name}\n")
                self.data.extend(f.readlines())

    def preprocess(self) -> str:
        """Perform preprocessing on the source files"""
        self.expand_imports()

        return "".join(self.data)

    import_regex = re.compile(r"^\s*import\s+([a-zA-Z0-9_.]+);")

    def expand_imports(self):
        for line in self.data:
            if self.import_regex.match(line):
                module = line.replace("import ", "").replace(";", "").strip()
                module_data = add_module(module)
                module_data = module_data.splitlines(keepends=True)

                i = self.data.index(line)
                self.data = self.data[:i] + module_data + self.data[i + 1 :]


if __name__ == "__main__":
    import sys

    files = [Path(x) for x in sys.argv[1:]]

    preprocessor = Preprocessor(*files)
    data = preprocessor.preprocess()

    print(data)
