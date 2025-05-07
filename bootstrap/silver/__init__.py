import os

from pathlib import Path

cwd = Path(os.path.curdir).parent.resolve()


VERSION_MAJOR = 0
VERSION_MINOR = 0
VERSION_PATCH = 1

VERSION_STRING = f"{VERSION_MAJOR}.{VERSION_MINOR}.{VERSION_PATCH}"


MODULE_PATH = [
    f"{cwd}",  ## Higher precedence to cwd
    "/usr/include/silver/",
]
"""Path to locations where we'll search for modules"""

LANG_MODULE_EXTENSION = ".ag"

MODULES = set()


def add_module_path(path: str):
    """Add a module path to the module path list"""
    if path not in MODULE_PATH:
        MODULE_PATH.insert(0, path)
    return MODULE_PATH


def resolve_module_rp(module: str):
    """Get a module from the module path list"""
    module_p = module.replace(".", "/")
    module_file = module_p + LANG_MODULE_EXTENSION

    for path in MODULE_PATH:
        if os.path.exists(path):
            module_path = os.path.join(path, module_file)
            if os.path.exists(module_path):
                return module_path
            else:
                print(f"Module {module_file} not found in {path}")

    raise FileNotFoundError(f"Module {module} not found in {MODULE_PATH}")


def add_module(module: str) -> str:
    mod_path = resolve_module_rp(module)
    if mod_path in MODULES:
        return ""
    MODULES.add(mod_path)
    data = f"//FILE {mod_path}"
    with open(mod_path, "r") as f:
        data += f.read()
    data += "\n"
    data += "//FILE END\n"

    return data


if __name__ == "__main__":
    pass
