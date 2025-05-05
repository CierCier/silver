"""Type definitions for Silver.
This module contains type definitions for the Silver Programming Language.
"""

from typing import List


class SilverType:
    """
    Base class for primitive types.
    """

    def __init__(self, name: str, size: int = 0):
        self.name = name
        self.__size = size

    def __repr__(self):
        return f"SilverType({self.name})"

    def size(self, set: int = None):
        """
        Get the size of the type.
        """
        if set is not None:
            self.__size = set
        return self.__size


class SilverStruct(SilverType):
    """
    Class for struct types.
    """

    def __init__(self, name: str, size: int = 0):
        super().__init__(name, size)
        self.fields = {}

    def add_field(self, name: str, type: SilverType):
        """
        Add a field to the struct.
        """
        self.fields[name] = {
            "type": type,
            "offset": super().size(),
        }
        super().size(super().size() + type.size())

    def get_offset(self, name: str):
        """
        Get the offset of a field in the struct.
        """
        return self.fields[name]["offset"] if name in self.fields else -1


class SilverArray(SilverType):
    """
    Class for array types.
    """

    def __init__(self, name: str, type: SilverType, size: int = 0):
        super().__init__(name, size)
        self.type = type
        super().size(type.size() * size)

    def __repr__(self):
        return f"SilverArray({self.name}, {self.type}, {self.size()})"


class SilverPointer(SilverType):
    """
    Class for pointer types.
    """

    def __init__(self, name: str, type: SilverType):
        super().__init__(name)
        self.type = type
        super().size(8)  # Assuming 64-bit pointers

    def __repr__(self):
        return f"SilverPointer({self.name}, {self.type})"


class SilverEnum(SilverType):
    def __init__(self, name: str, variants: List[str]):
        super().__init__(name)
        self.variants = variants

    def __repr__(self):
        return f"SilverEnum({self.name}, {self.variants})"

    def get_variant_index(self, variant: str):
        return self.variants.index(variant)


class SilverUnion(SilverType):
    def __init__(self, name: str, fields: List[tuple] = []):
        super().__init__(name)
        self.fields = fields

    def __repr__(self):
        return f"SilverUnion({self.name}, {self.fields})"

    def size(self):
        return max(field[0].size() for field in self.fields)

    def add_field(self, name: str, type: SilverType):
        self.fields.append((name, type))
        super().size(max(super().size(), type.size()))
