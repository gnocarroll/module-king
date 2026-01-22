"""
loc.py

count total LOC in src directory
"""

import argparse
import os
from pathlib import Path
from typing import Generator

FILEPATH = Path(__file__)
FILENAME = FILEPATH.name

# -> scripts -> project dir

PROJECT_DIR = FILEPATH.parent.parent

SRC_DIR = PROJECT_DIR / "src"

parser = argparse.ArgumentParser(
    FILENAME,
    "count LOC in src directory",
)

parser.add_argument(
    "--ext",
    default=".rs",
)

ext: str = parser.parse_args().ext

def listdir_recursive(path: str = ".") -> Generator[str, None, None]:
    """
    Similar to os.listdir except returns full paths (not just filenames)
    and is recursive so will recurse to child dirs
    (and grandchild dirs, etc.).
    
    :param path: directory to list entries from (recursively)
    :type path: str
    :return: yields full paths for child files of directory
    :rtype: Generator[str, None, None]
    """

    for child_path in Path(path).iterdir():
        child_path_str = str(child_path)
        is_dir = child_path.is_dir()

        if not is_dir:
            yield child_path_str
        else:
            for result in listdir_recursive(child_path_str):
                yield result

loc = 0

for filepath in listdir_recursive(SRC_DIR):
    # only add loc from files with right ext

    if not filepath.endswith(ext):
        continue

    with open(filepath, "r") as f:
        loc += sum(1 for _ in f)

print(loc)