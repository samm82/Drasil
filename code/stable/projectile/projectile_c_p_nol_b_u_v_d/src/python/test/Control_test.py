## \filename ControlTest.py
# \author Samuel J. Crawford
# \brief Runs tests for the program
import sys
import pytest
from pathlib import Path

from python import Control

def read_file(filename):
    with open(filename) as f:
        lines = f.readlines()
    return lines

# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
@pytest.mark.parametrize("filename", ["default_float", "default_int", "projectile_went_long"])
def test_main_valid(monkeypatch, filename):
    with monkeypatch.context() as m:
        m.setattr(sys, 'argv', ['Control.py', str(Path("test/test_input") / f"{filename}.txt")])
        Control.main()
        assert read_file(f"output.txt") == read_file(str(Path("test/test_output") / f"{filename}.txt"))
