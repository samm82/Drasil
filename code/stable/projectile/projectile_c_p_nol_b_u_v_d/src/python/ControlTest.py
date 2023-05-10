## \file ControlTest.py
# \author Samuel J. Crawford
# \brief Runs tests for the program
import sys
import pytest
from pathlib import Path

import Control

def read_file(filename):
    with open(filename) as f:
        lines = f.readlines()
    return lines

# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
@pytest.mark.parametrize("file", ["default", "typical"])
def test_valid(monkeypatch, file):
    with monkeypatch.context() as m:
        m.setattr(sys, 'argv', ['Control.py', str(Path("test_input") / f"{file}.txt")])
        Control.main()
        assert read_file(f"output.txt") == read_file(str(Path("test_output") / f"{file}.txt"))

# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
@pytest.mark.parametrize("file", ["zero_target_position"])
def test_exception(monkeypatch, file):
    with monkeypatch.context() as m:
        m.setattr(sys, 'argv', ['Control.py', str(Path("test_input") / f"{file}.txt")])
        with pytest.raises(ZeroDivisionError):
            Control.main()
