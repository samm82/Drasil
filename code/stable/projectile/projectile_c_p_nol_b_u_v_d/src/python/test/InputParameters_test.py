## \file InputParametersTest.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pathlib import Path
from pytest import mark
from sys import path
path.append("../")
from python import InputParameters

# for capturing stdout
from contextlib import redirect_stdout
from io import StringIO

valid_input_files = ["default_float", "default_int"]

def read_inParams(filename):
    return InputParameters.InputParameters(Path("test/test_input") / f"{filename}.txt")

# \brief Tests reading valid input
@mark.parametrize("filename", valid_input_files)
def test_get_input_valid(filename):
    inParams = read_inParams(filename)

    assert isclose(inParams.v_launch, 20)
    if filename.endswith("float"):
        assert isclose(inParams.theta, 0.785398)
    elif filename.endswith("int"):
        assert isclose(inParams.theta, 1)
    assert isclose(inParams.p_target, 41)

# \brief Tests constraint checking valid input
@mark.parametrize("filename", valid_input_files)
def test_input_constraints_valid(filename):
    inParams = read_inParams(filename)

    stdout = StringIO()
    with redirect_stdout(stdout):  
        inParams.input_constraints()
    assert stdout.getvalue() == ""
