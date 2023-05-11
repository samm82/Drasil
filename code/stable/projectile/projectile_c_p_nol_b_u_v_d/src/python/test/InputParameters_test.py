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
expected_valid_inputs = [
    ("default_float", 20.0, 0.785398, 41.0),
    ("default_int",   20,   1,        41),
]
invalid_input_files = ["too_large_theta", "zero_p_target", "zero_theta",
    "zero_v_launch"]

def read_inParams(filename):
    return InputParameters.InputParameters(
        Path("test/test_input") / f"{filename}.txt")

# \brief Tests reading valid input
@mark.parametrize("filename,v_launch,theta,p_target", expected_valid_inputs)
def test_get_input_valid(filename, v_launch, theta, p_target):
    inParams = read_inParams(filename)

    assert isclose(inParams.v_launch, v_launch)
    assert isclose(inParams.theta, theta)
    assert isclose(inParams.p_target, p_target)

# \brief Tests constraint checking valid input
@mark.parametrize("filename", valid_input_files)
def test_input_constraints_valid(filename):
    inParams = read_inParams(filename)

    stdout = StringIO()
    with redirect_stdout(stdout):  
        inParams.input_constraints()
    assert stdout.getvalue() == ""

# \brief Tests constraint checking invalid input
@mark.parametrize("filename", invalid_input_files)
def test_input_constraints_invalid(filename):
    inParams = read_inParams(filename)

    stdout = StringIO()
    with redirect_stdout(stdout):  
        inParams.input_constraints()
    assert "Warning: " in stdout.getvalue()
