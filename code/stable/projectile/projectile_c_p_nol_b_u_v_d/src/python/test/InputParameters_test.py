## \file InputParameters_test.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark

from .TestHelpers import get_expected, read_inParams

# for capturing stdout
from contextlib import redirect_stdout
from io import StringIO

valid_input_files = ["default_float", "default_int"]
invalid_input_files = [
    "zero_v_launch",   # violates lower bound of v_launch
    "zero_theta",      # violates lower bound of theta
    "too_large_theta", # violates upper bound of theta
    "zero_p_target",   # violates lower bound of p_target
]

# \brief Tests reading valid input
@mark.parametrize("filename,v_launch,theta,p_target",
                  get_expected("v_launch", "theta", "p_target"))
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
