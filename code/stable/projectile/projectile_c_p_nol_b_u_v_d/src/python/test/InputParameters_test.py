## \file InputParametersTest.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pathlib import Path
from sys import path
path.append("../")
from python import InputParameters

# for capturing stdout
from contextlib import redirect_stdout
from io import StringIO

def read_inParams(filename):
    return InputParameters.InputParameters(Path("test/test_input") / f"{filename}.txt")

# \brief Tests reading valid input
def test_get_input_valid():
    for filename in ["default_float", "default_int"]:
        inParams = read_inParams(filename)

        assert isclose(inParams.v_launch, 20)
        if filename.endswith("float"):
            assert isclose(inParams.theta, 0.785398)
        elif filename.endswith("int"):
            assert isclose(inParams.theta, 1)
        assert isclose(inParams.p_target, 41)

# \brief Tests constraint checking valid input
def test_input_constraints_valid():
    for filename in ["default_float", "default_int"]:
        inParams = read_inParams(filename)

        stdout = StringIO()
        with redirect_stdout(stdout):  
            inParams.input_constraints()
        assert stdout.getvalue() == ""
