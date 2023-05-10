## \file InputParametersTest.py
# \author Samuel J. Crawford
# \brief Runs tests for the input of parameters

from math import isclose
from pathlib import Path

import sys
sys.path.append("../")
from python import InputParameters

# \brief Tests reading valid input
def test_valid():
    for filename in ["default_float", "default_int"]:
        inParams = InputParameters.InputParameters(Path("test/test_input") / f"{filename}.txt")
        
        assert isclose(inParams.v_launch, 20)
        if filename.endswith("float"):
            assert isclose(inParams.theta, 0.785398)
        elif filename.endswith("int"):
            assert isclose(inParams.theta, 1)
        assert isclose(inParams.p_target, 41)
