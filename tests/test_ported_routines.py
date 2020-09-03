import sys, os
sys.path.insert(0, os.path.abspath('..'))
sys.path.insert(0, os.path.join(os.getcwd(), 'tests/fortran_routines'))
import fortran_routines.f90.f_routines as f
from gin import ported_routines
import pytest
import numpy as np


@pytest.mark.parametrize('i', [1, 2, 3, 4, 5])
def test_r1mach(i):
    fortran_val = f.r1mach(i)
    ported_val = ported_routines.r1mach(i)
    
    assert(fortran_val == ported_val)


@pytest.mark.parametrize('fun_args', [
    (lambda x: (np.exp(-0.5*x**2), 0), ())
])
@pytest.mark.parametrize('boun_inf', [
    (0.5, 1),
    (1.5, 1),
    (-0.5, -1),
    (-1.5, -1)
])
@pytest.mark.parametrize('a', np.linspace(0, 0.4, 3, endpoint=True))
@pytest.mark.parametrize('b', np.linspace(0.6, 1.0, 3, endpoint=True))
def test_qk15i(fun_args, boun_inf, a, b):
    fun = fun_args[0]
    args = fun_args[1]
    boun = boun_inf[0]
    inf = boun_inf[1]

    ported_output = ported_routines.qk15i(fun, boun, inf, a, b, *args)
    fortran_output = f.qk15i(fun, boun, inf, a, b, args)
    
    epsilon = 1e-5

    assert np.abs(ported_output[0] - fortran_output[0]) < epsilon
    assert np.abs(ported_output[1] - fortran_output[1]) < epsilon
    assert np.abs(ported_output[2] - fortran_output[2]) < epsilon
    assert np.abs(ported_output[3] - fortran_output[3]) < epsilon
    assert ported_output[4] == fortran_output[4]

