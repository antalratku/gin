import sys, os
sys.path.insert(0, os.path.abspath('..'))
sys.path.insert(0, os.path.join(os.getcwd(), 'tests/fortran_routines'))
import fortran_routines.f90.f_routines as f
from gin import ported_routines
import pytest


@pytest.mark.parametrized
@pytest.mark.parametrize('i', [1, 2, 3, 4, 5])
def test_r1mach(i):
    fortran_val = f.r1mach(i)
    ported_val = ported_routines.r1mach(i)
    
    assert(fortran_val == ported_val)

