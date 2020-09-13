# The gin library
A GPU accelerated numerical integration library.

The library ports selected functions of the QUADPACK Fortran library to Python.

Parallel calls to the ported integration routines are accelerated using CUDA. The acceleration is achieved with the help of [`numba`](https://numba.pydata.org/).

# Testing
`pytest` and `pytest-cov` are used for unit testing and test coverage calculations.

- To run all unit tests: `python -m -pytest`.
- To run all unit tests for a specific function: `python -m pytest ./tests/test_ported_routines.py::test_qk15i`.
- To run all unit tests and calculate the test coverage: `python -m pytest --cov-report term-missing --cov=gin tests/`.

## Current test coverage

```console
----------- coverage: platform linux, python 3.8.5-final-0 -----------
Name                     Stmts   Miss  Cover   Missing
------------------------------------------------------
gin/__init__.py              0      0   100%
gin/ported_routines.py     451     25    94%   254, 324, 371, 378, 384, 409, 472-489, 518
------------------------------------------------------
TOTAL                      451     25    94%
```

# Copyright and license
MIT license: see LICENSE file for details.
Copyright (c) 2020 Antal Ratku