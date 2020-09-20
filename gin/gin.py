import sys, os
sys.path.insert(0, os.path.abspath(''))
from numba import jit
from gin import ported_routines_numba as prn
import numpy as np
from numba import cuda

class Gin:
        
    def __init__(self, fun, bound, inf, epsabs, epsrel, limit, griddim, blockdim):
        self.fun = fun
        self.bound = bound
        self.inf = inf
        self.epsabs = epsabs
        self.epsrel = epsrel
        self.limit = limit
        self.griddim = griddim
        self.blockdim = blockdim

        self._XGK = np.array([0.9914553711208126e+00, 0.9491079123427585e+00, 0.8648644233597691e+00, 0.7415311855993944e+00,
                              0.5860872354676911e+00, 0.4058451513773972e+00, 0.2077849550078985e+00, 0.0000000000000000e+00],
                              dtype=np.float64)
        self._WGK = np.array([0.2293532201052922e-01, 0.6309209262997855e-01, 0.1047900103222502e+00, 0.1406532597155259e+00,
                              0.1690047266392679e+00, 0.1903505780647854e+00, 0.2044329400752989e+00, 0.2094821410847278e+00],
                              dtype=np.float64)
        self._WG = np.array([0.0000000000000000e+00, 0.1294849661688697e+00, 0.0000000000000000e+00, 0.2797053914892767e+00,
                             0.0000000000000000e+00, 0.3818300505051189e+00, 0.0000000000000000e+00, 0.4179591836734694e+00],
                             dtype=np.float64)
        
        self._push_helpers_to_gpu()
        self._initialize_kernel()


    def _push_helpers_to_gpu(self):
        
        self.d_XGK = cuda.to_device(self._XGK)
        self.d_WGK = cuda.to_device(self._WGK)
        self.d_WG = cuda.to_device(self._WG)
        self.d_alist = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, self.limit), dtype=np.float64))
        self.d_blist = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, self.limit), dtype=np.float64))
        self.d_elist = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, self.limit), dtype=np.float64))
        self.d_iord = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, self.limit), dtype=np.int))
        self.d_res3la = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, 3), dtype=np.float64))
        self.d_rlist = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, self.limit), dtype=np.float64))
        self.d_rlist2 = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, 52), dtype=np.float64))
        self.d_fv1 = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, 7), dtype=np.float64))
        self.d_fv2 = cuda.to_device(np.zeros(shape=(self.griddim * self.blockdim, 7), dtype=np.float64))


    def _initialize_kernel(self):

        fun = cuda.jit(device=True)(self.fun)

        def _integrator_kernel(result, abserr, ier,
                               bound, inf, epsabs, epsrel, limit,
                               alist, blist, elist, iord, res3la, rlist, rlist2,
                               xgk, wgk, wg,
                               fv1, fv2, args):

            startX = cuda.grid(1)
            gridX = cuda.gridDim.x * cuda.blockDim.x

            for x in range(startX, result.shape[0], gridX):
                if x < result.size:
                    temp_result = prn.qagie(fun, bound, inf, epsabs, epsrel, limit,
                                            alist[startX, :], blist[startX, :], elist[startX, :], iord[startX, :],
                                            res3la[startX, :], rlist[startX, :], rlist2[startX, :],
                                            xgk, wgk, wg,
                                            fv1[startX, :], fv2[startX, :], args[x, :])
                    result[x] = temp_result[0]
                    abserr[x] = temp_result[1]
                    ier[x] = temp_result[3]
                    for i in range(limit):
                        alist[startX, i] = 0.0
                        blist[startX, i] = 0.0
                        elist[startX, i] = 0.0
                        iord[startX, i] = 0
                        rlist[startX, i] = 0.0
                    for i in range(3):
                        res3la[startX, i] = 0.0
                    for i in range(52):
                        rlist2[startX, i] = 0.0
                    for i in range(7):
                        fv1[startX, i] = 0.0
                        fv2[startX, i] = 0.0

        
        self._integrator_kernel = cuda.jit(_integrator_kernel)

        
    def integrate(self, args):

        d_args = cuda.to_device(args)
        
        result = np.zeros(shape=len(args), dtype=np.float64)
        d_result = cuda.to_device(result)
        abserr = np.zeros(shape=len(args), dtype=np.float64)
        d_abserr = cuda.to_device(abserr)
        ier = np.zeros(shape=len(args), dtype=np.int)
        d_ier = cuda.to_device(ier)

        self._integrator_kernel[self.griddim, self.blockdim](d_result, d_abserr, d_ier,
                                                             self.bound, self.inf, self.epsabs, self.epsrel, self.limit,
                                                             self.d_alist, self.d_blist, self.d_elist, self.d_iord,
                                                             self.d_res3la, self.d_rlist, self.d_rlist2,
                                                             self.d_XGK, self.d_WGK, self.d_WG,
                                                             self.d_fv1, self.d_fv2, d_args)
        d_result.to_host()
        d_abserr.to_host()
        d_ier.to_host()
        return (result, abserr, ier)

