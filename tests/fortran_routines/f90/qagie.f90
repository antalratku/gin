!*==QAGIE.spg  processed by SPAG 6.72Dc at 12:05 on 30 Aug 2020
      SUBROUTINE QAGIE(F,Bound,Inf,Epsabs,Epsrel,Limit,Result,Abserr,   &
                     & Neval,Ier,Alist,Blist,Rlist,Elist,Iord,Last)
      IMPLICIT NONE
!*--QAGIE5
!*** Start of declarations inserted by SPAG
      REAL F
!*** End of declarations inserted by SPAG
!***begin prologue  qagie
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1,h2a4a1
!***keywords  automatic integrator, infinite intervals,
!             general-purpose, transformation, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math & progr. div - k.u.leuven
!           de doncker,elise,appl. math & progr. div - k.u.leuven
!***purpose  the routine calculates an approximation result to a given
!            integral   i = integral of f over (bound,+infinity)
!                    or i = integral of f over (-infinity,bound)
!                    or i = integral of f over (-infinity,+infinity),
!                    hopefully satisfying following claim for accuracy
!                    abs(i-result).le.max(epsabs,epsrel*abs(i))
!***description
!
! integration over infinite intervals
! standard fortran subroutine
!
!            f      - subroutine f(x,ierr,result) defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            bound  - real
!                     finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            inf    - real
!                     indicating the kind of integration range involved
!                     inf = 1 corresponds to  (bound,+infinity),
!                     inf = -1            to  (-infinity,bound),
!                     inf = 2             to (-infinity,+infinity).
!
!            epsabs - real
!                     absolute accuracy requested
!            epsrel - real
!                     relative accuracy requested
!                     if  epsabs.le.0
!                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            limit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), limit.ge.1
!
!         on return
!            result - real
!                     approximation to the integral
!
!            abserr - real
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                   - ier.gt.0 abnormal termination of the routine. the
!                             estimates for result and error are less
!                             reliable. it is assumed that the requested
!                             accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             limit (and taking the according dimension
!                             adjustments into account). however,if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties.
!                             if the position of a local difficulty can
!                             be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             result is the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs.le.0 and
!                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                             result, abserr, neval, last, rlist(1),
!                             elist(1) and iord(1) are set to zero.
!                             alist(1) and blist(1) are set to 0
!                             and 1 respectively.
!
!            alist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            blist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            rlist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real
!                     vector of dimension at least limit,  the first
!                     last elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension limit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last.le.(limit/2+2), and k = limit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced
!                     in the subdivision process
!
!***references  (none)
!***routines called  qelg,qk15i,qpsrt,r1mach
!***end prologue  qagie
!
      REAL abseps , Abserr , Alist , area , area1 , area12 , area2 ,    &
         & a1 , a2 , Blist , boun , Bound , b1 , b2 , correc , defabs , &
         & defab1 , defab2 , dres , R1MACH , Elist , epmach , Epsabs ,  &
         & Epsrel , erlarg , erlast , errbnd , errmax , error1 ,        &
         & error2 , erro12 , errsum , ertest , oflow , resabs , reseps ,&
         & Result , res3la , Rlist , rlist2 , small , uflow
      INTEGER id , Ier , ierro , Inf , Iord , iroff1 , iroff2 , iroff3 ,&
            & jupbnd , k , ksgn , ktmin , Last , Limit , maxerr ,       &
            & Neval , nres , nrmax , numrl2
      LOGICAL extrap , noext
!
      DIMENSION Alist(Limit) , Blist(Limit) , Elist(Limit) , Iord(Limit)&
              & , res3la(3) , Rlist(Limit) , rlist2(52)
!
      EXTERNAL F
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine qelg.
!
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least (limexp+2),
!                       containing the part of the epsilon table
!                       wich is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements currently in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained, it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           small     - length of the smallest interval considered up
!                       to now, multiplied by 1.5
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation
!                       is no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
      epmach = R1MACH(4)
!
!           test on validity of parameters
!           -----------------------------
!
!***first executable statement  qagie
      Ier = 0
      Neval = 0
      Last = 0
      Result = 0.0E+00
      Abserr = 0.0E+00
      Alist(1) = 0.0E+00
      Blist(1) = 0.1E+01
      Rlist(1) = 0.0E+00
      Elist(1) = 0.0E+00
      Iord(1) = 0
      IF ( Epsabs<=0.0E+00 .AND. Epsrel<AMAX1(0.5E+02*epmach,0.5E-14) ) &
         & Ier = 6
      IF ( Ier==6 ) GOTO 99999
!
!
!           first approximation to the integral
!           -----------------------------------
!
!           determine the interval to be mapped onto (0,1).
!           if inf = 2 the integral is computed as i = i1+i2, where
!           i1 = integral of f over (-infinity,0),
!           i2 = integral of f over (0,+infinity).
!
      boun = Bound
      IF ( Inf==2 ) boun = 0.0E+00
      CALL QK15I(F,boun,Inf,0.0E+00,0.1E+01,Result,Abserr,defabs,resabs,&
               & Ier)
      IF ( Ier<0 ) RETURN
!
!           test on accuracy
!
      Last = 1
      Rlist(1) = Result
      Elist(1) = Abserr
      Iord(1) = 1
      dres = ABS(Result)
      errbnd = AMAX1(Epsabs,Epsrel*dres)
      IF ( Abserr<=1.0E+02*epmach*defabs .AND. Abserr>errbnd ) Ier = 2
      IF ( Limit==1 ) Ier = 1
      IF ( Ier/=0 .OR. (Abserr<=errbnd .AND. Abserr/=resabs) .OR.       &
         & Abserr==0.0E+00 ) GOTO 400
!
!           initialization
!           --------------
!
      uflow = R1MACH(1)
      oflow = R1MACH(2)
      rlist2(1) = Result
      errmax = Abserr
      maxerr = 1
      area = Result
      errsum = Abserr
      Abserr = oflow
      nrmax = 1
      nres = 0
      ktmin = 0
      numrl2 = 2
      extrap = .FALSE.
      noext = .FALSE.
      ierro = 0
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      IF ( dres>=(0.1E+01-0.5E+02*epmach)*defabs ) ksgn = 1
!
!           main do-loop
!           ------------
!
      DO Last = 2 , Limit
!
!           bisect the subinterval with nrmax-th largest
!           error estimate.
!
         a1 = Alist(maxerr)
         b1 = 0.5E+00*(Alist(maxerr)+Blist(maxerr))
         a2 = b1
         b2 = Blist(maxerr)
         erlast = errmax
         CALL QK15I(F,boun,Inf,a1,b1,area1,error1,resabs,defab1,Ier)
         IF ( Ier<0 ) RETURN
         CALL QK15I(F,boun,Inf,a2,b2,area2,error2,resabs,defab2,Ier)
         IF ( Ier<0 ) RETURN
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
         area12 = area1 + area2
         erro12 = error1 + error2
         errsum = errsum + erro12 - errmax
         area = area + area12 - Rlist(maxerr)
         IF ( defab1/=error1 .AND. defab2/=error2 ) THEN
            IF ( ABS(Rlist(maxerr)-area12)<=0.1E-04*ABS(area12) .AND.   &
               & erro12>=0.99E+00*errmax ) THEN
               IF ( extrap ) iroff2 = iroff2 + 1
               IF ( .NOT.extrap ) iroff1 = iroff1 + 1
            ENDIF
            IF ( Last>10 .AND. erro12>errmax ) iroff3 = iroff3 + 1
         ENDIF
         Rlist(maxerr) = area1
         Rlist(Last) = area2
         errbnd = AMAX1(Epsabs,Epsrel*ABS(area))
!
!           test for roundoff error and eventually
!           set error flag.
!
         IF ( iroff1+iroff2>=10 .OR. iroff3>=20 ) Ier = 2
         IF ( iroff2>=5 ) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals limit.
!
         IF ( Last==Limit ) Ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at some points of the integration range.
!
         IF ( AMAX1(ABS(a1),ABS(b2))<=(0.1E+01+0.1E+03*epmach)          &
            & *(ABS(a2)+0.1E+04*uflow) ) Ier = 4
!
!           append the newly-created intervals to the list.
!
         IF ( error2>error1 ) THEN
            Alist(maxerr) = a2
            Alist(Last) = a1
            Blist(Last) = b1
            Rlist(maxerr) = area2
            Rlist(Last) = area1
            Elist(maxerr) = error2
            Elist(Last) = error1
         ELSE
            Alist(Last) = a2
            Blist(maxerr) = b1
            Blist(Last) = b2
            Elist(maxerr) = error1
            Elist(Last) = error2
         ENDIF
!
!           call subroutine qpsrt to maintain the descending ordering
!           in the list of error estimates and select the
!           subinterval with nrmax-th largest error estimate (to be
!           bisected next).
!
         CALL QPSRT(Limit,Last,maxerr,errmax,Elist,Iord,nrmax)
         IF ( errsum<=errbnd ) GOTO 300
         IF ( Ier/=0 ) GOTO 200
         IF ( Last==2 ) THEN
            small = 0.375E+00
            erlarg = errsum
            ertest = errbnd
            rlist2(2) = area
         ELSEIF ( .NOT.(noext) ) THEN
            erlarg = erlarg - erlast
            IF ( ABS(b1-a1)>small ) erlarg = erlarg + erro12
            IF ( .NOT.(extrap) ) THEN
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
               IF ( ABS(Blist(maxerr)-Alist(maxerr))>small ) GOTO 100
               extrap = .TRUE.
               nrmax = 2
            ENDIF
            IF ( ierro/=3 .AND. erlarg>ertest ) THEN
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors
!           over the larger intervals (erlarg) and perform
!           extrapolation.
!
               id = nrmax
               jupbnd = Last
               IF ( Last>(2+Limit/2) ) jupbnd = Limit + 3 - Last
               DO k = id , jupbnd
                  maxerr = Iord(nrmax)
                  errmax = Elist(maxerr)
                  IF ( ABS(Blist(maxerr)-Alist(maxerr))>small ) GOTO 100
                  nrmax = nrmax + 1
               ENDDO
            ENDIF
!
!           perform extrapolation.
!
            numrl2 = numrl2 + 1
            rlist2(numrl2) = area
            CALL QELG(numrl2,rlist2,reseps,abseps,res3la,nres)
            ktmin = ktmin + 1
            IF ( ktmin>5 .AND. Abserr<0.1E-02*errsum ) Ier = 5
            IF ( abseps<Abserr ) THEN
               ktmin = 0
               Abserr = abseps
               Result = reseps
               correc = erlarg
               ertest = AMAX1(Epsabs,Epsrel*ABS(reseps))
               IF ( Abserr<=ertest ) GOTO 200
            ENDIF
!
!            prepare bisection of the smallest interval.
!
            IF ( numrl2==1 ) noext = .TRUE.
            IF ( Ier==5 ) GOTO 200
            maxerr = Iord(1)
            errmax = Elist(maxerr)
            nrmax = 1
            extrap = .FALSE.
            small = small*0.5E+00
            erlarg = errsum
         ENDIF
 100  ENDDO
!
!           set final result and error estimate.
!           ------------------------------------
!
 200  IF ( Abserr/=oflow ) THEN
         IF ( (Ier+ierro)/=0 ) THEN
            IF ( ierro==3 ) Abserr = Abserr + correc
            IF ( Ier==0 ) Ier = 3
            IF ( Result==0.0E+00 .OR. area==0.0E+00 ) THEN
               IF ( Abserr>errsum ) GOTO 300
               IF ( area==0.0E+00 ) GOTO 400
            ELSEIF ( Abserr/ABS(Result)>errsum/ABS(area) ) THEN
               GOTO 300
            ENDIF
         ENDIF
!
!           test on divergence
!
         IF ( ksgn/=(-1) .OR. AMAX1(ABS(Result),ABS(area))              &
            & >defabs*0.1E-01 ) THEN
            IF ( 0.1E-01>(Result/area) .OR. (Result/area)>0.1E+03 .OR.  &
               & errsum>ABS(area) ) Ier = 6
         ENDIF
         GOTO 400
      ENDIF
!
!           compute global integral sum.
!
 300  Result = 0.0E+00
      DO k = 1 , Last
         Result = Result + Rlist(k)
      ENDDO
      Abserr = errsum
 400  Neval = 30*Last - 15
      IF ( Inf==2 ) Neval = 2*Neval
      IF ( Ier>2 ) Ier = Ier - 1
99999 END
