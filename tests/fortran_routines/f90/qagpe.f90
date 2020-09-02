!*==QAGPE.spg  processed by SPAG 6.72Dc at 08:15 on  2 Sep 2020
      SUBROUTINE QAGPE(F,A,B,Npts2,Points,Epsabs,Epsrel,Limit,Result,   &
                     & Abserr,Neval,Ier,Alist,Blist,Rlist,Elist,Pts,    &
                     & Iord,Level,Ndin,Last)
      IMPLICIT NONE
!*--QAGPE6
!*** Start of declarations inserted by SPAG
      REAL F
!*** End of declarations inserted by SPAG
!***begin prologue  qagpe
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, general-purpose,
!             singularities at user specified points,
!             extrapolation, globally adaptive.
!***author  piessens,robert ,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result to a given
!            definite integral i = integral of f over (a,b),hopefully
!            satisfying following claim for accuracy abs(i-result).le.
!            max(epsabs,epsrel*abs(i)). break points of the integration
!            interval, where local difficulties of the integrand may
!            occur(e.g. singularities,discontinuities),provided by user.
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real version
!
!        parameters
!         on entry
!            f      - subroutine f(x,ierr,result) defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real
!                     lower limit of integration
!
!            b      - real
!                     upper limit of integration
!
!            npts2  - integer
!                     number equal to two more than the number of
!                     user-supplied break points within the integration
!                     range, npts2.ge.2.
!                     if npts2.lt.2, the routine will end with ier = 6.
!
!            points - real
!                     vector of dimension npts2, the first (npts2-2)
!                     elements of which are the user provided break
!                     points. if these points do not constitute an
!                     ascending sequence there will be an automatic
!                     sorting.
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
!                     in the partition of (a,b), limit.ge.npts2
!                     if limit.lt.npts2, the routine will end with
!                     ier = 6.
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
!                     ier.gt.0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             limit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (i.e. singularity,
!                             discontinuity within the interval), it
!                             should be supplied to the routine as an
!                             element of the vector points. if necessary
!                             an appropriate special-purpose integrator
!                             must be used, which is designed for
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
!                             extrapolation table. it is presumed that
!                             the requested tolerance cannot be
!                             achieved, and that the returned result is
!                             the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.gt.0.
!                         = 6 the input is invalid because
!                             npts2.lt.2 or
!                             break points are specified outside
!                             the integration range or
!                             (epsabs.le.0 and
!                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
!                             or limit.lt.npts2.
!                             result, abserr, neval, last, rlist(1),
!                             and elist(1) are set to zero. alist(1) and
!                             blist(1) are set to a and b respectively.
!
!            alist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the left end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            blist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the right end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            rlist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real
!                     vector of dimension at least limit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            pts    - real
!                     vector of dimension at least npts2, containing the
!                     integration limits and the break points of the
!                     interval in ascending sequence.
!
!            level  - integer
!                     vector of dimension at least limit, containing the
!                     subdivision levels of the subinterval, i.e. if
!                     (aa,bb) is a subinterval of (p1,p2) where p1 as
!                     well as p2 is a user-provided break point or
!                     integration limit, then (aa,bb) has level l if
!                     abs(bb-aa) = abs(p2-p1)*2**(-l).
!
!            ndin   - integer
!                     vector of dimension at least npts2, after first
!                     integration over the intervals (pts(i)),pts(i+1),
!                     i = 0,1, ..., npts2-2, the error estimates over
!                     some of the intervals may have been increased
!                     artificially, in order to put their subdivision
!                     forward. if this happens for the subinterval
!                     numbered k, ndin(k) is put to 1, otherwise
!                     ndin(k) = 0.
!
!            iord   - integer
!                     vector of dimension at least limit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last.le.(limit/2+2), and k = limit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced in the
!                     subdivisions process
!
!***references  (none)
!***routines called  qelg,qk21,qpsrt,r1mach
!***end prologue  qagpe
      REAL A , abseps , Abserr , Alist , area , area1 , area12 , area2 ,&
         & a1 , a2 , B , Blist , b1 , b2 , correc , defabs , defab1 ,   &
         & defab2 , dres , R1MACH , Elist , epmach , Epsabs , Epsrel ,  &
         & erlarg , erlast , errbnd , errmax , error1 , erro12 ,        &
         & error2 , errsum , ertest , oflow , Points , Pts , resa ,     &
         & resabs , reseps , Result , res3la , Rlist , rlist2 , sign ,  &
         & temp , uflow
      INTEGER i , id , Ier , ierro , ind1 , ind2 , Iord , ip1 , iroff1 ,&
            & iroff2 , iroff3 , j , jlow , jupbnd , k , ksgn , ktmin ,  &
            & Last , levcur , Level , levmax , Limit , maxerr , Ndin ,  &
            & Neval , nint , nintp1 , npts , Npts2 , nres , nrmax ,     &
            & numrl2
      LOGICAL extrap , noext
!
!
      DIMENSION Alist(Limit) , Blist(Limit) , Elist(Limit) , Iord(Limit)&
              & , Level(Limit) , Ndin(Npts2) , Points(Npts2) ,          &
              & Pts(Npts2) , res3la(3) , Rlist(Limit) , rlist2(52)
!
      EXTERNAL F
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine epsalg (rlist2 should be of dimension
!            (limexp+2) at least).
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
!           rlist2    - array of dimension at least limexp+2
!                       containing the part of the epsilon table which
!                       is still needed for further computations
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
!           numrl2    - number of elements in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained, it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation is
!                       no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  qagpe
      epmach = R1MACH(4)
!
!            test on validity of parameters
!            -----------------------------
!
      Ier = 0
      Neval = 0
      Last = 0
      Result = 0.0E+00
      Abserr = 0.0E+00
      Alist(1) = A
      Blist(1) = B
      Rlist(1) = 0.0E+00
      Elist(1) = 0.0E+00
      Iord(1) = 0
      Level(1) = 0
      npts = Npts2 - 2
      IF ( Npts2<2 .OR. Limit<=npts .OR.                                &
         & (Epsabs<=0.0E+00 .AND. Epsrel<AMAX1(0.5E+02*epmach,0.5E-14)) &
         & ) Ier = 6
      IF ( Ier/=6 ) THEN
!
!            if any break points are provided, sort them into an
!            ascending sequence.
!
         sign = 1.0E+00
         IF ( A>B ) sign = -1.0E+00
         Pts(1) = AMIN1(A,B)
         IF ( npts/=0 ) THEN
            DO i = 1 , npts
               Pts(i+1) = Points(i)
            ENDDO
         ENDIF
         Pts(npts+2) = AMAX1(A,B)
         nint = npts + 1
         a1 = Pts(1)
         IF ( npts/=0 ) THEN
            nintp1 = nint + 1
            DO i = 1 , nint
               ip1 = i + 1
               DO j = ip1 , nintp1
                  IF ( Pts(i)>Pts(j) ) THEN
                     temp = Pts(i)
                     Pts(i) = Pts(j)
                     Pts(j) = temp
                  ENDIF
               ENDDO
            ENDDO
            IF ( Pts(1)/=AMIN1(A,B) .OR. Pts(nintp1)/=AMAX1(A,B) )      &
               & Ier = 6
            IF ( Ier==6 ) GOTO 99999
         ENDIF
!
!            compute first integral and error approximations.
!            ------------------------------------------------
!
         resabs = 0.0E+00
         DO i = 1 , nint
            b1 = Pts(i+1)
            CALL QK21(F,a1,b1,area1,error1,defabs,resa,Ier)
            IF ( Ier<0 ) RETURN
            Abserr = Abserr + error1
            Result = Result + area1
            Ndin(i) = 0
            IF ( error1==resa .AND. error1/=0.0E+00 ) Ndin(i) = 1
            resabs = resabs + defabs
            Level(i) = 0
            Elist(i) = error1
            Alist(i) = a1
            Blist(i) = b1
            Rlist(i) = area1
            Iord(i) = i
            a1 = b1
         ENDDO
         errsum = 0.0E+00
         DO i = 1 , nint
            IF ( Ndin(i)==1 ) Elist(i) = Abserr
            errsum = errsum + Elist(i)
         ENDDO
!
!           test on accuracy.
!
         Last = nint
         Neval = 21*nint
         dres = ABS(Result)
         errbnd = AMAX1(Epsabs,Epsrel*dres)
         IF ( Abserr<=0.1E+03*epmach*resabs .AND. Abserr>errbnd )       &
            & Ier = 2
         IF ( nint/=1 ) THEN
            DO i = 1 , npts
               jlow = i + 1
               ind1 = Iord(i)
               DO j = jlow , nint
                  ind2 = Iord(j)
                  IF ( Elist(ind1)<=Elist(ind2) ) THEN
                     ind1 = ind2
                     k = j
                  ENDIF
               ENDDO
               IF ( ind1/=Iord(i) ) THEN
                  Iord(k) = Iord(i)
                  Iord(i) = ind1
               ENDIF
            ENDDO
            IF ( Limit<Npts2 ) Ier = 1
         ENDIF
         IF ( Ier/=0 .OR. Abserr<=errbnd ) GOTO 99999
!
!           initialization
!           --------------
!
         rlist2(1) = Result
         maxerr = Iord(1)
         errmax = Elist(maxerr)
         area = Result
         nrmax = 1
         nres = 0
         numrl2 = 1
         ktmin = 0
         extrap = .FALSE.
         noext = .FALSE.
         erlarg = errsum
         ertest = errbnd
         levmax = 1
         iroff1 = 0
         iroff2 = 0
         iroff3 = 0
         ierro = 0
         uflow = R1MACH(1)
         oflow = R1MACH(2)
         Abserr = oflow
         ksgn = -1
         IF ( dres>=(0.1E+01-0.5E+02*epmach)*resabs ) ksgn = 1
!
!           main do-loop
!           ------------
!
         DO Last = Npts2 , Limit
!
!           bisect the subinterval with the nrmax-th largest
!           error estimate.
!
            levcur = Level(maxerr) + 1
            a1 = Alist(maxerr)
            b1 = 0.5E+00*(Alist(maxerr)+Blist(maxerr))
            a2 = b1
            b2 = Blist(maxerr)
            erlast = errmax
            CALL QK21(F,a1,b1,area1,error1,resa,defab1,Ier)
            IF ( Ier<0 ) RETURN
            CALL QK21(F,a2,b2,area2,error2,resa,defab2,Ier)
            IF ( Ier<0 ) RETURN
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
            Neval = Neval + 42
            area12 = area1 + area2
            erro12 = error1 + error2
            errsum = errsum + erro12 - errmax
            area = area + area12 - Rlist(maxerr)
            IF ( defab1/=error1 .AND. defab2/=error2 ) THEN
               IF ( ABS(Rlist(maxerr)-area12)<=0.1E-04*ABS(area12) .AND.&
                  & erro12>=0.99E+00*errmax ) THEN
                  IF ( extrap ) iroff2 = iroff2 + 1
                  IF ( .NOT.extrap ) iroff1 = iroff1 + 1
               ENDIF
               IF ( Last>10 .AND. erro12>errmax ) iroff3 = iroff3 + 1
            ENDIF
            Level(maxerr) = levcur
            Level(Last) = levcur
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
!           at a point of the integration range
!
            IF ( AMAX1(ABS(a1),ABS(b2))<=(0.1E+01+0.1E+03*epmach)       &
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
! ***jump out of do-loop
            IF ( errsum<=errbnd ) GOTO 150
! ***jump out of do-loop
            IF ( Ier/=0 ) GOTO 100
            IF ( .NOT.(noext) ) THEN
               erlarg = erlarg - erlast
               IF ( levcur+1<=levmax ) erlarg = erlarg + erro12
               IF ( .NOT.(extrap) ) THEN
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
                  IF ( Level(maxerr)+1<=levmax ) GOTO 50
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
! ***jump out of do-loop
                     IF ( Level(maxerr)+1<=levmax ) GOTO 50
                     nrmax = nrmax + 1
                  ENDDO
               ENDIF
!
!           perform extrapolation.
!
               numrl2 = numrl2 + 1
               rlist2(numrl2) = area
               IF ( numrl2>2 ) THEN
                  CALL QELG(numrl2,rlist2,reseps,abseps,res3la,nres)
                  ktmin = ktmin + 1
                  IF ( ktmin>5 .AND. Abserr<0.1E-02*errsum ) Ier = 5
                  IF ( abseps<Abserr ) THEN
                     ktmin = 0
                     Abserr = abseps
                     Result = reseps
                     correc = erlarg
                     ertest = AMAX1(Epsabs,Epsrel*ABS(reseps))
! ***jump out of do-loop
                     IF ( Abserr<ertest ) GOTO 100
                  ENDIF
!
!           prepare bisection of the smallest interval.
!
                  IF ( numrl2==1 ) noext = .TRUE.
                  IF ( Ier>=5 ) GOTO 100
               ENDIF
               maxerr = Iord(1)
               errmax = Elist(maxerr)
               nrmax = 1
               extrap = .FALSE.
               levmax = levmax + 1
               erlarg = errsum
            ENDIF
 50      ENDDO
!
!           set the final result.
!           ---------------------
!
!
 100     IF ( Abserr/=oflow ) THEN
            IF ( (Ier+ierro)/=0 ) THEN
               IF ( ierro==3 ) Abserr = Abserr + correc
               IF ( Ier==0 ) Ier = 3
               IF ( Result==0.0E+00 .OR. area==0.0E+00 ) THEN
                  IF ( Abserr>errsum ) GOTO 150
                  IF ( area==0.0E+00 ) GOTO 200
               ELSEIF ( Abserr/ABS(Result)>errsum/ABS(area) ) THEN
                  GOTO 150
               ENDIF
            ENDIF
!
!           test on divergence.
!
            IF ( ksgn/=(-1) .OR. AMAX1(ABS(Result),ABS(area))           &
               & >resabs*0.1E-01 ) THEN
               IF ( 0.1E-01>(Result/area) .OR. (Result/area)            &
                  & >0.1E+03 .OR. errsum>ABS(area) ) Ier = 6
            ENDIF
            GOTO 200
         ENDIF
!
!           compute global integral sum.
!
 150     Result = 0.0E+00
         DO k = 1 , Last
            Result = Result + Rlist(k)
         ENDDO
         Abserr = errsum
      ENDIF
 200  IF ( Ier>2 ) Ier = Ier - 1
      Result = Result*sign
99999 END
