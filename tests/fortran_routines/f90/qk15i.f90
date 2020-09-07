!*==QK15I.spg  processed by SPAG 6.72Dc at 12:06 on 30 Aug 2020
      SUBROUTINE QK15I(F,Boun,Inf,A,B,Result,Abserr,Resabs,Resasc)
      IMPLICIT NONE
!*--QK15I4
!*** Start of declarations inserted by SPAG
      INTEGER Ierr
!*** End of declarations inserted by SPAG
!***begin prologue  qk15i
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a2,h2a4a2
!***keywords  15-point transformed gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the original (infinite integration range is mapped
!            onto the interval (0,1) and (a,b) is a part of (0,1).
!            it is the purpose to compute
!            i = integral of transformed integrand over (a,b),
!            j = integral of abs(transformed integrand) over (a,b).
!***description
!
!           integration rule
!           standard fortran subroutine
!           real version
!
!           parameters
!            on entry
!              f      - real
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              boun   - real
!                       finite bound of original integration
!                       range (set to zero if inf = +2)
!
!              inf    - integer
!                       if inf = -1, the original interval is
!                                   (-infinity,bound),
!                       if inf = +1, the original interval is
!                                   (bound,+infinity),
!                       if inf = +2, the original interval is
!                                   (-infinity,+infinity) and
!                       the integral is computed as the sum of two
!                       integrals, one over (-infinity,0) and one over
!                       (0,+infinity).
!
!              a      - real
!                       lower limit for integration over subrange
!                       of (0,1)
!
!              b      - real
!                       upper limit for integration over subrange
!                       of (0,1)
!
!            on return
!              result - real
!                       approximation to the integral i
!                       result is computed by applying the 15-point
!                       kronrod rule(resk) obtained by optimal addition
!                       of abscissae to the 7-point gauss rule(resg).
!
!              abserr - real
!                       estimate of the modulus of the absolute error,
!                       which should equal or exceed abs(i-result)
!
!              resabs - real
!                       approximation to the integral j
!
!              resasc - real
!                       approximation to the integral of
!                       abs((transformed integrand)-i/(b-a)) over (a,b)
!
!***references  (none)
!***routines called  r1mach
!***end prologue  qk15i
!
      REAL A , absc , absc1 , absc2 , Abserr , B , Boun , centr , dinf ,&
         & R1MACH , epmach , F , fc , fsum , fval1 , fval2 , fvalt ,    &
         & fv1 , fv2 , hlgth , Resabs , Resasc , resg , resk , reskh ,  &
         & Result , tabsc1 , tabsc2 , uflow , wg , wgk , xgk
      INTEGER Inf , j , MIN0
      EXTERNAL F
!
      DIMENSION fv1(7) , fv2(7) , xgk(8) , wgk(8) , wg(8)
!
!           the abscissae and weights are supplied for the interval
!           (-1,1).  because of symmetry only the positive abscissae and
!           their corresponding weights are given.
!
!           xgk    - abscissae of the 15-point kronrod rule
!                    xgk(2), xgk(4), ... abscissae of the 7-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 7-point gauss rule
!
!           wgk    - weights of the 15-point kronrod rule
!
!           wg     - weights of the 7-point gauss rule, corresponding
!                    to the abscissae xgk(2), xgk(4), ...
!                    wg(1), wg(3), ... are set to zero.
!
      DATA xgk(1) , xgk(2) , xgk(3) , xgk(4) , xgk(5) , xgk(6) , xgk(7) &
         & , xgk(8)/0.9914553711208126E+00 , 0.9491079123427585E+00 ,   &
         & 0.8648644233597691E+00 , 0.7415311855993944E+00 ,            &
         & 0.5860872354676911E+00 , 0.4058451513773972E+00 ,            &
         & 0.2077849550078985E+00 , 0.0000000000000000E+00/
!
      DATA wgk(1) , wgk(2) , wgk(3) , wgk(4) , wgk(5) , wgk(6) , wgk(7) &
         & , wgk(8)/0.2293532201052922E-01 , 0.6309209262997855E-01 ,   &
         & 0.1047900103222502E+00 , 0.1406532597155259E+00 ,            &
         & 0.1690047266392679E+00 , 0.1903505780647854E+00 ,            &
         & 0.2044329400752989E+00 , 0.2094821410847278E+00/
!
      DATA wg(1) , wg(2) , wg(3) , wg(4) , wg(5) , wg(6) , wg(7) ,      &
         & wg(8)/0.0000000000000000E+00 , 0.1294849661688697E+00 ,      &
         & 0.0000000000000000E+00 , 0.2797053914892767E+00 ,            &
         & 0.0000000000000000E+00 , 0.3818300505051189E+00 ,            &
         & 0.0000000000000000E+00 , 0.4179591836734694E+00/
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc*  - abscissa
!           tabsc* - transformed abscissa
!           fval*  - function value
!           resg   - result of the 7-point gauss formula
!           resk   - result of the 15-point kronrod formula
!           reskh  - approximation to the mean value of the transformed
!                    integrand over (a,b), i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  qk15i
      epmach = R1MACH(4)
      uflow = R1MACH(1)
      dinf = MIN0(1,Inf)
!
      centr = 0.5E+00*(A+B)
      hlgth = 0.5E+00*(B-A)
      tabsc1 = Boun + dinf*(0.1E+01-centr)/centr
      CALL F(tabsc1,fval1)
      IF ( Inf==2 ) THEN
         CALL F(-tabsc1,fvalt)
         fval1 = fval1 + fvalt
      ENDIF
      fc = (fval1/centr)/centr
!
!           compute the 15-point kronrod approximation to
!           the integral, and estimate the error.
!
      resg = wg(8)*fc
      resk = wgk(8)*fc
      Resabs = ABS(resk)
      DO j = 1 , 7
         absc = hlgth*xgk(j)
         absc1 = centr - absc
         absc2 = centr + absc
         tabsc1 = Boun + dinf*(0.1E+01-absc1)/absc1
         tabsc2 = Boun + dinf*(0.1E+01-absc2)/absc2
         CALL F(tabsc1,fval1)
         CALL F(tabsc2,fval2)
         IF ( Inf==2 ) THEN
            CALL F(-tabsc1,fvalt)
            fval1 = fval1 + fvalt
         ENDIF
         IF ( Inf==2 ) THEN
            CALL F(-tabsc2,fvalt)
            fval2 = fval2 + fvalt
         ENDIF
         fval1 = (fval1/absc1)/absc1
         fval2 = (fval2/absc2)/absc2
         fv1(j) = fval1
         fv2(j) = fval2
         fsum = fval1 + fval2
         resg = resg + wg(j)*fsum
         resk = resk + wgk(j)*fsum
         Resabs = Resabs + wgk(j)*(ABS(fval1)+ABS(fval2))
      ENDDO
      reskh = resk*0.5E+00
      Resasc = wgk(8)*ABS(fc-reskh)
      DO j = 1 , 7
         Resasc = Resasc + wgk(j)*(ABS(fv1(j)-reskh)+ABS(fv2(j)-reskh))
      ENDDO
      Result = resk*hlgth
      Resasc = Resasc*hlgth
      Resabs = Resabs*hlgth
      Abserr = ABS((resk-resg)*hlgth)
      IF ( Resasc/=0.0E+00 .AND. Abserr/=0.E0 )                         &
         & Abserr = Resasc*AMIN1(0.1E+01,(0.2E+03*Abserr/Resasc)        &
         & **1.5E+00)
      IF ( Resabs>uflow/(0.5E+02*epmach) )                              &
         & Abserr = AMAX1((epmach*0.5E+02)*Resabs,Abserr)
      END
