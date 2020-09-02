!*==QK21.spg  processed by SPAG 6.72Dc at 08:15 on  2 Sep 2020
      SUBROUTINE QK21(F,A,B,Result,Abserr,Resabs,Resasc,Ierr)
      IMPLICIT NONE
!*--QK214
!*** Start of declarations inserted by SPAG
      INTEGER Ierr
      REAL Resasc
!*** End of declarations inserted by SPAG
!***begin prologue  qk21
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  21-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real version
!
!           parameters
!            on entry
!              f      - subroutine f(x,ierr,result) defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the driver program.
!
!              a      - real
!                       lower limit of integration
!
!              b      - real
!                       upper limit of integration
!
!            on return
!              result - real
!                       approximation to the integral i
!                       result is computed by applying the 21-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 10-point gauss rule (resg).
!
!              abserr - real
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result)
!
!              resabs - real
!                       approximation to the integral j
!
!              resasc - real
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***routines called  r1mach
!***end prologue  qk21
!
      REAL A , absc , Abserr , B , centr , dhlgth , epmach , fc , fsum ,&
         & fval1 , fval2 , fv1 , fv2 , hlgth , Resabs , resg , resk ,   &
         & reskh , Result , R1MACH , uflow , wg , wgk , xgk
      INTEGER j , jtw , jtwm1
      EXTERNAL F
!
      DIMENSION fv1(10) , fv2(10) , wg(5) , wgk(11) , xgk(11)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 21-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 10-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 10-point gauss rule
!
!           wgk    - weights of the 21-point kronrod rule
!
!           wg     - weights of the 10-point gauss rule
!
      DATA xgk(1) , xgk(2) , xgk(3) , xgk(4) , xgk(5) , xgk(6) , xgk(7) &
         & , xgk(8) , xgk(9) , xgk(10) , xgk(11)                        &
         & /0.9956571630258081E+00 , 0.9739065285171717E+00 ,           &
         & 0.9301574913557082E+00 , 0.8650633666889845E+00 ,            &
         & 0.7808177265864169E+00 , 0.6794095682990244E+00 ,            &
         & 0.5627571346686047E+00 , 0.4333953941292472E+00 ,            &
         & 0.2943928627014602E+00 , 0.1488743389816312E+00 ,            &
         & 0.0000000000000000E+00/
!
      DATA wgk(1) , wgk(2) , wgk(3) , wgk(4) , wgk(5) , wgk(6) , wgk(7) &
         & , wgk(8) , wgk(9) , wgk(10) , wgk(11)                        &
         & /0.1169463886737187E-01 , 0.3255816230796473E-01 ,           &
         & 0.5475589657435200E-01 , 0.7503967481091995E-01 ,            &
         & 0.9312545458369761E-01 , 0.1093871588022976E+00 ,            &
         & 0.1234919762620659E+00 , 0.1347092173114733E+00 ,            &
         & 0.1427759385770601E+00 , 0.1477391049013385E+00 ,            &
         & 0.1494455540029169E+00/
!
      DATA wg(1) , wg(2) , wg(3) , wg(4) ,                              &
         & wg(5)/0.6667134430868814E-01 , 0.1494513491505806E+00 ,      &
         & 0.2190863625159820E+00 , 0.2692667193099964E+00 ,            &
         & 0.2955242247147529E+00/
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result of the 10-point gauss formula
!           resk   - result of the 21-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  qk21
      epmach = R1MACH(4)
      uflow = R1MACH(1)
!
      centr = 0.5E+00*(A+B)
      hlgth = 0.5E+00*(B-A)
      dhlgth = ABS(hlgth)
!
!           compute the 21-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      resg = 0.0E+00
      CALL F(centr,Ierr,fc)
      IF ( Ierr<0 ) RETURN
      resk = wgk(11)*fc
      Resabs = ABS(resk)
      DO j = 1 , 5
         jtw = 2*j
         absc = hlgth*xgk(jtw)
         CALL F(centr-absc,Ierr,fval1)
         IF ( Ierr<0 ) RETURN
         CALL F(centr+absc,Ierr,fval2)
         IF ( Ierr<0 ) RETURN
         fv1(jtw) = fval1
         fv2(jtw) = fval2
         fsum = fval1 + fval2
         resg = resg + wg(j)*fsum
         resk = resk + wgk(jtw)*fsum
         Resabs = Resabs + wgk(jtw)*(ABS(fval1)+ABS(fval2))
      ENDDO
      DO j = 1 , 5
         jtwm1 = 2*j - 1
         absc = hlgth*xgk(jtwm1)
         CALL F(centr-absc,Ierr,fval1)
         IF ( Ierr<0 ) RETURN
         CALL F(centr+absc,Ierr,fval2)
         IF ( Ierr<0 ) RETURN
         fv1(jtwm1) = fval1
         fv2(jtwm1) = fval2
         fsum = fval1 + fval2
         resk = resk + wgk(jtwm1)*fsum
         Resabs = Resabs + wgk(jtwm1)*(ABS(fval1)+ABS(fval2))
      ENDDO
      reskh = resk*0.5E+00
      Resasc = wgk(11)*ABS(fc-reskh)
      DO j = 1 , 10
         Resasc = Resasc + wgk(j)*(ABS(fv1(j)-reskh)+ABS(fv2(j)-reskh))
      ENDDO
      Result = resk*hlgth
      Resabs = Resabs*dhlgth
      Resasc = Resasc*dhlgth
      Abserr = ABS((resk-resg)*hlgth)
      IF ( Resasc/=0.0E+00 .AND. Abserr/=0.0E+00 )                      &
         & Abserr = Resasc*AMIN1(0.1E+01,(0.2E+03*Abserr/Resasc)        &
         & **1.5E+00)
      IF ( Resabs>uflow/(0.5E+02*epmach) )                              &
         & Abserr = AMAX1((epmach*0.5E+02)*Resabs,Abserr)
      END
