module shapeFuncMod
contains


  ! ====================================================== !
  ! === shapeFunction :: Cartesian                     === !
  ! ====================================================== !
  function shapeF1st( rposit, iposit, dxInv )
    implicit none
    integer         , intent(in) :: iposit
    double precision, intent(in) :: rposit, dxInv
    double precision             :: delta
    double precision             :: shapeF1st(-2:2)

    delta         = rposit - iposit
    shapeF1st(-2) = 0.d0
    shapeF1st(-1) = 0.d0
    shapeF1st( 0) = 1.d0 - abs( delta )
    shapeF1st(+1) = 0.d0
    shapeF1st(+2) = 0.d0

    shapeF1st( int( sign( 1.d0, delta) ) ) = abs( delta )
    
    return
  end function shapeF1st


  ! ====================================================== !
  ! === shapeFunction :: spline-2nd                    === !
  ! ====================================================== !
  function shapeF2nd( rposit, iposit, dxInv )
    implicit none
    integer         , intent(in) :: iposit
    double precision, intent(in) :: rposit, dxInv
    double precision             :: delta
    double precision             :: shapeF2nd(-2:2)

    delta         = rposit - iposit
    shapeF2nd(-2) = 0.d0
    shapeF2nd(-1) = 0.50d0* ( 0.50d0-delta )**2
    shapeF2nd( 0) = 0.75d0 - delta**2
    shapeF2nd(+1) = 0.50d0* ( 0.50d0+delta )**2
    shapeF2nd(+2) = 0.d0
    return
  end function shapeF2nd
  

end module shapeFuncMod
