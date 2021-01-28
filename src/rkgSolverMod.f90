module rkgSolverMod
contains

  ! ====================================================== !
  ! === Runge-Kutta-4th solver (xp,vp)                 === !
  ! ====================================================== !

  subroutine RK4__tracker
    use variablesMod
    implicit none
    integer                     :: ipt
    double precision            :: hdt, sdt
    double precision            :: kx1(3), kx2(3), kx3(3), kx4(3)
    double precision            :: kv1(3), kv2(3), kv3(3), kv4(3)
    double precision, parameter :: onesixth = 1.d0 / 6.d0
    
    ! ------------------------------------------------------ !
    ! --- [1] Runge-Kutta 4th                            --- !
    ! ------------------------------------------------------ !

    hdt =    0.5d0 * dt
    sdt = onesixth * dt
    do ipt=1, npt

       if ( pxv(wt_,ipt).eq.0.d0 ) cycle
       ! -- [1-1] copy to old region -- !
       pxv(xo_:uz_,ipt) = pxv(xp_:vz_,ipt)

       ! -- [1-2] 1st step           -- !
       kx1 = pxv(vx_:vz_,ipt)
       kv1 = rhs_vp( pxv(xp_:zp_,ipt), pxv(vx_:vz_,ipt) )

       ! -- [1-3] 2nd step           -- !
       kx2 = pxv(vx_:vz_,ipt) + hdt*kv1
       kv2 = rhs_vp( pxv(xp_:zp_,ipt)+hdt*kx1, pxv(vx_:vz_,ipt)+hdt*kv1 )

       ! -- [1-4] 3rd step           -- !
       kx3 = pxv(vx_:vz_,ipt) + hdt*kv2
       kv3 = rhs_vp( pxv(xp_:zp_,ipt)+hdt*kx2, pxv(vx_:vz_,ipt)+hdt*kv2 )

       ! -- [1-5] 4th step           -- !
       kx4 = pxv(vx_:vz_,ipt) + dt*kv3
       kv4 = rhs_vp( pxv(xp_:zp_,ipt)+ dt*kx3, pxv(vx_:vz_,ipt)+ dt*kv3 )

       ! -- [1-6] actual step        -- !
       pxv(xp_:zp_,ipt) = pxv(xp_:zp_,ipt) + sdt * ( kx1 + 2.d0*kx2 + 2.d0*kx3 + kx4 )
       pxv(vx_:vz_,ipt) = pxv(vx_:vz_,ipt) + sdt * ( kv1 + 2.d0*kv2 + 2.d0*kv3 + kv4 )
       
    enddo

    return
  end subroutine RK4__tracker
    


  ! ====================================================== !
  ! === Left-Hand-Side to be integrated (xp)           === !
  ! ====================================================== !
  function rhs_vp( xp, vp )
    use variablesMod
    use shapeFuncMod
    implicit none
    double precision, intent(in) :: xp(3), vp(3)
    double precision             :: rhs_vp(3)
    integer                      :: i, j, k, ip, jp, kp
    double precision             :: gammaInv
    double precision             :: rposit(3), EBp(6), sfx(-2:2), sfy(-2:2), sfz(-2:2)
    double precision, parameter  :: cvSqInv = 1.d0 / cv**2
    integer         , parameter  :: vxh_=1, vyh_=2, vzh_=3
    
    ! ------------------------------------------------------ !
    ! --- [1] interpolate EB-Field                       --- !
    ! ------------------------------------------------------ !
    rposit(xp_)  = ( xp(xp_) - xMin ) * dxInv
    rposit(yp_)  = ( xp(yp_) - yMin ) * dyInv
    rposit(zp_)  = ( xp(zp_) - zMin ) * dzInv
    ip           = max( min( nint( rposit(xp_) ), LI ), 0 )
    jp           = max( min( nint( rposit(yp_) ), LJ ), 0 )
    kp           = max( min( nint( rposit(zp_) ), LK ), 0 )
    sfx          = shapeF1st( rposit(xp_), ip, dxInv )
    sfy          = shapeF1st( rposit(yp_), jp, dyInv )
    sfz          = shapeF1st( rposit(zp_), kp, dzInv )
    ip           = ip + 1
    jp           = jp + 1
    kp           = kp + 1
    EBp(:)       = 0.d0
    do k=-2, 2
       do j=-2, 2
          do i=-2, 2
             EBp(:) = EBp(:) + sfx(i)*sfy(j)*sfz(k) * EBf(:,ip+i,jp+j,kp+k)
          enddo
       enddo
    enddo
    ! ------------------------------------------------------ !
    ! --- [2] calculate L.H.S.                           --- !
    ! ------------------------------------------------------ !
    gammaInv     = sqrt( 1.d0 - ( vp(vxh_)**2 + vp(vyh_)**2 + vp(vzh_)**2 )*cvSqInv )
    rhs_vp(vxh_) = ( EBp(ex_) + ( vp(vyh_)*EBp(bz_) - vp(vzh_)*EBp(by_) ) ) * qm * gammaInv
    rhs_vp(vyh_) = ( EBp(ey_) + ( vp(vzh_)*EBp(bx_) - vp(vxh_)*EBp(bz_) ) ) * qm * gammaInv
    rhs_vp(vzh_) = ( EBp(ez_) + ( vp(vxh_)*EBp(by_) - vp(vyh_)*EBp(bx_) ) ) * qm * gammaInv

    return
  end function rhs_vp


end module rkgSolverMod
