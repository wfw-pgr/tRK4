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
       kx1 = rhs_xp(                           pxv(vx_:vz_,ipt)         )
       kv1 = rhs_vp( pxv(xp_:zp_,ipt)        , pxv(vx_:vz_,ipt)         )

       ! -- [1-3] 2nd step           -- !
       kx2 = rhs_xp(                           pxv(vx_:vz_,ipt)+hdt*kv1 )
       kv2 = rhs_vp( pxv(xp_:zp_,ipt)+hdt*kx1, pxv(vx_:vz_,ipt)+hdt*kv1 )

       ! -- [1-4] 3rd step           -- !
       kx3 = rhs_xp(                           pxv(vx_:vz_,ipt)+hdt*kv2 )
       kv3 = rhs_vp( pxv(xp_:zp_,ipt)+hdt*kx2, pxv(vx_:vz_,ipt)+hdt*kv2 )

       ! -- [1-5] 4th step           -- !
       kx4 = rhs_xp(                           pxv(vx_:vz_,ipt)+ dt*kv3 )
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
  function rhs_xp( vp )
    use variablesMod
    implicit none
    double precision, intent(in) :: vp(3)
    double precision             :: rhs_xp(3)
    double precision             :: gammaInv
    integer         , parameter  :: vxh_=1, vyh_=2, vzh_=3
    
    gammaInv     = 1.d0 / sqrt( 1.d0 + ( vp(vxh_)**2 + vp(vyh_)**2 + vp(vzh_)**2 )*cvSqInv )
    rhs_xp(vxh_) = vp(vxh_) * gammaInv
    rhs_xp(vyh_) = vp(vyh_) * gammaInv
    rhs_xp(vzh_) = vp(vzh_) * gammaInv
    return
  end function rhs_xp
    

  ! ====================================================== !
  ! === Left-Hand-Side to be integrated (vp)           === !
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
    integer         , parameter  :: vxh_=1, vyh_=2, vzh_=3
    
    ! ------------------------------------------------------ !
    ! --- [1] interpolate EB-Field                       --- !
    ! ------------------------------------------------------ !
    rposit(xp_)  = ( xp(xp_) - xMin ) * dxInv
    rposit(yp_)  = ( xp(yp_) - yMin ) * dyInv
    rposit(zp_)  = ( xp(zp_) - zMin ) * dzInv
    ip           = max( min( nint( rposit(xp_) ), LI-1 ), 0 )
    jp           = max( min( nint( rposit(yp_) ), LJ-1 ), 0 )
    kp           = max( min( nint( rposit(zp_) ), LK-1 ), 0 )
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
    gammaInv     = sqrt( 1.d0 * ( vp(vxh_)**2 + vp(vyh_)**2 + vp(vzh_)**2 )*cvSqInv )
    rhs_vp(vxh_) = qm * ( EBp(ex_) + gammaInv*( vp(vyh_)*EBp(bz_) - vp(vzh_)*EBp(by_) ) )
    rhs_vp(vyh_) = qm * ( EBp(ey_) + gammaInv*( vp(vzh_)*EBp(bx_) - vp(vxh_)*EBp(bz_) ) )
    rhs_vp(vzh_) = qm * ( EBp(ez_) + gammaInv*( vp(vxh_)*EBp(by_) - vp(vyh_)*EBp(bx_) ) )

    return
  end function rhs_vp


  ! ====================================================== !
  ! === convert into relativistic velocity             === !
  ! ====================================================== !
  subroutine into__relativistic
    use variablesMod
    implicit none
    integer                     :: ipt
    double precision            :: gamma

    ! ------------------------------------------------------ !
    ! --- [0] check velocity flag                        --- !
    ! ------------------------------------------------------ !
    if ( flag__relativisticVelocity ) then 
       write(6,*) "[into__relativistic] pxv(vx_:vz_) is already relativistic [ERROR]"
       stop
    else
       ! -- ok! nothing to do -- !
    endif
    ! ------------------------------------------------------ !
    ! --- [1] convert velocity into relativistic         --- !
    ! ------------------------------------------------------ !
    do ipt=1, npt
       gamma        = 1.d0 / sqrt( 1.d0 - &
            & cvSqInv*( pxv(vx_,ipt)**2 + pxv(vy_,ipt)**2 + pxv(vz_,ipt)**2 ) )
       pxv(vx_,ipt) = gamma * pxv(vx_,ipt)
       pxv(vy_,ipt) = gamma * pxv(vy_,ipt)
       pxv(vz_,ipt) = gamma * pxv(vz_,ipt)
    enddo
    ! ------------------------------------------------------ !
    ! --- [2] switch relativistic flag                   --- !
    ! ------------------------------------------------------ !
    flag__relativisticVelocity = .true.
    
    return
  end subroutine into__relativistic


  ! ====================================================== !
  ! === convert into non relativistic velocity         === !
  ! ====================================================== !
  subroutine into__non_relativistic
    use variablesMod
    implicit none
    integer                     :: ipt
    double precision            :: gamma

    ! ------------------------------------------------------ !
    ! --- [0] check velocity flag                        --- !
    ! ------------------------------------------------------ !
    if ( flag__relativisticVelocity ) then
       ! -- ok! nothing to do -- !
    else
       write(6,*) "[into__non_relativistic] pxv(vx_:vz_) is already non relativistic [ERROR]"
       stop
    endif
    ! ------------------------------------------------------ !
    ! --- [1] convert velocity into relativistic         --- !
    ! ------------------------------------------------------ !
    do ipt=1, npt
       gamma = sqrt( 1.d0 + cvSqInv*( pxv(vx_,ipt)**2 + pxv(vy_,ipt)**2 + pxv(vz_,ipt)**2 ) )
       pxv(vx_,ipt) = pxv(vx_,ipt) / gamma
       pxv(vy_,ipt) = pxv(vy_,ipt) / gamma
       pxv(vz_,ipt) = pxv(vz_,ipt) / gamma
    enddo
    ! ------------------------------------------------------ !
    ! --- [2] switch relativistic flag                   --- !
    ! ------------------------------------------------------ !
    flag__relativisticVelocity = .false.
    
    return
  end subroutine into__non_relativistic
  

  ! ====================================================== !
  ! === into__rtz_coordinate                           === !
  ! ====================================================== !
  subroutine into__rtz_coordinate
    use variablesMod
    implicit none
    integer            :: ipt
    double precision   :: rpos, rInv, vxh, vyh, costh, sinth
    
    do ipt=1, npt
       
       rpos     = sqrt( pxv(xp_,ipt)**2 + pxv(yp_,ipt)**2 )
       rInv     = 1.d0 / rpos
       if ( rpos.eq.0.d0 ) then
          costh = 1.d0
          sinth = 0.d0
       else
          costh = pxv(xp_,ipt) * rInv
          sinth = pxv(yp_,ipt) * rInv
       end if
       vxh          = pxv(vx_,ipt)
       vyh          = pxv(vy_,ipt)
       pxv(xp_,ipt) =   rpos
       pxv(yp_,ipt) =   0.d0
       pxv(vx_,ipt) =   costh*vxh + sinth*vyh
       pxv(vy_,ipt) = - sinth*vxh + costh*vyh
    enddo
    
    return
  end subroutine into__rtz_coordinate
  
  
end module rkgSolverMod



! ! -- (6) Get psued Coordinate (xyz)     -- !
! vnrm2        = pxv(vr_,m,k)**2 + pxv(vt_,m,k)**2 + pxv(vz_,m,k)**2
! chk          = chk  +  max( 0.d0,  ( vnrm2 - cSpeedLimit ) )
! gamma        = dt   / sqrt( 1.d0 +   vnrm2 )
! psuedxy(1)   = pxv(rp_,m,k) + gamma*pxv(vr_,m,k) + rMin
! psuedxy(2)   =              + gamma*pxv(vt_,m,k)
! psuedxy(3)   = sqrt( psuedxy(1)**2 + psuedxy(2)**2 )

! ! -- (7) Copy to Old Position           -- !
! pxv(ro_,m,k) = pxv(rp_,m,k)
! pxv(zo_,m,k) = pxv(zp_,m,k)

! ! -- (8) Reflection :: boundary condition for RMax, RMin
! if ( psuedxy(3).gt.rRefl2 ) call ReflecInXY( psuedxy, pxv(1:8,m,k), rRefl2, +1.d0, rMin, gamma, dr, dz )
! ! if ( psuedxy(3) .lt. rRefl1 ) call ReflecInXY( psuedxy, pxv(:,m,k), rRefl1, -1.d0, rMin, gamma, dr, dz )
! pxv(rp_,m,k) = psuedxy(3) - rMin
! pxv(zp_,m,k) = pxv(zp_,m,k) + gamma*pxv(vz_,m,k)

! ! -- (8) calculate cos(a) & sin(a)      -- !
! psuedxy(3)   = 1.d0  / psuedxy(3)
! if ( pxv(rp_,m,k).ne.0.d0 ) then
!    costh     = psuedxy(1) * psuedxy(3)
!    sinth     = psuedxy(2) * psuedxy(3)
! else
!    costh     = 1.d0
!    sinth     = 0.d0
! endif

! ! -- (9) rotate velocity                -- !
! vtmp(1)      = pxv(vr_,m,k)
! vtmp(2)      = pxv(vt_,m,k)
! pxv(vr_,m,k) =   costh * vtmp(1) + sinth * vtmp(2)
! pxv(vt_,m,k) = - sinth * vtmp(1) + costh * vtmp(2)
! ! pxv(vt_,m,k) = vtmp(2) * ( pxv(ro_,m,k)+rMin ) * psuedxy(3) ! angular-momentum conservation

