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
    !$omp parallel default(none) &
    !$omp shared(pxv,npt,hdt,sdt,dt) private(ipt,kx1,kv1,kx2,kv2,kx3,kv3,kx4,kv4)
    !$omp do
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
    !$omp end do
    !$omp end parallel
    
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
    implicit none
    double precision, intent(in) :: xp(3), vp(3)
    double precision             :: rhs_vp(3)
    double precision             :: gammaInv, EBp(6)
    integer         , parameter  :: vxh_=1, vyh_=2, vzh_=3
    
    ! ------------------------------------------------------ !
    ! --- [1] interpolate EB-Field                       --- !
    ! ------------------------------------------------------ !
    call interpolate__onParticle( xp, EBp )
    
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
  ! === interpolate on particle                        === !
  ! ====================================================== !
  subroutine interpolate__onParticle( xp, EBp )
    use variablesMod
    use shapeFuncMod
    implicit none
    double precision, intent(in)  :: xp(3)
    double precision, intent(out) :: EBp(6)
    integer                       :: i, j, k, ip, jp, kp, iF
    double precision              :: amplitude, rposit(3), xph(3)
    double precision              :: sfx(-2:2), sfy(-2:2), sfz(-2:2)

    ! ------------------------------------------------------ !
    ! --- [1] initialize EBp return variable             --- !
    ! ------------------------------------------------------ !
    EBp(1:6)  = 0.d0
    amplitude = 0.d0

    ! ------------------------------------------------------ !
    ! --- [2] EField interpolation                       --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
              
       if   ( ( ( xp(xp_).ge.efields(iF)%xMin ).and.( xp(xp_).le.efields(iF)%xMax ) ).and. &
            & ( ( xp(yp_).ge.efields(iF)%yMin ).and.( xp(yp_).le.efields(iF)%yMax ) ).and. &
            & ( ( xp(zp_).ge.efields(iF)%zMin ).and.( xp(zp_).le.efields(iF)%zMax ) ) ) then

          xph(xp_)     = xp(xp_) - efields(iF)%xLeng &
               &         * floor( ( xp(xp_) - efields(iF)%xMin ) * efields(iF)%xLengInv )
          xph(yp_)     = xp(yp_) - efields(iF)%yLeng &
               &         * floor( ( xp(yp_) - efields(iF)%yMin ) * efields(iF)%yLengInv )
          xph(zp_)     = xp(zp_) - efields(iF)%zLeng &
               &         * floor( ( xp(zp_) - efields(iF)%zMin ) * efields(iF)%zLengInv )
          rposit(xp_)  = ( xph(xp_) - efields(iF)%xMin ) * efields(iF)%dxInv
          rposit(yp_)  = ( xph(yp_) - efields(iF)%yMin ) * efields(iF)%dyInv
          rposit(zp_)  = ( xph(zp_) - efields(iF)%zMin ) * efields(iF)%dzInv
          ip           = max( min( nint( rposit(xp_) ), efields(iF)%LI-1 ), 0 )
          jp           = max( min( nint( rposit(yp_) ), efields(iF)%LJ-1 ), 0 )
          kp           = max( min( nint( rposit(zp_) ), efields(iF)%LK-1 ), 0 )
          sfx          = shapeF1st( rposit(xp_), ip, efields(iF)%dxInv )
          sfy          = shapeF1st( rposit(yp_), jp, efields(iF)%dyInv )
          sfz          = shapeF1st( rposit(zp_), kp, efields(iF)%dzInv )
          ip           = ip + 1
          jp           = jp + 1
          kp           = kp + 1
          amplitude    = efields(iF)%modulation * efields(iF)%amplitude_factor
          do k=-2, 2
             do j=-2, 2
                do i=-2, 2
                   EBp(ex_:ez_) = EBp(ex_:ez_) + sfx(i)*sfy(j)*sfz(k) * amplitude &
                        &                      * efields(iF)%EBf(fx_:fz_,ip+i,jp+j,kp+k)
                enddo
             enddo
          enddo
          
       endif
    enddo

    ! ------------------------------------------------------ !
    ! --- [3] BField interpolation                       --- !
    ! ------------------------------------------------------ !
    do iF=1, nBField
       
       if   ( ( ( xp(xp_).ge.bfields(iF)%xMin ).and.( xp(xp_).le.bfields(iF)%xMax ) ).and.&
            & ( ( xp(yp_).ge.bfields(iF)%yMin ).and.( xp(yp_).le.bfields(iF)%yMax ) ).and.&
            & ( ( xp(zp_).ge.bfields(iF)%zMin ).and.( xp(zp_).le.bfields(iF)%zMax ) ) ) then

          xph(xp_)     = xp(xp_) - bfields(iF)%xLeng &
               &         * floor( ( xp(xp_) - bfields(iF)%xMin ) * bfields(iF)%xLengInv )
          xph(yp_)     = xp(yp_) - bfields(iF)%yLeng &
               &         * floor( ( xp(yp_) - bfields(iF)%yMin ) * bfields(iF)%yLengInv ) 
          xph(zp_)     = xp(zp_) - bfields(iF)%zLeng &
               &         * floor( ( xp(zp_) - bfields(iF)%zMin ) * bfields(iF)%zLengInv )
          rposit(xp_)  = ( xph(xp_) - bfields(iF)%xMin ) * bfields(iF)%dxInv
          rposit(yp_)  = ( xph(yp_) - bfields(iF)%yMin ) * bfields(iF)%dyInv
          rposit(zp_)  = ( xph(zp_) - bfields(iF)%zMin ) * bfields(iF)%dzInv
          ip           = max( min( nint( rposit(xp_) ), bfields(iF)%LI-1 ), 0 )
          jp           = max( min( nint( rposit(yp_) ), bfields(iF)%LJ-1 ), 0 )
          kp           = max( min( nint( rposit(zp_) ), bfields(iF)%LK-1 ), 0 )
          sfx          = shapeF1st( rposit(xp_), ip, bfields(iF)%dxInv )
          sfy          = shapeF1st( rposit(yp_), jp, bfields(iF)%dyInv )
          sfz          = shapeF1st( rposit(zp_), kp, bfields(iF)%dzInv )
          ip           = ip + 1
          jp           = jp + 1
          kp           = kp + 1
          amplitude    = bfields(iF)%modulation * bfields(iF)%amplitude_factor
          do k=-2, 2
             do j=-2, 2
                do i=-2, 2
                   EBp(bx_:bz_) = EBp(bx_:bz_) + sfx(i)*sfy(j)*sfz(k) * amplitude &
                        &                      * bfields(iF)%EBf(fx_:fz_,ip+i,jp+j,kp+k)
                enddo
             enddo
          enddo

       endif
    enddo

    return
  end subroutine interpolate__onParticle


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



    ! if   ( ( ( xp(xp_).ge.xMin ).and.( xp(xp_).le.xMax ) ).and. &
    !      & ( ( xp(yp_).ge.yMin ).and.( xp(yp_).le.yMax ) ).and. &
    !      & ( ( xp(zp_).ge.zMin ).and.( xp(zp_).le.zMax ) ) ) then
       
    !    rposit(xp_)  = ( xp(xp_) - xMin ) * dxInv
    !    rposit(yp_)  = ( xp(yp_) - yMin ) * dyInv
    !    rposit(zp_)  = ( xp(zp_) - zMin ) * dzInv
    !    ip           = max( min( nint( rposit(xp_) ), LI-1 ), 0 )
    !    jp           = max( min( nint( rposit(yp_) ), LJ-1 ), 0 )
    !    kp           = max( min( nint( rposit(zp_) ), LK-1 ), 0 )
    !    sfx          = shapeF1st( rposit(xp_), ip, dxInv )
    !    sfy          = shapeF1st( rposit(yp_), jp, dyInv )
    !    sfz          = shapeF1st( rposit(zp_), kp, dzInv )
    !    ip           = ip + 1
    !    jp           = jp + 1
    !    kp           = kp + 1
    !    EBp(:)       = 0.d0
    !    do k=-2, 2
    !       do j=-2, 2
    !          do i=-2, 2
    !             EBp(:) = EBp(:) + sfx(i)*sfy(j)*sfz(k) * EBf(:,ip+i,jp+j,kp+k)
    !          enddo
    !       enddo
    !    enddo
    ! else
    !    EBp(:) = 0.d0
    ! endif
    
