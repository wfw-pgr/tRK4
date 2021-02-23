module diagnosisMod
contains


  ! ====================================================== !
  ! === probe__eulerField                              === !
  ! ====================================================== !
  subroutine probe__eulerField( action )
    use variablesMod
    use shapeFuncMod
    implicit none
    character(5), intent(in) :: action
    integer                  :: ipt, i, j, k, ip, jp, kp, ibuff
    character(6)             :: cpt
    character(cLen)          :: probeFile
    double precision         :: gammaInv
    double precision         :: rposit(3), sfx(-2:2), sfy(-2:2), sfz(-2:2), EBp(6)
    integer     , save       :: store_count

    ! ------------------------------------------------------ !
    ! --- [1] initialize probe files                     --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."initi" ) then
       write(6,"(a)",advance="no") "[probe__eulerField] action :: Initialize probeFiles... "
       do ipt=1, npt
          write(cpt,"(i6.6)") ipt
          probeFile = trim( probeFileBase ) // cpt // ".dat"
          open (lun,file=trim(probeFile),status="replace",form="formatted")
          write(lun,"(a)") "# ptime xp yp zp vx vy vz ex ey ez bx by bz wt"
          close(lun)
       enddo
       !  -- [1-2] buff initialization                   --  !
       allocate( probeBuff(14,npt,buffLength), probe_counter(npt) )
       probeBuff(:,:,:) = 0.d0
       probe_counter(:) = 0
       store_count      = 0
       !  -- [1-3] time schedule settings                --  !
       t_nextProbe      = t_probeStart
       write(6,"(a)",advance="yes") "[Done]"
    endif
    
    ! ------------------------------------------------------ !
    ! --- [2] interpolation EB-Field  & save it          --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."store" ) then

       store_count = store_count + 1

       do ipt=1, npt
          
          !  -- [2-1] not moving particle                   --  !
          if ( pxv(wt_,ipt).eq.0.d0 ) cycle
          
          !  -- [2-2] interpolation of field                --  !
          if   ( ( ( pxv(xp_,ipt).ge.xMin ).and.( pxv(xp_,ipt).le.xMax ) ).and. &
               & ( ( pxv(yp_,ipt).ge.yMin ).and.( pxv(yp_,ipt).le.yMax ) ).and. &
               & ( ( pxv(zp_,ipt).ge.zMin ).and.( pxv(zp_,ipt).le.zMax ) ) ) then
             
             rposit(xp_)  = ( pxv(xp_,ipt) - xMin ) * dxInv
             rposit(yp_)  = ( pxv(yp_,ipt) - yMin ) * dyInv
             rposit(zp_)  = ( pxv(zp_,ipt) - zMin ) * dzInv
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
          else
             EBp(:) = 0.d0
          endif
          
          gammaInv  = 1.d0 / sqrt( 1.d0 + &
               & ( pxv(vx_,ipt)**2 + pxv(vy_,ipt)**2 + pxv(vz_,ipt)**2 )*cvSqInv )
          probeBuff(   1,ipt,store_count) = ptime
          probeBuff( 2:4,ipt,store_count) = pxv(xp_:zp_,ipt)
          probeBuff( 5:7,ipt,store_count) = pxv(vx_:vz_,ipt) * gammaInv
          probeBuff(8:13,ipt,store_count) = EBp(ex_:bz_)
          probeBuff(  14,ipt,store_count) = pxv(    wt_,ipt)
          probe_counter(ipt)              = probe_counter(ipt) + 1
          
       enddo
       
       ! -- cyclic coordinate output -- !
       if ( flag__cyclicCoordinate ) then
          ! -- cyclic particle coordinate [xMin,xMax] -- !
          ! -- nothing to do -- !
       else 
          if ( trim(particleBoundary__x).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   probeBuff(xp_+1,ipt,store_count) = probeBuff(xp_+1,ipt,store_count) &
                        &                             + dble( period_counter(ipt) )*xLeng
                endif
             enddo
          endif
          if ( trim(particleBoundary__y).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   probeBuff(yp_+1,ipt,store_count) = probeBuff(yp_+1,ipt,store_count) &
                        &                             + dble( period_counter(ipt) )*yLeng
                endif
             enddo
          endif
          if ( trim(particleBoundary__z).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   probeBuff(zp_+1,ipt,store_count) = probeBuff(zp_+1,ipt,store_count) &
                        &                             + dble( period_counter(ipt) )*zLeng
                endif
             enddo
          endif
       endif
       ! ------------------------------ !
    endif

    ! ------------------------------------------------------ !
    ! --- [3] save (buff is full)                        --- !
    ! ------------------------------------------------------ !
    if ( ( store_count.eq.buffLength ).or.( trim(action).eq."final" ) ) then
       !  -- [3-1] save buff contents                    --  !
       do ipt=1, npt
          if ( probe_counter(ipt).gt.0 ) then
             write(cpt,"(i6.6)") ipt
             probeFile = trim( probeFileBase ) // cpt // ".dat"
             open( lun,file=trim(probeFile),form="formatted",position="append" )
             do ibuff=1, probe_counter(ipt)
                write(lun,"(14(e15.8,1x))") probeBuff(1:14,ipt,ibuff)
             enddo
             close(lun)
          endif
       enddo
       !  -- [3-2] buff counter reset                    --  !
       store_count       = 0
       probe_counter(:)  = 0
    end if
    
    ! ------------------------------------------------------ !
    ! --- [4] update next schedule                       --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."store" ) then
       if ( ptime.gt.t_probeEnd ) then
          t_nextProbe = t_nextProbe + 2.0d0*( t_simuEnd - t_simuStart )
       else
          t_nextProbe = t_nextProbe + t_probeStep
       endif
    endif
    
    return
  end subroutine probe__eulerField


end module diagnosisMod
