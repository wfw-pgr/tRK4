module pBoundaryMod
contains

  
  ! ====================================================== !
  ! === particle boundary condition                    === !
  ! ====================================================== !
  subroutine particle__Boundary
    use variablesMod
    implicit none

    ! ------------------------------------------------------ !
    ! --- [1] particle Boundary                          --- !
    ! ------------------------------------------------------ !
    if (   ( trim(particleBoundary__x).eq."periodic" ).or.&
         & ( trim(particleBoundary__y).eq."periodic" ).or.&
         & ( trim(particleBoundary__z).eq."periodic" ) ) then
       call pBoundary__periodic
    end if

    ! ------------------------------------------------------ !
    ! --- [2] particle popout Boundary                   --- !
    ! ------------------------------------------------------ !
    if ( flag__popoutBoundary ) then
       call pBoundary__popout
    endif
    
    return
  end subroutine particle__Boundary
  
  
  ! ====================================================== !
  ! === particle__popout__boundary                     === !
  ! ====================================================== !
  subroutine pBoundary__popout
    use variablesMod
    implicit none
    integer       :: ipt
    logical, save :: flag__initFileMake = .true.

    ! ------------------------------------------------------ !
    ! --- [1] initialize popoutFile                      --- !
    ! ------------------------------------------------------ !
    if ( flag__initFileMake ) then
       open(lun,file=trim(popoutFile),form="formatted",status="replace")
       write(lun,*) "# ipt, pxv "
       close(lun)
    endif
    
    ! ------------------------------------------------------ !
    ! --- [2] popout boundary check                      --- !
    ! ------------------------------------------------------ !
    
    do ipt=1, npt
       if ( pxv(wt_,ipt).gt.0.d0 ) then
          ! ------------------------------------------------------ !
          ! --- [2-1] x-boundary popout                        --- !
          ! ------------------------------------------------------ !
          if ( ( pxv(xp_,ipt).lt.xMin ).or.( pxv(xp_,ipt).gt.xMax ) ) then
             write(6,*) "[particle__boundary] ipt = ", ipt, " pxv = ", pxv(:,ipt)
             open (lun,file=trim(popoutFile),form="formatted",position="append")
             write(lun,*) "ipt = ", ipt, " pxv = ", pxv(:,ipt)
             close(lun)
             pxv(wt_,ipt) = 0.d0
          endif
          ! ------------------------------------------------------ !
          ! --- [2-2] z-boundary popout                        --- !
          ! ------------------------------------------------------ !
          if ( ( pxv(zp_,ipt).lt.zMin ).or.( pxv(zp_,ipt).gt.zMax ) ) then
             write(6,*) "[particle__boundary] ipt = ", ipt, " pxv = ", pxv(:,ipt)
             open (lun,file=trim(popoutFile),form="formatted",position="append")
             write(lun,*) "ipt = ", ipt, " pxv = ", pxv(:,ipt)
             close(lun)
             pxv(wt_,ipt) = 0.d0
          endif
       endif
    enddo
    if ( flag__axisymmetry.eqv..false. ) then
       ! ------------------------------------------------------ !
       ! --- [2-3] y-boundary popout                        --- !
       ! ------------------------------------------------------ !
       if ( ( pxv(yp_,ipt).lt.yMin ).or.( pxv(yp_,ipt).gt.yMax ) ) then
          write(6,*) "[particle__boundary] ipt = ", ipt, " pxv = ", pxv(:,ipt)
          open (lun,file=trim(popoutFile),form="formatted",position="append")
          write(lun,*) "ipt = ", ipt, " pxv = ", pxv(:,ipt)
          close(lun)
          pxv(wt_,ipt) = 0.d0
       endif
    endif

    return
  end subroutine pBoundary__popout
  
  
  ! ====================================================== !
  ! === periodic Boundary condition in z               === !
  ! ====================================================== !
  subroutine pBoundary__periodic
    use variablesMod
    implicit none
    integer                  :: ipt

    ! ------------------------------------------------------ !
    ! --- [1] Boundary condition -- x                    --- !
    ! ------------------------------------------------------ !
    if ( trim(particleBoundary__x).eq."periodic" ) then
       do ipt=1, npt
          if ( pxv(xp_,ipt).lt.xMin ) then
             pxv(xp_,ipt) = pxv(xp_,ipt) + xLeng
          endif
          if ( pxv(xp_,ipt).ge.xMax ) then
             pxv(xp_,ipt) = pxv(xp_,ipt) - xLeng
          endif
       enddo
    endif
    ! ------------------------------------------------------ !
    ! --- [2] Boundary condition -- y                    --- !
    ! ------------------------------------------------------ !
    if ( trim(particleBoundary__y).eq."periodic" ) then
       do ipt=1, npt
          if ( pxv(yp_,ipt).lt.yMin ) then
             pxv(yp_,ipt) = pxv(yp_,ipt) + yLeng
          endif
          if ( pxv(yp_,ipt).ge.yMax ) then
             pxv(yp_,ipt) = pxv(yp_,ipt) - yLeng
          endif
       enddo
    endif
    ! ------------------------------------------------------ !
    ! --- [3] Boundary condition -- z                    --- !
    ! ------------------------------------------------------ !
    if ( trim(particleBoundary__z).eq."periodic" ) then
       do ipt=1, npt
          if ( pxv(zp_,ipt).lt.zMin ) then
             pxv(zp_,ipt) = pxv(zp_,ipt) + zLeng
          endif
          if ( pxv(zp_,ipt).ge.zMax ) then
             pxv(zp_,ipt) = pxv(zp_,ipt) - zLeng
          endif
       enddo
    endif
    
    return
  end subroutine pBoundary__periodic

end module pBoundaryMod



  ! ! ====================================================== !
  ! ! === particle__popout__boundary ( axisymm ver. )    === !
  ! ! ====================================================== !
  ! subroutine pBoundary__popout_axisymm
  !   use variablesMod
  !   implicit none
  !   integer       :: ipt
  !   logical, save :: flag__initFileMake = .true.
    
  !   ! ------------------------------------------------------ !
  !   ! --- [1] initialize popoutFile                      --- !
  !   ! ------------------------------------------------------ !
  !   if ( flag__initFileMake ) then
  !      open(lun,file=trim(popoutFile),form="formatted",status="replace")
  !      write(lun,*) "# ipt, pxv "
  !      close(lun)
  !   endif

  !   ! ------------------------------------------------------ !
  !   ! --- [2] popout boundary check                      --- !
  !   ! ------------------------------------------------------ !

  !   if ( flag__popoutBoundary ) then
    
  !      do ipt=1, npt
  !         if ( pxv(wt_,ipt).gt.0.d0 ) then

  !            ! ------------------------------------------------------ !
  !            ! --- [2-1] x-boundary popout                        --- !
  !            ! ------------------------------------------------------ !
  !            if ( ( pxv(xp_,ipt).lt.xMin ).or.( pxv(xp_,ipt).ge.xMax ) ) then
  !               write(6,*) "[particle__boundary] ipt = ", ipt, " pxv = ", pxv(:,ipt)
  !               open(lun,file=trim(popoutFile),form="formatted",position="append")
  !               write(6,*) "ipt = ", ipt, " pxv = ", pxv(:,ipt)
  !               close(lun)
  !               pxv(wt_,ipt) = 0.d0
  !            endif

  !            ! ------------------------------------------------------ !
  !            ! --- [2-2] z-boundary popout                        --- !
  !            ! ------------------------------------------------------ !
  !            if ( ( pxv(zp_,ipt).lt.zMin ).or.( pxv(zp_,ipt).ge.zMax ) ) then
  !               write(6,*) "[particle__boundary] ipt = ", ipt, " pxv = ", pxv(:,ipt)
  !               open(lun,file=trim(popoutFile),form="formatted",position="append")
  !               write(6,*) "ipt = ", ipt, " pxv = ", pxv(:,ipt)
  !               close(lun)
  !               pxv(wt_,ipt) = 0.d0
  !            endif

  !         endif
  !      enddo

  !   endif

  !   return
  ! end subroutine pBoundary__popout_axisymm
  

