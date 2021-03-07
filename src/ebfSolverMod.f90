module ebfSolverMod
contains

  ! ====================================================== !
  ! === Time Modulation of the field                   === !
  ! ====================================================== !
  subroutine modulate__ebfields
    use variablesMod
    implicit none
    integer                     :: iF
    double precision            :: phi
    double precision, parameter :: twopi = 8.d0 * atan(1.d0)

    ! ------------------------------------------------------ !
    ! --- [1] modulate efield                            --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
       if      ( trim(efields(iF)%modulation_type).eq."cos" ) then
          phi = twopi*efields(iF)%frequency*ptime + efields(iF)%phase/360.0*twopi
          efields(iF)%modulation = cos( phi )
       else if ( trim(efields(iF)%modulation_type).eq."sin" ) then
          phi = twopi*efields(iF)%frequency*ptime + efields(iF)%phase/360.0*twopi
          efields(iF)%modulation = sin( phi )
       else if ( trim(efields(iF)%modulation_type).eq."off" ) then
          efields(iF)%modulation = 1.d0
       else
          write(6,*) "[modulate__ebfields] modulation_type ??? >> into off "
          write(6,*) "                     ", trim( efields(iF)%modulation_type )
          efields(iF)%modulation = 1.d0
       endif
    enddo
    ! ------------------------------------------------------ !
    ! --- [2] modulate bfield                            --- !
    ! ------------------------------------------------------ !
    do iF=1, nBField
       if      ( trim(bfields(iF)%modulation_type).eq."cos" ) then
          phi = twopi*bfields(iF)%frequency*ptime + bfields(iF)%phase/360.0*twopi
          bfields(iF)%modulation = cos( phi )
       else if ( trim(bfields(iF)%modulation_type).eq."sin" ) then
          phi = twopi*bfields(iF)%frequency*ptime + bfields(iF)%phase/360.0*twopi
          bfields(iF)%modulation = sin( phi )
       else if ( trim(bfields(iF)%modulation_type).eq."off" ) then
          bfields(iF)%modulation = 1.d0
       else
          write(6,*) "[modulate__ebfields] modulation_type ??? >> into off "
          write(6,*) "                     ", trim( bfields(iF)%modulation_type )
          bfields(iF)%modulation = 1.d0
       endif
    enddo
    return
  end subroutine modulate__ebfields
  
  
  ! ! ====================================================== !
  ! ! === Oscillate Eigenmode for SW                     === !
  ! ! ====================================================== !
  ! subroutine modulate__swEigenMode
  !   use variablesMod
  !   use fBoundaryMod
  !   implicit none
  !   integer                     :: i, j, k, cmp
  !   double precision            :: coswt, theta_D
  !   double precision, parameter :: twopi = 8.d0*atan(1.d0)
    
  !   ! ------------------------------------------------------ !
  !   ! --- [1] superposition of 2 EigenMode               --- !
  !   ! ------------------------------------------------------ !
  !   theta_D = phase_delay / 360.0 * twopi
  !   coswt   = cos( twopi * freq * ptime + theta_D )
  !   do k=1, LK
  !      do j=1, LJ
  !         do i=1, LI
  !            do cmp=ex_, bz_
  !               EBf(cmp,i,j,k) = Em1(cmp,i,j,k)*coswt
  !            enddo
  !         enddo
  !      enddo
  !   enddo
    
  !   ! ------------------------------------------------------ !
  !   ! --- [2] Boundary Condition                         --- !
  !   ! ------------------------------------------------------ !
  !   call Field__Boundary

  !   return
  ! end subroutine modulate__swEigenMode


  
  

  ! ! ====================================================== !
  ! ! === Blend 2 orthogonal Eigenmode for TW            === !
  ! ! ====================================================== !
  ! subroutine modulate__twEigenMode
  !   use variablesMod
  !   use fBoundaryMod
  !   implicit none
  !   integer                     :: i, j, k, cmp
  !   double precision            :: coswt, sinwt, theta_D
  !   double precision, parameter :: twopi = 8.d0*atan(1.d0)

  !   ! ------------------------------------------------------ !
  !   ! --- [1] superposition of 2 EigenMode               --- !
  !   ! ------------------------------------------------------ !
  !   theta_D = phase_delay / 360.0 * twopi
  !   coswt   = cos( twopi * freq * ptime + theta_D )
  !   sinwt   = sin( twopi * freq * ptime + theta_D )
  !   do k=1, LK
  !      do j=1, LJ
  !         do i=1, LI
  !            do cmp=ex_, bz_
  !               EBf(cmp,i,j,k) = Em1(cmp,i,j,k)*coswt + Em2(cmp,i,j,k)*sinwt
  !            enddo
  !         enddo
  !      enddo
  !   enddo
    
  !   ! ------------------------------------------------------ !
  !   ! --- [2] Boundary Condition                         --- !
  !   ! ------------------------------------------------------ !
  !   call Field__Boundary

  !   return
  ! end subroutine modulate__twEigenMode


end module ebfSolverMod
