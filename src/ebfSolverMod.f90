module ebfSolverMod
contains

  ! ====================================================== !
  ! === Time Modulation of the field                   === !
  ! ====================================================== !
  subroutine modulate__ebfields
    use variablesMod
    implicit none
    integer                     :: iF
    double precision, parameter :: twopi = 16.d0 * atan(1.d0)

    ! ------------------------------------------------------ !
    ! --- [1] modulate efield                            --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
       if      ( trim(efields(iF)%modulation_type).eq."cos" ) then
          efields(iF)%modulation = cos( twopi*freq*ptime + phase_delay/360.0*twopi )
       else if ( trim(efields(iF)%modulation_type).eq."sin" ) then
          efields(iF)%modulation = sin( twopi*freq*ptime + phase_delay/360.0*twopi )
       else if ( trim(efields(iF)%modulation_type).eq."off" ) then
          efields(iF)%modulation = 1.d0
       endif
    enddo
    ! ------------------------------------------------------ !
    ! --- [2] modulate bfield                            --- !
    ! ------------------------------------------------------ !
    do iF=1, nBField
       if      ( trim(bfields(iF)%modulation_type).eq."cos" ) then
          bfields(iF)%modulation = cos( twopi*freq*ptime + phase_delay/360.0*twopi )
       else if ( trim(bfields(iF)%modulation_type).eq."sin" ) then
          bfields(iF)%modulation = sin( twopi*freq*ptime + phase_delay/360.0*twopi )
       else if ( trim(bfields(iF)%modulation_type).eq."off" ) then
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
