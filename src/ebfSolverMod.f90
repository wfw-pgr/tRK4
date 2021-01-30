module ebfSolverMod
contains
  

  ! ====================================================== !
  ! === Blend 2 orthogonal Eigenmode for TW            === !
  ! ====================================================== !
  subroutine modulate__twEigenMode
    use variablesMod
    use fBoundaryMod
    implicit none
    integer                     :: i, j, k, cmp
    double precision            :: coswt, sinwt
    double precision, parameter :: twopi = 8.d0*atan(1.d0)

    ! ------------------------------------------------------ !
    ! --- [1] superposition of 2 EigenMode               --- !
    ! ------------------------------------------------------ !
    coswt = cos( twopi * freq * ptime )
    sinwt = sin( twopi * freq * ptime )
    do k=1, LK
       do j=1, LJ
          do i=1, LI
             do cmp=ex_, ez_
                EBf(cmp,i,j,k) = Em1(cmp,i,j,k)*coswt + Em2(cmp,i,j,k)*sinwt
             enddo
          enddo
       enddo
    enddo
    
    ! ------------------------------------------------------ !
    ! --- [2] Boundary Condition                         --- !
    ! ------------------------------------------------------ !
    call Field__Boundary

    return
  end subroutine modulate__twEigenMode


end module ebfSolverMod
