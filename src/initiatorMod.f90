module initiatorMod
contains


  ! ====================================================== !
  ! === initialize settings of particle tracking       === !
  ! ====================================================== !
  subroutine initialize__variables
    use variablesMod
    implicit none

    ! ------------------------------------------------------ !
    ! --- [1] initialization of constants                --- !
    ! ------------------------------------------------------ !
    qm    =  qe / Mp

    ! ------------------------------------------------------ !
    ! --- [2] initialization of length                   --- !
    ! ------------------------------------------------------ !
    xLeng =  xMax - xMin
    yLeng =  yMax - yMin
    zLeng =  zMax - zMin
    dx    = xLeng / dble( LI-1 )
    dy    = yLeng / dble( LJ-1 )
    dz    = zLeng / dble( LK-1 )
    dxInv =  1.d0 / dx
    dyInv =  1.d0 / dy
    dzInv =  1.d0 / dz

    ! ------------------------------------------------------ !
    ! --- [3] initialization of time sequence            --- !
    ! ------------------------------------------------------ !
    ptime = t_simuStart
    
    return
  end subroutine initialize__variables


  ! ====================================================== !
  ! === Determination of dt                            === !
  ! ====================================================== !
  subroutine Determination__DT
    use variablesMod
    implicit none
    integer                     :: i, j, k
    double precision            :: dt_CFL, dt_wci, maxB, absB
    double precision, parameter :: twopi = 8.d0 * atan(1.d0)

    ! ------------------------------------------------------ !
    ! --- [1] Determination of DT                        --- !
    ! ------------------------------------------------------ !
    !  -- [1-1] dt  by CFL condition ( for PIC )         --  !
    dt_wci = 0.d0
    dt_CFL = alpha_CFL / ( cv*dxInv + cv*dyInv + cv*dzInv )

    !  -- [1-2] dt  by cyclotron motion ( BField )       --  !
    if ( flag__BField ) then
       maxB   = 0.d0
       do k=1, LK
          do j=1, LJ
             do i=1, LI
                absB = sqrt( EBf(bx_,i,j,k)**2 + EBf(by_,i,j,k)**2 + EBf(bz_,i,j,k)**2 )
                maxB =  max( maxB, absB )
             enddo
          enddo
       enddo
       dt_wci = alpha_wci * ( twopi * Mp ) / ( qe * maxB )
    else
       if ( ( trim( type__dt ).eq."wci_" ).or.( trim(type__dt).eq."mix_" ) ) then
          write(6,*) "[Determination__DT] No BField File, but dt is given by [ wci_ / mix_ ] mode " 
          stop
       endif
       dt_wci = 10.d0 * dt_CFL
    endif

    !  -- [1-3] dt  determination                        --  !
    if      ( trim(type__dt).eq."wci_" ) then
       dt = dt_wci
    else if ( trim(type__dt).eq."CFL_" ) then    
       dt = dt_CFL
    else if ( trim(type__dt).eq."mix_" ) then
       dt = min( dt_wci, dt_CFL )
    else if ( trim(type__dt).eq."load" ) then
       ! -- nothing -- !
    else
       write(6,*) "[Determination__DT] incompatible type__dt [ERROR] "
       stop
    endif
    
    ! ------------------------------------------------------ !
    ! --- [2] Display of dt                              --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,*) "[Determination__DT]        ========  [Determination of   DT   ]  ======== "
    write(6,*) "[Determination__DT]          :        type__dt == ", trim(type__dt)
    write(6,*) "[Determination__DT]          :        dt_wci   == ", dt_wci
    write(6,*) "[Determination__DT]          :        dt_CFL   == ", dt_CFL
    write(6,*) "[Determination__DT]         --------------------------------------- "
    write(6,*) "[Determination__DT]          :            DT   == ", dt
    write(6,*) "[Determination__DT]        ============================================== "
    write(6,*)
    
    return
  end subroutine Determination__DT
  

  ! ====================================================== !
  ! === Determination of iterMax                       === !
  ! ====================================================== !
  subroutine Determination__iterMax
    use variablesMod
    implicit none

    if      ( trim(type__iterMax).eq."load" ) then
       ! -- nothing -- !
    else if ( trim(type__iterMax).eq."Auto" ) then
       iterMax = ceiling( ( t_simuEnd - t_simuStart ) / dt )
    else
       write(6,*) "[Determination__iterMax] incompatible type__iterMax [ERROR] "
       stop
    endif
       
    write(6,*)
    write(6,*) "[Determination__iterMax]   ========  [Determination of iterMax] ======== "
    write(6,*) "[Determination__iterMax]     : type__iterMax   == ", trim(type__iterMax)
    write(6,*) "[Determination__iterMax]     :       iterMax   == ", iterMax
    write(6,*) "[Determination__iterMax]   ============================================= "
    write(6,*)
    return
  end subroutine 


end module initiatorMod
