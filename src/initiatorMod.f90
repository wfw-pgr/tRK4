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

    if ( flag__axisymmetry ) then
       dx    = xLeng / dble( LI-1 )
       dy    =  0.d0
       dz    = zLeng / dble( LK-1 )
       dxInv =  1.d0 / dx
       dyInv =  0.d0
       dzInv =  1.d0 / dz
    else
       dx    = xLeng / dble( LI-1 )
       dy    = yLeng / dble( LJ-1 )
       dz    = zLeng / dble( LK-1 )
       dxInv =  1.d0 / dx
       dyInv =  1.d0 / dy
       dzInv =  1.d0 / dz
    endif

    ! ------------------------------------------------------ !
    ! --- [3] initialization of time sequence            --- !
    ! ------------------------------------------------------ !
    ptime = t_simuStart


    ! ------------------------------------------------------ !
    ! --- [4] display variables                          --- !
    ! ------------------------------------------------------ !
    
    write(6,"(a)"            ) "[initialize__variables]    ========  [initialized variables   ]  ======== "
    write(6,"(a,18x,a,e15.8)") "qm"   ,  " :: ", qm
    write(6,"(a,15x,a,e15.8)") "xLeng",  " :: ", xLeng
    write(6,"(a,15x,a,e15.8)") "yLeng",  " :: ", yLeng
    write(6,"(a,15x,a,e15.8)") "zLeng",  " :: ", zLeng
    write(6,"(a,18x,a,e15.8)") "dx"   ,  " :: ", dx
    write(6,"(a,18x,a,e15.8)") "dy"   ,  " :: ", dy
    write(6,"(a,18x,a,e15.8)") "dz"   ,  " :: ", dz
    write(6,"(a,15x,a,e15.8)") "dxInv",  " :: ", dxInv
    write(6,"(a,15x,a,e15.8)") "dyInv",  " :: ", dyInv
    write(6,"(a,15x,a,e15.8)") "dzInv",  " :: ", dzInv
    write(6,"(a)"            ) "[initialize__variables]    ============================================== "    
    
    return
  end subroutine initialize__variables


  
  ! ====================================================== !
  ! === initialize periodic field condition            === !
  ! ====================================================== !
  subroutine initialize__periodicField
    use variablesMod
    implicit none

    allocate( period_counter(npt) )
    period_counter(:) = 0
    write(6,"(a)") "[initialize__periodicField] initilizing period_counter... [Done]"
    return
  end subroutine initialize__periodicField


  
  ! ====================================================== !
  ! === initialize axisymmetric mode                   === !
  ! ====================================================== !
  subroutine initialize__axisymmMode
    use variablesMod
    use rkgSolverMod
    implicit none

    ! ------------------------------------------------------ !
    ! --- [1] check simulation condition                 --- !
    ! ------------------------------------------------------ !
    if ( ( LJ.ne.1 ).or.( yMin.ne.0.d0 ).or.( yMax.ne.0.d0 ) ) then
       write(6,"(a)") "[initialize__axisymmMode]  illegal coordinate  [ERROR]"
       write(6,*    ) "            :: flag__axisymmetry   :: ", flag__axisymmetry
       write(6,*    ) "            :: LJ   ( == 1 )       :: ", LJ
       write(6,*    ) "            :: yMin ( == 0 )       :: ", yMin
       write(6,*    ) "            :: yMax ( == 0 )       :: ", yMax
       stop
    endif
    if ( trim(FieldBoundary__y).eq."Neumann" ) then
       ! --- nothing to do --- !
    else
       write(6,"(a)") "[initialize__axisymmMode]  FieldBoundary__y  !=  Neumann  [CAUTION]"
       write(6,"(a)") "[initialize__axisymmMode]      change into Neumann..... "
       FieldBoundary__y = "Neumann"
    endif
    
    ! ------------------------------------------------------ !
    ! --- [2] particle coordinate ==> rtz                --- !
    ! ------------------------------------------------------ !
    call into__rtz_coordinate

    ! ------------------------------------------------------ !
    ! --- [3] Message & return                           --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,"(a)") "[initialize__axisymmMode] flag__axisymmetry == .true. >>> axisymmetric Mode."
    write(6,*)

    return
  end subroutine initialize__axisymmMode
    
  


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
          write(6,"(a)") "[Determination__DT] No BField File, but dt is given by [ wci_ / mix_ ] mode " 
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
       write(6,"(a)") "[Determination__DT] incompatible type__dt [ERROR] "
       stop
    endif
    
    ! ------------------------------------------------------ !
    ! --- [2] Display of dt                              --- !
    ! ------------------------------------------------------ !
    write(6,"(a)"      )
    write(6,"(a)"      ) "[Determination__DT]        ========  [Determination of   DT   ]  ======== "
    write(6,"(a,a)"    ) "[Determination__DT]          :        type__dt == ", trim(type__dt)
    write(6,"(a,e15.8)") "[Determination__DT]          :        dt_wci   == ", dt_wci
    write(6,"(a,e15.8)") "[Determination__DT]          :        dt_CFL   == ", dt_CFL
    write(6,"(a)"      ) "[Determination__DT]         --------------------------------------- "
    write(6,"(a,e15.8)") "[Determination__DT]          :            DT   == ", dt
    write(6,"(a)"      ) "[Determination__DT]        ============================================== "
    write(6,"(a)"      )

    if ( ( dt_wci.lt.dt ).or.( dt_CFL.lt.dt ) ) then
       write(6,*)
       write(6,*)
       write(6,"(a)") "#############################  [CAUTION]  ##############################"
       write(6,*)
       write(6,"(a)") "[Determination__DT]  [CAUTION] dt < dt_wci , dt < dt_CFL !!!! "
       write(6,*)
       write(6,"(a)") "########################################################################"
       write(6,*)
       write(6,*)
    endif
    
    return
  end subroutine Determination__DT
  

  ! ====================================================== !
  ! === Determination of iterMax                       === !
  ! ====================================================== !
  subroutine Determination__iterMax
    use variablesMod
    implicit none

    if      ( trim(type__iterMax).eq."load" ) then
       t_simuEnd = t_simuStart + dt*iterMax
    else if ( trim(type__iterMax).eq."Auto" ) then
       iterMax = ceiling( ( t_simuEnd - t_simuStart ) / dt )
    else
       write(6,"(a)") "[Determination__iterMax] incompatible type__iterMax [ERROR] "
       stop
    endif
       
    write(6,"(a)"    )
    write(6,"(a)"    ) "[Determination__iterMax]   ========  [Determination of iterMax] ======== "
    write(6,"(a,a)"  ) "[Determination__iterMax]     : type__iterMax   == ", trim(type__iterMax)
    write(6,"(a,i10)") "[Determination__iterMax]     :       iterMax   == ", iterMax
    write(6,"(a)"    ) "[Determination__iterMax]   ============================================= "
    write(6,"(a)"    )
    return
  end subroutine 


end module initiatorMod
