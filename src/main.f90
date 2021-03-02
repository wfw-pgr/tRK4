program main
  use variablesMod
  use ioUtilityMod
  use initiatorMod
  use utilitiesMod
  use rkgSolverMod
  use ebfSolverMod
  use pBoundaryMod
  use fBoundaryMod
  use diagnosisMod
  implicit none
  integer :: i, j, k, ipt
  
  ! ------------------------------------------------------ !
  ! --- [1] preparation of simulation                  --- !
  ! ------------------------------------------------------ !

  call show__programLogo
  
  call load__configFile
  call initialize__variables

  call load__ebFieldFile
  call Field__Boundary

  call load__particles
  call into__relativistic
     
  call check__scale_of_variables
  call Determination__DT
  call Determination__iterMax

  if ( flag__axisymmetry  ) call initialize__axisymmMode
  if ( flag__saveParticle ) call save__particles  ( "initi" )
  if ( flag__probeField   ) call probe__eulerField( "initi" )

  if (   ( trim(particleBoundary__x).eq."periodic" ).or.&
       & ( trim(particleBoundary__y).eq."periodic" ).or.&
       & ( trim(particleBoundary__z).eq."periodic" ) ) then
     call initialize__periodicField
  endif
  
  write(6,"(a)")
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)") "[main] ===                  Begening of Main Loop.                  === "
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)")
  ! ------------------------------------------------------ !
  ! --- [2] Main Loop                                  --- !
  ! ------------------------------------------------------ !
  do iter=1, iterMax
     !  -- [2-1] display progress bar                  --  !
     ptime = ptime + dt
     if ( mod( iter, progress_interval ).eq.0 ) then
        call show__progressBar( iter, iterMax )
     endif
     
     ! -- [2-2] field solver                          --  !
     call modulate__ebfields
     
     !  -- [2-3] step forward particle info.           --  !
     call RK4__tracker
     if ( flag__axisymmetry    ) call into__rtz_coordinate
     call particle__Boundary
     
     !  -- [2-4] save particle information             --  !
     if ( ( flag__saveParticle ).and.( ptime.gt.t_nextSave  ) ) then
        call save__particles( "store" )
     endif
     if ( ( flag__probeField   ).and.( ptime.gt.t_nextProbe ) ) then
        call probe__eulerField( "store" )
     endif
     
     !  -- [2-5] beam position monitor                 --  !
     ! if ( flag__beamposmonitor ) then
     !    call screen__beamPosMonitor( bpm_direction, bpm_screen_pos )
     ! endif
     
  enddo
  write(6,"(a)")
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)") "[main] ===                      END of Main Loop.                   === "
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)")

  ! ------------------------------------------------------ !
  ! --- [3] End of Program                             --- !
  ! ------------------------------------------------------ !
  
  if ( flag__saveParticle ) call save__particles  ( "final" )
  if ( flag__probeField   ) call probe__eulerField( "final" )
  
  call show__endLogo  
  deallocate( pxv )

end program main




! if ( flag__standingWave ) then
!    call load__swEigenMode
! endif
! if ( flag__travellingWave ) then
!    call load__twEigenMode
! endif
! if ( flag__EField ) call load__EFieldFile
! if ( flag__BField ) call load__BFieldFile

! if ( flag__standingWave ) then
!    call modulate__swEigenMode
! endif
! if ( flag__travellingWave ) then
!    call modulate__twEigenMode
! endif

