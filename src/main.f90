program main
  use variablesMod
  use ioUtilityMod
  use initiatorMod
  use utilitiesMod
  use rkgSolverMod
  use pBoundaryMod
  use fBoundaryMod
  implicit none
  integer :: i, j, k
  
  ! ------------------------------------------------------ !
  ! --- [1] preparation of simulation                  --- !
  ! ------------------------------------------------------ !

  call show__programLogo
  
  call load__configFile
  call initialize__variables

  if ( flag__EField ) call load__EFieldFile
  if ( flag__BField ) call load__BFieldFile

  call Field__Boundary

  
  call load__particles
  call save__particles( "initi" )

  call Determination__DT
  call Determination__iterMax

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
     call show__progressBar( iter, iterMax )

     !  -- [2-2] step forward particle info.           --  !
     call RK4__tracker
     call particle__Boundary

     !  -- [2-3] save particle information             --  !
     if ( ptime.gt.t_nextSave ) then
        call save__particles( "store" )
     endif
  enddo
  write(6,"(a)")
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)") "[main] ===                      END of Main Loop.                   === "
  write(6,"(a)") "[main] ================================================================ "
  write(6,"(a)")

  ! ------------------------------------------------------ !
  ! --- [3] End of Program                             --- !
  ! ------------------------------------------------------ !
  call save__particles( "final" )
  call show__endLogo  
  deallocate( EBf, pxv )

end program main


