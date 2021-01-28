module fBoundaryMod
contains

  ! ====================================================== !
  ! === Field Boundary Condition                       === !
  ! ====================================================== !
  subroutine Field__Boundary
    use variablesMod
    implicit none

    ! ------------------------------------------------------ !
    ! --- [1] x-Boundary                                 --- !
    ! ------------------------------------------------------ !
    if      ( trim(FieldBoundary__x).eq."Neumann"  ) then
       call fBoundary__Neumann ( "x" )
    else if ( trim(FieldBoundary__x).eq."periodic" ) then
       call fBoundary__periodic( "x" )
    endif

    ! ------------------------------------------------------ !
    ! --- [2] y-Boundary                                 --- !
    ! ------------------------------------------------------ !
    if      ( trim(FieldBoundary__y).eq."Neumann"  ) then
       call fBoundary__Neumann ( "y" )
    else if ( trim(FieldBoundary__y).eq."periodic" ) then
       call fBoundary__periodic( "y" )
    endif

    ! ------------------------------------------------------ !
    ! --- [3] z-Boundary                                 --- !
    ! ------------------------------------------------------ !
    if      ( trim(FieldBoundary__z).eq."Neumann"  ) then
       call fBoundary__Neumann ( "z" )
    else if ( trim(FieldBoundary__z).eq."periodic" ) then
       call fBoundary__periodic( "z" )
    endif

    return
  end subroutine Field__Boundary

  
    
  ! ====================================================== !
  ! === Field Boundary :: Neumann                      === !
  ! ====================================================== !
  subroutine fBoundary__Neumann( boundary )
    use variablesMod
    character(1), intent(in) :: boundary

    if ( boundary.eq."x" ) then
       ! -- xMin side -- !
       EBf(:,   0,:,:) = EBf(:, 1,:,:)
       EBf(:,  -1,:,:) = EBf(:, 1,:,:)
       EBf(:,  -2,:,:) = EBf(:, 1,:,:)
       ! -- xMax side -- !
       EBf(:,LI+1,:,:) = EBf(:,LI,:,:)
       EBf(:,LI+2,:,:) = EBf(:,LI,:,:)
       EBf(:,LI+3,:,:) = EBf(:,LI,:,:)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: x :: Neumann "
    endif

    if ( boundary.eq."y" ) then
       ! -- yMin side -- !
       EBf(:,:,   0,:) = EBf(:,:, 1,:)
       EBf(:,:,  -1,:) = EBf(:,:, 1,:)
       EBf(:,:,  -2,:) = EBf(:,:, 1,:)
       ! -- yMax side -- !
       EBf(:,:,LJ+1,:) = EBf(:,:,LJ,:)
       EBf(:,:,LJ+2,:) = EBf(:,:,LJ,:)
       EBf(:,:,LJ+3,:) = EBf(:,:,LJ,:)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: y :: Neumann "
    endif

    if ( boundary.eq."z" ) then
       ! -- zMin side -- !
       EBf(:,:,:,   0) = EBf(:,:,:, 1)
       EBf(:,:,:,  -1) = EBf(:,:,:, 1)
       EBf(:,:,:,  -2) = EBf(:,:,:, 1)
       ! -- zMax side -- !
       EBf(:,:,:,LK+1) = EBf(:,:,:,LK)
       EBf(:,:,:,LK+2) = EBf(:,:,:,LK)
       EBf(:,:,:,LK+3) = EBf(:,:,:,LK)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: z :: Neumann "
    endif
    return
  end subroutine fBoundary__Neumann


  ! ====================================================== !
  ! === Field Boundary :: periodic                     === !
  ! ====================================================== !
  subroutine fBoundary__periodic( boundary )
    use variablesMod
    character(1), intent(in) :: boundary
    
    if ( boundary.eq."x" ) then
       ! -- xMin side -- !
       EBf(:,   0,:,:) = EBf(:,LI  ,:,:)
       EBf(:,  -1,:,:) = EBf(:,LI-1,:,:)
       EBf(:,  -2,:,:) = EBf(:,LI-2,:,:)
       ! -- xMax side -- !
       EBf(:,LI+1,:,:) = EBf(:,   1,:,:)
       EBf(:,LI+2,:,:) = EBf(:,   2,:,:)
       EBf(:,LI+3,:,:) = EBf(:,   3,:,:)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: x :: periodic "
    endif

    if ( boundary.eq."y" ) then
       ! -- yMin side -- !
       EBf(:,:,   0,:) = EBf(:,:,LJ  ,:)
       EBf(:,:,  -1,:) = EBf(:,:,LJ-1,:)
       EBf(:,:,  -2,:) = EBf(:,:,LJ-2,:)
       ! -- yMax side -- !
       EBf(:,:,LJ+1,:) = EBf(:,:,   1,:)
       EBf(:,:,LJ+2,:) = EBf(:,:,   2,:)
       EBf(:,:,LJ+3,:) = EBf(:,:,   3,:)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: y :: periodic "
    endif

    if ( boundary.eq."z" ) then
       ! -- zMin side -- !
       EBf(:,:,:,   0) = EBf(:,:,:,LK  )
       EBf(:,:,:,  -1) = EBf(:,:,:,LK-1)
       EBf(:,:,:,  -2) = EBf(:,:,:,LK-2)
       ! -- zMax side -- !
       EBf(:,:,:,LK+1) = EBf(:,:,:,   1)
       EBf(:,:,:,LK+2) = EBf(:,:,:,   2)
       EBf(:,:,:,LK+3) = EBf(:,:,:,   3)
       write(6,"(a)") "[fBoundary__Neumann] Field Boundary :: z :: periodic "
    endif
    return
  end subroutine fBoundary__periodic

  
end module fBoundaryMod
