module fBoundaryMod
contains

  
  ! ====================================================== !
  ! === Field Boundary Condition                       === !
  ! ====================================================== !
  subroutine Field__Boundary
    use variablesMod
    implicit none
    integer :: iF

    ! ------------------------------------------------------ !
    ! --- [1] x-Boundary                                 --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
       if      ( trim( efields(iF)%boundary_x ).eq."Neumann"  ) then
          call fBoundary__Neumann ( efields(iF)%EBf, "x", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       else if ( trim( efields(iF)%boundary_x ).eq."periodic" ) then
          call fBoundary__Periodic( efields(iF)%EBf, "x", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       end if
    enddo
    do iF=1, nBfield
       if      ( trim( bfields(iF)%boundary_x ).eq."Neumann"  ) then
          call fBoundary__Neumann ( bfields(iF)%EBf, "x", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       else if ( trim( bfields(iF)%boundary_x ).eq."periodic" ) then
          call fBoundary__Periodic( bfields(iF)%EBf, "x", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       end if
    enddo

    ! ------------------------------------------------------ !
    ! --- [2] y-Boundary                                 --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
       if      ( trim( efields(iF)%boundary_y ).eq."Neumann"  ) then
          call fBoundary__Neumann ( efields(iF)%EBf, "y", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       else if ( trim( efields(iF)%boundary_y ).eq."periodic" ) then
          call fBoundary__Periodic( efields(iF)%EBf, "y", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       end if
    enddo
    do iF=1, nBfield
       if      ( trim( bfields(iF)%boundary_y ).eq."Neumann"  ) then
          call fBoundary__Neumann ( bfields(iF)%EBf, "y", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       else if ( trim( bfields(iF)%boundary_y ).eq."periodic" ) then
          call fBoundary__Periodic( bfields(iF)%EBf, "y", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       end if
    enddo

    ! ------------------------------------------------------ !
    ! --- [3] z-Boundary                                 --- !
    ! ------------------------------------------------------ !
    do iF=1, nEField
       if      ( trim( efields(iF)%boundary_z ).eq."Neumann"  ) then
          call fBoundary__Neumann ( efields(iF)%EBf, "z", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       else if ( trim( efields(iF)%boundary_z ).eq."periodic" ) then
          call fBoundary__Periodic( efields(iF)%EBf, "z", &
               & efields(iF)%LI, efields(iF)%LJ, efields(iF)%LK )
       end if
    enddo
    do iF=1, nBfield
       if      ( trim( bfields(iF)%boundary_z ).eq."Neumann"  ) then
          call fBoundary__Neumann ( bfields(iF)%EBf, "z", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       else if ( trim( bfields(iF)%boundary_z ).eq."periodic" ) then
          call fBoundary__Periodic( bfields(iF)%EBf, "z", &
               & bfields(iF)%LI, bfields(iF)%LJ, bfields(iF)%LK )
       end if
    enddo

    return
  end subroutine Field__Boundary

  
  ! ====================================================== !
  ! === Field Boundary :: Neumann                      === !
  ! ====================================================== !
  subroutine fBoundary__Neumann( EBh, boundary, LIh, LJh, LKh )
    use variablesMod
    implicit none
    integer         , intent(in)    :: LIh, LJh, LKh
    character(1)    , intent(in)    :: boundary
    double precision, intent(inout) :: EBh(6,-2:LIh+3,-2:LJh+3,-2:LKh+3)

    if ( boundary.eq."x" ) then
       ! -- xMin side -- !
       EBh(:,    0,:,:) = EBh(:,  1,:,:)
       EBh(:,   -1,:,:) = EBh(:,  1,:,:)
       EBh(:,   -2,:,:) = EBh(:,  1,:,:)
       ! -- xMax side -- !
       EBh(:,LIh+1,:,:) = EBh(:,LIh,:,:)
       EBh(:,LIh+2,:,:) = EBh(:,LIh,:,:)
       EBh(:,LIh+3,:,:) = EBh(:,LIh,:,:)
    endif

    if ( boundary.eq."y" ) then
       ! -- yMin side -- !
       EBh(:,:,    0,:) = EBh(:,:,  1,:)
       EBh(:,:,   -1,:) = EBh(:,:,  1,:)
       EBh(:,:,   -2,:) = EBh(:,:,  1,:)
       ! -- yMax side -- !
       EBh(:,:,LJh+1,:) = EBh(:,:,LJh,:)
       EBh(:,:,LJh+2,:) = EBh(:,:,LJh,:)
       EBh(:,:,LJh+3,:) = EBh(:,:,LJh,:)
    endif

    if ( boundary.eq."z" ) then
       ! -- zMin side -- !
       EBh(:,:,:,    0) = EBh(:,:,:,  1)
       EBh(:,:,:,   -1) = EBh(:,:,:,  1)
       EBh(:,:,:,   -2) = EBh(:,:,:,  1)
       ! -- zMax side -- !
       EBh(:,:,:,LKh+1) = EBh(:,:,:,LKh)
       EBh(:,:,:,LKh+2) = EBh(:,:,:,LKh)
       EBh(:,:,:,LKh+3) = EBh(:,:,:,LKh)
    endif
    return
  end subroutine fBoundary__Neumann


  ! ====================================================== !
  ! === Field Boundary :: periodic                     === !
  ! ====================================================== !
  subroutine fBoundary__periodic( EBh, boundary, LIh, LJh, LKh )
    use variablesMod
    implicit none
    integer         , intent(in)    :: LIh, LJh, LKh
    character(1)    , intent(in)    :: boundary
    double precision, intent(inout) :: EBh(6,-2:LIh+3,-2:LJh+3,-2:LKh+3)
    
    if ( boundary.eq."x" ) then
       ! -- xMin side -- !
       EBh(:,    0,:,:) = EBh(:,LIh  ,:,:)
       EBh(:,   -1,:,:) = EBh(:,LIh-1,:,:)
       EBh(:,   -2,:,:) = EBh(:,LIh-2,:,:)
       ! -- xMax side -- !
       EBh(:,LIh+1,:,:) = EBh(:,    1,:,:)
       EBh(:,LIh+2,:,:) = EBh(:,    2,:,:)
       EBh(:,LIh+3,:,:) = EBh(:,    3,:,:)
    endif

    if ( boundary.eq."y" ) then
       ! -- yMin side -- !
       EBh(:,:,    0,:) = EBh(:,:,LJh  ,:)
       EBh(:,:,   -1,:) = EBh(:,:,LJh-1,:)
       EBh(:,:,   -2,:) = EBh(:,:,LJh-2,:)
       ! -- yMax side -- !
       EBh(:,:,LJh+1,:) = EBh(:,:,    1,:)
       EBh(:,:,LJh+2,:) = EBh(:,:,    2,:)
       EBh(:,:,LJh+3,:) = EBh(:,:,    3,:)
    endif

    if ( boundary.eq."z" ) then
       ! -- zMin side -- !
       EBh(:,:,:,    0) = EBh(:,:,:,LKh  )
       EBh(:,:,:,   -1) = EBh(:,:,:,LKh-1)
       EBh(:,:,:,   -2) = EBh(:,:,:,LKh-2)
       ! -- zMax side -- !
       EBh(:,:,:,LKh+1) = EBh(:,:,:,    1)
       EBh(:,:,:,LKh+2) = EBh(:,:,:,    2)
       EBh(:,:,:,LKh+3) = EBh(:,:,:,    3)
    endif
    
    return
  end subroutine fBoundary__periodic

  
end module fBoundaryMod
