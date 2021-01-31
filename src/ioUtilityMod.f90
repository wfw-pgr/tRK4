module ioUtilityMod
contains

  ! ====================================================== !
  ! === load parameter.conf File                       === !
  ! ====================================================== !
  subroutine load__configFile
    use variablesMod
    implicit none
    character(cLen)          :: cmt
    character(17), parameter :: char_fmt = "(a24,1x,a14,1x,a)"

    namelist /parameters/ particleFile, EFieldFile, BFieldFile, twEigenFile1, twEigenFile2,             &
         &                type__EFieldFile, type__BFieldFile, trackFileBase, probeFileBase, popoutFile, &
         &                flag__EField, flag__BField, flag__cyclicCoordinate, flag__travellingWave,     &
         &                flag__saveParticle, flag__probeField,                                         &
         &                FieldBoundary__x, FieldBoundary__y, FieldBoundary__z,                         &
         &                particleBoundary__x, particleBoundary__y, particleBoundary__z,                &
         &                LI, LJ, LK, xMin, xMax, yMin, yMax, zMin, zMax,                               &
         &                type__iterMax, type__dt, iterMax, dt, alpha_wci, alpha_CFL, freq,             &
         &                t_simuStart, t_simuEnd, t_trackStart, t_trackStep, t_trackEnd,                &
         &                t_probeStart, t_probeStep, t_probeEnd
    
    open(lun,file=trim(configFile),status="old",form="formatted")
    read(lun,nml=parameters)
    close(lun)
    
    return
  end subroutine load__configFile
  

  ! ====================================================== !
  ! === load particle from external File               === !
  ! ====================================================== !
  subroutine load__particles
    use variablesMod
    implicit none
    integer            :: ipt, nLines, nCmpr
    integer, parameter :: nCmp = 13
    character(cLen)    :: cmt
    logical, parameter :: flag__particleCheck = .false.
    
    ! ------------------------------------------------------ !
    ! --- [1] prepare pxv                                --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,"(a,a)") "[load__particles]  particles :: ", trim( particleFile )
    write(6,"(a)",advance="no" ) "[load__particles]  loading particles.... "

    open(lun,file=trim(particleFile),form="formatted")
    read(lun,*) cmt
    read(lun,*) cmt
    read(lun,*) cmt, npt, nCmpr
    allocate( pxv(nCmp,npt) )
    pxv(:,:) = 0.d0
    do ipt=1, npt
       read(lun,*) pxv(xp_:vz_,ipt)
    enddo
    close(lun)
    
    write(6,"(a)",advance="yes") "[Done]"
    write(6,"(a,i8)") "[load__particles] Number of Particles ::  ", npt
    write(6,*)

    ! ------------------------------------------------------ !
    ! --- [2] copy old info & set weight                 --- !
    ! ------------------------------------------------------ !
    do ipt=1, npt
       pxv(xo_:uz_,ipt) = pxv(xp_:vz_,ipt)
       pxv(wt_    ,ipt) = 1.d0
    enddo

    ! ------------------------------------------------------ !
    ! --- [3] check loaded particles                     --- !
    ! ------------------------------------------------------ !
    if ( flag__particleCheck ) then
       do ipt=1, npt
          write(6,"(i8,6(e12.5,1x))") ipt, pxv(xp_:vz_,ipt)
       enddo
    endif
    
    return
  end subroutine load__particles
  

  ! ====================================================== !
  ! === load BField from external File                 === !
  ! ====================================================== !
  subroutine load__BFieldFile
    use variablesMod
    implicit none
    integer                       :: i, j, k, LIr, LJr, LKr, nCmpr
    character(cLen)               :: cmt
    double precision              :: xg(3)

    ! ------------------------------------------------------ !
    ! --- [1] Preparation                                --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,"(a,a)") "[load__BFieldFile]  BFieldFile :: ", trim( BFieldFile )
    write(6,"(a)",advance="no" ) "[load__BFieldFile]  loading BField.... "
    
    ! ------------------------------------------------------ !
    ! --- [2] point (Text) File Type case                --- !
    ! ------------------------------------------------------ !
    if ( trim(type__BFieldFile).eq."point" ) then
       open (lun,file=trim(BFieldFile),status="old",form="formatted")
       read (lun,*) cmt
       read (lun,*) cmt
       read (lun,*) cmt, LKr, LJr, LIr, nCmpr
       if ( ( LI.ne.LIr ).or.( LJ.ne.LJr ).or.( LK.ne.LKr ) ) then
          write(6,*)
          write(6,*) "[load__BFieldFile] [ERROR] LI, LJ, LK  != LIr, LJr, LKr "
          stop
       endif
       if ( .not.( allocated( EBf ) ) ) then
          allocate( EBf(6,-2:LI+3,-2:LJ+3,-2:LK+3) )
       endif
       do k=1, LK
          do j=1, LJ
             do i=1, LI
                read(lun,*) xg(1:3), EBf(4:6,i,j,k)
             enddo
          enddo
       enddo
       close(lun)
    else
       write(6,*)
       write(6,*)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*) "[load__BFieldFile] cannot find BFieldFile [ERROR]  :: ", trim(BFieldFile)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*)
       write(6,*)
       stop
    endif

    ! ------------------------------------------------------ !
    ! --- [3] post process                               --- !
    ! ------------------------------------------------------ !
    write(6,"(a)",advance="yes") "[Done]"
    write(6,*)

    return
  end subroutine load__BFieldFile

  
  ! ====================================================== !
  ! === load EField from external File                 === !
  ! ====================================================== !
  subroutine load__EFieldFile
    use variablesMod
    implicit none
    integer                       :: i, j, k, LIr, LJr, LKr, nCmpr
    character(cLen)               :: cmt
    double precision              :: xg(3)

    ! ------------------------------------------------------ !
    ! --- [1] Preparation                                --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,"(a,a)") "[load__EFieldFile]  EFieldFile :: ", trim( EFieldFile )
    write(6,"(a)",advance="no" ) "[load__EFieldFile]  loading EField.... "

    ! ------------------------------------------------------ !
    ! --- [2] point (Text) File Type case                --- !
    ! ------------------------------------------------------ !
    if ( trim(type__EFieldFile).eq."point" ) then
       open (lun,file=trim(EFieldFile),status="old",form="formatted")
       read (lun,*) cmt
       read (lun,*) cmt
       read (lun,*) cmt, LKr, LJr, LIr, nCmpr
       if ( ( LI.ne.LIr ).or.( LJ.ne.LJr ).or.( LK.ne.LKr ) ) then
          write(6,*)
          write(6,*) "[load__EFieldFile] [ERROR] LI, LJ, LK  != LIr, LJr, LKr "
          stop
       endif
       if ( .not.( allocated( EBf ) ) ) then
          allocate( EBf(6,-2:LI+3,-2:LJ+3,-2:LK+3) )
       endif
       do k=1, LK
          do j=1, LJ
             do i=1, LI
                read(lun,*) xg(1:3), EBf(1:3,i,j,k)
             enddo
          enddo
       enddo
       close(lun)
    else
       write(6,*)
       write(6,*)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*) "[load__EFieldFile] cannot find EFieldFile [ERROR]  :: ", trim(EFieldFile)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*)
       write(6,*)
       stop
    endif

    ! ------------------------------------------------------ !
    ! --- [3] post process                               --- !
    ! ------------------------------------------------------ !
    write(6,"(a)",advance="yes") "[Done]"
    write(6,*)
    
    return
  end subroutine load__EFieldFile


  ! ====================================================== !
  ! === load travelling wave eigenmode                 === !
  ! ====================================================== !
  subroutine load__twEigenMode
    use variablesMod
    implicit none
    integer                       :: i, j, k, LIr, LJr, LKr, nCmpr
    character(cLen)               :: cmt
    double precision              :: xg(3)

    ! ------------------------------------------------------ !
    ! --- [1] Preparation                                --- !
    ! ------------------------------------------------------ !
    write(6,*)
    write(6,"(a,a)") "[load__twEigenMode]  twEigenFile1 :: ", trim( twEigenFile1 )
    write(6,"(a,a)") "[load__twEigenMode]  twEigenFile2 :: ", trim( twEigenFile2 )
    write(6,"(a)",advance="no" ) "[load__twEigenMode]  loading EField.... "

    ! ------------------------------------------------------ !
    ! --- [2] point (Text) File Type case ( mode1 )      --- !
    ! ------------------------------------------------------ !
    if ( trim(type__EFieldFile).eq."point" ) then
       
       !  -- [2-1] mode 1                                --  !
       open (lun,file=trim(twEigenFile1),status="old",form="formatted")
       read (lun,*) cmt
       read (lun,*) cmt
       read (lun,*) cmt, LKr, LJr, LIr, nCmpr
       if ( ( LI.ne.LIr ).or.( LJ.ne.LJr ).or.( LK.ne.LKr ) ) then
          write(6,*)
          write(6,*) "[load__twEigenMode] [ERROR] LI, LJ, LK  != LIr, LJr, LKr "
          stop
       endif
       if ( .not.( allocated( Em1 ) ) ) then
          allocate( Em1(6,-2:LI+3,-2:LJ+3,-2:LK+3) )
       endif
       do k=1, LK
          do j=1, LJ
             do i=1, LI
                read(lun,*) xg(1:3), Em1(1:3,i,j,k)
             enddo
          enddo
       enddo
       close(lun)
       
       !  -- [2-2] mode 2                                --  !
       open (lun,file=trim(twEigenFile2),status="old",form="formatted")
       read (lun,*) cmt
       read (lun,*) cmt
       read (lun,*) cmt, LKr, LJr, LIr, nCmpr
       if ( ( LI.ne.LIr ).or.( LJ.ne.LJr ).or.( LK.ne.LKr ) ) then
          write(6,*)
          write(6,*) "[load__twEigenMode] [ERROR] LI, LJ, LK  != LIr, LJr, LKr "
          stop
       endif
       if ( .not.( allocated( Em2 ) ) ) then
          allocate( Em2(6,-2:LI+3,-2:LJ+3,-2:LK+3) )
       endif
       do k=1, LK
          do j=1, LJ
             do i=1, LI
                read(lun,*) xg(1:3), Em2(1:3,i,j,k)
             enddo
          enddo
       enddo
       close(lun)
       
       !  -- [2-3] EBf allocation                        --  !
       if ( .not.( allocated( EBf ) ) ) then
          allocate( EBf(6,-2:LI+3,-2:LJ+3,-2:LK+3) )
       endif
       
    else
       write(6,*)
       write(6,*)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*) "[load__twEigenMode] cannot find twEigenFile [ERROR]  :: ", trim(twEigenFile1)
       write(6,*) "[load__twEigenMode] cannot find twEigenFile [ERROR]  :: ", trim(twEigenFile2)
       write(6,*) "-----------------------------------------------------------------"
       write(6,*)
       write(6,*)
       stop
    endif

    ! ------------------------------------------------------ !
    ! --- [4] set flags                                  --- !
    ! ------------------------------------------------------ !
    flag__EField          = .false.
    flag__BoundaryMessage = .false.
    
    ! ------------------------------------------------------ !
    ! --- [3] post process                               --- !
    ! ------------------------------------------------------ !
    write(6,"(a)",advance="yes") "[Done]"
    write(6,*)
    
    return
  end subroutine load__twEigenMode


  ! ====================================================== !
  ! === save__particles in a file                      === !
  ! ====================================================== !
  subroutine save__particles( action )
    use variablesMod
    implicit none
    character(5)    , intent(in)  :: action
    integer                       :: ipt, ibuff
    character(6)                  :: cpt
    character(cLen)               :: trackFile
    integer         , save        :: buff_count
    
    ! ------------------------------------------------------ !
    ! --- [1] initialization                             --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."initi" ) then
       write(6,"(a)",advance="no") "[save__particles] action :: Initialization of track files... "
       !  -- [1-1] file initialization                   --  !
       do ipt=1, npt
          write(cpt,"(i6.6)") ipt
          trackFile = trim( trackFileBase ) // cpt // ".dat"
          open( lun,file=trim(trackFile),form="formatted",status="replace" )
          write(lun,"(a)") "# time xp yp zp vx vy vz xo yo zo ux uy uz wt"
          close(lun)
       enddo
       !  -- [1-2] buff initialization                   --  !
       allocate( pxvbuff(14,npt,buffLength), nbuff_save(npt) )
       pxvbuff(:,:,:) = 0.d0
       nbuff_save(:)  = 0
       buff_count     = 0
       !  -- [1-3] time schedule settings                --  !
       t_nextSave     = t_trackStart
       write(6,"(a)",advance="yes") "[Done]"
    endif

    ! ------------------------------------------------------ !
    ! --- [2] store in buffer                            --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."store" ) then
       buff_count = buff_count + 1
       do ipt=1, npt
          if ( pxv(wt_,ipt).gt.0.d0 ) then
             pxvbuff(   1,ipt,buff_count) = ptime
             pxvbuff(2:14,ipt,buff_count) = pxv(1:13,ipt)
             nbuff_save(ipt)              = nbuff_save(ipt) + 1
          endif
       enddo
       ! -- cyclic coordinate output -- !
       if ( flag__cyclicCoordinate ) then
          ! -- cyclic particle coordinate [xMin,xMax] -- !
          ! -- nothing to do -- !
       else 
          if ( trim(particleBoundary__x).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   pxvbuff(xp_+1,ipt,buff_count) = pxvbuff(xp_+1,ipt,buff_count) &
                        &                        + dble( period_counter(ipt) )*xLeng
                endif
             enddo
          endif
          if ( trim(particleBoundary__y).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   pxvbuff(yp_+1,ipt,buff_count) = pxvbuff(yp_+1,ipt,buff_count) &
                        &                        + dble( period_counter(ipt) )*yLeng
                endif
             enddo
          endif
          if ( trim(particleBoundary__z).eq."periodic" ) then
             do ipt=1, npt
                if ( pxv(wt_,ipt).gt.0.d0 ) then
                   pxvbuff(zp_+1,ipt,buff_count) = pxvbuff(zp_+1,ipt,buff_count) &
                        &                        + dble( period_counter(ipt) )*zLeng
                endif
             enddo
          endif
       endif
       ! ------------------------------ !
    endif

    ! ------------------------------------------------------ !
    ! --- [3] save (buff is full)                        --- !
    ! ------------------------------------------------------ !
    if ( ( buff_count.eq.buffLength ).or.( trim(action).eq."final" ) ) then
       !  -- [3-1] save buff contents                    --  !
       do ipt=1, npt
          if ( nbuff_save(ipt).gt.0 ) then
             write(cpt,"(i6.6)") ipt
             trackFile = trim( trackFileBase ) // cpt // ".dat"
             open( lun,file=trim(trackFile),form="formatted",position="append" )
             do ibuff=1, nbuff_save(ipt)
                write(lun,"(14(e15.8,1x))") pxvbuff(:,ipt,ibuff)
             enddo
             close(lun)
          endif
       enddo
       !  -- [3-2] buff counter reset                    --  !
       buff_count       = 0
       nbuff_save(:)    = 0
    end if

    ! ------------------------------------------------------ !
    ! --- [4] update next schedule                       --- !
    ! ------------------------------------------------------ !
    if ( trim(action).eq."store" ) then
       if ( ptime.gt.t_trackEnd ) then
          t_nextSave = t_nextSave + 2.0d0*( t_simuEnd - t_simuStart )
       else
          t_nextSave = t_nextSave + t_trackStep
       endif
    endif
    
    return
  end subroutine save__particles

end module ioUtilityMod






    ! read(lun,char_fmt) cmt, cmt, particleFile
    ! read(lun,char_fmt) cmt, cmt, EFieldFile
    ! read(lun,char_fmt) cmt, cmt, BFieldFile
    ! read(lun,char_fmt) cmt, cmt, twEigenFile1
    ! read(lun,char_fmt) cmt, cmt, twEigenFile2
    ! read(lun,char_fmt) cmt, cmt, type__EFieldFile
    ! read(lun,char_fmt) cmt, cmt, type__BFieldFile
    
    ! read(lun,char_fmt) cmt, cmt, trackFileBase
    ! read(lun,char_fmt) cmt, cmt, probeFileBase
    ! read(lun,char_fmt) cmt, cmt, popoutFile

    ! read(lun,*)  cmt, cmt, flag__EField
    ! read(lun,*)  cmt, cmt, flag__BField
    ! read(lun,*)  cmt, cmt, flag__cyclicCoordinate
    ! read(lun,*)  cmt, cmt, flag__travellingWave
    ! read(lun,*)  cmt, cmt, flag__saveParticle
    ! read(lun,*)  cmt, cmt, flag__probeField
    
    
    ! read(lun,char_fmt)  cmt, cmt, FieldBoundary__x
    ! read(lun,char_fmt)  cmt, cmt, FieldBoundary__y
    ! read(lun,char_fmt)  cmt, cmt, FieldBoundary__z
    
    ! read(lun,char_fmt)  cmt, cmt, particleBoundary__x
    ! read(lun,char_fmt)  cmt, cmt, particleBoundary__y
    ! read(lun,char_fmt)  cmt, cmt, particleBoundary__z

    ! read(lun,*)  cmt, cmt, LI
    ! read(lun,*)  cmt, cmt, LJ
    ! read(lun,*)  cmt, cmt, LK

    ! read(lun,*)  cmt, cmt, xMin
    ! read(lun,*)  cmt, cmt, xMax
    ! read(lun,*)  cmt, cmt, yMin
    ! read(lun,*)  cmt, cmt, yMax
    ! read(lun,*)  cmt, cmt, zMin
    ! read(lun,*)  cmt, cmt, zMax

    ! read(lun,char_fmt) cmt, cmt, type__iterMax
    ! read(lun,char_fmt) cmt, cmt, type__dt

    ! read(lun,*)  cmt, cmt, iterMax
    ! read(lun,*)  cmt, cmt, dt
    ! read(lun,*)  cmt, cmt, alpha_wci
    ! read(lun,*)  cmt, cmt, alpha_CFL
    ! read(lun,*)  cmt, cmt, freq
    

    ! read(lun,*)  cmt, cmt, t_simuStart
    ! read(lun,*)  cmt, cmt, t_simuEnd
    ! read(lun,*)  cmt, cmt, t_trackStart
    ! read(lun,*)  cmt, cmt, t_trackStep
    ! read(lun,*)  cmt, cmt, t_trackEnd

    ! read(lun,*)  cmt, cmt, t_probeStart
    ! read(lun,*)  cmt, cmt, t_probeStep
    ! read(lun,*)  cmt, cmt, t_probeEnd

