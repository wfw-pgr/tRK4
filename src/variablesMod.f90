module variablesMod
  implicit none

  type field
     integer                       :: LI, LJ, LK, nRepeat_x, nRepeat_y, nRepeat_z
     character(10)                 :: boundary_x, boundary_y, boundary_z
     character(3)                  :: modulation_type
     double precision              :: dx, dy, dz, dxInv, dyInv, dzInv
     double precision              :: xMin, xMax, yMin, yMax, zMin, zMax
     double precision              :: xshift, yshift, zshift
     double precision              :: modulation, amplitude_factor, frequency, phase
     double precision              :: xLeng, yLeng, zLeng, xLengInv, yLengInv, zLengInv
     double precision, allocatable :: EBf(:,:,:,:)
  end type field

  integer                       :: nBField, nEField
  type(field), allocatable      :: bfields(:), efields(:)
  
  integer         , parameter   :: cLen              = 300
  integer         , parameter   :: lun               = 50
  integer         , parameter   :: buffLength        = 100
  integer         , parameter   :: progress_interval = 100
  double precision, parameter   :: cv                = 2.9979246d+08
  double precision, parameter   :: cvSqInv           = 1.d0 / cv**2
  character(cLen) , parameter   :: configFile        = "dat/input.lst"
  
  integer                       :: LI, LJ, LK, npt
  integer                       :: iter, iterMax
  double precision, allocatable :: pxv(:,:)

  integer         , allocatable :: nbuff_save(:), probe_counter(:)
  double precision, allocatable :: probeBuff(:,:,:)

  character(cLen)               :: EFieldListFile, EFieldParamFile
  character(cLen)               :: BFieldListFile, BFieldParamFile
  character(cLen)               :: particleFile
  character(cLen)               :: probeFileBase
  character(cLen)               :: popoutFile, bpmFile
  
  logical                       :: flag__axisymmetry
  logical                       :: flag__probeField
  logical                       :: flag__popoutBoundary
  logical                       :: flag__beamposmonitor

  character(cLen)               :: particleBoundary__x, particleBoundary__y, particleBoundary__z

  double precision              :: mp, qe, qm
  double precision              :: dt, dx, dy, dz, dxInv, dyInv, dzInv
  double precision              :: xMin, xMax, yMin, yMax, zMin, zMax
  double precision              :: xLeng, yLeng, zLeng
  double precision              :: BMax

  character(1)                  :: bpm_direction
  double precision              :: bpm_screen_pos

  character(cLen)               :: type__dt, type__iterMax
  double precision              :: alpha_wci, alpha_CFL
  double precision              :: ptime
  double precision              :: t_simuStart, t_simuEnd
  double precision              :: t_nextProbe, t_probeStart, t_probeStep, t_probeEnd

  logical                       :: flag__relativisticVelocity = .false.
  logical                       :: flag__all_particle_popout  = .false.
  
  integer         , parameter   :: xp_=1, yp_=2, zp_=3, vx_=4 , vy_=5 , vz_=6
  integer         , parameter   :: xo_=7, yo_=8, zo_=9, ux_=10, uy_=11, uz_=12, wt_=13
  integer         , parameter   :: ex_=1, ey_=2, ez_=3, bx_=4 , by_=5 , bz_=6
  integer         , parameter   :: fx_=4, fy_=5, fz_=6

  

end module variablesMod


  ! double precision, allocatable :: EBf(:,:,:,:)
  ! double precision, allocatable :: Em1(:,:,:,:), Em2(:,:,:,:)
  ! integer         , allocatable :: period_counter(:)
  ! double precision, allocatable :: pxvbuff(:,:,:), probeBuff(:,:,:)
  ! character(cLen)               :: EFieldFile, type__EFieldFile
  ! character(cLen)               :: BFieldFile, type__BFieldFile
  ! character(cLen)               :: twEigenFile1, twEigenFile2
  ! character(cLen)               :: trackFileBase, probeFileBase
  ! logical                       :: flag__EField
  ! logical                       :: flag__BField
  ! logical                       :: flag__cyclicCoordinate
  ! logical                       :: flag__standingWave
  ! logical                       :: flag__travellingWave
  ! logical                       :: flag__saveParticle
  ! character(cLen)               :: FieldBoundary__x, FieldBoundary__y, FieldBoundary__z
  ! double precision              :: freq, phase_delay
  ! double precision              :: efield_factor, bfield_factor
  ! double precision              :: t_nextSave , t_trackStart, t_trackStep, t_trackEnd
  ! logical                       :: flag__BoundaryMessage      = .true.
