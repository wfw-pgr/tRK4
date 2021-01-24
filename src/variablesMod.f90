module variablesMod
  implicit none
  integer         , parameter   :: cLen       = 300
  integer         , parameter   :: lun        = 50
  integer         , parameter   :: buffLength = 10
  character(cLen) , parameter   :: configFile = "dat/input.conf"
  double precision, parameter   :: Mp         = 1.6726219d-27
  double precision, parameter   :: qe         = 1.6021766d-19
  double precision, parameter   :: cv         = 2.9979246d+08

  integer                       :: LI, LJ, LK, npt
  integer                       :: iter, iterMax
  double precision, allocatable :: EBf(:,:,:,:)
  double precision, allocatable :: pxv(:,:)

  integer         , allocatable :: nbuff_save(:)
  double precision, allocatable :: pxvbuff(:,:,:)

  
  character(cLen)               :: EFieldFile, type__EFieldFile
  character(cLen)               :: BFieldFile, type__BFieldFile
  character(cLen)               :: particleFile
  character(cLen)               :: trackFileBase
  character(cLen)               :: popoutFile

  logical                       :: flag__EField
  logical                       :: flag__BField

  double precision              :: qm
  double precision              :: dt, dx, dy, dz, dxInv, dyInv, dzInv
  double precision              :: xMin, xMax, yMin, yMax, zMin, zMax
  double precision              :: xLeng, yLeng, zLeng

  character(cLen)               :: type__dt, type__iterMax
  double precision              :: alpha_wci, alpha_CFL
  double precision              :: ptime
  double precision              :: t_simuStart, t_simuEnd
  double precision              :: t_nextSave , t_trackStart, t_trackStep, t_trackEnd
  
  integer         , parameter   :: xp_=1, yp_=2, zp_=3, vx_=4 , vy_=5 , vz_=6
  integer         , parameter   :: xo_=7, yo_=8, zo_=9, ux_=10, uy_=11, uz_=12, wt_=13
  integer         , parameter   :: ex_=1, ey_=2, ez_=3, bx_=4 , by_=5 , bz_=6
  

end module variablesMod
