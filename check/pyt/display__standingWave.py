import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display__initialAxisymmField                     === #
# ========================================================= #


def display__initialAxisymmField():

    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile  = "dat/parameter.conf"
    const    = lcn.load__constants( inpFile=cnsFile )

    import nkUtilities.load__pointFile as lpf
    efield   = lpf.load__pointFile( inpFile=const["EFieldFile"], returnType="structured" )
    bfield   = lpf.load__pointFile( inpFile=const["BFieldFile"], returnType="structured" )

    config   = lcf.load__config()
    pngFile  = "png/field_init_{0}.png"
    

    # ------------------------------------------------- #
    # --- [2] prepare cos & sin theta               --- #
    # ------------------------------------------------- #
    time      = np.linspace( const["tw_tStart"], const["tw_tEnd"], const["tw_tDiv"] )
    theta     = 2.0*np.pi*const["freq"] * time + const["phase_delay"]
    costh     = np.cos( theta )
    
    # ------------------------------------------------- #
    # --- [2] convert into vts File                 --- #
    # ------------------------------------------------- #

    import nkVTKRoutines.convert__vtkStructuredGrid as vts
    import nkUtilities.save__pointFile              as spf
    vtsFile   = "png/wave{0:04}.vts"
    Data      = np.zeros( (efield.shape[0],efield.shape[1],efield.shape[2],9) )
    
    # --  [2-2] Main Loop                           --  #
    for ik in range( const["tw_tDiv"] ):
        # --  [3-3] wave data synthesize            --  #
        wave          = np.zeros_like( Data )
        wave[...,0:3] = np.copy( efield[...,0:3] )
        wave[...,3:6] = efield[...,3:]*costh[ik]
        wave[...,6:9] = bfield[...,3:]*costh[ik]
        # --  [3-4] save as vts file                --  #
        vts.convert__vtkStructuredGrid( Data=wave, outFile=vtsFile.format(ik) )

        

# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__initialAxisymmField()
