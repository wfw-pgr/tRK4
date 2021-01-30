import numpy as np


# ========================================================= #
# ===  generate travelling wave                         === #
# ========================================================= #

def generate__travellingWave():

    phase    = 0.0

    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile  = "dat/parameter.conf"
    const    = lcn.load__constants( inpFile=cnsFile )

    import nkUtilities.load__pointFile as lpf
    wave1    = lpf.load__pointFile( inpFile=const["twEigenFile1"], returnType="structured" )
    wave2    = lpf.load__pointFile( inpFile=const["twEigenFile2"], returnType="structured" )


    # ------------------------------------------------- #
    # --- [2] prepare cos & sin theta               --- #
    # ------------------------------------------------- #
    time      = np.linspace( const["tw_tStart"], const["tw_tEnd"], const["tw_tDiv"] )
    theta     = 2.0*np.pi*const["freq"] * time + phase
    costh     = np.cos( theta )
    sinth     = np.sin( theta )
    
    # ------------------------------------------------- #
    # --- [3] save in File                          --- #
    # ------------------------------------------------- #
    # --  [3-1] preparation                         --  #
    import nkVTKRoutines.convert__vtkStructuredGrid as vts
    import nkUtilities.save__pointFile as spf
    vtsFile   = "png/wave{0:04}.vts"
    
    # --  [3-2] Main Loop                           --  #
    for ik in range( const["tw_tDiv"] ):
        # --  [3-3] wave data synthesize            --  #
        wave        = np.zeros_like( wave1 )
        wave[...,0:3] = wave1[...,0:3]
        wave[..., 3:] = wave1[..., 3:]*costh[ik] + wave2[..., 3:]*sinth[ik]
        # --  [3-4] save as vts file                --  #
        vts.convert__vtkStructuredGrid( Data=wave, outFile=vtsFile.format(ik) )


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    generate__travellingWave()
