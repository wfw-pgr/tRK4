import sys
import numpy                    as np
import nkUtilities.plot1D       as pl1
import nkUtilities.load__config as lcf

# ========================================================= #
# ===  display__axial_wavePropagation                   === #
# ========================================================= #
def display__axial_wavePropagation( inpFile=None ):

    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile    = "dat/parameter.conf"
    pngFile    = "png/axial_wavePropagation.png"
    config     = lcf.load__config()
    const      = lcn.load__constants( inpFile=cnsFile )

    import nkUtilities.load__pointFile as lpf
    wave1      = lpf.load__pointFile( inpFile=const["tw_cosEigenFile"], returnType="point" )
    wave2      = lpf.load__pointFile( inpFile=const["tw_sinEigenFile"], returnType="point" )

    val        = 0.0
    eps        = 1.e-10
    index1     = np.where( np.abs( wave1[:,x_]-val ) <= eps )
    index2     = np.where( np.abs( wave2[:,x_]-val ) <= eps )
    wave1      = wave1[index1]
    wave2      = wave2[index2]
    zAxis      = wave1[:, z_]
    ez1        = wave1[:,vz_]
    ez2        = wave2[:,vz_]

    # ------------------------------------------------- #
    # --- [2] prepare cos & sin theta               --- #
    # ------------------------------------------------- #
    flag__easyplot = True
    if ( flag__easyplot ):
        const["tw_timeStart"] = 0.0
        const["tw_nCycle"]    = 1
        const["tw_nTime"]     = 12
    
    t_period  = 1.0 / ( const["tw_frequency"] )
    tStart    = const["tw_timeStart"]
    tEnd      = const["tw_timeStart"] + t_period * const["tw_nCycle"]
    time      = np.linspace( tStart, tEnd, const["tw_nTime"] )
    theta     = 2.0*np.pi*const["tw_frequency"]*time + const["tw_phase"]
    costh     = + np.cos( theta )
    sinth     = + np.sin( theta )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    config["FigSize"]        = (8,4)
    config["cmp_position"]   = [0.16,0.12,0.97,0.88]
    config["xTitle"]         = "Z (m)"
    config["yTitle"]         = "Ez (V/m)"
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [0.0,0.15]
    config["plt_yRange"]     = [-6.e+6,6.e+6]
    config["xMajor_Nticks"]  = 6
    
    # ------------------------------------------------- #
    # --- [4] plot Figure ( bfield )                --- #
    # ------------------------------------------------- #

    fig    = pl1.plot1D( config=config, pngFile=pngFile )
    import matplotlib.cm
    colors = matplotlib.cm.jet( np.linspace(0,1,const["tw_nTime"]) )
    for ik in range( const["tw_nTime"] ):
        Ez     = ez1*costh[ik] + ez2*sinth[ik]
        fig.add__plot( xAxis=zAxis, yAxis=Ez, label="t={0:.3}".format( time[ik]/1e-9 ), color=colors[ik] )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()



# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__axial_wavePropagation()

