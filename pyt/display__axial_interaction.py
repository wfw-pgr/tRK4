import sys
import numpy                    as np
import nkUtilities.plot1D       as pl1
import nkUtilities.load__config as lcf

# ========================================================= #
# ===  display__axial_interaction                       === #
# ========================================================= #
def display__axial_interaction( time=0.0, nRepeat=1 ):

    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5

    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile    = "dat/parameter.conf"
    pngFile    = "png/axial/axial_interaction_{0}.png"
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

    ez1Stack   = np.copy( ez1   )
    ez2Stack   = np.copy( ez2   )
    zStack     = np.copy( zAxis )
    zLeng      = np.max( zAxis ) - np.min( zAxis )
    for ik in range( 1, nRepeat ):
        ez1_     = ( np.copy( ez1 ) ) [1:]
        ez2_     = ( np.copy( ez2 ) ) [1:]
        zAxis_   = ( np.copy( zAxis ) + zLeng * float( ik ) )[1:]
        ez1Stack = np.concatenate( [ez1Stack,  ez1_] )
        ez2Stack = np.concatenate( [ez2Stack,  ez2_] )
        zStack   = np.concatenate( [  zStack,zAxis_] )
    zAxis = zStack
    ez1   = ez1Stack
    ez2   = ez2Stack
    
    # ------------------------------------------------- #
    # --- [2] prepare cos & sin theta               --- #
    # ------------------------------------------------- #
    theta      = 2.0*np.pi*const["tw_frequency"]*time + const["tw_phase"]
    costh      = + np.cos( theta )
    sinth      = + np.sin( theta )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    config["FigSize"]        = (10,3)
    config["cmp_position"]   = [0.12,0.16,0.97,0.92]
    config["xTitle"]         = "Z (m)"
    config["yTitle"]         = "Ez (V/m)"
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [0.0,1.40]
    config["plt_yRange"]     = [-6.e+6,6.e+6]
    config["xMajor_Nticks"]  = 6

    # ------------------------------------------------- #
    # --- [4] collect particles                     --- #
    # ------------------------------------------------- #
    import collect__particles as clp
    ret    = clp.collect__particles( target_time=time )
    npt    = ret.shape[0]
    height = 3.e6
    ppos   = np.linspace( -height, +height, npt )
    zpos   = ret[:,3]
    
    # ------------------------------------------------- #
    # --- [4] plot Figure ( bfield )                --- #
    # ------------------------------------------------- #
    stime  = "t{0:.3f}ns".format( time / 1e-9 )
    Ez     = ez1*costh + ez2*sinth
    fig    = pl1.plot1D( config=config, pngFile=pngFile.format( stime ) )
    fig.add__plot( xAxis=zAxis, yAxis=Ez   )
    fig.add__plot( xAxis=zpos , yAxis=ppos, marker="o" )
    fig.set__axis()
    fig.save__figure()



# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    tMin    = 0.0
    tMax    = 0.35e-9
    nTime   = 51
    nRepeat = 2
    time  = np.linspace( tMin, tMax, nTime )
    for it, htime in enumerate( time ):
        display__axial_interaction( time=htime )

