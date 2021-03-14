import numpy                    as np
import nkUtilities.load__config as lcf
import nkUtilities.plot1D       as pl1


# ========================================================= #
# ===  display__phasespace                              === #
# ========================================================= #


def display__phasespace():
    
    x_ , y_ , z_  = 1, 2, 3
    vx_, vy_, vz_ = 4, 5, 6
    MeV           = 1.e+6

    # ------------------------------------------------- #
    # --- [1] load config & data                    --- #
    # ------------------------------------------------- #
    #  -- [2-1]  load config                         -- #
    cnsFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const = lcn.load__constants( inpFile=cnsFile )
    
    #  -- [2-2]  load data & energy calculation      -- #
    inpFile = "prb/collected.dat"
    with open( inpFile, "r" ) as f:
        Data = np.loadtxt( f )
    Data = Data[ np.where( Data[:,x_] < 0.010 ) ]
    Data = Data[ np.where( Data[:,13] > 0.0   ) ]
    Data[:,vx_:vz_+1] = Data[:,vx_:vz_+1] / const["cv"]

    # ------------------------------------------------- #
    # --- [3] ploting                               --- #
    # ------------------------------------------------- #
    #  -- [3-1]  settings                           --  #
    pngFile                  = "png/phasespace_{0}.png"
    config                   = lcf.load__config()
    config["FigSize"]        = (5,5)
    config["plt_marker"]     = "o"
    config["plt_markersize"] = 0.3
    config["plt_linewidth"]  = 0.0
    
    #  -- [3-2]  xAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "x_vx" ), config=config )
    fig.add__plot( xAxis=Data[:,x_], yAxis=Data[:,vx_] )
    fig.set__axis()
    fig.save__figure()
    #  -- [3-3]  yAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "y_vy" ), config=config )
    fig.add__plot( xAxis=Data[:,y_], yAxis=Data[:,vy_] )
    fig.set__axis()
    fig.save__figure()
    #  -- [3-4]  zAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "z_vz" ), config=config )
    fig.add__plot( xAxis=Data[:,z_], yAxis=Data[:,vz_] )
    fig.set__axis()
    fig.save__figure()


    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    display__phasespace()
