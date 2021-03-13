import sys
import numpy                    as np
import nkUtilities.load__config as lcf
import nkUtilities.plot1D       as pl1


# ========================================================= #
# ===  display__xyzE_histogram.py                       === #
# ========================================================= #

def display__xyzE_histogram():

    bins_x, bins_y, bins_z, bins_e     = 100, 100, 100, 100
    range_x, range_y, range_z, range_e = None, None, None, None
    
    pngFile       = "png/histogram_{0}.png"    
    MeV           = 1.e+6
    x_ , y_ , z_  = 1, 2, 3
    vx_, vy_, vz_ = 4, 5, 6

    # ------------------------------------------------- #
    # --- [1] load config & data                    --- #
    # ------------------------------------------------- #
    #  -- [1-1]  load config                         -- #
    cnsFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const   = lcn.load__constants( inpFile=cnsFile )
    config  = lcf.load__config()

    #  -- [1-2]  load data & energy calculation      -- #
    inpFile = "prb/collected.dat"
    with open( inpFile, "r" ) as f:
        Data = np.loadtxt( f )

    #  -- [1-3] energy calculation                   -- #
    beta   = np.sqrt( Data[:,vx_]**2 + Data[:,vy_]**2 + Data[:,vz_]**2 ) / const["cv"]
    gamma  = 1.0 / ( np.sqrt( 1.0 - beta**2 ) )
    energy = ( gamma - 1.0 ) * const["mp"] * const["cv"]**2 / np.abs( const["qe"] ) / MeV

    # ------------------------------------------------- #
    # --- [2] draw histogram                        --- #
    # ------------------------------------------------- #
    #  -- [2-1] Number of particles                 --  #
    config["yTitle"] = "Number of particles"

    #  -- [2-2] Number of particles                 --  #
    import nkUtilities.make__histogram as hst
    config["xTitle"] = "X (m)"
    hist_x, bound_x = hst.make__histogram( Data=Data[:,x_], bins=bins_x, range=range_x, \
                                           config=config, pngFile=pngFile.format( "x" ) )
    config["xTitle"] = "Y (m)"
    hist_y, bound_y = hst.make__histogram( Data=Data[:,y_], bins=bins_y, range=range_y, \
                                           config=config, pngFile=pngFile.format( "y" ) )
    config["xTitle"] = "Z (m)"
    hist_z, bound_z = hst.make__histogram( Data=Data[:,z_], bins=bins_z, range=range_z, \
                                           config=config, pngFile=pngFile.format( "z" ) )


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    display__xyzE_histogram()
