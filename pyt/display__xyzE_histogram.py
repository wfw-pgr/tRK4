import sys
import numpy                    as np
import nkUtilities.load__config as lcf
import nkUtilities.plot1D       as pl1


# ========================================================= #
# ===  display__xyzE_histogram.py                       === #
# ========================================================= #

def display__xyzE_histogram():

    bins       = 100
    range_x    = None
    range_y    = None
    range_z    = None
    range_e    = None
    
    # x_ , y_ , z_  = 1, 2, 3
    # vx_, vy_, vz_ = 4, 5, 6
    MeV           = 1.e+6
    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5

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
    beta   = np.sqrt( Data[:,vx_]**2 + Data[:,vy_]**2 + Data[:,vz_]**2 ) / const["cv"]
    gamma  = 1.0 / ( np.sqrt( 1.0 - beta**2 ) )
    energy = ( gamma - 1.0 ) * const["mp"] * const["cv"]**2 / np.abs( const["qe"] ) / MeV

    # ------------------------------------------------- #
    # --- [2] calculation of histgram               --- #
    # ------------------------------------------------- #
    hist_x,bound_x = np.histogram(   Data[:,x_], bins=bins, range=range_x )
    hist_y,bound_y = np.histogram(   Data[:,y_], bins=bins, range=range_y )
    hist_z,bound_z = np.histogram(   Data[:,z_], bins=bins, range=range_z )
    hist_e,bound_e = np.histogram( energy      , bins=bins, range=range_e )
    bound_x        = 0.5*( bound_x[:1] + bound_x[1:] )
    bound_y        = 0.5*( bound_y[:1] + bound_y[1:] )
    bound_z        = 0.5*( bound_z[:1] + bound_z[1:] )
    bound_e        = 0.5*( bound_e[:1] + bound_e[1:] )
    
    # ------------------------------------------------- #
    # --- [3] ploting                               --- #
    # ------------------------------------------------- #
    #  -- [3-1]  settings                           --  #
    pngFile    = "png/histogram_{0}.png"
    config     = lcf.load__config()
    #  -- [3-2]  xAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "x" ), config=config )
    fig.add__bar( xAxis=bound_x, yAxis=hist_x, width=0.6 )
    fig.set__axis()
    fig.save__figure()
    #  -- [3-3]  yAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "y" ), config=config )
    fig.add__bar( xAxis=bound_y, yAxis=hist_y, width=0.6 )
    fig.set__axis()
    fig.save__figure()
    #  -- [3-4]  zAxis                              --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "z" ), config=config )
    fig.add__bar( xAxis=bound_z, yAxis=hist_z, width=0.6 )
    fig.set__axis()
    fig.save__figure()
    #  -- [3-5]  Energy                             --  #
    fig = pl1.plot1D( pngFile=pngFile.format( "e" ), config=config )
    fig.add__bar( xAxis=bound_e, yAxis=hist_e, width=0.6 )
    fig.set__axis()
    fig.save__figure()
    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    display__xyzE_histogram()
