# ========================================================= #
# ===  plot particle statistic                          === #
# ========================================================= #

def plot__particle_statistic():

    x_ , y_ , z_  = 1, 2, 3
    vx_, vy_, vz_ = 4, 5, 6
    MeV           = 1.e+6

    # ------------------------------------------------- #
    # --- [1] load config & data                    --- #
    # ------------------------------------------------- #
    
    cnsFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const = lcn.load__constants( inpFile=cnsFile )

    inpFile = "prb/collected.dat"
    with open( inpFile, "r" ) as f:
        Data = np.loadtxt( f )
    xpos   = Data[:,x_]
    ypos   = Data[:,y_]
    zpos   = Data[:,z_]
    vx     = Data[:,vx_]
    vy     = Data[:,vy_]
    vz     = Data[:,vz_]
    beta   = np.sqrt( vx**2 + vy**2 + vz**2 ) / const["cv"]
    gamma  = 1.0 / ( np.sqrt( 1.0 - beta**2 ) )
    energy = ( gamma - 1.0 ) * const["mp"] * const["cv"]**2 / const["qe"] / MeV


    # ------------------------------------------------- #
    # --- [2] distribution at target           --- #
    # ------------------------------------------------- #

    pngFile = "png/p_pos_histogram_{0}.png"

    #  -- [2-1]  xpos  -- #
    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax  = fig.add_subplot()
    ax.hist( xpos, bins=300 )
    fig.savefig( pngFile.format("x") )
    print( "[collect__particles.py] outFile :: {0} ".format( pngFile.format("x") ) )

    #  -- [2-2]  ypos  -- #
    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax  = fig.add_subplot()
    ax.hist( ypos, bins=300 )
    fig.savefig( pngFile.format("y") )
    print( "[collect__particles.py] outFile :: {0} ".format( pngFile.format("y") ) )

    #  -- [2-3]  zpos  -- #
    import matplotlib.pyplot as plt
    fig = plt.figure()
    ax  = fig.add_subplot()
    ax.hist( zpos, bins=300 )
    fig.savefig( pngFile.format("z") )
    print( "[collect__particles.py] outFile :: {0} ".format( pngFile.format("z") ) )

    
    # ------------------------------------------------- #
    # --- [3] plot particls positions               --- #
    # ------------------------------------------------- #

    #  -- [3-0]  common settings   -- #
    import nkUtilities.plot1D       as pl1
    import nkUtilities.load__config as lcf
    config = lcf.load__config()
    config["plt_linewidth"] = 0.0
    config["plt_marker"]    = "o"
    pngFile                 = "png/dist_at_time_{0}.png"

    #  -- [3-1]  xy dist  -- #
    config["xTitle"] = "x (m)"
    config["yTitle"] = "y (m)"
    fig = pl1.plot1D( xAxis=xpos, yAxis=ypos, config=config, pngFile=pngFile.format( "x" ) )

    #  -- [3-2]  zx dist  -- #
    config["xTitle"] = "z (m)"
    config["yTitle"] = "x (m)"
    fig = pl1.plot1D( xAxis=zpos, yAxis=xpos, config=config, pngFile=pngFile.format( "y" ) )

    #  -- [3-3]  zy dist  -- #
    config["xTitle"] = "z (m)"
    config["yTitle"] = "y (m)"
    fig = pl1.plot1D( xAxis=zpos, yAxis=ypos, config=config, pngFile=pngFile.format( "z" ) )


    # ------------------------------------------------- #
    # --- [4] energy dist.                          --- #
    # ------------------------------------------------- #
    #  -- [4-0]  config  settings    -- #
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [0.0,0.8]
    config["plt_yRange"]     = [0.0,0.2]
    config["xTitle"]         = "z (m)"
    config["yTitle"]         = "Energy (MeV)"
    #  -- [4-1]  plot energy dist.   -- #
    pngFile = "png/dist_at_time_z_energy.png"
    fig     = pl1.plot1D( xAxis=zpos, yAxis=energy, config=config, pngFile=pngFile )
    
    

