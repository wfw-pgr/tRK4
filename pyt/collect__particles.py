import sys, subprocess
import numpy as np

# ========================================================= #
# ===  collect__particles.py                            === #
# ========================================================= #


def collect__particles( target_time=None ):

    # ------------------------------------------------- #
    # --- [0] preparation                           --- #
    # ------------------------------------------------- #

    if ( target_time is None ):
        print( "[coolect__particles.py] target_time ?? >>  ", end="" )
        target_time = input()

        if ( len( target_time ) == 0 ):
            sys.exit("[collect__particles.py] enter time !! ")
        else:
            target_time = float( target_time )
    
    # ------------------------------------------------- #
    # --- [1] load constants                        --- #
    # ------------------------------------------------- #

    cnsFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    gconst  = lcn.load__constants( inpFile=cnsFile )

    cnsFile = "dat/particle.conf"
    import nkUtilities.load__constants as lcn
    pconst  = lcn.load__constants( inpFile=cnsFile )
    
    # ------------------------------------------------- #
    # --- [2] collect particles                     --- #
    # ------------------------------------------------- #

    nCmp        = 14
    ret         = np.zeros( (pconst["npt"],nCmp) )
    
    for ip in range( pconst["npt"] ):
        
        prbFile   = "prb/probe{0:06}.dat".format( ip+1 )
        Data      = np.loadtxt( prbFile )
        nData     = Data.shape[0]
        nCmp      = Data.shape[1]
        timearray = np.ravel( Data[:,0] )
        idx       = np.argmin( np.abs( timearray - target_time ) )
        diff      = timearray[idx] - target_time

        if   ( diff == 0.0    ):
            ret[ip,:] = Data[idx,:]
            
        elif ( ( idx == 0     ) and ( diff >= 0.0 ) ):
            ret[ip,:] = Data[idx,:]
            
        elif ( ( idx == nData ) and ( diff <= 0.0 ) ):
            ret[ip,:] = Data[idx,:]
            
        elif ( diff > 0.0    ):
            t1, t2 = timearray[idx-1], timearray[idx  ]
            p1, p2 = ( t2-target_time ) / ( t2-t1 ), ( target_time-t1 ) / ( t2-t1 )
            ret[ip,:]    = p1*Data[idx-1,:] + p2*Data[idx,:]

        elif ( diff < 0.0    ):
            t1, t2 = timearray[idx], timearray[idx+1]
            p1, p2 = ( t2-target_time ) / ( t2-t1 ), ( target_time-t1 ) / ( t2-t1 )
            ret[ip,:]    = p1*Data[idx,:] + p2*Data[idx+1,:]


    # ------------------------------------------------- #
    # --- [3] save in file                          --- #
    # ------------------------------------------------- #
    outFile   = "prb/collected.dat"
    with open( outFile, "w" ) as f:
        np.savetxt( f, ret )
    return()



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
    # --- [2] distribution at target_time           --- #
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
    
    


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):

    # mode = "single"
    mode = "successive"
    
    # ------------------------------------------------- #
    # --- [1] select time ver.                      --- #
    # ------------------------------------------------- #

    if ( mode == "single" ):
        collect__particles()
        plot__particle_statistic()


    # ------------------------------------------------- #
    # --- [2] time given in array                   --- #
    # ------------------------------------------------- #

    if ( mode == "successive" ):
    
        tMin = 1.e-9
        tMax = 4.e-9
        nt   = 7
    
        timelist = np.linspace( tMin, tMax, nt )
        
        for ik,time in enumerate( timelist ):
            
            cmd = "mkdir -p sav/dir{0}".format( ik )
            subprocess.call( cmd.split() )
            print( "time = {0}".format( timelist[ik] ) )
            
            collect__particles( target_time=timelist[ik] )
            plot__particle_statistic( target_time=timelist[ik] )
            cmd = "mv png/* sav/dir{0}".format( ik )
            subprocess.call( cmd, shell=True )
            
