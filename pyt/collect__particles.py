import sys
import numpy as np

# ========================================================= #
# ===  collect__particles.py                            === #
# ========================================================= #


def collect__particles():

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
# ===  plot particle position                           === #
# ========================================================= #

def plot__particle_position():

    x_ , y_ , z_  = 1, 2, 3
    vx_, vy_, vz_ = 3, 4, 5
    
    import nkUtilities.load__config as lcf
    import nkUtilities.plot1D as pl1

    inpFile = "prb/collected.dat"
    with open( inpFile, "r" ) as f:
        Data = np.loadtxt( f )
    xAxis = Data[:,x_]
    yAxis = Data[:,y_]
    zAxis = Data[:,z_]
    vz    = Data[:,vz_]
    print( xAxis.shape, zAxis.shape )

    config = lcf.load__config()
    config["plt_linewidth"] = 0.0
    config["plt_marker"]    = "o"

    import matplotlib.pyplot as plt
    pngFile = "png/x_vs_z.png"

    plt.hist( zAxis, bins=50 )
    plt.savefig( pngFile )
    
    # fig = pl1.plot1D( config=config, pngFile=pngFile )
    # fig.add__plot( xAxis=zAxis, yAxis=vz )
    # fig.add__legend()
    # fig.set__axis()
    # fig.save__figure()




# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    collect__particles()
    plot__particle_position()
