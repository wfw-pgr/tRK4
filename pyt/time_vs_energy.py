import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__constants as lcn
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import nkUtilities.configSettings  as cfs
import matplotlib.cm               as cm


# ========================================================= #
# ===  draw time evolution of particle Energy           === #
# ========================================================= #

def time_vs_energy( nums=None ):

    t_            =  0
    x_ , y_ , z_  =  1,  2,  3
    vx_, vy_, vz_ =  4,  5,  6
    ex_, ey_, ez_ =  7,  8,  9
    bx_, by_, bz_ = 10, 11, 12
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( nums is None ):
        print( "[time_vs_energy] please input particle number : ( e.g. :: 1 2 3 ) >>> ", end="" )
        nums = input()
        if ( len( nums ) == 0 ):
            import glob
            files = glob.glob( "prb/probe*.dat" )
            nums  = [ int(num+1) for num in range( len(files) ) ]
        else:
            nums = [ int(num) for num in nums.split() ]
        
    pngFile = "png/time_vs_energy.png"
    config  = lcf.load__config()
    cnsFile = "dat/parameter.conf"
    const   = lcn.load__constants( inpFile=cnsFile )

    # ------------------------------------------------- #
    # --- [2] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "Time (s)"
    config["yTitle"]         = "Energy (MeV)"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5
    config["plt_marker"]     = None
    
    import nkUtilities.generate__colors as col
    colors = col.generate__colors( nColors=len(nums) )
    
    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig    = pl1.plot1D( config=config, pngFile=pngFile )
    for ik,num in enumerate( nums ):
        inpFile = "prb/probe{0:06}.dat".format( num )
        Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
        xAxis   = Data[:,t_]
        beta    = np.sqrt( Data[:,vx_]**2 + Data[:,vy_]**2 + Data[:,vz_]**2 ) / const["cv"]
        gamma   = 1.0 / ( np.sqrt( 1.0 - beta**2 ) )
        yAxis   = ( gamma - 1.0 ) * const["mp"] * const["cv"]**2 / const["qe"] / 1.e6
        
        fig.add__plot( xAxis=xAxis, yAxis=yAxis, color=colors[ik] )
        
    fig.set__axis()
    fig.save__figure()
    
    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    import nkUtilities.genArgs as gar
    args = gar.genArgs()
    nums = args["array"]
    time_vs_energy( nums=nums )
