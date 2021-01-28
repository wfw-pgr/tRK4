import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import nkUtilities.configSettings  as cfs
import matplotlib.cm               as cm


# ========================================================= #
# ===  draw trajectory__tx                              === #
# ========================================================= #

def trajectory__tx( nums=None, axis=None ):

    t_         = 0
    x_, y_, z_ = 1, 2, 3
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( nums is None ):
        print( "[trajectory__tx] please input particle number >> ( e.g. :: 1 2 3 )" )
        nums = input()
        nums = [ int(num) for num in nums.split() ]

    if ( axis is None ):
        print( "[trajectory__tx] please input axis ( x/y/z )  >> ( e.g. :: x     )" )
        axis = input()
        
    if ( not( axis.lower() in ["x","y","z"] ) ):
        print( "[trajectory__tx] axis != x/y/z   [ERROR] " )
        sys.exit()

    pngFile = "png/trajectory__time_{0}.png".format( axis )
    config  = lcf.load__config()

    # ------------------------------------------------- #
    # --- [2] axis settings                         --- #
    # ------------------------------------------------- #
    if   ( axis.lower() == "x" ):
        p_     = x_
        yTitle = "X (m)"
    elif ( axis.lower() == "y" ):
        p_     = y_
        yTitle = "Y (m)"
    elif ( axis.lower() == "z" ):
        p_     = z_
        yTitle = "Z (m)"

    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "Time (s)"
    config["yTitle"]         = yTitle
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5
    config["plt_marker"]     = "o"
    
    import nkUtilities.generate__colors as col
    colors = col.generate__colors( nColors=len(nums) )

    
    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig    = pl1.plot1D( config=config, pngFile=pngFile )
    for ik,num in enumerate( nums ):
        inpFile = "trk/track{0:06}.dat".format( num )
        Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
        xAxis   = Data[:,t_]
        yAxis   = Data[:,p_]
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
    trajectory__tx( nums=nums, axis=args["key"] )
