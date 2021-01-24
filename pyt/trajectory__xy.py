import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import nkUtilities.configSettings  as cfs
import matplotlib.cm               as cm


# ========================================================= #
# ===  draw trajectory__xy                              === #
# ========================================================= #

def trajectory__xy( nums=None ):

    x_, y_, z_ = 1, 2, 3
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( nums is None ): nums = [ 1 ]
    pngFile = "png/trajectory__xy.png"
    config  = lcf.load__config()

    # ------------------------------------------------- #
    # --- [2] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5

    # ------------------------------------------------- #
    # --- [3] plot Figure                           --- #
    # ------------------------------------------------- #
    fig    = pl1.plot1D( config=config, pngFile=pngFile )
    colors = [ cm.jet( ik / float( len(nums) ) ) for ik in range( len( nums ) ) ]
    for ik,num in enumerate( nums ):
        inpFile = "trk/track{0:06}.dat".format( num )
        Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
        xAxis   = Data[:,x_]
        yAxis   = Data[:,y_]
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
    trajectory__xy( nums=nums )
