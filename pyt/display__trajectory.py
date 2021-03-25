import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import nkUtilities.configSettings  as cfs
import matplotlib.cm               as cm


# ========================================================= #
# ===  display__trajectory                              === #
# ========================================================= #

def display__trajectory( nums=None, plain=None ):
    
    x_, y_, z_ = 1, 2, 3
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( nums is None  ):
        import select__particles as spf
        nums  = spf.load__selected()
    
    if ( plain is None ):
        print( "[trajectory__tx] please input plain (xy/yx/yz/zy/zx/xz/) ( e.g. :: xy ) >> ", \
               end="" )
        plain = input()
        
    if ( not( plain.lower() in ["xy","yz","zx","yx","zy","xz"] ) ):
        print( "[trajectory__tx] plain != (xy/yx/yz/zy/zx/xz/)  [ERROR] " )
        sys.exit()

    pngFile = "png/trajectory__{0}.png".format( plain )
    config  = lcf.load__config()    

    # ------------------------------------------------- #
    # --- [2] axis settings                         --- #
    # ------------------------------------------------- #
    if   ( plain.lower() == "xy" ):
        a1_, a2_      = x_, y_
        xTitle,yTitle = "X (m)", "Y (m)"
    elif ( plain.lower() == "yx" ):
        a1_, a2_      = y_, x_
        xTitle,yTitle = "Y (m)", "X (m)"
    elif ( plain.lower() == "yz" ):
        a1_, a2_      = y_, z_
        xTitle,yTitle = "Y (m)", "Z (m)"
    elif ( plain.lower() == "zy" ):
        a1_, a2_      = z_, y_
        xTitle,yTitle = "Z (m)", "Y (m)"
    elif ( plain.lower() == "zx" ):
        a1_, a2_      = z_, x_
        xTitle,yTitle = "Z (m)", "X (m)"
    elif ( plain.lower() == "xz" ):
        a1_, a2_      = x_, z_
        xTitle,yTitle = "X (m)", "Z (m)"

    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = xTitle
    config["yTitle"]         = yTitle
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5

    import nkUtilities.generate__colors as col
    colors = col.generate__colors( nColors=len(nums) )
    
    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig    = pl1.plot1D( config=config, pngFile=pngFile )
    for ik,num in enumerate( nums ):
        inpFile = "prb/probe{0:06}.dat".format( num )
        Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
        xAxis   = Data[:,a1_]
        yAxis   = Data[:,a2_]
        fig.add__plot( xAxis=xAxis, yAxis=yAxis, color=colors[ik] )
    fig.set__axis()
    fig.save__figure()


    
# ========================================================= #
# ===  display__trajectory3d                            === #
# ========================================================= #

def display__trajectory3d():

    x_,y_,z_ = 1, 2, 3
    
    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    
    import matplotlib.pyplot  as plt
    from mpl_toolkits.mplot3d import Axes3D

    import select__particles as sel
    selected  = sel.load__selected()
    npt       = selected.shape[0]

    inpFile   = "prb/probe{0:06}.dat"
    pngFile   = "png/trajectory3d.png"

    # ------------------------------------------------- #
    # --- [2] plot 3d graph                         --- #
    # ------------------------------------------------- #

    fig = plt.figure( figsize=(8,6) )
    ax  = fig.add_subplot( 111, projection="3d" )

    for ik,ipt in enumerate( selected ):
        # -- load data -- #
        Data = lpf.load__pointFile( inpFile=inpFile.format( ipt ), returnType="point" )
        # -- plot data -- #
        ax.plot( Data[:,x_], Data[:,y_], Data[:,z_] )

    fig.savefig( pngFile )
    print( "[display__trajectory3d.py] output :: {0} ".format( pngFile ) )
    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    import nkUtilities.genArgs as gar
    args  = gar.genArgs()
    nums  = args["array"]
    plain = args["key"]
    display__trajectory  ( nums=nums, plain=plain )
    display__trajectory3d()
