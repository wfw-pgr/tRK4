import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import nkUtilities.configSettings  as cfs
import matplotlib.cm               as cm


# ========================================================= #
# ===  draw time evolution of particle                  === #
# ========================================================= #

def time_vs_probeData( nums=None, axis=None ):

    t_            =  0
    x_ , y_ , z_  =  1,  2,  3
    vx_, vy_, vz_ =  4,  5,  6
    ex_, ey_, ez_ =  7,  8,  9
    bx_, by_, bz_ = 10, 11, 12

    cnsFile = "dat/particle.conf"
    import nkUtilities.load__constants as lcn
    pconst  = lcn.load__constants( inpFile=cnsFile )
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( nums is None ):
        print( "[trajectory__tx] please input particle number : ( e.g. :: 1 2 3, 1-4, [empty::all] ) >>> ", end="" )
        nums = input()
        if   ( len(nums) == 0 ):
            nums    = list( range( 1, pconst["npt"]+1 ) )
        elif ( len( nums.split("-") ) == 2 ):
            imin = int( ( nums.split("-")[0] ).strip() )
            imax = int( ( nums.split("-")[0] ).strip() )
            nums = list( range( imin, imax+1 ) ) 
        else:
            nums = [ int(num) for num in nums.split() ]

    if ( axis is None ):
        print( "[trajectory__tx] please input axis ( x/y/z )  : ( e.g. :: x     ) >>> ", end="" )
        axis = input()
        
    if ( not( axis.lower() in ["x" ,"y" ,"z" ,"vx","vy","vz",\
                               "ex","ey","ez","bx","by","bz"] ) ):
        print( "[trajectory__tx] axis != x/y/z   [ERROR] " )
        sys.exit()

    pngFile = "png/time_vs_{0}.png".format( axis )
    config  = lcf.load__config()

    # ------------------------------------------------- #
    # --- [2] axis settings                         --- #
    # ------------------------------------------------- #
    if   ( axis.lower() == "x" ):
        p_     = x_
        yTitle = "x (m)"
    elif ( axis.lower() == "y" ):
        p_     = y_
        yTitle = "y (m)"
    elif ( axis.lower() == "z" ):
        p_     = z_
        yTitle = "z (m)"
    elif ( axis.lower() == "vx" ):
        p_     = vx_
        yTitle = "vx (m/s)"
    elif ( axis.lower() == "vy" ):
        p_     = vy_
        yTitle = "vy (m/s)"
    elif ( axis.lower() == "vz" ):
        p_     = vz_
        yTitle = "vz (m/s)"
    elif ( axis.lower() == "ex" ):
        p_     = ex_
        yTitle = "ex (V/m)"
    elif ( axis.lower() == "ey" ):
        p_     = ey_
        yTitle = "ey (V/m)"
    elif ( axis.lower() == "ez" ):
        p_     = ez_
        yTitle = "ez (V/m)"
    elif ( axis.lower() == "bx" ):
        p_     = bx_
        yTitle = "bx (T)"
    elif ( axis.lower() == "by" ):
        p_     = by_
        yTitle = "by (T)"
    elif ( axis.lower() == "bz" ):
        p_     = bz_
        yTitle = "bz (T)"

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
        yAxis   = Data[:,p_]
        fig.add__plot( xAxis=xAxis, yAxis=yAxis, color=colors[ik] )
    fig.set__axis()
    fig.save__figure()
    
    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    import nkUtilities.genArgs         as gar
    import nkUtilities.load__constants as lcn
    cnsFile = "dat/particle.conf"
    pconst  = lcn.load__constants( inpFile=cnsFile )
    args    = gar.genArgs()

    # ------------------------------------------------- #
    # --- [2] mode selection                        --- #
    # ------------------------------------------------- #
    print( "[time_vs_probeData.py] default :: plot all data ?? ( y/n ) >>> ", end="" )
    yorn = input()
    if ( ( yorn == "y" ) or ( len(yorn)==0 ) ):
        nums    = list( range( 1, pconst["npt"]+1 ) )
        axis    = "all"
    else:
        nums = args["array"]
        axis = args["key"]

    # ------------------------------------------------- #
    # --- [3] plot                                  --- #
    # ------------------------------------------------- #
    if ( axis == "all" ):
        for ax in ["x","y","z","vx","vy","vz","ex","ey","ez","bx","by","bz"]:
            time_vs_probeData( nums=nums, axis=ax )
    else:
        time_vs_probeData( nums=nums, axis=axis )
