import sys
import numpy                       as np
import nkUtilities.load__config    as lcf
import nkUtilities.load__constants as lcn
import nkUtilities.configSettings  as cfs
import nkUtilities.load__pointFile as lpf
import nkUtilities.plot1D          as pl1
import matplotlib.cm               as cm


# ========================================================= #
# ===  display__beamline                                === #
# ========================================================= #
def display__beamline( color="blue", colormode="energy", eRange=None ):

    eRange = [15e6,30e6]
    
    # ------------------------------------------------- #
    # --- [1] Preparation                           --- #
    # ------------------------------------------------- #
    #  -- [1-1] input / output                      --  #
    x_, y_, z_  = 1, 2, 3
    vx_,vy_,vz_ = 4, 5, 6
    config      = lcf.load__config()
    cnsFile     = "dat/parameter.conf"
    const       = lcn.load__constants( inpFile=cnsFile )
    pngFile     = "png/beamline.png"
    inpFile     = "prb/probe{0:06}.dat"

    #  -- [1-2] load__selected                      --  #
    import select__probeFile as spf
    FileList    = spf.load__selected()
    nFile       = ( FileList.shape[0] )

    #  -- [1-3] eRange                              --  #
    if ( eRange is not None ):
        eMin, eMax = eRange[0], eRange[1]
        AutoRange  = False
    else:
        eMin, eMax = 1.e64, 0.0
        AutoRange  = True
        
    # ------------------------------------------------- #
    # --- [2] calculate energy                      --- #
    # ------------------------------------------------- #
    if   ( colormode == "single" ):
        colors  = np.repeat( color, nFile )

    elif ( colormode == "random" ):
        import matplotlib.pyplot as plt
        colors  = plt.get_cmap( "jet" )
        colors  = colors( np.linspace( 0.0,1.0, nFile ) )

    elif ( colormode == "energy" ):

        energy_list=[]
        for ik,num in enumerate( FileList ):
            #  -- load     --  #
            Data   = lpf.load__pointFile( inpFile=inpFile.format( num ) )
            #  -- energy   --  #
            beta   = np.sqrt( Data[:,vx_]**2 + Data[:,vy_]**2 + Data[:,vz_]**2 ) / const["cv"]
            gamma  = 1.0 / ( np.sqrt( 1.0 - beta**2 ) )
            energy = ( gamma - 1.0 ) * const["mp"] * const["cv"]**2 / np.abs( const["qe"] )
            #  -- eMax     --  #
            if ( AutoRange ):
                eMin = np.min( [eMin,np.min( energy )] )
                eMax = np.max( [eMax,np.max( energy )] )
                
            #  -- packing  --  #
            energy_list.append( energy )
        print( "[display__beamline]  ( eMin, eMax ) = ( {0}, {1} ) ".format( eMin, eMax ) )
        colors  = [ (henergy-eMin)/(eMax-eMin) for henergy in energy_list ]
        
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    config["FigSize"]        = (8.0,2.0)
    config["plt_position"]   = [0.16,0.24,0.94,0.94]
    config["xTitle"]         = "Z (m)"
    config["yTitle"]         = "R (m)"
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [-0.5,+3.0]
    config["plt_yRange"]     = [-0.000,+0.020]
    config["plt_linewidth"]  = 0.2
    config["xMajor_Nticks"]  = 8
    config["yMajor_Nticks"]  = 3
    
    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig       = pl1.plot1D( config=config, pngFile=pngFile )
    for ik, num in enumerate( FileList ):
        Data  = lpf.load__pointFile( inpFile=inpFile.format( num ) )
        if ( colormode in ["energy"] ):
            fig.add__colorline( xAxis=Data[:,z_], yAxis=Data[:,x_], color=colors[ik], cmap="jet" )
        else:
            fig.add__plot     ( xAxis=Data[:,z_], yAxis=Data[:,x_], color=colors[ik] )
    fig.set__axis()
    fig.save__figure()

    
# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__beamline()
