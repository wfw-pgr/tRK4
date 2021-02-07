import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.plot1D         as pl1
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display__particles                               === #
# ========================================================= #
def display__particles():
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config  = lcf.load__config()
    datFile = "dat/particles.dat"
    pngFile = ( datFile.replace( "dat", "png" ) ).replace( ".png", "_{0}.png" )

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data  = lpf.load__pointFile( inpFile=datFile, returnType="point" )
    xAxis = Data[:,0]
    yAxis = Data[:,1]
    zAxis = Data[:,2]
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    cfs.configSettings( configType="plot1D_mark", config=config )
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5
    config["plt_linewidth"]  = 0.0

    # ------------------------------------------------- #
    # --- [4] x-y plot Figure                       --- #
    # ------------------------------------------------- #
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    fig = pl1.plot1D( config=config, pngFile=pngFile.format( "xy" ) )
    fig.add__plot( xAxis=xAxis, yAxis=yAxis )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()

    # ------------------------------------------------- #
    # --- [5] y-z plot Figure                       --- #
    # ------------------------------------------------- #
    config["xTitle"]         = "Y (m)"
    config["yTitle"]         = "Z (m)"
    fig = pl1.plot1D( config=config, pngFile=pngFile.format( "yz" ) )
    fig.add__plot( xAxis=yAxis, yAxis=zAxis )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()
    
    # ------------------------------------------------- #
    # --- [6] z-x plot Figure                       --- #
    # ------------------------------------------------- #
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Z (m)"
    fig = pl1.plot1D( config=config, pngFile=pngFile.format( "xz" ) )
    fig.add__plot( xAxis=xAxis, yAxis=zAxis )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()

    # ------------------------------------------------- #
    # --- [7] convert particles plot into vtk       --- #
    # ------------------------------------------------- #
    print( Data.shape )
    import nkVTKRoutines.scatter__vtkPoint as sct
    sct.scatter__vtkPoint( Data=Data, vtkFile="png/particles.vtu" )
    
    # import nkVTKRoutines.convert__vtkPolyLine as vpl
    # vpl.convert__vtkPolyLine( Data=Data, outFile="png/particles.vtp" )
    

# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__particles()

