import numpy as np
import nkUtilities.load__pointFile as lpf
import nkVTKRoutines.convert__vtkPolyLine as vpl

# ========================================================= #
# ===  convert__trajectory2vtp                          === #
# ========================================================= #

def convert__trajectory2vtp():

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #

    import select__particles as sel
    selected  = sel.load__selected()
    npt       = selected.shape[0]

    inpFile   = "prb/probe{0:06}.dat"
    outFile   = "png/trajectory{0:06}.vtp"

    # ------------------------------------------------- #
    # --- [2] load trajectory                       --- #
    # ------------------------------------------------- #
    
    for ik in range( npt ):

        # -- load data -- #
        Data = lpf.load__pointFile( inpFile=inpFile.format( ik+1 ), returnType="point" )
        Data = Data[:,1:]

        # -- save data -- #
        vpl.convert__vtkPolyLine( Data=Data, outFile=outFile.format( ik+1 ) )

        
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    convert__trajectory2vtp()
