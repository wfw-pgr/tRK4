import numpy as np

# ========================================================= #
# ===  generate BField sample File                      === #
# ========================================================= #

def generate__bFieldSample():

    x_,y_,z_    = 0, 1, 2
    vx_,vy_,vz_ = 0, 1, 2
    
    BField_strength = 0.0
    
    # ------------------------------------------------- #
    # --- [1] load config File                      --- #
    # ------------------------------------------------- #

    inpFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const = lcn.load__constants( inpFile=inpFile )

    
    # ------------------------------------------------- #
    # --- [2] grid generation                       --- #
    # ------------------------------------------------- #
    
    import nkUtilities.equiSpaceGrid as esg
    x1MinMaxNum = [ const["xMin"], const["xMax"], const["LI"] ]
    x2MinMaxNum = [ const["yMin"], const["yMax"], const["LJ"] ]
    x3MinMaxNum = [ const["zMin"], const["zMax"], const["LK"] ]
    ret         = esg.equiSpaceGrid( x1MinMaxNum=x1MinMaxNum, x2MinMaxNum=x2MinMaxNum, \
                                     x3MinMaxNum=x3MinMaxNum, returnType = "structured" )
    bfield          = np.zeros_like( ( ret ) )
    bfield[...,vy_] = BField_strength

    Data            = np.concatenate( (ret,bfield), axis=3 )
    
    # ------------------------------------------------- #
    # --- [3] save in File                          --- #
    # ------------------------------------------------- #

    outFile   = "dat/BField.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=Data )

    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    generate__bFieldSample()
