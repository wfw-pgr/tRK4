import numpy as np

# ========================================================= #
# ===  generate EField sample File                      === #
# ========================================================= #

def generate__eFieldSample():

    x_,y_,z_        = 0, 1, 2
    vx_,vy_,vz_     = 0, 1, 2
    EField_strength = 0.0
    # EField_strength = 1.0e5
    
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
    efield          = np.zeros_like( ( ret ) )
    efield[...,vx_] = EField_strength

    Data            = np.concatenate( (ret,efield), axis=3 )
    
    # ------------------------------------------------- #
    # --- [3] save in File                          --- #
    # ------------------------------------------------- #

    outFile   = "dat/EField.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=Data )

    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    generate__eFieldSample()
