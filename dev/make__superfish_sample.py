import numpy as np


# ========================================================= #
# ===  make__superfish_sample.py                        === #
# ========================================================= #

def make__superfish_sample():

    # ------------------------------------------------- #
    # --- [1] load constants                        --- #
    # ------------------------------------------------- #
    LI, LJ, LK = 21, 21, 1
    import nkUtilities.equiSpaceGrid as esg
    x1MinMaxNum = [ 0.0, 1.0, LI ]
    x2MinMaxNum = [ 0.0, 1.0, LJ ]
    x3MinMaxNum = [ 0.0, 0.0, LK ]
    ret         = esg.equiSpaceGrid( x1MinMaxNum=x1MinMaxNum, x2MinMaxNum=x2MinMaxNum, \
                                     x3MinMaxNum=x3MinMaxNum, returnType = "point" )
    Data        = np.zeros( (ret.shape[0],7) )
    names       = ["xp","yp","zp","Ez","Er","|E|","Hp"]
    Data[:,0:3] = np.copy( ret[:,:] )
    Data[:,3]   = 1.e+6
    Data[:,4]   = 0.0
    Data[:,5]   = np.sqrt( Data[:,3]**2 + Data[:,4]**2 )
    Data[:,6]   = 0.0

    
    Data        = np.reshape( Data, (LK,LJ,LI,7) )
    outFile     = "dat/superfish_sample.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=Data )
    
    return()


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    make__superfish_sample()
