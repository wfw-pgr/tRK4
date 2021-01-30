import numpy as np


# ========================================================= #
# ===  generate__particleSample                         === #
# ========================================================= #

def generate__particleSample():


    npt         = 11
    beta        = 0.99
    cv          = 2.9979246e+08
    v0          = beta * cv

    xp_,yp_,zp_ = 0, 1, 2
    vx_,vy_,vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] particle generation                   --- #
    # ------------------------------------------------- #

    Data        = np.zeros( (npt,6) )
    Data[:,xp_] = 0.0
    Data[:,yp_] = np.linspace( -0.005, 0.005, 11 )
    Data[:,zp_] = 0.0
    Data[:,vx_] = 0.0
    Data[:,vy_] = 0.0
    Data[:,vz_] = + v0
    
    # ------------------------------------------------- #
    # --- [2] save in outFile                       --- #
    # ------------------------------------------------- #
    outFile = "dat/particles.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=Data )

    return()


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    generate__particleSample()
