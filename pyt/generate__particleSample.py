import numpy as np


# ========================================================= #
# ===  generate__particleSample                         === #
# ========================================================= #

def generate__particleSample():

    xp_,yp_,zp_ = 0, 1, 2
    vx_,vy_,vz_ = 3, 4, 5
    
    import nkUtilities.load__constants as lcn
    inpFile     = "dat/particle.conf"
    const       = lcn.load__constants( inpFile=inpFile )

    # ------------------------------------------------- #
    # --- [1] particle generation                   --- #
    # ------------------------------------------------- #

    #  -- [1-1] systematic components (xp)          --  #
    Data        = np.zeros( (const["npt"],6) )
    Data[:,xp_] = np.linspace( const["xMin"], const["xMax"], const["npt"] )
    Data[:,yp_] = np.linspace( const["yMin"], const["yMax"], const["npt"] )
    Data[:,zp_] = np.linspace( const["zMin"], const["zMax"], const["npt"] )
    #  -- [1-2] systematic components (vp)          --  #
    Data[:,vx_] = const["cv"] * const["beta_x"]
    Data[:,vy_] = const["cv"] * const["beta_y"]
    Data[:,vz_] = const["cv"] * const["beta_z"]

    #  -- [1-3] random components (xp)              --  #
    Data[:,xp_] = Data[:,xp_] + np.random.randn( const["npt"] ) * const["sigma_x"]
    Data[:,yp_] = Data[:,yp_] + np.random.randn( const["npt"] ) * const["sigma_y"]
    Data[:,zp_] = Data[:,zp_] + np.random.randn( const["npt"] ) * const["sigma_z"]
    #  -- [1-4] random components (vp)              --  #
    Data[:,vx_] = Data[:,vx_] + np.random.randn( const["npt"] )*const["cv"]*const["sigma_vx"]
    Data[:,vy_] = Data[:,vy_] + np.random.randn( const["npt"] )*const["cv"]*const["sigma_vy"]
    Data[:,vz_] = Data[:,vz_] + np.random.randn( const["npt"] )*const["cv"]*const["sigma_vz"]

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
