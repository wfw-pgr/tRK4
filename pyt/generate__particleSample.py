import numpy as np


# ========================================================= #
# ===  generate__particleSample                         === #
# ========================================================= #

def generate__particleSample():

    xp_,yp_,zp_ = 0, 1, 2
    vx_,vy_,vz_ = 3, 4, 5

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #

    #  -- [1-1] constants                           --  #
    import nkUtilities.load__constants as lcn
    inpFile     = "dat/particle.conf"
    const       = lcn.load__constants( inpFile=inpFile )

    #  -- [1-2] systematic components (xp)          --  #
    npt         = const["npt"]
    Data        = np.zeros( (const["npt"],6) )
    xD          = ( const["xMax"] - const["xMin"] )
    yD          = ( const["yMax"] - const["yMin"] )
    zD          = ( const["zMax"] - const["zMin"] )
    xM          = ( const["xMax"] + const["xMin"] ) * 0.5
    yM          = ( const["yMax"] + const["yMin"] ) * 0.5
    zM          = ( const["zMax"] + const["zMin"] ) * 0.5

    # ------------------------------------------------- #
    # --- [2] generate position                     --- #
    # ------------------------------------------------- #
    
    #  -- [2-1] distribution for x                  --  #
    if   ( const["distribution_x"] == "equispace" ):
        Data[:,xp_] = np.linspace( const["xMin"], const["xMax"], const["npt"] )
    elif ( const["distribution_x"] == "uniform"   ):
        Data[:,xp_] = const["xMin"] + xD * np.random.rand( npt )
    elif ( const["distribution_x"] == "gaussian"  ):
        Data[:,xp_] = np.random.normal( loc=xM, scale=xD/3, size=npt ) # -- xD/3 :: 3 sigma -- #
        
    #  -- [2-2] distribution for y                  --  #
    if   ( const["distribution_y"] == "equispace" ):
        Data[:,yp_] = np.linspace( const["yMin"], const["yMax"], const["npt"] )
    elif ( const["distribution_y"] == "uniform"   ):
        Data[:,yp_] = const["yMin"] + yD * np.random.rand( npt )
    elif ( const["distribution_y"] == "gaussian"  ):
        Data[:,yp_] = np.random.normal( loc=yM, scale=yD/3, size=npt )  # -- yD/3 :: 3 sigma -- #

    #  -- [2-3] distribution for z                  --  #
    if   ( const["distribution_z"] == "equispace" ):
        Data[:,zp_] = np.linspace( const["zMin"], const["zMax"], const["npt"] )
    elif ( const["distribution_z"] == "uniform"   ):
        Data[:,zp_] = const["zMin"] + zD * np.random.rand( npt )
    elif ( const["distribution_z"] == "gaussian"  ):
        Data[:,zp_] = np.random.normal( loc=zM, scale=zD/3, size=npt )  # -- zD/3 :: 3 sigma -- #


    # ------------------------------------------------- #
    # --- [3] generate velocity                     --- #
    # ------------------------------------------------- #

    #  -- [3-1] energy distribution                 --  #
    eD = ( const["energy_Max"] - const["energy_Min"] ) / 3     # -- for 3 sigma = eD -- #
    eM = ( const["energy_Max"] + const["energy_Min"] ) * 0.5
    if   ( const["distribution_energy"] == "equispace" ):
        energies    = np.linspace( const["energy_Min"], const["energy_Max"], const["npt"] )
    elif ( const["distribution_energy"] == "uniform" ):
        energies    = const["energy_Min"] + eD * np.random.rand( npt )
    elif ( const["distribution_energy"] == "gaussian" ):
        energies    = np.random.normal( loc=eM, scale=eD, size=npt )

    #  -- [3-2] absolute velocity                   --  #
    th_e    = np.abs( const["qp"] ) * energies / ( const["mp"] * const["cv"]**2 )
    beta_e  = np.sqrt( 1.0 - 1.0 / ( 1.0 + th_e )**2 )
    abs_vel = beta_e * const["cv"]
        
    #  -- uniform angle -- #
    deg2rad = np.pi / 180.0
    cone    = np.cos( const["angle_theta2"]*deg2rad ) - np.cos( const["angle_theta1"]*deg2rad)
    urand   = np.cos( const["angle_theta1"]*deg2rad ) + np.random.rand( npt ) * cone
    theta   = np.arccos( urand )
    phi     = 2.0*np.pi * np.random.rand( npt )

    #  -- direction     -- #
    rx    = np.sin( theta ) * np.cos( phi )
    ry    = np.sin( theta ) * np.sin( phi )
    rz    = np.cos( theta )

    #  -- [3-5] velocity definition                 --  #
    Data[:,vx_] = abs_vel[:] * rx[:]
    Data[:,vy_] = abs_vel[:] * ry[:]
    Data[:,vz_] = abs_vel[:] * rz[:]

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
