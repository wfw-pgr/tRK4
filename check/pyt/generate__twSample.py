import numpy as np

# ========================================================= #
# ===  generate travelling wave sample File             === #
# ========================================================= #

def generate__twSample():

    #  E = E0 * exp ( iwt - ikx )   -->  exp( - ikx ) = cos(kx) - sin(kx)
    
    # ------------------------------------------------- #
    # --- [0] parameters                            --- #
    # ------------------------------------------------- #
    wave_number     = 2
    freq            = 2.856e9
    beta            = 0.99
    cv              = 2.9979246e+08
    EField_strength = 1.0e7

    # ------------------------------------------------- #
    # --- [1] parameter settings                    --- #
    # ------------------------------------------------- #
    x_,y_,z_        = 0, 1, 2
    vx_,vy_,vz_     = 0, 1, 2

    vphase          = beta * cv
    wavelength      = vphase / freq
    zMin, zMax      = 0.0, wavelength * wave_number
    k_wave          = 2.0 * np.pi / wavelength
    phase_shift1    = 0.0
    phase_shift2    = +90.0 / 180.0 * np.pi
        
    print()
    print( "[generate__twSample.py]  zMin & zMax are determined by periodic condtion... " )
    print( "[generate__twSample.py]         (zMin,zMax)  :: ({0},{1})".format( zMin, zMax ) )
    print()
    print( "[generate__twSample.py]  set above value in your parameter.conf" )
    print( "[generate__twSample.py]  press any key to continue..... >>  " )
    print()
    
    # ------------------------------------------------- #
    # --- [2] load config File                      --- #
    # ------------------------------------------------- #
    inpFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const = lcn.load__constants( inpFile=inpFile )
    
    # ------------------------------------------------- #
    # --- [3] grid generation                       --- #
    # ------------------------------------------------- #
    import nkUtilities.equiSpaceGrid as esg
    x1MinMaxNum     = [ const["xMin"], const["xMax"], const["LI"] ]
    x2MinMaxNum     = [ const["yMin"], const["yMax"], const["LJ"] ]
    x3MinMaxNum     = [        zMin  ,         zMax , const["LK"] ]
    grid            = esg.equiSpaceGrid( x1MinMaxNum=x1MinMaxNum, x2MinMaxNum=x2MinMaxNum, \
                                         x3MinMaxNum=x3MinMaxNum, returnType = "structured" )
    eigen1          = np.zeros_like( ( grid ) )
    eigen2          = np.zeros_like( ( grid ) )
    eigen1[...,vz_] = EField_strength * np.cos( - k_wave * grid[...,z_] + phase_shift1 )
    eigen2[...,vz_] = EField_strength * np.cos( - k_wave * grid[...,z_] + phase_shift2 )
    
    Data1           = np.concatenate( (grid,eigen1), axis=3 )
    Data2           = np.concatenate( (grid,eigen2), axis=3 )
    
    # ------------------------------------------------- #
    # --- [4] save in File                          --- #
    # ------------------------------------------------- #

    import nkUtilities.save__pointFile as spf
    outFile1   = "dat/Eigen1.dat"
    outFile2   = "dat/Eigen2.dat"
    spf.save__pointFile( outFile=outFile1, Data=Data1 )
    spf.save__pointFile( outFile=outFile2, Data=Data2 )

    
# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    generate__twSample()
