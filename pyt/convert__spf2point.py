import numpy as np


# ========================================================= #
# ===  convert superfish file into pointFile            === #
# ========================================================= #

def convert__spf2point( inpFile=None ):

    #
    #  x => r direction
    #  y => t direction
    #
    xp_, yp_, zp_  = 0, 1, 2
    ex_, ey_, ez_  = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] arguments                             --- #
    # ------------------------------------------------- #
    cnsFile    = "dat/field.conf"
    import nkUtilities.load__constants as lcn
    const = lcn.load__constants( inpFile=cnsFile )
    
    if ( inpFile is None ):
        inpFile = const["superfishFile"]
        
    import nkUtilities.load__pointFile as lpf
    Data       = lpf.load__pointFile( inpFile=inpFile, returnType="structured" )
    print( Data.shape )
    LK, LJ, LI = Data.shape[0], Data.shape[1], Data.shape[2]
    Data       = np.reshape( Data, (LK*LJ*LI,7) )

    # ------------------------------------------------- #
    # --- [2] convert into field-type pointFile     --- #
    # ------------------------------------------------- #
    pData          = np.zeros( (Data.shape[0],6) )
    pData[:,xp_]   = Data[:,1]
    pData[:,yp_]   = 0.0
    pData[:,zp_]   = Data[:,0]
    pData[:,ex_]   = Data[:,4] * const["efield_factor"]
    pData[:,ey_]   = 0.0       * const["efield_factor"]
    pData[:,ez_]   = Data[:,3] * const["efield_factor"]

    index          = np.lexsort( ( pData[:,xp_], pData[:,yp_], pData[:,zp_]) )
    pData          = pData[index]
    pData          = np.reshape( pData, (LI,1,LJ,6) )

    import nkUtilities.save__pointFile as spf
    names = ["xp","yp","zp","Ex","Ey","Ez"]
    spf.save__pointFile( outFile=const["efieldFile"], Data=pData, names=names )
    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):

    import nkUtilities.genArgs as gar
    args    = gar.genArgs()
    inpFile = args["file"]
    convert__spf2point( inpFile=inpFile )
