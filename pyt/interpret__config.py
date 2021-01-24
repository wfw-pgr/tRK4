import nkUtilities.load__constants as lcn
import nkUtilities.save__constants as scn


# ========================================================= #
# ===  load Free-Format config  & save as Formated one  === #
# ========================================================= #

def interpret__config( inpFile="dat/parameter.conf", outFile="dat/input.conf" ):

    # ------------------------------------------------- #
    # --- [1] load constants info                   --- #
    # ------------------------------------------------- #
    const = lcn.load__constants( inpFile=inpFile )
    keys  = lcn.load__constants( inpFile=inpFile, returnKeys=True )
    print( keys )

    # ------------------------------------------------- #
    # --- [2] save constants in File                --- #
    # ------------------------------------------------- #
    scn.save__constants( outFile=outFile, const=const, keys=keys )
    print()
    print( "[interpret__config.py]  inpFile :: {0} ".format( inpFile ) )
    print( "[interpret__config.py]  converted into... " )
    print( "[interpret__config.py]  outFile :: {0} ".format( outFile ) )
    print()
    return()

    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    interpret__config()
