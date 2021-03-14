import nkUtilities.load__constants as lcn
import nkUtilities.save__namelist  as snl

# ========================================================= #
# ===  load Free-Format config  & save as Formated one  === #
# ========================================================= #

def into__namelist( inpFile="dat/parameter.conf", outFile="dat/input.lst" ):

    # ------------------------------------------------- #
    # --- [1] load constants info                   --- #
    # ------------------------------------------------- #
    const    = lcn.load__constants( inpFile=inpFile )
    keys     = lcn.load__constants( inpFile=inpFile, returnKeys=True )
    skipkeys = [ "cv", "tw_cosEigenFile", "tw_sinEigenFile", \
                 "tw_timeStart", "tw_nCycle", "tw_nTime", "tw_frequency", "tw_phase", \
                 "P_input", "Lcavity", "beta_wave", "Ustored", "Qvalue", "rsh", "t_transit_time",\
                 "strength_bfield", "xMin_bfield", "xMax_bfield", "yMin_bfield", "yMax_bfield", "zMin_bfield", "zMax_bfield", "LI_bfield", "LJ_bfield", "LK_bfield" ]
    print( keys )

    # ------------------------------------------------- #
    # --- [2] save constants in File                --- #
    # ------------------------------------------------- #
    snl.save__namelist( outFile=outFile, const=const, keys=keys, skipkeys=skipkeys )
    print()
    print( "[into__namelist.py]  inpFile :: {0} ".format( inpFile ) )
    print( "[into__namelist.py]  converted into... " )
    print( "[into__namelist.py]  outFile :: {0} ".format( outFile ) )
    print()
    return()

    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    into__namelist()
