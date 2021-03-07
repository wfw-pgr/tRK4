import numpy as np


# ========================================================= #
# ===  adjust__twt_Ustored.py                           === #
# ========================================================= #

def adjust__twt_Ustored():

    # ------------------------------------------------- #
    # --- [1] load info                             --- #
    # ------------------------------------------------- #

    inpFile  = "dat/cavity_info.dat"
    cavities = []
    
    with open( inpFile, "r" ) as f:
        lines = f.readlines()
        
    for ik,line in enumerate( lines ):
        if ( ( line.strip() )[0] == "#" ):
            pass
        else:
            cavity             = {}
            contents           = ( line.split() )
            cavity["FileName"] = contents[0]
            cavity["ZTT"]      = float( contents[1] )
            cavity["Q"]        = float( contents[2] )
            cavity["U"]        = float( contents[3] )
            cavity["twt_ID"]   = float( contents[4] )
            cavity["refFlag"]  =  bool( contents[5] )
            cavities.append( cavity )

    print( cavities )
    return()


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    adjust__twt_Ustored()
