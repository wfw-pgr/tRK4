import os, sys, subprocess
import nkBasicAlgs.execute__commands as exe


# ========================================================= #
# ===  call__analysis.py                                === #
# ========================================================= #

def call__analysis():

    targets  = [ "png", "dat", "bpm", "prb" ]

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    import nkUtilities.genArgs as gar
    args     = gar.genArgs()
    if ( args["job"] is None ):
        print()
        print( "[call__analysis.py] please input job name ( or, --job xxx ) " )
        print( "[call__analysis.py]    job >>> ", end="" )
        job = input()
    else:
        job = args["job"]
    
    # ------------------------------------------------- #
    # --- [2] directory settings                    --- #
    # ------------------------------------------------- #
    jobDir   = "job/{0}/".format( job )
    dirlist  = [ "{0}{1}/".format( jobDir, target ) for target in targets ]
    if ( os.path.exists( jobDir ) is False ):
        print( "\n[call__analysis.py]  No such job Directory..... [ERROR]\n" )
        sys.exit()

    # ------------------------------------------------- #
    # --- [3] copy files                            --- #
    # ------------------------------------------------- #
    print( "--------------------------------------------------------" )
    print( "[call__analysis.py] copy files. " )
    print( "--------------------------------------------------------" )
    commands = [ "rsync -av {0} {1}".format( dirlist[ik], targets[ik] ) \
                 for ik in range( len( targets ) ) ]
    exe.execute__commands( commands=commands, shell=True )
    print()
    return()



# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):
    call__analysis()
        
