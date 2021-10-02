import os, sys, subprocess
import nkBasicAlgs.execute__commands as exe


# ========================================================= #
# ===  save__analysis.py                                === #
# ========================================================= #

def save__analysis():

    targets  = [ "png", "dat", "bpm", "prb" ]

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    import nkUtilities.genArgs as gar
    args     = gar.genArgs()
    if ( args["job"] is None ):
        print()
        print( "[save__analysis.py] please input job name ( or, --job xxx ) " )
        print( "[save__analysis.py]    job >>> ", end="" )
        job = input()
    else:
        job = args["job"]
    
    # ------------------------------------------------- #
    # --- [2] directory settings                    --- #
    # ------------------------------------------------- #
    jobDir   = "job/{0}/".format( job )
    dirlist  = [ "{0}{1}/".format( jobDir, target ) for target in targets ]

    # ------------------------------------------------- #
    # --- [3] settings                              --- #
    # ------------------------------------------------- #
    print( "--------------------------------------------------------" )
    print( "[save__analysis.py] make job Directories. " )
    print( "--------------------------------------------------------" )

    commands = [ "mkdir -p {0}".format( dirname ) for dirname in dirlist ]
    exe.execute__commands( commands=commands )
    print()

    # ------------------------------------------------- #
    # --- [4] copy files                            --- #
    # ------------------------------------------------- #
    print( "--------------------------------------------------------" )
    print( "[save__analysis.py] copy files. " )
    print( "--------------------------------------------------------" )
    commands = [ "rsync -av {0} {1}".format( targets[ik], dirlist[ik] ) \
                 for ik in range( len( targets ) ) ]
    exe.execute__commands( commands=commands, shell=True )
    print()
    
    # ------------------------------------------------- #
    # --- [5] delete files                          --- #
    # ------------------------------------------------- #
    print( "--------------------------------------------------------" )
    print( "[save__analysis.py] delete files. " )
    print( "--------------------------------------------------------" )

    if ( args["mode"] is None ):
        print()
        print( "[save__analysis.py] clear files after saving ??? ( y/n ) >>> ", end="" )
        delete = input()
        print()
    else:
        if   ( args["mode"].lower() in [ "y", "yes", "delete" ] ):
            delete = "y"
        elif ( args["mode"].lower() in [ "n", "no" ] ):
            delete = "n"
        
    if   ( delete.lower() == "y" ):
        print( "[save__analysis.py] deleting files....  " )
        targets_ = list( set( targets ) - { "dat" } )
        commands = [ "rm -r {0}/*".format( target ) for target in targets_ ]
        exe.execute__commands( commands=commands, shell=True )
        print()
    elif ( delete.lower() == "n" ):
        print( "[save__analysis.py] No delete. Keep files..." )
        print()
    else:
        print( "[save__analysis.py] unknown delete mode. please type ( y/n )." )
        sys.exit()

    return()



# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):
    save__analysis()
        
