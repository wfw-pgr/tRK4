import nkUtilities.load__constants as lcn
import nkUtilities.save__namelist  as snl

# ========================================================= #
# ===  load fieldlist.conf & save as Formated lists     === #
# ========================================================= #

def into__fieldinput():

    # ------------------------------------------------- #
    # --- [1] load constants & list                 --- #
    # ------------------------------------------------- #
    inpFile     = "dat/fieldlist.conf"
    cnsFile     = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const       = lcn.load__constants( inpFile=cnsFile )

    # ------------------------------------------------- #
    # --- [2] load fieldlist.conf                   --- #
    # ------------------------------------------------- #
    #  -- [2-1] load fieldlist.conf                 --  #
    with open( inpFile, "r" ) as f:
        lines = f.readlines()
        
    #  -- [2-2] decompose and save it in 2 files    --  #
    efieldlists = ""
    bfieldlists = ""
    efieldparam = "# key Amplitude Frequency Phase xyzshift modulation_type boundary_xyz nRepeat_xyz\n"
    bfieldparam = "# key Amplitude Frequency Phase xyzshift modulation_type boundary_xyz nRepeat_xyz\n"
    for iL, line in enumerate( lines ):
        
        if ( ( line.strip() )[0] == "#" ):
            pass # comment line
        else:
            
            items = ( line.strip() ).split()
            
            if   ( items[1].lower() == "e" ):
                key      = items[0].split( "/" )[-1]
                contents = " ".join( [key] + items[2:] )
                efieldlists += items[0] + "\n"
                efieldparam += contents + "\n"
                
            elif ( items[1].lower() == "b" ):
                key      = items[0].split( "/" )[-1]
                contents = " ".join( [key] + items[2:] )
                bfieldlists += items[0] + "\n"
                bfieldparam += contents + "\n"
                
            else:
                sys.exit( "[into__fieldlist.py] field type ??? >> ( E / B ) ??" )

    # ------------------------------------------------- #
    # --- [3] save as file                          --- #
    # ------------------------------------------------- #
    print()
    with open( const["EFieldListFile"], "w" ) as f:
        f.write( efieldlists )
    print( "[into__fieldlist.py]  outFile :: {0} ".format( const["EFieldListFile"] ) )
    with open( const["BFieldListFile"], "w" ) as f:
        f.write( bfieldlists )
    print( "[into__fieldlist.py]  outFile :: {0} ".format( const["BFieldListFile"] ) )
    with open( const["EFieldParamFile"], "w" ) as f:
        f.write( efieldparam )
    print( "[into__fieldlist.py]  outFile :: {0} ".format( const["EFieldParamFile"] ) )
    with open( const["EFieldParamFile"], "w" ) as f:
        f.write( bfieldparam )
    print( "[into__fieldlist.py]  outFile :: {0} ".format( const["BFieldParamFile"] ) )
    print()
    
    return()

    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    into__fieldinput()
