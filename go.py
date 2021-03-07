import os, sys, subprocess
import pyt.generate__particleSample as gps
import pyt.into__namelist           as inl
import pyt.into__fieldinput         as ifi


# ========================================================= #
# ===  go.py                                            === #
# ========================================================= #

def go():

    # ------------------------------------------------- #
    # --- [1] generate particle Sample              --- #
    # ------------------------------------------------- #
    print()
    print( "[go.py] generate particles ??? (y/n) >>> ", end="" )
    yorn = input()
    if ( (yorn.lower() == "y") or ( len(yorn) == 0 ) ):
        gps.generate__particleSample()

    # ------------------------------------------------- #
    # --- [2] convert parameter.conf >> input.lst   --- #
    # ------------------------------------------------- #
    print()
    print( "[go.py] convert parameter.conf  into  input.lst ??? (y/n) >>> ", end="" )
    yorn = input()
    if ( (yorn.lower() == "y") or ( len(yorn) == 0 ) ):
        inl.into__namelist()
        
    # ------------------------------------------------- #
    # --- [3] generate particle Sample              --- #
    # ------------------------------------------------- #
    print()
    print( "[go.py] convert fieldlist.conf  into  field & parameter file ??? (y/n) >>> ", end="" )
    yorn = input()
    if ( (yorn.lower() == "y") or ( len(yorn) == 0 ) ):
        ifi.into__fieldinput()
        
    # ------------------------------------------------- #
    # --- [4] compile source code                   --- #
    # ------------------------------------------------- #
    os.chdir( "src/" )
    cmd = "make all"
    print( cmd )
    subprocess.call( cmd.split() )
    os.chdir( "../" )
    
    return()



# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):
    go()
