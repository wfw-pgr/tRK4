import os, sys
import numpy                       as np
import nkUtilities.load__pointFile as lpf

# ========================================================= #
# ===  select__particles                                === #
# ========================================================= #

def select__particles_judge():

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    #  -- [1-1] probe file                          --  #
    outFile = "dat/selected.dat"
    inpFile = "prb/probe{0:06}.dat"

    #  -- [1-2] load constants                      --  #
    cnsFile = "dat/particle.conf"
    import nkUtilities.load__constants as lcn
    const   = lcn.load__constants( inpFile=cnsFile )
    
    # ------------------------------------------------- #
    # --- [2] select probe File                     --- #
    # ------------------------------------------------- #

    with open( outFile, "w" ) as f:
        # -- [2-1] write header                     --  #
        f.write( "# FileNumber\n" )

        # -- [2-2] judge and save result            --  #
        for ik in range( const["npt"] ):
            Data      = lpf.load__pointFile( inpFile=inpFile.format( ik+1 ), returnType="point" )
            if ( judge__particles( Data=Data ) is True ):
                f.write( "{0}\n".format( ik+1 ) )
    
    # ------------------------------------------------- #
    # --- [3] save as file                          --- #
    # ------------------------------------------------- #
    return()



# ========================================================= #
# ===  judge__probeData :: criterion to pick particle   === #
# ========================================================= #

def judge__particles( Data=None ):

    t_, x_, y_, z_ = 0, 1, 2, 3
    
    index = np.where( Data[:,x_] > 0.01 )
    if ( len( index[0] ) > 0 ):
        ret = False
    else:
        ret = True
        
    return( ret )



# ========================================================= #
# ===  load__selected                                   === #
# ========================================================= #

def load__selected():

    # ------------------------------------------------- #
    # --- [1] load selected                         --- #
    # ------------------------------------------------- #
    inpFile  = "dat/selected.dat"
    with open( inpFile, "r" ) as f:
        nums = np.array( np.loadtxt( f ), dtype=np.int )
    return( nums )


# ========================================================= #
# ===  select__allparticles                             === #
# ========================================================= #

def select__allparticles():

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    #  -- [1-1] probe file                          --  #
    outFile = "dat/selected.dat"

    #  -- [1-2] load constants                      --  #
    cnsFile = "dat/particle.conf"
    import nkUtilities.load__constants as lcn
    const   = lcn.load__constants( inpFile=cnsFile )

    #  -- [1-3] write all particles num             --  #
    with open( outFile, "w" ) as f:
        # -- [1-3-1] write header                   --  #
        f.write( "# FileNumber\n" )
        # -- [1-3-2] judge and save result          --  #
        for ik in range( const["npt"] ):
            f.write( "{0}\n".format( ik+1 ) )
    return()


# ========================================================= #
# ===  select__particles_array                          === #
# ========================================================= #

def select__particles_array():

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    #  -- [1-1] probe file                          --  #
    outFile = "dat/selected.dat"

    #  -- [1-2] args                                --  #
    import nkUtilities.genArgs as gar
    args    = gar.genArgs()
    array   = None
    if ( args["array"] is not None ):
        array = [ int( val ) for val in args["array"] ]
        
    #  -- [1-3] load constants                      --  #
    if ( array is None ):
        import nkUtilities.load__constants as lcn
        cnsFile = "dat/parameter.conf"
        const   = lcn.load__constants( inpFile=cnsFile )
        array = [ int( val ) for val in const["post.select.pt.array"] ]
        
    #  -- [1-3] write all particles num             --  #
    with open( outFile, "w" ) as f:
        # -- [1-3-1] write header                   --  #
        f.write( "# FileNumber\n" )
        # -- [1-3-2] judge and save result          --  #
        for ipt in array:
            f.write( "{0}\n".format( ipt ) )
    return()



# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):

    import nkUtilities.genArgs as gar
    args = gar.genArgs()

    if   ( args["mode"] is None ):
        select__allparticles()
    elif ( args["mode"] in ["all"] ):
        select__allparticles()
    elif ( args["mode"] in ["judge"] ):
        select__particles_judge()
    elif ( args["mode"] in ["array","choose"] ):
        select__particles_array()
    else:
        print( "[select__particles.py] unknown mode :: {0} ".format( args["mode"] ) )
        print( "[select__particles.py] choose from  :: [ all (default), judge, (array/chooose) ]")
        sys.exit()
