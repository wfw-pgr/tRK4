import numpy                       as np
import nkUtilities.load__pointFile as lpf

# ========================================================= #
# ===  select__probeFile                                === #
# ========================================================= #

def select__probeFile():

    # ------------------------------------------------- #
    # --- [1] preparation                           --- #
    # ------------------------------------------------- #
    #  -- [1-1] probe file                          --  #
    outFile = "prb/selectedFile.dat"
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
            if ( judge__probeData( Data=Data ) is True ):
                f.write( "{0}\n".format( ik ) )
    
    # ------------------------------------------------- #
    # --- [3] save as file                          --- #
    # ------------------------------------------------- #
    return()



# ========================================================= #
# ===  judge__probeData :: criterion to pick particle   === #
# ========================================================= #

def judge__probeData( Data=None ):

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
    inpFile  = "prb/selectedFile.dat"
    with open( inpFile, "r" ) as f:
        nums = np.array( np.loadtxt( f ), dtype=np.int )
    return( nums )


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):
    select__probeFile()
