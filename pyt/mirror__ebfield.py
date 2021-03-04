import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt

# ========================================================= #
# ===  mirror__ebfield.py                               === #
# ========================================================= #

def mirror__ebfield( inpFile=None ):


    x_, y_, z_    = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [0] preparation                           --- #
    # ------------------------------------------------- #
    
    if ( inpFile is None ):

        print( "[mirror__ebfield.py] inpFile ( def. dat/efield.dat ) >> ??? " )
        inpFile = input()

        if ( len(inpFile)==0 ):
            print( "[mirror__ebfield.py] def. dat/efield.dat " )
            inpFile = "dat/efield.dat"

    
    # ------------------------------------------------- #
    # --- [1] load field                            --- #
    # ------------------------------------------------- #

    import nkUtilities.load__pointFile as lpf
    Data = lpf.load__pointFile( inpFile=inpFile, returnType="structured" )
    ret  = np.copy( Data )
    ret[:,:,:,3:6] = np.copy( Data[::-1,:,:,3:6] )

    Data_ = np.reshape( Data, (-1,6) )
    ret_  = np.reshape(  ret, (-1,6) )

    config  = lcf.load__config()
    pngFile = "png/before_mirror.png"
    cmt.cMapTri( xAxis=Data_[:,z_], yAxis=Data_[:,x_], cMap=Data_[:,vz_], \
    	         pngFile=pngFile, config=config )

    config  = lcf.load__config()
    pngFile = "png/after_mirror.png"
    cmt.cMapTri( xAxis=ret_[:,z_], yAxis=ret_[:,x_], cMap=ret_[:,vz_], \
    	         pngFile=pngFile, config=config )

    # ------------------------------------------------- #
    # --- [2] save mirrored file                    --- #
    # ------------------------------------------------- #

    outFile = "dat/mirrored.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=ret )
    print()
    print( "[mirror_ebfield.py] outFile :: {0} ".format( outFile ) )
    print( "[mirror_ebfield.py] mv {0} some_file_name ".format( outFile ) )
    print()
    

# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    mirror__ebfield()
