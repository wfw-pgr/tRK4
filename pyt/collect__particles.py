import sys, subprocess
import numpy as np

# ========================================================= #
# ===  collect__particles.py                            === #
# ========================================================= #

def collect__particles( target=None, pos=None ):

    # ------------------------------------------------- #
    # --- [0] preparation                           --- #
    # ------------------------------------------------- #
    
    if ( target is None ):
        print( "[coolect__particles.py] target_time ?? ( t,x,y,z ) >>  ", end="" )
        target = input()
    if   ( target.lower() in ["t"] ):
        target = 0
    elif ( target.lower() in ["x"] ):
        target = 1
    elif ( target.lower() in ["y"] ):
        target = 2
    elif ( target.lower() in ["z"] ):
        target = 3
    else:
        sys.exit("[collect__particles.py] choose from (t,x,y,z) ")

    if ( pos is None ):
        print( "[coolect__particles.py] position ?? >>  ", end="" )
        pos = float( input() )
    
    # ------------------------------------------------- #
    # --- [1] load constants                        --- #
    # ------------------------------------------------- #

    cnsFile = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    gconst  = lcn.load__constants( inpFile=cnsFile )

    # ------------------------------------------------- #
    # --- [2] collect particles                     --- #
    # ------------------------------------------------- #

    nCmp        = 14
    import select__particles as sel
    selected    = sel.load__selected()
    npt         = selected.shape[0]
    ret         = np.zeros( (npt,nCmp) )
    
    for ip,iS in enumerate( selected ):
        
        prbFile   = "prb/probe{0:06}.dat".format( iS )
        Data      = np.loadtxt( prbFile )
        axis      = np.ravel( Data[:,target] )
        idx       = np.argmin( np.abs( axis - pos ) )
        diff      = axis[idx] - pos

        if   ( diff == 0.0    ):
            ret[ip,:] = Data[idx,:]
            
        elif ( ( idx == 0 ) or ( idx == Data.shape[0]-1 ) ):
            ret[ip,:] = Data[idx,:]
            
        elif ( ( axis[idx-1] <= pos ) and ( pos <= axis[idx  ] ) ):
            t1, t2    = axis[idx-1], axis[idx  ]
            p1, p2    = ( t2-pos ) / ( t2-t1 ), ( pos-t1 ) / ( t2-t1 )
            ret[ip,:] = p1*Data[idx-1,:] + p2*Data[idx  ,:]

        elif ( ( axis[idx  ] <= pos ) and ( pos <= axis[idx+1] ) ):
            t1, t2    = axis[idx  ], axis[idx+1]
            p1, p2    = ( t2-pos ) / ( t2-t1 ), ( pos-t1 ) / ( t2-t1 )
            ret[ip,:] = p1*Data[idx  ,:] + p2*Data[idx+1,:]
            
    # ------------------------------------------------- #
    # --- [3] save in file                          --- #
    # ------------------------------------------------- #
    outFile   = "prb/collected.dat"
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile=outFile, Data=ret )
    return( ret )


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #
if ( __name__=="__main__" ):
    collect__particles()
