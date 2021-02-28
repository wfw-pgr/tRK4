import numpy                       as np
import nkUtilities.load__pointFile as lpf


# ========================================================= #
# ===  make histogram along energy axis                 === #
# ========================================================= #

def histogram__energy( div=11, eRange=None ):

    # inpFile = "dat/particles.dat"
    # pngFile = "png/histogram__initParticles.png"

    inpFile = "bpm/screen_bpm.dat"
    pngFile = "png/histogram__bpmParticles.png"

    
    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5

    # ------------------------------------------------- #
    # --- [1] load constants & data                 --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile  = "dat/parameter.conf"
    pcnsFile = "dat/particle.conf"
    const    = lcn.load__constants( inpFile= cnsFile )
    pconst   = lcn.load__constants( inpFile=pcnsFile )

    
    # ------------------------------------------------- #
    # --- [2] make hist of particle's energy        --- #
    # ------------------------------------------------- #

    Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
    
    vabs    = np.sqrt( Data[:,vx_]**2 + Data[:,vy_]**2 + Data[:,vz_]**2 )
    beta    = vabs / const["cv"]
    print( np.min( beta ), np.max( beta ) )
    Th      = 1.0 /( np.sqrt( 1.0 - beta**2 ) ) - 1.0
    hEk     = const["mp"] * const["cv"]**2 / const["qe"] * Th

    # histogram, bins = np.histogram( hEk, bins=div, range=eRange )
    # haxis = ( 0.5 * ( bins + np.roll( bins, 1 ) ) )[1:]
    # haxis = haxis / 1000.0

    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(1)
    # ax.set_facecolor( "lightcyan" )
    ax.grid( zorder=1, color="gray" )
    ax.hist( hEk, bins=div, range=eRange, rwidth=0.7, color="royalblue", \
             edgecolor="white", linewidth=1.8, zorder=2 )
    fig.savefig( pngFile )



# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    div    = 31
    eRange = ( 8e3, 12e3 )
    histogram__energy( div=div, eRange=eRange )
