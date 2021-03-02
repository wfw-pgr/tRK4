import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display__initialAxisymmField                     === #
# ========================================================= #


def display__initialAxisymmField():

    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile  = "dat/parameter.conf"
    const    = lcn.load__constants( inpFile=cnsFile )

    import nkUtilities.load__pointFile as lpf
    efield   = lpf.load__pointFile( inpFile=const["EFieldFile"], returnType="point" )
    bfield   = lpf.load__pointFile( inpFile=const["BFieldFile"], returnType="point" )

    config   = lcf.load__config()
    pngFile  = "png/field_init_{0}.png"
    
    # ------------------------------------------------- #
    # --- [2] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="cMap_def", config=config )
    config["FigSize"]        = (8,4)
    config["cmp_position"]   = [0.16,0.12,0.97,0.88]
    config["xTitle"]         = "Z (m)"
    config["yTitle"]         = "R (m)"
    config["cmp_xAutoRange"] = True
    config["cmp_yAutoRange"] = True
    config["cmp_xRange"]     = [-5.0,+5.0]
    config["cmp_yRange"]     = [-5.0,+5.0]

    config["vec_AutoScale"]    = True
    config["vec_AutoRange"]    = True
    config["vec_AutoScaleRef"] = 200.0
    config["vec_nvec_x"]       = 24
    config["vec_nvec_y"]       = 6
    config["vec_interpolation"] = "nearest"

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #

    xAxis = np.copy( efield[:,z_] )
    yAxis = np.copy( efield[:,x_] )

    cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=efield[:,vz_], \
                 pngFile=pngFile.format( "Ez" ), config=config )
    cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=efield[:,vx_], \
                 pngFile=pngFile.format( "Er" ), config=config )
    cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=bfield[:,vy_], \
                 pngFile=pngFile.format( "Hp" ), config=config )
    
    fig = cmt.cMapTri( pngFile=pngFile.format( "Ev" ), config=config )
    fig.add__contour( xAxis=xAxis, yAxis=yAxis, Cntr=bfield[:,vy_] )
    fig.add__cMap   ( xAxis=xAxis, yAxis=yAxis, cMap=bfield[:,vy_] )
    fig.add__vector ( xAxis=xAxis, yAxis=yAxis, uvec=efield[:,vz_], vvec=efield[:,vx_], \
                      color="blue" )
    fig.save__figure()

    # ------------------------------------------------- #
    # --- [5] 1D plot                               --- #
    # ------------------------------------------------- #

    config["xTitle"] = "Z (m)"
    config["yTitle"] = "E (V/m)"

    val   = 0.005
    eps   = 1.e-10
    index = np.where( ( efield[:,x_] >= val-eps ) & ( efield[:,x_] <= val+eps ) )
    xAxis = np.copy( efield[index][:, z_] )
    ez    = np.copy( efield[index][:,vz_] )
    ex    = np.copy( efield[index][:,vx_] )

    import nkUtilities.plot1D as pl1

    fig = pl1.plot1D( config=config, pngFile=pngFile.format( "1D" ) )
    fig.add__plot( xAxis=xAxis, yAxis=ez, color="royalblue", label="Ez" )
    fig.add__plot( xAxis=xAxis, yAxis=ex, color="magenta"  , label="Ex" )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()

    
# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__initialAxisymmField()
