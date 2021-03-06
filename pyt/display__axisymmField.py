import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display__axisymmField                            === #
# ========================================================= #
def display__axisymmField( inpFile=None ):

    x_ , y_ , z_  = 0, 1, 2
    vx_, vy_, vz_ = 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] Load Config & Eigenmode               --- #
    # ------------------------------------------------- #

    import nkUtilities.load__constants as lcn
    cnsFile    = "dat/parameter.conf"
    pngFile    = "png/{0}field_{1}_{2}.png"
    config     = lcf.load__config()
    const      = lcn.load__constants( inpFile=cnsFile )

    eflist     = const["EFieldListFile"]
    bflist     = const["BFieldListFile"]
    
    with open( eflist, "r" ) as f:
        efiles = f.readlines()
    with open( bflist, "r" ) as f:
        bfiles = f.readlines()
    
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
    # --- [4] plot Figure ( efield )                --- #
    # ------------------------------------------------- #

    for ik, efile in enumerate( efiles ):

        import nkUtilities.load__pointFile as lpf
        efield = lpf.load__pointFile( inpFile=efile.strip(), returnType="point" )
    
        xAxis = np.copy( efield[:,z_] )
        yAxis = np.copy( efield[:,x_] )
        absE  = np.sqrt( efield[:,vx_]**2 + efield[:,vz_]**2 )

        cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=efield[:,vz_], \
                     pngFile=pngFile.format( "e", ik, "Ez" ), config=config )
        cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=efield[:,vx_], \
                     pngFile=pngFile.format( "e", ik, "Er" ), config=config )
    
        fig = cmt.cMapTri( pngFile=pngFile.format( "e", ik, "Ev",  ), config=config )
        fig.add__cMap   ( xAxis=xAxis, yAxis=yAxis, cMap=absE )
        fig.add__vector ( xAxis=xAxis, yAxis=yAxis, uvec=efield[:,vz_], vvec=efield[:,vx_], \
                          color="blue" )
        fig.save__figure()

    # ------------------------------------------------- #
    # --- [5] plot Figure ( bfield )                --- #
    # ------------------------------------------------- #

    for ik, bfile in enumerate( bfiles ):

        import nkUtilities.load__pointFile as lpf
        bfield = lpf.load__pointFile( inpFile=bfile.strip(), returnType="point" )
    
        xAxis = np.copy( bfield[:,z_] )
        yAxis = np.copy( bfield[:,x_] )
        
        cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=bfield[:,vy_], \
                     pngFile=pngFile.format( "b", ik, "Hp" ), config=config )
    

# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display__axisymmField()






    # # ------------------------------------------------- #
    # # --- [5] 1D plot                               --- #
    # # ------------------------------------------------- #

    # config["xTitle"] = "Z (m)"
    # config["yTitle"] = "E (V/m)"

    # val   = 0.0
    # eps   = 1.e-10
    # index = np.where( ( efield[:,x_] >= val-eps ) & ( efield[:,x_] <= val+eps ) )
    # xAxis = np.copy( efield[index][:, z_] )
    # ez    = np.copy( efield[index][:,vz_] )

    # import nkUtilities.plot1D as pl1

    # fig = pl1.plot1D( config=config, pngFile=pngFile.format( "1D" ) )
    # fig.add__plot( xAxis=xAxis, yAxis=ez, color="royalblue", label="Ez" )
    # fig.add__legend()
    # fig.set__axis()
    # fig.save__figure()

