import numpy  as np

# ========================================================= #
# ===  show__waveCondition.py                           === #
# ========================================================= #

def show__waveCondition():

    # ------------------------------------------------- #
    # --- [1] constants                             --- #
    # ------------------------------------------------- #

    cnsFile   = "dat/parameter.conf"
    import nkUtilities.load__constants as lcn
    const     = lcn.load__constants( inpFile=cnsFile )

    # ------------------------------------------------- #
    # --- [2] calculate amplitude                   --- #
    # ------------------------------------------------- #

    omega            = 2.0*np.pi*const["freq"]
    tau              = 1.0/      const["freq"]
    vphT             = const["beta_wave"] * const["cv"] * tau
    P_loss           = omega*const["Ustored"] / const["Qvalue"]
    amplitude_factor = np.sqrt( const["P_input"] / P_loss )
    energy_gain      = np.sqrt( const["Lcavity"] * const["rsh"] * const["P_input"] )
    
    # ------------------------------------------------- #
    # --- [3] display                               --- #
    # ------------------------------------------------- #

    print()
    print( "[calculate__power_of_wave] frequency     :: {0}".format( const["freq"]      ) )
    print( "[calculate__power_of_wave]     omega     :: {0}".format( omega              ) )
    print( "[calculate__power_of_wave]         T     :: {0}".format( tau                ) )
    print( "[calculate__power_of_wave]   Lcavity     :: {0}".format( const["Lcavity"]   ) )
    print( "[calculate__power_of_wave]   vph * T     :: {0}".format( vphT               ) )
    print()
    print( "[calculate__power_of_wave]   Ustored     :: {0}".format( const["Ustored"]   ) )
    print( "[calculate__power_of_wave]    Qvalue     :: {0}".format( const["Qvalue"]    ) )
    print( "[calculate__power_of_wave]       rsh     :: {0}".format( const["rsh"]       ) )
    print( "[calculate__power_of_wave]   P_input     :: {0}".format( const["P_input"]   ) )
    print( "[calculate__power_of_wave] t_transit     :: {0}".format( const["t_transit_time"] ) )
    print()
    print( "[calculate__power_of_wave]    P_loss     :: {0}".format( P_loss             ) )
    print( "[calculate__power_of_wave] amplitude     :: {0}".format( amplitude_factor   ) )
    print( "[calculate__power_of_wave] energy_gain   :: {0}".format( energy_gain        ) )
    print()


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    show__waveCondition()
    
