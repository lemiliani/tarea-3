module mod_mfp
    !
    ! This module handles the calculation of the distance to the next interaction.
    ! Exponential transformation is implemented.
    !

    use iso_fortran_env, only: int32, real64
    
    use mod_rng

implicit none

contains

    real(kind=real64) function mfp(sigma, c, u)
        !
        ! This function samples the free flight, the path length of the particle 
        ! before suffering an interaction. The analytical inversion method is implemented.
        !
        real(kind=real64), intent(in) :: sigma, c, u ! total interaction cross-section (cm-1)
        real(kind=real64) :: rnno

        rnno = rng_set()
        mfp = -log(1.0_real64-rnno)/(sigma-c*u)
        
    end function mfp

end module mod_mfp