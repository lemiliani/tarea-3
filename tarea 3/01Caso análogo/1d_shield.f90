program shield_1d
    !
    ! Program that simulates the particle transport through a 1D shield using Monte Carlo methods.
    ! Multi-region version, Method: 'caso análogo'
    !
    ! Authors: Luis Emiliani, Andrés Aracena
    ! ruta: gfortran 1d_shield.f90 mod_rng.f90 mod_mfp.f90 mod_scatt.f90
    ! run: ./a.out

    use iso_fortran_env, only: int32, real64

    use mod_rng
    use mod_mfp
    use mod_scatt

implicit none

    ! Geometry parameters
    integer(kind=int32), parameter :: nreg = 1              ! number of region in the 1D shield
    real(kind=real64), dimension(nreg) :: xthick = (/5.0/)  ! thickness of the 1D shield (cm)
    real(kind=real64), dimension(nreg+1) :: xbounds = 0.0    ! boundaries of the shield regions (cm)

    ! Transport parameters
    real(kind=real64), dimension(nreg) :: sigma_t = (/2.0/)  ! Total interaction cross section (cm-1)
    real(kind=real64), dimension(nreg) :: sigma_a            ! Absorption cross section (cm-1)
    real(kind=real64), dimension(nreg) :: sigma_s            ! Scattering cross section (cm-1)
    real(kind=real64), parameter :: ce = 0.2                 ! relaship between ce = sigma_s/sigma_t

    ! Particle parameters
    real(kind=real64), parameter :: xin = 0.0    ! initial position (cm)
    real(kind=real64), parameter :: uin = 1.0    ! initial direction
    real(kind=real64), parameter :: wtin = 1.0   ! statistical weight
    integer(kind=int32), parameter :: irin = 1  ! initial region

    integer(kind=int32) :: ir
    real(kind=real64) :: x, u, wt

    ! Simulation parameters
    integer(kind=int32), parameter :: nperbatch = 1E6    
    integer(kind=int32), parameter :: nbatch = 10               ! number of statistical batches
    integer(kind=int32), parameter :: nhist = nbatch*nperbatch  ! number of simulated histories

    ! Scoring variables
    real(kind=real64), dimension(0:nreg+1) :: score, score2 = 0.0    ! score(0) : reflection
                                                                     ! score(1:nreg) : absorption
                                                                     ! score(nreg+1) : transmission

    real(kind=real64), dimension(0:nreg+1) :: mean_score = 0.0   ! mean value of scoring array
    real(kind=real64), dimension(0:nreg+1) :: unc_score = 0.0    ! uncertainty values of scoring array

    integer(kind=int32) :: i, ihist, ibatch   ! loop counters 
    logical :: pdisc                          ! flag to discard a particle
    integer(kind=int32) :: irnew              ! index of new region 
    real(kind=real64) :: pstep                ! distance to next interaction 
    real(kind=real64) :: dist                 ! distance to boundary along particle direction
    real(kind=real64) :: rnno 
    real(kind=real64) :: fom
    real(kind=real64) :: start_time, end_time

    call cpu_time(start_time)

    ! Initialize RNG
    call rng_init(20180815)

    ! Calculation of coeficient of absorption and scattering cross section 
    sigma_s = ce*sigma_t
    sigma_a = sigma_t - sigma_s

    ! Print some info to console
    write(*,'(A, I15)') 'Number of histories : ', nhist
    write(*,'(A, I15)') 'Number of batches : ', nbatch
    write(*,'(A, I15)') 'Number of histories per batch : ', nperbatch

    do i = 1,nreg
        write(*,'(A, I5, A)') 'For region ', i, ':'
        write(*,'(A, F15.5)') 'Total interaction cross-section (cm-1): ', sigma_t(i)
        write(*,'(A, F15.5)') 'Absoption interaction cross-section (cm-1): ', sigma_a(i)
        write(*,'(A, F15.5)') 'Scattering interaction cross-section (cm-1): ', sigma_s(i)
        write(*,'(A, F15.5)') '1D shield thickness (cm): ', xthick(i)
    enddo
    
    ! Initialize simulation geometry.
    write(*,'(A)') 'Region boundaries (cm):'
    write(*,'(F15.5)') xbounds(1)
    do i = 1,nreg
        xbounds(i+1) = xbounds(i) + xthick(i)
        write(*,'(F15.5)') xbounds(i+1)
    enddo
    
    ibatch_loop: do ibatch = 1,nbatch
        ihist_loop: do ihist = 1,nperbatch
            
            ! Initialize history
            x = xin
            u = uin
            wt = wtin
            ir = irin
            
            ! Set flag used for particle discard.
            pdisc = .false.

            ! Enter transport process
            particle_loop: do
            
                ptrans_loop: do

                    ! Distance to the next interaction.
                    if(ir == 0 .or. ir == nreg+1) then
                        ! Vacuum step
                        pstep = 1.0E8
                    else
                        pstep = mfp(sigma_t(ir))
                    endif

                    ! Save particle current region.
                    irnew = ir

                    ! Check expected particle step with geometry.
                    if(u .lt. 0.0) then
                        if(irnew == 0) then
                            ! The particle is leaving the geometry, discard it.
                            pdisc = .true.
                        else
                            ! The particle goes to the front face of the shield.
                            dist = (xbounds(ir) - x)/u

                            ! Now check if the particle leaves current region.
                            if(dist < pstep) then
                                pstep = dist                      
                                irnew = irnew - 1
                            endif
                        endif
                        
                    else if(u .gt. 0.0) then
                        if(irnew == nreg+1) then
                            ! The particle is leaving the geometry, discard it.
                            pdisc = .true.
                        else
                            ! The particle goes to the back face of the shield.
                            dist = (xbounds(ir+1) - x)/u

                            ! Now check if the particle leaves current region.
                            if(dist < pstep) then
                                pstep = dist
                                irnew = irnew + 1
                            endif
                        endif
                        
                    endif

                    ! Check if particle has been discarded.
                    if(pdisc .eqv. .true.) then
                        exit
                    endif

                    ! Update position of particle.
                    x = x + pstep*u

                    ! Reached this point, if the particle has not changed region, it must interact.
                    if(ir == irnew) then
                        exit
                    else
                        ! Update particle region index and continue transport process.
                        ir = irnew
                    endif

                enddo ptrans_loop    
            
                if(pdisc .eqv. .true.) then
                    ! The particle has been discarded. Score it and end tracking.
                    score(ir) = score(ir) + wt
                    exit
                endif

                ! Determine interaction type.
                rnno = rng_set()
                if(rnno .le. sigma_a(ir)/sigma_t(ir)) then
                    ! The particle was absorbed.
                    score(ir) = score(ir) + wt
                    exit
                else
                    ! The particle was scattered, get new direction to re-enter transport loop.
                    u = scatt(u)
                endif
            
            enddo particle_loop
        enddo ihist_loop

        ! Accumulate results and reset score arrays.
        mean_score = mean_score + score
        unc_score = unc_score + score**2

        score = 0.0
        score2 = 0.0

    enddo ibatch_loop

    ! Statistical procesing
    mean_score = mean_score/nbatch
    unc_score = (unc_score - nbatch*mean_score**2)/(nbatch-1)
    unc_score = unc_score/nbatch
    unc_score = sqrt(unc_score)

    ! Calculate relative uncertainty, to be used for the FOM.
    unc_score = unc_score/mean_score

    ! Print results to console.
    write(*,'(A,F10.5,A,F10.5,A)') 'Reflection : ', mean_score(0)/nperbatch, ' +/-', 100.0*unc_score(0), '%'

    ! To calculate the uncertainty of the absorption we need to combine the uncertainty of the deposition 
    ! in each region.
    write(*,'(A,F10.5,A,F10.5,A)') 'Absorption : ', sum(mean_score(1:nreg))/nperbatch, ' +/-', & 
        100.0*sum(unc_score(1:nreg)), '%'
    
    write(*,'(A,F10.5,A,F10.5,A)') 'Transmission : ', mean_score(nreg+1)/nperbatch, ' +/-', &
        100.0*unc_score(nreg+1), '%'

    ! Get elapsed time
    call cpu_time(end_time)
    write(*,'(A,F15.5)') 'Elapsed time (s) : ', end_time - start_time

    ! Now calculate figure of merit for transmission.
    fom = 1.0/(unc_score(nreg+1)**2*(end_time - start_time))
    write(*,'(A,F15.5)') 'Figure of merit (FOM) : ', fom

end program shield_1d