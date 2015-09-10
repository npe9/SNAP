PROGRAM snap_main

!-----------------------------------------------------------------------
!
!    SNAP - SN Application Proxy
!
!    Parallel programming model based on PARTISN
!
!
!    SNAP: SN (Discrete Ordinates) Application Proxy
!    Version 1.x (C13087)
!    LA-CC-13-016
!
!    This code is Unclassified, and contains no Unclassified Controlled
!    Nuclear Information
!
!    Copyright (c) 2013, Los Alamos National Security, LLC
!    All rights reserved.
!
!    Copyright 2013. Los Alamos National Security, LLC. This software
!    was produced under U.S. Government contract DE-AC52-06NA25396 for
!    Los Alamos National Laboratory (LANL), which is operated by Los
!    Alamos National Security, LLC for the U.S. Department of Energy.
!    The U.S. Government has rights to use, reproduce, and distribute
!    this software. NEITHER THE GOVERNMENT NOR LOS ALAMOS NATIONAL
!    SECURITY, LLC MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
!    ANY LIABILITY FOR THE USE OF THIS SOFTWARE. If software is
!    modified to produce derivative works, such modified software should
!    be clearly marked, so as not to confuse it with the version
!    available from LANL.
!
!    Additionally, redistribution and use in source and binary forms,
!    with or without modification, are permitted provided that the
!    following conditions are met:
!    --Redistributions of source code must retain the above copyright
!      notice, this list of conditions and the following disclaimer.
!    --Redistributions in binary form must reproduce the above copyright
!      notice, this list of conditions and the following disclaimer in
!      the documentation and/or other materials provided with the
!      distribution.
!    --Neither the name of Los Alamos National Security, LLC, Los Alamos
!      National Laboratory, LANL, the U.S. Government, nor the names of
!      its contributors may be used to endorse or promote products
!      derived from this software without specific prior written
!      permission.
!
!    THIS SOFTWARE IS PROVIDED BY LOS ALAMOS NATIONAL SECURITY, LLC AND
!    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL LOS ALAMOS NATIONAL SECURITY, LLC OR
!    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
!    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
!    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!    USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
!    OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
!    SUCH DAMAGE.
!
!-----------------------------------------------------------------------

  USE global_module, ONLY: i_knd, r_knd, ifile, ofile, segment, iunit, ounit

  USE utils_module

  USE version_module, ONLY: version_print

  USE input_module, ONLY: read_input

  USE setup_module, ONLY: setup

  USE output_module, ONLY: output

  USE time_module, ONLY: tsnap, wtime, tparset, time_summ, tgrind

  USE dealloc_module

  USE plib_module, ONLY: pinit, iproc, root, comm_snap, bcast,         &
    pcomm_set, pinit_omp

  USE control_module, ONLY: otrdone, swp_typ

  IMPLICIT NONE
!_______________________________________________________________________
!
! Local variables
!_______________________________________________________________________

  CHARACTER(LEN=1) :: star='*'

  CHARACTER(LEN=64) :: error

  INTEGER(i_knd) :: ierr, i

  REAL(r_knd) :: t1, t2, t3, t4, t5
!_______________________________________________________________________
!
! Perform calls that set up the parallel environment in MPI and
! OpenMP. Also starts the timer. Update parallel setup time.
!_______________________________________________________________________
  call mmap_off()
  ierr = 0
  error = ' '
  CALL pinit ( t1 )

  CALL wtime ( t2 )
  write (*, *) 'successfully wtimed'
  write (*, *) 'tparset ',tparset
  write (*, *) 't2 ', t2
  write (*, *) 't1 ', t1
  tparset = tparset + t2 - t1
!_______________________________________________________________________
!
! Read the command line arguments to get i/o file names. Open the two
! files.
!_______________________________________________________________________


  CALL cmdarg ( ierr, error )

  CALL share_init( iproc, segment )

  CALL bcast ( ierr, comm_snap, root )
  IF ( ierr /= 0 ) THEN
    CALL print_error ( 0, error )
    CALL stop_run ( 0, 0, 0, 0 )
  END IF
 
  IF (ifile == 'stdin') THEN
    iunit = 5
  ELSE
    CALL open_file ( iunit, ifile, 'OLD', 'READ', ierr, error )
  END IF

  CALL bcast ( ierr, comm_snap, root )
  IF ( ierr /= 0 ) THEN
    CALL print_error ( 0, error )
    CALL stop_run ( 0, 0, 0, 0 )
  END IF
  IF (ofile == 'stdout') THEN
    ounit = 6
  ELSE
    CALL open_file ( ounit, ofile, 'REPLACE', 'WRITE', ierr, error )
END IF
!!WRITE (*, *) 'second bcasting'
!!WRITE (*, *) 'second bcasting'
!!WRITE (*, *) 'second bcasting'
!!WRITE (*, *) 'second bcasting'
  CALL bcast ( ierr, comm_snap, root )
  IF ( ierr /= 0 ) THEN
!!      WRITE (*, *) 'second bcast err'
!!      WRITE (*, *) 'second bcast err'
!!      WRITE (*, *) 'second bcast err'
!!      WRITE (*, *) 'second bcast err'
    CALL print_error ( 0, error )
    CALL stop_run ( 0, 0, 0, 0 )
  END IF
!_______________________________________________________________________
!
! Write code version and execution time to output.
!_______________________________________________________________________

  IF ( iproc == root ) CALL version_print
!_______________________________________________________________________
!
! Read input
!_______________________________________________________________________
  IF (ifile /= 'stdin') THEN
   CALL read_input
  END IF

!!  WRITE (*,*) "close file"
!!  WRITE (*,*) "close file"
!!  WRITE (*,*) "close file"
!!  WRITE (*,*) "close file"
  CALL close_file ( iunit, ierr, error )
  
!!  WRITE (*,*) "bcast"
!!  WRITE (*,*) "bcast"
!!  WRITE (*,*) "bcast"
!!  WRITE (*,*) "bcast"
  CALL bcast ( ierr, comm_snap, root )
  IF ( ierr /= 0 ) THEN
    CALL print_error ( ounit, error )
    CALL stop_run ( 0, 0, 0, 0 )
  END IF
!_______________________________________________________________________
!
! Get nthreads for each proc. Print the warning about resetting nthreads
! if necessary. Don't stop run. Set up the SDD MPI topology.
!_______________________________________________________________________

!!  WRITE (*,*) "wtime"
!!  WRITE (*,*) "wtime"
!!  WRITE (*,*) "wtime"
!!  WRITE (*,*) "wtime"
  CALL wtime ( t3 )

  CALL pinit_omp ( ierr, error )
  IF ( ierr /= 0 ) CALL print_error ( 0, error )

  CALL pcomm_set

!!  WRITE (*,*) "pcomm_set"
!!  WRITE (*,*) "pcomm_set"
!!  WRITE (*,*) "pcomm_set"
!!  WRITE (*,*) "pcomm_set"
  CALL wtime ( t4 )
  tparset = tparset + t4 - t3
!_______________________________________________________________________
!
! Setup problem
!_______________________________________________________________________
  !!write (*,*) "setting up" 
  !!write (*,*) "setting up" 
  CALL setup
  
!!  !WRITE (*,*) "set up"
!!  !WRITE (*,*) "set up"
!!  !WRITE (*,*) "set up"
!!  !WRITE (*,*) "set up"
!_______________________________________________________________________
!
! Call for the problem solution
!_______________________________________________________________________

!!  !  WRITE (*, *) 'translving'
!!  !  WRITE (*, *) 'translving'
!!  !  WRITE (*, *) 'translving'
!!  !  WRITE (*, *) 'translving'
  CALL translv
!! WRITE (*, *) 'translved'
!! WRITE (*, *) 'translved'
!! WRITE (*, *) 'translved'
!! WRITE (*, *) 'translved'
!_______________________________________________________________________
!
! Output the results. Print the timing summary.
!_______________________________________________________________________

!!    WRITE (*, *) 'outputting'
!!    WRITE (*, *) 'outputting'
!!    WRITE (*, *) 'outputting'
!!    WRITE (*, *) 'outputting'
  CALL output
!!  WRITE (*, *) 'outputted'
!!  WRITE (*, *) 'outputted'
!!  WRITE (*, *) 'outputted'
!!  WRITE (*, *) 'outputted'
!!!  WRITE (*, *) 'time summing'
!!!  WRITE (*, *) 'time summing'
!!!  WRITE (*, *) 'time summing'
!!!  WRITE (*, *) 'time summing'
  IF ( iproc == root ) CALL time_summ
!!!  WRITE (*, *) 'time summed'
!!!  WRITE (*, *) 'time summed'
!!!  WRITE (*, *) 'time summed'
!!!  WRITE (*, *) 'time summed'
!_______________________________________________________________________
!
! Final cleanup: deallocate, close output file, end the program
!_______________________________________________________________________

!!    WRITE (*, *) 'deallocating input'
!!    WRITE (*, *) 'deallocating input'
!!    WRITE (*, *) 'deallocating input'
!!    WRITE (*, *) 'deallocating input'
  CALL dealloc_input ( 3 )
  CALL dealloc_solve ( swp_typ, 3 )

  CALL wtime ( t5 )
!!  WRITE (*, *) 'wtimed'
!!  WRITE (*, *) 'wtimed'
!!  WRITE (*, *) 'wtimed'
!!  WRITE (*, *) 'wtimed'
  tsnap = t5 - t1

!!  WRITE (*, *) 'writing the snap data'
!!  WRITE (*, *) 'writing the snap data'
!!  WRITE (*, *) 'writing the snap data'
!!  WRITE (*, *) 'writing the snap data'
  IF ( iproc == root ) THEN
    WRITE( ounit, 501 ) tsnap
    WRITE( ounit, 502 ) tgrind, ( star, i = 1, 80 )
  END IF

!!  WRITE (*, *) 'finalizing'
!!  WRITE (*, *) 'finalizing'
!!  WRITE (*, *) 'finalizing'
!!  WRITE (*, *) 'finalizing'
  CALL finalize
!!  WRITE (*, *) 'finalized'
!!  WRITE (*, *) 'finalized'
!!  WRITE (*, *) 'finalized'
!!  WRITE (*, *) 'finalized'
!!  WRITE (*, *) 'closing file'
!!  WRITE (*, *) 'closing file'
!!  WRITE (*, *) 'closing file'
!!  WRITE (*, *) 'closing file'
  !CALL close_file ( ounit, ierr, error )
!!  WRITE (*, *) 'closed file'
!!  WRITE (*, *) 'closed file'
!!  WRITE (*, *) 'closed file'
!!  WRITE (*, *) 'closed file'
  CALL bcast ( ierr, comm_snap, root )
  IF ( ierr /= 0 ) THEN
    CALL print_error ( 0, error )
    CALL stop_run ( 1, 0, 0, 0 )
  END IF

!!  WRITE (*, *) 'otrdone'
!!  WRITE (*, *) 'otrdone'
!!  WRITE (*, *) 'otrdone'
!!  WRITE (*, *) 'otrdone'
  IF ( otrdone ) THEN
    CALL stop_run ( 1, 0, 0, 1 )
  ELSE
    CALL stop_run ( 1, 0, 0, 2 )
  END IF
!_______________________________________________________________________

  501 FORMAT( 2X, 'Total Execution time', T41, ES11.4, / )
  502 FORMAT( 2X, 'Grind Time (nanoseconds)', 8X, ES11.4, /, /, 80A )
!_______________________________________________________________________
!_______________________________________________________________________

END PROGRAM snap_main
