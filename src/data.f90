!-----------------------------------------------------------------------
!
! MODULE: data_module
!> @brief
!> This module contains the variables and setup subroutines for the mock
!> cross section data. It establishes the number of groups and constructs
!> the cross section arrays.
!
!-----------------------------------------------------------------------

MODULE data_module

  USE global_module, ONLY: i_knd, r_knd, zero, segment

  USE sn_module, ONLY: cmom

  USE iso_c_binding

  IMPLICIT NONE

  PUBLIC

  SAVE
  !_______________________________________________________________________
  !
  ! Module Input Variables
  !
  ! ng       - number of groups
  ! mat_opt  - material layout, 0/1/2=homogeneous/center/corner, with
  !            two materials, and material 2 nowhere/center/corner
  ! src_opt  - source layout, 0/1/2=homogenous/src-center/src-corner=
  !            source everywhere/center of problem/corner, strength=10.0
  ! scatp    - 0/1=no/yes print the full scattering matrix to file 'slgg'
  !_______________________________________________________________________

  INTEGER(i_knd) :: ng=1, mat_opt=0, src_opt=0, scatp=0
  !_______________________________________________________________________
  !
  ! Run-time variables
  !
  ! v(ng)         - mock velocity array
  ! nmat          - number of materials
  ! mat(nx,ny,nz) - material identifier array
  !
  ! qi(nx,ny,nz,ng)             - fixed source array for src_opt<3
  ! qim(nang,nx,ny,nz,noct,ng)  - fixed source array for src_opt>=3
  !
  ! sigt(nmat,ng)          - total interaction
  ! siga(nmat,ng)          - absorption
  ! sigs(nmat,ng)          - scattering, total
  ! slgg(nmat,nmom,ng,ng)  - scattering matrix, all moments/groups
  ! vdelt(ng)              - time-absorption coefficient
  !_______________________________________________________________________

  INTEGER(i_knd) :: nmat=1

  INTEGER(i_knd), POINTER, DIMENSION(:,:,:) :: mat

  REAL(r_knd), POINTER, DIMENSION(:) :: v, vdelt

  REAL(r_knd), POINTER, DIMENSION(:,:) :: sigt, siga, sigs

  REAL(r_knd), POINTER, DIMENSION(:,:,:,:) :: qi, slgg

  REAL(r_knd), POINTER, DIMENSION(:,:,:,:,:,:) :: qim

  INTEGER(c_size_t) array_size

  type(c_ptr) :: cptr_in

CONTAINS


  SUBROUTINE data_allocate ( nx, ny, nz, nmom, nang, noct, timedep,    &
       istat )

    !-----------------------------------------------------------------------
    !
    ! Allocate data_module arrays.
    !
    !-----------------------------------------------------------------------

    INTEGER(i_knd), INTENT(IN) :: nx, ny, nz, nmom, nang, noct, timedep

    INTEGER(i_knd), INTENT(INOUT) :: istat
    !_______________________________________________________________________
    !
    !   Establish number of materials according to mat_opt
    !_______________________________________________________________________

    IF ( mat_opt > 0 ) nmat = 2
    !_______________________________________________________________________
    !
    !   Allocate velocities
    !_______________________________________________________________________

    istat = 0

    CALL CREATE_SHARED(noct, cmom, nmat)

    IF ( timedep == 1 ) THEN
       array_size = ng
       CALL ALLOCATE_V(array_size, cptr_in, 0)
       CALL C_F_POINTER(cptr_in, v, [ng])
    ELSE
       ALLOCATE( v(0), STAT=istat )
    END IF
    IF ( istat /= 0 ) RETURN
!    write (*,*) 'data_alloc before v ', v
!    write (*,*) 'data_alloc before v ', v
    v = zero
!    write (*,*) 'data_alloc after v ', v
!    write (*,*) 'data_alloc after v ', v
    !_______________________________________________________________________
    !
    !   Allocate the material identifier array. ny and nz are 1 if not
    !   2-D/3-D.
    !_______________________________________________________________________

    array_size = nx*ny*nz
    ALLOCATE(mat(nx,ny,nz), STAT=istat)

    IF ( istat /= 0 ) RETURN

    mat = 1
    !_______________________________________________________________________
    !
    !   Allocate the fixed source array. If src_opt < 3, allocate the qi
    !   array, not the qim. Do the opposite (store the full angular copy) of
    !   the source, qim, if src_opt>=3 (MMS). Allocate array not used to 0.
    !   ny and nz are 1 if not 2-D/3-D.
    !_______________________________________________________________________

    IF ( src_opt < 3 ) THEN
!       write (*,*) 'src_opt < 3'
!       write (*,*) 'src_opt < 3'
       ALLOCATE(qi(nx,ny,nz,ng), STAT=istat)
       IF ( istat /= 0 ) RETURN
       ALLOCATE(qim(0,0,0,0,0,0), STAT=istat )
       IF ( istat /= 0 ) RETURN
       qi = zero
    ELSE
!       write (*,*) 'src_opt >= 3'
!       write (*,*) 'src_opt >= 3'
       ALLOCATE(qi(nx,ny,nz,ng), STAT=istat)
       IF ( istat /= 0 ) RETURN
       ALLOCATE(qim(nang,nx,ny,nz,noct,ng), STAT=istat)
       IF ( istat /= 0 ) RETURN

       qi = zero
       qim = zero
    END IF
    !_______________________________________________________________________
    !
    !   Allocate mock cross sections
    !_______________________________________________________________________
    ALLOCATE( sigt(nmat,ng), siga(nmat,ng), sigs(nmat,ng), slgg(nmat,nmom,ng,ng), STAT=istat )
    IF ( istat /= 0 ) RETURN
    sigt = zero
    siga = zero
    sigs = zero
    slgg = zero
    !_______________________________________________________________________
    !
    !   Allocate the vdelt array
    !_______________________________________________________________________

    ALLOCATE( vdelt(ng), STAT=istat )
    IF ( istat /= 0 ) RETURN

    vdelt = zero
    !_______________________________________________________________________
    !_______________________________________________________________________

  END SUBROUTINE data_allocate


  SUBROUTINE data_deallocate

    !-----------------------------------------------------------------------
    !
    ! Deallocate data_module arrays.
    !
    !-----------------------------------------------------------------------
    !_______________________________________________________________________

!!    !WRITE (*,*) 'dealloc v'
!!    !WRITE (*,*) 'dealloc v'
!!    !WRITE (*,*) 'dealloc v'
!!    !WRITE (*,*) 'dealloc v'
    !    DEALLOCATE( v )
!!    !WRITE (*,*) 'dealloc mat'
!!    !WRITE (*,*) 'dealloc mat'
!!    !WRITE (*,*) 'dealloc mat'
!!    !WRITE (*,*) 'dealloc mat'
    ! I think this needs the C call
    !DEALLOCATE( mat )
!!    !WRITE (*,*) 'dealloc qi qim'
!!    !WRITE (*,*) 'dealloc qi qim'
!!    !WRITE (*,*) 'dealloc qi qim'
!!    !WRITE (*,*) 'dealloc qi qim'
    !DEALLOCATE( qi, qim )
!!    !WRITE (*,*) 'dealloc sigt siga sigs'
!!    !WRITE (*,*) 'dealloc sigt siga sigs'
!!    !WRITE (*,*) 'dealloc sigt siga sigs'
!!    !WRITE (*,*) 'dealloc sigt siga sigs'
    !DEALLOCATE( sigt, siga, sigs )
!!    !WRITE (*,*) 'dealloc slgg'
!!    !WRITE (*,*) 'dealloc slgg'
!!    !WRITE (*,*) 'dealloc slgg'
!!    !WRITE (*,*) 'dealloc slgg'
    !DEALLOCATE( slgg )
!!    !WRITE (*,*) 'dealloc vdelt'
!!    !WRITE (*,*) 'dealloc vdelt'
!!    !WRITE (*,*) 'dealloc vdelt'
!!    !WRITE (*,*) 'dealloc vdelt'
    !DEALLOCATE( vdelt )
    !_______________________________________________________________________
    !_______________________________________________________________________

  END SUBROUTINE data_deallocate


END MODULE data_module
