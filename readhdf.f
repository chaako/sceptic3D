c*********************************************************************
c NB: Any changes must be mirrored in outputhdf.f
c*********************************************************************
c Reads the hdf output file
      subroutine readhdf(dt,k,fave,icolntype,colnwt,filename)
c Load the hdf5 module
      use hdf5
c Don't allow implicit definitions
      implicit none
c Input variables
      real dt, fave, colnwt
      integer k, icolntype
c Common data
      include 'piccom.f'
      include 'errcom.f'
      include 'colncom.f'
c Functions
      integer nbcat
c Local variables
c     File, group, and dataset names
      CHARACTER(LEN=155) :: filename
      CHARACTER(LEN=55) :: groupname
      CHARACTER(LEN=55) :: dsetname
c     File and group ids
      INTEGER(HID_T) :: file_id
      INTEGER(HID_T) :: group_id
c     Error flag
      INTEGER :: error
c     Dimensions of dataset (limit rank to <=10)
      INTEGER(HSIZE_T), DIMENSION(10) :: data_dims, storage_dims
c     Rank of dataset
      INTEGER :: rank
c     Working variables
      INTEGER :: i, j
      integer idf

c Initialize FORTRAN interface.
      CALL h5open_f(error)

c Open file
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)

c Now read each desired common block as an hdf group, using the two
c   custom made functions readhdfintmat() and readhdfdrealmat() to
c   read integer and real (respectively) variables and arrays.
c   The logical type is treated as integer.
c   Note that size of each array must be specified throught the rank,
c   dimensions of the used array (data_dims), and dimensions of the
c   full arary in memory (storage_dims).



c Open a group in the file.
      groupname = 'piccom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'npart'
      call readhdfintmat(group_id,dsetname,
     $ npart,storage_dims,data_dims,rank)

      dsetname = 'nr'
      call readhdfintmat(group_id,dsetname,
     $ nr,storage_dims,data_dims,rank)

      dsetname = 'nth'
      call readhdfintmat(group_id,dsetname,
     $ nth,storage_dims,data_dims,rank)

      dsetname = 'npsi'
      call readhdfintmat(group_id,dsetname,
     $ npsi,storage_dims,data_dims,rank)

      dsetname = 'lsubcycle'
      call readhdfintmat(group_id,dsetname,
     $ lsubcycle,storage_dims,data_dims,rank)

      dsetname = 'verlet'
      call readhdfintmat(group_id,dsetname,
     $ verlet,storage_dims,data_dims,rank)

      dsetname = 'lfulloutput'
      call readhdfintmat(group_id,dsetname,
     $ lfulloutput,storage_dims,data_dims,rank)

c      dsetname = 'LCIC'
c      call readhdfintmat(group_id,dsetname,
c     $ LCIC,storage_dims,data_dims,rank)

      dsetname = 'collcic'
      call readhdfintmat(group_id,dsetname,
     $ collcic,storage_dims,data_dims,rank)

      dsetname = 'NRUSED'
      call readhdfintmat(group_id,dsetname,
     $ NRUSED,storage_dims,data_dims,rank)

      dsetname = 'NTHUSED'
      call readhdfintmat(group_id,dsetname,
     $ NTHUSED,storage_dims,data_dims,rank)

      dsetname = 'NPSIUSED'
      call readhdfintmat(group_id,dsetname,
     $ NPSIUSED,storage_dims,data_dims,rank)

      dsetname = 'NRFULL'
      call readhdfintmat(group_id,dsetname,
     $ NRFULL,storage_dims,data_dims,rank)

      dsetname = 'NTHFULL'
      call readhdfintmat(group_id,dsetname,
     $ NTHFULL,storage_dims,data_dims,rank)

      dsetname = 'NPSIFULL'
      call readhdfintmat(group_id,dsetname,
     $ NPSIFULL,storage_dims,data_dims,rank)

      dsetname = 'ninjcomp'
      call readhdfintmat(group_id,dsetname,
     $ ninjcomp,storage_dims,data_dims,rank)

      dsetname = 'iocprev'
      call readhdfintmat(group_id,dsetname,
     $ iocprev,storage_dims,data_dims,rank)

      dsetname = 'cerr'
      call readhdfrealmat(group_id,dsetname,
     $ cerr,storage_dims,data_dims,rank)

      dsetname = 'bdyfc'
      call readhdfrealmat(group_id,dsetname,
     $ bdyfc,storage_dims,data_dims,rank)

      dsetname = 'Ti'
      call readhdfrealmat(group_id,dsetname,
     $ Ti,storage_dims,data_dims,rank)

      dsetname = 'vd'
      call readhdfrealmat(group_id,dsetname,
     $ vd,storage_dims,data_dims,rank)

      dsetname = 'cd'
      call readhdfrealmat(group_id,dsetname,
     $ cd,storage_dims,data_dims,rank)

      dsetname = 'cB'
      call readhdfrealmat(group_id,dsetname,
     $ cB,storage_dims,data_dims,rank)

      dsetname = 'Bz'
      call readhdfrealmat(group_id,dsetname,
     $ Bz,storage_dims,data_dims,rank)

      dsetname = 'diags'
      call readhdfintmat(group_id,dsetname,
     $ diags,storage_dims,data_dims,rank)

      dsetname = 'lplot'
      call readhdfintmat(group_id,dsetname,
     $ lplot,storage_dims,data_dims,rank)

      dsetname = 'ldist'
      call readhdfintmat(group_id,dsetname,
     $ ldist,storage_dims,data_dims,rank)

      dsetname = 'linsulate'
      call readhdfintmat(group_id,dsetname,
     $ linsulate,storage_dims,data_dims,rank)

      dsetname = 'lfloat'
      call readhdfintmat(group_id,dsetname,
     $ lfloat,storage_dims,data_dims,rank)

      dsetname = 'lat0'
      call readhdfintmat(group_id,dsetname,
     $ lat0,storage_dims,data_dims,rank)

      dsetname = 'lap0'
      call readhdfintmat(group_id,dsetname,
     $ lap0,storage_dims,data_dims,rank)

      dsetname = 'localinj'
      call readhdfintmat(group_id,dsetname,
     $ localinj,storage_dims,data_dims,rank)

      dsetname = 'lfixedn'
      call readhdfintmat(group_id,dsetname,
     $ lfixedn,storage_dims,data_dims,rank)

      dsetname = 'myid'
      call readhdfintmat(group_id,dsetname,
     $ myid,storage_dims,data_dims,rank)

      dsetname = 'numprocs'
      call readhdfintmat(group_id,dsetname,
     $ numprocs,storage_dims,data_dims,rank)

      dsetname = 'rmtoz'
      call readhdfrealmat(group_id,dsetname,
     $ rmtoz,storage_dims,data_dims,rank)

      dsetname = 'lphiavg'
      call readhdfintmat(group_id,dsetname,
     $ lphiavg,storage_dims,data_dims,rank)

      dsetname = 'ltrsiphon'
      call readhdfintmat(group_id,dsetname,
     $ ltrsiphon,storage_dims,data_dims,rank)

      dsetname = 'trsiphon'
      call readhdfrealmat(group_id,dsetname,
     $ trsiphon,storage_dims,data_dims,rank)

c     Variable arrays
      dsetname = 'xp'
      rank = 2
      data_dims(1) = ndim
      data_dims(2) = npart
      storage_dims(1) = ndim
      storage_dims(2) = npartmax
      call readhdfrealmat(group_id,dsetname,
     $  xp,storage_dims,data_dims,rank)

      dsetname = 'ivelreset'
      rank = 1
      data_dims(1) = npart
      storage_dims(1) = npartmax
      call readhdfintmat(group_id,dsetname,
     $  ivelreset,storage_dims,data_dims,rank)

c Disregard rest if slave and only storing particles
      if (myid.gt.0 .and. lpartonly) goto 999

      dsetname = 'vzinit'
      rank = 1
      data_dims(1) = npart
      storage_dims(1) = npartmax
      call readhdfrealmat(group_id,dsetname,
     $  vzinit,storage_dims,data_dims,rank)

      dsetname = 'dtprec'
      rank = 1
      data_dims(1) = npart
      storage_dims(1) = npartmax
      call readhdfrealmat(group_id,dsetname,
     $  dtprec,storage_dims,data_dims,rank)

      dsetname = 'ipf'
      rank = 1
      data_dims(1) = npart
      storage_dims(1) = npartmax
      call readhdfintmat(group_id,dsetname,
     $  ipf,storage_dims,data_dims,rank)

      dsetname = 'phi'
      rank = 3
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      data_dims(3) = npsi+1
      storage_dims(1) = nrsize+1
      storage_dims(2) = nthsize+1
      storage_dims(3) = npsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  phi,storage_dims,data_dims,rank)

c     Indexing is reversed compared to h5dump
c      write(*,*) phi(6,0,0), 'should be 0.0124691'
c      write(*,*) phi(6,5,3), 'should be -0.00164074'

      dsetname = 'phiavg'
      rank = 3
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      data_dims(3) = npsi+1
      storage_dims(1) = nrsize+1
      storage_dims(2) = nthsize+1
      storage_dims(3) = npsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  phiavg,storage_dims,data_dims,rank)

      dsetname = 'phiaxis'
      rank = 2
      data_dims(1) = nr+1
      data_dims(2) = npsi+1
      storage_dims(1) = nrsize
      storage_dims(2) = npsisize
      call readhdfrealmat(group_id,dsetname,
     $  phiaxis,storage_dims,data_dims,rank)

      dsetname = 'rho'
      rank = 3
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      data_dims(3) = npsi+1
      storage_dims(1) = nrsize+1
      storage_dims(2) = nthsize+1
      storage_dims(3) = npsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  rho,storage_dims,data_dims,rank)

      dsetname = 'rhoDiag'
      rank = 3
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      data_dims(3) = npsi+1
      storage_dims(1) = nrsize+1
      storage_dims(2) = nthsize+1
      storage_dims(3) = npsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  rhoDiag,storage_dims,data_dims,rank)

      dsetname = 'Bvect'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  Bvect,storage_dims,data_dims,rank)

      dsetname = 'drvect'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  drvect,storage_dims,data_dims,rank)

      dsetname = 'magdir'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  magdir,storage_dims,data_dims,rank)

      dsetname = 'ecbdrift'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  ecbdrift,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'momcom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

c      dsetname = 'nx'
c      call readhdfintmat(group_id,dsetname,
c     $ nx,storage_dims,data_dims,rank)

c      dsetname = 'ny'
c      call readhdfintmat(group_id,dsetname,
c     $ ny,storage_dims,data_dims,rank)

c      dsetname = 'nz'
c      call readhdfintmat(group_id,dsetname,
c     $ nz,storage_dims,data_dims,rank)

c      dsetname = 'nvx'
c      call readhdfintmat(group_id,dsetname,
c     $ nvx,storage_dims,data_dims,rank)

c      dsetname = 'nvy'
c      call readhdfintmat(group_id,dsetname,
c     $ nvy,storage_dims,data_dims,rank)

c      dsetname = 'nvz'
c      call readhdfintmat(group_id,dsetname,
c     $ nvz,storage_dims,data_dims,rank)

c     Variable arrays
      rank = 3
      data_dims(1) = nr
      data_dims(2) = nth
      data_dims(3) = npsi
      storage_dims(1) = nrsize-1
      storage_dims(2) = nthsize-1
      storage_dims(3) = npsisize-1

      dsetname = 'psum'
      call readhdfrealmat(group_id,dsetname,
     $  psum,storage_dims,data_dims,rank)

      dsetname = 'vrsum'
      call readhdfrealmat(group_id,dsetname,
     $  vrsum,storage_dims,data_dims,rank)

      dsetname = 'vtsum'
      call readhdfrealmat(group_id,dsetname,
     $  vtsum,storage_dims,data_dims,rank)

      dsetname = 'vpsum'
      call readhdfrealmat(group_id,dsetname,
     $  vpsum,storage_dims,data_dims,rank)

      dsetname = 'vr2sum'
      call readhdfrealmat(group_id,dsetname,
     $  vr2sum,storage_dims,data_dims,rank)

      dsetname = 'vt2sum'
      call readhdfrealmat(group_id,dsetname,
     $  vt2sum,storage_dims,data_dims,rank)

      dsetname = 'vp2sum'
      call readhdfrealmat(group_id,dsetname,
     $  vp2sum,storage_dims,data_dims,rank)

      dsetname = 'vrtsum'
      call readhdfrealmat(group_id,dsetname,
     $  vrtsum,storage_dims,data_dims,rank)

      dsetname = 'vrpsum'
      call readhdfrealmat(group_id,dsetname,
     $  vrpsum,storage_dims,data_dims,rank)

      dsetname = 'vtpsum'
      call readhdfrealmat(group_id,dsetname,
     $  vtpsum,storage_dims,data_dims,rank)

      dsetname = 'vxsum'
      call readhdfrealmat(group_id,dsetname,
     $  vxsum,storage_dims,data_dims,rank)

      dsetname = 'vysum'
      call readhdfrealmat(group_id,dsetname,
     $  vysum,storage_dims,data_dims,rank)

      dsetname = 'vzsum'
      call readhdfrealmat(group_id,dsetname,
     $  vzsum,storage_dims,data_dims,rank)

      dsetname = 'pDiag'
      call readhdfrealmat(group_id,dsetname,
     $  pDiag,storage_dims,data_dims,rank)

      dsetname = 'vrDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vrDiag,storage_dims,data_dims,rank)

      dsetname = 'vtDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vtDiag,storage_dims,data_dims,rank)

      dsetname = 'vpDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vpDiag,storage_dims,data_dims,rank)

      dsetname = 'vr2Diag'
      call readhdfrealmat(group_id,dsetname,
     $  vr2Diag,storage_dims,data_dims,rank)

      dsetname = 'vt2Diag'
      call readhdfrealmat(group_id,dsetname,
     $  vt2Diag,storage_dims,data_dims,rank)

      dsetname = 'vp2Diag'
      call readhdfrealmat(group_id,dsetname,
     $  vp2Diag,storage_dims,data_dims,rank)

      dsetname = 'vrtDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vrtDiag,storage_dims,data_dims,rank)

      dsetname = 'vrpDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vrpDiag,storage_dims,data_dims,rank)

      dsetname = 'vtpDiag'
      call readhdfrealmat(group_id,dsetname,
     $  vtpDiag,storage_dims,data_dims,rank)

      rank = 4
      data_dims(2) = nx
      data_dims(3) = ny
      data_dims(4) = nz
      storage_dims(2) = nx
      storage_dims(3) = ny
      storage_dims(4) = nz

      dsetname = 'vxdistf'
      data_dims(1) = nvx
      storage_dims(1) = nvx
      call readhdfintmat(group_id,dsetname,
     $  vxdistf,storage_dims,data_dims,rank)

      dsetname = 'vydistf'
      data_dims(1) = nvy
      storage_dims(1) = nvy
      call readhdfintmat(group_id,dsetname,
     $  vydistf,storage_dims,data_dims,rank)

      dsetname = 'vzdistf'
      data_dims(1) = nvz
      storage_dims(1) = nvz
      call readhdfintmat(group_id,dsetname,
     $  vzdistf,storage_dims,data_dims,rank)

      dsetname = 'vxbins'
      rank = 1
      data_dims(1) = nvx+1
      storage_dims(1) = nvx+1
      call readhdfrealmat(group_id,dsetname,
     $  vxbins,storage_dims,data_dims,rank)

      dsetname = 'vybins'
      rank = 1
      data_dims(1) = nvy+1
      storage_dims(1) = nvy+1
      call readhdfrealmat(group_id,dsetname,
     $  vybins,storage_dims,data_dims,rank)

      dsetname = 'vzbins'
      rank = 1
      data_dims(1) = nvz+1
      storage_dims(1) = nvz+1
      call readhdfrealmat(group_id,dsetname,
     $  vzbins,storage_dims,data_dims,rank)

      dsetname = 'curr'
      rank = 1
      data_dims(1) = 4
      storage_dims(1) = 4
      call readhdfrealmat(group_id,dsetname,
     $  curr,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'meshcom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'rfac'
      call readhdfrealmat(group_id,dsetname,
     $ rfac,storage_dims,data_dims,rank)

      dsetname = 'tfac'
      call readhdfrealmat(group_id,dsetname,
     $ tfac,storage_dims,data_dims,rank)

      dsetname = 'pfac'
      call readhdfrealmat(group_id,dsetname,
     $ pfac,storage_dims,data_dims,rank)

      dsetname = 'avelim'
      call readhdfrealmat(group_id,dsetname,
     $ avelim,storage_dims,data_dims,rank)

      dsetname = 'cgparallel'
      call readhdfintmat(group_id,dsetname,
     $ cgparallel,storage_dims,data_dims,rank)

      dsetname = 'idim1'
      call readhdfintmat(group_id,dsetname,
     $ idim1,storage_dims,data_dims,rank)

      dsetname = 'idim2'
      call readhdfintmat(group_id,dsetname,
     $ idim2,storage_dims,data_dims,rank)

      dsetname = 'idim3'
      call readhdfintmat(group_id,dsetname,
     $ idim3,storage_dims,data_dims,rank)

c     Variable arrays
      dsetname = 'r'
      rank = 1
      data_dims(1) = nr+1
      storage_dims(1) = nrsize+1
      call readhdfrealmat(group_id,dsetname,
     $  r,storage_dims,data_dims,rank)

      dsetname = 'rcc'
      rank = 1
      data_dims(1) = nr+1
      storage_dims(1) = nrsize+1
      call readhdfrealmat(group_id,dsetname,
     $  rcc,storage_dims,data_dims,rank)

      dsetname = 'th'
      rank = 1
      data_dims(1) = nth+1
      storage_dims(1) = nthsize+1
      call readhdfrealmat(group_id,dsetname,
     $  th,storage_dims,data_dims,rank)

      dsetname = 'tcc'
      rank = 1
      data_dims(1) = nth+1
      storage_dims(1) = nthsize+1
      call readhdfrealmat(group_id,dsetname,
     $  tcc,storage_dims,data_dims,rank)

      dsetname = 'thang'
      rank = 1
      data_dims(1) = nth+1
      storage_dims(1) = nthsize+1
      call readhdfrealmat(group_id,dsetname,
     $  thang,storage_dims,data_dims,rank)

      dsetname = 'pcc'
      rank = 1
      data_dims(1) = NPSIFULL+1
      storage_dims(1) = npsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  pcc,storage_dims,data_dims,rank)

      dsetname = 'volinv'
      rank = 1
      data_dims(1) = nr+1
      storage_dims(1) = nrsize+1
      call readhdfrealmat(group_id,dsetname,
     $  volinv,storage_dims,data_dims,rank)

      dsetname = 'irpre'
      rank = 1
      data_dims(1) = nrpre
      storage_dims(1) = nrpre
      call readhdfintmat(group_id,dsetname,
     $  irpre,storage_dims,data_dims,rank)

      dsetname = 'itpre'
      rank = 1
      data_dims(1) = ntpre
      storage_dims(1) = ntpre
      call readhdfintmat(group_id,dsetname,
     $  itpre,storage_dims,data_dims,rank)

      dsetname = 'ippre'
      rank = 1
      data_dims(1) = nppre
      storage_dims(1) = nppre
      call readhdfintmat(group_id,dsetname,
     $  ippre,storage_dims,data_dims,rank)

      dsetname = 'hr'
      rank = 1
      data_dims(1) = nr+2
      storage_dims(1) = nrsize+2
      call readhdfrealmat(group_id,dsetname,
     $  hr,storage_dims,data_dims,rank)

      dsetname = 'zeta'
      rank = 1
      data_dims(1) = nr+2
      storage_dims(1) = nrsize+2
      call readhdfrealmat(group_id,dsetname,
     $  zeta,storage_dims,data_dims,rank)

      dsetname = 'zetahalf'
      rank = 1
      data_dims(1) = nr+2
      storage_dims(1) = nrsize+2
      call readhdfrealmat(group_id,dsetname,
     $  zetahalf,storage_dims,data_dims,rank)

      dsetname = 'cminus'
      rank = 1
      data_dims(1) = nr
      storage_dims(1) = nrsize
      call readhdfrealmat(group_id,dsetname,
     $  cminus,storage_dims,data_dims,rank)

      dsetname = 'cmid'
      rank = 1
      data_dims(1) = nr
      storage_dims(1) = nrsize
      call readhdfrealmat(group_id,dsetname,
     $  cmid,storage_dims,data_dims,rank)

      dsetname = 'cplus'
      rank = 1
      data_dims(1) = nr
      storage_dims(1) = nrsize
      call readhdfrealmat(group_id,dsetname,
     $  cplus,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'rancom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'bcphi'
      call readhdfintmat(group_id,dsetname,
     $ bcphi,storage_dims,data_dims,rank)

      dsetname = 'bcr'
      call readhdfintmat(group_id,dsetname,
     $ bcr,storage_dims,data_dims,rank)

      dsetname = 'infdbl'
      call readhdfintmat(group_id,dsetname,
     $ infdbl,storage_dims,data_dims,rank)

c     Variable arrays
      dsetname = 'Qcom'
      rank = 1
      data_dims(1) = nQth
      storage_dims(1) = nQth
      call readhdfrealmat(group_id,dsetname,
     $  Qcom,storage_dims,data_dims,rank)

      dsetname = 'Gcom'
      rank = 2
      data_dims(1) = nvel
      data_dims(2) = nQth
      storage_dims(1) = nvel
      storage_dims(2) = nQth
      call readhdfrealmat(group_id,dsetname,
     $  Gcom,storage_dims,data_dims,rank)

      dsetname = 'Vcom'
      rank = 1
      data_dims(1) = nvel
      storage_dims(1) = nvel
      call readhdfrealmat(group_id,dsetname,
     $  Vcom,storage_dims,data_dims,rank)

      dsetname = 'pu1'
      rank = 1
      data_dims(1) = nvel
      storage_dims(1) = nvel
      call readhdfrealmat(group_id,dsetname,
     $  pu1,storage_dims,data_dims,rank)

      dsetname = 'pu2'
      rank = 1
      data_dims(1) = nQth
      storage_dims(1) = nQth
      call readhdfrealmat(group_id,dsetname,
     $  pu2,storage_dims,data_dims,rank)

      dsetname = 'Pc'
      rank = 2
      data_dims(1) = nQth
      data_dims(2) = nvel
      storage_dims(1) = nQth
      storage_dims(2) = nvel
      call readhdfrealmat(group_id,dsetname,
     $  Pc,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
c     Need to open orbtrack before diagcom for maxsteps
      groupname = 'orbtrack'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'orbinit'
      call readhdfintmat(group_id,dsetname,
     $ orbinit,storage_dims,data_dims,rank)

      dsetname = 'maxsteps'
      call readhdfintmat(group_id,dsetname,
     $ maxsteps,storage_dims,data_dims,rank)

      dsetname = 'trackinit'
      call readhdfintmat(group_id,dsetname,
     $ trackinit,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'diagcom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'nrein'
      call readhdfintmat(group_id,dsetname,
     $ nrein,storage_dims,data_dims,rank)

      dsetname = 'nreintry'
      call readhdfintmat(group_id,dsetname,
     $ nreintry,storage_dims,data_dims,rank)

      dsetname = 'ninner'
      call readhdfintmat(group_id,dsetname,
     $ ninner,storage_dims,data_dims,rank)

      dsetname = 'vrange'
      call readhdfrealmat(group_id,dsetname,
     $ vrange,storage_dims,data_dims,rank)

      dsetname = 'phiout'
      call readhdfrealmat(group_id,dsetname,
     $ phiout,storage_dims,data_dims,rank)

      dsetname = 'zmomprobe'
      call readhdfrealmat(group_id,dsetname,
     $ zmomprobe,storage_dims,data_dims,rank)

      dsetname = 'xmomprobe'
      call readhdfrealmat(group_id,dsetname,
     $ xmomprobe,storage_dims,data_dims,rank)

      dsetname = 'ymomprobe'
      call readhdfrealmat(group_id,dsetname,
     $ ymomprobe,storage_dims,data_dims,rank)

      dsetname = 'enerprobe'
      call readhdfrealmat(group_id,dsetname,
     $ enerprobe,storage_dims,data_dims,rank)

      dsetname = 'zmout'
      call readhdfrealmat(group_id,dsetname,
     $ zmout,storage_dims,data_dims,rank)

      dsetname = 'xmout'
      call readhdfrealmat(group_id,dsetname,
     $ xmout,storage_dims,data_dims,rank)

      dsetname = 'ymout'
      call readhdfrealmat(group_id,dsetname,
     $ ymout,storage_dims,data_dims,rank)

      dsetname = 'bohm'
      call readhdfintmat(group_id,dsetname,
     $ bohm,storage_dims,data_dims,rank)

      dsetname = 'spotrein'
      call readhdfrealmat(group_id,dsetname,
     $ spotrein,storage_dims,data_dims,rank)

      dsetname = 'averein'
      call readhdfrealmat(group_id,dsetname,
     $ averein,storage_dims,data_dims,rank)

      dsetname = 'fluxrein'
      call readhdfrealmat(group_id,dsetname,
     $ fluxrein,storage_dims,data_dims,rank)

      dsetname = 'rhoinf'
      call readhdfrealmat(group_id,dsetname,
     $ rhoinf,storage_dims,data_dims,rank)

      dsetname = 'ntrapre'
      call readhdfintmat(group_id,dsetname,
     $ ntrapre,storage_dims,data_dims,rank)

      dsetname = 'adeficit'
      call readhdfrealmat(group_id,dsetname,
     $ adeficit,storage_dims,data_dims,rank)

      dsetname = 'ircell'
      call readhdfintmat(group_id,dsetname,
     $ ircell,storage_dims,data_dims,rank)

      dsetname = 'itcell'
      call readhdfintmat(group_id,dsetname,
     $ itcell,storage_dims,data_dims,rank)

c     Variable arrays
      rank = 1
      data_dims(1) = nvmax
      storage_dims(1) = nvmax
      dsetname = 'nvdiag'
      call readhdfrealmat(group_id,dsetname,
     $  nvdiag,storage_dims,data_dims,rank)

      dsetname = 'nvdiagave'
      call readhdfrealmat(group_id,dsetname,
     $  nvdiagave,storage_dims,data_dims,rank)

      dsetname = 'vdiag'
      call readhdfrealmat(group_id,dsetname,
     $  vdiag,storage_dims,data_dims,rank)

      dsetname = 'vrdiagin'
      call readhdfrealmat(group_id,dsetname,
     $  vrdiagin,storage_dims,data_dims,rank)

      dsetname = 'vtdiagin'
      call readhdfrealmat(group_id,dsetname,
     $  vtdiagin,storage_dims,data_dims,rank)

      rank = 1
      data_dims(1) = nr
      storage_dims(1) = nrsize
      dsetname = 'diagrho'
      call readhdfrealmat(group_id,dsetname,
     $  diagrho,storage_dims,data_dims,rank)

      dsetname = 'diagphi'
      call readhdfrealmat(group_id,dsetname,
     $  diagphi,storage_dims,data_dims,rank)

      dsetname = 'diagchi'
      rank = 1
      data_dims(1) = nth+1
      storage_dims(1) = nthsize+1
      call readhdfrealmat(group_id,dsetname,
     $  diagchi,storage_dims,data_dims,rank)

      dsetname = 'fluxprobe'
      rank = 1
      data_dims(1) = maxsteps
      storage_dims(1) = nstepmax
      call readhdfrealmat(group_id,dsetname,
     $  fluxprobe,storage_dims,data_dims,rank)

      dsetname = 'zmom'
      rank = 3
      data_dims(1) = maxsteps
      data_dims(2) = 5
      data_dims(3) = 2
      storage_dims(1) = nstepmax
      storage_dims(2) = 5
      storage_dims(3) = 2
      call readhdfrealmat(group_id,dsetname,
     $  zmom,storage_dims,data_dims,rank)

      dsetname = 'xmom'
      rank = 3
      data_dims(1) = maxsteps
      data_dims(2) = 4
      data_dims(3) = 2
      storage_dims(1) = nstepmax
      storage_dims(2) = 4
      storage_dims(3) = 2
      call readhdfrealmat(group_id,dsetname,
     $  xmom,storage_dims,data_dims,rank)

      dsetname = 'ymom'
      rank = 3
      data_dims(1) = maxsteps
      data_dims(2) = 4
      data_dims(3) = 2
      storage_dims(1) = nstepmax
      storage_dims(2) = 4
      storage_dims(3) = 2
      call readhdfrealmat(group_id,dsetname,
     $  ymom,storage_dims,data_dims,rank)

      dsetname = 'enertot'
      rank = 1
      data_dims(1) = maxsteps
      storage_dims(1) = nstepmax
      call readhdfrealmat(group_id,dsetname,
     $  enertot,storage_dims,data_dims,rank)

      rank = 3
      data_dims(1) = nth
      data_dims(2) = npsi
      data_dims(3) = maxsteps+1
      storage_dims(1) = nthsize
      storage_dims(2) = npsisize
      storage_dims(3) = nstepmax+1
      dsetname = 'nincellstep'
      call readhdfrealmat(group_id,dsetname,
     $  nincellstep,storage_dims,data_dims,rank)

      dsetname = 'vrincellstep'
      call readhdfrealmat(group_id,dsetname,
     $  vrincellstep,storage_dims,data_dims,rank)

      dsetname = 'vr2incellstep'
      call readhdfrealmat(group_id,dsetname,
     $  vr2incellstep,storage_dims,data_dims,rank)

      rank = 2
      data_dims(1) = nth
      data_dims(2) = npsi
      storage_dims(1) = nthsize
      storage_dims(2) = npsisize
      dsetname = 'nincell'
      call readhdfrealmat(group_id,dsetname,
     $  nincell,storage_dims,data_dims,rank)

      dsetname = 'vrincell'
      call readhdfrealmat(group_id,dsetname,
     $  vrincell,storage_dims,data_dims,rank)

      dsetname = 'vr2incell'
      call readhdfrealmat(group_id,dsetname,
     $  vr2incell,storage_dims,data_dims,rank)

      dsetname = 'fincellave'
      call readhdfrealmat(group_id,dsetname,
     $  fincellave,storage_dims,data_dims,rank)

      dsetname = 'vrincellave'
      call readhdfrealmat(group_id,dsetname,
     $  vrincellave,storage_dims,data_dims,rank)

      dsetname = 'vr2incellave'
      call readhdfrealmat(group_id,dsetname,
     $  vr2incellave,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'poisson'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'debyelen'
      call readhdfrealmat(group_id,dsetname,
     $ debyelen,storage_dims,data_dims,rank)

      dsetname = 'vprobe'
      call readhdfrealmat(group_id,dsetname,
     $ vprobe,storage_dims,data_dims,rank)

      dsetname = 'Ezext'
      call readhdfrealmat(group_id,dsetname,
     $ Ezext,storage_dims,data_dims,rank)

      dsetname = 'lbcg'
      call readhdfintmat(group_id,dsetname,
     $ lbcg,storage_dims,data_dims,rank)

c     Variable arrays
      rank = 1
      data_dims(1) = nr+1
      storage_dims(1) = nrsize+1
      dsetname = 'apc'
      call readhdfrealmat(group_id,dsetname,
     $  apc,storage_dims,data_dims,rank)

      dsetname = 'bpc'
      call readhdfrealmat(group_id,dsetname,
     $  bpc,storage_dims,data_dims,rank)

      rank = 2
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      storage_dims(1) = nrsize+1
      storage_dims(2) = nthsize+1
      dsetname = 'cpc'
      call readhdfrealmat(group_id,dsetname,
     $  cpc,storage_dims,data_dims,rank)

      dsetname = 'dpc'
      call readhdfrealmat(group_id,dsetname,
     $  dpc,storage_dims,data_dims,rank)

      dsetname = 'epc'
      call readhdfrealmat(group_id,dsetname,
     $  epc,storage_dims,data_dims,rank)

      dsetname = 'fpc'
      call readhdfrealmat(group_id,dsetname,
     $  fpc,storage_dims,data_dims,rank)

      dsetname = 'gpc'
      rank = 3
      data_dims(1) = nth+1
      data_dims(2) = npsi+1
      data_dims(3) = 5
      storage_dims(1) = nthsize+1
      storage_dims(2) = npsisize+1
      storage_dims(3) = 5
      call readhdfrealmat(group_id,dsetname,
     $  gpc,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'stepave'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'nstepsave'
      call readhdfintmat(group_id,dsetname,
     $ nstepsave,storage_dims,data_dims,rank)

      dsetname = 'nsamax'
      call readhdfintmat(group_id,dsetname,
     $ nsamax,storage_dims,data_dims,rank)

      dsetname = 'diagsamp'
      call readhdfintmat(group_id,dsetname,
     $ diagsamp,storage_dims,data_dims,rank)

      dsetname = 'samp'
      call readhdfintmat(group_id,dsetname,
     $ samp,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'orbits'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'norbits'
      call readhdfintmat(group_id,dsetname,
     $ norbits,storage_dims,data_dims,rank)

c     Variable arrays
      if (norbits.gt.0) then

      rank = 2
      data_dims(1) = maxsteps
      data_dims(2) = norbits
      storage_dims(1) = nstepmax
      storage_dims(2) = nobsmax
      dsetname = 'xorbit'
      call readhdfrealmat(group_id,dsetname,
     $  xorbit,storage_dims,data_dims,rank)

      dsetname = 'yorbit'
      call readhdfrealmat(group_id,dsetname,
     $  yorbit,storage_dims,data_dims,rank)

      dsetname = 'zorbit'
      call readhdfrealmat(group_id,dsetname,
     $  zorbit,storage_dims,data_dims,rank)

      dsetname = 'vxorbit'
      call readhdfrealmat(group_id,dsetname,
     $  vxorbit,storage_dims,data_dims,rank)

      dsetname = 'vyorbit'
      call readhdfrealmat(group_id,dsetname,
     $  vyorbit,storage_dims,data_dims,rank)

      dsetname = 'vzorbit'
      call readhdfrealmat(group_id,dsetname,
     $  vzorbit,storage_dims,data_dims,rank)

      dsetname = 'rorbit'
      call readhdfrealmat(group_id,dsetname,
     $  rorbit,storage_dims,data_dims,rank)

      rank = 1
      data_dims(1) = norbits
      storage_dims(1) = nobsmax
      dsetname = 'iorbitlen'
      call readhdfintmat(group_id,dsetname,
     $  iorbitlen,storage_dims,data_dims,rank)

      endif

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'mcr'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'mcrnpart'
      call readhdfintmat(group_id,dsetname,
     $ mcrnpart,storage_dims,data_dims,rank)

      dsetname = 'mcrntheta'
      call readhdfintmat(group_id,dsetname,
     $ mcrntheta,storage_dims,data_dims,rank)

      dsetname = 'mcrnpsi'
      call readhdfintmat(group_id,dsetname,
     $ mcrnpsi,storage_dims,data_dims,rank)

      dsetname = 'mcrninjd'
      call readhdfintmat(group_id,dsetname,
     $ mcrninjd,storage_dims,data_dims,rank)

      dsetname = 'mcrtotflux'
      call readhdfrealmat(group_id,dsetname,
     $ mcrtotflux,storage_dims,data_dims,rank)

c     Variable arrays
      dsetname = 'mcrpsi'
      rank = 1
      data_dims(1) = mcrnpsi+1
      storage_dims(1) = mcrpsisize+1
      call readhdfrealmat(group_id,dsetname,
     $  mcrpsi,storage_dims,data_dims,rank)

      dsetname = 'mcrcostheta'
      rank = 1
      data_dims(1) = mcrntheta+1
      storage_dims(1) = mcrthetasize+1
      call readhdfrealmat(group_id,dsetname,
     $  mcrcostheta,storage_dims,data_dims,rank)

      dsetname = 'mcrpart'
      rank = 2
      data_dims(1) = mcrndim
      data_dims(2) = mcrnpart
      storage_dims(1) = mcrndim
      storage_dims(2) = mcrpartsize
      call readhdfrealmat(group_id,dsetname,
     $ mcrpart,storage_dims,data_dims,rank)

      dsetname = 'mcrfacenpart'
      rank = 2
      data_dims(1) = mcrntheta
      data_dims(2) = mcrnpsi
      storage_dims(1) = mcrthetasize
      storage_dims(2) = mcrpsisize
      call readhdfintmat(group_id,dsetname,
     $  mcrfacenpart,storage_dims,data_dims,rank)


      if (lfulloutput) then

      dsetname = 'mcrfacepart'
      rank = 3
      data_dims(1) = mcrnpart
      data_dims(2) = mcrntheta
      data_dims(3) = mcrnpsi
      storage_dims(1) = mcrpartsize
      storage_dims(2) = mcrthetasize
      storage_dims(3) = mcrpsisize
      call readhdfintmat(group_id,dsetname,
     $  mcrfacepart,storage_dims,data_dims,rank)

      dsetname = 'mcrfacecumnormv'
      rank = 3
      data_dims(1) = mcrnpart
      data_dims(2) = mcrntheta
      data_dims(3) = mcrnpsi
      storage_dims(1) = mcrpartsize
      storage_dims(2) = mcrthetasize
      storage_dims(3) = mcrpsisize
      call readhdfrealmat(group_id,dsetname,
     $ mcrfacecumnormv,storage_dims,data_dims,rank)

      endif


      dsetname = 'mcrcumfacewght'
      rank = 1
      data_dims(1) = mcrntheta*mcrnpsi
      storage_dims(1) = mcrthetasize*mcrpsisize
      call readhdfrealmat(group_id,dsetname,
     $ mcrcumfacewght,storage_dims,data_dims,rank)

      dsetname = 'mcrxpinjd'
      rank = 2
      data_dims(1) = ndim
      data_dims(2) = mcrninjd
      storage_dims(1) = ndim
      storage_dims(2) = npartmax
      call readhdfrealmat(group_id,dsetname,
     $ mcrxpinjd,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'err'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'lgotooutput'
      call readhdfintmat(group_id,dsetname,
     $ lgotooutput,storage_dims,data_dims,rank)

      dsetname = 'lsavephi'
      call readhdfintmat(group_id,dsetname,
     $ lsavephi,storage_dims,data_dims,rank)

      if (stepcount .lt. saveatstep) then
         lsavemat = .false.
      endif

      dsetname = 'lsavemat'
      call readhdfintmat(group_id,dsetname,
     $ lsavemat,storage_dims,data_dims,rank)

      dsetname = 'stepcount'
      call readhdfintmat(group_id,dsetname,
     $ stepcount,storage_dims,data_dims,rank)

      dsetname = 'saveatstep'
      call readhdfintmat(group_id,dsetname,
     $ saveatstep,storage_dims,data_dims,rank)

      dsetname = 'rshieldingsave'
      call readhdfintmat(group_id,dsetname,
     $ rshieldingsave,storage_dims,data_dims,rank)

c     Variable arrays
      if (lsavephi) then

      dsetname = 'phisave'
      rank = 4
      data_dims(1) = nr+1
      data_dims(2) = nth+1
      data_dims(3) = npsi+1
      data_dims(4) = min(maxsteps,nstepssave)
      storage_dims(1) = nrsizesave+1
      storage_dims(2) = nthsizesave+1
      storage_dims(3) = npsisizesave+1
      storage_dims(4) = nstepssave
      call readhdfrealmat(group_id,dsetname,
     $  phisave,storage_dims,data_dims,rank)

      dsetname = 'phiaxissave'
      rank = 4
      data_dims(1) = nr+1
      data_dims(2) = 2
      data_dims(3) = npsi+1
      data_dims(4) = min(maxsteps,nstepssave)
      storage_dims(1) = nrsizesave+1
      storage_dims(2) = 2
      storage_dims(3) = npsisizesave+1
      storage_dims(4) = nstepssave
      call readhdfrealmat(group_id,dsetname,
     $  phiaxissave,storage_dims,data_dims,rank)

      endif

      if (lsavemat) then

      rank = 6
      data_dims(1) = rshieldingsave
      data_dims(2) = nth
      data_dims(3) = npsi
      data_dims(4) = rshieldingsave
      data_dims(5) = nth
      data_dims(6) = npsi
      storage_dims(1) = nrsizesave
      storage_dims(2) = nthsizesave
      storage_dims(3) = npsisizesave
      storage_dims(4) = nrsizesave
      storage_dims(5) = nthsizesave
      storage_dims(6) = npsisizesave

      dsetname = 'Asave'
      call readhdfrealmat(group_id,dsetname,
     $  Asave,storage_dims,data_dims,rank)

      dsetname = 'Atsave'
      call readhdfrealmat(group_id,dsetname,
     $  Atsave,storage_dims,data_dims,rank)

      rank = 2
      data_dims(1) = rshieldingsave*nth*npsi
      data_dims(2) = rshieldingsave*nth*npsi
      storage_dims(1) = nrsizesave*nthsizesave*npsisizesave
      storage_dims(2) = nrsizesave*nthsizesave*npsisizesave

      dsetname = 'Amat'
      call readhdfrealmat(group_id,dsetname,
     $  Amat,storage_dims,data_dims,rank)

      dsetname = 'Atmat'
      call readhdfrealmat(group_id,dsetname,
     $  Atmat,storage_dims,data_dims,rank)

      rank = 3
      data_dims(1) = rshieldingsave
      data_dims(2) = nthused+1
      data_dims(3) = npsiused+1
      storage_dims(1) = nrsizesave
      storage_dims(2) = nthsizesave+1
      storage_dims(3) = npsisizesave+1

      dsetname = 'bsave'
      call readhdfrealmat(group_id,dsetname,
     $  bsave,storage_dims,data_dims,rank)

      dsetname = 'xsave'
      call readhdfrealmat(group_id,dsetname,
     $  xsave,storage_dims,data_dims,rank)

      rank = 1
      data_dims(1) = rshieldingsave*nthused*npsiused
      storage_dims(1) = nrsizesave*(nthsizesave+1)*(npsisizesave+1)

      dsetname = 'bsavevect'
      call readhdfrealmat(group_id,dsetname,
     $  bsavevect,storage_dims,data_dims,rank)

      dsetname = 'xsavevect'
      call readhdfrealmat(group_id,dsetname,
     $  xsavevect,storage_dims,data_dims,rank)

      endif

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'colncom'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'vneutral'
      call readhdfrealmat(group_id,dsetname,
     $ vneutral,storage_dims,data_dims,rank)

      dsetname = 'cnd'
      call readhdfrealmat(group_id,dsetname,
     $ cnd,storage_dims,data_dims,rank)

      dsetname = 'psind'
      call readhdfrealmat(group_id,dsetname,
     $ psind,storage_dims,data_dims,rank)

      dsetname = 'Tneutral'
      call readhdfrealmat(group_id,dsetname,
     $ Tneutral,storage_dims,data_dims,rank)

      dsetname = 'Eneutral'
      call readhdfrealmat(group_id,dsetname,
     $ Eneutral,storage_dims,data_dims,rank)

      dsetname = 'NCneutral'
      call readhdfintmat(group_id,dsetname,
     $ NCneutral,storage_dims,data_dims,rank)

c     Variable arrays
      dsetname = 'vneut'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  vneut,storage_dims,data_dims,rank)

      dsetname = 'Eneut'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  Eneut,storage_dims,data_dims,rank)

      dsetname = 'reldrift'
      rank = 1
      data_dims(1) = 3
      storage_dims(1) = 3
      call readhdfrealmat(group_id,dsetname,
     $  reldrift,storage_dims,data_dims,rank)

c Close the group.
      CALL h5gclose_f(group_id, error)


c Open a group in the file.
      groupname = 'noncommonblock'
      CALL h5gopen_f(file_id, groupname, group_id, error)

c Read the data in this group

c     Single value variables
      rank = 1
      data_dims(1) = 1
      storage_dims(1) = 1

      dsetname = 'dt'
      call readhdfrealmat(group_id,dsetname,
     $ dt,storage_dims,data_dims,rank)

      dsetname = 'fave'
      call readhdfrealmat(group_id,dsetname,
     $ fave,storage_dims,data_dims,rank)

      dsetname = 'colnwt'
      call readhdfrealmat(group_id,dsetname,
     $ colnwt,storage_dims,data_dims,rank)

      dsetname = 'icolntype'
      call readhdfintmat(group_id,dsetname,
     $ icolntype,storage_dims,data_dims,rank)

c Close the group.
 999  CALL h5gclose_f(group_id, error)


c Terminate access to the file.
      CALL h5fclose_f(file_id, error)

c Close FORTRAN interface.
      CALL h5close_f(error)

      end


c*********************************************************************
c Reads a real matrix from a specified group in an hdf5 file
      subroutine readhdfrealmat(group_id,dsetname,dset_data,
     $  storage_dims,data_dims,rank)
c Load the hdf5 module
      use hdf5
c Don't allow implicit definitions
      implicit none
c Input variables
      INTEGER(HID_T) :: group_id
      CHARACTER(LEN=55) :: dsetname
      REAL, DIMENSION(*) :: dset_data
      INTEGER(HSIZE_T), DIMENSION(*) :: data_dims, storage_dims
      INTEGER     ::   rank
c Local variables
      INTEGER(HID_T) :: dataset_id
      INTEGER(HID_T) :: dataspace_id, memspace_id
      INTEGER     ::   error
c     Hyperslab offset in memory
      INTEGER(HSIZE_T), DIMENSION(10) :: offset =
     $  (/0,0,0,0,0,0,0,0,0,0/)

c Open the dataset
      CALL h5dopen_f(group_id, dsetname, dataset_id, error)

c Create a data space for the memory (since only part of array used).
      CALL h5screate_simple_f(rank,storage_dims,memspace_id,error)

c Select hyperslab in memory.
      CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F,
     $  offset, data_dims, error)

c Read the dataset.
      CALL h5dread_f(dataset_id, H5T_NATIVE_REAL, dset_data,
     $  data_dims, error, memspace_id)

c Close the dataspace.
      CALL h5sclose_f(memspace_id, error)

c Close the dataset
      CALL h5dclose_f(dataset_id, error)

      end

c*********************************************************************
c Reads an integer matrix from a specified group in an hdf5 file
      subroutine readhdfintmat(group_id,dsetname,dset_data,
     $  storage_dims,data_dims,rank)
c Load the hdf5 module
      use hdf5
c Don't allow implicit definitions
      implicit none
c Input variables
      INTEGER(HID_T) :: group_id
      CHARACTER(LEN=55) :: dsetname
      INTEGER, DIMENSION(*) :: dset_data
      INTEGER(HSIZE_T), DIMENSION(*) :: data_dims, storage_dims
      INTEGER     ::   rank
c Local variables
      INTEGER(HID_T) :: dataset_id
      INTEGER(HID_T) :: dataspace_id, memspace_id
      INTEGER     ::   error
c     Hyperslab offset in memory
      INTEGER(HSIZE_T), DIMENSION(10) :: offset =
     $  (/0,0,0,0,0,0,0,0,0,0/)

c Open the dataset
      CALL h5dopen_f(group_id, dsetname, dataset_id, error)

c Create a data space for the memory (since only part of array used).
      CALL h5screate_simple_f(rank,storage_dims,memspace_id,error)

c Select hyperslab in memory.
      CALL h5sselect_hyperslab_f(memspace_id, H5S_SELECT_SET_F,
     $  offset, data_dims, error)

c Read the dataset.
      CALL h5dread_f(dataset_id, H5T_NATIVE_INTEGER, dset_data,
     $  data_dims, error, memspace_id)

c Close the dataspace.
      CALL h5sclose_f(memspace_id, error)

c Close the dataset
      CALL h5dclose_f(dataset_id, error)

      end
