c Tool for interpolating field values
      subroutine interpolateFieldsSceptic3D(filename,x,y,z,nNodes,
     $             potential,ax,ay,az,density,filenameLength)

c Input variables
      character*155 filename
      double precision x(*),y(*),z(*)
      integer nNodes
      integer filenameLength
c Output variables
      double precision potential(*),ax(*),ay(*),az(*),density(*)

c Non-commonblock variables in hdf file
      real dt, fave, colnwt
      integer k, icolntype

c Local variables
      real accel(3)
      integer i,il,ith,ipl,ih
      real rf,tf,pf,st,ct,sp,cp,rp,zetap,hf
      character*155 filenameFixed

c Common storage
      include 'piccom.f'
      include 'errcom.f'
      include 'colncom.f'
      include 'fvcom.f'

      filenameFixed(1:filenameLength) = filename
      do i=(filenameLength+1),155
         filenameFixed(i:i) = ' '
      enddo
c      write(*,*) filenameFixed
c      call exit(0)

c Populate common blocks from hdf file
      call readhdf(dt,k,fave,icolntype,colnwt,filenameFixed)

c Find acceleration at points
      do i=1,nNodes
c        Reuse first particle slot each time
         xp(1,1)=x(i)
         xp(2,1)=y(i)
         xp(3,1)=z(i)

         ih=1
         hf=77.
         call ptomesh(1,il,rf,ith,tf,ipl,pf,st,ct,
     $        sp,cp,rp,zetap,ih,hf)
c        Potential interpolation from istrapped()
         potential(i)=
     $        (phi(il,ith,ipl)*(1.-tf)+phi(il,ith+1,ipl)*tf)*(1.-rf) +
     $        (phi(il+1,ith,ipl)*(1.-tf)+phi(il+1,ith+1,ipl)*tf)*rf
         density(i)=
     $        (rhoDiag(il,ith,ipl)*(1.-tf)+
     $         rhoDiag(il,ith+1,ipl)*tf)*(1.-rf) +
     $        (rhoDiag(il+1,ith,ipl)*(1.-tf)+
     $         rhoDiag(il+1,ith+1,ipl)*tf)*rf
         call getaccel(1,accel,il,rf,ith,tf,ipl,pf,st,ct,
     $        sp,cp,rp,zetap,ih,hf)
         ax(i)=accel(1)
         ay(i)=accel(2)
         az(i)=accel(3)
c For debugging
c         write(*,*) 'pos:', xp(1,i), xp(2,i), xp(3,i)
c         write(*,*) 'pot accel:',i,potential(i),ax(i),ay(i),az(i)
      enddo

      end
