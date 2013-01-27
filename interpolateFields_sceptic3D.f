c Tool for interpolating field values
      subroutine interpolateFields_sceptic3D(filename,x,y,z,nNodes,
     $             potential,ax,ay,az)

c Input variables
      character filename(*)
      double precision x(*),y(*),z(*)
      integer nNodes
c Output variables
      double precision potential(*),ax(*),ay(*),az(*)

c Non-commonblock variables in hdf file
      real dt, fave, colnwt
      integer k, icolntype

c Local variables
      real accel(3)
      integer i,il,ith,ipl,ih
      real rf,tf,pf,st,ct,sp,cp,rp,zetap,hf

c Common storage
      include 'piccom.f'
      include 'errcom.f'
      include 'colncom.f'
      include 'fvcom.f'

c Populate common blocks from hdf file
      call readhdf(dt,k,fave,icolntype,colnwt,filename)

c Find acceleration at points
      do i=1,nNodes
         xp(1,i)=x(i)
         xp(2,i)=y(i)
         xp(3,i)=z(i)

         ih=1
         hf=77.
         call ptomesh(i,il,rf,ith,tf,ipl,pf,st,ct,
     $        sp,cp,rp,zetap,ih,hf)
c        Potential interpolation from istrapped()
         potential(i)=
     $        (phi(il,ith,ipl)*(1.-tf)+phi(il,ith+1,ipl)*tf)*(1.-rf) +
     $        (phi(il+1,ith,ipl)*(1.-tf)+phi(il+1,ith+1,ipl)*tf)*rf
         call getaccel(i,accel,il,rf,ith,tf,ipl,pf,st,ct,
     $        sp,cp,rp,zetap,ih,hf)
         ax(i)=accel(1)
         ay(i)=accel(2)
         az(i)=accel(3)
c For debugging
c         write(*,*) 'pos:', xp(1,i), xp(2,i), xp(3,i)
c         write(*,*) 'pot accel:',i,potential(i),ax(i),ay(i),az(i)
      enddo

      end
