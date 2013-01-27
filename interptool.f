c Tool for interpolating field values
      program interptool

      character*155 filename
      integer nNodes
      parameter (nNodes=10)
      double precision x(nNodes),y(nNodes),z(nNodes)
      double precision potential(nNodes)
      double precision ax(nNodes),ay(nNodes),az(nNodes)

c Deal with arguments
      if(iargc().eq.0) goto 51
      call getarg(1,filename)
c      write(*,*) filename

      do i=1,nNodes
         x(i) = 1.+i/30.
         y(i) = i/30.
         z(i) = 2.+i/30.
      enddo

      call interpolateFields_sceptic3D(filename,x,y,z,nNodes,
     $       potential,ax,ay,az)

c For debugging
c      do i=1,nNodes
c         write(*,*) 'i pot accel:',i,potential(i),ax(i),ay(i),az(i)
c      enddo

c Return success
      call exit(0)

 51   continue
c Help section
      write(*,*) 'Usage: interptool inputfile.h5'

      end
