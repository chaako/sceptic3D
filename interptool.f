c Tool for interpolating field values
      program interptool

      character*155 filename
      real dt, fave, colnwt
      integer k, icolntype


c Common storage
      include 'piccom.f'
      include 'errcom.f'
      include 'colncom.f'
      include 'fvcom.f'

c Deal with arguments
      if(iargc().eq.0) goto 51
      call getarg(1,filename)
c      write(*,*) filename

      call readhdf(dt,k,fave,icolntype,colnwt,filename)

c Return success
      call exit(0)

 51   continue
c Help section
      write(*,*) 'Usage: interptool inputfile.h5'

      end
