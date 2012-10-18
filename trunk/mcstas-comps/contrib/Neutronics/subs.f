      module subs

c     Code to interface MCNP and McStas using the Source Surface Read/Write functionality of MCNP.
c     Intented to allow ping-pong between MCNP and McStas, so that first a MCNP simulation is run
c     and at a relevant surface, a Source Surface Write card is given (e.g. for MCNP surface #1, add the line:
c     SSW 1 
c     to the end of the input file.
c     By this a ".w" file is produced, which then serves as input to the McStas simulation. This results (among others)
c     in a file called: wssa
c     which is of the same type as the original .w file, and can be refed to the MCNP simulation
c     Written by Esben Klinkby, Risø-DTU in January 2012
c     Thanks to Harald Breitkreutz for help understanding the SSW/SSR format
c
c     Not yet validated!!!
c      
      implicit double precision (a-h,o-z)

c     General varialbles
      character kods*8          ! Code
      character vers*5          ! Version
      character lods*28         ! Date
      character idtms*19        ! Machine-Designator
      character probs*19        ! Problem-ID
      character aids*80         ! Creation-Run Problem-Title-Card
      
c     Surfaces
      dimension tpps(10,64)     ! Array for surface parameters
      dimension ntppsp(10)      ! Array for number of suface parameters
      dimension kstpps(10)      ! Array for surface types
      dimension isurfs(10)      ! Array for surfaces
      
c     Structure of SSB-Arrays 
      dimension ssb(11)         ! Surface-Source Info
      dimension nslr(14,10)     ! SS Info record
                  
      double precision, parameter :: Mev2Joule = 1.602e-13     ! MeV to Joule conversion factor
      double precision, parameter :: MNEUTRON = 1.67492728e-27 ![kg] mass of neutron CODATA 2002
      
c     Logical used to figure out whether file exists or not
      logical :: bfile          ! Data file exists
      
c     Datafiles
      integer, parameter:: iusr  = 123 ! RSSA-Data - SSR input. 
      integer, parameter:: iusw  = 124 ! Ptrak file
      integer, parameter:: iusw2 = 125 ! WSSA-Data - SSW output
                  
      integer nrcd,nrcdo         !Needed public for file read from loop 
      integer njsw, np1o, np1, niss, mipts,niwr,knods,kjaq
      end module subs

c---------------------------------------------------------------------------------------------
      
      subroutine readheader(ntrk,nhis)

      use subs                                ! Use module subs for varialbles common across subroutines
      implicit double precision (a-h,o-z)

      ! Open SSW file. Assumes the name: rssa
      open(iusr,file='rssa',form='unformatted', status='old')
      rewind iusr
      
      ! Start reading the header
      read(iusr,end=1666)kods,vers,lods,idtms,probs,aids,knods
      read(iusr,end=1666)np1,nrss,nrcd,njsw,niss
      ntrk=nrss
      nhis=niss
      nrcdo=nrcd
      np1o=np1
            
      if(np1.le.0) then
         np1=abs(np1)          
         read(iusr,end=1666)niwr,mipts,kjaq
      endif
      if(nrcd.ne.6.and.nrcd.ne.10)nrcd=nrcd-1
      
      do 1000 i=1,njsw+niwr
         read(iusr,end=1666)isurfs(i),kstpps(i),ntppsp(i),
     1        (tpps(i,j),j=1,ntppsp(i))
 1000 continue
      
      read(iusr,end=1666)a,
     1     ((nslr(i,j),i=1,2+4*mipts),j=1,njsw+niwr)

      ! Open ascii file for ptrak output (EK: make switch allowing user to skip)
      open(iusw,file='ptrak',status='replace',form='formatted')
      rewind(iusw)

      return
      
c     Error treatment
 1666 print*
      print*,'Something wrong in readheader :-('
      print*
      stop
      
      end subroutine readheader
      
c---------------------------------------------------------------------------------------------

      subroutine readneutron(to_mcstas)
            
      use subs
      implicit double precision (a-h,o-z)
      dimension to_mcstas(9)
      
      read(iusr,end=666)(ssb(i),i=1,nrcd)

c     variable gymnastics needed to record ptrak output file & to hand to mcstas
      zz=abs(sqrt(max(0.,1.-ssb(9)**2-ssb(10)**2)))
     &     *SSB(2)/abs(SSB(2))
      
c------------------write ptrak section----below-----------!
 310  write(iusw,311) ssb(6),ssb(7),ssb(8),ssb(9),
     &     ssb(10),zz,ssb(5),ssb(3),ssb(4)
 311  format(1x,10e13.5)
c------------------write ptrak section----up--------------!
      
      
c     more variable gymnastics needed to convert to McStas coordinates & units
      to_mcstas(1) = ssb(6) / 100 ! x
      to_mcstas(2) = ssb(7) / 100 ! y
      to_mcstas(3) = ssb(8) / 100    ! z

      speed=sqrt(2.* ssb(4)*Mev2Joule/MNEUTRON)

      to_mcstas(4) = ssb(9)  * speed ! px
      to_mcstas(5) = ssb(10) * speed ! py 
      to_mcstas(6) = zz      * speed ! pz

      to_mcstas(7) = ssb(3)          ! weight
      to_mcstas(8) = ssb(4)          ! time

      return

c          Error treatmeant
 666  print*
      print*,'Something wrong in readneutron :-('
      print*,'If you are running iteratively, it can'
      print*,'be that the job merely ran out of input' 
      print*,'events. Check for McStas ERROR messages'
      print*,'if they are OK, the job is fine'
      stop

      end subroutine readneutron

c---------------------------------------------------------------------------------------------

      subroutine writeneutron(from_mcstas)
      
      use subs
      implicit double precision (a-h,o-z)
      dimension from_mcstas(9)

c     Inverse conversion as in readheader. Fill ssb(i) records (i=1,nrcd) based on McStas output
      ssb(3) = from_mcstas(7);
      ssb(4) = from_mcstas(8);

      ssb(6) = from_mcstas(1) * 100 ! x
      ssb(7) = from_mcstas(2) * 100 ! y
      ssb(8) = from_mcstas(3) * 100 ! z

      speed=sqrt(from_mcstas(4)**2+from_mcstas(5)**2+from_mcstas(6)**2)
      energy=0.5*MNEUTRON*(speed**2)/Mev2Joule
      ssb(5)=energy
      
      ssb(9)  = from_mcstas(4) / speed  ! cosu
      ssb(10) = from_mcstas(5) / speed  ! cosv

      write(iusw2)(ssb(i),i=1,nrcd)

      end subroutine writeneutron

c---------------------------------------------------------------------------------------------

      subroutine writeheader(ntrk,nhis)

      use subs
      implicit double precision (a-h,o-z)

c     Handling of output file
      inquire(file='wssa',exist=bfile)
      if(bfile) then
         print*,'WARNING: file wssa already exists!'
         print*,'will be replaced...too late...:-)!'
         !stop
      endif
      open(iusw2,file='wssa',status='replace',form='unformatted')
      rewind iusw2

      nrss=ntrk
      niss=nhis
c     Put your hardcoded #histories & #tracks here

      print*,'WARNING: Filling wssa header: ', nrss, ' events'
      print*,'WARNING: Filling wssa header: ', niss, ' histories'
      print*,'WARNING: In case of event loss in McStas simulation,'
      print*,'WARNING: these values are inconsistent with file content'
      print*,'WARNING: which can cause trouble in subsequent MCNP run'
      print*,'WARNING: To resolve, see BUGS in:'
      print*,'WARNING: Virtual_mcnp_ss_input.comp'


c     Write header
      write(iusw2)kods,vers,lods,idtms,probs,aids,knods
      write(iusw2)np1o,nrss,nrcdo,njsw,niss
      if(np1o.le.0) write(iusw2)niwr,mipts,kjaq
            
      do 2000 i=1,njsw+niwr
         write(iusw2)isurfs(i),kstpps(i),ntppsp(i),
     1        (tpps(i,j),j=1,ntppsp(i))
 2000 continue

      write(iusw2)a,
     1     ((nslr(i,j),i=1,2+4*mipts),j=1,njsw+niwr)

      end subroutine writeheader

c---------------------------------------------------------------------------------------------

      subroutine closefiles()

      use subs

      close(iusr)
      close(iusw)
      close(iusw2)

      end subroutine closefiles

c---------------------------------------------------------------------------------------------

