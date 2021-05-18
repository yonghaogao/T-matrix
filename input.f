      module inputfile 
!   defines input for potential, grid, and t-matrix
!!>  this subroutine generates some constants 
!  Parameters :
!  xmin(xmax):the lower(upper) limit of integral
!  tol:  error range
!  hbarc: h*c/(2*pi)=197.32698
!  rm : rm: reduced mass 
!       rm=mp*mn/(mp+mn)
!  xm: match point
        implicit none
        integer::n,lmax
	integer::Lmax1,lamdamax1,lmax2,lamdamax2,abar,abarmax
        real*8,parameter::hbarc=197.327d0
        real*8,parameter::mp=1.0078d0
        real*8,parameter::mn=1.0086d0
        real*8,parameter::amu=931.49432d0
	integer,parameter::AA=12
	integer,parameter::NN=6
	integer,parameter::ZZ=6
        real*8::xmin,xmax
        real*8::rm,ecm,h,k,elab
	real*8::a,b,c,d
	real*8::m1,m2,m3
	
      contains
!***********************************************************************
      subroutine input()
        implicit none
        namelist /global/ Lmax1,lamdamax1,lmax2,lamdamax2,abar,abarmax
        !namelist /grids/ l2,lamda2,l3,lamda3,J 
        lmax1=10;lamdamax1=10;lmax2=10;lamdamax2=10;abar=10;abarmax=10
        read (5,nml=global)
        write (6,nml=global)
        
        !l2=10;lamda2=10;l3=10;lamda3=10;J=20
        !read (5,nml=grids)
        !write (6,nml=grids)

        rm=mp*(ZZ*mp+NN*mn)*amu/(mp+ZZ*mp+NN*mn)
        write(*,*)'the reduced mass =',rm
        ecm=elab*(ZZ*mp+NN*mn)/(mp+ZZ*mp+NN*mn)
        write(*,*)'Energy of the central mass system =',ecm
        h=(xmax-xmin)/n
        write(*,*)'the step length =',h
        k=sqrt(2*rm*ecm)/hbarc
	write(*,*)'the wave number =',k
	m1=mn*amu
	write(*,*)'the mass of the m1 =',m1
	m2=mp*amu
	write(*,*)'the mass of the m2 =',m2
	m3=(NN*mn+ZZ*mp)*amu      
	write(*,*)'the mass of the m3 =',m3
	a=m3/(m1+m3)
	write(*,*)'the coeffcient of x2 for f =',a
	b=1.0d0
	write(*,*)'the coeffcient of y2 for f =',b
	c=1-m2*m3/(m1+m2)/(m1+m3)
	write(*,*)'the coeffcient of x2 for g =',c
	d=-1
	write(*,*)'the coeffcient of y2 for g =',d
        write (6,*)
        write (6,*) '============  Output  ==================='
        write (6,*)
      end subroutine
***********************************************************************
      end module 
