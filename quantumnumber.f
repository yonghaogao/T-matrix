      program quantnumbers
      use inputfile
      implicit none
      integer::alnumber,apnumber
      call input()
    
      call quantnumber(alnumber,apnumber)
     
      call quantumnumber(alnumber,apnumber)
       
      end program
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c---------------calculate the number of alpha & alphapring-------------
      subroutine quantnumber(alphanumber,alphapringnumber)
      use inputfile
      implicit none
      integer::alphanumber,alphapringnumber
      integer::l1,lamda1,l2,lamda2,J
      
      alphanumber=1
      do l1=0,Lmax1
	do lamda1=0,lamdamax1
	  do J=abs(l1-lamda1),l1+lamda1
	        alphanumber=alphanumber+1
	  end do
	end do
      end do
      alphapringnumber=1
      do l2=0,Lmax2
	do lamda2=0,lamdamax2
	  do J=abs(l2-lamda2),l2+lamda2
	        alphapringnumber=alphapringnumber+1
	  end do
	end do
      end do
      write(*,*)"alphanumber=",alphanumber
      write(*,*)"alphapringnumber=",alphapringnumber
      end subroutine


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c-----------------------calculate the l lamda & J--------------------
      subroutine quantumnumber(alphanumber,alphapringnumber)
      use inputfile
      implicit none
      
      integer::int1,num1,num2,num3
      integer::alphanumber,alphapringnumber
      integer::alpha,alphapring
      integer::alphaparity,alphapringparity
      integer,allocatable::l1(:),lamda1(:),l2(:),lamda2(:),J1(:),J2(:)
      integer,allocatable::abarmatrix(:,:)
      allocate(l1(0:alphanumber),lamda1(0:alphanumber))
      allocate(l2(0:alphapringnumber),lamda2(0:alphapringnumber))
      allocate(J1(0:alphanumber),J2(0:alphapringnumber))
      allocate(abarmatrix(1:2,1:abarmax**2))
      
      ! caculate quantum number of the alpha matrix
      int1=1
      do num1=0,Lmax1
	do num2=0,lamdamax1
	  do num3=abs(num1-num2),num1+num2
	       l1(int1)=num1
	       lamda1(int1)=num2
               J1(int1)=num3
               int1=int1+1
	  end do
	end do
      end do
      ! caculate quantum number of the alphapring matrix
      int1=1
      do num1=0,Lmax2
	do num2=0,lamdamax2
	  do num3=abs(num1-num2),num1+num2
	       l2(int1)=num1
	       lamda2(int1)=num2
               J2(int1)=num3
               int1=int1+1
	  end do
	end do
      end do
      ! filtrate the appropriate quantum number
      int1=1
      do alpha=1,abarmax
        do alphapring=1,abarmax
          if(J1(alpha)==J2(alphapring))then
             alphaparity=(-1)**(l1(alpha)+lamda1(alpha))
             alphapringparity=(-1)**(l2(alphapring)+lamda2(alphapring))
             if(alphaparity==alphapringparity)then
               abarmatrix(1,int1)=alpha
               abarmatrix(2,int1)=alphapring
               int1=int1+1
             end if
          end if
        end do
      end do
ccccccccccccccccccccccccccccccccccccccccccccc
      write(*,*)"         l1        ","lamda1        ","l2        ","lamda2        ","J        "
      do num1=1,10
        alpha=abarmatrix(1,num1)
        alphapring=abarmatrix(2,num1)
        write(*,*)l1(alpha),lamda1(alpha),l2(alphapring),lamda2(alphapring),J1(alpha)
        
      end do
      write(*,*)"test "
ccccccccccccccccccccccccccccccccccccccccccccc
      alpha=abarmatrix(1,abar)
      alphapring=abarmatrix(2,abar)
      write(*,*)"l1=",l1(alpha),"lamda1=",lamda1(alpha),"l2=",l2(alphapring),"lamda2=",lamda2(alphapring),"J=",J1(alpha)

      deallocate(l1,lamda1,l2,lamda2,J1,J2)     
      end subroutine
   
     
