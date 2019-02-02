program improve
use mytype; use params;
implicit none
! Expansion coefficients for the completes state vector
complex (dp), dimension (-5:1,-5:total_num)   :: rou,rwa
complex (dp), dimension (1:length_coeff)      :: fin,fout
! Simple arrays to read ind write the RK4-routine
real (dp)                                     :: pop_mnp,pop_g,pop_1e,pop_2e,pop_sem 
! Total electronic population and distribution in the conduction band
real (dp)                                     :: time,omega
integer                                       :: a,b,aa,bb  
real (dp)                                     :: t_help,p_help,pp_help  
integer                                       :: t,ss,circle,num_circle     
real (dp)                                     :: fdata_max,stop_time        
real (dp)                                     :: s  
integer, dimension (1:num_max,1:6)            :: nearatom 
integer, dimension (1:num_submax,1:6)         :: nearatom2 
integer, dimension (1:num_submax,1:8)         :: ncenter
integer, dimension (1:num_max,1:8)            :: cor_cen
real (dp), dimension (1:200)                  :: pop_en
real (dp), dimension (1:200)                  :: pop_1en,pop_2en
real (dp), dimension (1:200)                  :: pop_em
real (dp), dimension (1:200)                  :: pop_1em,pop_2em
real (dp), dimension (1:200)                  :: pop_tn,pop_tm
complex (dp),dimension (-5:1,-5:total_num)    :: omega_telta
real (dp),dimension (-5:-3)                   :: dipole_mnp
real (dp),dimension (-5:-3)                   :: J_1eI
real (dp),dimension (-5:-3)                   :: J_2eI
real (dp),dimension (-1: 0)                   :: J_1e2e
real (dp),dimension (-1: 0,-5:-3)             :: J_eI
real (dp),dimension (1:total_num)             :: dis_Tpl
real (dp),dimension (1:total_num)             :: dis_mT1
real (dp),dimension (1:total_num)             :: dis_mT2

interface
    subroutine rk4 (y,x,h,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_1eI,J_2eI,J_1e2e,dipole_mnp,yout)
    use mytype; use params     
implicit none
    complex (dp), dimension (:), intent (in)    :: y
    real (dp), intent (in)                      :: x,h
    complex (dp), dimension (:), intent(out)    :: yout
    integer, dimension (1:num_max,1:6)          :: nearatom
    integer, dimension (1:num_submax,1:6)       :: nearatom2
    integer, dimension (1:num_max,1:8)          :: cor_cen
    integer, dimension (1:num_submax,1:8)       :: ncenter
    complex (dp),dimension (-5:1,-5:total_num)  :: omega_telta
    real (dp),dimension (-5:-3)                 :: dipole_mnp
    real (dp),dimension (-5:-3)                 :: J_1eI
    real (dp),dimension (-5:-3)                 :: J_2eI
    real (dp),dimension (-1: 0)                 :: J_1e2e
    real (dp),dimension (1:total_num)           :: dis_Tpl
    real (dp),dimension (1:total_num)           :: dis_mT1
    real (dp),dimension (1:total_num)           :: dis_mT2
    end subroutine rk4      
end interface

open (unit = 11, file='pop_g.dat', status='replace', access='sequential')
open (unit = 12, file='pop_1e.dat', status='replace', access='sequential')
open (unit = 13, file='pop_2e.dat', status='replace', access='sequential')
open (unit = 14, file='pop_mnp.dat', status='replace', access='sequential') 
open (unit = 15, file='pop_sem.dat', status='replace', access='sequential')

call calnearatom (nearatom,ncenter,nearatom2)
call cornercenter (ncenter,cor_cen)

if (total_num == num_max) then
  ncenter = 0
  nearatom2 = 0   
  cor_cen = 0   
end if
nearatom2 = 0  

call calpar(omega_telta,J_1eI,J_2eI,J_1e2e,dipole_mnp)

ss = 0
time = 0
t = 1
! Initial Values for the Expnasion Coefficients
do a = -5, 1
  do b = -5, total_num
    if ((a==b).and.(a==-2))then
      rwa(a,b) = 1.0d0             ! In case of a=-2, b=-2: r(a,b)=r(g,g)
      rou(a,b) = 1.0d0
    else
      rwa(a,b) = 0.0d0
      rou(a,b) = 0.0d0
    end if
  end do
end do

! Initial Values
pop_g = 1.0d0
pop_1e = 0.0d0
pop_2e = 0.0d0
pop_mnp = 0.0d0 
pop_sem = 0.0d0

write (*,*) " time "," pop_g "," pop_1e "," pop_2e "," pop_mnp "," pop_sem "
write (*,*) "out" ,rou(-2,-2), rwa(-2,-2)
write(11,*) time, pop_g  
write(12,*) time, pop_1e
write(13,*) time, pop_2e
write(14,*) time, pop_mnp
write(15,*) time, pop_sem 

do
  call rk4 (fin,time,delta_time,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_1eI,J_2eI,J_1e2e,dipole_mnp,fout)
  call multi_to_single (rwa,fin)
  call single_to_multi (fout,rwa)
  time = time + delta_time
  t = t + 1
  if (mod(t, 50)==0) then
    ss=ss+1

write (*,*)  "in ", rwa(-2,-2), rou(-2,-2)

  do a = -5, 0
    do b = -5, total_num
      if ((a/=b).and.(b==-2)) then                  
        rou(a,b) = rwa(a,b)*exp(-ii*omega_0*time)                   ! ρ=r*e^(-i*ω0*t):
        ! rou(-5~-3,-2):ρ(I,g), rou(-1,-2):ρ(1e,g), rou(0,-2):ρ(2e,g)
      else if ((a==-2).and.(b==-2)) then
        rou(a,b) = rwa(a,b)
      else
        rou(a,b) = rwa(a,b)
      end if
    end do
  end do

  do b = 1, total_num
! rou(-2,b): ρ(g,m)
    rou(-2,b) = rwa(-2,b)*exp(ii*omega_0*time)
! rou(1,b): ρ(n,m)
    rou(1,b) = rwa(1,b)*conjg(rwa(1,b))
  end do

  pop_g   = rou(-2,-2)
  pop_1e  = rou(-1,-1)
  pop_2e  = rou(0,0)
  pop_mnp = 0.0d0
  pop_sem = 0.0d0

  do  a = -5, -3
    pop_mnp = pop_mnp + rou(a,a)
  end do   
  do a = 1, total_num
    pop_sem = pop_sem + real(rwa(1,a)*conjg(rwa(1,a)))
  end do

write (*,*)  "in ", rwa(-2,-2), rou(-2,-2)
write (*,*)  time, pop_g, pop_1e, pop_2e, pop_mnp, pop_sem
write (11,*) time, pop_g
write (12,*) time, pop_1e
write (13,*) time, pop_2e
write (14,*) time, pop_mnp
write (15,*) time, pop_sem

end if       
  if (time > time_final) exit
end do

end program improve

subroutine multi_to_single(multi_array,simple_array)
use mytype; use params
implicit none
integer      :: int_auxil
integer      :: mm_sub, nn_sub
complex (dp) :: multi_array(-5:1, -5:total_num)
complex (dp) :: simple_array(1:length_coeff)

int_auxil = 1
do nn_sub = -5, 1  
  do mm_sub = -5, total_num          
    simple_array(int_auxil) = multi_array(nn_sub,mm_sub)      
    int_auxil = int_auxil + 1
  end do
end do
return
end subroutine multi_to_single

subroutine single_to_multi(simple_array,multi_array)
use mytype; use params
implicit none
integer      :: int_auxil
integer      :: mm_sub, nn_sub
complex (dp) :: simple_array(1:length_coeff)
complex (dp) :: multi_array(-5:1, -5:total_num)

int_auxil = 1
do nn_sub = -5, 1
  do mm_sub = -5, total_num
    multi_array(nn_sub,mm_sub) = simple_array(int_auxil)
    int_auxil = int_auxil + 1
  end do
end do
return
end subroutine single_to_multi

subroutine calpar(omega_telta,J_1eI,J_2eI,J_1e2e,dipole_mnp)
use mytype; use params
integer                                     :: x_sub,y_sub,z_sub,a_sub,b_sub,e_sub
real (dp),dimension(1:total_num)            :: dis_Tpl
real (dp),dimension (1:total_num)           :: dis_mT1
real (dp),dimension (1:total_num)           :: dis_mT2
real (dp),dimension(-5:total_num)           :: omega_alfa,gama_alfa
complex (dp),dimension(-5:1,-5:total_num)   :: omega_telta
real (dp),dimension(-5:-3)                  :: dipole_mnp
real (dp),dimension(-1: 0,-5:-3)            :: J_eI
real (dp),dimension(-5:-3)                  :: J_1eI
real (dp),dimension(-5:-3)                  :: J_2eI
real (dp),dimension(-1:0)                   :: J_1e2e
real (dp)                                   :: J_1eI_x, J_1eI_y, J_1eI_z
real (dp)                                   :: J_2eI_x, J_2eI_y, J_2eI_z
omega_alfa(0) = omega_mol  
gama_alfa(0) = gama_mol
omega_alfa(-1) = omega_mol   
gama_alfa(-1) = gama_mol
omega_alfa(-2) = 0.0d0
gama_alfa(-2) = 0.0d0
omega_alfa(-3) = omega_pl
gama_alfa(-3) = gama_pl
omega_alfa(-4) = omega_pl
gama_alfa(-4) = gama_pl
omega_alfa(-5) = omega_pl
gama_alfa(-5) = gama_pl
J_1eI_x = -3.0d0*(-mnp_x*dipole_pl)*(z_mol+radius_mnp)*dipole_mol1/(sqrt(dis_mpl1)**3*dis_mpl1)
J_1eI_y = -3.0d0*(-mnp_y*dipole_pl)*(z_mol+radius_mnp)*dipole_mol1/(sqrt(dis_mpl1)**3*dis_mpl1)
J_1eI_z = (dipole_mol1*dipole_pl-3.0d0*(z_mol+radius_mnp)**2/dis_mpl1*dipole_pl*dipole_mol1)/(sqrt(dis_mpl1)**3)
J_1eI(-3) = J_1eI_x
J_1eI(-4) = J_1eI_y
J_1eI(-5) = J_1eI_z
J_2eI_x = -3.0d0*(-mnp_x*dipole_pl)*(z_mol+radius_mnp-dis_mol)*dipole_mol2/(sqrt(dis_mpl2)**3*dis_mpl2)
J_2eI_y = -3.0d0*(-mnp_y*dipole_pl)*(z_mol+radius_mnp-dis_mol)*dipole_mol2/(sqrt(dis_mpl2)**3*dis_mpl2)
J_2eI_z = (dipole_mol2*dipole_pl-3.0d0*(z_mol+radius_mnp-dis_mol)**2/dis_mpl2*dipole_pl*dipole_mol2)/(sqrt(dis_mpl2)**3)
J_2eI(-3) = J_2eI_x
J_2eI(-4) = J_2eI_y
J_2eI(-5) = J_2eI_z
! H model:
J_1e2e(-1) = (dipole_mol1*dipole_mol2)/(dis_mol**3)
! J model:
!J_1e2e(-1) = -2*(dipole_mol1*dipole_mol2)/(dis_mol**3)
J_1e2e(0) = J_1e2e(-1)
do a_sub = -5, -3
  J_1eI (a_sub) = J_1eI (a_sub)*ratio_dd
  J_2eI (a_sub) = J_2eI (a_sub)*ratio_dd
end do
do e_sub = -1,0
  J_1e2e(e_sub) = J_1e2e(e_sub)*ratio_dd
end do

dipole_mnp(-5) = dipole_pl*field_z
dipole_mnp(-4) = dipole_pl*field_y
dipole_mnp(-3) = dipole_pl*field_x

do x_sub =1, num_x     
  do y_sub =1, num_y
    do z_sub =1, num_z
      a_sub =(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub
      dis_mT1(a_sub)=((half_x-x_sub)*Ti_x)**2+((half_y-y_sub)*Ti_y)**2+(z_mol-(z_sub-1)*Ti_z)**2
      dis_mT2(a_sub)=((half_x-x_sub)*Ti_x)**2+((half_y-y_sub)*Ti_y)**2+(z_mol-dis_mol-(z_sub-1)*Ti_z)**2
      dis_Tpl(a_sub)=((x_sub-half_x)*Ti_x-mnp_x)**2+((y_sub-half_y)*Ti_y-mnp_y)**2+((z_sub-1)*Ti_z+radius_mnp)**2
      omega_alfa(a_sub)=omega_semi  
      gama_alfa(a_sub)=gama_semi
    end do
  end do
end do 

do x_sub =1, num_x-1     
  do y_sub =1, num_y-1
    do z_sub =1, num_z-1
      if(total_num == num_max) then
        a_sub =0
      else
        a_sub =(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub
      end if
      dis_mT1(a_sub+num_max) = ((half_x-(x_sub-1))*Ti_x-Ti_x/2.0d0)**2 &
                              +((half_y-(y_sub-1))*Ti_y-Ti_y/2.0d0)**2 &
                              +(z_mol-(z_sub-1)*Ti_z-Ti_z/2.0d0)**2
      dis_mT2(a_sub+num_max) = ((half_x-(x_sub-1))*Ti_x-Ti_x/2.0d0)**2 &
                              +((half_y-(y_sub-1))*Ti_y-Ti_y/2.0d0)**2 &
                              +(z_mol-dis_mol-(z_sub-1)*Ti_z-Ti_z/2.0d0)**2
      dis_Tpl(a_sub+num_max) = ((x_sub-1-half_x)*Ti_x+Ti_x/2.0d0-mnp_x)**2 &
                              +((y_sub-1-half_y)*Ti_y+Ti_y/2.0d0-mnp_y)**2 &
                              +((z_sub-1)*Ti_z+Ti_z/2.0d0+radius_mnp)**2
      omega_alfa(a_sub+num_max)=omega_semi  
      gama_alfa(a_sub+num_max)=gama_semi
    end do
  end do
end do

do a_sub = -5,0   
  do b_sub = -5,total_num 
    if((a_sub == b_sub).and.(a_sub.gt.0)) then
      omega_telta(a_sub,b_sub)=0.0d0  
    else
      omega_telta(a_sub,b_sub)=omega_alfa(a_sub)-omega_alfa(b_sub)-ii*gama_alfa(a_sub)-ii*gama_alfa(b_sub)
    end if
  end do            
end do 
do a_sub = 1,total_num
  omega_telta(1,a_sub)=omega_alfa(a_sub)
end do
return
end subroutine calpar 

subroutine calnearatom (nearatom,ncenter,nearatom2)
use mytype; use params
integer                                   :: num
integer                                   :: num_sub
integer                                   :: x_sub, y_sub, z_sub
integer                                   :: a_sub, c_sub
integer                                   :: com_sub, near_sub
integer, dimension(1:num_max,1:6)         :: nearatom
integer, dimension(1:num_submax,1:6)      :: nearatom2
integer, dimension(1:num_submax,1:8)      :: ncenter
do a_sub = 1, num_max
  do x_sub = 1,num_x   
    do y_sub = 1,num_y
      do z_sub = 1,num_z
        com_sub=(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub
        if (a_sub == com_sub)then         
          if (x_sub.ne.1) then      
            c_sub =(x_sub-2)*num_y*num_z+(y_sub-1)*num_z+z_sub
            nearatom(a_sub,1)=c_sub 
          else
            nearatom(a_sub,1)=0 
          end if 
          if (x_sub.ne.num_x) then
            c_sub =(x_sub)*num_y*num_z+(y_sub-1)*num_z+z_sub    
            nearatom(a_sub,2)=c_sub
          else
            nearatom(a_sub,2)=0 
          end if 
          if (y_sub.ne.1) then
            c_sub =(x_sub-1)*num_y*num_z+(y_sub-2)*num_z+z_sub 
            nearatom(a_sub,3)=c_sub
          else
            nearatom(a_sub,3)=0  
          end if 
          if (y_sub.ne.num_y) then           
            c_sub =(x_sub-1)*num_y*num_z+(y_sub)*num_z+z_sub 
            nearatom(a_sub,4)=c_sub
          else
            nearatom(a_sub,4)=0
          end if 
          if (z_sub.ne.1) then
            c_sub =(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub-1         
            nearatom(a_sub,5)=c_sub
          else
            nearatom(a_sub,5)=0
          end if 
          if (z_sub.ne.num_z) then
            c_sub =(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub+1
            nearatom(a_sub,6)=c_sub
          else
            nearatom(a_sub,6)=0
          end if 
        end if     
      end do  
    end do   
  end do  
end do

do near_sub = 1, num_submax
  do x_sub = 1,num_x-1   
    do y_sub = 1,num_y-1
      do z_sub = 1,num_z-1   
        com_sub=(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub
        if (near_sub == com_sub) then         
          if (x_sub .ne. 1) then      
            c_sub =(x_sub-2)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub
            nearatom2(near_sub,1)=c_sub         
          else
            nearatom2(near_sub,1)=0 
          end if 
          if (x_sub .ne. (num_x-1)) then
            c_sub =(x_sub)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub    
            nearatom2(near_sub,2)=c_sub
          else
            nearatom2(near_sub,2)=0 
          end if 
          if (y_sub .ne. 1) then
            c_sub =(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub-2)*(num_z-1)+z_sub 
            nearatom2(near_sub,3)=c_sub
          else
            nearatom2(near_sub,3)=0  
          end if 
          if (y_sub .ne. (num_y-1)) then           
            c_sub =(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub)*(num_z-1)+z_sub 
            nearatom2(near_sub,4)=c_sub
          else
            nearatom2(near_sub,4)=0
          end if 
          if (z_sub .ne. 1) then
            c_sub =(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub-1        
            nearatom2(near_sub,5)=c_sub
          else
            nearatom2(near_sub,5)=0
          end if
          if (z_sub .ne. num_z-1) then
            c_sub =(x_sub-1)*(num_y-1)*(num_z-1)+(y_sub-1)*(num_z-1)+z_sub+1
            nearatom2(near_sub,6)=c_sub
          else
            nearatom2(near_sub,6)=0
          end if 
          ncenter(near_sub,1)=(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub
          ncenter(near_sub,2)=(x_sub-1)*num_y*num_z+(y_sub-1)*num_z+z_sub+1 
          ncenter(near_sub,3)=(x_sub-1)*num_y*num_z+(y_sub)*num_z+z_sub
          ncenter(near_sub,4)=(x_sub-1)*num_y*num_z+(y_sub)*num_z+z_sub+1
          ncenter(near_sub,5)=(x_sub)*num_y*num_z+(y_sub-1)*num_z+z_sub
          ncenter(near_sub,6)=(x_sub)*num_y*num_z+(y_sub-1)*num_z+z_sub+1 
          ncenter(near_sub,7)=(x_sub)*num_y*num_z+(y_sub)*num_z+z_sub
          ncenter(near_sub,8)=(x_sub)*num_y*num_z+(y_sub)*num_z+z_sub+1  
        end if
      end do  
    end do   
  end do  
end do

end subroutine calnearatom

subroutine cornercenter(ncenter,cor_cen)
use mytype; use params
integer                               :: n_sub,m_sub,a_sub,b_sub,c_sub
integer, dimension(1:num_max,1:8)     :: cor_cen
integer, dimension(1:num_submax,1:8)  :: ncenter
do a_sub =1, num_max
  do n_sub =1,8
    cor_cen(a_sub,n_sub)=0
  end do
end do   
do b_sub =1,num_submax       
  do n_sub =1,8
    do a_sub =1,num_max 
      if(ncenter(b_sub,n_sub)==a_sub) then
        do m_sub =1,8   
          if(cor_cen(a_sub,m_sub)==0) then
            cor_cen(a_sub,m_sub)=b_sub
            exit
          end if      
        end do
      end if
    end do
  end do
end do  
end subroutine cornercenter

subroutine derivs(func, t_sub,nearatom,nearatom2,ncenter,cor_cen,omega_telta, J_eI,J_1eI,J_2eI,J_1e2e,dipole_mnp,dfdt)
  
! computation of time derivatives
use mytype; use params
implicit none
real (dp)                                 :: t_sub    
complex (dp)                              :: func(1:length_coeff), dfdt(1:length_coeff)
integer                                   :: a_sub, b_sub, c_sub, n_sub
integer                                   :: e_sub
integer                                   :: i_sub
integer                                   :: com_sub, num
integer                                   :: x_sub, y_sub, z_sub, sublevel
complex (dp),dimension(-5:1,-5:total_num) :: rwa_sub
complex (dp),dimension(-5:1,-5:total_num) :: dc_subdt
complex (dp),dimension(-5:1,-5:total_num) :: omega_telta
real (dp),dimension(-1: 0)                :: field_sub
real (dp),dimension(-1: 0)                :: dipole_mol
real (dp),dimension(-5:-3)                :: dipole_mnp
real (dp),dimension(-5:-3)                :: J_1eI,J_2eI
real (dp),dimension(-1: 0)                :: J_1e2e
real (dp),dimension(-1: 0,-5:-3)          :: J_eI
real (dp),dimension(1:total_num)          :: dis_Tpl
real (dp),dimension(1:total_num)          :: dis_mT1
real (dp),dimension(1:total_num)          :: dis_mT2
complex (dp)                              :: coeff_e
integer, dimension(1:num_max,1:6)         :: nearatom
integer, dimension(1:num_submax,1:6)      :: nearatom2
integer, dimension(1:num_submax,1:8)      :: ncenter
integer, dimension(1:num_max,1:8)         :: cor_cen 

call single_to_multi(func,rwa_sub)

dipole_mol(-1) = dipole_mol1
dipole_mol( 0) = dipole_mol2

do e_sub = -1, 0
  field_sub(e_sub) = omega_rabi/dipole_mol(e_sub)*exp(-2.0d0*((t_sub-pulse_max)/pulse_length)**2)

if (e_sub.eq.-1) then
  do a_sub = -5, -3
    J_eI(e_sub,a_sub)=J_1eI(a_sub)
  end do
end if
if (e_sub.eq.0) then
  do a_sub = -5, -3
    J_eI(e_sub,a_sub)=J_2eI(a_sub)
  end do
end if

end do

! Computation of conduction band coefficients  
   
do a_sub = -5, 0
  do b_sub = -5, total_num
    dc_subdt(a_sub, b_sub) = 0.0d0
  end do
end do

do a_sub = 1, total_num
  dc_subdt(1, a_sub) = 0.0d0
end do

do a_sub = -5, 0
  do b_sub = -5, total_num

! 1 ~ 14:

! 1.r_gg: rou(-2,-2)        
    if ((a_sub.eq.-2).and.(b_sub.eq.-2)) then
      do e_sub = -1, 0
        do c_sub = -5, -3 
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+2.0d0*gama_pl*rwa_sub(c_sub,c_sub)
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+2.0d0*dipole_mnp(c_sub)*field_sub(e_sub)*real(ii*rwa_sub(c_sub,b_sub))
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+2.0d0 &
            *dipole_mol(e_sub)*field_z*field_sub(e_sub)*real(ii*rwa_sub(e_sub,b_sub))
        end do
      end do
    end if
! 2.r_1eg: rou(-1,-2) 
    if ((a_sub.eq.-1).and.(b_sub.eq.-2)) then
      do e_sub = -1, 0
        do c_sub = -5, -3       
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1eI(c_sub)*rwa_sub(c_sub,b_sub)
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mnp(c_sub)*field_sub(e_sub)*rwa_sub(a_sub,c_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub) &
          -ii*(omega_telta(a_sub,b_sub)-omega_0)*rwa_sub(a_sub,b_sub) &
          -ii*J_1em*conjg(rwa_sub(b_sub,pmol)) &
          +ii*dipole_mol1*field_z*field_sub(e_sub)*(rwa_sub(b_sub,b_sub))
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(a_sub,e_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1e2e(0)*rwa_sub(0,b_sub)
      end do
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if
! 3.r_2eg: rou(0,-2)
    if ((a_sub.eq.0).and.(b_sub.eq.-2)) then
      do e_sub = -1, 0
        do c_sub = -5, -3       
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_2eI(c_sub)*rwa_sub(c_sub,b_sub)
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mnp(c_sub)*field_sub(e_sub)*rwa_sub(a_sub,c_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub) &
          -ii*(omega_telta(a_sub,b_sub)-omega_0)*rwa_sub(a_sub,b_sub) &
          +ii*dipole_mol2*field_z*field_sub(e_sub)*(rwa_sub(b_sub,b_sub))
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(a_sub,e_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1e2e(-1)*rwa_sub(-1,b_sub)
      end do
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if

! 4. r_1e1e: rou(-1,-1)
    if ((a_sub.eq.-1).and.(b_sub.eq.-1)) then
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-2.0d0*J_1eI(c_sub)*real(ii*rwa_sub(c_sub,b_sub))
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+2.0d0*dipole_mol1*field_z*field_sub(e_sub)*real(ii*rwa_sub(-2,a_sub))  
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-2.0d0*J_1em*real(ii*conjg(rwa_sub(b_sub,pmol)))
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-2.0d0*J_1e2e(0)*real(ii*rwa_sub(0,b_sub))
    end if

! 5. r_2e2e: rou(0,0)
    if ((a_sub.eq.0).and.(b_sub.eq.0)) then
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-2.0d0*J_2eI(c_sub)*real(ii*rwa_sub(c_sub,b_sub))
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+2.0d0*dipole_mol2*field_z*field_sub(e_sub)*real(ii*rwa_sub(-2,a_sub))  
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-2.0d0*J_1e2e(-1)*real(ii*rwa_sub(-1,b_sub))
    end if

! 6. r_1e2e: rou(-1,0)
    if ((a_sub.eq.-1).and.(b_sub.eq.0)) then
      i_sub = 0
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1eI(c_sub)*rwa_sub(c_sub,b_sub) &
            +ii*J_2eI(c_sub)*rwa_sub(a_sub,c_sub)
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1e2e(-1)*rwa_sub(b_sub,b_sub) &
            +ii*J_1e2e(i_sub)*rwa_sub(a_sub,a_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*dipole_mol1*field_z*field_sub(e_sub)*rwa_sub(-2,b_sub) &
          -ii*rwa_sub(a_sub,-2)*dipole_mol2
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*(omega_telta(a_sub,b_sub))*rwa_sub(a_sub,b_sub)
!      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1em*rwa_sub(pmol,b_sub)
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1em*rwa_sub(b_sub,pmol)
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if

! 7. r_Ig: rou(I,-2)
    if ((a_sub.le.-3).and.(b_sub.eq.-2)) then
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mnp(c_sub)*field_sub(e_sub)*rwa_sub(a_sub,c_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1eI(a_sub)*rwa_sub(-1,b_sub) &
          -ii*J_2eI(a_sub)*rwa_sub(0,b_sub) &
          -ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(a_sub,e_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*dipole_mnp(a_sub)*field_sub(e_sub)*rwa_sub(b_sub,b_sub)
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*(omega_telta(a_sub,b_sub)-omega_0)*rwa_sub(a_sub,b_sub) 
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if

! 8.r_gm, r_mg=(r_gm)*
    if ((a_sub.eq.-2).and.(b_sub.ge.1)) then
      if(b_sub.le.num_max) then
        do n_sub = 1, 6
          if (nearatom(b_sub,n_sub).ne.0) then
            c_sub = nearatom(b_sub,n_sub)
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mn*rwa_sub(a_sub,c_sub) 
          end if
        end do
        do n_sub = 1, 8
          if(cor_cen(b_sub,n_sub).ne.0) then
            c_sub = num_max+cor_cen(b_sub,n_sub)
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mnn*rwa_sub(a_sub,c_sub)
          end if
        end do
      end if
      if(b_sub.gt.num_max) then
        do n_sub = 1, 6
          if (nearatom2(b_sub-num_max,n_sub).ne.0) then
            c_sub = num_max+nearatom2(b_sub-num_max,n_sub)
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mn*rwa_sub(a_sub, c_sub) 
          end if
        end do
        do n_sub = 1, 8
          c_sub =ncenter(b_sub-num_max,n_sub) 
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mnn*rwa_sub(a_sub,c_sub) 
        end do
      end if
      if (b_sub.eq.pmol) then
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*J_1em*rwa_sub(a_sub,-1)
      end if
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*dipole_mnp(c_sub)*field_sub(e_sub)*rwa_sub(c_sub,b_sub)
        end do
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub) &
          -ii*(omega_telta(a_sub,b_sub)+omega_0)*rwa_sub(a_sub,b_sub) &
          +ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(e_sub,b_sub)
      end do
      dc_subdt(b_sub,a_sub)=conjg(dc_subdt(a_sub,b_sub))
    end if

! 9. r_1eI: rou(-1,I), a_sub=-1, b_sub=Iâ‰¤-3:
    if ((a_sub.eq.-1).and.(b_sub.le.-3)) then
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1eI(c_sub)*rwa_sub(c_sub,b_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mnp(b_sub)*field_sub(e_sub)*rwa_sub(a_sub,-2)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*dipole_mol(a_sub)*field_sub(e_sub)*field_z*rwa_sub(-2,b_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*J_1eI(b_sub)*rwa_sub(a_sub,a_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1e2e(0)*rwa_sub(0,b_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*J_2eI(b_sub)*rwa_sub(a_sub,0)
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1em*conjg(rwa_sub(b_sub,pmol))
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub, b_sub)*rwa_sub(a_sub,b_sub)
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if

! 10. r_2eI: rou(0,I), a_sub=0, b_sub=Iâ‰¤-3:
    if ((a_sub.eq.0).and.(b_sub.le.-3)) then
      do e_sub = -1, 0
        do c_sub = -5, -3
          dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_2eI(c_sub)*rwa_sub(c_sub,b_sub)
        end do
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*dipole_mnp(b_sub)*field_sub(e_sub)*rwa_sub(a_sub,-2)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*dipole_mol(a_sub)*field_sub(e_sub)*field_z*rwa_sub(-2,b_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*J_2eI(b_sub)*rwa_sub(a_sub,a_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*J_1e2e(-1)*rwa_sub(-1,b_sub)
        dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)+ii*J_1eI(b_sub)*rwa_sub(a_sub,-1)
      end do
      dc_subdt(a_sub,b_sub) = dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub, b_sub)*rwa_sub(a_sub,b_sub)
      dc_subdt(b_sub,a_sub) = conjg(dc_subdt(a_sub,b_sub))
    end if

! 11. r_1em : rou(-1,b_sub), a_sub=-1, b_sub=mâ‰¥1:
    if ((a_sub.eq.-1).and.(b_sub.ge.1)) then
      e_sub = -1
      do c_sub = -5, -3
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_1eI(c_sub)*rwa_sub(c_sub,b_sub)
      end do
      if(b_sub.le.num_max) then
        do n_sub = 1, 6                     
          if (nearatom(b_sub,n_sub).ne.0) then  
            c_sub = nearatom(b_sub,n_sub)  
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)       
          end if 
        end do 
        do n_sub = 1, 8
          if(cor_cen(b_sub,n_sub).ne.0) then
            c_sub = num_max+cor_cen(b_sub,n_sub)
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mnn*rwa_sub(a_sub,c_sub)        
          end if
        end do              
      end if
      if(b_sub.gt.num_max) then
        do n_sub = 1, 6                     
          if (nearatom2(b_sub-num_max,n_sub) /= 0) then  
            c_sub = num_max+nearatom2(b_sub-num_max,n_sub)  
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)     
          end if 
        end do 
        do n_sub = 1, 8             
          c_sub =ncenter(b_sub-num_max,n_sub) 
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mnn*rwa_sub(a_sub,c_sub) 
        end do   
      end if 
      if (b_sub.eq.pmol) then
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*J_1em*rwa_sub(e_sub,e_sub)
      end if
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_1em*rwa_sub(1,pmol)*conjg(rwa_sub(1,b_sub))
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(-2,b_sub)
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_1e2e(e_sub)*rwa_sub(0,b_sub)
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub,b_sub)*rwa_sub(a_sub,b_sub)
    end if

! 12. r_2em : rou(0,b_sub), a_sub=0, b_sub=mâ‰¥1:
    if ((a_sub.eq.0).and.(b_sub.ge.1)) then
      e_sub = 0
      do c_sub = -5, -3
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_2eI(c_sub)*rwa_sub(c_sub,b_sub)
      end do
      if(b_sub.le.num_max) then
        do n_sub = 1, 6                     
          if (nearatom(b_sub,n_sub).ne.0) then  
            c_sub = nearatom(b_sub,n_sub)  
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)       
          end if 
        end do 
        do n_sub = 1, 8
          if(cor_cen(b_sub,n_sub).ne.0) then
            c_sub = num_max+cor_cen(b_sub,n_sub)
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mnn*rwa_sub(a_sub,c_sub)        
          end if
        end do              
      end if
      if(b_sub.gt.num_max) then !
        do n_sub = 1, 6                     
          if (nearatom2(b_sub-num_max,n_sub) /= 0) then  
            c_sub = num_max+nearatom2(b_sub-num_max,n_sub)  
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)     
          end if 
        end do 
        do n_sub = 1, 8             
          c_sub =ncenter(b_sub-num_max,n_sub) 
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*v_mnn*rwa_sub(a_sub,c_sub) 
        end do   
      end if 
      if (b_sub.eq.pmol) then
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*J_1em*rwa_sub(e_sub,-1)
      end if
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*dipole_mol(e_sub)*field_z*field_sub(e_sub)*rwa_sub(-2,b_sub) !
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_1e2e(e_sub)*rwa_sub(-1,b_sub) !
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub,b_sub)*rwa_sub(a_sub,b_sub)  !
    end if

! 13. r_II' : a_sub = I(-5,-4,-3), b_sub = I'(-5,-4,-3)
    if((a_sub.le.-3).and.(b_sub.le.-3)) then
      if (a_sub.le.b_sub) then
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub,b_sub)*rwa_sub(a_sub,b_sub)
        do e_sub = -1, 0
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_eI(e_sub,a_sub)*(rwa_sub(e_sub,b_sub)-rwa_sub(a_sub,e_sub))
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*dipole_mnp(a_sub)*field_sub(e_sub)*rwa_sub(-2,b_sub)
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*dipole_mnp(b_sub)*field_sub(e_sub)*rwa_sub(a_sub,-2)
        end do
        if(a_sub.ne.b_sub) then
          dc_subdt(b_sub,a_sub)=conjg(dc_subdt(a_sub,b_sub))
        end if
      end if
    end if

! 14. r_Im : a_sub = I(-5,-4,-3), b_sum = m(1,2,3,...)
    if((a_sub.le.-3).and.(b_sub.ge.1)) then
      if(b_sub.le.num_max) then
        do n_sub =1, 6                  
          if (nearatom(b_sub,n_sub).ne.0) then  
            c_sub = nearatom(b_sub,n_sub) 
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)             
          end if 
        end do
        do n_sub =1, 8
          if (cor_cen(b_sub,n_sub).ne.0) then  
            c_sub = num_max+cor_cen(b_sub,n_sub) 
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mnn*rwa_sub(a_sub,c_sub)            
          end if
        end do 
      end if
      if(b_sub.gt.num_max) then
        do n_sub =1,6                     
          if (nearatom2(b_sub-num_max,n_sub).ne.0) then  
            c_sub = num_max+nearatom2(b_sub-num_max,n_sub)  
            dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mn*rwa_sub(a_sub,c_sub)        
          end if 
        end do 
        do n_sub =1,8             
          c_sub =ncenter(b_sub-num_max,n_sub) 
          dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*V_mnn*rwa_sub(a_sub,c_sub) 
        end do
      end if
      dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*omega_telta(a_sub,b_sub)*rwa_sub(a_sub,b_sub)
      do e_sub = -1, 0
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)-ii*J_eI(e_sub,a_sub)*rwa_sub(e_sub,b_sub)
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*dipole_mnp(a_sub)*field_sub(e_sub)*rwa_sub(-2,b_sub)
      end do
      if(b_sub.eq.pmol) then
        dc_subdt(a_sub,b_sub)=dc_subdt(a_sub,b_sub)+ii*J_1em*rwa_sub(a_sub,-1)
      end if
    end if






  end do
end do

! 15:
! 15. r_mn : with approximation for a large TiO2 lattice
coeff_e = 0.0d0
num = 0
do a_sub =1,total_num
  if (abs(real(rwa_sub(1,a_sub))).ge.1.0d-30 .and.abs(real(ii*rwa_sub(1,a_sub))).ge.1.0d-30) then
    coeff_e=coeff_e+rwa_sub(-1,a_sub)/conjg(rwa_sub(1,a_sub))
    num=num+1
  end if
end do
if (num /= 0)then   
  coeff_e=coeff_e/num
else      
  coeff_e=sqrt(real(rwa_sub(-1,-1)))
end if
do a_sub = 1, total_num
  dc_subdt(1,a_sub) = dc_subdt(1,a_sub)-ii*omega_telta(1,a_sub)*rwa_sub(1,a_sub) 
  if(a_sub.le.num_max) then
    do n_sub =1, 6
      if (nearatom(a_sub,n_sub).ne.0) then  
        b_sub=nearatom(a_sub,n_sub) 
        dc_subdt(1,a_sub)=dc_subdt(1,a_sub)-ii*V_mn*rwa_sub(1,b_sub)
      end if 
    end do 
    do n_sub =1, 8
      if(cor_cen(a_sub,n_sub).ne.0) then
        b_sub = num_max+cor_cen(a_sub,n_sub)
        dc_subdt(1,a_sub)=dc_subdt(1,a_sub)-ii*V_mnn*rwa_sub(1,b_sub)
      end if
    end do
  end if 
  if(a_sub.gt.num_max) then
    do n_sub =1, 6
      if (nearatom2(a_sub-num_max,n_sub).ne.0) then
        b_sub = num_max+nearatom2(a_sub-num_max,n_sub)
        dc_subdt(1,a_sub)=dc_subdt(1,a_sub)-ii*V_mn*rwa_sub(1,b_sub)
      end if
    end do
    do n_sub =1, 8
      b_sub =ncenter(a_sub-num_max,n_sub) 
      dc_subdt(1,a_sub)=dc_subdt(1,a_sub)-ii*V_mnn*rwa_sub(1,b_sub)       
    end do   
  end if  
  if(a_sub == pmol) then
    dc_subdt(1,a_sub) = dc_subdt(1,a_sub)-ii*J_1em*coeff_e
  end if
end do

call multi_to_single(dc_subdt,dfdt)

end subroutine derivs

subroutine rk4 (y,x,h,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_1eI,J_2eI,J_1e2e,dipole_mnp,yout)
use mytype; use params
implicit none
complex(dp), dimension(:), intent(in)       :: y
real(dp), intent(in)                        :: x,h
complex(dp), dimension(:), intent(out)      :: yout
real(dp)                                    :: h6,hh,xh
complex(dp), dimension(size(y))             :: dyi,dym,dyt,yt
integer, dimension(1:num_max,1:6)           :: nearatom
integer, dimension(1:num_submax,1:6)        :: nearatom2
integer, dimension(1:num_max,1:8)           :: cor_cen
integer, dimension(1:num_submax,1:8)        :: ncenter
complex (dp),dimension(-5:1,-5:total_num)   :: omega_telta
real (dp),dimension(-5:total_num)           :: omega_alfa,gama_alfa
real (dp),dimension(-1: 0,-5:-3)            :: J_eI
real (dp),dimension(-5:-3)                  :: J_1eI,J_2eI
real (dp),dimension(-1: 0)                  :: J_1e2e
real (dp),dimension(-5:-3)                  :: dipole_mnp
real (dp),dimension(1:total_num)            :: dis_Tpl
real (dp),dimension(1:total_num)            :: dis_mT1
real (dp),dimension(1:total_num)            :: dis_mT2
call derivs(y,x,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_eI,J_1eI,J_2eI,J_1e2e,dipole_mnp,dyi)
  hh = h*0.5d0
  h6 = h/6.d0
  xh = x+hh
  yt = y+hh*dyi
call derivs(yt,xh,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_eI,J_1eI,J_2eI,J_1e2e,dipole_mnp,dyt)
  yt = y+hh*dyt
call derivs(yt,xh,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_eI,J_1eI,J_2eI,J_1e2e,dipole_mnp,dym)
  yt = y+h*dym
  dym = dyt+dym
call derivs(yt,x+h,nearatom,nearatom2,ncenter,cor_cen,omega_telta,J_eI,J_1eI,J_2eI,J_1e2e,dipole_mnp,dyt)
  yout = y+h6*(dyi+dyt+2.d0*dym)
end subroutine rk4