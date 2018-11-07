! ******************************************************************
! /**
!  * Calculs relatifs à la nappe
!  */
!DR 23/07/2012 pas besoin de codefente
!subroutine nappe(P_codefente,macropor,P_epc,hnappe,hnapperch,hmax,       &
subroutine nappe(macropor,P_epc,hnappe,hnapperch,hmax,       &
                 P_profdrain,nh,profsol,P_bformnappe,exces,              &
                 nhe,hph,hpb,de,P_distdrain,profnappe,P_codhnappe,ldrains) 

!! arguments entrants
!  integer, intent(IN)   ::  P_codefente  !>  // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
  integer, intent(IN)   ::  nh  !>  
  integer, intent(IN)   ::  nhe  !>  
  integer, intent(IN)   ::  P_codhnappe  !>  // PARAMETER // mode of calculation of watertable level // code 1/2 // PARAM // 0 
  real,    intent(IN)   ::  macropor(nh)
  real,    intent(IN)   ::  P_epc(nh)  !< // PARAMETER // thickness of each soil layer // cm   // PARSOL // 1      // OUTPUT // Thickness of the horizon (1 or 2 )// cm
  real,    intent(IN)   ::  exces(0:5)   !> // OUTPUT // Amount of water  present in the macroporosity of the horizon 1  // mm
  real,    intent(IN)   ::  P_profdrain  !>  // PARAMETER // drain depth // cm // PARSOL // 1 
  real,    intent(IN)   ::  profsol  
  real,    intent(IN)   ::  P_bformnappe  !>  // PARAMETER // coefficient of water table shape (artificial drained soil) // SD // PARAM // 1 
  real,    intent(IN)   ::  de  !>  
  real,    intent(IN)   ::  P_distdrain  !>  // PARAMETER // distance to the drain to calculate watertable height // cm // PARAM // 1 
  real,    intent(IN)   :: ldrains  

!! arguments sortants
  real,    intent(OUT)  ::  Hmax  !>    // OUTPUT // Maximum height of water table between drains // cm
  real,    intent(OUT)  ::  Hnappe  !>    // OUTPUT // Height of water table with active effects on the plant // cm
  real,    intent(OUT)  ::  Hnapperch  !>  
  real,    intent(OUT)  ::  Hpb  !>    // OUTPUT // Minimum depth of perched water table // cm
  real,    intent(OUT)  ::  Hph  !>    // OUTPUT // Maximum depth of perched water table // cm
  real,    intent(OUT)  ::  profnappe   !> // OUTPUT // Depth of water table // cm
  
! VARIABLES LOCALES
  integer ::  profhoriz  !>  
  integer ::  icou  !>  
  integer ::  ic  !>  
  integer ::  ii  
  real    ::  hmoy  !>  
  real    ::  d  !>  
  real    ::  x  !>  
  real    ::  w  !>  
  real    ::  cotedrain  

      profhoriz = nhe 
      Hnapperch  = 0.
      Hnappe = 0.
      
      do icou = nh,1,-1
        
      !! ??
        Hmoy = exces(icou)/macropor(icou)*P_epc(icou)
        
      !! HAUTEUR DE LA NAPPE
        if (Hmoy>=P_epc(icou) .and. (icou==nh .or. Hnappe>=(profsol-profhoriz))) then
          Hnappe = Hnappe + Hmoy
        endif
        
        if(Hmoy<P_epc(icou) .and. Hnappe>=(profsol-profhoriz)) then
          Hnappe = Hnappe + Hmoy
        endif
      
      !! ETENDUE DE LA NAPPE PERCHEE
        if(Hmoy>=P_epc(icou) .and. Hnappe<(profsol-profhoriz)) then
          Hnapperch = Hnapperch + Hmoy
        endif
        
        if(Hmoy<P_epc(icou) .and. Hnappe<(profsol-profhoriz) .and. Hnapperch>=0) then
          Hnapperch = Hnapperch + Hmoy
        endif
      
        profhoriz = profhoriz - int(P_epc(icou))
      end do
      
      ! calcul de la profondeur maximale de la nappe perchée (hph)
      hph = 0.
      if (Hnapperch>0.) then
        do ic=1,nh
          if (exces(ic)>0. .and. Hph<=0.) then
            do ii = ic,1,-1
              Hph = Hph + P_epc(ii)
            end do
            Hph = Hph - (exces(ic)/macropor(ic)*P_epc(ic))
          endif
        end do
      endif
      
      
      ! *- NB - 17/06/2004 - calcul de Hpb : pourquoi cumul Hph,Hnapperch ?
      Hpb = Hph + Hnapperch
      
      ! ** NB - le 27/04
      ! *- calcul de la hauteur à l'interdrain
      ! *- Nb - le 05/06
      cotedrain = profsol-P_profdrain
      if ((Hnappe-cotedrain).gt.0.)  then
        Hmax = (Hnappe-cotedrain)/P_Bformnappe
      else
        Hmax = 0.0
      endif
      
      ! NB le 08/06
      ! N Delouvigny
      ! test le 10/07/01 : il peut y avoir de l'eau stagnante 
      ! au dessus du sol (mais on  met  un seuil pour le moment)
      Hmax = min(hmax,P_profdrain+5.0)
      
      ! calcul de la profondeur de nappe utilisée pour l'effet sur la plante
      ! P_codhnappe = 1 : on utilise la hauteur moyenne
      ! P_codhnappe = 2 : on calcule une hauteur à une distance P_distdrain du drain
      ! Nb le 05/06
      if (P_codhnappe==1 .or. hmax<=0.) then
        profnappe = P_profdrain - hnappe + cotedrain
      else
        d = de / hmax
        x = 1.0 - (P_distdrain / ldrains)
        w = -d + sqrt((d+1)**2-x**2*(1+2*d))
        profnappe = P_profdrain - w * hmax
      endif     
      
      ! ** NB - le 05/06 - pour éviter anoxie parasite en bas de profil
      if (profnappe==profsol) profnappe = profsol + 1.0
      
      
      !---------------------------------------------------------------------------
      ! INFO fichiers secondaires
      !---------------------------------------------------------------------------
      !        if(Hnappe.ge.0) then
      !      write(2,222)n,Hnappe
      !  222      format('n',3x,i3,5x,'Hnappe moy (cm)',3x,f6.2)
      !    else
      !      write(2,222)n,0.0
      !    endif
      !    if(Hph.ne.0.and.Hpb.ne.0) then
      !          write(4,*)'n',n,'    Nappe entre  ',Hph,'  cm  et 
      !     s    ',Hpb,'  cm de prof'
      !    else
      !      write(4,*)'n',n,'    Pas de nappe perchée'
      !    endif
      !  if(maj.eq.0) then
      !     write(5,51)n,Hmax,Hnappe
      !   51   format(/,'n',1x,i3,25x,'Hmax',4x,f6.2,1x,'cm',/,'Hnappe avant 
      !     s drainage agricole',11x,f6.2,1x,'cm')
      !  else
      !     write(5,52)n,Hmax,Hnappe
      !   52   format(/,'n',1x,i3,25x,'Hmax',4x,f6.2,1x,'cm',/,'Hnappe après 
      !     s drainage agricole',11x,f6.2,1x,'cm')
      !  endif

return 
end 
 
