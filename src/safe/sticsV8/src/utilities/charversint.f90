    subroutine charversint(cara,cara_entier)
! codebbch_entier,sc%codebbch(p%ipl)
! fonction nous permettant de transformer un entier en caractere
! utilisé pour les annees donc un entier compris entre 0 et 9999

  implicit none

    character(len=3) ,intent(IN) :: cara  
    integer , intent(OUT) :: cara_entier  
! variables locales
    integer ent1,ent2,ent3,i
    character :: chiffre(10)  
    chiffre = (/'0','1','2','3','4','5','6','7','8','9'/)



!    write(*,*)'cara ',cara
    do i=1,10

!if (lge(P_codesimul,'feuille') .eqv. .FALSE.) then
     if(lge(cara(1:1),chiffre(i)).eqv. .TRUE.)ent1=i-1
    if(cara(2:2)==chiffre(i))ent2=i-1
    if(cara(3:3).eq.chiffre(i))ent3=i-1
    enddo
! print *, ent1,ent2,ent3
    if(cara(1:1)=='-')then
        cara_entier=-(ent2*10+ent3)
    else
        cara_entier=ent1*10+ent2
    endif

!    write(*,*)'charversint',cara,cara_entier
    return
    end
 
 
