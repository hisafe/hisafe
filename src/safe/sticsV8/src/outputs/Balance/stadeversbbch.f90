! sous programme de conversion des stades en code bbch pour les sorties journalieres
!> conversion of the development stages in BBCH code
subroutine stadeversbbch(n,p)

USE Plante

implicit none

    integer,          intent(IN)  :: n  
    type(Plante_),    intent(INOUT)  :: p

!    character(len=3), intent(OUT) :: codebbch(2)   !> // OUTPUT // BBCH stage (see plant file) // SD

!!!        p%codebbch = '-01'

 !!!MODIF HISAFE 1 : suppression des chaines de caractères
 !!!MODIF HISAFE à réactiver si UTILE ?
 !!!       if (n >= p%nplt .and. p%nplt /= 0) p%codebbch = p%P_stadebbchplt
 !!!       if (n >= p%nger .and. p%nger /= 0) p%codebbch = p%P_stadebbchger
 !!!       if (n >= p%nlev .and. p%nlev /= 0) p%codebbch = p%P_stadebbchlev
 !!!       if (n >= p%namf .and. p%namf /= 0) p%codebbch = p%P_stadebbchamf
 !!!       if (n >= p%nlax .and. p%nlax /= 0) p%codebbch = p%P_stadebbchlax
 !!!       if (n >= p%nflo .and. p%nflo /= 0) p%codebbch = p%P_stadebbchflo
 !!!       if (n >= p%ndrp .and. p%ndrp /= 0) p%codebbch = p%P_stadebbchdrp
 !!!       if (n >= p%nsen .and. p%nsen /= 0) p%codebbch = p%P_stadebbchsen
 !!!       if (n >= p%ndebdes .and. p%ndebdes /= 0) p%codebbch = p%P_stadebbchdebdes
 !!!       if (n >= p%nmat .and. p%nmat /= 0) p%codebbch = p%P_stadebbchmat
 !!!       if (n == p%nrec .and. p%nrec /= 0) p%codebbch = p%P_stadebbchrec
 !!!       if (n > p%nrec .and. p%nrec /= 0) p%codebbch = '-01'



return
end
 
 
