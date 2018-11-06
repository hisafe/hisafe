!> Module Messages
!! Description
!! management messages to history
module Messages

USE iso_varying_string

integer :: fichist = 5 ! stdin  
logical :: onEcrit = .false.  

! DR 18/03/2015 on augmente la taille du vecteur message qui etait insuffisante
integer :: nbMessages = 6500
type(varying_string) :: messagesStr(6500)


interface EnvoyerMsgHistorique
module procedure e_EnvoyerMsgHistorique,r_EnvoyerMsgHistorique,         &
                 i_EnvoyerMsgHistorique,c_EnvoyerMsgHistorique,         &
                 cc_EnvoyerMsgHistorique,cc_r_EnvoyerMsgHistorique,     &
                 cc_i_EnvoyerMsgHistorique,cc_c_EnvoyerMsgHistorique,   &
                 cc_t_r_EnvoyerMsgHistorique
end interface EnvoyerMsgHistorique

!interface setSorties
!module procedure isetSorties,rsetSorties
!end interface setSorties

contains

subroutine remplirMessages(langue)

    character(len=2), intent(IN) :: langue  

    select case(langue)

! DR 13/08/2012 maintenant tout est en anglais
        case('FR')
!            call remplirMessages_FR(messagesStr,nbMessages) !TODO: taille définie en dur, à passer en dynamique un jour ?
!            write(*,*)'!!!! FR'
        case default
            call remplirMessages_ENG(messagesStr,nbMessages)
!            write(*,*)'!!!! ang'
    end select

return
end subroutine remplirMessages


subroutine setFichierHistorique(fic)

    integer, intent(IN) :: fic  

    fichist = fic

return
end subroutine setFichierHistorique

subroutine setFlagEcriture(flag)

    logical, intent(IN) :: flag  

    onEcrit = flag

return
end subroutine setFlagEcriture

! DR 06/09/2012 me semble devenue obsolete
!function getMessage(nbM) result(msg)
!
!    integer, intent(IN) :: nbM
!
!    type(varying_string) :: msg
!
!    msg = messagesStr(nbM)
!
!return
!end function getMessage


subroutine e_EnvoyerMsgHistorique(numMsg)

implicit none

    integer, intent(IN)           :: numMsg  

    if (.not. onEcrit) return

    write (fichist,*) char(messagesStr(numMsg))

end subroutine e_EnvoyerMsgHistorique

subroutine r_EnvoyerMsgHistorique(numMsg, valeurs,fmt)

implicit none

    integer, intent(IN) :: numMsg  
    real,    intent(IN) :: valeurs  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) char(messagesStr(numMsg)), valeurs
    else
      write (fichist,*) char(messagesStr(numMsg)), valeurs
    endif

end subroutine r_EnvoyerMsgHistorique

subroutine i_EnvoyerMsgHistorique(numMsg, valeurs,fmt)

implicit none

    integer, intent(IN) :: numMsg  
    integer, intent(IN) :: valeurs  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) char(messagesStr(numMsg)), valeurs
    else
      write (fichist,*) char(messagesStr(numMsg)), valeurs
    endif

end subroutine i_EnvoyerMsgHistorique

subroutine c_EnvoyerMsgHistorique(numMsg, valeurs,fmt)

implicit none

    integer,           intent(IN)   :: numMsg  
    character(len=*), intent(IN)   :: valeurs  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) char(messagesStr(numMsg)), valeurs
    else
      write (fichist,*) char(messagesStr(numMsg)), valeurs
    endif

end subroutine c_EnvoyerMsgHistorique

subroutine cc_EnvoyerMsgHistorique(message)

implicit none

    character(len=*), intent(IN)   :: message  

    if (.not. onEcrit) return

    write (fichist,*) message

end subroutine cc_EnvoyerMsgHistorique

subroutine cc_r_EnvoyerMsgHistorique(message,valeur,fmt)

implicit none

    character(len=*), intent(IN)   :: message  
    real,             intent(IN)   :: valeur  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) message, valeur
    else
      write (fichist,*) message, valeur
    endif

end subroutine cc_r_EnvoyerMsgHistorique

subroutine cc_t_r_EnvoyerMsgHistorique(message,valeur,fmt)

implicit none

    character(len=*), intent(IN)   :: message  
    real,             intent(IN)   :: valeur(:)  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) message, valeur
    else
      write (fichist,*) message, valeur
    endif

end subroutine cc_t_r_EnvoyerMsgHistorique

subroutine cc_i_EnvoyerMsgHistorique(message,valeur,fmt)

implicit none

    character(len=*), intent(IN)   :: message  
    integer,          intent(IN)   :: valeur  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) message, valeur
    else
      write (fichist,*) message, valeur
    endif

end subroutine cc_i_EnvoyerMsgHistorique

subroutine cc_c_EnvoyerMsgHistorique(message,valeur,fmt)

implicit none

    character(len=*), intent(IN)   :: message  
    character(len=*), intent(IN)   :: valeur  
    character(len=*), intent(IN), optional   :: fmt  

    if (.not. onEcrit) return

    if (present(fmt)) then
      write (fichist,fmt) message, valeur
    else
      write (fichist,*) message, valeur
    endif

end subroutine cc_c_EnvoyerMsgHistorique
end module Messages



 
 
