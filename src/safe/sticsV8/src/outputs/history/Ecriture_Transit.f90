!> write to the file history.sti
!!
!! general paramv6 parameters
subroutine Ecriture_Transit(t)

USE Stics
USE Messages

implicit none

    type(Stics_Transit_), intent(IN) :: t  

!TODO: revoir les paramètres

    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique(264)
    call EnvoyerMsgHistorique('*********************************************')

    call EnvoyerMsgHistorique('codeNmindec',t%P_codeNmindec)
    call EnvoyerMsgHistorique('rapNmindec',t%P_rapNmindec)
    call EnvoyerMsgHistorique('fNmindecmin ',t%P_fNmindecmin )
    call EnvoyerMsgHistorique('P_codetempfauche',t%P_codetempfauche)
    call EnvoyerMsgHistorique('P_coefracoupe(1)',t%P_coefracoupe(1))
    call EnvoyerMsgHistorique('P_codepluiepoquet',t%P_codepluiepoquet)
    call EnvoyerMsgHistorique('P_nbjoursrrversirrig',t%P_nbjoursrrversirrig)
!    call EnvoyerMsgHistorique('P_codedlaimin',t%P_codedlaimin)
!    call EnvoyerMsgHistorique('P_dlaimin(1)',t%P_dlaimin(1))
!    call EnvoyerMsgHistorique('P_dlaimin(2)',t%P_dlaimin(2))
    call EnvoyerMsgHistorique('P_resplmax',t%P_resplmax)
    call EnvoyerMsgHistorique('P_swfacmin',t%P_swfacmin)

    call EnvoyerMsgHistorique('P_codetranspitalle',t%P_codetranspitalle)

    call EnvoyerMsgHistorique('P_codedyntalle(1)',t%P_codedyntalle(1))
    call EnvoyerMsgHistorique('P_SurfApex(1)',t%P_SurfApex(1))
    call EnvoyerMsgHistorique('P_SeuilMorTalle(1)',t%P_SeuilMorTalle(1))
    call EnvoyerMsgHistorique('P_SigmaDisTalle(1)',t%P_SigmaDisTalle(1))
    call EnvoyerMsgHistorique('P_VitReconsPeupl(1)',t%P_VitReconsPeupl(1))
    call EnvoyerMsgHistorique('P_SeuilReconsPeupl(1)',t%P_SeuilReconsPeupl(1))
    call EnvoyerMsgHistorique('P_MaxTalle(1)',t%P_MaxTalle(1))

    call EnvoyerMsgHistorique('P_codedyntalle(2)',t%P_codedyntalle(2))
    call EnvoyerMsgHistorique('P_SurfApex(2)',t%P_SurfApex(2))
    call EnvoyerMsgHistorique('P_SeuilMorTalle(2)',t%P_SeuilMorTalle(2))
    call EnvoyerMsgHistorique('P_SigmaDisTalle(2)',t%P_SigmaDisTalle(2))
    call EnvoyerMsgHistorique('P_VitReconsPeupl(2)',t%P_VitReconsPeupl(2))
    call EnvoyerMsgHistorique('P_SeuilReconsPeupl(2)',t%P_SeuilReconsPeupl(2))
    call EnvoyerMsgHistorique('P_MaxTalle(2)',t%P_MaxTalle(2))

    call EnvoyerMsgHistorique('P_code_adapt_MO_CC',t%P_code_adapt_MO_CC)
    call EnvoyerMsgHistorique('P_periode_adapt_CC',t%P_periode_adapt_CC)
    call EnvoyerMsgHistorique('P_an_debut_serie_histo',t%P_an_debut_serie_histo)
    call EnvoyerMsgHistorique('P_an_fin_serie_histo',t%P_an_fin_serie_histo)
    call EnvoyerMsgHistorique('P_param_tmoy_histo',t%P_param_tmoy_histo)
    call EnvoyerMsgHistorique('P_code_adaptCC_miner',t%P_code_adaptCC_miner)
    call EnvoyerMsgHistorique('P_code_adaptCC_nit',t%P_code_adaptCC_nit)
    call EnvoyerMsgHistorique('P_code_adaptCC_denit',t%P_code_adaptCC_denit)
    call EnvoyerMsgHistorique('P_TREFdenit1',t%P_TREFdenit1)
    call EnvoyerMsgHistorique('P_TREFdenit2',t%P_TREFdenit2)
    call EnvoyerMsgHistorique('P_nbj_pr_apres_semis',t%P_nbj_pr_apres_semis)
    call EnvoyerMsgHistorique('P_eau_mini_decisemis',t%P_eau_mini_decisemis)
    call EnvoyerMsgHistorique('P_humirac_decisemis',t%P_humirac_decisemis)
! nouveaux param BM
    call EnvoyerMsgHistorique('P_eau_mini_decisemis',t%P_eau_mini_decisemis)
    call EnvoyerMsgHistorique('P_humirac_decisemis',t%P_humirac_decisemis)

return
end
 
 
