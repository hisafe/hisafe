!> write to the file history.sti
!!
!! general parameters
subroutine Ecriture_Parametres_Generaux(pg)

USE Parametres_Generaux
USE Messages

implicit none

    type(Parametres_Generaux_), intent(IN) :: pg  

    integer :: ires ! variables de boucle

! DR 21/03/2014 je mets à jour l'ecriture des parametres conformement à la demande de Francoise (749)
    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique(104)
    call EnvoyerMsgHistorique('*********************************************')
if (pg%P_codeinnact == 1) call EnvoyerMsgHistorique(5170, pg%P_codeinnact)
if (pg%P_codeh2oact == 1) call EnvoyerMsgHistorique(5171, pg%P_codeh2oact)
if (pg%P_codeminopt == 1) call EnvoyerMsgHistorique(5172, pg%P_codeminopt)
if (pg%P_iniprofil == 1) call EnvoyerMsgHistorique(5173, pg%P_iniprofil)
if (pg%P_codeprofmes == 1) call EnvoyerMsgHistorique(5174, pg%P_codeprofmes)
if (pg%P_codeprofmes == 2) call EnvoyerMsgHistorique(5175, pg%P_codeprofmes)
if (pg%P_codeinitprec == 1) call EnvoyerMsgHistorique(5176, pg%P_codeinitprec)
if (pg%P_codeinitprec == 2) call EnvoyerMsgHistorique(5177, pg%P_codeinitprec)
if (pg%P_codemsfinal == 2) call EnvoyerMsgHistorique(5178, pg%P_codemsfinal)
if (pg%P_codeactimulch == 1)then
    call EnvoyerMsgHistorique(5179, pg%P_codeactimulch)
endif
if (pg%P_codefrmur == 1) call EnvoyerMsgHistorique(5180, pg%P_codefrmur)
if (pg%P_codefrmur == 2) call EnvoyerMsgHistorique(5181, pg%P_codefrmur)
if (pg%P_codemicheur == 1) call EnvoyerMsgHistorique(5182, pg%P_codemicheur)
if (pg%P_codeoutscient == 1) call EnvoyerMsgHistorique(5183, pg%P_codeoutscient)
if (pg%P_codeseprapport == 1) call EnvoyerMsgHistorique(5184, pg%P_codeseprapport)
!!!MODIF HISAFE 1 : suppression des chaines de caractères
!!!if (pg%P_codeseprapport == 2) call EnvoyerMsgHistorique(5185, pg%P_separateurrapport)
! DR 21/03/2014 voir si on supprime pas ce code qui ne sert à rien en l'etat
if (pg%P_codesensibilite == 1) call EnvoyerMsgHistorique(5186, pg%P_codesensibilite)
if (pg%P_flagecriture == 1) call EnvoyerMsgHistorique('P_flagEcriture', pg%P_flagecriture)
if (pg%P_flagecriture .lt.1) then
  call EnvoyerMsgHistorique(5187, pg%P_flagecriture)
  elseif (pg%P_flagecriture .le.4) then
    call EnvoyerMsgHistorique(5188, pg%P_flagecriture)
    elseif (pg%P_flagecriture .le.8) then
      call EnvoyerMsgHistorique(5189, pg%P_flagecriture)
endif
    call EnvoyerMsgHistorique('P_parsurrg', pg%P_parsurrg)
    call EnvoyerMsgHistorique('P_coefb', pg% P_coefb)
    call EnvoyerMsgHistorique('P_proprac', pg% P_proprac)
    call EnvoyerMsgHistorique('P_y0msrac ', pg%P_y0msrac)
    call EnvoyerMsgHistorique('P_khaut', pg%P_khaut)
    call EnvoyerMsgHistorique('P_dacohes', pg%P_dacohes)
    call EnvoyerMsgHistorique('P_daseuilbas', pg%P_daseuilbas)
    call EnvoyerMsgHistorique('P_daseuilhaut', pg%P_daseuilhaut)
    call EnvoyerMsgHistorique('P_beta', pg% P_beta)
    call EnvoyerMsgHistorique('P_lvopt', pg% P_lvopt)
    call EnvoyerMsgHistorique('P_rayon', pg% P_rayon)
    call EnvoyerMsgHistorique('P_difN', pg% P_difN)
    call EnvoyerMsgHistorique('P_concrr', pg% P_concrr)
    call EnvoyerMsgHistorique('P_plNmin', pg% P_plNmin)
    call EnvoyerMsgHistorique('P_irrlev', pg% P_irrlev)
    call EnvoyerMsgHistorique('P_QNpltminINN', pg% P_QNpltminINN)
if (pg%P_codesymbiose == 1)call EnvoyerMsgHistorique('Nitrogen fixation by legumes = critical nitrogen', pg%P_codesymbiose)
if (pg%P_codesymbiose == 2)then
    call EnvoyerMsgHistorique('Nitrogen fixation by legumes = nodule activity', pg%P_codesymbiose)
    if (pg%P_codefxn == 1) call EnvoyerMsgHistorique(5190, pg%P_codefxn)
    if (pg%P_codefxn == 2) call EnvoyerMsgHistorique(5191, pg%P_codefxn)
    if (pg%P_codefxn == 3) call EnvoyerMsgHistorique(5192, pg%P_codefxn)
endif
    call EnvoyerMsgHistorique('P_FTEMh', pg%P_FTEMh)
    call EnvoyerMsgHistorique('P_FTEMha', pg%P_FTEMha)
    call EnvoyerMsgHistorique('P_TREFh', pg% P_TREFh)
    call EnvoyerMsgHistorique('P_FTEMr', pg%P_FTEMr)
    call EnvoyerMsgHistorique('P_FTEMra', pg%P_FTEMra)
    call EnvoyerMsgHistorique('P_TREFr', pg% P_TREFr)
    call EnvoyerMsgHistorique('P_finert', pg% P_finert)
    call EnvoyerMsgHistorique('P_fmin1', pg%P_fmin1)
    call EnvoyerMsgHistorique('P_fmin2', pg%P_fmin2)
    call EnvoyerMsgHistorique('P_fmin3', pg%P_fmin3)
    call EnvoyerMsgHistorique('P_Wh', pg%P_Wh)
    call EnvoyerMsgHistorique('P_pHminvol', pg%P_pHminvol)
    call EnvoyerMsgHistorique('P_pHmaxvol', pg%P_pHmaxvol)
    call EnvoyerMsgHistorique('P_Vabs2', pg% P_Vabs2)
    call EnvoyerMsgHistorique('P_Xorgmax', pg% P_Xorgmax)
    call EnvoyerMsgHistorique('P_hminm', pg%P_hminm)
    call EnvoyerMsgHistorique('P_hoptm', pg%P_hoptm)
    call EnvoyerMsgHistorique('P_hminn', pg%P_hminn)
    call EnvoyerMsgHistorique('P_hoptn', pg%P_hoptn)
    call EnvoyerMsgHistorique('P_fnx', pg%P_fnx)
    call EnvoyerMsgHistorique('P_pHminnit', pg%P_pHminnit)
    call EnvoyerMsgHistorique('P_pHmaxnit', pg%P_pHmaxnit)
    call EnvoyerMsgHistorique('P_tnitmin', pg%P_tnitmin)
    call EnvoyerMsgHistorique('P_tnitopt', pg%P_tnitopt)
    call EnvoyerMsgHistorique('P_tnitopt2', pg%P_tnitopt2)
    call EnvoyerMsgHistorique('P_tnitmax', pg%P_tnitmax)
    call EnvoyerMsgHistorique('P_rationit', pg%P_rationit)
    call EnvoyerMsgHistorique('P_ratiodenit', pg%P_ratiodenit)
    call EnvoyerMsgHistorique('P_alphapH', pg%P_alphapH)
    call EnvoyerMsgHistorique('P_dpHvolmax', pg%P_dpHvolmax)
    call EnvoyerMsgHistorique('P_pHvols', pg%P_pHvols)
    call EnvoyerMsgHistorique('P_fhminsat', pg%P_fhminsat)
    call EnvoyerMsgHistorique('P_fredkN', pg%P_fredkN)
    call EnvoyerMsgHistorique('P_fredlN', pg%P_fredlN)
    call EnvoyerMsgHistorique('P_fNCBiomin', pg%P_fNCBiomin)
    call EnvoyerMsgHistorique('P_fredNsup', pg%P_fredNsup)
    call EnvoyerMsgHistorique('P_Primingmax', pg%P_Primingmax)
    call EnvoyerMsgHistorique('P_pminruis', pg%P_pminruis)
    call EnvoyerMsgHistorique('P_diftherm', pg%P_diftherm)
    call EnvoyerMsgHistorique('P_Bformnappe', pg%P_Bformnappe)
    call EnvoyerMsgHistorique('P_rdrain', pg%P_rdrain)
    call EnvoyerMsgHistorique('P_psihumin', pg%P_psihumin)
    call EnvoyerMsgHistorique('P_psihucc', pg%P_psihucc)
    call EnvoyerMsgHistorique('P_prophumtasssem', pg%P_prophumtasssem)
    call EnvoyerMsgHistorique('P_prophumtassrec', pg%P_prophumtassrec)
if (pg%P_codhnappe == 1)call EnvoyerMsgHistorique(5193, pg%P_codhnappe)
if (pg%P_codhnappe == 2)then
    call EnvoyerMsgHistorique(5194, pg%P_distdrain)
    call EnvoyerMsgHistorique('P_distdrain', pg%P_distdrain)
endif
    call EnvoyerMsgHistorique('P_proflabour', pg%P_proflabour)
    call EnvoyerMsgHistorique('P_proftravmin', pg%P_proftravmin)


! TODO: ça dépend de code relatif au sol qui sont lus après les paramètres généraux.
!       Il faut donc déplacer l'écriture après l'ensemble des lectures ou bien déplacer
!       ces quelques lignes ailleurs.
!       A se demander si avoir une routine d'écriture dans l'historique par structure
!       avec des interdépendances est une bonne idée.
!       Peut être avoir une routine qui soit valable pour l'ensemble des structures.
!    if (pg%P_codecailloux == 1) then
!      call EnvoyerMsgHistorique('P_masvolcx', pg%P_masvolcx(P_typecailloux(1)))
!      call EnvoyerMsgHistorique('P_hcccx', pg%P_hcccx(P_typecailloux(1)))
!    endif

! *!* PB - 09/03/2004 - P_engrais, variable plante pour l'instant donc on utilise par defaut celui de la plante principale
! ** Domi 21/10/2004 P_engrais est lu dans lectech donc je depace l'ecriture dans lectech
!    call EnvoyerMsgHistorique('P_engamm', pg%P_engamm(P_engrais(1))
!    call EnvoyerMsgHistorique('P_orgeng', pg%P_orgeng(P_engrais(1))
!    call EnvoyerMsgHistorique('P_deneng', pg%P_deneng(P_engrais(1))
!    call EnvoyerMsgHistorique('P_voleng', pg%P_voleng(P_engrais(1))
  do ires=1, pg%nbresidus
    call EnvoyerMsgHistorique('P_kbio', pg%P_kbio(ires))
    call EnvoyerMsgHistorique('P_yres', pg% P_yres(ires))
    call EnvoyerMsgHistorique('P_CroCo', pg%P_CroCo(ires))
    call EnvoyerMsgHistorique('P_akres', pg%P_akres(ires))
    call EnvoyerMsgHistorique('P_bkres', pg%P_bkres(ires))
    call EnvoyerMsgHistorique('P_awb', pg%P_awb(ires))
    call EnvoyerMsgHistorique('P_bwb', pg%P_bwb(ires))
    call EnvoyerMsgHistorique('P_cwb', pg%P_cwb(ires))
    call EnvoyerMsgHistorique('P_ahres', pg%P_ahres(ires))
    call EnvoyerMsgHistorique('P_bhres', pg%P_bhres(ires))
    call EnvoyerMsgHistorique('P_CNresmin', pg%P_CNresmin(ires))
    call EnvoyerMsgHistorique('P_CNresmax', pg%P_CNresmax(ires))
    call EnvoyerMsgHistorique('P_qmulchruis0', pg%P_qmulchruis0(ires))
    call EnvoyerMsgHistorique('P_mouillabilmulch', pg%P_mouillabilmulch(ires))
    call EnvoyerMsgHistorique('P_kcouvmlch', pg%P_kcouvmlch(ires))
    call EnvoyerMsgHistorique('P_albedomulchresidus', pg%P_albedomulchresidus(ires))
  end do
! ** Domi 21/10/2004 pour l'instant on sait pas quel type de paillage on a
!    donc je les ecrirai dans lectech
!      if(P_codepaillage(1).eq.2) then
!      call EnvoyerMsgHistorique('decomposmulch', pg%decomposmulch
!      endif

return
end subroutine Ecriture_Parametres_Generaux
 
 
