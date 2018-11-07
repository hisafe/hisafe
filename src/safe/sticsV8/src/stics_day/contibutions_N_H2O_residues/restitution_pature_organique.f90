subroutine restitution_pature_organique(n,CNplante,  msrec_fou,perte, coderes_pature, Crespc_pature, Nminres_pature, &
                                        eaures_pature,coef_calcul_doseN,  &
                                        qres, CsurNres, coderes, Crespc, Nminres, eaures)

! DR 30/03/2016 on implemente la prise en compte de l'urée des vaches comme un apport d'engrais mineral
    implicit none


integer , intent(IN)  :: n, coderes_pature
real    , intent(IN)  :: msrec_fou, CNplante
real    , intent(IN)  :: perte, coef_calcul_doseN,  eaures_pature, Nminres_pature, Crespc_pature
real    , intent(OUT) :: qres, CsurNres, Crespc, Nminres, eaures
integer , intent(OUT) :: coderes


!perte=0.2
!coderes=3
!Crespc=7.4
!Nminres=0.045
!eaures=87.0

coderes=coderes_pature
Crespc=Crespc_pature
Nminres=Nminres_pature
eaures=eaures_pature

CsurNres = (-80.847*6.25*(CNplante/100))+32.201
qres = 1/(1-eaures/100)*100/2.87*coef_calcul_doseN/1000*msrec_fou
qres = Qres*(1-perte)


end subroutine restitution_pature_organique
