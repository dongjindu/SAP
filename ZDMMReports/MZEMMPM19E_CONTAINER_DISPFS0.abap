
*---------------------------------------------------------------------*
*       Modulpool ML01SFS0: Allgemeine Leseroutinen                   *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         LAGP_LESEN          Lesen eines LAGP Satzes                 *
*         LINV_LESEN          Lesen eines LINV-Satzes                 *
*         LQUA_LESEN          Lesen eines LQUA Satzes                 *
*         MAKT_LESEN          Lesen des Materialkurztextes            *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM LAGP_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines LAGP Satzes                                       *
*---------------------------------------------------------------------*
*       VALUE(PAR_LGNUM)               Lagernummer                    *
*       VALUE(PAR_LGTYP)               Lagertyp
*       VALUE(PAR_LGPLA)               Lagerplatz                     *
*---------------------------------------------------------------------*
FORM LAGP_LESEN USING VALUE(P_LGNUM)
                      VALUE(P_LGTYP)
                      VALUE(P_LGPLA).
    SELECT * FROM LAGP
         WHERE LGNUM = P_LGNUM
           AND LGTYP = P_LGTYP
           AND LGPLA = P_LGPLA.
    ENDSELECT.
    ENDFORM.


*---------------------------------------------------------------------*
*       FORM LINV_LESEN                                               *
*---------------------------------------------------------------------*
*       text                                                          *
*---------------------------------------------------------------------*
FORM LINV_LESEN USING LGNUM IVNUM IVPOS LQNUM INIT_NANUM.

    SELECT * INTO LINV FROM LINV
    WHERE LGNUM = LGNUM
      AND IVNUM = IVNUM
      AND IVPOS = IVPOS
      AND LQNUM = LQNUM
      AND NANUM = INIT_NANUM.
    ENDSELECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LQUA_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines LQUA Satzes                                       *
*---------------------------------------------------------------------*
*       VALUE(PAR_LGNUM)               Lagernummer                    *
*       VALUE(PAR_LQNUM)               Lagerquantnummer
*---------------------------------------------------------------------*
FORM LQUA_LESEN USING VALUE(P_LGNUM)
                      VALUE(P_LQNUM).
    SELECT * INTO LQUA FROM LQUA
      WHERE LGNUM = P_LGNUM
        AND LQNUM = P_LQNUM.
    ENDSELECT.
    ENDFORM.

