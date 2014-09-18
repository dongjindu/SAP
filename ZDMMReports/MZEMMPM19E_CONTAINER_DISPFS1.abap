
*---------------------------------------------------------------------*
*       Modulpool ML01SFS1: Sperren/Entsperren zu SAPML01S            *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         LAGP_ENTSPERREN         Entsperren eines LAGP Satzes        *
*         LAGP_ENTSPERREN_NEU     Entsperren eines LAGP Satzes neu    *
*         LAGP_SPERREN            Sperren eines LAGP Satzes           *
*         LAGP_SPERREN_NEU        Sperren eines LAGP Satzes neu       *
*         LQUA_ENTSPERREN         Entsperren eines LLHM Satzes        *
*         LQUA_SPERREN            Sperren eines LLHM Satzes           *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM LAGP_ENTSPERREN                                          *
*---------------------------------------------------------------------*
*       Entsperen LAGP                                                *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_LGTYP                        Lagertyp                       *
*       P_LGPLA                        Lagerplatz                     *
*---------------------------------------------------------------------*
FORM LAGP_ENTSPERREN USING P_LGNUM
                           P_LGTYP
                           P_LGPLA.

    CALL FUNCTION   'DEQUEUE_ELLAGPE'
         EXPORTING  LGNUM = P_LGNUM
                    LGTYP = P_LGTYP
                    LGPLA = P_LGPLA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_ENTSPERREN_NEU                                      *
*---------------------------------------------------------------------*
*       Entsperren LAGP nach neuer Sperrmimik                         *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_LGTYP                        Lagertyp                       *
*       P_LGPLA                        Lagerplatz                     *
*---------------------------------------------------------------------*
FORM LAGP_ENTSPERREN_NEU USING P_LGNUM
                               P_LGTYP
                               P_LGPLA.
      CALL FUNCTION 'L_BIN_LOCATION_DEQUEUE'
        EXPORTING  I_LGNUM = P_LGNUM
                   I_LGTYP = P_LGTYP
                   I_LGPLA = P_LGPLA
                   I_LENUM = INIT_LENUM.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_SPERREN                                             *
*---------------------------------------------------------------------*
*       Sperren LAGP                                                  *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_LGTYP                        Lagertyp                       *
*       P_LGPLA                        Lagerplatz                     *
*---------------------------------------------------------------------*
FORM LAGP_SPERREN USING P_LGNUM
                        P_LGTYP
                        P_LGPLA.
DATA:  USER LIKE SY-UNAME.
    CALL FUNCTION   'ENQUEUE_ELLAGPE'
         EXPORTING  LGNUM = P_LGNUM
                    LGTYP = P_LGTYP
                    LGPLA = P_LGPLA
         EXCEPTIONS FOREIGN_LOCK   = 1
                    SYSTEM_FAILURE = 2.

    CASE SY-SUBRC.
      WHEN 1.  USER = SY-MSGV1.
               MESSAGE E593 WITH P_LGPLA USER.   "blocked by user ...
      WHEN 2.  MESSAGE E595.                "...Systemfehler
    ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_SPERREN_NEU                                         *
*---------------------------------------------------------------------*
*       Sperren LAGP nach neuer Sperrmimik                            *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_LGTYP                        Lagertyp                       *
*       P_LGPLA                        Lagerplatz                     *
*---------------------------------------------------------------------*
FORM LAGP_SPERREN_NEU USING P_LGNUM
                            P_LGTYP
                            P_LGPLA.
DATA:  USER LIKE SY-UNAME.
    CALL FUNCTION 'L_BIN_LOCATION_ENQUEUE'
         EXPORTING
              I_ENQUE        = T340D-ENQUE
              I_LGNUM        = P_LGNUM
              I_LGTYP        = P_LGTYP
              I_LGPLA        = P_LGPLA
              I_LENUM        = INIT_LENUM
         EXCEPTIONS
              FOREIGN_LOCK   = 01
              SYSTEM_FAILURE = 02.

    CASE SY-SUBRC.
      WHEN 1.  USER = SY-MSGV2.
               MESSAGE E593 WITH P_LGPLA USER.   "blocked by user ...
      WHEN 2.  MESSAGE E595.                "...Systemfehler
    ENDCASE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM LQUA_ENTSPERREN                                          *
*---------------------------------------------------------------------*
*       Entsperen LQUA                                                *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_MATNR                        Materialnummer                 *
*---------------------------------------------------------------------*
FORM LQUA_ENTSPERREN USING P_LGNUM
                           P_MATNR.
    CALL FUNCTION   'DEQUEUE_ELLQUAE'
         EXPORTING  LGNUM = P_LGNUM
                    MATNR = P_MATNR.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LQUA_SPERREN                                             *
*---------------------------------------------------------------------*
*       Sperren LQUA                                                  *
*---------------------------------------------------------------------*
*       P_LGNUM                        Lagernummer                    *
*       P_MATNR                        Materialnummer                 *
*---------------------------------------------------------------------*
FORM LQUA_SPERREN USING P_LGNUM
                        P_MATNR.
    CALL FUNCTION   'ENQUEUE_ELLQUAE'
         EXPORTING  LGNUM = P_LGNUM
                    MATNR = P_MATNR
         EXCEPTIONS FOREIGN_LOCK   = 1
                    SYSTEM_FAILURE = 2.

    CASE SY-SUBRC.
      WHEN 1.  MESSAGE E594.                   "...durch anderen Benutz.
      WHEN 2.  MESSAGE E595.                   "...Systemfehler
    ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*     Form  t30at_lesen
*--------------------------------------------------------------------*
*     Lesen eines T30AT-Eintrages
*---------------------------------------------------------------------*
FORM T30AT_LESEN USING VALUE(009_LGNUM)
                       VALUE(009_LGTYP)
                       VALUE(009_KOBER).
SELECT SINGLE * FROM T30AT
WHERE LGNUM = 009_LGNUM AND
      LGTYP = 009_LGTYP AND
      KOBER = 009_KOBER AND
      SPRAS = SY-LANGU.
ENDFORM.                    " T30AT_LESEN
*&---------------------------------------------------------------------*
*&      Form  SR001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SR001.

  if linv-ivnum is initial.
    screen-input = con_off.
  endif.

ENDFORM.                    " SR001
*&---------------------------------------------------------------------*
*&      Form  SR002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SR002.

  if lqua-lqnum is initial.
    screen-input = con_off.
  endif.

ENDFORM.                    " SR002
*&---------------------------------------------------------------------*
*&      Form  SR003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SR003.

  if bin_is_checked is initial and
     ( t340-trtyp eq con_veraendern or
       t340-trtyp eq con_hinzufuegen ).
    screen-input = con_off.
  endif.

ENDFORM.                    " SR003
*&---------------------------------------------------------------------*
*&      Form  SR004
*&---------------------------------------------------------------------*
*       140499
*----------------------------------------------------------------------*
FORM SR004.

  if linv-ivnum is initial.
    screen-input = con_off.
    screen-invisible = con_on.
  endif.

ENDFORM.                    " SR004
*&---------------------------------------------------------------------*
*&      Form  SR005
*&---------------------------------------------------------------------*
*       140499
*----------------------------------------------------------------------*
FORM SR005.

  if lqua-lqnum is initial.
    screen-input = con_off.
    screen-invisible = con_on.                "140499
  endif.

ENDFORM.                    " SR005
