FUNCTION ZFMMMRFK_KBN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_BARCO) TYPE  CHAR11
*"     VALUE(P_MENGE) TYPE  CHAR13 OPTIONAL
*"     VALUE(P_UNAME) LIKE  LTAK-BNAME OPTIONAL
*"  EXPORTING
*"     VALUE(P_PKKEY) TYPE  CHAR10
*"     VALUE(P_PKBST) LIKE  PKPS-PKBST
*"     VALUE(P_RFSTA) TYPE  CHAR1
*"     VALUE(P_SEMSG) LIKE  T100-TEXT
*"----------------------------------------------------------------------

*&--------------------------------------------------------------------&*
*&  Date         User    Transport       Description
*&  10/14/2005   Shiva   UD1K917958      performance tunning.
*&--------------------------------------------------------------------&*

*&-----Convert from CHAR->NUMC.
  W_BARCO = P_BARCO.

** added by Furong
  W_STRLEN = STRLEN( P_BARCO ).
  IF W_STRLEN < 11.
    P_RFSTA = 'E'.
    P_SEMSG = TEXT-008.
    EXIT.
  ENDIF.
** end of addition

  WRITE: W_BARCO LEFT-JUSTIFIED  TO W_PKKEY,
         W_BARCO RIGHT-JUSTIFIED TO W_PKBST.

  CASE W_PKBST.
    WHEN 2.
*&---Empty.
      PERFORM CHANGE_KBN_STATUS USING P_BARCO P_MENGE
                                CHANGING W_RET W_MSG.
      IF W_RET NE 0.
      ELSE.
        PERFORM READ_CHECK_CONTROLCYCLE USING    W_PKKEY
                                        CHANGING W_RET W_MSG.
      ENDIF.
      IF W_RET NE 0.
      ELSE.
** changed  by Furong on 11/08/05 create new function module for update
*   .
*        CALL FUNCTION 'ZFMMMRFK_SUB_TOHEADER'
*        STARTING NEW TASK wa_taskname
*        DESTINATION IN GROUP 'PG_STK'
*        EXPORTING
*            p_pkkey       = w_pkkey
*            p_lgnum       = w_lgnum
*            p_ausfb       = wa_pkps_pkhd-prvbe
*            p_werks       = wa_pkps_pkhd-werks
*            .
** end of change
      ENDIF.
    WHEN 4.
*&---In transit.
      PERFORM CHANGE_KBN_STATUS USING P_BARCO P_MENGE
                                CHANGING W_RET W_MSG.
    WHEN 5.
*&----Full.
      IF P_MENGE IS INITIAL.
        P_PKKEY = W_PKKEY.
        P_PKBST = W_PKBST.
        P_RFSTA = 'E'.
        P_SEMSG = TEXT-006.
        EXIT.
      ELSE.
        PERFORM READ_CHECK_CONTROLCYCLE USING W_PKKEY
                                        CHANGING W_RET W_MSG.
        IF W_RET EQ 0.
          PERFORM CHANGE_KBN_STATUS USING P_BARCO P_MENGE
                                CHANGING W_RET W_MSG.

          "Determine if actual quantity differs from kanban quantity
          W_DIFF = WA_PKPS_PKHD-BEHMG - P_menge.
*S__COMMENT : CHANGE PERFORM TO FUNCTION..
          IF W_DIFF > 0.
            CALL FUNCTION 'MB_CHANGE_RESERVATION'
              EXPORTING
                CHANGE_RESB       = 'X'
                CHANGE_RKPF       = ''
                NEW_RESB          = ''
             TABLES
               XRESB             = ZRESB
                      .
          ENDIF.
*          CASE W_DIFF.
*            WHEN 0.
*            WHEN OTHERS.
*              IF W_DIFF > 0.
*                PERFORM STOCK_TRAnSFER CHANGING w_RET w_MSG.
*              ENDIF.
*          ENDCASE.
*E__<
        ENDIF.
      ENDIF.
  ENDCASE.

*&-----RF result parameters.
  P_PKKEY = W_PKKEY.
  P_PKBST = W_PKBST.
  IF W_RET NE 0.
    P_RFSTA = 'E'.
  ELSE.
    P_RFSTA = 'S'.
  ENDIF.
  P_SEMSG = W_MSG.

  REFRESH: IT_LTAP_CONF, IT_MESS.
  CLEAR: WA_PKPS_PKHD, WA_TRTO_INFO, WA_BDCMSGCOLL, W_RET, W_MSG,
         W_LGNUM,W_TANUM.

ENDFUNCTION.
