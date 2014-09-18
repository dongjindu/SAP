FUNCTION zfmmmrfk.
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
  w_barco = p_barco.

** added by Furong
  w_strlen = strlen( p_barco ).
  IF w_strlen < 11.
    p_rfsta = 'E'.
    p_semsg = text-008.
    EXIT.
  ENDIF.
** end of addition

  WRITE: w_barco LEFT-JUSTIFIED TO w_pkkey,
         w_barco RIGHT-JUSTIFIED TO w_pkbst.

  CASE w_pkbst.
    WHEN 2.
*&---Empty.
      PERFORM change_kbn_status USING p_barco P_MENGE
                                CHANGING w_ret w_msg.
      IF w_ret NE 0.
      ELSE.
        PERFORM read_check_controlcycle USING    w_pkkey
                                        CHANGING w_ret w_msg.
      ENDIF.
      IF w_ret NE 0.
      ELSE.
** changed  by Furong on 11/08/05 create new function module for update
*   .
*      perform change_to_header using w_tanum w_lgnum wa_pkps_pkhd-prvbe
*                                         changing w_ret w_msg.
        CALL FUNCTION 'ZFMMMRFK_SUB_TOHEADER'
        STARTING NEW TASK wa_taskname
        DESTINATION IN GROUP 'PG_STK'
        EXPORTING
            p_pkkey       = w_pkkey
            p_lgnum       = w_lgnum
            p_ausfb       = wa_pkps_pkhd-prvbe
            p_werks       = wa_pkps_pkhd-werks
*         IMPORTING
*           P_FLAG        =
            .
** end of change
      ENDIF.
    WHEN 4.
*&---In transit.
      PERFORM change_kbn_status USING p_barco P_MENGE
                                CHANGING w_ret w_msg.
    WHEN 5.
*&----Full.
      IF p_menge IS INITIAL.
        p_pkkey = w_pkkey.
        p_pkbst = w_pkbst.
        p_rfsta = 'E'.
        p_semsg = text-006.
        EXIT.
      ELSE.
        PERFORM read_check_controlcycle USING w_pkkey
                                        CHANGING w_ret w_msg.
        IF w_ret NE 0.
        ELSE.
          PERFORM confirm_tr_to USING  p_menge p_uname p_barco
                                CHANGING w_ret w_msg.
        ENDIF.
      ENDIF.
  ENDCASE.

*&-----RF result parameters.
  p_pkkey = w_pkkey.
  p_pkbst = w_pkbst.
  IF w_ret NE 0.
    p_rfsta = 'E'.
  ELSE.
    p_rfsta = 'S'.
  ENDIF.
  p_semsg = w_msg.

  REFRESH: it_ltap_conf, it_mess.
  CLEAR: wa_pkps_pkhd, wa_trto_info, wa_bdcmsgcoll, w_ret, w_msg,
         w_lgnum,w_tanum.

ENDFUNCTION.
