FUNCTION zfmmmrfk_sub_toheader.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_PKKEY) LIKE  PKPS-PKKEY
*"     VALUE(P_LGNUM) LIKE  LTAP-LGNUM
*"     VALUE(P_AUSFB) LIKE  PKHD-PRVBE
*"     VALUE(P_WERKS) LIKE  PKHD-WERKS
*"  EXPORTING
*"     VALUE(P_FLAG) TYPE  CHAR1
*"----------------------------------------------------------------------
  DATA: w_tanum_char(10) TYPE c,
          w_stim_char(8) TYPE c,
          w_etim_char(8) TYPE c,
          l_tbnum LIKE pkps-tbnum,
          w_nln TYPE i,
          l_datum like sy-datum,
          l_date_8(8).

  DATA: it_ztmm_kanban_to_update LIKE table of ztmm_kanban_to_u
                                            WITH HEADER LINE.
  CLEAR: w_lgnum,w_tanum.
  SELECT SINGLE tbnum INTO l_tbnum FROM pkps
          WHERE pkkey = p_pkkey.

  DO 5000 TIMES.
    SELECT SINGLE lgnum tanum INTO (w_lgnum,w_tanum)
                               FROM ltbp
                               WHERE tbnum = l_tbnum
                               AND   lgnum = c_p01.
    IF w_tanum > 1.
      EXIT.
    ELSE.
      WAIT UP TO '0.1' SECONDS.
    ENDIF.
  ENDDO.
  SELECT SINGLE stuzt enuzt FROM ztmm_kbn_time
                     INTO (w_sttime, w_edtime)
                     WHERE werks = p_werks
                     AND   prvbe = p_ausfb
                     AND   zzstuzt LE sy-uzeit
                     AND   zzenuzt GT sy-uzeit.
  IF sy-subrc NE 0.
    w_sttime = '000000'.
    w_edtime = '000000'.
  ENDIF.
  WRITE: w_tanum TO w_tanum_char,
         sy-datum TO w_stdate.

  IF w_sttime > '230000'.
    w_caldate = sy-datum + 1.
    WRITE w_caldate TO w_endate.
  ELSE.
    WRITE sy-datum TO w_endate.
  ENDIF.
*&------Convert to char for BDC.
  WRITE: w_sttime TO w_stim_char,
         w_edtime TO w_etim_char .

  PERFORM bdc_dynpro USING 'SAPML03T' '0126'.
  PERFORM bdc_field  USING 'LTAK-TANUM' w_tanum_char.
  PERFORM bdc_field  USING 'LTAK-LGNUM' w_lgnum.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/0'.
  PERFORM bdc_dynpro USING 'SAPML03T' '0103'.
  PERFORM bdc_field  USING 'LTAK-STDAT' w_stdate.
  PERFORM bdc_field  USING 'LTAK-STUZT' w_sttime.
  PERFORM bdc_field  USING 'LTAK-ENDAT' w_endate.
  PERFORM bdc_field  USING 'LTAK-ENUZT' w_edtime.
  PERFORM bdc_field  USING 'LTAK-AUSFB' p_ausfb.
  PERFORM bdc_field  USING 'BDC_OKCODE' 'BU'.
  PERFORM bdc_transaction TABLES it_mess
                          USING  'LT1A'
                                  'X'
                                  'N'
                                  'S'.
  DESCRIBE TABLE it_mess LINES w_nln.
  IF w_nln > 0.
    READ TABLE it_mess INTO wa_bdcmsgcoll INDEX 1.
    wa_msglog-msgid = wa_bdcmsgcoll-msgid.
    wa_msglog-msgno = wa_bdcmsgcoll-msgnr.
    wa_msglog-msgv1 = wa_bdcmsgcoll-msgv1.
    wa_msglog-msgv2 = wa_bdcmsgcoll-msgv2.
    wa_msglog-msgv3 = wa_bdcmsgcoll-msgv3.
    wa_msglog-msgv4 = wa_bdcmsgcoll-msgv4.

    CALL FUNCTION 'MESSAGE_TEXTS_READ'
         EXPORTING
              msg_log_imp  = wa_msglog
         IMPORTING
              msg_text_exp = wa_msgtxt.

    it_ztmm_kanban_to_update-PKkEY = p_pkkey.
    concatenate w_stdate+6(4) w_stdate+0(2) w_stdate+3(2) into l_date_8.
    l_datum = l_date_8.
    it_ztmm_kanban_to_update-stdat = l_datum.
    concatenate w_endate+6(4) w_endate+0(2) w_endate+3(2) into l_date_8.
    l_datum = l_date_8.
    it_ztmm_kanban_to_update-endat = l_datum.
    it_ztmm_kanban_to_update-stuzt = w_sttime.
    it_ztmm_kanban_to_update-enuzt = w_edtime.
    it_ztmm_kanban_to_update-MESSA = wa_msgtxt.
    append it_ztmm_kanban_to_update.
    modify ztmm_kanban_to_u from table it_ztmm_kanban_to_update.
    clear: it_ztmm_kanban_to_update.
** Populate to z
*    p_ret = 1.
*    p_msg = wa_msgtxt-msgtx.
  ENDIF.
  COMMIT WORK AND WAIT.

ENDFUNCTION.
