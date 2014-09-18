FUNCTION z_mm_container_tracking.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL
*"     REFERENCE(I_BODY) LIKE  ZMMT0031 STRUCTURE  ZMMT0031
*"  EXPORTING
*"     REFERENCE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"----------------------------------------------------------------------

  DATA : lv_msg(255),
         lv_budat     LIKE sy-datum,
         wadat_ist_la LIKE sy-datum,
         l_ev_date(10),
         l_es_date(10),
         l_ev_time(4),
         l_es_time(4).
  DATA :  if_date  TYPE  string,
          ef_date  TYPE  string.
  DATA : lv_line TYPE i.  "03/08/2010 by Victor

  CLEAR : lv_msg.

  CLEAR : wa_option.
  m_cl  : bdcdata, gt_bdcmsg.

  l_ev_time = i_body-ev_time(4).
  l_es_time = i_body-es_time(4).


  write: i_body-ev_date to l_ev_date,
        i_body-es_date to l_es_date.
*  if_date  = i_body-ev_date.


*  CALL FUNCTION '/SAPDII/SPP05_CONVERT_DATE'
*    EXPORTING
*      if_date = if_date
*    IMPORTING
*      ef_date = ef_date.
*
*
*  l_ev_date  =  ef_date.
*
*
*  if_date  = i_body-es_date.
*
*
*  CALL FUNCTION '/SAPDII/SPP05_CONVERT_DATE'
*    EXPORTING
*      if_date = if_date
*    IMPORTING
*      ef_date = ef_date.
*
*
*  l_es_date  =  ef_date.


*  CALL FUNCTION 'CONVERT_DATE_FORMAT'
*    EXPORTING
*      i_date      = i_body-ev_date
*    IMPORTING
*      e_calc_date = l_ev_date.
*
*  CALL FUNCTION 'CONVERT_DATE_FORMAT'
*    EXPORTING
*      i_date      = i_body-es_date
*    IMPORTING
*      e_calc_date = l_es_date.


  wa_option-dismode = 'N'.
  wa_option-updmode = 'S'.
  wa_option-defsize = 'X'.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '0108'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIKP-VBELN'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UELA'.
  PERFORM bdc_field       USING 'LIKP-VBELN'
                                i_vbeln.
  PERFORM bdc_dynpro      USING 'SAPMV50A' '0270'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIPS-MATNR(13)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=LAKD'.
*-< 03/08/2010 by Victor  for  Reprocess
  IF i_body-message+0(8) = 'No batch'.
    PERFORM bdc_dynpro      USING 'SAPLSPO1' '0100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=YES'.
  ENDIF.
*->
  PERFORM bdc_dynpro      USING 'SAPMV50A' '0370'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  IF NOT i_body-route IS INITIAL.
    PERFORM bdc_field       USING 'LIKP-ROUTE'
                                  i_body-route.
  ENDIF.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'IDT_TBL_ESTLOC'.
  PERFORM bdc_field       USING 'IDT_TBL_EVTQUA'
                                i_body-ev_status.
  PERFORM bdc_field       USING 'IDT_TBL_EVTDAT'
                                l_ev_date.
  PERFORM bdc_field       USING 'IDT_TBL_EVTTIM'
                                l_ev_time.
  PERFORM bdc_field       USING 'IDT_TBL_EVTLOC'
                                i_body-ev_location.

  IF NOT i_body-es_location IS INITIAL.
    PERFORM bdc_field       USING 'IDT_TBL_ESTQUA'
                                  i_body-es_status.
    PERFORM bdc_field       USING 'IDT_TBL_ESTDAT'
                                  l_es_date.
    PERFORM bdc_field       USING 'IDT_TBL_ESTTIM'
                                  l_es_time.
    PERFORM bdc_field       USING 'IDT_TBL_ESTLOC'
                                  i_body-es_location.
  ENDIF.

  CALL TRANSACTION 'VL32' USING bdcdata OPTIONS FROM wa_option
                           MESSAGES INTO gt_bdcmsg.

  READ TABLE gt_bdcmsg WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.

    CLEAR lv_msg.
    PERFORM build_message USING lv_msg
                                gt_bdcmsg-msgid
                                gt_bdcmsg-msgnr
                                gt_bdcmsg-msgv1
                                gt_bdcmsg-msgv2
                                gt_bdcmsg-msgv3
                                gt_bdcmsg-msgv4.

    e_return-type    = 'E'.
    e_return-message = lv_msg.
  ELSE.
*-<  03/08/2010 by Victor
*-  popup window (data inconsistency)
    READ TABLE gt_bdcmsg WITH KEY msgv1 = 'SAPLSPO1'
                                  msgv2 = '0100'.
    IF sy-subrc = 0.
      CLEAR lv_msg.
      PERFORM build_message USING lv_msg
                                  gt_bdcmsg-msgid
                                  gt_bdcmsg-msgnr
                                  gt_bdcmsg-msgv1
                                  gt_bdcmsg-msgv2
                                  gt_bdcmsg-msgv3
                                  gt_bdcmsg-msgv4.

      e_return-type    = 'E'.
      e_return-message = lv_msg.

    ELSE.
      DESCRIBE TABLE gt_bdcmsg LINES lv_line.
*    READ TABLE gt_bdcmsg WITH KEY msgtyp = 'S'.
      READ TABLE gt_bdcmsg  INDEX lv_line.
*->


      CLEAR lv_msg.
      PERFORM build_message USING lv_msg
                                  gt_bdcmsg-msgid
                                  gt_bdcmsg-msgnr
                                  gt_bdcmsg-msgv1
                                  gt_bdcmsg-msgv2
                                  gt_bdcmsg-msgv3
                                  gt_bdcmsg-msgv4.

      e_return-type    = 'S'.
      e_return-message = lv_msg.
    ENDIF.
  ENDIF.

ENDFUNCTION.
