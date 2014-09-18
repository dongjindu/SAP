FUNCTION z_mm_unloading_ct_pack_gi.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL OPTIONAL
*"     REFERENCE(I_BUDAT) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0034
*"----------------------------------------------------------------------

  DATA : lv_msg(255),
         lv_budat(10),
         wadat_ist_la LIKE sy-datum.
  DATA :  if_date  TYPE  string,
          ef_date  TYPE  string.

  CHECK not i_vbeln IS INITIAL.
  CLEAR : lv_msg.

  CLEAR : wa_option.
  m_cl  : bdcdata, gt_bdcmsg.


  wa_option-dismode = 'N'.
  wa_option-updmode = 'S'.
  wa_option-defsize = 'X'.


  if_date  = i_budat.


CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
 EXPORTING
   DATE_INTERNAL                  = if_date
 IMPORTING
   DATE_EXTERNAL                  = ef_date
 EXCEPTIONS
   DATE_INTERNAL_IS_INVALID       = 1
   OTHERS                         = 2
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

*  CALL FUNCTION '/SAPDII/SPP05_CONVERT_DATE'
*    EXPORTING
*      if_date = if_date
*    IMPORTING
*      ef_date = ef_date.


  lv_budat  =  ef_date.

*  CALL FUNCTION 'CONVERT_DATE_FORMAT'
*    EXPORTING
*      i_date      = i_budat
*    IMPORTING
*      e_calc_date = lv_budat.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '4004'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIKP-VBELN'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LIKP-VBELN'
                                i_vbeln.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VERP_T'.
  PERFORM bdc_field       USING 'LIKP-WADAT_IST'
                                lv_budat.


  PERFORM bdc_dynpro      USING 'SAPLV51G' '6000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UE6VDIR'.

  LOOP AT it_body.
    PERFORM bdc_dynpro      USING 'SAPLV51G' '6000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM bdc_field       USING 'VEKP-EXIDV'
                                  it_body-bktxt.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPLV51G' '6000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BACK'.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH_T'.

  CALL TRANSACTION 'VL02N' USING bdcdata OPTIONS FROM wa_option
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
    READ TABLE gt_bdcmsg WITH KEY msgtyp = 'S'.

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

ENDFUNCTION.
