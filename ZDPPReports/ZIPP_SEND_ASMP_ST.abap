*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     :  ZIPP_SEND_ASMP_ST
*& Program Name   : Send B-Part Stock
*& Created by     : Victor Park
*& Created on     : 06.13.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------

REPORT  zipp_send_asmp_st NO STANDARD PAGE HEADING LINE-SIZE 132
                                             MESSAGE-ID zmpp.

TABLES : mard, pgmi, mara, marc, ztpp_asmpst, ztpp_asmpst_h.

DATA : BEGIN OF it_matnr OCCURS 0,
        matnr LIKE pbim-matnr,
        werks LIKE pbim-werks,
      END OF it_matnr.

DATA : BEGIN OF wa_mard ,
        werks LIKE mard-werks,
        matnr LIKE mard-matnr,
        labst LIKE mard-labst,
       END OF wa_mard.

DATA : it_bpart LIKE wa_mard OCCURS 0 WITH HEADER LINE.

DATA :  BEGIN OF it_data_tmp  OCCURS 0,
          werks LIKE zspp_asmpst-werks,
          matnr  LIKE zspp_asmpst-matnr,
          labst TYPE labst,
        END OF it_data_tmp.

DATA : it_data     LIKE zspp_asmpst OCCURS 0 WITH HEADER LINE.
DATA : it_asmpst   LIKE ztpp_asmpst OCCURS 0 WITH HEADER LINE,
       it_asmpst_h LIKE ztpp_asmpst_h OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_werks  FOR   marc-werks NO-EXTENSION
                                     OBLIGATORY MEMORY ID wrk.
SELECT-OPTIONS : s_prgrp  FOR   pgmi-prgrp . "NO-EXTENSION.

*-Delete
SELECT-OPTIONS : s_matnr FOR  mara-matnr.
SELECTION-SCREEN ULINE.
PARAMETERS : p_send AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM check_data.
  PERFORM select_data.
  PERFORM process_data.


*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data .
  DATA : v_dest(30) VALUE 'WMPP01'.   "Interface Destination.
  DATA : e_result TYPE  zresult,
         e_msg    TYPE  bapi_msg,
         l_msgtxt(200).

  CALL FUNCTION 'ZFPP_ASMP_ST' DESTINATION v_dest
    IMPORTING
      e_result              = e_result
      e_msg                 = e_msg
    TABLES
      t_asmp_st             = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt
      resource_failure      = 3
      OTHERS                = 4.

  IF e_result = 'S' AND sy-subrc = 0.
    PERFORM save_log  USING 'S' 'Success'    ''.
    PERFORM write_log .
  ELSE.
    PERFORM save_log  USING 'E' e_msg l_msgtxt.
    PERFORM write_log .
  ENDIF.


ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
FORM check_data .
  LOOP AT s_werks.
    IF s_werks-low+0(1) <> 'P'.
      MESSAGE s000 WITH 'Engine plant is not acceptable'.
      STOP.
    ENDIF.
  ENDLOOP.

  IF p_send IS INITIAL.
    MESSAGE s000 WITH 'Please click Send Data for MES I/F'.
    STOP.
  ENDIF.

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  CLEAR : it_data[], it_data.

*-Get Product group
  SELECT a~werks a~nrmit AS matnr
    INTO CORRESPONDING FIELDS OF TABLE it_matnr
  FROM pgmi AS a
  WHERE a~werks IN s_werks
    AND a~prgrp IN s_prgrp.

  CHECK it_matnr[] IS NOT INITIAL.

*-Get B-part
  SELECT  a~werks b~idnrk AS matnr
    INTO CORRESPONDING FIELDS OF TABLE it_bpart
  FROM mast AS a INNER JOIN stpo AS b
           ON a~stlnr = b~stlnr
    FOR ALL ENTRIES IN it_matnr
  WHERE a~matnr = it_matnr-matnr
    AND a~werks = it_matnr-werks
    AND a~stlan = '1'
*    AND A~STLAL =    "Alt BOM????
    AND b~idnrk IN s_matnr
    AND b~stlty = 'M'
    AND b~idnrk LIKE 'MV%'.

*-Get Stock
  SELECT a~werks a~matnr a~labst a~lgort
    INTO CORRESPONDING FIELDS OF wa_mard
  FROM mard AS a
    FOR ALL ENTRIES IN it_bpart
  WHERE a~matnr = it_bpart-matnr
    AND a~werks = it_bpart-werks.
    MOVE-CORRESPONDING wa_mard TO it_data_tmp.

    COLLECT it_data_tmp.
  ENDSELECT.

  SORT it_data_tmp BY werks matnr.

*  IF it_data_tmp[] IS INITIAL.
*    MESSAGE s000 WITH 'There is No data'.
*    STOP.
*  ENDIF.

  LOOP AT it_bpart.
    READ TABLE it_data_tmp WITH KEY werks = it_bpart-werks
                                    matnr = it_bpart-matnr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_bpart TO it_data.
      it_data-zsdat = sy-datum.
      it_data-zstim = sy-uzeit.
      it_data-labst = it_data_tmp-labst.
      APPEND it_data.
    ENDIF.

  ENDLOOP.

*  LOOP AT it_data_tmp.
*    MOVE-CORRESPONDING it_data_tmp to it_data.
*    it_data-zsdat = sy-datum.
*    it_data-zstim = sy-uzeit.
*
*    append it_data.
*  ENDLOOP.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'S_WERKS-HIGH' OR screen-name = 'S_PRGRP-HIGH'.
      screen-invisible = 1.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
FORM write_log .
  DATA : l_labst TYPE i.
  WRITE:/ '--------------------------------'.
  WRITE:/        'Interface Result'.
  WRITE:/ '--------------------------------'.
  SKIP.

  WRITE AT : 1(10) 'Plant', 10(20) 'Material No', 30(15) 'quantity', 50(10)'Result', 60(100) 'Message text'.
  ULINE.

  LOOP AT it_data.
    l_labst = it_data-labst.
    WRITE AT : /1(10) it_data-werks, 10(20) it_data-matnr, 30(15) l_labst,
               50(10) it_data-zresult, 60(100) it_data-zmsg.
  ENDLOOP.
ENDFORM.                    " WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log   USING    p_type p_msg1 p_msg2.
  DATA : l_datum TYPE sy-datum.

  CLEAR : it_asmpst[], it_asmpst_h[], it_asmpst, it_asmpst_h.

  l_datum = sy-datum - 90.
*  DELETE FROM ztpp_asmpst.
  DELETE FROM ztpp_asmpst_h WHERE zedat < l_datum.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data   TO it_asmpst.
    it_asmpst-zedat = it_asmpst-zsdat.
    it_asmpst-zetim = it_asmpst-zstim.
    it_asmpst-zuser = sy-uname.
    it_asmpst-zresult = it_data-zresult = p_type.
    IF p_msg1 IS INITIAL.
      it_asmpst-zmsg = it_data-zmsg = p_msg2.
    ELSE.
      it_asmpst-zmsg = it_data-zmsg = p_msg1.
    ENDIF.

    MOVE-CORRESPONDING it_asmpst TO it_asmpst_h.

*    it_asmpst_h-zsdat = it_asmpst_h-zedat = it_asmpst-zsdat = sy-datum.
*    it_asmpst_h-zstim = it_asmpst_h-zetim = it_asmpst-zstim = sy-uzeit.

    APPEND it_asmpst.
    APPEND it_asmpst_h.
    MODIFY it_data.
  ENDLOOP.

  MODIFY ztpp_asmpst FROM TABLE it_asmpst.
  INSERT ztpp_asmpst_h FROM TABLE it_asmpst_h.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init.
  CLEAR : s_prgrp[], s_prgrp.

  SELECT matnr INTO   s_prgrp-low
    FROM mara
   WHERE mtart = 'PROD'
     AND matnr LIKE '%PNL'
     and MATNR not LIKE 'MV%'.

    s_prgrp-sign = 'I'.
    s_prgrp-option = 'EQ'.
    APPEND s_prgrp.
  ENDSELECT.
ENDFORM.                    " INIT
