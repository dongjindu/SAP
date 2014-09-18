*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     :  ZIPP_SEND_ASMP_PP
*& Program Name   : Send AS/MP Production Plan
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

REPORT  zipp_send_asmp_pp NO STANDARD PAGE HEADING LINE-SIZE 132

                                             MESSAGE-ID zmpp.

TABLES : mara, marc, ztpp_asmppp, ztpp_asmppp_h.

*DATA : BEGIN OF it_pir OCCURS 0,
*        matnr LIKE pbim-matnr,
*        werks LIKE pbim-werks,
*        pdatu LIKE pbed-pdatu,
*        plnmg LIKE pbed-plnmg,
*      END OF it_pir.

DATA : it_data     LIKE zspp_asmppp OCCURS 0 WITH HEADER LINE.
DATA : it_asmppp   LIKE ztpp_asmppp OCCURS 0 WITH HEADER LINE,
       it_asmppp_h LIKE ztpp_asmppp_h OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_date OCCURS 0,
        pdatu LIKE zspp_asmppp-pdatu,
       END OF it_date.

DATA : l_wdatu LIKE ztpp_asmp_pir-wdatu.

*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_werks  FOR   marc-werks NO-EXTENSION
                                 OBLIGATORY MEMORY ID wrk.
*SELECT-OPTIONS : s_matnr FOR  mara-matnr.
SELECT-OPTIONS : s_pdatu FOR ztpp_asmppp-pdatu
                              NO-EXTENSION MODIF ID m01.

SELECTION-SCREEN ULINE.
PARAMETERS : p_send AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM pro_init.

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

  CALL FUNCTION 'ZFPP_ASMP_PP' DESTINATION v_dest
    EXPORTING
      I_SDATE               = l_wdatu
    IMPORTING
      e_result              = e_result
      e_msg                 = e_msg
    TABLES
      t_asmp_pp             = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt
      resource_failure      = 3
      OTHERS                = 4.

  IF e_result = 'S' AND sy-subrc = 0.
    PERFORM save_log  USING 'S' 'Success'    ''.
    PERFORM write_log.
  ELSE.
    PERFORM save_log  USING 'E' e_msg l_msgtxt.
    PERFORM write_log.
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

*  IF p_psttr < sy-datum.
*    MESSAGE s000 WITH 'Past plan date can Not be accepted'.
*    STOP.
*  ENDIF.

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

  SELECT a~werks a~bpart AS matnr  a~pdatu SUM( a~plnmg ) AS plnmg
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM ztpp_asmp_pir AS a
  WHERE a~werks IN s_werks
    AND a~pdatu IN s_pdatu
    AND a~wdatu = l_wdatu
  GROUP BY a~werks a~bpart  a~pdatu.

  DELETE it_data WHERE plnmg = 0.

  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.

*  SELECT a~werks a~matnr b~pdatu  SUM( b~plnmg ) AS plnmg
*    INTO CORRESPONDING FIELDS OF TABLE it_pir
*  FROM pbim AS a INNER JOIN pbed AS b
*            ON a~bdzei  = b~bdzei
*
*  WHERE a~matnr IN s_matnr
*    AND a~matnr LIKE 'MV%'
*    AND a~werks IN s_werks
*    AND a~versb = 'AS'
*    AND pdatu = p_psttr
*  GROUP BY a~werks a~matnr b~pdatu .
*
*  IF it_pir[] IS INITIAL.
*    MESSAGE s000 WITH 'There is No data'.
*    STOP.
*  ENDIF.
*
*  LOOP AT it_pir.
*    MOVE-CORRESPONDING it_pir TO it_data.
**    it_data-zuser = sy-uname.
**    it_data-zedat = sy-datum.
**    it_data-zetim = sy-uzeit.
*
*    APPEND it_data.
*  ENDLOOP.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'S_WERKS-HIGH'.
      screen-invisible = 1.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'M01'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
FORM write_log.
  sort it_data by pdatu.

  WRITE:/ '--------------------------------'.
  WRITE:/        'Interface Result'.
  WRITE:/ '--------------------------------'.
  SKIP.

  WRITE AT : 1(10) 'Plant', 10(10) 'Plan date', 25(20) 'Material No',
          50(10) 'quantity', 60(10) 'Result', 70(100) 'Message text'.
  ULINE.

  LOOP AT it_data.
    WRITE AT :/1(10) it_data-werks, 10(10) it_data-pdatu,
              25(20) it_data-matnr, 50(10) it_data-plnmg,
              60(10) it_data-zresult, 70(100) it_data-zmsg.
  ENDLOOP.
ENDFORM.                    " WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log   USING    p_type p_msg1 p_msg2.
  DATA : l_datum TYPE sy-datum.

  CLEAR : it_asmppp[], it_asmppp_h[], it_asmppp, it_asmppp_h.

  l_datum = sy-datum - 90.
  DELETE FROM ztpp_asmppp WHERE werks IN s_werks
                            AND pdatu IN s_pdatu.

  DELETE FROM ztpp_asmppp_h WHERE zedat < l_datum.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data   TO it_asmppp.
    it_asmppp-zedat = it_asmppp-zsdat = sy-datum.
    it_asmppp-zetim = it_asmppp-zstim = sy-uzeit.
    it_asmppp-zuser = sy-uname.
    it_asmppp-zresult = it_data-zresult = p_type.
    IF p_msg1 IS INITIAL.
      it_asmppp-zmsg = it_data-zmsg = p_msg2.
    ELSE.
      it_asmppp-zmsg = it_data-zmsg = p_msg1.
    ENDIF.

    MOVE-CORRESPONDING it_asmppp TO it_asmppp_h.

*    it_asmppp_h-zsdat = it_asmppp_h-zedat = it_asmppp-zsdat = sy-datum.
*    it_asmppp_h-zstim = it_asmppp_h-zetim = it_asmppp-zstim = sy-uzeit.

    APPEND it_asmppp.
    APPEND it_asmppp_h.
    MODIFY it_data.
  ENDLOOP.

  INSERT ztpp_asmppp FROM TABLE it_asmppp.
  INSERT ztpp_asmppp_h FROM TABLE it_asmppp_h.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  PRO_INIT
*&---------------------------------------------------------------------*
FORM pro_init .
  DATA : l_lines TYPE i.

  CLEAR : s_pdatu[], s_pdatu.

  SELECT MAX( wdatu ) INTO l_wdatu
  FROM ztpp_asmp_pir.

  SELECT DISTINCT pdatu INTO TABLE it_date
  FROM ztpp_asmp_pir
  WHERE werks IN s_werks
    AND wdatu = l_wdatu.

  SORT it_date BY pdatu.
  DESCRIBE TABLE it_date LINES l_lines.
  READ TABLE it_date INDEX l_lines.

  s_pdatu-low  = sy-datum.
  s_pdatu-high = it_date-pdatu.
  s_pdatu-sign = 'I'.
  s_pdatu-option = 'BT'.
  APPEND s_pdatu.

ENDFORM.                    " PRO_INIT
