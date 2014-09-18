************************************************************************
* Program name : ZEMMGM01E_BOM_REG                                     *
* Created by   : Min-su Park                                           *
* Created on   : 2003.11.10.                                           *
* Pattern      :                                                       *
* Description  : BOM Registration Request Program                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.10.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZEMMGM01E_BOM_REGF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  REQUEST_BOM_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM request_bom_chk.
  DATA : w_matnr LIKE mara-matnr.
*Material check
  SELECT SINGLE matnr
           INTO w_matnr
           FROM mara
          WHERE matnr = it_end_part-request_part.
  IF sy-subrc <> 0.
    MESSAGE e018.
  ENDIF.

*Vendor check
  SELECT SINGLE name1
           INTO it_end_part-name1
           FROM lfa1
          WHERE lifnr = it_end_part-lifnr.
  IF sy-subrc <> 0.
    MESSAGE e019.
  ENDIF.
ENDFORM.                    " REQUEST_BOM_CHK
*&---------------------------------------------------------------------*
*&      Form  PAGE_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_control.
  CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
            entry_act             = tc_end_part-top_line
            entry_to              = tc_end_part-lines
            last_page_full        = ' '
            loops                 = w_loopc
            ok_code               = w_fcode
            overlapping           = 'X'
       IMPORTING
            entry_new             = tc_end_part-top_line
       EXCEPTIONS
            no_entry_or_page_act  = 1
            no_entry_to           = 2
            no_ok_code_or_page_go = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " PAGE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.
  CASE w_sel.
    WHEN r1. "Request BOM Save
      PERFORM request_bom_save.
    WHEN r3. "Manage Sub Part save
      PERFORM manage_sub_save.
  ENDCASE.
ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  REQUEST_BOM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM request_bom_save.
  DATA : w_line TYPE i.
  DESCRIBE TABLE it_end_part LINES w_line.
  CHECK w_line <> 0.
  LOOP AT it_end_part.
    CLEAR wa_end_part.
    MOVE-CORRESPONDING it_end_part TO wa_end_part.
    wa_end_part-request_date = sy-datum.
    UPDATE ztmm_end_part FROM wa_end_part.
    IF sy-subrc <> 0.
      INSERT ztmm_end_part FROM wa_end_part.
    ENDIF.
  ENDLOOP.
  MESSAGE s020.
ENDFORM.                    " REQUEST_BOM_SAVE
*&---------------------------------------------------------------------*
*&      Form  GET_DISPLAY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_display_status.
  SELECT * FROM ztmm_end_part
           INTO CORRESPONDING FIELDS OF TABLE it_end_part
          WHERE lifnr IN s_lifnr
            AND request_date IN s_date
            AND return_code  IN s_code.
  IF sy-subrc <> 0.
    MESSAGE e015.
  ENDIF.
ENDFORM.                    " GET_DISPLAY_STATUS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.
  CASE w_sel.
    WHEN r2. PERFORM get_display_status.
    WHEN r3. PERFORM get_sub_part.
  ENDCASE.
ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  GET_SUB_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_part.
  TABLES : t159l.
  DATA : w_mstae LIKE mara-mstae,
         w_disgr LIKE marc-disgr.
  DATA : tmp_end_part LIKE ztmm_end_part OCCURS 0 WITH HEADER LINE,
            tmp_stpox LIKE TABLE OF stpox WITH HEADER LINE        ,
            emeng     TYPE weemg VALUE 10                         .

* Get END_PART data
  CLEAR : it_end_part, it_end_part[].
  SELECT * FROM ztmm_end_part
           INTO CORRESPONDING FIELDS OF TABLE tmp_end_part
          WHERE lifnr IN s_lifnr
            AND request_date IN s_date.
  IF sy-subrc <> 0.
    MESSAGE e015.
  ENDIF.
* Get SUB_PART data
  LOOP AT tmp_end_part.
    CLEAR it_end_part.
    MOVE-CORRESPONDING tmp_end_part TO it_end_part.
    SELECT SINGLE *
           FROM t159l
          WHERE werks = 'P001'.
*BOM Explosion
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              aumng                 = 0
              capid                 = t159l-capid
              datuv                 = sy-datum
              emeng                 = emeng
              mktls                 = 'X'
              mehrs                 = 'X'
              mtnrv                 = tmp_end_part-request_part
              sanfr                 = 'X'
              stlan                 = t159l-stlan
              werks                 = 'P001'
         TABLES
              stb                   = tmp_stpox
         EXCEPTIONS
              alt_not_found         = 1
              call_invalid          = 2
              material_not_found    = 3
              missing_authorization = 4
              no_bom_found          = 5
              no_plant_data         = 6
              no_suitable_bom_found = 7
              conversion_error      = 8
              OTHERS                = 9.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    LOOP AT tmp_stpox.
*Material group and MRP group check
      IF tmp_stpox-matmk = 'AM'.
        CONTINUE.
      ENDIF.
*      SELECT SINGLE DISGR
*               INTO W_DISGR
*               FROM MARC
*              WHERE MATNR = TMP_STPOX-IDNRK
*                AND WERKS = TMP_STPOX-WERKS.
*      IF SY-SUBRC = 0 AND W_DISGR = 'P010'.
*         CONTINUE.
*      ENDIF.
*Get Sub part
      it_end_part-sub_part = tmp_stpox-idnrk.
*Desicion steel field.
      SELECT SINGLE mstae
               INTO w_mstae
               FROM mara
              WHERE matnr = tmp_stpox-idnrk.
      IF sy-subrc = 0 AND w_mstae = 'SB'.
        it_end_part-steel = 'X'.
      ENDIF.
      APPEND it_end_part.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_SUB_PART
*&---------------------------------------------------------------------*
*&      Form  MANAGE_SUB_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM manage_sub_save.
  DATA: wa_head       LIKE bapimathead, "Header with control information
        wa_clientdata LIKE bapi_mara  , "Client-specific material data
       wa_clientdatax LIKE bapi_marax , "Information on update for
                                        "CLIENTDATA
          it_bapiret2 LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_end_part WHERE steel = 'X'.
    CLEAR : it_bapiret2, it_bapiret2[].
    wa_head-material          = it_end_part-sub_part.
    wa_clientdata-pur_status  = 'SB'.
    wa_clientdatax-pur_status = 'X'.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = wa_head
              clientdata     = wa_clientdata
              clientdatax    = wa_clientdatax
         TABLES
              returnmessages = it_bapiret2.

    READ TABLE it_bapiret2 WITH KEY type = 'E'.
    IF sy-subrc = 0.  "Error Occurred !
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =    .
    ENDIF.

  ENDLOOP.
ENDFORM.                    " MANAGE_SUB_SAVE
