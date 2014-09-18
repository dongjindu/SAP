************************************************************************
* Program Name      : ZRPP_ENGINE_GR_CHECKL
* Creation Date     : 09/18/12
* Development Request No :
* Addl Documentation:
* Description       : Send ROH1 to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrpp_engine_gr_check NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.
TYPE-POOLS: slis, vrm.
TABLES: ztpperm.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztpperm.
DATA: END OF it_data.
DATA: it_error LIKE TABLE OF it_data WITH HEADER LINE.
*
*DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
*       it_fieldname    TYPE slis_t_fieldcat_alv,
*       it_sort         TYPE lvc_t_sort WITH HEADER LINE.
*
*DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
*       w_fieldname    LIKE LINE OF it_fieldname.  "IT_FIELDCAT.
*
*DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
*      wa_variant TYPE disvariant.      "for parameter IS_VARIANT
*
*DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
*      alv_grid          TYPE REF TO cl_gui_alv_grid,
*      grid_container    TYPE REF TO cl_gui_custom_container.
*
*DATA: ok_code LIKE sy-ucomm,
*      w_repid LIKE sy-repid,
*      w_cnt   TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_date LIKE ztpperm-zbdat DEFAULT sy-datum.
SELECT-OPTIONS: s_time FOR ztpperm-zbtim.
SELECTION-SCREEN SKIP.
PARAMETERS: p_rver  LIKE somlreci1-receiver OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
  ELSE.
    PERFORM send_email..
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_string(20).

  CONCATENATE 'GR' '%' INTO l_string.

  SELECT *
    INTO TABLE it_data
    FROM ztpperm
WHERE zresult = 'S'
 AND zbdat = p_date
 AND   zbtim  IN s_time
 AND   erpid IN ('E01', 'E02', 'E03', 'E05')
 AND   zmsg NOT LIKE l_string.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'GRs are not created for the following engines' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=============================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Engine Code'TO lt_body+0(15),
        'RP' TO lt_body+15(10),
        'Engine Assy ID'TO lt_body+25(25),
        'SAP BDC Date' TO lt_body+50(15),
        'SAP BDC Time' TO lt_body+65(15).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO  lt_body+0(20),
        '------------------------------' TO  lt_body+20(30),
        '------------------------------' TO  lt_body+50(30).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_data.
    MOVE: it_data-EITEM TO lt_body+0(15),
          it_data-ERPID TO lt_body+15(10),
          it_data-EASSYID TO lt_body+25(25),
          it_data-zbdat TO lt_body+50(15),
          it_data-zbtim TO lt_body+65(15).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Engines without GR created (backflush'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .
  s_time-sign = 'I'.
  s_time-option = 'BT'.
  s_time-low = sy-uzeit - 7200.
  if  s_time-low > sy-uzeit.
      clear:  s_time-low.
  endif.
  s_time-high = sy-uzeit.
  APPEND s_time.
ENDFORM.                    " INIT_DATA
