************************************************************************
* Program Name      : ZRPP902R_VRLOG_ARCHIVE
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.12.08.
* Specifications By : B. Choi
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Data backup from ZTPPVR to ZTPPVR_HIS.
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zrpp902r_vrlog_archive  MESSAGE-ID zmpp  LINE-SIZE 120.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztppvr    ,  " Vehicle Interface Log..
        ztppvr_his.  " Vehicle Interface History LOG.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: it_ztppvr              LIKE TABLE OF ztppvr     WITH HEADER LINE.


*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_error           TYPE c,
      wa_date            TYPE d.


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_run       TYPE c   DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM set_duration   .
  PERFORM read_ztppvr    .
  PERFORM delete_ztppvr  .
  PERFORM move_to_hist   .


*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM display_data.

*----------------------------------------------------------------------
AT LINE-SELECTION.
*----------------------------------------------------------------------


*----------------------------------------------------------------------
TOP-OF-PAGE DURING LINE-SELECTION  .
*----------------------------------------------------------------------


*----------------------------------------------------------------------
TOP-OF-PAGE.
*----------------------------------------------------------------------
  PERFORM top_of_page.


*&---------------------------------------------------------------------*
*&      Form  READ_ZTPPVR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ztppvr    .
  DATA: lw_ztppvr            LIKE ztppvr.

  SELECT SINGLE * INTO lw_ztppvr
    FROM ztppvr
   WHERE k04pdat < wa_date
     AND zresult NE 'S'    .

  IF sy-subrc = 0.
    MESSAGE e002 WITH text-001  ':'  lw_ztppvr .
    STOP.
  ENDIF.

  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_ztppvr
    FROM ztppvr
   WHERE k04pdat < wa_date  .
ENDFORM.                    " READ_ZTPPVR

*&---------------------------------------------------------------------*
*&      Form  DELETE_ZTPPVR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_ztppvr  .
  DELETE FROM ztppvr  WHERE k04pdat < wa_date .
  IF sy-subrc NE 0.   wa_error = 'X'.   ENDIF.
ENDFORM.                    " DELETE_ZTPPVR

*&---------------------------------------------------------------------*
*&      Form  Top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
    WRITE AT: /001(04)  'Flag'           ,
               005(01)  sy-vline         ,
               006(16)  'ALC-Date / Ser.',
               022(01)  sy-vline         ,
               023(06)  'Status'         ,
               029(01)  sy-vline         ,
               030(10)  'Model No.'      ,
               040(01)  sy-vline         ,
               041(14)  'Work Order'     ,
               055(01)  sy-vline         ,
               056(05)  'Dest.'          ,
               061(01)  sy-vline         ,
               062(03)  'Ext'            ,
               065(01)  sy-vline         ,
               066(03)  'Int'            ,
               069(01)  sy-vline         ,
               070(18)  'VIN Code'       ,
               088(01)  sy-vline         ,
               089(06)  'R-Code'         ,
               095(01)  sy-vline         ,
               096(23)  'Message'        ,
               119(01)  sy-vline         .
    ULINE.
ENDFORM.                    " Top_of_page

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_color.

  IF wa_error = 'X'.
    " Rollback
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    " Commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ENDIF.

  LOOP AT it_ztppvr.
    IF l_color <> 'X'.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      l_color = 'X'.
    ELSE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED ON.
      l_color = ' '.
    ENDIF.
    WRITE AT: /001(04)  it_ztppvr-flag,
               005(01)  sy-vline      ,
               006(10)  it_ztppvr-k04pdat,
               016(01)  '/'           ,
               017(05)  it_ztppvr-k04ser ,
               022(01)  sy-vline      ,
               023(06)  it_ztppvr-p_status,
               029(01)  sy-vline      ,
               030(03)  it_ztppvr-p_model ,
               033(06)  it_ztppvr-p_body_serial,
               040(01)  sy-vline      ,
               041(14)  it_ztppvr-p_work_order,
               055(01)  sy-vline      ,
               056(05)  it_ztppvr-p_dest_code,
               061(01)  sy-vline      ,
               062(03)  it_ztppvr-p_ext_color,
               065(01)  sy-vline      ,
               066(03)  it_ztppvr-p_int_color,
               069(01)  sy-vline      ,
               070(18)  it_ztppvr-p_vin      ,
               088(01)  sy-vline      ,
               089(06)  it_ztppvr-zresult    ,
               095(01)  sy-vline      ,
               096(23)  it_ztppvr-zresult    ,
               119(01)  sy-vline      .
    ULINE.
  ENDLOOP.
ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  SET_DURATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_duration.
  DATA: l_kalid             LIKE kako-kalid,
        l_date              TYPE d.

  l_date = sy-datum .
  PERFORM read_shop_calid   USING  l_kalid.
  DO 3 TIMES.
    l_date = l_date - 1 .
    PERFORM read_fin_date   USING '-'  l_kalid  l_date.
  ENDDO.
  wa_date = l_date      .
ENDFORM.                    " SET_DURATION

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE fabkl INTO pa_kalid
    FROM t001w
   WHERE werks = 'P001'.
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  read_FIN_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2407   text
*      -->P_L_KALID  text
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM read_fin_date USING    pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " read_FIN_date

*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_HIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_to_hist.
  CHECK wa_error = space .
  MODIFY ztppvr_his FROM TABLE it_ztppvr.
  IF sy-subrc NE 0.   wa_error = 'X'.   ENDIF.
ENDFORM.                    " MOVE_TO_HIST
