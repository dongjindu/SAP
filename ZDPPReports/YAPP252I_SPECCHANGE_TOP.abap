*----------------------------------------------------------------------*
*   INCLUDE YAPP252L_SPECCHANGE_TOP                                    *
*----------------------------------------------------------------------*

CONTROLS: tc_APP252 TYPE TABLEVIEW USING SCREEN 2201.

TABLES: ztpp_change,  "Spec & Line Back Change
        ztpp_veh_model.  "Vehicle Model master

DATA: BEGIN OF it_app252 OCCURS 0.
        INCLUDE STRUCTURE ztpp_change.
DATA: orderno(20).
DATA: END OF it_app252.

DATA: BEGIN OF IT_EXCEL_2201 OCCURS 0,
        col01(20),  "Serial
        col02(20),  "Body_NO
        col03(20),  "Current RP
        col04(20),  "VIN
        col05(20),  "ORDER_NO(ORDNO + NATION + DEALER)
        col06(20),  "Ext.Color
        col07(20),  "Int.Color
        col08(20),  "Spec
        col09(20),  "OCN
        col10(20),  "Date
        col11(20),  "Bef.OrdNo
        col12(20),  "Bef.Nation
        col13(20),  "Bef.Dealer
        col14(20),  "Bef.Ext.Color
        col15(20),  "Bef.Ext.Color
        col16(20),  "Bef.Spec
        col17(20),  "Bef.OCN
        col18(20),  "Bef.VIN
      END OF IT_EXCEL_2201.

TYPE-POOLS: vrm.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

* Parameters
DATA: p_company(10),  "Company
      p_model(10),  "Model
      p_cdate_st TYPE sy-datum,  "From_date
      p_cdate_en TYPE sy-datum,  "To_date
      p_yyyymm TYPE s021-spmon.  "Center_YearMonth

* DROPDOWN LIST for Parameter
* P_MODEL(Model)
DATA: name        TYPE vrm_id,
      model_list  TYPE vrm_values,
      model_value LIKE LINE OF model_list.
RANGES: r_model FOR ztpp_veh_model-model.

DATA  WA_INIT_2201.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting PF STATUS
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_2201 OUTPUT.
  IF WA_INIT_2201 IS INITIAL.
    WA_INIT_2201 = 'X'.

**Setting Parameters
*   Company Code
    p_company = 'HMMA'.
*   Model(Car Type)
    CLEAR: model_list, model_value.
    name = 'P_MODEL'.
    PERFORM set_field_model.
    PERFORM call_function_vrm USING model_list.
*   Date
    p_cdate_st = sy-datum.
    p_cdate_en = sy-datum.
*   yyyymm
    p_yyyymm = sy-datum+00(06).

  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       Calling a Function To Make a Dropdown list box
*----------------------------------------------------------------------*
*      -->P_B_RP_LIST  text
*----------------------------------------------------------------------*
FORM call_function_vrm USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_screen_2201 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table With table Control's Current Line
*----------------------------------------------------------------------*
MODULE modify_data_2201 INPUT.
  MODIFY it_app252 INDEX tc_app252-current_line.
ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_2201  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE user_command_2201 INPUT.
  CASE ok_code.
    WHEN 'SEL'.
      PERFORM select_data_2201.
      CLEAR ok_code.
    WHEN 'EXC'.
      PERFORM process_download_2201.
      CLEAR ok_code.
    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM sort_ascending_2201.
      CLEAR ok_code.

    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM sort_descending_2201.
      CLEAR ok_code.
  ENDCASE.
ENDMODULE.                 " user_command_2201  INPUT
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       The Process of Data Selection
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_2201.
  PERFORM set_parameters_2201.
  PERFORM make_data_2201.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
*       Setting Parameters For Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameters_2201.
* Car Type(Model)
  IF p_model <> space.
    CLEAR r_model.
    REFRESH r_model.
    r_model-option = 'EQ'.
    r_model-sign = 'I'.
    r_model-low = p_model.
    APPEND r_model.
  ELSE.
    CLEAR r_model.
    REFRESH r_model.
  ENDIF.

ENDFORM.                    " SET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Making Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data_2201.

* Model(MODEL) = bodyno+0(03).
  DATA: l_model(04).
  CONCATENATE p_model '%' INTO l_model.

******************************************************
* IT should be clear ---> Check Where condition.
******************************************************
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_app252
    FROM ztpp_change
    WHERE cdate BETWEEN p_cdate_st and
          p_cdate_en and
          BODYNO like L_MODEL.
  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH 'No Data'.
  ENDIF.
* Order Number = ordno + nation + dealer.
  LOOP AT it_app252.
    CONCATENATE it_app252-ordno
                it_app252-nation
                it_app252-dealer
                INTO it_app252-orderno.
    MODIFY it_app252.
  ENDLOOP.

* Body_No, C.Date, Serial.
  SORT it_app252 BY bodyno cdate serial.

ENDFORM.                    " make_data
*&---------------------------------------------------------------------*
*&      Form  process_download
*&---------------------------------------------------------------------*
*       The Process of Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_download_2201.
  PERFORM set_excel_data_2201.
  PERFORM call_function_for_excel_2201.
ENDFORM.                    " process_download
*&---------------------------------------------------------------------*
*&      Form  SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_excel_data_2201.
*
  CLEAR IT_EXCEL_2201.
  REFRESH IT_EXCEL_2201.
  MOVE 'Serial' TO IT_EXCEL_2201-col01.
  MOVE 'BODY_NO' TO IT_EXCEL_2201-col02.
  MOVE 'Current RP' TO IT_EXCEL_2201-col03.
  MOVE 'VIN' TO IT_EXCEL_2201-col04.
  MOVE 'ORDER_NO' TO IT_EXCEL_2201-col05.
  MOVE 'OUT COLOR' TO IT_EXCEL_2201-col06.
  MOVE 'IN COLOR' TO IT_EXCEL_2201-col07.
  MOVE 'SPEC' TO IT_EXCEL_2201-col08.
  MOVE 'OCN' TO IT_EXCEL_2201-col09.
  MOVE 'Date(CDATE)' TO IT_EXCEL_2201-col10.
  MOVE 'Bef.OrdNo' TO IT_EXCEL_2201-col11.
  MOVE 'Bef.Nation' TO IT_EXCEL_2201-col12.
  MOVE 'Bef.Dealer' TO IT_EXCEL_2201-col13.
  MOVE 'Bef.Ext.C' TO IT_EXCEL_2201-col14.
  MOVE 'Bef.Int.C' TO IT_EXCEL_2201-col15.
  MOVE 'Bef.Spec' TO IT_EXCEL_2201-col16.
  MOVE 'Bef.OCN' TO IT_EXCEL_2201-col17.
  MOVE 'Bef.VIN' TO IT_EXCEL_2201-col18.
  APPEND IT_EXCEL_2201.
*
  LOOP AT it_app252.
    CLEAR IT_EXCEL_2201.
    MOVE it_app252-serial TO IT_EXCEL_2201-col01.
    MOVE it_app252-bodyno TO IT_EXCEL_2201-col02.
    MOVE it_app252-crp TO IT_EXCEL_2201-col03.
    MOVE it_app252-vin TO IT_EXCEL_2201-col04.
    MOVE it_app252-orderno TO IT_EXCEL_2201-col05.
    MOVE it_app252-extc TO IT_EXCEL_2201-col06.
    MOVE it_app252-intc TO IT_EXCEL_2201-col07.
    MOVE it_app252-mi TO IT_EXCEL_2201-col08.
    MOVE it_app252-ocn TO IT_EXCEL_2201-col09.
    MOVE it_app252-cdate TO IT_EXCEL_2201-col10.
    MOVE it_app252-b_ordno TO IT_EXCEL_2201-col11.
    MOVE it_app252-b_nation TO IT_EXCEL_2201-col12.
    MOVE it_app252-b_dealer TO IT_EXCEL_2201-col13.
    MOVE it_app252-b_extc TO IT_EXCEL_2201-col14.
    MOVE it_app252-b_intc TO IT_EXCEL_2201-col15.
    MOVE it_app252-b_mi TO IT_EXCEL_2201-col16.
    MOVE it_app252-b_ocn TO IT_EXCEL_2201-col17.
    MOVE it_app252-b_vin TO IT_EXCEL_2201-col18.
    APPEND IT_EXCEL_2201.
  ENDLOOP.
*
ENDFORM.                    " SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function_for_excel_2201.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'LINE_BACK.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       TABLES
            data_tab                = IT_EXCEL_2201
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's Total line number
*----------------------------------------------------------------------*
MODULE modify_lines_2201 OUTPUT.
  DATA: l_line TYPE i.
  DESCRIBE TABLE it_app252 LINES l_line.
  tc_APP252-lines = l_line.
ENDMODULE.                 " MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_field3
*&---------------------------------------------------------------------*
*       Setting Data To Make A Dropdown List Box - MODEL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_model.
  SELECT DISTINCT model name
    INTO (model_value-key , model_value-text)
    FROM ztpp_veh_model.
    APPEND model_value TO model_list.

  ENDSELECT.

ENDFORM.                    " set_field3
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending_2201.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP252'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app252 ASCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending_2201.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP252'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app252 DESCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " SORT_DESCENDING
