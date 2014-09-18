*----------------------------------------------------------------------*
*   INCLUDE YTEST_KGH01_TOP01                                          *
*----------------------------------------------------------------------*

controls: tc_app250 type tableview using screen 2200.

tables: ztpp_change,  "Spec & Line Back Change
        ztpp_process,  "Vehicle plant Process
        ztpp_veh_model.  "Vehicle Model master

data: begin of it_app250 occurs 0.
        include structure ztpp_change.
data: orderno(20).
data: end of it_app250.

data: begin of it_excel_2200 occurs 0,
        col01(20),
        col02(20),
        col03(20),
        col04(20),
        col05(20),
        col06(20),
        col07(20),
        col08(20),
        col09(20),
        col10(20),
        col11(20),
      end of it_excel_2200.

type-pools: vrm.

data: ok_code type sy-ucomm,
      save_ok type sy-ucomm.

data: wa_company(10),
      wa_cartype(10),
      wa_b_rp type ztpp_change-b_rp,
      wa_cdate_st type sy-datum,
      wa_cdate_en type sy-datum,
      wa_bodyno type ztpp_change-bodyno.

* DROPDOWN LIST
* WA_B_RP
data: name        type vrm_id,
      b_rp_list   type vrm_values,
      b_rp_value  like line of b_rp_list.
ranges: r_b_rp for ztpp_change-b_rp.
* WA_BODYNO
data: bodyno_list type vrm_values,
      bodyno_value like line of bodyno_list.
ranges: r_bodyno for ztpp_change-bodyno.
* WA_CARTYPE(Model)
data: cartype_list type vrm_values,
      cartype_value like line of cartype_list.
ranges: r_cartype for ztpp_veh_model-model.

data  wa_init_2200.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting PF-STATUS
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS100'.
*  SET TITLEBAR 'xxx'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
module exit input.
  leave program.
endmodule.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
module initialization_2200 output.
  if wa_init_2200 is initial.
    wa_init_2200 = 'X'.
*
*   Company Code
    wa_company = 'HMMA'.
*   Model(Car Type)
    clear: cartype_list, cartype_value.
    name = 'WA_CARTYPE'.
    perform set_field_cartype.
    perform call_function_vrm using cartype_list.
*   Before RP
    clear : b_rp_list, b_rp_value.
    name = 'WA_B_RP'.
    perform set_field_brp.
    perform call_function_vrm using b_rp_list.
*   Date
    wa_cdate_st = sy-datum.
    wa_cdate_en = sy-datum.
*   BODY NO
    clear : bodyno_list, bodyno_value.
    name = 'WA_BODYNO'.
    perform set_field_bodyno.
    perform call_function_vrm using bodyno_list.
*
  endif.
endmodule.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Searching Data To Set a dropdown list box - Bef.Progress
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_brp.
  select distinct status progress
    into (b_rp_value-key , b_rp_value-text)
    from ztpp_process
    where vmrp <> ''.
    append b_rp_value to b_rp_list.

  endselect.

endform.                    " set_field
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       Calling a Function For making a dropdown list box
*----------------------------------------------------------------------*
*      -->P_B_RP_LIST  text
*----------------------------------------------------------------------*
form call_function_vrm using    p_list.
  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = p_list.

endform.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD2
*&---------------------------------------------------------------------*
*       Getting Data For a parameter - BODYNO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_bodyno.
  select distinct bodyno
    into bodyno_value-key
    from ztpp_change.
*   BODYNO_VALUE-TEXT
    append bodyno_value to bodyno_list.

  endselect.

endform.                    " SET_FIELD2
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
module modify_screen_2200 output.
  modify screen.
endmodule.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table with Table Control's Currnet line
*----------------------------------------------------------------------*
module modify_data_2200 input.
  modify it_app250 index tc_app250-current_line.
endmodule.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_2200  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
module user_command_2200 input.
  case ok_code.
    when 'SEL'.
      perform select_data_2200.
      clear ok_code.
    when 'EXC'.
      perform process_download_2200.
      clear ok_code.
    when 'SORTA'. "SORTING ASCENDING.
      perform sort_ascending_2200.
      clear ok_code.

    when 'SORTD'. "SORTING DESCENDING.
      perform sort_descending_2200.
      clear ok_code.
  endcase.
endmodule.                 " user_command_2200  INPUT
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       The Process of Data Selection
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data_2200.
  perform set_parameters_2200.
  perform make_data_2200.

endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
*       Setting Parameters For Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_parameters_2200.
* Car Type(Model)
  if wa_cartype <> space.
    clear r_cartype.
    refresh r_cartype.
    r_cartype-option = 'EQ'.
    r_cartype-sign = 'I'.
    r_cartype-low = wa_cartype.
    append r_cartype.
  else.
    clear r_cartype.
    refresh r_cartype.
  endif.

* Before Process
  if wa_b_rp <> space.
    clear   r_b_rp.
    refresh r_b_rp.
    r_b_rp-option = 'EQ'.
    r_b_rp-sign   = 'I'.
    r_b_rp-low    = wa_b_rp.
    append r_b_rp.
  else.
    clear  r_b_rp.
    refresh r_b_rp.
  endif.

* Body Number
  if wa_bodyno <> space.
    clear   r_bodyno.
    refresh r_bodyno.
    r_bodyno-option = 'EQ'.
    r_bodyno-sign   = 'I'.
    r_bodyno-low    = wa_bodyno.
    append r_bodyno.
  else.
    clear  r_bodyno.
    refresh r_bodyno.
  endif.

endform.                    " SET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_2200.

* Model(CarType) = bodyno+0(03).
  data: l_cartype(04).
  concatenate wa_cartype '%' into l_cartype.

  select *
    into corresponding fields of table it_app250
    from ztpp_change
    where b_rp in r_b_rp and
          cdate between wa_cdate_st and
          wa_cdate_en and
          bodyno in r_bodyno and
          bodyno like l_cartype.
  if sy-subrc <> 0.
    message s000 with 'No Data'.
  endif.

* Order Number = ordno + nation + dealer.
  loop at it_app250.
    concatenate it_app250-ordno
                it_app250-nation
                it_app250-dealer
                into it_app250-orderno.
    modify it_app250.
  endloop.

* Cdate, ShopDate, BodyNo, OrderNo.
  sort it_app250 by cdate b_rp_shopdat bodyno orderno.

endform.                    " make_data
*&---------------------------------------------------------------------*
*&      Form  process_download
*&---------------------------------------------------------------------*
*       The Process of download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form process_download_2200.
  perform set_excel_data_2200.
  perform call_function_for_excel_2200.
endform.                    " process_download
*&---------------------------------------------------------------------*
*&      Form  SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_excel_data_2200.
*
  clear it_excel_2200.
  refresh it_excel_2200.
  move 'DATE' to it_excel_2200-col01.
  move 'SERIAL' to it_excel_2200-col02.
  move 'Body No' to it_excel_2200-col03.
  move 'Order No' to it_excel_2200-col04.
  move 'Spec' to it_excel_2200-col05.
  move 'OUT COLOR' to it_excel_2200-col06.
  move 'IN COLOR' to it_excel_2200-col07.
  move 'BEFORE RP' to it_excel_2200-col08.
  move 'Current RP' to it_excel_2200-col09.
  move 'Act. Date' to it_excel_2200-col10.
  append it_excel_2200.
*
  loop at it_app250.
    clear it_excel_2200.
    move it_app250-cdate to it_excel_2200-col01.
    move it_app250-serial to it_excel_2200-col02.
    move it_app250-bodyno to it_excel_2200-col03.
    move it_app250-orderno to it_excel_2200-col04.
    move it_app250-mi to it_excel_2200-col05.
    move it_app250-extc to it_excel_2200-col06.
    move it_app250-intc to it_excel_2200-col07.
    move it_app250-b_rp to it_excel_2200-col08.
    move it_app250-crp to it_excel_2200-col09.
    move it_app250-b_rp_actdat to it_excel_2200-col10.
    append it_excel_2200.
  endloop.
*
endform.                    " SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_function_for_excel_2200.
  call function 'DOWNLOAD'
       exporting
            filename                = 'LINE_BACK.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       tables
            data_tab                = it_excel_2200
       exceptions
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            others                  = 7.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's Lines
*----------------------------------------------------------------------*
module modify_lines_2200 output.
  data: l_line type i.
  describe table it_app250 lines l_line.
  tc_app250-lines = l_line.
endmodule.                 " MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_field3
*&---------------------------------------------------------------------*
*       Getting Data To Make a Dropdown list box - MODEL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_cartype.
  select distinct model name
    into (cartype_value-key , cartype_value-text)
    from ztpp_veh_model.
    append cartype_value to cartype_list.

  endselect.

endform.                    " set_field3
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_ascending_2200.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(09) = 'IT_APP250'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app250 ascending by (field_name01).
  endif.
*
endform.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_descending_2200.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(09) = 'IT_APP250'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app250 descending by (field_name01).
  endif.
*
endform.                    " SORT_DESCENDING
