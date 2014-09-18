*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_dropdown_list_box_app245.
* Plant
  clear: plant_list, PLANT_LIST[],
         plant_value.
  name = 'P_PLANT_APP245'.
  perform set_field_plant_app245.
  perform call_function_vrm_app245 using plant_list.
* Model
  clear: model_list, MODEL_LIST[],
         model_value.
  name = 'P_MODEL_APP245'.
  perform set_field_model_app245.
  perform call_function_vrm_app245 using model_list.
* Line
  clear: line_list, LINE_LIST[],
         line_value.
  name = 'P_LINE_APP245'.
  perform set_field_line_app245.
  perform call_function_vrm_app245 using line_list.
* Progress
  clear: progress_list, PROGRESS_LIST[],
         progress_value.
  name = 'P_PROG_APP245'.
  perform set_field_prog_app245.
  perform call_function_vrm_app245 using progress_list.
* Color O or X
  clear: color_list, COLOR_LIST[],
         color_value.
  name = 'P_COLOR_APP245'.
  perform set_field_color_app245.
  perform call_function_vrm_app245 using color_list.
* Ending Date
  clear: endate_list, ENDATE_LIST[],
         endate_value.
  name = 'P_END_DATE_APP245'.
  perform set_field_endate_app245.
  perform call_function_vrm_app245 using endate_list.
* Summary Type
  CLEAR: TYPE_LIST, TYPE_LIST[],
         TYPE_VALUE.
  name = 'P_TYPE_APP245'.
  perform set_field_type_app245.
  perform call_function_vrm_app245 using type_list.
* Work Order
  clear: wono_list, WONO_LIST[],
         wono_value.
  name = 'P_WONO_APP245'.
  perform set_field_wono_app245.
  perform call_function_vrm_app245 using wono_list.
* External Color
  clear: extc_list, EXTC_LIST[],
         extc_value.
  name = 'P_EXTC_APP245'.
  perform set_field_extc_app245.
  perform call_function_vrm_app245 using extc_list.
* Internal Color
  clear: intc_list, INTC_LIST[],
         intc_value.
  name = 'P_INTC_APP245'.
  perform set_field_intc_app245.
  perform call_function_vrm_app245 using intc_list.
* Columnes
  perform set_columns_APP245 .

endform.                    " make_dropdown_list_box_APP245
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANT_LIST  text
*----------------------------------------------------------------------*
form call_function_vrm_app245 using    p_list.
  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = p_list.

endform.                    " call_function_vrm_APP245
*&---------------------------------------------------------------------*
*&      Form  set_field_plant_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_plant_app245.
  select distinct au~atwrt
    into plant_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_TRIM_PLANT_NO' .
    concatenate plant_value-key ' ''s Plant'
      into plant_value-text .
    append plant_value to plant_list.

  endselect.

endform.                    " set_field_plant_APP245
*&---------------------------------------------------------------------*
*&      Form  set_field_model_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_model_app245.
  select distinct au~atwrt
    into model_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_MODEL_APP245' .
    append model_value to model_list.

  endselect.

endform.                    " set_field_model_APP245
*&---------------------------------------------------------------------*
*&      Form  set_field_line_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_line_app245.
  select distinct au~atwrt
    into line_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_TRIM_LINE_NO' .
    append line_value to line_list.
  endselect.

endform.                    " set_field_line_APP245
*&---------------------------------------------------------------------*
*&      Form  set_field_PROG_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_prog_app245.
* For Testing
  progress_value-key = '26'.
  progress_value-text = 'Test-26'.
  append progress_value to progress_list.

  progress_value-key = '00'.
  progress_value-text = 'Test-00'.
  append progress_value to progress_list.

* B/IN(01)
  progress_value-key  = '01'.
  progress_value-text = 'B/IN'.
  append progress_value to progress_list.
* P/IN(02)
  progress_value-key  = '02'.
  progress_value-text = 'P/IN'.
  append progress_value to progress_list.
* T/C(03)
  progress_value-key  = '03'.
  progress_value-text = 'T/C'.
  append progress_value to progress_list.
* P/OUT(04)
  progress_value-key  = '04'.
  progress_value-text = 'P/OUT'.
  append progress_value to progress_list.
* PBS/I(05)
  progress_value-key  = '05'.
  progress_value-text = 'PBS/I'.
  append progress_value to progress_list.
* T/IN(07)
  progress_value-key  = '07'.
  progress_value-text = 'T/IN'.
  append progress_value to progress_list.
* C/F(17)
  progress_value-key  = '17'.
  progress_value-text = 'C/F'.
  append progress_value to progress_list.
* S/OFF(18)
  progress_value-key  = '18'.
  progress_value-text = 'S/OFF'.
  append progress_value to progress_list.
* C/GATE(19)
  progress_value-key  = '19'.
  progress_value-text = 'C/GATE'.
  append progress_value to progress_list.
* VPC/I(21)
  progress_value-key  = '21'.
  progress_value-text = 'VPC/I'.
  append progress_value to progress_list.
* VPC/O(22)
  progress_value-key  = '22'.
  progress_value-text = 'VPC/O'.
  append progress_value to progress_list.

endform.                    " set_field_PROG_APP245
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COLUMN_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_column_app245 using p_list  like column01_list[]
                                   p_value like column01_value .
  data: l_count(03).
  do 219 times.
    p_value-key = l_count = l_count + 1.
    condense p_value-key .
    append p_value to p_list .
  enddo.

endform.                    " SET_FIELD_COLUMN_APP245
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_wono_app245.
  select distinct au~atwrt
    into wono_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_WORK_ORDER' .
    append wono_value to wono_list.
  endselect.

endform.                    " SET_FIELD_WONO_APP245
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_EXTC_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_extc_app245.
  select distinct au~atwrt
    into extc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_EXT_COLOR' .
    clear extc_list.
    append extc_value to extc_list.
  endselect.

endform.                    " SET_FIELD_EXTC_APP245
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INTC_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_intc_app245.
  select distinct au~atwrt
    into intc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_INT_COLOR' .
    append intc_value to intc_list.
  endselect.

endform.                    " SET_FIELD_INTC_APP245
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_data_APP245.
  data: l_error ,
        l_text(50) .
  clear l_error.
  perform set_parameter_for_app245 using l_error l_text.
  if l_error <> space.
    concatenate 'Enter The Necessary Parameters!!! -' l_text
      into l_text.
    message i000 with l_text.
    exit.
  endif.
  clear: it_objek, it_objek[], it_app245, it_app245[].
  perform get_vehicle_master_no_app245 tables it_objek.
  perform create_data_app245 .
  perform modify_data_app245 .
endform.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_parameter_for_app245 using p_error p_text .
* Plant
  if p_plant_app245 <> space.
  else.
  endif.

* Model
  if p_model_app245 <> space.
  else.
  endif.

* Line
  if p_line_app245 <> space.
  else.
  endif.

* Production Date
  if p_shop_date_app245 <> space.
  else.
    p_error = 'X'.
    p_text = 'Production Date'.
    exit.
  endif.
* Ending Date
  if p_end_date_app245 <> space.
  else.
    p_error = 'X'.
    p_text = 'Ending Date'.
    exit.
  endif.

* Progress
  if p_prog_app245 <> space.
  else.
    p_error = 'X'.
    p_text = 'Progress'.
    exit.
  endif.

* Summary Type
  if p_type_app245 <> space.
  else.
    p_error = 'X'.
    p_text = 'Summary Type'.
    exit.
  endif.

* Whether or Not Color
  if p_color_app245 <> space.
  else.
    p_error = 'X'.
    p_text = 'Color Yes or No'.
    exit.
  endif.

* Work Order
  if p_wono_app245 <> space.
  else.

  endif.

* External Color
  if p_extc_app245 <> space.
  else.
  endif.

* Internal Color
  if p_intc_app245 <> space.
  else.
  endif.

* Column
  if p_column01_app245 <> space.
  else.
    if p_type_app245 = '2'.
      p_error = 'X'.
      p_text = 'Column'.
      exit.
    endif.
  endif.


endform.                    " SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
form get_vehicle_master_no_app245 tables p_it_objek structure it_objek .
  data: l_subrc    type sy-subrc ,
        l_atnam    type cabn-atnam,
        l_atwrt    type ausp-atwrt,
        l_atflv_st type ausp-atflv,
        l_temp(06),
        l_datum    type sy-datum,
        l_atflv_en type ausp-atflv,
        l_num(08) type n.
* r_prog FOR P_PROG_APP245,       "P_RP_STATUS
  select distinct objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where klart = '002'           and
          au~atwrt = p_prog_app245       and
          ca~atnam = 'P_RP_STATUS'  .
    if sy-subrc = 0  .
**    P_SHOP_DATE_APP245     "P_RPxx_SHOP_DATE
      if p_shop_date_app245 <> space.
        clear l_subrc .
*       Setting Starting Date
        concatenate 'P_RP' p_prog_app245 '_SHOP_DATE'
          into l_atnam .
        l_atflv_st = l_num = p_shop_date_app245 .
*       Setting Ending Date
        concatenate p_shop_date_app245+00(06) p_end_date_app245
          into l_datum.
        if p_shop_date_app245 > l_datum.
          call function 'RP_CALC_DATE_IN_INTERVAL'
               exporting
                    date      = p_shop_date_app245
                    months    = '01'
                    days      = '00'
                    signum    = '+'
                    years     = '00'
               importing
                    calc_date = l_datum.
          concatenate l_datum+00(06) p_end_date_app245
            into l_datum.
        endif.
        l_atflv_en = l_num = l_datum .
        perform check_shop_date_app245 using    it_objek-objek
                                                l_atnam
                                                l_atflv_st
                                                l_atflv_en
                                       changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_MODEL_APP245,     "P_MODEL
      if p_model_app245 <> space.
        clear l_subrc .
        move p_model_app245 to l_atwrt .
        perform check_data_of_vm_app245 using it_objek-objek
                                              'P_MODEL'
                                              l_atwrt
                                        changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_WONO_APP245,       "P_WORK_ORDER
      if p_wono_app245 <> space.
        clear l_subrc .
        move p_wono_app245 to l_atwrt .
        perform check_data_of_vm_app245 using    it_objek-objek
                                                'P_WORK_ORDER'
                                                l_atwrt
                                        changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_EXTC_APP245,       "P_EXT_COLOR
      if p_extc_app245 <> space.
        clear l_subrc .
        move p_extc_app245 to l_atwrt .
        perform check_data_of_vm_app245 using it_objek-objek
                                       'P_EXT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_INTC_APP245.       "P_INT_COLOR
      if p_intc_app245 <> space.
        clear l_subrc .
        move p_intc_app245 to l_atwrt .
        perform check_data_of_vm_app245 using it_objek-objek
                                       'P_INT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_COLUMN01_APP245 ~ 10  "P_219_xxx
      perform check_219_code_app245 using    it_objek-objek
                             changing l_subrc .
      if l_subrc <> 0.
        continue.
      endif.
      append it_objek.
    endif.
  endselect.
  sort it_objek by objek .
endform.                    " get_vehicle_master_no
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_data_app245.
  data: l_rpno(02) type n          ,
        l_atnam    type cabn-atnam .
  clear: it_temp_app245, it_temp_app245[].
  loop at it_objek.
    clear it_app245.
*   V/M No.
    move-corresponding it_objek to it_temp_app245.
**  Summary Type : Order No.
    if p_type_app245 = 1. "<-- Order No
*     Work Order(Serial)
      perform read_normal_class_app245 using    it_temp_app245-objek
                                                  'P_WORK_ORDER'
                                         changing it_temp_app245-suminf.
      if p_color_app245 = 'O'.
*      External Color
       perform read_normal_class_app245 using    it_temp_app245-objek
                                                          'P_EXT_COLOR'
                                           changing it_temp_app245-extc.
*      Internal Color
       perform read_normal_class_app245 using    it_temp_app245-objek
                                                          'P_INT_COLOR'
                                           changing it_temp_app245-intc.
      endif.
**    Date : P_RPxx_SHOP_DATE.
      l_rpno = p_prog_app245 .
      concatenate 'P_RP' l_rpno '_SHOP_DATE'
        into l_atnam .
      perform read_shop_date_app245 using    it_temp_app245-objek
                                      l_atnam
                             changing it_temp_app245-date.

**  Summary Type : 219 Code.
    else.          "<-- 219 Code : P_TYPE_APP245 = '2'.
*---> CASE : Only The First Column's Code & Value
      perform read_code_inf_app245 using    it_temp_app245-objek
                                     p_column01_app245
                                     p_value01_app245
                            changing it_temp_app245-suminf.
**    Date : P_RPxx_SHOP_DATE.
      l_rpno = p_prog_app245 .
      concatenate 'P_RP' l_rpno '_SHOP_DATE'
        into l_atnam .
      perform read_shop_date_app245 using    it_temp_app245-objek
                                      l_atnam
                             changing it_temp_app245-date.

    endif.
*
    append it_temp_app245.
*
  endloop.
  sort it_temp_app245 by suminf extc intc date .

endform.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP245_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP245_MI  text
*----------------------------------------------------------------------*
form read_normal_class_app245 using    p_vmno
                                         p_char
                                changing p_value.
  select single au~atwrt
    into p_value
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_vmno      and
          klart = '002'       and
          ca~atnam = p_char  .

endform.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_data_of_vm_app245 using    p_vmno
                                      p_char
                                      p_value
                             changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_vmno         and
          klart = '002'          and
          au~atwrt = p_value     and
          ca~atnam = p_char      .
  p_subrc = sy-subrc.
endform.                    " check_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form download_data_app245.
  clear: it_excel_app245, it_excel_app245[].
  perform set_header_app245         tables it_excel_app245.
  perform set_body_app245           tables it_excel_app245.
  perform call_func_download_app245 tables it_excel_app245.

endform.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
form set_header_app245 tables   p_it_excel structure it_excel_app245.
  write: 'Summary Object' to p_it_excel-suminf   ,    "Summary Type
         'External Color' to p_it_excel-extc    ,      "P_EXT_COLOR(03)
         'Internal Color' to p_it_excel-intc    ,      "P_INT_COLOR(03)
*
         'Total'   to p_it_excel-total(20)    ,          "Total Quantity
*
         '1st QTY' to p_it_excel-01qty(20)    ,
         '2nd QTY' to p_it_excel-02qty(20)    ,
         '3rd QTY' to p_it_excel-03qty(20)    ,
         '4th QTY' to p_it_excel-04qty(20)    ,
         '5th QTY' to p_it_excel-05qty(20)    ,
         '6th QTY' to p_it_excel-06qty(20)    ,
         '7th QTY' to p_it_excel-07qty(20)    ,
         '8th QTY' to p_it_excel-08qty(20)    ,
         '9th QTY' to p_it_excel-09qty(20)    ,
         '10th QTY' to p_it_excel-10qty(20)    ,
         '11st QTY' to p_it_excel-11qty(20)    ,
         '12nd QTY' to p_it_excel-12qty(20)    ,
         '13rd QTY' to p_it_excel-13qty(20)    ,
         '14th QTY' to p_it_excel-14qty(20)    ,
         '15th QTY' to p_it_excel-15qty(20)    ,
         '16th QTY' to p_it_excel-16qty(20)    ,
         '17th QTY' to p_it_excel-17qty(20)    ,
         '18th QTY' to p_it_excel-18qty(20)    ,
         '19th QTY' to p_it_excel-19qty(20)    ,
         '20th QTY' to p_it_excel-20qty(20)    ,
         '21st QTY' to p_it_excel-21qty(20)    ,
         '22nd QTY' to p_it_excel-22qty(20)    ,
         '23rd QTY' to p_it_excel-23qty(20)    ,
         '24th QTY' to p_it_excel-24qty(20)    ,
         '25th QTY' to p_it_excel-25qty(20)    ,
         '26th QTY' to p_it_excel-26qty(20)    ,
         '27th QTY' to p_it_excel-27qty(20)    ,
         '28th QTY' to p_it_excel-28qty(20)    ,
         '29th QTY' to p_it_excel-29qty(20)    ,
         '30th QTY' to p_it_excel-30qty(20)    ,
         '31th QTY' to p_it_excel-31qty    .
*
  append p_it_excel.

endform.                    " set_header
*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
form set_body_app245 tables   p_it structure it_excel_app245 .
  loop at it_app245.
    clear p_it.
    move-corresponding it_app245 to p_it.
    append p_it.
  endloop.
endform.                    " set_body
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
form call_func_download_app245 tables   p_it structure it_excel_app245.
  call function 'DOWNLOAD'
       exporting
            filename                =
               'Prod Results Per Each Progress.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       tables
            data_tab                = p_it
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

endform.                    " call_func_download
*&---------------------------------------------------------------------*
*&      Form  sort_ascending_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_ascending_app245.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app245 ascending by (field_name01).
  endif.

endform.                    " sort_ascending_APP245
*&---------------------------------------------------------------------*
*&      Form  sort_descending_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_descending_app245.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app245 descending by (field_name01).
  endif.

endform.                    " sort_descending_APP245
*&---------------------------------------------------------------------*
*&      Form  check_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_shop_date_app245 using    p_objek
                                     p_atnam
                                     p_atflv_st
                                     p_atflv_en
                            changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_objek and
          klart = '002'          and
          au~atflv >= p_atflv_st and
          au~atflv <= p_atflv_en and
          ca~atnam = p_atnam      .
  p_subrc = sy-subrc.

endform.                    " check_shop_date
*&---------------------------------------------------------------------*
*&      Form  check_interval_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      -->P_0857   text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_interval_data using    p_objek
                                  p_atnam
                                  p_atwrt
                                  p_mark
                         changing p_subrc.
  data: lc_from    type c value ' ',
        lc_to      type c value 'X'.
  ranges: lr_atwrt for  ausp-atwrt .

  clear: lr_atwrt, lr_atwrt[].
  if p_mark = lc_from.
    lr_atwrt-low = p_atwrt .
    lr_atwrt-option = 'GE' .
    lr_atwrt-sign   = 'I'  .
    append lr_atwrt.
  else.                     "p_mark = lc_to .
    lr_atwrt-low = p_atwrt .
    lr_atwrt-option = 'LE' .
    lr_atwrt-sign   = 'I'  .
    append lr_atwrt.
  endif.

  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_objek        and
          klart = '002'          and
          au~atwrt in lr_atwrt   and
          ca~atnam = p_atnam      .
  p_subrc = sy-subrc.

endform.                    " check_interval_data
*&---------------------------------------------------------------------*
*&      Form  check_219_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_219_code_app245 using    p_objek
                    changing p_subrc.
  data: lc_column(10) type c,
        lc_value(10)  type c,
        lc_num(02)    type n ,
        l_atwrt       type ausp-atwrt,
        l_atnam       type cabn-atnam.
  field-symbols: <fs_column> ,
                 <fs_value>  .

  do 10 times.
    clear p_subrc.
*   Defining Column
    lc_num = lc_num + 1 .
    concatenate 'P_COLUMN' lc_num '_APP245'
      into lc_column.
    assign (lc_column) to <fs_column> .
*   Defining 219 Code Name         "ATNAM
    write <fs_column> to l_atnam left-justified.
    concatenate 'P_219_' l_atnam
      into l_atnam .
*   Defining 219 Code's Value      "ATWRT
    concatenate 'P_VALUE' lc_num '_APP245'
      into lc_value.
    assign (lc_value) to <fs_value> .
    move <fs_value> to l_atwrt .
*
    if ( <fs_column> =  space and
         <fs_value>  =  space     ) or
       ( <fs_column> <> space and
         <fs_value>  =  '*'     ) .
      continue .
    endif.
*
    perform check_data_of_vm_app245 using it_objek-objek
                                   l_atnam
                                   l_atwrt
                             changing p_subrc.
    if p_subrc <> 0.
      exit.
    endif.

  enddo.


endform.                    " check_219_code
*&---------------------------------------------------------------------*
*&      Form  set_field_color_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_color_app245.
  color_value-key = 'O'.
  color_value-text = 'Yes'.
  append color_value to color_list.
  color_value-key = 'X'.
  color_value-text = 'No'.
  append color_value to color_list.

endform.                    " set_field_color_app245
*&---------------------------------------------------------------------*
*&      Form  set_field_endate_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_endate_app245.
  data l_num(02) type n.
  do 31 times.
    l_num = l_num + 1.
    move l_num to endate_value-key.
    append endate_value to endate_list.
  enddo.

endform.                    " set_field_endate_app245
*&---------------------------------------------------------------------*
*&      Form  set_field_type_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_type_app245.
  type_value-key = '1'.
  type_value-text = 'Order No'.
  append type_value to type_list.
  type_value-key = '2'.
  type_value-text = '219'.
  append type_value to type_list.

endform.                    " set_field_type_app245
*&---------------------------------------------------------------------*
*&      Form  set_columnes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_columns_APP245.
* Column01
  clear: column01_list, COLUMN01_LIST[],
         column01_value.
  name = 'P_COLUMN01_APP245'.
  perform set_field_column_app245 using column01_list
                                        column01_value.
  perform call_function_vrm_app245 using column01_list.
* COLUMN02
  clear: column02_list, COLUMN02_LIST[],
         column02_value.
  name = 'P_COLUMN02_APP245'.
  perform set_field_column_app245 using column02_list
                                        column02_value.
  perform call_function_vrm_app245 using column02_list.
* COLUMN03
  clear: column03_list, column03_list[], column03_value.
  name = 'P_COLUMN03_APP245'.
  perform set_field_column_app245 using column03_list
                                        column03_value.
  perform call_function_vrm_app245 using column03_list.
* COLUMN04
  clear: column04_list, column04_list[], column04_value.
  name = 'P_COLUMN04_APP245'.
  perform set_field_column_app245 using column04_list
                                        column04_value.
  perform call_function_vrm_app245 using column04_list.
* COLUMN05
  clear: column05_list, column05_list[], column05_value.
  name = 'P_COLUMN05_APP245'.
  perform set_field_column_app245 using column05_list
                                        column05_value.
  perform call_function_vrm_app245 using column05_list.
* Column06
  clear: column06_list, column06_list[], column06_value.
  name = 'P_COLUMN06_APP245'.
  perform set_field_column_app245 using column06_list
                                        column06_value.
  perform call_function_vrm_app245 using column06_list.
* Column07
  clear: column07_list, column07_list[], column07_value.
  name = 'P_COLUMN07_APP245'.
  perform set_field_column_app245 using column07_list
                                        column07_value.
  perform call_function_vrm_app245 using column07_list.
* Column08
  clear: column08_list, column08_list[], column08_value.
  name = 'P_COLUMN08_APP245'.
  perform set_field_column_app245 using column08_list
                                        column08_value.
  perform call_function_vrm_app245 using column08_list.
* Column09
  clear: column09_list, column09_list[], column09_value.
  name = 'P_COLUMN09_APP245'.
  perform set_field_column_app245 using column09_list
                                        column09_value.
  perform call_function_vrm_app245 using column09_list.
* Column10
  clear: column10_list, column10_list[], column10_value.
  name = 'P_COLUMN10_APP245'.
  perform set_field_column_app245 using column10_list
                                        column10_value.
  perform call_function_vrm_app245 using column10_list.

endform.                    " set_columnes
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_date_APP245.
  data : l_date        type sy-datum,
         l_num(02)     type n,
         l_num_tot(02) type n,
         l_count(02)   type n,
         l_header(20)  type c.
  field-symbols : <fs_field>.
  if p_shop_date_app245 <> space and
     p_end_date_app245  <> space   .
*   Setting The First Day.
    move: p_shop_date_app245 to l_date,
          l_date+06(02) to l_num.
*   Caculating The Total Days to be set.
    l_num_tot = p_end_date_app245 - l_num + 1 .
    do 31 times.
      l_count = l_count + 1.
      concatenate 'P_D' l_count '_APP245' into l_header .
      assign (l_header) to <fs_field>.
*     Setting The Day.
      <fs_field> = l_num .
*     Increasing The Day.
      l_date = l_date + 1.
      l_num = l_date+06(02).
    enddo.

  endif.
endform.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  read_code_inf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_COLUMN01_APP245  text
*      -->P_P_VALUE01_APP245  text
*      <--P_IT_TEMP_APP245_SUMINF  text
*----------------------------------------------------------------------*
form read_code_inf_app245 using    p_objek
                            p_column
                            p_value
                   changing p_suminf.
  data:   l_atnam type cabn-atnam ,
          l_atwrt type ausp-atwrt .
  ranges: lr_atwrt for ausp-atwrt .

* Setting Characteristic Name.
  concatenate 'P_219_' p_column
    into l_atnam.
* Setting Characteristic Value.
  if p_value <> '*'.
    lr_atwrt-option = 'EQ'.
    lr_atwrt-sign   = 'I' .
    lr_atwrt-low    = p_value .
    append lr_atwrt .
  else.
    clear: lr_atwrt, lr_atwrt[].
  endif.

  select single ca~atnam au~atwrt
    into (l_atnam, l_atwrt)
    from ausp as au
      inner join cabn as ca on ca~atinn = au~atinn
    where au~objek =  p_objek     and
          au~klart =  '002'       and
          ca~atnam =  l_atnam     and
          au~atwrt in lr_atwrt      .
  concatenate l_atnam l_atwrt
    into p_suminf separated by space .

endform.                    " read_code_inf
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_APP245_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP245_DATE  text
*----------------------------------------------------------------------*
form read_shop_date_app245 using    p_objek
                             p_atnam
                    changing p_date. "p_atflv.
  data: l_atflv   type ausp-atflv,
        l_num(08) type n         .
  select single au~atflv
    into l_atflv
    from ausp as au
      inner join cabn as ca on ca~atinn = au~atinn
    where au~objek =  p_objek     and
          au~klart =  '002'       and
          ca~atnam =  p_atnam       .

  p_date = l_num = l_atflv .

endform.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  modify_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_data_app245.
  data: l_field_name(30) .
  field-symbols : <fs_dqty> .
  clear: it_date, it_date[].
*
  perform setting_date_app245 tables it_date.
*
  loop at it_temp_app245.
    clear it_app245 .
    move-corresponding it_temp_app245 to it_app245 .
    it_app245-total = 1 .
    read table it_date with key date = it_temp_app245-date .
    concatenate 'IT_APP245-' it_date-num 'QTY'
      into l_field_name .
    assign (l_field_name) to <fs_dqty>.
    <fs_dqty> = 1 .
    collect it_app245 .
  endloop.
  loop at it_app245.
    at last.
      sum.
      it_app245-suminf = 'Total'.
      append it_app245.
      exit.
    endat.
  endloop.
endform.                    " modify_data
*&---------------------------------------------------------------------*
*&      Form  setting_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATE  text
*----------------------------------------------------------------------*
form setting_date_app245 tables   p_it_date structure  it_date .
  data: l_date    type sy-datum,
        l_num(02) type n.

  move p_shop_date_app245 to l_date.
  l_num = '01'.
  do 31 times.
    clear p_it_date.
    move: l_date to p_it_date-date,
          l_num  to p_it_date-num .
    append p_it_date.
    l_date = l_date + 1.
    l_num  = l_num  + 1.
    if l_date+06(02) > p_end_date_app245.
      exit .
    endif.
  enddo.

endform.                    " setting_date
