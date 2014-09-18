*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_dropdown_list_box_app244.
* Plant
  clear: plant_list, plant_value.
  name = 'P_PLANT_APP245'.
  perform set_field_plant_app244.
  perform call_function_vrm_app244 using plant_list.
* Model
  clear: model_list, model_value.
  name = 'P_MODEL_APP244'.
  perform set_field_model_app244.
  perform call_function_vrm_app244 using model_list.
* Body Serial "P_BODY_SERIAL
  clear: body_ser_list, body_ser_value.
  name = 'P_BODYNO_APP244'.
  perform set_field_body_ser_app244.
  perform call_function_vrm_app244 using body_ser_list.
* Line
  clear: line_list, line_value.
  name = 'P_LINE_APP244'.
  perform set_field_line_app244.
  perform call_function_vrm_app244 using line_list.
* Progress
  clear: progress_list, progress_value.
  name = 'P_PROG_APP244'.
  perform set_field_prog_app244.
  perform call_function_vrm_app244 using progress_list.
* Work Order
  clear: wono_list, wono_value.
  name = 'P_WONO_APP244'.
  perform set_field_wono_app244.
  perform call_function_vrm_app244 using wono_list.
* External Color
  clear: extc_list, extc_value.
  name = 'P_EXTC_APP244'.
  perform set_field_extc_app244.
  perform call_function_vrm_app244 using extc_list.
* Internal Color
  clear: intc_list, intc_value.
  name = 'P_INTC_APP244'.
  perform set_field_intc_app244.
  perform call_function_vrm_app244 using intc_list.
* Columnes
  perform set_columns_app244 .

endform.                    " make_dropdown_list_box_APP244
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANT_LIST  text
*----------------------------------------------------------------------*
form call_function_vrm_app244 using    p_list.
  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = p_list.

endform.                    " call_function_vrm_APP244
*&---------------------------------------------------------------------*
*&      Form  set_field_plant_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_plant_app244.
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

endform.                    " set_field_plant_APP244
*&---------------------------------------------------------------------*
*&      Form  set_field_model_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_model_app244.
  select distinct au~atwrt
    into model_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_MODEL' .
    append model_value to model_list.

  endselect.

endform.                    " set_field_model_APP244
*&---------------------------------------------------------------------*
*&      Form  set_field_line_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_line_app244.
  select distinct au~atwrt
    into line_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_TRIM_LINE_NO' .
    append line_value to line_list.
  endselect.

endform.                    " set_field_line_APP244
*&---------------------------------------------------------------------*
*&      Form  set_field_PROG_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_prog_app244.
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

endform.                    " set_field_PROG_APP244
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COLUMN_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_column_app244 using p_list  like column01_list[]
                                   p_value like column01_value .
  data: l_count(03).
  do 219 times.
    p_value-key = l_count = l_count + 1.
    condense p_value-key .
    append p_value to p_list .
  enddo.

endform.                    " SET_FIELD_COLUMN_APP244
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_wono_app244.
  select distinct au~atwrt
    into wono_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_WORK_ORDER' .
    append wono_value to wono_list.
  endselect.

endform.                    " SET_FIELD_WONO_APP244
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_EXTC_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_extc_app244.
  select distinct au~atwrt
    into extc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_EXT_COLOR' .
    clear extc_list.
    append extc_value to extc_list.
  endselect.

endform.                    " SET_FIELD_EXTC_APP244
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INTC_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_intc_app244.
  select distinct au~atwrt
    into intc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_INT_COLOR' .
    append intc_value to intc_list.
  endselect.

endform.                    " SET_FIELD_INTC_APP244
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_data_app244.
  data: l_error ,
        l_text(50) .
  clear l_error.
  perform set_parameter_for_app244 using l_error l_text.
  if l_error <> space.
    concatenate 'Enter The Necessary Parameters!!! -' l_text
      into l_text.
    message i000 with l_text.
    exit.
  endif.
  clear: it_objek, it_objek[], it_app244, it_app244[].
  perform get_vehicle_master_no_app244 tables it_objek.
  perform create_data_app244 .
endform.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_parameter_for_app244 using p_error p_text .
* Plant
  if p_plant_app244 <> space.
  else.
  endif.

* Model
  if p_model_app244 <> space.
  else.
  endif.

* Line
  if p_line_app244 <> space.
  else.
  endif.

* Production Date
  if p_prod_date_app244 <> space.
  else.
    p_error = 'X'.
    p_text = 'Production Date'.
    exit.
  endif.

* Progress
  if p_prog_app244 <> space.
  else.
    p_error = 'X'.
    p_text = 'Progress'.
    exit.
  endif.

* Work Order
  if p_wono_app244 <> space.
  else.

  endif.

* External Color
  if p_extc_app244 <> space.
  else.
  endif.

* Internal Color
  if p_intc_app244 <> space.
  else.
  endif.



endform.                    " SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
form get_vehicle_master_no_app244 tables p_it_objek structure it_objek .
  data: l_subrc    type sy-subrc ,
        l_atnam    type cabn-atnam,
        l_atwrt    type ausp-atwrt,
        l_atflv_st type ausp-atflv,
        l_temp(06),
        l_datum    type sy-datum,
        l_atflv_en type ausp-atflv,
        l_num(08) type n.
* r_prog FOR P_PROG_APP244,       "P_RP_STATUS
  select distinct objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where klart = '002'           and
          au~atwrt = p_prog_app244       and
          ca~atnam = 'P_RP_STATUS'  .
    if sy-subrc = 0  .
**    P_PROD_DATE_APP244     "P_RPxx_SHOP_DATE
      if p_prod_date_app244 <> space.
        clear l_subrc .
        concatenate 'P_RP' p_prog_app244 '_SHOP_DATE'
          into l_atnam .
        l_atflv_st = l_num = p_prod_date_app244 .
        perform check_shop_date_app244 using    it_objek-objek
                                         l_atnam
                                         l_atflv_st
                                changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_MODEL_APP244,     "P_MODEL
      if p_model_app244 <> space.
        clear l_subrc .
        move p_model_app244 to l_atwrt .
        perform check_data_of_vm_app244 using it_objek-objek
                                       'P_MODEL'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_BODYNO_APP244,     "P_BODY_SERIAL
      if p_bodyno_app244 <> space.
        clear l_subrc .
        move p_bodyno_app244 to l_atwrt.
        perform check_data_of_vm_app244 using it_objek-objek
                                       'P_BODY_SERIAL'
                                       l_atwrt
                                 changing l_subrc.
        if l_subrc <> 0.
          continue .
        endif.
      endif.
**    P_SERIAL_ST_APP244,   "P_RPxx_SERIAL(From)
      if p_serial_st_app244 <> space.
        clear l_subrc .
        move p_serial_st_app244 to l_atwrt .
        concatenate 'P_RP' p_prog_app244 '_SERIAL'
          into l_atnam .
        perform check_from_data_of_vm_app244 using it_objek-objek
                                            l_atnam
                                            l_atwrt
                                      changing l_subrc.
        if l_subrc <> 0.
          continue.
        endif.
      endif.
**    P_SERIAL_EN_APP244,   "P_RPxx_SERIAL(To)
      if p_serial_en_app244 <> space.
        clear l_subrc .
        move p_serial_en_app244 to l_atwrt .
        concatenate 'P_RP' p_prog_app244 '_SERIAL'
          into l_atnam .
        perform check_to_data_of_vm_app244 using it_objek-objek
                                          l_atnam
                                          l_atwrt
                                    changing l_subrc.
        if l_subrc <> 0.
          continue .
        endif.
      endif.
**    P_ACT_DATE_ST_APP244, "P_RPxx_ACTUAL_DATE+00(08) (From)
      if not ( p_act_date_st_app244 is initial ) .
        clear l_subrc .
        move p_act_date_st_app244 to l_atwrt.
**      P_ACT_TIME_ST_APP244, "P_RPxx_ACTUAL_DATE+08(06) (From)
        if p_act_time_st_app244 is initial.
          concatenate l_atwrt '000000'
            into l_atwrt .
        else.
          concatenate l_atwrt p_act_time_st_app244
            into l_atwrt .
        endif.
        concatenate 'P_RP' p_prog_app244 '_ACTUAL_DATE'
          into l_atnam .
        perform check_from_data_of_vm_app244 using it_objek-objek
                                            l_atnam
                                            l_atwrt
                                      changing l_subrc.
        if l_subrc <> 0.
          continue.
        endif.
      endif.
**    P_ACT_DATE_EN_APP244, "P_RPxx_ACTUAL_DATE+00(08) (To)
      if not ( p_act_date_en_app244 is initial ).
        clear l_subrc .
        move p_act_date_en_app244 to l_atwrt.
**      P_ACT_TIME_EN_APP244, "P_RPxx_ACTUAL_DATE+08(06) (To)
        if p_act_time_en_app244 is initial.
          concatenate l_atwrt '999999'
            into l_atwrt .
        else.
          concatenate l_atwrt p_act_time_en_app244
            into l_atwrt .
        endif.
        concatenate 'P_RP' p_prog_app244 '_ACTUAL_DATE'
          into l_atnam .
        perform check_to_data_of_vm_app244 using it_objek-objek
                                          l_atnam
                                          l_atwrt
                                    changing l_subrc.
        if l_subrc <> 0.
          continue.
        endif.
      endif.
**    P_WONO_APP244,       "P_WORK_ORDER
      if p_wono_app244 <> space.
        clear l_subrc .
        move p_wono_app244 to l_atwrt .
        perform check_data_of_vm_app244 using    it_objek-objek
                                        'P_WORK_ORDER'
                                        l_atwrt
                               changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_EXTC_APP244,       "P_EXT_COLOR
      if p_extc_app244 <> space.
        clear l_subrc .
        move p_extc_app244 to l_atwrt .
        perform check_data_of_vm_app244 using it_objek-objek
                                       'P_EXT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    P_INTC_APP244.       "P_INT_COLOR
      if p_intc_app244 <> space.
        clear l_subrc .
        move p_intc_app244 to l_atwrt .
        perform check_data_of_vm_app244 using it_objek-objek
                                       'P_INT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
**    p_column01 ~ 10  "P_219_xxx
      perform check_219_code_app244 using    it_objek-objek
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
form create_data_app244.
  data: l_rpno(02) type n          ,
        l_atnam    type cabn-atnam ,
        l_atwrt    type ausp-atwrt .
  clear: it_app244, it_app244[].
  loop at it_objek.
    clear it_app244.
*   V/M No.
    move-corresponding it_objek to it_app244.
*   Model
    perform read_normal_class_app244 using it_app244-objek
                                             'P_MODEL'
                                       changing it_app244-model .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    perform read_normal_class_app244 using it_app244-objek
                                             'P_BODY_SERIAL'
                                       changing it_app244-bodyno .
    concatenate it_app244-model it_app244-bodyno
      into it_app244-bodyno .
*   vin TYPE ausp-atwrt,                                "P_VIN(17)
    perform read_normal_class_app244 using it_app244-objek
                                             'P_VIN'
                                       changing it_app244-vin .
*   vendor(10),    "Not Defined
*   mi TYPE ausp-atwrt,                                 "P_MI (07)
    perform read_normal_class_app244 using it_app244-objek
                                             'P_MI'
                                       changing it_app244-mi .
*   ocn TYPE ausp-atwrt,                                "P_OCN (04)
    perform read_normal_class_app244 using it_app244-objek
                                             'P_OCN'
                                       changing it_app244-ocn .
*   ver TYPE ausp-atwrt,  "P_VERSION(03)
    perform read_normal_class_app244 using it_app244-objek
                                             'P_VERSION'
                                       changing it_app244-ver .
*   serial TYPE ausp-atwrt,  "P_RPxx_SERIAL(06)
    l_rpno = p_prog_app244 .
    concatenate 'P_RP' l_rpno '_SERIAL'
      into l_atnam.
    perform read_normal_class_app244 using    it_app244-objek
                                                l_atnam
                                       changing it_app244-serial .
*   Work Order(Serial)
    perform read_normal_class_app244 using    it_app244-objek
                                                'P_WORK_ORDER'
                                       changing it_app244-wono.
*   External Color
    perform read_normal_class_app244 using    it_app244-objek
                                                'P_EXT_COLOR'
                                        changing it_app244-extc.
*   Internal Color
    perform read_normal_class_app244 using    it_app244-objek
                                                'P_INT_COLOR'
                                       changing it_app244-intc.
*   prog TYPE ausp-atwrt,      "P_STATUS
    perform read_normal_class_app244 using    it_app244-objek
                                                'P_RP_STATUS'
                                       changing it_app244-prog .
**  Date : P_RPxx_ACTUAL_DATE.
    l_rpno = p_prog_app244 .
    concatenate 'P_RP' l_rpno '_ACTUAL_DATE'
      into l_atnam .
    perform read_normal_class_app244 using    it_app244-objek
                                                l_atnam
                                       changing l_atwrt .
    it_app244-act_date = l_atwrt+00(08).
    it_app244-act_time = l_atwrt+08(06).
*
    append it_app244.
*
  endloop.
  sort it_app244 by bodyno .

endform.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP244_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP244_MI  text
*----------------------------------------------------------------------*
form read_normal_class_app244 using    p_vmno
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
form check_data_of_vm_app244 using    p_vmno
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
form download_data_app244.
  clear: it_excel_app244, it_excel_app244[].
  perform set_header_app244         tables it_excel_app244.
  perform set_body_app244           tables it_excel_app244.
  perform call_func_download_app244 tables it_excel_app244.

endform.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP244  text
*----------------------------------------------------------------------*
form set_header_app244 tables   p_it_excel structure it_excel_app244.
  write:  'Body No.' to  p_it_excel-bodyno  ,
          'V.I.N.' to  p_it_excel-vin  ,
          'Order-No.' to  p_it_excel-wono  ,
          'External Color' to  p_it_excel-extc  ,
          'Internal Color' to  p_it_excel-intc ,
          'Vendor' to  p_it_excel-vendor  ,    "Not Defined
          'Model Index' to  p_it_excel-mi  ,
          'OCN' to  p_it_excel-ocn  ,
          'Version' to  p_it_excel-ver  ,
          'Reporting Date' to  p_it_excel-act_date  ,
          'Reporting Time' to  p_it_excel-act_time ,
          'Present Progress' to  p_it_excel-prog  ,
          'Serial' to  p_it_excel-serial  .
  append p_it_excel.

endform.                    " set_header
*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP244  text
*----------------------------------------------------------------------*
form set_body_app244 tables   p_it structure it_excel_app244 .
  loop at it_app244.
    clear p_it.
    move-corresponding it_app244 to p_it.
    append p_it.
  endloop.
endform.                    " set_body
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP244  text
*----------------------------------------------------------------------*
form call_func_download_app244 tables   p_it structure it_excel_app244.
  call function 'DOWNLOAD'
       exporting
            filename                = 'VM Spec Per Each Progress.XLS'
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
*&      Form  sort_ascending_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_ascending_app244.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app244 ascending by (field_name01).
  endif.

endform.                    " sort_ascending_APP244
*&---------------------------------------------------------------------*
*&      Form  sort_descending_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_descending_app244.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app244 descending by (field_name01).
  endif.

endform.                    " sort_descending_APP244
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
form check_shop_date_app244 using    p_objek
                              p_atnam
                              p_atflv
                     changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_objek and
          klart = '002'      and
          au~atflv = p_atflv and
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
form check_219_code_app244 using    p_objek
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
    concatenate 'P_COLUMN' lc_num
      into lc_column.
    assign (lc_column) to <fs_column> .
*   Defining 219 Code Name         "ATNAM
    write <fs_column> to l_atnam left-justified.
    concatenate 'P_219_' l_atnam
      into l_atnam .
*   Defining 219 Code's Value      "ATWRT
    concatenate 'P_VALUE' lc_num
      into lc_value.
    assign (lc_value) to <fs_value> .
    move <fs_value> to l_atwrt .
*
    if <fs_column> =  space  .
      continue .
    endif.
*
    perform check_data_of_vm_app244 using it_objek-objek
                                   l_atnam
                                   l_atwrt
                             changing p_subrc.
    if p_subrc <> 0.
      exit.
    endif.

  enddo.


endform.                    " check_219_code
*&---------------------------------------------------------------------*
*&      Form  set_columnes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_columns_app244.
* Column01
  clear: column01_list, column01_value.
  name = 'P_COLUMN01_APP244'.
  perform set_field_column_app244 using column01_list
                                        column01_value.
  perform call_function_vrm_app244 using column01_list.
* COLUMN02
  clear: column02_list, column02_value.
  name = 'P_COLUMN02_APP244'.
  perform set_field_column_app244 using column02_list
                                        column02_value.
  perform call_function_vrm_app244 using column02_list.
* COLUMN03
  clear: column03_list, column03_value.
  name = 'P_COLUMN03_APP244'.
  perform set_field_column_app244 using column03_list
                                        column03_value.
  perform call_function_vrm_app244 using column03_list.
* COLUMN04
  clear: column04_list, column04_value.
  name = 'P_COLUMN04_APP244'.
  perform set_field_column_app244 using column04_list
                                        column04_value.
  perform call_function_vrm_app244 using column04_list.
* COLUMN05
  clear: column05_list, column05_value.
  name = 'P_COLUMN05_APP244'.
  perform set_field_column_app244 using column05_list
                                        column05_value.
  perform call_function_vrm_app244 using column05_list.
* Column06
  clear: column06_list, column06_value.
  name = 'P_COLUMN06_APP244'.
  perform set_field_column_app244 using column06_list
                                        column06_value.
  perform call_function_vrm_app244 using column06_list.
* Column07
  clear: column07_list, column07_value.
  name = 'P_COLUMN07_APP244'.
  perform set_field_column_app244 using column07_list
                                        column07_value.
  perform call_function_vrm_app244 using column07_list.
* Column08
  clear: column08_list, column08_value.
  name = 'P_COLUMN08_APP244'.
  perform set_field_column_app244 using column08_list
                                        column08_value.
  perform call_function_vrm_app244 using column08_list.
* Column09
  clear: column09_list, column09_value.
  name = 'P_COLUMN09_APP244'.
  perform set_field_column_app244 using column09_list
                                        column09_value.
  perform call_function_vrm_app244 using column09_list.
* Column10
  clear: column10_list, column10_value.
  name = 'P_COLUMN10_APP244'.
  perform set_field_column_app244 using column10_list
                                        column10_value.
  perform call_function_vrm_app244 using column10_list.

endform.                    " set_columnes
*&---------------------------------------------------------------------*
*&      Form  set_field_body_ser_app244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_body_ser_app244.
  select distinct au~atwrt
    into body_ser_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_BODY_SERIAL' .
    append body_ser_value to body_ser_list.

  endselect.

endform.                    " set_field_body_ser_app244
*&---------------------------------------------------------------------*
*&      Form  check_from_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_from_data_of_vm_app244 using    p_objek
                                    p_atnam
                                    p_atwrt
                           changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_objek         and
          klart = '002'           and
          au~atwrt =  p_atwrt     and
          ca~atnam >= p_atnam      .
  p_subrc = sy-subrc.

endform.                    " check_from_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  check_to_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
form check_to_data_of_vm_app244 using    p_objek
                                  p_atnam
                                  p_atwrt
                         changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = p_objek         and
          klart = '002'           and
          au~atwrt =  p_atwrt     and
          ca~atnam <= p_atnam      .
  p_subrc = sy-subrc.

endform.                    " check_to_data_of_vm
