*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_dropdown_list_box_app240.
* Plant
  clear: plant_list, plant_list[],
         plant_value.
  name = 'P_PLANT_APP240'.
  perform set_field_plant_app240.
  perform call_function_vrm_app240 using plant_list.
* Model
  clear: model_list, model_list[],
         model_value.
  name = 'P_MODEL_APP240'.
  perform set_field_model_app240.
  perform call_function_vrm_app240 using model_list.
* Line
  clear: line_list, line_list[],
         line_value.
  name = 'P_LINE_APP240'.
  perform set_field_line_app240.
  perform call_function_vrm_app240 using line_list.
* Progress
  clear: progress_list, progress_list[],
         progress_value.
  name = 'P_PROG_APP240'.
  perform set_field_prog_app240.
  perform call_function_vrm_app240 using progress_list.
* Part - U or C
  clear: part_list, part_list[],
         part_value.
  name = 'P_PART_APP240'.
  perform set_field_part_app240.
  perform call_function_vrm_app240 using part_list.
* Column
  clear: column_list, column_list[],
         column_value.
  name = 'P_COLUMN_APP240'.
  perform set_field_column_app240.
  perform call_function_vrm_app240 using column_list.
* Body No.
  clear: bodyno_list, bodyno_list[],
         bodyno_value.
  name = 'P_BODYNO_APP240'.
  perform set_field_bodyno_app240.
  perform call_function_vrm_app240 using bodyno_list.
* Work Order
  clear: wono_list, wono_list[],
         wono_value.
  name = 'P_WONO_APP240'.
  perform set_field_wono_app240.
  perform call_function_vrm_app240 using wono_list.
* External Color
  clear: extc_list, extc_list[],
         extc_value.
  name = 'P_EXTC_APP240'.
  perform set_field_extc_app240.
  perform call_function_vrm_app240 using extc_list.
* Internal Color
  clear: intc_list, intc_list[],
         intc_value.
  name = 'P_INTC_APP240'.
  perform set_field_intc_app240.
  perform call_function_vrm_app240 using intc_list.

endform.                    " make_dropdown_list_box_app240
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANT_APP240_LIST  text
*----------------------------------------------------------------------*
form call_function_vrm_app240 using    p_list.
  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = p_list.

endform.                    " call_function_vrm_APP240
*&---------------------------------------------------------------------*
*&      Form  set_field_plant_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_plant_app240.
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

endform.                    " set_field_plant_app240
*&---------------------------------------------------------------------*
*&      Form  set_field_model_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_model_app240.
  select distinct au~atwrt
    into model_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_MODEL' .
    append model_value to model_list.

  endselect.

endform.                    " set_field_model_app240
*&---------------------------------------------------------------------*
*&      Form  set_field_line_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_line_app240.
  select distinct au~atwrt
    into line_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_TRIM_LINE_NO' .
    append line_value to line_list.
  endselect.

endform.                    " set_field_line_app240
*&---------------------------------------------------------------------*
*&      Form  set_field_PROG_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_prog_app240.
  select distinct au~atwrt
    into progress_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_RP_STATUS' .
    append progress_value to progress_list.
  endselect.

endform.                    " set_field_PROG_app240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_PART_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_part_app240.
  part_value-key = 'U'.
  part_value-text = 'Unique Part'.
  append part_value to part_list .

  part_value-key = 'C'.
  part_value-text = 'Color Part'.
  append part_value to part_list.

endform.                    " SET_FIELD_PART_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COLUMN_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_column_app240.
  data: l_count(03).
  do 219 times.
    column_value-key = l_count = l_count + 1.
    write column_value-key to column_value-key left-justified .
    append column_value to column_list .
  enddo.

endform.                    " SET_FIELD_COLUMN_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_BODYNO_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_bodyno_app240.
  select distinct au~atwrt
    into bodyno_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_BODY_SERIAL' .
    append bodyno_value to bodyno_list.
  endselect.

endform.                    " SET_FIELD_BODYNO_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_wono_app240.
  select distinct au~atwrt
    into wono_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_WORK_ORDER' .
    append wono_value to wono_list.
  endselect.

endform.                    " SET_FIELD_WONO_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_EXTC_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_extc_app240.
  select distinct au~atwrt
    into extc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_EXT_COLOR' .
    clear extc_list.
    append extc_value to extc_list.
  endselect.

endform.                    " SET_FIELD_EXTC_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INTC_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_intc_app240.
  select distinct au~atwrt
    into intc_value-key
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '002' and
          ca~atnam = 'P_INT_COLOR' .
    append intc_value to intc_list.
  endselect.

endform.                    " SET_FIELD_INTC_APP240
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_data_APP240.
  data: l_error .
  clear l_error.
  clear: R_PLANT_APP240,  R_PLANT_APP240[],
         R_MODEL_APP240,  R_MODEL_APP240[],
         R_LINE_APP240,   R_LINE_APP240[],
         R_PROG_APP240,   R_PROG_APP240[],
         R_PART_APP240,   R_PART_APP240[],
         R_COLUMN_APP240, R_COLUMN_APP240[],
         R_BODYNO_APP240, R_BODYNO_APP240[],
         R_WONO_APP240,   R_WONO_APP240[],
         R_EXTC_APP240,   R_EXTC_APP240[],
         R_INTC_APP240,   R_INTC_APP240[].
  perform set_parameter_for_APP240 using l_error.
  if l_error <> space.
    message i000 with 'Enter The Necessary Parameters!!!'.
    exit.
  endif.
  clear: it_objek, it_objek[], it_app240, it_app240[].
  perform get_vehicle_master_no_APP240 tables it_objek.
  perform create_data_APP240 .
endform.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_parameter_for_APP240 using p_error .
* Plant
  if p_plant_app240 <> space.
    R_PLANT_APP240-option = 'EQ'.
    R_PLANT_APP240-sign   = 'I'.
    R_PLANT_APP240-low    = p_plant_app240.
    append R_PLANT_APP240.
  else.
    clear: R_PLANT_APP240, R_PLANT_APP240[].
  endif.

* Model
  if p_model_app240 <> space.
    R_MODEL_APP240-option = 'EQ'.
    R_MODEL_APP240-sign   = 'I'.
    R_MODEL_APP240-low    = p_model_app240.
    append R_MODEL_APP240.
  else.
    clear: R_MODEL_APP240, R_MODEL_APP240[].
  endif.

* Line
  if p_line_app240 <> space.
    R_LINE_APP240-option = 'EQ'.
    R_LINE_APP240-sign   = 'I'.
    R_LINE_APP240-low    = p_line_app240.
    append R_LINE_APP240.
  else.
    clear: R_LINE_APP240, R_LINE_APP240[].
  endif.

* Progress
  if p_prog_app240 <> space.
    R_PROG_APP240-option = 'EQ'.
    R_PROG_APP240-sign   = 'I'.
    R_PROG_APP240-low    = p_prog_app240.
    append R_PROG_APP240.
  else.
    clear: R_PROG_APP240, R_PROG_APP240[].
    p_error = 'X'.
  endif.

* Part
  if p_part_app240 <> space.
    R_PART_APP240-option = 'EQ'.
    R_PART_APP240-sign   = 'I'.
    R_PART_APP240-low    = p_part_app240.
    append R_PART_APP240 .
  else.
    clear: R_PART_APP240, R_PART_APP240[].
    p_error = 'X'.
  endif.

* Column
  if p_column_app240 <> space.
    R_COLUMN_APP240-option = 'EQ'.
    R_COLUMN_APP240-sign   = 'I'.
    R_COLUMN_APP240-low    = p_column_app240.
    append R_COLUMN_APP240.
  else.
    clear: R_COLUMN_APP240, R_COLUMN_APP240[].
    p_error = 'X'.
  endif.

* Body Serial
  if p_bodyno_app240 <> space.
    R_BODYNO_APP240-option = 'EQ'.
    R_BODYNO_APP240-sign   = 'I'.
    R_BODYNO_APP240-low    = p_bodyno_app240.
    append R_BODYNO_APP240.
  else.
    clear: R_BODYNO_APP240, R_BODYNO_APP240[].
  endif.

* Work Order
  if p_wono_app240 <> space.
    R_WONO_APP240-option = 'EQ'.
    R_WONO_APP240-sign   = 'I'.
    R_WONO_APP240-low    = p_wono_app240.
    append R_WONO_APP240.
  else.
    clear: R_WONO_APP240, R_WONO_APP240[].
  endif.

* External Color
  if p_extc_app240 <> space.
    R_EXTC_APP240-option = 'EQ'.
    R_EXTC_APP240-sign   = 'I'.
    R_EXTC_APP240-low    = p_extc_app240.
    append R_EXTC_APP240.
  else.
    clear: R_EXTC_APP240, R_EXTC_APP240[].
  endif.

* Internal Color
  if p_intc_app240 <> space.
    R_INTC_APP240-option = 'EQ'.
    R_INTC_APP240-sign   = 'I'.
    R_INTC_APP240-low    = p_intc_app240.
  else.
    clear: R_INTC_APP240, R_INTC_APP240[].
  endif.

endform.                    " SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
form get_vehicle_master_no_APP240 tables p_it_objek structure it_objek .
  data: l_subrc type sy-subrc ,
        l_atwrt type ausp-atwrt.
* R_PROG_APP240 FOR P_PROG_APP240,       "P_RP_STATUS
  select distinct objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where klart = '002'           and
          au~atwrt in R_PROG_APP240      and
          ca~atnam = 'P_RP_STATUS'  .
    if sy-subrc = 0  .
*     R_MODEL_APP240 FOR P_MODEL_APP240,     "P_MODEL
      if p_model_app240 <> space.
        clear l_subrc .
        move p_model_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_MODEL'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_BODYNO_APP240 FOR P_BODYNO_APP240,   "P_BODY_SERIAL
      if p_bodyno_app240 <> space.
        clear l_subrc .
        move p_bodyno_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_BODY_SERIAL'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_PLANT_APP240 FOR P_PLANT_APP240,     "P_TRIM_PLANT_NO
      if p_plant_app240 <> space.
        clear l_subrc .
        move p_plant_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_TRIM_PLANT_NO'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_LINE_APP240 FOR P_LINE_APP240,       "P_TRIM_LINE_NO
      if p_line_app240 <> space.
        clear l_subrc .
        move p_line_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_TRIM_LINE_NO'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_WONO_APP240 FOR P_WONO_APP240,       "P_WORK_ORDER
      if p_wono_app240 <> space.
        clear l_subrc .
        move p_wono_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using    it_objek-objek
                                        'P_WORK_ORDER'
                                        l_atwrt
                               changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_EXTC_APP240 FOR P_EXTC_APP240,       "P_EXT_COLOR
      if p_extc_app240 <> space.
        clear l_subrc .
        move p_extc_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_EXT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
      endif.
*     R_INTC_APP240 FOR P_INTC_APP240.       "P_INT_COLOR
      if p_intc_app240 <> space.
        clear l_subrc .
        move p_intc_app240 to l_atwrt .
        perform check_data_of_vm_APP240 using it_objek-objek
                                       'P_INT_COLOR'
                                       l_atwrt
                                 changing l_subrc .
        if l_subrc <> 0 .
          continue.
        endif.
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
form create_data_APP240.
  data: l_rpno(02)         type n,
        l_rpserial(13)     type c,
        l_model   type ausp-atwrt,
        l_bodyno  type ausp-atwrt.
  loop at it_objek.
    clear it_app240.
*   V/M No.
    move-corresponding it_objek to it_app240.
*   BODYNO (P_MODEL & P_BODY_SERIAL)
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_MODEL'
                                       changing l_model .
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_BODY_SERIAL'
                                       changing l_bodyno .
    concatenate l_model l_bodyno
      into it_app240-bodyno .
*   Work Order(Serial), Ext.C, Int.C
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_WORK_ORDER'
                                       changing it_app240-wono .
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_EXT_COLOR'
                                       changing it_app240-extc .
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_INT_COLOR'
                                       changing it_app240-intc .
*   MI
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_MI'
                                       changing it_app240-mi .
*   OCN
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_OCN'
                                       changing it_app240-ocn .
*   Version
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_VERSION'
                                       changing it_app240-ver .
*   Engine
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_219_9'
                                       changing it_app240-eng .
*   T/M
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_219_7'
                                       changing it_app240-tm .
*   T/L
    perform read_normal_class_APP240 using    it_app240-objek
                                                'P_219_5'
                                       changing it_app240-tl .
*   Serial
    move p_prog_app240 to l_rpno.
    concatenate 'P_RP' l_rpno '_SERIAL'
      into l_rpserial .
    perform read_normal_class_APP240 using    it_app240-objek
                                                l_rpserial
                                       changing it_app240-serial .
*   ALC
    perform read_alc_from_wo_APP240 using    p_part_app240
                                      p_column_app240
                                      it_app240-wono
                                      it_app240-extc
                                      it_app240-intc
                             changing it_app240-alc .
    append it_app240.
  endloop.
  sort it_app240 by serial bodyno.

endform.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP240_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP240_MI  text
*----------------------------------------------------------------------*
form read_normal_class_APP240 using    p_vmno
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
*&      Form  read_alc_from_wo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PART  text
*      -->P_P_COLUMN  text
*      -->P_IT_APP240_WONO  text
*      -->P_IT_APP240_EXTC  text
*      -->P_IT_APP240_INTC  text
*      <--P_IT_APP240_ALC  text
*----------------------------------------------------------------------*
form read_alc_from_wo_app240 using    p_part
                               p_column
                               p_wono
                               p_extc
                               p_intc
                      changing p_alc.
  data: l_matnr type ausp-objek,
        l_atnam type cabn-atnam,
        l_column type i.
* alc TYPE ausp-atwrt,     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
  l_column = p_column.
  if p_part_app240 = 'U'.
    l_matnr = p_wono.
    write l_column to l_atnam left-justified .
    concatenate 'P_ALC_U_' l_atnam
      into l_atnam.
  else.
    write l_column to l_atnam left-justified .
    concatenate p_wono p_extc p_intc
      into l_matnr .
    concatenate 'P_ALC_C_' l_atnam
      into l_atnam.
  endif.

  select single atwrt
    into p_alc
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where au~objek = l_matnr and
          klart    = '001'   and
          ca~atnam = l_atnam   .

endform.                    " read_alc_from_wo
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
form check_data_of_vm_APP240 using    p_vmno
                               p_char
                               p_value
                      changing p_subrc.
  select single objek
    into it_objek-objek
    from ausp as au
      inner join cabn as ca on au~atinn = ca~atinn
    where objek = it_objek-objek and
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
form download_data_APP240.
  clear: it_excel_app240, it_excel_app240[].
  perform set_header_APP240         tables it_excel_app240.
  perform set_body_APP240           tables it_excel_app240.
  perform call_func_download_APP240 tables it_excel_app240.

endform.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
form set_header_APP240 tables   p_it_excel structure it_excel_app240.
  write: 'Serial' to p_it_excel-serial,
         'Body No.' to p_it_excel-bodyno,
         'Work Order' to p_it_excel-wono,
         'Model Index' to p_it_excel-mi,
         'OCN' to p_it_excel-ocn,
         'Version' to p_it_excel-ver,
         'External Color' to p_it_excel-extc,
         'Internal Color' to p_it_excel-intc,
         'ALC' to p_it_excel-alc,
         'Engine' to p_it_excel-eng,
         'T/M' to p_it_excel-tm,
         'T/L' to p_it_excel-tl.
  append p_it_excel.

endform.                    " set_header
*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
form set_body_APP240 tables   p_it structure it_excel_app240 .
  loop at it_app240.
    clear p_it.
    move-corresponding it_app240 to p_it.
    append p_it.
  endloop.
endform.                    " set_body
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
form call_func_download_APP240 tables   p_it structure it_excel_app240.
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
*&      Form  sort_ascending_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_ascending_app240.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app240 ascending by (field_name01).
  endif.

endform.                    " sort_ascending_app240
*&---------------------------------------------------------------------*
*&      Form  sort_descending_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_descending_app240.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app240 descending by (field_name01).
  endif.

endform.                    " sort_descending_app240
