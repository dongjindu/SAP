*----------------------------------------------------------------------*
***INCLUDE ZAPP236M_FOB .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VEHICLE_NUMBER_SEARCH
*&---------------------------------------------------------------------*
*       Searching V/M's Data & Setting Screen's Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form vehicle_number_search_app236.

  clear g_equichk_app236.
  if st_key_app236-inqopt eq st_code_app236-inqopt      and
     st_app236-model  eq st_code_app236-model and
     st_app236-bodyno eq st_code_app236-bodyno   .
    exit.
  endif.

  check  st_key_app236-inqopt  eq  'VEH'.
  if  st_app236-model  is  initial.
    message s000  with 'Input Model code !'.
    g_crsr_fld_app236 = 'ST_APP236-MODEL'.
  endif.
  move   st_key_app236-inqopt   to  st_code_app236-inqopt.
  move   st_app236-model    to  st_code_app236-model.
  move   st_app236-bodyno   to  st_code_app236-bodyno.

  perform  itab_and_variable_init_app236.

  concatenate  st_app236-model st_app236-bodyno  into  g_equnr_app236.
**  PERFORM  EQUI_MASTER_CHECK_APP236 USING  G_EQUNR_APP236
*                                            G_EQUICHK_APP236.

  check  g_equichk_app236  eq  space.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object    = g_equnr_app236
       tables
            val_table = it_vmv_app236.

  if sy-subrc ne 0.
    message s003 with g_equnr_app236 'Vehicle History Not found!'.
    clear st_app236.
    move   st_code_app236-model   to st_app236-model.
    move   st_code_app236-bodyno  to st_app236-bodyno.
    exit.
  endif.

  describe table it_vmv_app236  lines  it_lines_app236.

  check  it_lines_app236 > 0.
  clear st_app236.
* PROGRESS DATA TABLE
  perform  create_table_it_wip_app236.  "PROGRESS INITIAL LOAD

* VEHICLE DATA
  perform  general_veh_history_app236.  "move data to screen

* 219 option table made
  perform  create_219_option_table.
* Order table made
  perform  create_order_table.
* Airbag  table made
  perform  create_airbag_table.

  move-corresponding  st_app236  to  st_code_app236.
  move-corresponding  st_iss_app236  to  st_code_app236.
  move-corresponding  st_key_app236  to  st_code_app236.

endform.                    " VEHICLE_NUMBER_SEARCH
*&---------------------------------------------------------------------*
*&      Form  vmdata_to_screen_field
*&---------------------------------------------------------------------*
*       Searching a Value Per Characteristic
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form  vmdata_to_screen_field_app236.
  case  it_vmv_app236-atnam.
    when 'P_MODEL'.
      perform  filed_changing_app236  changing  st_app236-model.
    when 'P_BODY_SERIAL'.
      perform  filed_changing_app236  changing  st_app236-bodyno.
    when 'P_WORK_ORDER'.
      perform  filed_changing_app236  changing  st_app236-won.
    when 'P_EXT_COLOR'.
      perform  filed_changing_app236  changing  st_app236-extc.
    when 'P_INT_COLOR'.
      perform  filed_changing_app236  changing  st_app236-intc.
    when 'P_MI'.
      perform  filed_changing_app236  changing  st_app236-mi.
    when 'P_OCN'.
      perform  filed_changing_app236  changing  st_app236-ocn.
    when 'P_VERSION'.
      perform  filed_changing_app236  changing  st_app236-vers.
    when 'P_BC_WORK_ORDER'.
      perform  filed_changing_app236  changing  st_app236-bcs_won.
    when 'P_BC_EXT_COLOR'.
      perform  filed_changing_app236  changing  st_app236-bcs_extc.
    when 'P_BC_INT_COLOR'.
      perform  filed_changing_app236  changing  st_app236-bcs_intc.
    when 'P_BC_MI'.
      perform  filed_changing_app236  changing  st_app236-bcs_mi.
    when 'P_BC_OCN'.
      perform  filed_changing_app236  changing  st_app236-bcs_ocn.
    when 'P_BC_VERSION'.
      perform  filed_changing_app236  changing  st_app236-bcs_ver.
    when 'P_BC_CHANGE_DATE'.
      perform  filed_changing_app236  changing  st_app236-bcs_cdt.
    when 'P_DESTINATION_CODE'.
      perform  filed_changing_app236  changing  st_app236-destc.
    when 'P_MITU'.
      perform  filed_changing_app236  changing  st_app236-mitu.
    when 'P_MITU_DATE'.
      perform  filed_changing_app236  changing  st_app236-mitudat.
    when 'P_VIN'.
      perform  filed_changing_app236  changing  st_app236-vin.
    when 'P_COATING'.
      perform  filed_changing_app236  changing  st_app236-cot.
    when 'P_DELIVERY'.
      perform  filed_changing_app236  changing  st_app236-deliv.
    when 'P_LC_NO'.
      perform  filed_changing_app236  changing  st_app236-lcn.
    when 'P_SEQUENCE_DATE'.
      perform  filed_changing_app236  changing  st_app236-seqdat.
    when 'P_SEQUENCE_SERIAL'.
      perform  filed_changing_app236  changing  st_app236-seqser.
    when 'P_SEQUENCE_CODE'.
      perform  filed_changing_app236  changing  st_app236-seqcod.
    when 'P_PROBLEM'.
      perform  filed_changing_app236  changing  st_app236-pct.
    when 'P_ENGINE_NO'.
      perform  filed_changing_app236  changing  st_app236-engno.
    when 'P_TM_NO'.
      perform  filed_changing_app236  changing  st_app236-tmno.
    when 'P_KEY_NO'.
      perform  filed_changing_app236  changing  st_app236-keyno.
    when 'P_CONSIGN_VENDOR'.
      perform  filed_changing_app236  changing  st_app236-lifnr.
    when 'P_CONSIGN'.
      perform  filed_changing_app236  changing  st_app236-cons.
    when 'P_AUTO_POOL_LOC'.
      perform  filed_changing_app236  changing  st_app236-lot.
    when 'P_TRIM_PLANT_NO'.
      perform  filed_changing_app236  changing  P_TRIM01_APP236.
    when 'P_TRIM_LINE_NO'.
      perform  filed_changing_app236  changing  P_TRIM02_APP236.
  endcase.

* REPORTING POINT DATA
  check  it_vmv_app236-atnam(4) eq  'P_RP'.

  data: l_progress(2)  type n.
  clear: G_SHOP_DATE_APP236,
         G_ACT_DATE_APP236,
         G_SERIAL_APP236.
  do  27 times.
    move   sy-index  to   l_progress.
    concatenate 'P_RP' l_progress '_SHOP_DATE'
      into G_SHOP_DATE_APP236.
    concatenate 'P_RP' l_progress '_SERIAL'
      into G_SERIAL_APP236.
    concatenate 'P_RP' l_progress '_ACTUAL_DATE'
      into G_ACT_DATE_APP236.

    if it_vmv_app236-atnam  eq  G_SHOP_DATE_APP236.
      read table it_wip_app236  with key progress =  l_progress.
      if sy-subrc eq 0.
        call function 'CONVERSION_EXIT_DATEX_INPUT'
             exporting
                  input  = it_vmv_app236-atwrt
             importing
                  output = it_wip_app236-shop_dat.
*        MOVE IT_VMV_APP236-atwrt TO  IT_WIP_APP236-shop_dat.
        modify  it_wip_app236  index sy-tabix.
      endif.
      exit.
    endif.
    if it_vmv_app236-atnam  eq  G_ACT_DATE_APP236.
      read table it_wip_app236  with key progress =  l_progress.
      if sy-subrc eq 0.
        move it_vmv_app236-atwrt to  it_wip_app236-act_dat.
        modify  it_wip_app236 index sy-tabix.
      endif.
      exit.
    endif.

    if it_vmv_app236-atnam  eq  G_SERIAL_APP236.
      read table it_wip_app236  with key progress =  l_progress.
      if sy-subrc eq 0.
        move it_vmv_app236-atwrt to  it_wip_app236-serial.
        modify  it_wip_app236 index sy-tabix.
      endif.
      exit.
    endif.

  enddo.

endform.                    " vmdata_to_screen_field
*&---------------------------------------------------------------------*
*&      Form  FILED_CHANGING
*&---------------------------------------------------------------------*
*       Moving V/M's Date Type Data to a parameter
*----------------------------------------------------------------------*
*      <--P_ST_APP236_MODEL  text
*----------------------------------------------------------------------*
form filed_changing_app236 changing p_st_app236.
*  CALL FUNCTION 'CONVERSION_EXIT_DATEX_INPUT'
*    EXPORTING
*      INPUT = IT_VMV_APP236-atwrt
*    IMPORTING
*      OUTPUT = p_ST_APP236 .

  move  it_vmv_app236-atwrt     to  p_st_app236.

endform.                    " FILED_CHANGING
*&---------------------------------------------------------------------*
*&      Form  KEY_FIELD_ATTR
*&---------------------------------------------------------------------*
*       Setting Required Keys' Attribute
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form key_field_attr_app236.
  check  sy-ucomm eq 'ATTR'.

  clear: st_app236, it_wip_app236, st_code_app236, st_iss_app236.
  refresh: it_wip_app236.

  case st_key_app236-inqopt.
    when  'VEH'.
      g_attr_app236 = '1'.
    when  'VIN'.
      g_attr_app236 = '2'.
    when  'ENG'.
      g_attr_app236 = '3'.
    when  'TMN'.
      g_attr_app236 = '4'.
  endcase.

endform.                    " KEY_FIELD_ATTR
*&---------------------------------------------------------------------*
*&      Form  equi_master_check
*&---------------------------------------------------------------------*
*       Checking V/M's Number
*----------------------------------------------------------------------*
*      -->P_G_EQUNR  text
*      -->P_G_EQUICHK  text
*----------------------------------------------------------------------*
form equi_master_check_app236 using    p_equnr
                                       p_equichk.
  clear p_equichk.
  select  single  eqktx  into  st_iss_app236-eqktx
    from  eqkt
    where equnr  eq  p_equnr
      and spras  eq  sy-langu.
  if sy-subrc ne 0.
    p_equichk = 'N'.
    message  s000 with 'Vehicle Master Not Found !'.
  endif.
endform.                    " equi_master_check
*&---------------------------------------------------------------------*
*&      Form  vin_number_search
*&---------------------------------------------------------------------*
*       Searching Data By V/M NO.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form vin_number_search_app236.

  if  st_app236-vin  is  initial.
    message s000  with 'Input VIN No !'.
    g_crsr_fld_app236 = 'ST_APP236-VIN'.
  endif.
  if st_key_app236-inqopt eq st_code_app236-inqopt  and
     st_app236-vin  eq st_code_app236-vin   .
    exit.
  endif.

  move   st_key_app236-inqopt   to  st_code_app236-inqopt.
  move   st_app236-vin      to  st_code_app236-vin.

  perform  itab_and_variable_init_app236.

  perform  conversion_atinn_call  using  'P_VIN'
                                          G_VIN_APP236.

  select   single   objek
    into   g_equnr_app236
    from   ausp
    where  atinn  eq   G_VIN_APP236
      and  atwrt  eq   st_app236-vin.

  if sy-subrc ne 0.
    message s000  with 'VIN number not found !'.
    clear st_app236.
    move  st_code_app236-vin to  st_app236-vin.
    exit.
  endif.

  perform  equi_master_check_app236  using  g_equnr_app236
                                            g_equichk_app236.

  check  g_equichk_app236  eq  space.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object    = g_equnr_app236
       tables
            val_table = it_vmv_app236.

  if sy-subrc ne 0.
    message s003 with g_equnr_app236 'Vehicle History Not found!'.
    exit.
  endif.

  describe table it_vmv_app236  lines  it_lines_app236.

  check  it_lines_app236 > 0.
  clear st_app236.

* PROGRESS DATA TABLE
  perform  create_table_it_wip_app236.  "PROGRESS INITIAL LOAD
* VEHICLE DATA
  perform  general_veh_history_app236.  "move data to screen

* 219 option table made
  perform  create_219_option_table.
* Order table made
  perform  create_order_table.
* Airbag  table made
  perform  create_airbag_table.

  move-corresponding  st_app236  to  st_code_app236.
  move-corresponding  st_iss_app236  to  st_code_app236.
  move-corresponding  st_key_app236  to  st_code_app236.

endform.                    " vin_number_search
*&---------------------------------------------------------------------*
*&      Form  engine_number_search
*&---------------------------------------------------------------------*
*       Searching Data By a Engine No.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form engine_number_search_app236.
  data  l_eng_lines type i.
  if  st_app236-engno  is  initial.
    message s000  with 'Input Engine No !'.
    g_crsr_fld_app236 = 'ST_APP236-ENGNO'.
  endif.
  if st_key_app236-inqopt eq st_code_app236-inqopt      and
     st_app236-engno  eq st_code_app236-engno   .
    exit.
  endif.

  move   st_key_app236-inqopt   to  st_code_app236-inqopt.
  move   st_app236-engno    to  st_code_app236-engno.

  perform  itab_and_variable_init_app236.
*
  perform  conversion_atinn_call  using  'P_ENGINE_NO'
                                          G_ENGNO_APP236.
* DUP ENGINE CHECK ------------------------------------
  refresh: it_eng_app236.  clear it_eng_app236.
  select   objek  atwrt
    into   (IT_ENG_APP236-objek, IT_ENG_APP236-engno)
                                  " The biggest SEQUENCE_CODE
    from   ausp
    where  atinn  eq   G_ENGNO_APP236
      and  atwrt  eq   st_app236-engno.
    append it_eng_app236.
  endselect.

  describe table it_eng_app236  lines  l_eng_lines.

  if  l_eng_lines is initial.
    message s000  with 'Engine number not found !'.
    clear st_app236.
    move st_code_app236-engno  to  st_app236-engno.
    exit.
  endif.
  sort it_eng_app236   by  objek .
  read table it_eng_app236  index  1.

  g_equnr_app236 = it_eng_app236-objek.

  if l_eng_lines > 1.
    loop at it_eng_app236.
      case  sy-tabix.
        when 1.
          st_iss_app236-dupeng1 = it_eng_app236-objek.
        when 2.
          st_iss_app236-dupeng2 = it_eng_app236-objek.
        when 3.
          st_iss_app236-dupeng3 = it_eng_app236-objek.
        when 4.
          st_iss_app236-dupeng4 = it_eng_app236-objek.
        when others.
          exit.
      endcase.
    endloop.
  endif.
*------------------------------------------------------
  perform  equi_master_check_app236  using  g_equnr_app236
                                     g_equichk_app236.

  check  g_equichk_app236  eq  space.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object    = g_equnr_app236
       tables
            val_table = it_vmv_app236.

  if sy-subrc ne 0.
    message s003 with g_equnr_app236 'Vehicle History Not found!'.
    refresh: it_wip_app236.
    exit.
  endif.

  describe table it_vmv_app236  lines  it_lines_app236.

  check  it_lines_app236 > 0.
  clear st_app236.
* PROGRESS DATA TABLE
  perform  create_table_it_wip_app236.  "PROGRESS INITIAL LOAD

* VEHICLE DATA
  perform  general_veh_history_app236.  "move data to screen

* 219 option table made
  perform  create_219_option_table.
* Order table made
  perform  create_order_table.
* Airbag  table made
  perform  create_airbag_table.

  move-corresponding  st_app236  to  st_code_app236.
  move-corresponding  st_iss_app236  to  st_code_app236.
  move-corresponding  st_key_app236  to  st_code_app236.

endform.                    " engine_number_search
*&---------------------------------------------------------------------*
*&      Form  tm_number_search
*&---------------------------------------------------------------------*
*       Searching Data By a T/M No.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form tm_number_search_app236.
  if  st_app236-vin  is  initial.
    message s000  with 'Input TM No !'.
    g_crsr_fld_app236 = 'ST_APP236-TMNO'.
  endif.
  if st_key_app236-inqopt eq st_code_app236-inqopt    and
     st_app236-tmno  eq st_code_app236-tmno   .
    exit.
  endif.

  move   st_key_app236-inqopt   to  st_code_app236-inqopt.
  move   st_app236-tmno     to  st_code_app236-tmno.

  perform  itab_and_variable_init_app236.

  perform  conversion_atinn_call  using  'P_TM_NO'
                                          g_tmno_APP236.

  select   single   objek
    into   g_equnr_app236                " The biggest
    from   ausp
    where  atinn  eq   g_tmno_APP236
      and  atwrt  eq   st_app236-tmno.

  if sy-subrc ne 0.
    message s000  with 'T/M number not found !'.
    clear st_app236.
    move st_code_app236-tmno  to  st_app236-tmno.
    exit.
  endif.

  perform  equi_master_check_app236 using  g_equnr_app236
                                           g_equichk_app236.

  check  g_equichk_app236  eq  space.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object    = g_equnr_app236
       tables
            val_table = it_vmv_app236.

  if sy-subrc ne 0.
    message s003 with g_equnr_app236 'Vehicle History Not found!'.
    exit.
  endif.

  describe table it_vmv_app236  lines  it_lines_app236.

  check  it_lines_app236 > 0.
  clear st_app236.
* PROGRESS DATA TABLE
  perform  create_table_it_wip_app236.  "PROGRESS INITIAL LOAD
* VEHICLE DATA
  perform  general_veh_history_app236.  "move data to screen

* 219 option table made
  perform  create_219_option_table.
* Order table made
  perform  create_order_table.
* Airbag  table made
  perform  create_airbag_table.

  move-corresponding  st_app236  to  st_code_app236.
  move-corresponding  st_iss_app236  to  st_code_app236.
  move-corresponding  st_key_app236  to  st_code_app236.

endform.                    " tm_number_search

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ATINN_CALL
*&---------------------------------------------------------------------*
*       Characteristic Name Conversion To Internal No. of Char
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_L_MODEL  text
*----------------------------------------------------------------------*
form conversion_atinn_call using    p_value  "Characteristic Name
                                    p_atinn. "internal no of char

  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_value
       importing
            output = p_atinn.

endform.                    " CONVERSION_ATINN_CALL
*&---------------------------------------------------------------------*
*&      Form  CREATE_TABLE_IT_WIP_APP236
*&---------------------------------------------------------------------*
*       Setting RP's No.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_table_it_wip_app236.
  clear it_wip_app236.
  do  27 times.
    move  sy-index   to   it_wip_app236-progress.
    append  it_wip_app236.
  enddo.

endform.                    " CREATE_TABLE_IT_WIP_APP236
*&---------------------------------------------------------------------*
*&      Form  GENERAL_VEH_HISTORY
*&---------------------------------------------------------------------*
*       Setting Screen's Fields By Internal Table's Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form general_veh_history_app236.

  loop at it_vmv_app236.
    perform  vmdata_to_screen_field_app236.
  endloop.

endform.                    " GENERAL_VEH_HISTORY
*&---------------------------------------------------------------------*
*&      Form  219_OPTION_DISPLAY
*&---------------------------------------------------------------------*
*       Calling a Screen For 219 Option Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form 219_option_display.

  read table it_219_app236 index 1.
  if sy-subrc eq 0.
    call screen  110  starting at  48  8
                      ending at 111 23.
    exit.
  else.
    message s000 with 'There is no 219 option information !'.
  endif.

endform.                    " 219_OPTION_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  option_219_value
*&---------------------------------------------------------------------*
*       Searching 219's Values By Table 'ZTBM_219_VALUE'
*----------------------------------------------------------------------*
*      -->P_IT_WO_MODEL  text
*      -->P_L_COL  text
*      -->P_IT_WO_ATWRT  text
*      <--P_IT_219_APP236_VAL  text
*      <--P_IT_219_APP236_VALTX  text
*----------------------------------------------------------------------*
form option_219_value using    p_model
                               p_col
                               p_val
                      changing p_valtx.
************************************************************************
* Because of the Changed Table Relationship, Program Source was changed
* By Tonkey On 01/27/2004
* Reference Table : ZTBM_ABXOPVDT
************************************************************************
* Request No. :
************************************************************************
  select single ab~vanm
    into p_valtx
    from ztbm_abxopvdt as ab
           inner join ztpp_veh_model as vh on ab~carx = vh~model02
    where vh~model = p_model and
          ab~clno  = p_col   and
          ab~valu  = p_val   .
*  select single code_name1  into  p_valtx
*    from ztbm_219_value
*    where  model  eq  p_model
*      and  serial eq  p_col
*      and  value  eq  p_val.

endform.                    " option_219_value
*&---------------------------------------------------------------------*
*&      Form  order_list_display
*&---------------------------------------------------------------------*
*       Call a Screen For Order List Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form order_list_display.
*
  read table IT_UCPART_APP236 index 1.

  if sy-subrc eq 0.
    call screen  120  starting at  50  8
                        ending at 111 23.
    exit.
  else.
    message s000 with 'There is no Order information !'.
  endif.
*
endform.                    " order_list_display
*&---------------------------------------------------------------------*
*&      Form  UNIQUE_PART_DISPLAY
*&---------------------------------------------------------------------*
*       Display of Unique Part's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form unique_part_display.

  refresh  IT_PART_APP236.  clear IT_PART_APP236.

  loop at IT_UCPART_APP236  where  ucgub eq 'U'.
    move-corresponding  IT_UCPART_APP236 to IT_PART_APP236.
    append IT_PART_APP236.
  endloop.

  g_part_APP236 = 'U'.  "Next Color part

endform.                    " UNIQUE_PART_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_DISPLAY
*&---------------------------------------------------------------------*
*       Display of Color Part's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form color_part_display.

  refresh  IT_PART_APP236.  clear IT_PART_APP236.

  loop at IT_UCPART_APP236  where  ucgub eq 'C'.
    move-corresponding  IT_UCPART_APP236 to IT_PART_APP236.
    append IT_PART_APP236.
  endloop.

  g_part_APP236 = 'C'. "next unique part

endform.                    " COLOR_PART_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  airbag_display
*&---------------------------------------------------------------------*
*       Calling a Screen For AirBag Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form airbag_display.

  read table it_abag_app236 index 1.
  if sy-subrc eq 0.
    call screen  130  starting at  48  8
                      ending at 111 23.
    exit.
  else.
    message s000 with 'There is no Airbag information !'.
  endif.

endform.                    " airbag_display
*&---------------------------------------------------------------------*
*&      Form  create_219_option_table
*&---------------------------------------------------------------------*
*       Searching 219 Option Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_219_option_table.
  check not  st_app236-won  is initial.
  clear G_CUOBF_APP236.

  select single matnr into G_CUOBF_APP236
    from mara
    where matnr eq st_app236-won.
  move st_app236-won to G_CUOBF_APP236.

  check not G_CUOBF_APP236 is  initial.

  call function 'Z_FPP_HANDLING_MASTER'
    exporting
      object             = G_CUOBF_APP236
*   MODE               = 'R'
*   CTYPE              =
*   DISPLAY            = 'D'
    tables
      val_table          = IT_WO_APP236
* EXCEPTIONS
*   NO_DATA            = 1
*   ERROR_MODE         = 2
*   ERROR_OBJECT       = 3
*   OTHERS             = 4
            .

  check sy-subrc eq 0.
  check not st_app236-model is initial.

* Create 219 option table 'IT_219_APP236' ---
  refresh: it_219_app236. clear it_219_app236.
************************************************************************
* Because of the Changed Table Relationship, Program Source was changed
* By Tonkey On 01/27/2004
* Reference Table : ZTBM_ABXOPVDT
************************************************************************
* Request No. :
************************************************************************
  select distinct ab~clno ab~clnm
    into (it_219_app236-col, it_219_app236-coltx)
    from ztbm_abxopvdt as ab
           inner join ztpp_veh_model as vh on ab~carx = vh~model02
    where vh~model = st_app236-model .
    append it_219_app236. clear it_219_app236.
  endselect.

*  select name_219 desc_219
*      into (IT_219_APP236-col, IT_219_APP236-coltx)
*      from ztbm_219_desc
*      where model eq ST_APP236-model.
*    append IT_219_APP236.  clear IT_219_APP236.
*  endselect.

  data: l_col(3)  type  n.
  describe  table it_219_app236  lines  tc110-lines.
  check tc110-lines > 0.
* 219 option value update
  loop at IT_WO_APP236  where atnam(5) eq 'P_219'
                   and atwrt    ne '-'.
    move  IT_WO_APP236-atnam+6(3)  to  l_col.
    read table  it_219_app236  with key col = l_col.
    if sy-subrc eq 0.
      move  IT_WO_APP236-atwrt   to  it_219_app236-val.
      perform option_219_value using     st_app236-model
                                         l_col
                                         it_219_app236-val
                               changing  it_219_app236-valtx.
      modify it_219_app236  index  sy-tabix.
    endif.
  endloop.
* no value record delete
  delete it_219_app236  where  valtx eq space.

endform.                    " create_219_option_table
*&---------------------------------------------------------------------*
*&      Form  create_order_table
*&---------------------------------------------------------------------*
*       Searching ALC' Unique & Color Part Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_order_table.
  data: l_col(3)  type  n.
*
  clear:   IT_UCPART_APP236, IT_PART_APP236.
*
  check not  st_app236-won  is initial.
  clear G_CUOBF_APP236.
  select single matnr  into  G_CUOBF_APP236
     from  mara
     where matnr eq st_app236-won.

  check not G_CUOBF_APP236 is  initial.
  refresh: IT_WO_APP236.
* W/O Selection ----------------------------------------------
  call function 'Z_FPP_HANDLING_MASTER'
    exporting
      object             = G_CUOBF_APP236
*   MODE               = 'R'
*   CTYPE              =
*   DISPLAY            = 'D'
    tables
      val_table          = IT_WO_APP236
* EXCEPTIONS
*   NO_DATA            = 1
*   ERROR_MODE         = 2
*   ERROR_OBJECT       = 3
*   OTHERS             = 4
            .

  check sy-subrc eq 0.


  sort IT_WO_APP236 by atnam.
  loop at IT_WO_APP236  where atnam(5) eq 'P_ALC'
                   and atwrt    ne ' '.
    move  IT_WO_APP236-atnam+8(3)  to  l_col.
    move  IT_WO_APP236-atnam+6(1)  to  IT_UCPART_APP236-ucgub.
    move  l_col             to  IT_UCPART_APP236-col.
    move  IT_WO_APP236-atwrt       to  IT_UCPART_APP236-code.
    append  IT_UCPART_APP236.
  endloop.
* table for display
  loop at IT_UCPART_APP236  where  ucgub eq 'U'.
    move-corresponding  IT_UCPART_APP236  to  IT_PART_APP236.
    append IT_PART_APP236.
  endloop.

  g_part_APP236 = 'U'.

* itab create for tabstrip
  loop at IT_UCPART_APP236  where  ucgub eq 'U'.
    move-corresponding  IT_UCPART_APP236  to  it_upart_app236.
    append it_upart_app236.
  endloop.
  loop at IT_UCPART_APP236  where  ucgub eq 'C'.
    move-corresponding  IT_UCPART_APP236  to  it_cpart_app236.
    append it_cpart_app236.
  endloop.
*
endform.                    " create_order_table
*&---------------------------------------------------------------------*
*&      Form  create_airbag_table
*&---------------------------------------------------------------------*
*       getting Airbag Inf. From a Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_airbag_table.

  refresh: it_abag_app236.  clear it_abag_app236.
  loop at it_vmv_app236  where atnam(8) eq 'P_AIRBAG'.
    move  it_vmv_app236-atnam   to  it_abag_app236-airbag.
    move  it_vmv_app236-atwrt   to  it_abag_app236-code.
    append  it_abag_app236.
  endloop.

endform.                    " create_airbag_table
*&---------------------------------------------------------------------*
*&      FORM  itab_and_variable_init_APP236
*&---------------------------------------------------------------------*
*       Initialization of Internal Tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form itab_and_variable_init_app236.

  refresh: it_wip_app236,
           it_219_app236,
           it_abag_app236,
           IT_UCPART_APP236,
           IT_PART_APP236.
  refresh: it_vmv_app236, IT_WO_APP236.
  refresh: it_upart_app236, it_cpart_app236.
  clear:   st_iss_app236.

endform.                    " itab_and_variable_init
*&---------------------------------------------------------------------*
*&      Form  export_to_excel
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form export_to_excel.

endform.                    " export_to_excel
