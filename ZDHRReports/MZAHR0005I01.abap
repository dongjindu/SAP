*----------------------------------------------------------------------*
*   INCLUDE MZAHR0005I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  set screen 0. leave screen.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5000 INPUT.
  case sy-ucomm.
    when 'BUT1'.            " module & group
      perform get_data_from_pcp01.
    when 'BUT2'.            " code
      perform init_value_and_call_scr.
  endcase.
ENDMODULE.                 " USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  case sy-ucomm.
    when 'MODE'.
      clear it_stats. refresh it_stats.
      case w_modes.
        when 0.  w_modes = 1.
        when 1.  w_modes = 0.
          it_stats-fcode = 'SAVE'. append it_stats.
          it_stats-fcode = 'DELE'. append it_stats.
          it_stats-fcode = 'NEWN'. append it_stats.
      endcase.
    when 'DELE'.
      perform delete_line_pcp01.
    when 'DETL'.
      perform go_to_9100_screen.
    when 'ASOT'.
      get cursor field w_field.
      perform ascending_sort.
    when 'DSOT'.
      get cursor field w_field.
      perform descending_sort.
    when 'NEWN'.
      perform append_new_entries.
    when 'SAVE'.
      perform save_pcp01_data.
******
*    WHEN 'PICK'.
*      perform go_to_9100_screen.
  endcase.
  clear sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_intab  INPUT
*&---------------------------------------------------------------------*
MODULE modify_intab INPUT.
  if it_pcp01-ernam eq space.
    it_pcp01-erdat = sy-datum.
    it_pcp01-erzet = sy-uzeit.
    it_pcp01-ernam = sy-uname.
  else.
    it_pcp01-aedat = sy-datum.
    it_pcp01-aezet = sy-uzeit.
    it_pcp01-aenam = sy-uname.
  endif.
*      if it_pcp02-chkbx = 'X'.
*        w_tc_index = tc9000-current_line.
*    endif.

*
  modify it_pcp01 index tc9000-current_line.
ENDMODULE.                 " modify_intab  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_duplication  INPUT
*&---------------------------------------------------------------------*
MODULE check_duplication INPUT.
  clear zthr_pcp01.
  select single zgrup into zthr_pcp01-zgrup
    from zthr_pcp01 where zmodl = it_pcp01-zmodl
                      and zgrup = it_pcp01-zgrup.
*
  if sy-subrc = 0.
    message w001 with 'input value already exist'.
  else.
    modify it_pcp01 index tc9000-current_line.
  endif.
ENDMODULE.                 " check_duplication  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_module_value  INPUT
*&---------------------------------------------------------------------*
MODULE get_module_value INPUT.
  clear it_moval. refresh it_moval.
*
  clear zthr_pcp01.
  select distinct zmodl zmtxt
    into (zthr_pcp01-zmodl, zthr_pcp01-zmtxt)
    from zthr_pcp01 where erdat > '20000101'.
    it_moval-zmodl = zthr_pcp01-zmodl.
    it_moval-zmtxt = zthr_pcp01-zmtxt.
    append it_moval. clear it_moval.
  endselect.
*
  clear it_field. refresh it_field.
*
  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZMODL'.
  it_field-selectflag = 'X'.
  append it_field. clear it_field.

  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZMTXT'.
  it_field-selectflag = ' '.
  append it_field. clear it_field.
*
  clear: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = w_fname
       IMPORTING
            IND              = w_tabix
            SELECT_VALUE     = w_fldvl
       TABLES
            FIELDS           = it_field
            FULL_TABLE       = it_moval.
*
  w_zmodl = w_fldvl.
  perform screen_value_update.
ENDMODULE.                 " get_module_value  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_group_value  INPUT
*&---------------------------------------------------------------------*
MODULE get_group_value INPUT.
  clear it_grval. refresh it_grval.
*
  clear zthr_pcp01.
  select zgrup zgtxt into (zthr_pcp01-zgrup, zthr_pcp01-zgtxt)
    from zthr_pcp01 where zmodl = w_zmodl
                      and erdat > '20000101'.
    it_grval-zgrup = zthr_pcp01-zgrup.
    it_grval-zgtxt = zthr_pcp01-zgtxt.
    append it_grval. clear it_grval.
  endselect.
*
  clear it_field. refresh it_field.
*
  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGRUP'.
  it_field-selectflag = 'X'.
  append it_field. clear it_field.

  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGTXT'.
  it_field-selectflag = ' '.
  append it_field. clear it_field.
*
  clear: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = w_fname
       IMPORTING
            IND              = w_tabix
            SELECT_VALUE     = w_fldvl
       TABLES
            FIELDS           = it_field
            FULL_TABLE       = it_grval.
*
  w_zgrup = w_fldvl.
  perform screen_value_update_1.
ENDMODULE.                 " get_group_value  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  case sy-ucomm.
    when 'EXEC'.
      perform get_data_from_pcp02.
    when 'MODE'.
      clear it_stats. refresh it_stats.
      case w_modes.
        when 0. w_modes = 1.
        when 1. w_modes = 0.
          it_stats-fcode = 'SAVE'. append it_stats.
          it_stats-fcode = 'DELE'. append it_stats.
          it_stats-fcode = 'NEWN'. append it_stats.
      endcase.
    when 'DELE'.
      perform delete_line_pcp02.
    when 'NEWN'.
      perform append_new_lines.
    when 'SAVE'.
      perform save_pcp02_data.
  endcase.
  clear sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_duplication_code  INPUT
*&---------------------------------------------------------------------*
MODULE check_duplication_code INPUT.
  clear zthr_pcp02.
  select single zvald into zthr_pcp02-zvald
    from zthr_pcp02 where zmodl = it_pcp02-zmodl
                      and zgrup = it_pcp02-zgrup
                      and zcode = it_pcp02-zcode.
*
  if sy-subrc = 0.
    message w001 with 'input value already exist'.
  else.
*    if it_pcp02-chkbx = 'X'.
*        w_tc_index = tc9100-current_line.
*    endif.
    it_pcp02-zvald = sy-datum.
    modify it_pcp02 index tc9100-current_line.
  endif.
ENDMODULE.                 " check_duplication_code  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_pcp02  INPUT
*&---------------------------------------------------------------------*
MODULE modify_pcp02 INPUT.
  if it_pcp02-ernam eq space.
    it_pcp02-erdat = sy-datum.
    it_pcp02-erzet = sy-uzeit.
    it_pcp02-ernam = sy-uname.
  else.
    it_pcp02-aedat = sy-datum.
    it_pcp02-aezet = sy-uzeit.
    it_pcp02-aenam = sy-uname.
  endif.
*
  modify it_pcp02 index tc9100-current_line.
ENDMODULE.                 " modify_pcp02  INPUT
