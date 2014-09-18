*----------------------------------------------------------------------*
***INCLUDE ZAPP221M_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit command.
*----------------------------------------------------------------------*
module exit_1203 input.
  check  sy-ucomm eq 'BACK' or sy-ucomm eq 'EXIT'.
  if g_editdata_1203  ne 'X'.
    leave  to  screen  0.
  endif.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            textline1 = 'Nation Data has been changed.'
            textline2 = 'Save data?'
            titel     = 'Save Confirm'
       importing
            answer    = G_ANSWER_1203.

  case  G_ANSWER_1203.
    when  'J'.
      perform  nation_data_save_1203.
      leave to screen 0.
    when  'N'.
      leave to screen 0.
    when  'A'.
  endcase.
endmodule.                 " exit_100  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1203  INPUT
*&---------------------------------------------------------------------*
*       Setting System's Commands
*----------------------------------------------------------------------*
module user_command_1203 input.

  case  sy-ucomm.
    when  'ENTER'.
*     Read Nation Information
      perform  nation_display_1203.
    when  'TOGGLE'.
*     Change Toggle's Mode
      perform  display_change_toggle_1203.
    when  'PREVN'.
*     Display The Previous Nation Information
      perform  prev_nation_display_1203.
    when  'NEXTN'.
*     Display The Next Nation Information
      perform  next_nation_display_1203.
    when  'SAVE'.          "Change data save
*     Save The Data
      perform  nation_data_save_1203.
    when  'ISRT'.
*     Insert New Data
      perform  nation_data_insert_1203.
    when  'TRANS'.
*     Transport Data To Legacy Sys.
      perform  transport_to_alc_1203.
    when  'BACK' or 'EXIT'.
      perform exit_process_1203.
  endcase.

endmodule.                 " user_command_1203  INPUT
*&---------------------------------------------------------------------*
*&      Module  drive_type  INPUT
*&---------------------------------------------------------------------*
*       Handling Error - Drive Type
*----------------------------------------------------------------------*
module drive_type_1203 input.
  if is_app221-drive  eq  'L' or is_app221-drive  eq 'R'.
  else.
    message e000  with  'Drive Type is Mismatch'.
  endif.

  g_editdata_1203  =  'X'.

endmodule.                 " drive_type  INPUT
*&---------------------------------------------------------------------*
*&      Module  weather_type  INPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Editor Mode
*----------------------------------------------------------------------*
module weather_type_1203 input.

  g_editdata_1203  =  'X'.

endmodule.                 " weather_type  INPUT
*&---------------------------------------------------------------------*
*&      Module  region_gubun  INPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Editor Mode
*----------------------------------------------------------------------*
module region_gubun_1203 input.

  g_editdata_1203  =  'X'.

endmodule.                 " region_gubun  INPUT
*&---------------------------------------------------------------------*
*&      Module  spec_gubun  INPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Editor Mode
*----------------------------------------------------------------------*
module spec_gubun_1203 input.

  g_editdata_1203  =  'X'.

endmodule.                 " spec_gubun  INPUT
*&---------------------------------------------------------------------*
*&      Form  next_nation_display
*&---------------------------------------------------------------------*
*       Displaying The Next Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form next_nation_display_1203.
  data: l_seq  type  i.
  read  table  it_app221  with key nation = is_app221-nation.

  check sy-subrc eq 0.
  move  it_app221-seq  to  l_seq.
  l_seq = l_seq + 1.
  if l_seq  >  g_seq_1203.
    l_seq = g_seq_1203.
    message s000 with 'Last Nation Code.'.
  endif.
  read  table  it_app221  with key seq = l_seq.
  if sy-subrc eq 0.
    move-corresponding  it_app221   to  is_app221.
  endif.

endform.                    " next_nation_display
*&---------------------------------------------------------------------*
*&      Module  FIELD_TOUCH  INPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Editor Mode
*----------------------------------------------------------------------*
module field_touch_1203 input.

  g_EDITDATA_1203  =  'X'.

endmodule.                 " FIELD_TOUCH  INPUT
*&---------------------------------------------------------------------*
*&      Form  prev_nation_display
*&---------------------------------------------------------------------*
*       Displaying The Previous Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form prev_nation_display_1203.
  data: l_seq  type  i.
  read  table  it_app221  with key nation = is_app221-nation.

  check sy-subrc eq 0.
  move  it_app221-seq  to  l_seq.
  l_seq = l_seq - 1.
  if l_seq  <   1.
    l_seq =  1.
    message s000 with 'First Nation Code.'.
  endif.
  read  table  it_app221  with key seq = l_seq.
  if sy-subrc eq 0.
    move-corresponding  it_app221   to  is_app221.
  endif.

endform.                    " prev_nation_display
