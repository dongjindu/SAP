*----------------------------------------------------------------------*
***INCLUDE ZAPP237M_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit_2107  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
module exit_2107 input.
  leave  to  screen 0.
endmodule.                 " exit_2107  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2107  INPUT
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
module user_command_2107 input.

  if zspp_app237-model ne sv_key-model  or
     zspp_app237-gubun ne sv_key-gubun.

    perform  made_delay_data_2107.

  endif.

  check  WA_DLY_LINES_2107  >  0.

  IT_DLS_2107[] = IT_DLY_2107[].

  if not zspp_app237-bodyno is initial.
    delete IT_DLS_2107  where  bodyno <  zspp_app237-bodyno.
  endif.
  delete IT_DLS_2107  where  dday <  zspp_app237-dday.

  describe table IT_DLS_2107  lines  zspp_app237-caunt.

  case zspp_app237-inqs.
    when  1.
      sort IT_DLS_2107  by  dday  descending.
    when  2.
      sort IT_DLS_2107  by  bodyno.
  endcase.

endmodule.                 " USER_COMMAND_2107  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_model_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_model_check_2107 input.

endmodule.                 " field_model_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_inqs_check  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module field_inqs_check_2107 input.

endmodule.                 " field_inqs_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_gubun_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_gubun_check_2107 input.

endmodule.                 " field_gubun_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_dday_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_dday_check_2107 input.

endmodule.                 " field_dday_check  INPUT
