***INCLUDE ZAFI99_CLAIM_SETTLEMENT_FM .
*&spwizard: output module for tc 'TC_100'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module TC_100_change_tc_attr output.
  describe table IT_OUTPUT lines TC_100-lines.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  DATA: SAVE_OKCODE LIKE SY-UCOMM.
*  case ok_code.
  SAVE_OKCODE = SY-UCOMM.
  CASE SAVE_OKCODE.
  when 'BACK' OR 'EXIT'.
    LEAVE PROGRAM.
  when 'POST'.
     Perform post_invocie.
  WHEN 'SELALL'.
     PERFORM SEL_ALL.
  WHEN 'DESELECT'.
     PERFORM DESEL_ALL.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  post_invocie
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_invocie.
   loop at it_output where check  = 'X'.
   endloop.
endform.                    " post_invocie
*&---------------------------------------------------------------------*
*&      Module  set_status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_status output.
   set pf-status '100'.
   SET TITLEBAR 'T_100'.
endmodule.                 " set_status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module EXIT input.
  LEAVE PROGRAM.
endmodule.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SEL_ALL.
  LOOP AT IT_OUTPUT.
    IT_OUTPUT-CHECK = 'X'.
    APPEND IT_OUTPUT.
  ENDLOOP.
endform.                    " SEL_ALL
*&---------------------------------------------------------------------*
*&      Form  DESEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DESEL_ALL.
  LOOP AT IT_OUTPUT.
    CLEAR: IT_OUTPUT-CHECK.
    APPEND IT_OUTPUT.
  ENDLOOP.

endform.                    " DESEL_ALL
