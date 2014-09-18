***INCLUDE ZACO_AALA_CHANGE_IO .
* OUTPUT MODULE FOR TABSTRIP 'TC2': SETS ACTIVE TAB
MODULE TC2_ACTIVE_TAB_SET OUTPUT.
  TC2-ACTIVETAB = G_TC2-PRESSED_TAB.
  CASE G_TC2-PRESSED_TAB.
    WHEN C_TC2-TAB1.
      G_TC2-SUBSCREEN = '0101'.
    WHEN C_TC2-TAB2.
      G_TC2-SUBSCREEN = '0102'.
    WHEN C_TC2-TAB3.
      G_TC2-SUBSCREEN = '0103'.
    WHEN C_TC2-TAB4.
      G_TC2-SUBSCREEN = '0104'.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.

* INPUT MODULE FOR TABSTRIP 'TC2': GETS ACTIVE TAB
MODULE TC2_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TC2-TAB1.
      G_TC2-PRESSED_TAB = C_TC2-TAB1.
    WHEN C_TC2-TAB2.
      G_TC2-PRESSED_TAB = C_TC2-TAB2.
    WHEN C_TC2-TAB3.
      G_TC2-PRESSED_TAB = C_TC2-TAB3.
    WHEN C_TC2-TAB4.
      G_TC2-PRESSED_TAB = C_TC2-TAB4.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC24_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TC24_PAI input.

endmodule.                 " TC24_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC23_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TC23_PAI input.

endmodule.                 " TC23_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC22_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TC22_PAI input.

  DATA: L_ERROR.
  ClEAR L_ERROR.

  PERFORM VALIDATE_CHANGE USING IT_MIP TC22-CURRENT_LINE L_ERROR.
  IF L_ERROR NE 'X'.
    MODIFY IT_MIP INDEX TC22-CURRENT_LINE.
  ENDIF.

endmodule.                 " TC22_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC21_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module TC21_PAI input.
 modify it_parts index tc21-current_line.
endmodule.                 " TC21_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form VALIDATE_CHANGE USING P_MIP LIKE IT_MIP
                           P_LINE
                           P_ERROR.
   DATA: L_TEXT(50).
   DATA: WA_MIP LIKE IT_MIP.
   DATA: L_AMOUNT LIKE IT_MIP-MCOST.

   READ TABLE IT_MIP INTO WA_MIP INDEX P_LINE.

   IF WA_MIP-ACOST NE P_MIP-ACOST.
      S_DATA_CHANGED  = 'X'.
*     P_MIP-MCOST    = P_MIP-WRBTR - P_MIP-ACOST.
      P_MIP-WRBTR     = P_MIP-MCOST + P_MIP-ACOST.
      P_MIP-FSTTR     = P_MIP-WRBTR * P_MIP-FSTPP / 100.
      P_MIP-SNDTR     = P_MIP-WRBTR * P_MIP-SNDPP / 100.

   ELSEIF WA_MIP-MCOST NE P_MIP-MCOST.
      S_DATA_CHANGED  = 'X'.
*     P_MIP-ACOST    = P_MIP-WRBTR - P_MIP-MCOST.
      P_MIP-WRBTR     = P_MIP-MCOST + P_MIP-ACOST.
      P_MIP-FSTTR     = P_MIP-WRBTR * P_MIP-FSTPP / 100.
      P_MIP-SNDTR     = P_MIP-WRBTR * P_MIP-SNDPP / 100.


   ENDIF.

   IF P_MIP-FSTPP NE WA_MIP-FSTPP.
     S_DATA_CHANGED = 'X'.
     P_MIP-FSTTR    = P_MIP-WRBTR * P_MIP-FSTPP / 100.

   ENDIF.

   IF P_MIP-SNDPP NE WA_MIP-SNDPP.
     S_DATA_CHANGED = 'X'.
     P_MIP-SNDTR    = P_MIP-WRBTR * P_MIP-SNDPP / 100.
   ENDIF.

   IF P_MIP-AAPRS NE WA_MIP-AAPRS.
     S_DATA_CHANGED = 'X'.
   ENDIF.

   L_AMOUNT = P_MIP-FSTTR + P_MIP-SNDTR.

   IF P_MIP-ACOST GT P_MIP-WRBTR OR
      P_MIP-MCOST GT P_MIP-WRBTR OR
      P_MIP-AAPRS GT P_MIP-WRBTR OR
      P_MIP-FSTTR GT P_MIP-WRBTR OR
      P_MIP-SNDTR GT P_MIP-WRBTR OR
      L_AMOUNT    GT P_MIP-WRBTR.
      L_TEXT  = 'THE VALUE CAN NOT BE GREATER THAN TOTAL VALUE'.
      P_ERROR = 'X'.
      MESSAGE I000 WITH L_TEXT.
   ENDIF.
endform.                    " VALIDATE_CHANGE
