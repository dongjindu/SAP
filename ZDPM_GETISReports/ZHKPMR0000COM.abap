*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0000COM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_WRITE_NO_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_WRITE_NO_MASK  USING    p_text_amount.

  CASE usr01-dcpfm.
    WHEN 'X'.    " Decimal point is period: N,NNN.NN
      PERFORM    set_change_symbol    USING p_text_amount ',' ' '.
      CONDENSE         p_text_amount    NO-GAPS.
    WHEN 'Y'.    " Decimal point is N NNN NNN,NN
      PERFORM    set_change_symbol    USING p_text_amount  ',' '.'.
      CONDENSE         p_text_amount    NO-GAPS.
    WHEN OTHERS. " Decimal point is comma: N.NNN,NN
      PERFORM    set_change_symbol    USING p_text_amount  '.' ' '.
      PERFORM    set_change_symbol    USING p_text_amount  ',' '.'.
      CONDENSE         p_text_amount    NO-GAPS.
  ENDCASE.

ENDFORM.                    " SET_WRITE_NO_MASK
*&---------------------------------------------------------------------*
*&      Form  SET_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TEXT_AMOUNT  text
*      -->P_0013   text
*      -->P_0014   text
*----------------------------------------------------------------------*
FORM SET_CHANGE_SYMBOL  USING    p_amount  p_from  p_to.

  DO.
    REPLACE  p_from   WITH   p_to  INTO    p_amount.
    IF  sy-subrc  <>    0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " SET_CHANGE_SYMBOL
