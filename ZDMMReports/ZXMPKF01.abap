*----------------------------------------------------------------------*
***INCLUDE ZXMPKF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  modify_header_attribute
*&---------------------------------------------------------------------*
*       Modify Header Attribute
*----------------------------------------------------------------------*
*      -->IF_MODE  Update Mode
*----------------------------------------------------------------------*
FORM modify_header_attribute  USING    if_mode.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'PKHD-ZFEEDER'.
        CASE gs_pkhd-rksta.
          WHEN 'I' OR 'K' OR 'A'.   "Event-Driven,SumJC
            screen-active   = 1.
            CASE if_mode.
              WHEN 'X' OR '03'.
                screen-input    = 0.
              WHEN OTHERS.
                screen-input    = 1.
                screen-required = 1.
            ENDCASE.
          WHEN OTHERS.
            screen-active   = 0.
        ENDCASE.
        MODIFY SCREEN.

      WHEN 'PKHD-ZRHLH'.
        CASE gs_pkhd-rksta.
          WHEN 'I' OR 'K' OR 'A'.   "RH/LH
            screen-active   = 1.
            CASE if_mode.
              WHEN 'X' OR '03'.
                screen-input    = 0.
              WHEN OTHERS.
                screen-input    = 1.
            ENDCASE.
          WHEN OTHERS.
            screen-active   = 0.
        ENDCASE.
        MODIFY SCREEN.

      WHEN 'PKHD-ZZFSTP'.
        CASE gs_pkhd-rksta.
          WHEN 'I' OR 'K' OR 'A'.   "FEEDER STOP
            screen-active   = 1.
            CASE if_mode.
              WHEN 'X' OR '03'.
                screen-input    = 0.
              WHEN OTHERS.
                screen-input    = 1.
            ENDCASE.
          WHEN OTHERS.
            screen-active   = 0.
        ENDCASE.
        MODIFY SCREEN.

      WHEN 'PKHD-ZZEISBE' OR
           'PKHD-ZZTIM'.
        CASE gs_pkhd-rksta.
          WHEN 'I' OR 'M' OR 'A'.   "Event-Driven,SumJC
            screen-active   = 1.
            CASE if_mode.
              WHEN 'X' OR '03'.
                screen-input    = 0.
              WHEN OTHERS.
                screen-input    = 1.
            ENDCASE.
          WHEN OTHERS.
            screen-active   = 0.
        ENDCASE.
        MODIFY SCREEN.
      WHEN 'PKHD-MEINS'.
        CASE gs_pkhd-rksta.
          WHEN 'I' OR 'M' OR 'A'.   "Event-Driven,SumJC
            screen-active   = 1.
          WHEN OTHERS.
            screen-active   = 0.
        ENDCASE.
        MODIFY SCREEN.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " modify_header_attribute
*&---------------------------------------------------------------------*
*&      Form  move_field_value_pbo
*&---------------------------------------------------------------------*
*       Move Field Value
*----------------------------------------------------------------------*
FORM move_field_value_pbo .
  pkhd = gs_pkhd.
ENDFORM.                    " move_field_value_pbo
