*----------------------------------------------------------------------*
*                                                                      *
*       Input-modules for infotype 9880                                *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ucommand  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ucommand INPUT.

  CASE psyst-ioper.
    WHEN 'INS' OR 'MOD' OR 'DIS' OR copy.
      CASE fcode.
        WHEN 'UPLOAD'.
          PERFORM attach_upload.
        WHEN 'DISPLAY'.
          PERFORM attach_display.
        WHEN 'UPD'.
          PERFORM save_attach.
      ENDCASE.

    WHEN 'DEL'.
      CASE fcode.
        WHEN 'UPD'.
          PERFORM dele_attach.
      ENDCASE.

  ENDCASE.

ENDMODULE.                 " ucommand  INPUT
