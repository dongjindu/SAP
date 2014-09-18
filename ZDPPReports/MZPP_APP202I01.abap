*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP202I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_100 INPUT.
  data: return_code type i.

  CASE G_OK_CODE.
    when 'TEST'. call method g_tree->expand_node
      exporting node_key = 'New1'.
    WHEN 'BACK'. " Finish program
      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A001(ZMPP) WITH 'ERROR!!!!' .
        ENDIF.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
      ENDIF.
      LEAVE PROGRAM.
    WHEN '0101' .
  ENDCASE.

* CAUTION: clear ok code!
  CLEAR G_OK_CODE.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1101 INPUT.
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code.
    WHEN 'SCR_E'.
      LEAVE TO SCREEN 0 .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1101  INPUT
