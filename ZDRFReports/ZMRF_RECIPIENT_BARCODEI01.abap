*----------------------------------------------------------------------*
*   INCLUDE ZMRF_RECIPIENT_BARCODEI01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  OK_CODE = OKCODE.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&spwizard: input module for tc 'TC_RECP'. do not change this line!
*&spwizard: modify table
MODULE TC_RECP_MODIFY INPUT.
  IF CHANGE_MODE EQ 'X'.
    PERFORM DATA_INSERT_CHECK.
  ELSE.
    MOVE-CORRESPONDING ZTRF_RECIPIENT TO G_TC_RECP_WA.
    MODIFY G_TC_RECP_ITAB
      FROM G_TC_RECP_WA
      INDEX TC_RECP-CURRENT_LINE.
  ENDIF.
ENDMODULE.

*&spwizard: input module for tc 'TC_RECP'. do not change this line!
*&spwizard: mark table
MODULE TC_RECP_MARK INPUT.
  IF TC_RECP-LINE_SEL_MODE = 1 AND
     G_TC_RECP_WA-FLAG = 'X'.
    LOOP AT G_TC_RECP_ITAB INTO G_TC_RECP_WA
      WHERE FLAG = 'X'.
      G_TC_RECP_WA-FLAG = ''.
      MODIFY G_TC_RECP_ITAB
        FROM G_TC_RECP_WA
        TRANSPORTING FLAG.
    ENDLOOP.
    G_TC_RECP_WA-FLAG = 'X'.
  ENDIF.
  MODIFY G_TC_RECP_ITAB
    FROM G_TC_RECP_WA
    INDEX TC_RECP-CURRENT_LINE
    TRANSPORTING FLAG.
ENDMODULE.

*&spwizard: input module for tc 'TC_RECP'. do not change this line!
*&spwizard: process user command
MODULE TC_RECP_USER_COMMAND INPUT.
  OKCODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_RECP'
                              'G_TC_RECP_ITAB'
                              'FLAG'
                     CHANGING OKCODE.
  SY-UCOMM = OKCODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9900 INPUT.
  OK_CODE = OKCODE.
  CLEAR OKCODE.
  CASE OK_CODE.
    WHEN 'PRINT'.
      PERFORM PRINT_PROCESS.
    WHEN 'NLINE'.
      PERFORM NEW_LINE.
    WHEN 'DISP'.
      PERFORM DISP_MODE.
    WHEN 'SAVE'.
      PERFORM SAVE_PROCESS.
    WHEN 'ENTR'.
      PERFORM ENTER_PROCESS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9900  INPUT
