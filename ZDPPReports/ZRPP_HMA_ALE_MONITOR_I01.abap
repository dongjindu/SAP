*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_I01                                        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* MODULE  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA LV_CNT TYPE P.
  DATA LV_ANS(1).
  CLEAR OK_CODE.
  OK_CODE = SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REF'.
      PERFORM P2100_CLEAR_DATA.
      PERFORM P2000_GET_DATA.
      PERFORM P1000_CREATE_OBJECT .
*      PERFORM REFRESH_DISPLAY .
    WHEN 'PROC'.
      PERFORM P3000_CALL_RBDPROC.

    WHEN 'DISPLAY'.
      PERFORM P3100_CALL_RBDMON00.
       CALL SCREEN 0200.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT

*----------------------------------------------------------------------*
* MODULE  EXIT  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
*  PERFORM P1110_DESTROY_OBJECT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  pai_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE OK_CODE.
    WHEN 'DISPLAY'.
      PERFORM SHOW_IDOC_DATA USING 'I'.
    WHEN 'LINKAGE'.
      PERFORM SHOW_IDOC_DATA USING 'R'.
    WHEN 'SHOWERR'.
      PERFORM SHOW_IDOC_DATA USING 'E'.
    WHEN 'OBJECT'.
      PERFORM GET_IDOC_OBJECTS.
      WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      IF NOT G_ALV_CONTAINER IS INITIAL.
        " destroy alv container (detroys contained alv control, too)
        CALL METHOD G_ALV_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
      ENDIF.
      FREE: G_GRID1, G_ALV_CONTAINER.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR OK_CODE.
ENDMODULE.                 " pai_0200  INPUT
