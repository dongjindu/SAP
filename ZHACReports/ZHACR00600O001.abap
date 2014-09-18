*&---------------------------------------------------------------------*
*&  Include           ZHARC00600O001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ALV_CL_PROCESS1  OUTPUT
*&---------------------------------------------------------------------*
MODULE ALV_CL_PROCESS1 OUTPUT.
 IF G_DOCKING_CONTAINER1 IS INITIAL.
    PERFORM ALV_CL_CONTROL1.
  ELSE.
*   ALV Refresh
    PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                            USING  G_ALV_GRID1
                                   G_REC_STABLE1.
  ENDIF.
ENDMODULE.                 " ALV_CL_PROCESS1  OUTPUT
