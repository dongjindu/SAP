*----------------------------------------------------------------------*
***INCLUDE MZ903M_CODITION_MAINTO01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'MENU_9000' .
  SET TITLEBAR  'TITL_9000' .
ENDMODULE.                 " status_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen OUTPUT.
  IF WA_9000 = SPACE .
     WA_9000 = 'X'   .
     CONCATENATE C_SAMPLE1 C_SAMPLE2 INTO WA_SAMPLE .
     PERFORM READ_CONDITION.
  ENDIF.
ENDMODULE.                 " init_screen  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'MAIN_9100'.
  SET TITLEBAR  'TITL_9100'.
ENDMODULE.                 " STATUS_9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_9100 OUTPUT.
  IF wa_init = space  .
    wa_code = sv_code.
    wa_init = 'X'    .
  ENDIF.
ENDMODULE.                 " init_9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODI_SCREEN9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modi_screen9100 OUTPUT.
  CASE wa_code.
    WHEN 'COPY' OR 'NEW'.
    WHEN 'READ'  .
      LOOP AT SCREEN.
        IF SCREEN-GROUP2 = 'ALL'.
           SCREEN-INPUT  = 0    .
           MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN 'UPDATE'.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'KEY'.
           SCREEN-INPUT  = 0    .
           MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " MODI_SCREEN9100  OUTPUT
