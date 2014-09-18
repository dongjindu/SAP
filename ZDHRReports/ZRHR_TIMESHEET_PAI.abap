**----------------------------------------------------------------------
*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'PRT'.
*      SET PF-STATUS 'PRI'.
      PERFORM print.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*TOP-OF-PAGE.
*  IF g_flag IS INITIAL.
*    PERFORM write_title.
*    g_flag = 'X'.
*  ELSE.
*    PERFORM write_title.
*    WRITE : / sy-uline.
*  ENDIF.
*
END-OF-PAGE.
  WRITE : / sy-uline.

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_form.

  CLEAR g_flag.

  DATA: l_date LIKE sy-datum,
        l_day(20),
        l_weekday  LIKE dtresr-weekday,
        l_cn TYPE i,
        l_line(1),
        l_pernr LIKE pa0007-pernr.

  DATA    l_line_int.

  NEW-PAGE LINE-SIZE 207.

*  new-page line-size 198 print on.
*  print-control FONT 8.


** print content
*  READ TABLE IT_TAB INDEX 1.
*  L_PERNR = IT_tAB-PERNR.
  l_pernr = '*'.
  l_cn = 0.
  LOOP AT it_tab.

    IF it_tab-pernr <> l_pernr.
      IF l_cn = 0.
*        FORMAT COLOR 2.
        FORMAT COLOR OFF.
        l_cn = 1.
        l_line_int = 'X'.
      ELSE.
        FORMAT COLOR OFF.
        l_cn = 0.
        l_line_int = space.
      ENDIF.
      WRITE : / sy-uline.
      WRITE : / sy-vline NO-GAP, (8) it_tab-pernr    NO-GAP,
                  sy-vline NO-GAP, (25) it_tab-sname    NO-GAP.
      l_pernr = it_tab-pernr.
      l_line = ' '.
    ELSE.
      WRITE: / sy-vline NO-GAP, (8) ' '  NO-GAP,
               sy-vline NO-GAP, (25) ' ' NO-GAP.
      ULINE 36(131).
      WRITE: 207 sy-vline NO-GAP.

      WRITE: / sy-vline NO-GAP, (8) ' '  NO-GAP,
                sy-vline NO-GAP, (25) ' ' NO-GAP.
      l_line = 'X'.
    ENDIF.
    WRITE: sy-vline NO-GAP, (8) it_tab-schkz    NO-GAP.
    WRITE: sy-vline NO-GAP, (6) it_tab-awart    NO-GAP,
           sy-vline NO-GAP, (25) it_tab-atext    NO-GAP,

           sy-vline NO-GAP, (10) it_tab-catshours_01 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_02 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_03 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_04 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_05 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_06 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_07 NO-GAP,
           sy-vline NO-GAP, (10) it_tab-total    NO-GAP,
** Changed by Furong on 02/06/08
*              SY-VLINE NO-GAP, (40) SPACE               NO-GAP,
           sy-vline NO-GAP.
    IF l_line = ' '.
      IF l_line_int EQ 'X'.
        WRITE: 167(34) it_tab-sname NO-GAP COLOR 2.
      ELSE.
        WRITE: 167(34) it_tab-sname NO-GAP.
      ENDIF.
    ELSE.
      WRITE: 167(38) space NO-GAP.
    ENDIF.
    WRITE: 207 sy-vline NO-GAP.
** End of change
  ENDLOOP.
  WRITE : / sy-uline.
  FORMAT COLOR OFF.
  WRITE : / sy-vline NO-GAP, (76) 'Supervisor Signature:' NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (10) space NO-GAP,
            sy-vline NO-GAP, (40) space NO-GAP,
            sy-vline NO-GAP.
  WRITE : / sy-uline.

ENDFORM.                    " PRINT_FORM
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm .
*   CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print.
  LEAVE TO LIST-PROCESSING.
  PERFORM print_form.
*   leave list-processing.
*   call screen 0300.
ENDFORM.                    " PRINT
