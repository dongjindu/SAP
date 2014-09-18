*&---------------------------------------------------------------------*
*& Report  YRMMPM24R_DOCK                                              *
*&---------------------------------------------------------------------*
REPORT  yrmmpm24r_dock NO STANDARD PAGE HEADING
                          LINE-SIZE 90  LINE-COUNT 80
                          MESSAGE-ID zmmm.

TABLES : ztmm_dock.

*------ internal table
DATA : BEGIN OF  it_ztmm_dock OCCURS 0.
        INCLUDE STRUCTURE ztmm_dock.
DATA : END OF it_ztmm_dock.


DATA : BEGIN OF it_wait OCCURS 0,
       zdock  LIKE ztmm_dock-zdock,
       zoccup TYPE  i,
       zwait  TYPE  i,
       END OF it_wait.

*------     Global variable
DATA: fg_color,
      g_tot_page LIKE sy-pagno.     "Total Page

*------ SELECTION SCREEN
SELECTION-SCREEN : BEGIN OF BLOCK bl1  WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_dock FOR ztmm_dock-zdock.
PARAMETERS p_occu AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_wait AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN : END OF BLOCK bl1.

*----- At selection screen
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
*  PERFORM check_rtn.
  PERFORM read_data.

*------ TOP-OF-PAGE
TOP-OF-PAGE.
  PERFORM dispaly_heager.

*----- Start of selection
START-OF-SELECTION.
  PERFORM display_data.
*&---------------------------------------------------------------------*
*&      Form  display_data
*----------------------------------------------------------------------*
FORM display_data.

  LOOP AT it_wait.

    IF p_wait EQ 'X' AND p_occu EQ 'X' OR
       p_wait EQ ' ' AND p_occu EQ ' '.

      WRITE :  1  '|'  NO-GAP, it_wait-zdock
                       COLOR COL_NORMAL INTENSIFIED OFF,
               29 '|'  NO-GAP, it_wait-zoccup
                       COLOR COL_NORMAL INTENSIFIED OFF,
               60 '|'  NO-GAP, it_wait-zwait
                       COLOR COL_NORMAL INTENSIFIED OFF,
               90 '|'  NO-GAP.
      ULINE.
    ENDIF.


    IF p_occu EQ 'X' AND p_wait NE 'X' .
      CHECK  NOT It_wait-zoccup IS INITIAL.

      WRITE :  1  '|'  NO-GAP, it_wait-zdock
                       COLOR COL_NORMAL INTENSIFIED OFF,
               29 '|'  NO-GAP, it_wait-zoccup
                       COLOR COL_NORMAL INTENSIFIED OFF,
               60 '|'  NO-GAP, it_wait-zwait
                       COLOR COL_NORMAL INTENSIFIED OFF,
               90 '|'  NO-GAP.
      ULINE.
  ENDIF.


    IF p_wait EQ 'X' AND p_occu NE 'X'.
     CHECK  NOT It_wait-ZWAIT IS INITIAL.

    WRITE :  1  '|'  NO-GAP, it_wait-zdock
                       COLOR COL_NORMAL INTENSIFIED OFF,
               29 '|'  NO-GAP, it_wait-zoccup
                       COLOR COL_NORMAL INTENSIFIED OFF,
               60 '|'  NO-GAP, it_wait-zwait
                       COLOR COL_NORMAL INTENSIFIED OFF,
               90 '|'  NO-GAP.
      ULINE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM dispaly_heager.
  WRITE : 1(90) text-h01 CENTERED.
  SKIP 2.

  WRITE : /2 text-t02,
          50 text-t03, 70(15) sy-datum.
  WRITE : /2 text-t04, sy-uname,
          50 text-t05, 70(15) sy-repid.


  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.
  WRITE : text-t08.
  ULINE.
ENDFORM.                    " dispaly_heager
*&------------------------------------------------------------------*
*&      Form  READ_DATA
*&------------------------------------------------------------------*
FORM read_data.
  CLEAR : it_ztmm_dock, it_ztmm_dock[].

*-- Read data

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_dock
       FROM ztmm_dock
        WHERE zdock IN s_dock.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

*-- Read data  Occupied, waiting  CLEAR : it_wait[].

  DATA : lw_fld_name(30) TYPE c.               "  Field name Variable
  FIELD-SYMBOLS : <lw_fs>.
  DATA : lw_fld_cnt(2)  TYPE n.                " '01' ~ '10'

  LOOP AT it_ztmm_dock.

    CLEAR it_wait.

    MOVE : it_ztmm_dock-zdock TO it_wait-zdock.

    CLEAR lw_fld_cnt.

    DO 10 TIMES.
      lw_fld_cnt = lw_fld_cnt + 1.
      CONCATENATE 'IT_ZTMM_DOCK-TR_' lw_fld_cnt INTO lw_fld_name.
      ASSIGN  (lw_fld_name) TO <lw_fs>.

      IF NOT <lw_fs> IS INITIAL.
        it_wait-zwait = it_wait-zwait + 1.
      ENDIF.

    ENDDO.

*    IF NOT IT_ZTMM_DOCK-TR_01 IS INITIAL.
*      IT_WAIT-ZWAIT = IT_WAIT-ZWAIT + 1.
*    ENDIF.

    IF it_wait-zwait NE space.
      it_wait-zwait = it_wait-zwait - 1.
      it_wait-zoccup = 1.
    ENDIF.
    APPEND it_wait.

  ENDLOOP.
ENDFORM.                    " READ_DATA
