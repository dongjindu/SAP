*&---------------------------------------------------------------------*
*&  Include           MP988300I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZCGSJIKUN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_ZZCGSJIKUN INPUT.


  DATA:
                lt_zzcgsjikun   LIKE TABLE OF gs_jikun.



  IF gt_zzcgsjikun IS INITIAL.

    SELECT domvalue_l AS zzcgsjikun ddtext AS value
      FROM dd07v
      INTO CORRESPONDING FIELDS OF TABLE gt_zzcgsjikun
      WHERE domname = 'ZCGSJIKUN'
            AND ddlanguage = sy-langu.

  ENDIF.

  lt_zzcgsjikun = gt_zzcgsjikun.

  CASE p9883-zzcgsjbgrp.
    WHEN 'E'.
      DELETE lt_zzcgsjikun WHERE zzcgsjikun = ' T' OR zzcgsjikun = 'Z' .
    WHEN '1'.
      DELETE lt_zzcgsjikun WHERE zzcgsjikun = 'C' OR zzcgsjikun = 'O' .
    WHEN OTHERS.
  ENDCASE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZZCGSJIKUN'
      dynpprog        = sy-repid
      dynpnr          = '2000'
      dynprofield     = 'P9883-ZZCGSJIKUN'
      value_org       = 'S'
    TABLES
      value_tab       = lt_zzcgsjikun
      return_tab      = gt_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
 EXPORTING
   FUNCTIONCODE                 = '/00'
 EXCEPTIONS
   FUNCTION_NOT_SUPPORTED       = 1
   OTHERS                       = 2
          .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


ENDMODULE.                 " F4_ZZCGSJIKUN  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_ZZCGSJIKUB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_ZZCGSJIKUB INPUT.


  DATA:
                  lt_zzcgsjikub   LIKE TABLE OF gs_jikub.

  IF gt_zzcgsjikub IS INITIAL.

    SELECT domvalue_l AS zzcgsjikub ddtext AS value
      FROM dd07v
      INTO CORRESPONDING FIELDS OF TABLE gt_zzcgsjikub
      WHERE domname = 'ZCGSJIKUB'
            AND ddlanguage = sy-langu.

  ENDIF.

  lt_zzcgsjikub = gt_zzcgsjikub.

  CASE p9883-zzcgsjikun.
    WHEN 'C'.
      DELETE lt_zzcgsjikub   WHERE zzcgsjikub <> 'G5'.
    WHEN 'M' .
      CASE p9883-zzcgsjbgrp.
        WHEN 'E'.
          DELETE lt_zzcgsjikub
          WHERE zzcgsjikub <> 'G1' AND zzcgsjikub <> 'G2'
                AND zzcgsjikub <> 'G3' AND zzcgsjikub <> 'G4'.
        WHEN '1'.
          DELETE lt_zzcgsjikub
          WHERE zzcgsjikub <> 'MG1' AND zzcgsjikub <> 'MG2'
                AND zzcgsjikub <> 'MG3' AND zzcgsjikub <> 'MG4'
                AND zzcgsjikub <> 'MG5'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'R'.
      CASE p9883-zzcgsjbgrp.
        WHEN 'E'.
          DELETE lt_zzcgsjikub
          WHERE zzcgsjikub <> 'R1' AND zzcgsjikub <> 'R2'.
        WHEN '1'.
          DELETE lt_zzcgsjikub
          WHERE zzcgsjikub <> 'MG1' AND zzcgsjikub <> 'MG2'
                AND zzcgsjikub <> 'MG3' AND zzcgsjikub <> 'MG4'
                AND zzcgsjikub <> 'MG5'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'O'.
      DELETE lt_zzcgsjikub   WHERE zzcgsjikub <> 'N'.
    WHEN 'T'.
      DELETE lt_zzcgsjikub
         WHERE zzcgsjikub <> 'TG1' AND zzcgsjikub <> 'TG2' AND zzcgsjikub <> 'TG3'.
    WHEN 'Z'.
      DELETE lt_zzcgsjikub   WHERE zzcgsjikub <> 'N'.
    WHEN OTHERS.
  ENDCASE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZZCGSJIKUB'
      dynpprog        = sy-repid
      dynpnr          = '2000'
      dynprofield     = 'P9883-ZZCGSJIKUB'
      value_org       = 'S'
    TABLES
      value_tab       = lt_zzcgsjikub
      return_tab      = gt_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
 EXPORTING
   FUNCTIONCODE                 = '/00'
 EXCEPTIONS
   FUNCTION_NOT_SUPPORTED       = 1
   OTHERS                       = 2
          .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDMODULE.                 " F4_ZZCGSJIKUB  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMEND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMEND_2000 INPUT.
  CASE sy-ucomm.
    WHEN 'HELP'. " Open Help file (PDF format)
      PERFORM open_file.
  ENDCASE.
ENDMODULE.                 " USER_COMMEND_2000  INPUT
