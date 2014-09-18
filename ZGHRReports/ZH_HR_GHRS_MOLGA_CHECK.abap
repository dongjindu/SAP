*&---------------------------------------------------------------------*
*& Report  ZH_HR_GHRS_MOLGA_CHECK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zh_hr_ghrs_molga_check.

CONSTANTS: gc_molga    TYPE molga VALUE '10'.

DATA: lt_t001p    TYPE TABLE OF t001p WITH HEADER LINE.
DATA: lt_t500t    TYPE TABLE OF t500t WITH HEADER LINE.

DATA: BEGIN OF lt_pernr OCCURS 0,
        pernr     TYPE pa0001-pernr,
        werks     TYPE pa0001-werks,
        btrtl     TYPE pa0001-btrtl,
      END OF lt_pernr.

DATA: lv_error(1).

START-OF-SELECTION.

  SELECT SINGLE * FROM t500t INTO lt_t500t
    WHERE spras = sy-langu
    AND   molga = gc_molga.
  WRITE:/ 'Used Molga : ', gc_molga, '(', lt_t500t-ltext, ')'.

  SELECT * FROM t001p INTO TABLE lt_t001p
    ORDER BY werks btrtl.

  SELECT pernr werks btrtl
    FROM pa0001
    INTO TABLE lt_pernr
    WHERE begda <= sy-datum
    AND   endda >= sy-datum.

  LOOP AT lt_pernr.
    CLEAR lt_t001p.
    READ TABLE lt_t001p
      WITH KEY werks = lt_pernr-werks
               btrtl = lt_pernr-btrtl.
    IF sy-subrc <> 0.
      WRITE:/ '[', lt_pernr-pernr, '] - Not found MOLGA data'.
      lv_error = 'X'.
      CONTINUE.
    ENDIF.

    IF lt_t001p-molga <> gc_molga.
      CLEAR lt_t500t.
      READ TABLE lt_t500t WITH KEY molga = lt_t001p-molga.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM t500t INTO lt_t500t
          WHERE spras = sy-langu
          AND   molga = gc_molga.
        IF sy-subrc = 0.
          APPEND lt_t500t.
        ENDIF.
      ENDIF.
      WRITE:/ '[', lt_pernr-pernr, '] - ',
              lt_t001p-molga, '(', lt_t500t-ltext, ')'.
      lv_error = 'X'.
    ENDIF.

  ENDLOOP.

  IF lv_error IS INITIAL.
    WRITE:/ 'Not found different Molga'.
  ENDIF.
