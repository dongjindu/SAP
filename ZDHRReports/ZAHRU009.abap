*----------------------------------------------------------------------
* Program ID        : ZAHRU009
* Title             : [HR] Badge ID Correction for Availability System
* Created on        : 5/1/2009
* Created by        : I.G.MOON
* Specifications By : EUNA LEE
* Description       : [HR] Badge ID Correction for Availability System
* 5/10/2012        t-code is deleted by APM Monitoring
*----------------------------------------------------------------------
REPORT zahru009 MESSAGE-ID zmco.

TABLES:  pa0001, zthr_bhisthmma.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

RANGES r_employeenumber FOR zthr_bhisthmma-employeenumber.

DATA: BEGIN OF gt_emp OCCURS 0,
          badge LIKE zthr_bhisthmma-badge,
          employeenumber LIKE zthr_bhisthmma-employeenumber,
          pernr LIKE pa0001-pernr,
          rdate LIKE zthr_bhisthmma-rdate,
      END OF gt_emp.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_pernr FOR pa0001-pernr MATCHCODE OBJECT prem.
SELECT-OPTIONS s_badge FOR zthr_bhisthmma-BADGE.
SELECT-OPTIONS s_date FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS p_upd AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  sy-title = '[HR] Badge ID Correction for Availability System'.

  s_date = 'IBT'.
  s_date-high = sy-datum.
  s_date-low  = sy-datum - 365.
  APPEND s_date.

START-OF-SELECTION.

  __cls : gt_emp, r_employeenumber.

  IF NOT s_pernr[] IS INITIAL.
    LOOP AT s_pernr.
      r_employeenumber = s_pernr.
      r_employeenumber-low = s_pernr-low+2.
      r_employeenumber-high = s_pernr-high+2.
      APPEND r_employeenumber.
    ENDLOOP.

    SELECT badge employeenumber rdate
    INTO CORRESPONDING FIELDS OF TABLE gt_emp
    FROM zthr_bhisthmma
    WHERE employeenumber IN r_employeenumber
    AND rdate IN s_date
    and badge in s_badge
    GROUP BY BADGE employeenumber RDATE .

    SORT gt_emp BY badge.

  ELSE.

    SELECT badge employeenumber rdate
    INTO CORRESPONDING FIELDS OF TABLE gt_emp
    FROM zthr_bhisthmma
    WHERE ( employeenumber LIKE '1%' OR employeenumber LIKE '0%' )
    AND rdate IN s_date
    and badge in s_badge
    GROUP BY BADGE employeenumber RDATE .

    SORT gt_emp BY badge employeenumber DESCENDING.

  ENDIF.

  DELETE ADJACENT DUPLICATES FROM gt_emp
    COMPARING badge.

  DELETE gt_emp WHERE employeenumber IS initial.

END-OF-SELECTION.

  DATA cnt TYPE i.
  DATA updcnt TYPE i.

  CLEAR :cnt, updcnt.

  LOOP AT gt_emp.

    SELECT SINGLE * FROM zthr_bhisthmma
     WHERE badge EQ gt_emp-badge
       AND employeenumber NE gt_emp-employeenumber
       AND rdate IN s_date.

    IF sy-subrc EQ 0.

      ADD 1 TO cnt.
      IF zthr_bhisthmma-employeenumber IS INITIAL.
        WRITE :/ gt_emp-badge,  ' : blank'  COLOR COL_NEGATIVE.
      ELSE.
        IF zthr_bhisthmma-employeenumber NE gt_emp-employeenumber.
          WRITE :/ gt_emp-badge,
                   ' : inconsistency'  COLOR COL_NEGATIVE,
                   zthr_bhisthmma-employeenumber,'<>',
                   gt_emp-employeenumber.
        ENDIF.
      ENDIF.

      IF p_upd EQ true.

       UPDATE zthr_bhisthmma SET employeenumber = gt_emp-employeenumber
                 WHERE badge EQ gt_emp-badge
                   AND rdate IN s_date
                   AND employeenumber NE gt_emp-employeenumber.

        COMMIT WORK.
        ADD 1 TO updcnt.

      ENDIF.

    ENDIF.

  ENDLOOP.

  IF cnt > 0.
    WRITE:/ 'Error count : ', cnt COLOR COL_NEGATIVE.
  ELSE.
    WRITE:/ 'No error was found.' COLOR COL_POSITIVE.
  ENDIF.

  IF updcnt > 0.
    WRITE:/ 'Updated : ', updcnt COLOR COL_NEGATIVE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
