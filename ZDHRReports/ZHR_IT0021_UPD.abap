REPORT zhr_it0021_upd NO STANDARD PAGE HEADING  MESSAGE-ID zg.
*-----------------------------------------------------------------------
* Name: ZHR_IT0021_UPD
* Tech. Resource: Euna Lee
* Desc: IT0021 Update program with Sq. No
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZHR_IT0021_UPD
*  Author         : ig.moon
*  Creation Data  : 11/10/2008
*  Requirements by: Euna Lee
*  Description    : IT0021 Update program with Sq. No.
************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES: pa0021.

DATA : BEGIN OF itab OCCURS 0,
        pernr LIKE pa0021-pernr,
        n_subty(2) TYPE n,
        subty LIKE pa0021-subty,
        objps LIKE pa0021-objps,
        sprps LIKE pa0021-sprps,
        erbnr LIKE pa0021-erbnr,
        endda LIKE pa0021-endda,
        begda LIKE pa0021-begda,
        seqnr LIKE pa0021-seqnr,
       END OF itab.

DATA  $ix TYPE i.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
SELECT-OPTIONS s_pernr FOR pa0021-pernr.
PARAMETERS : p_upd AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK 0.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE u_break.
  if not p_debug is initial.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

****************************** Global Data *****************************

INITIALIZATION.
  IF sy-uname NE '103569' AND sy-uname NE 'HIS20065'.
    LEAVE PROGRAM.
  ENDIF.
*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  __cls itab.

  SELECT pernr subty sprps objps endda begda seqnr erbnr
    INTO CORRESPONDING FIELDS OF TABLE itab
         FROM pa0021
         WHERE pernr IN s_pernr
         AND endda EQ '99991231'.

  LOOP AT itab.
    itab-n_subty = itab-subty.
    MODIFY itab INDEX sy-tabix TRANSPORTING n_subty.
  ENDLOOP.

*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*
  DATA $flag.
  DATA width TYPE i.
  DATA even TYPE i VALUE 0.            " Checker even/odd
  DATA count TYPE i.
  DATA nc(2) TYPE n.
  width = 28.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  ULINE AT /(width).

  WRITE:/ '|'  NO-GAP,
          '  pernr ' COLOR COL_HEADING NO-GAP,
          '|'  NO-GAP,
          'subty' COLOR COL_HEADING  NO-GAP, '|'  NO-GAP,
          'objps' COLOR COL_HEADING  NO-GAP, '|'  NO-GAP,
          'ERBNR' COLOR COL_HEADING  NO-GAP, '|'  NO-GAP.

  ULINE AT /(width).

  SORT itab BY pernr n_subty objps.

  LOOP AT itab.

    AT NEW  pernr.
      CLEAR nc.
    ENDAT.

    ADD 1 TO nc.

    itab-erbnr = nc.
    MODIFY itab INDEX sy-tabix TRANSPORTING erbnr.
  ENDLOOP.

  CLEAR count.
  LOOP AT itab.

    AT NEW pernr.
      ADD 1 TO count.
    ENDAT.

    even = count MOD 2.                " Check for even/odd
    IF even = '0'.
      FORMAT COLOR COL_NORMAL INTENSIFIED.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.

    WRITE:/ '|'  NO-GAP,
            itab-pernr  NO-GAP, '|'  NO-GAP,
            itab-subty  NO-GAP, ' |'  NO-GAP,
            itab-objps  NO-GAP, '   |'  NO-GAP,
            itab-erbnr(2)  NO-GAP, '   |'  NO-GAP.

    IF p_upd EQ true.
      UPDATE pa0021 SET erbnr = itab-erbnr
      WHERE pernr EQ itab-pernr
        AND subty EQ itab-subty
        AND objps EQ itab-objps
        AND sprps EQ itab-sprps
        AND endda EQ itab-endda
        AND begda EQ itab-begda
        AND seqnr EQ itab-seqnr.
      IF sy-subrc EQ 0.
      ELSE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  ULINE AT /(width).
