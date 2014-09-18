*&------------------------------------------------------------------
*& Program ID     : ZMMR05800T
*& Profram Name   : Interface Detail Table Maintenance(ZMMT0102)
*& Created by     : jeongsu.youn
*& Created on     : 18.03.2011
*& Development ID : T00022
*& Reference Pgm. :
*& Description    : Interface Detail Table Maintenance(ZMMT0102)
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*& 18.03.2011  jeongsu.youn                   first dev.
*&--------------------------------------------------------------------
REPORT  zmmr05800t LINE-COUNT 50(2) LINE-SIZE 100.

TABLES : zmmt0102.
TYPES : ty_zmmt0101 TYPE zmmt0101.


DATA : lt_zmmt0101 TYPE ty_zmmt0101 OCCURS 0 WITH HEADER LINE,
*DATA : lt_zmmt0101 LIKE zmmt0101 OCCURS 0 WITH HEADER LINE,
       lt_zmmt0102 LIKE zmmt0102 OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
*SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_ifkey  FOR zmmt0102-ifkey.
SELECTION-SCREEN END   OF BLOCK bl1.
*----------------------------------------------------------------------*
* TOP-OF-PAGE                                                          *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  WRITE : /2(30) 'Interface Key',
          34(02) 'Country',
          38(10) 'Index.Date',
          50(12) 'Dele.Cnt',
          64(30) 'Result'.
  ULINE /2(94).



*----------------------------------------------------------------------*
*START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA : lv_ixdate LIKE sy-datum.
  DATA : lv_cnt LIKE sy-tfill.

  CLEAR lt_zmmt0101. REFRESH lt_zmmt0101.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zmmt0101
    FROM zmmt0101
   WHERE ifkey IN s_ifkey.

  LOOP AT lt_zmmt0101.

    IF lt_zmmt0101-tbpri > 0.
      lv_ixdate = sy-datum - lt_zmmt0101-tbpri.

      CLEAR lt_zmmt0102. REFRESH lt_zmmt0102.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zmmt0102
        FROM zmmt0102
       WHERE ifkey = lt_zmmt0101-ifkey
         AND centy = lt_zmmt0101-centy
         AND etdat =< lv_ixdate.
      IF sy-subrc EQ 0.
        CLEAR lv_cnt.
        DESCRIBE TABLE lt_zmmt0102 LINES lv_cnt.

        DELETE FROM zmmt0102 WHERE ifkey = lt_zmmt0101-ifkey
                               AND centy = lt_zmmt0101-centy
                               AND etdat =< lv_ixdate.
        IF sy-subrc EQ 0.
          WRITE : /2(30) lt_zmmt0101-ifkey,
                  34(02) lt_zmmt0101-centy,
                  38(10) lv_ixdate,
                  50(12) lv_cnt,
                  64(30) 'Successfully deleted.'.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDLOOP.
