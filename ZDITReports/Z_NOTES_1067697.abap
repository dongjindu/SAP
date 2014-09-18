*&---------------------------------------------------------------------*
*& Report  Z_NOTES_1067697
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_NOTES_1067697.

TABLES pnodtx.
SELECT-OPTIONS: so_pguid FOR pnodtx-pnguid,
so_spras FOR pnodtx-spras.

START-OF-SELECTION.
  DATA: lv_cursor TYPE cursor,
  lt_pnodtx TYPE TABLE OF pnodtx.
  FIELD-SYMBOLS: <fs_pnodtx> TYPE pnodtx.
  OPEN CURSOR WITH HOLD lv_cursor FOR
  SELECT * FROM pnodtx
  WHERE pnguid IN so_pguid
  AND spras IN so_spras.
  WHILE 1 = 1.
    REFRESH lt_pnodtx.
    FETCH NEXT CURSOR lv_cursor
    APPENDING CORRESPONDING FIELDS
    OF TABLE lt_pnodtx
    PACKAGE SIZE 1000.
    IF sy-subrc <> 0.
      CLOSE CURSOR lv_cursor.
      EXIT.
    ENDIF.
    LOOP AT lt_pnodtx ASSIGNING <fs_pnodtx>
    WHERE NOT pntext IS INITIAL.
      UPDATE pnwtyh SET h_pntext = <fs_pnodtx>-pntext
      WHERE pnguid = <fs_pnodtx>-pnguid.
      IF sy-subrc <> 0.
        UPDATE pnwtyv SET pntext = <fs_pnodtx>-pntext
        WHERE pnguid = <fs_pnodtx>-pnguid.
        IF sy-subrc <> 0.
          sy-subrc = 0. "no WTY-Text
        ENDIF.
      ENDIF.
    ENDLOOP.
    UNASSIGN <fs_pnodtx>.
    CALL FUNCTION 'DB_COMMIT'.
  ENDWHILE.
