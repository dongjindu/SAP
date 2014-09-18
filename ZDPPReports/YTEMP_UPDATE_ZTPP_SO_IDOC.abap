*&---------------------------------------------------------------------*
*& Report  YTEMP_UPDATE_ZTPP_SO_IDOC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ytemp_update_ztpp_so_idoc.

DATA: it_so_idoc_old LIKE ztpp_so_idoc OCCURS 0 WITH HEADER LINE.
DATA: it_so_idoc_new LIKE ztpp_so_idoc OCCURS 0 WITH HEADER LINE.

DATA: v_fridx TYPE i VALUE 1,
      v_toidx TYPE i VALUE 10000,
      v_lines TYPE i.

PARAMETERS: p_check.

START-OF-SELECTION.

  CHECK p_check EQ 'X'.

  SELECT * INTO TABLE it_so_idoc_old
    FROM ztpp_so_idoc.


  DESCRIBE TABLE it_so_idoc_old LINES v_lines.
  IF v_lines EQ 0.
    EXIT.
  ENDIF.

  DO.
    CLEAR: it_so_idoc_new, it_so_idoc_new[].

    V_FRIDX = ( SY-INDEX * 10000 ) - 10000 + 1.
    V_TOIDX =   SY-INDEX * 10000 .

    IF v_lines < v_toidx.
      MOVE: v_lines TO v_toidx.
    ENDIF.

    APPEND LINES OF it_so_idoc_old FROM v_fridx TO v_toidx
        TO it_so_idoc_new.

    LOOP AT it_so_idoc_new.
      MOVE: it_so_idoc_new-natn TO it_so_idoc_new-nation.
      MODIFY it_so_idoc_new.
    ENDLOOP.

    INSERT ztpp_so_idoc FROM TABLE it_so_idoc_new
                        ACCEPTING DUPLICATE KEYS.

    COMMIT WORK AND WAIT.

    IF V_TOIDX >= V_LINES.
      EXIT.
    ENDIF.
  ENDDO.

  DELETE FROM ZTPP_SO_IDOC WHERE NATION EQ SPACE.
