*----------------------------------------------------------------------*
*   INCLUDE MZEMMGM01E_6007F01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  make_it_func
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_func.

ENDFORM.                    " make_it_func
*---------------------------------------------------------------------*
*       FORM get_unit_of_matnr                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_MATNR)                                                *
*  -->  VALUE(P_MEINS)                                                *
*---------------------------------------------------------------------*
FORM get_unit_of_matnr USING    value(p_matnr)
                       CHANGING value(p_meins).
  CLEAR: p_meins.
  SELECT SINGLE meins INTO p_meins
    FROM mara
    WHERE matnr = p_matnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  make_tc_9200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSMM_6007_01  text
*      -->P_1      text
*      -->P_TC_9200  text
*----------------------------------------------------------------------*
FORM make_tc_9200 TABLES ta_itab LIKE it_zsmm_6007_02
                  USING  value(p_visiblelines)
                         value(p_tc) TYPE cxtab_control.
  DATA: itablines TYPE i.  "Lines of itab
  STATICS: itemno(4) TYPE n.
  "Item no type numeric char (original is char)
*  DELETE ta_itab WHERE sernr = space.  "delete where space
*  SORT ta_itab BY sernr.               "sort
  DESCRIBE TABLE ta_itab LINES itablines.
  "in order to determine total lines of itab
  IF itablines IS INITIAL.   " i.e. itab is empty
    DO p_visiblelines TIMES.
      CLEAR ta_itab.
      MOVE-CORRESPONDING ztmm_6007_02 TO ta_itab.
      itemno = itemno + 10.
      ta_itab-posnr = itemno.  "BOM item number
      APPEND ta_itab.
*      APPEND INITIAL LINE TO ta_itab.
    ENDDO.
  ELSE.                     " i.e. itab is not empty
    DATA remainedlines TYPE i.
    DATA blanklines    TYPE i.
    remainedlines = ( itablines - p_tc-top_line ) + 1.

    IF itemno IS INITIAL.
      IF sy-tcode = 'ZMME84'. "Create
        SELECT SINGLE MAX( posnr ) INTO gs_zvmm_6007_05-posnr
          FROM zvmm_6007_05    "EMMGM01 BOM List
          WHERE matnr = ztmm_6007_02-endpart AND
                werks = ztmm_6007_02-werks.
        itemno = gs_zvmm_6007_05-posnr.
      ELSE.
        SELECT SINGLE MAX( posnr ) INTO ztmm_6007_02-posnr
          FROM ztmm_6007_02
          WHERE endpart = ztmm_6007_02-endpart AND
                werks   = ztmm_6007_02-werks.
        itemno = ztmm_6007_02-posnr.
      ENDIF.
    ENDIF.

    IF remainedlines < p_visiblelines.
      blanklines = p_visiblelines - remainedlines.
      DO blanklines TIMES.
        CLEAR ta_itab.
        MOVE-CORRESPONDING ztmm_6007_02 TO ta_itab.
        itemno = itemno + 10.
        ta_itab-posnr = itemno.  "BOM item number
        APPEND ta_itab.
*        APPEND INITIAL LINE TO ta_itab.
      ENDDO.
    ENDIF.
  ENDIF.
ENDFORM.                    " make_tc_9200
*&---------------------------------------------------------------------*
*&      Form  check_dup_subpart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSMM_6007_02  text
*      <--P_IX_CHECK  text
*----------------------------------------------------------------------*
FORM check_dup TABLES   ta_itab STRUCTURE zsmm_6007_02
               CHANGING value(p_ix_check).

ENDFORM.                    " check_dup_subpart
*&---------------------------------------------------------------------*
*&      Form  check_dup_subpart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSMM_6007_02  text
*      <--P_IX_CHECK  text
*----------------------------------------------------------------------*
FORM check_dup_subpart TABLES   p_itab STRUCTURE zsmm_6007_02
                       CHANGING value(p_ix_check).
  CLEAR: p_ix_check.
  FIELD-SYMBOLS: <fs_itab> LIKE LINE OF p_itab.
** For duplicate check
  DATA: BEGIN OF it_dup OCCURS 0,
          f1(30),
          f2(30),
          f3(30),
          f4(30),
          f5(30),
        END OF it_dup.

* IN P_ITAB
  CLEAR it_dup. REFRESH it_dup.
  LOOP AT p_itab ASSIGNING <fs_itab>.
    READ TABLE it_dup WITH KEY f1 = <fs_itab>-subpart.
    IF sy-subrc = 0.
      p_ix_check = p_ix_check + 1.
      MESSAGE w999(zmmm) WITH <fs_itab>-subpart
                              ': Duplicate Data!'.
      p_ix_check = p_ix_check + 1.
      EXIT.
    ELSE.
      MOVE <fs_itab>-subpart    TO it_dup-f1.
      APPEND it_dup.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_dup_subpart
