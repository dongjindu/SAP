REPORT zcpp100_module_subpart .
*----------------------------------------------------------------------*
* TABLE AREA
*----------------------------------------------------------------------*
TABLES: ztbm_abxduldt,  "MODULE vs SUB-PART
        ztbm_abxebmdt,  "ECM BOM STRUCTURE
        aenr,  "Change Master
        mast,  "Material to BOM Link
        stko,  "BOM Header
        stpo,  "BOM item (NONE VERSION)
        stas.  "BOMs - Item Selection (HISTORY)
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_ebmdt OCCURS 0,
        mtno TYPE ztbm_abxebmdt-mtno,
        plnt TYPE ztbm_abxebmdt-plnt,
        usag TYPE ztbm_abxebmdt-usag,
        altn TYPE ztbm_abxebmdt-altn,
        pref TYPE ztbm_abxebmdt-pref,
        comp TYPE ztbm_abxebmdt-comp,
        suff TYPE ztbm_abxebmdt-suff,
        qnty TYPE ztbm_abxebmdt-qnty,
        unit TYPE ztbm_abxebmdt-unit,
        seqc TYPE ztbm_abxebmdt-seqc,
        sequ TYPE ztbm_abxebmdt-sequ,
        eono TYPE ztbm_abxebmdt-eono,
        upct TYPE ztbm_abxebmdt-upct,
      END OF it_ebmdt.
DATA: it_duldt TYPE ztbm_abxduldt OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_zedat TYPE ztbm_abxebmdt-zedat.

*SELECT-OPTIONS:S_MTNO FOR ZTBM_ABXEBMDT-MTNO.
SELECTION-SCREEN END   OF BLOCK b1.

*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM read_process.
  PERFORM data_process.
*  PERFORM WRITE_PROCESS.

END-OF-SELECTION.

TOP-OF-PAGE.
*  PERFORM TOP_OF_PAGE.

END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  p_zedat = sy-datum.

*  S_MTNO.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_process.
  SELECT mtno "TYPE ZTBM_ABXEBMDT-MTNO,
         plnt "TYPE ZTBM_ABXEBMDT-PLNT,
         usag "TYPE ZTBM_ABXEBMDT-USAG,
         altn "TYPE ZTBM_ABXEBMDT-ALTN,
         pref "TYPE ZTBM_ABXEBMDT-PREF,
         comp "TYPE ZTBM_ABXEBMDT-COMP,
         suff "TYPE ZTBM_ABXEBMDT-SUFF,
         qnty "TYPE ztbm_abxebmdt-qnty,
         unit "TYPE ztbm_abxebmdt-unit,
         seqc "TYPE ZTBM_ABXEBMDT-SEQC,
         sequ "TYPE ZTBM_ABXEBMDT-SEQU,
         eono "TYPE ZTBM_ABXEBMDT-EONO,
         upct "TYPE ZTBM_ABXEBMDT-UPCT,
  FROM ztbm_abxebmdt
  INTO TABLE it_ebmdt
  WHERE zedat EQ p_zedat
    AND usag EQ '2'
    AND zresult NE 'E'.


  IF sy-subrc EQ 0.
  ELSE.
    WRITE: / 'NO DATA'.
  ENDIF.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_process.
  PERFORM data_move.
  PERFORM data_table_insert.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_AENR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EBMDT_EONO  text
*      <--P_L_DATUV  text
*----------------------------------------------------------------------*
FORM read_aenr USING    p_eono
               CHANGING p_datuv.
  SELECT SINGLE datuv
  FROM aenr
  INTO p_datuv
  WHERE aennr EQ p_eono.

  IF sy-subrc EQ 0.

  ELSE.
    CLEAR p_datuv.
  ENDIF.


ENDFORM.                    " READ_AENR
*&---------------------------------------------------------------------*
*&      Form  DATA_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_move.
  DATA: l_datuv TYPE sy-datum,
        l_eono TYPE ztbm_abxebmdt-eono.
  LOOP AT it_ebmdt.
    MOVE-CORRESPONDING it_ebmdt TO it_duldt.
    IF it_ebmdt-upct EQ '1'.
      CLEAR l_datuv.
      PERFORM read_aenr USING     it_ebmdt-eono
                        CHANGING  l_datuv.
      it_duldt-datuv = l_datuv.
      it_duldt-datub = '99991231'.

*      IT_EBMDT-EONO
    ELSEIF it_ebmdt-upct EQ '2'.
      it_ebmdt-sequ = it_ebmdt-sequ - 1.
      SELECT SINGLE eono
                  FROM ztbm_abxebmdt
                  INTO l_eono
                  WHERE mtno EQ it_duldt-mtno
                  AND   plnt EQ it_duldt-plnt
                  AND   usag EQ it_duldt-usag
                  AND   altn EQ it_duldt-altn
                  AND   pref EQ it_duldt-pref
                  AND   comp EQ it_duldt-comp
                  AND   suff EQ it_duldt-suff
                  AND   sequ EQ it_ebmdt-sequ.
      IF sy-subrc EQ 0.
        CLEAR l_datuv.
        PERFORM read_aenr USING     l_eono
                           CHANGING  l_datuv.
        it_duldt-datuv = l_datuv.
      ENDIF.

      CLEAR l_datuv.
      PERFORM read_aenr USING     it_ebmdt-eono
                        CHANGING  l_datuv.
      it_duldt-datub = l_datuv.
    ENDIF.
    it_duldt-andat = sy-datum.
    it_duldt-annam = sy-uname.
    APPEND it_duldt.
    CLEAR : it_duldt, it_ebmdt.
  ENDLOOP.

ENDFORM.                    " DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  DATA_TABLE_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_table_insert.
  DATA: la_duldt LIKE it_duldt,
        lt_duldt LIKE it_duldt OCCURS 0 WITH HEADER LINE.
*  INSERT ZTBM_ABXDULDT FROM TABLE IT_DULDT ACCEPTING DUPLICATE KEYS .
*  IF SY-SUBRC EQ 0.
*    WRITE: / 'SUCCESS'.
*    COMMIT WORK.
*  ELSE.
*    WRITE : / 'DUPLICATE'.
*    ROLLBACK WORK.
*  ENDIF.
  LOOP AT it_duldt.
    la_duldt = it_duldt.
    INSERT INTO ztbm_abxduldt VALUES la_duldt.
    IF sy-subrc EQ 0.
      WRITE: / 'SUCCESS'.
      COMMIT WORK.
    ELSE.
*    WRITE : / 'DUPLICATE'.
      lt_duldt = it_duldt.
      APPEND lt_duldt.
      ROLLBACK WORK.
    ENDIF.
    CLEAR : it_duldt, lt_duldt.
  ENDLOOP.
  IF NOT lt_duldt[] IS INITIAL.
    WRITE : / 'DUPLICATE'.
    LOOP AT lt_duldt.
      WRITE: / sy-tabix.
*    WRITE: / IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO,
*             IT_DULDT-MTNO.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DATA_TABLE_INSERT
