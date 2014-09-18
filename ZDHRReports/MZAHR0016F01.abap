*----------------------------------------------------------------------*
***INCLUDE MZAHR0016F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE_OTHER_EXPENSES_PAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_other_expenses_payment.
  DATA : it_00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.
  DATA : w_head TYPE i,
         w_head2 TYPE i,
         w_index TYPE sy-tabix,
         w_amunt LIKE it_9000-amunt,
         w_other LIKE it_9000-amunt.

  SELECT * INTO TABLE it_00
           FROM zthr_pcp00
           WHERE zpera = w_werks
             AND zyear = w_zyear
             AND zvers = w_zvers .
*change 20040621 request by njj
*  IF W_SAVE = 'X'.
*    MESSAGE E000(ZMHR) WITH 'Saved Bonus '.
*  ENDIF.
**************
  IF sy-subrc NE 0.
    MESSAGE e000(zmhr) WITH 'NO date for update'.
  ENDIF.

  SORT it_00 BY zcost zobjc.

  LOOP AT it_9000.
    CLEAR : w_amunt, w_other, w_index.
    LOOP AT it_00 WHERE zcost = it_9000-zcost
                    AND zobjc = it_9000-zobjc
                    AND zperg = it_9000-zperg
                    AND ZSENR = IT_9000-ZSENR.
      w_index = sy-tabix.
      it_00-act22 =
*     ( ( it_9000-amunt / it_9000-zhedc )  * it_00-zhedc )    .
*        ( it_9000-amunt  * it_00-zhedc )    .
         ( it_9000-amunt )." * it_00-zhedc )
      w_amunt = it_00-act22 + w_amunt.

      MODIFY it_00.
    ENDLOOP.
  ENDLOOP.
  MODIFY zthr_pcp00 FROM TABLE it_00.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
  ELSE.
    w_input = ''.
    w_save  = 'X'.
    COMMIT WORK.
    MESSAGE s001(zmhr) WITH 'Transaction was processed successfully'.
  ENDIF.

ENDFORM.                    " SAVE_OTHER_EXPENSES_PAYMENT

*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_main_data.
  DATA : wl_answer,
         w_line TYPE i.

  CLEAR : it_9000[], it_9000.

  SELECT zcost zperg zobjc zsenr
         SUM( zhedc )  AS zhedc
         SUM( act22 )  AS amunt
         INTO CORRESPONDING FIELDS OF TABLE it_9000
         FROM zthr_pcp00
         WHERE zpera = w_werks
           AND zyear = w_zyear
           AND zvers = w_zvers
           GROUP by zcost zperg zobjc zsenr.

  SORT it_9000 BY zcost zperg zobjc zsenr.

  LOOP AT it_9000.
*    it_9000-amunt = ( it_9000-amunt  / 12 )  / ( it_9000-zhedc / 12 ) .
    it_9000-amunt = ( it_9000-amunt  / 12 ) ."/ it_9000-zsenr.
    it_9000-zhedc = it_9000-zhedc / 12 .

    SELECT SINGLE ktext INTO it_9000-ktext
     FROM cskt WHERE spras = sy-langu
                 AND kostl = it_9000-zcost
                 AND datbi = '99991231'.

    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO it_9000-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND ( zgrup = '1260' or zgrup = '1270' )
             AND zctxt EQ  it_9000-zcost.
    ENDIF.

    SELECT SINGLE short INTO it_9000-zjobk
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_9000-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.

    MODIFY it_9000.
  ENDLOOP.
* TABLE CONTROL
*  SORT IT_9000 BY ZOBJC .
  PERFORM data_sorting.
  REFRESH CONTROL 'TC_9000' FROM SCREEN 9000.
  DESCRIBE TABLE it_9000 LINES tc_9000-lines.

ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  SORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_table.
  DATA: cols LIKE LINE OF tc_9000-cols,
       sort_name(80),
       sort_dumy(80),
        lv_off  TYPE i.

  IF sy-ucomm = 'ASD'.
    CLEAR: sort_dumy, sort_name.
    READ TABLE tc_9000-cols INTO cols WITH KEY selected = 'X'.
    IF sy-subrc = 0.
      SPLIT cols-screen-name AT '-' INTO sort_dumy sort_name .
      SORT it_9000 STABLE BY (sort_name) ASCENDING.
      cols-selected = ' '.
      MODIFY tc_9000-cols FROM cols INDEX sy-tabix.
    ENDIF.
  ELSEIF sy-ucomm = 'DES'.
    CLEAR: sort_dumy, sort_name.
    READ TABLE tc_9000-cols INTO cols WITH KEY selected = 'X'.
    IF sy-subrc = 0.
      SPLIT cols-screen-name AT '-' INTO sort_dumy sort_name .
      SORT it_9000 STABLE BY (sort_name) DESCENDING.
      cols-selected = ' '.
      MODIFY tc_9000-cols FROM cols INDEX sy-tabix.
    ENDIF.
  ENDIF.

ENDFORM.                    " SORT_TABLE
*&---------------------------------------------------------------------*
*&      Form  DOWN_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_excel_file.
  DATA : wa_filename    LIKE rlgrap-filename .

  DATA: BEGIN OF it_down OCCURS 0,
        zcost(40),
        ktext(40),
        zperg(40),
        zobjc(40),
        zjobk(40),
        zhedc(40),
        amunt(40),
       END OF it_down.

  CLEAR : it_down, it_down[].

  IF it_9000[] IS INITIAL.
    PERFORM select_main_data.
  ENDIF.

  LOOP AT it_9000.
    MOVE-CORRESPONDING it_9000 TO it_down.
    APPEND it_down. CLEAR it_down.

  ENDLOOP.
  it_down-zcost = 'Cost Center'.
  it_down-ktext = 'Cost Center name'.
  it_down-zperg = 'EE group'.
  it_down-zobjc = 'Job code'.
  it_down-zjobk = 'JOB name '.
  it_down-zhedc = 'Head count'.
  it_down-amunt = 'Amount'.
  INSERT it_down INDEX 1.
  CLEAR it_down.


  IF wa_filename IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD wa_filename.
    IF sy-subrc NE 0.CLEAR  wa_filename.ENDIF.
  ENDIF.


  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = wa_filename
            def_path         = wa_filename
            mask             = ',*.xls.'
            mode             = 'S'
            title            = sy-title
       IMPORTING
            filename         = wa_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " DOWN_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  DATA_SORTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_sorting.


  DATA : BEGIN OF it_sort OCCURS 0,
         val1 LIKE zthr_pcp02-zval1,
         val2 LIKE zthr_pcp02-zval2,
         END OF it_sort.

  CLEAR : it_sort, it_sort[].
  SELECT zval1 zval2 INTO (it_sort-val1, it_sort-val2)
   FROM zthr_pcp02
   WHERE zmodl = '02'
     AND zgrup = '1240' .
    APPEND it_sort.
  ENDSELECT.

  LOOP AT it_9000.
* OBJID
    READ TABLE it_sort WITH KEY val2 = it_9000-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_9000-zval1.
      MODIFY it_9000.
    ENDIF.
  ENDLOOP.

  SORT it_9000
                ASCENDING BY  zcost  zperg   DESCENDING
                                     zval1   ASCENDING.
ENDFORM.                    " DATA_SORTING
