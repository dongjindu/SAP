*----------------------------------------------------------------------*
*   INCLUDE MZAHR0010F01                                               *
*----------------------------------------------------------------------*

*&-------------- ------------------------------------------------------*
*&      Form  SELECT_BASIC_DATA
*&---------------------------------------------------------------------*
FORM select_basic_data.
  CLEAR it_pcp00. REFRESH it_pcp00.
*
  CLEAR zthr_pcp00.
  SELECT zcost zpera zperg zsubg zobjc zsenr zhedc zsaly
         act01 act02 act03 ancur mthly omthly
    INTO (zthr_pcp00-zcost, zthr_pcp00-zpera, zthr_pcp00-zperg,
          zthr_pcp00-zsubg, zthr_pcp00-zobjc, zthr_pcp00-zsenr,
          zthr_pcp00-zhedc, zthr_pcp00-zsaly,
          zthr_pcp00-act01, zthr_pcp00-act02, zthr_pcp00-act03,
          zthr_pcp00-ancur, zthr_pcp00-mthly, zthr_pcp00-omthly)
    FROM zthr_pcp00 WHERE zyear = w_zyear
                      AND zmons = w_zmons
                      AND zvers = w_zvers.



    it_pcp00-zcost = zthr_pcp00-zcost.
    it_pcp00-zpera = zthr_pcp00-zpera.
    it_pcp00-zperg = zthr_pcp00-zperg.
    it_pcp00-zsubg = zthr_pcp00-zsubg.
    it_pcp00-zobjc = zthr_pcp00-zobjc.
    it_pcp00-zsenr = zthr_pcp00-zsenr.
    it_pcp00-zhedc = zthr_pcp00-zhedc.
    it_pcp00-zsaly = it_pcp00-zsalc = zthr_pcp00-mthly .
    it_pcp00-osaly = zthr_pcp00-omthly.
    it_pcp00-ancur = zthr_pcp00-ancur.
    APPEND it_pcp00. CLEAR it_pcp00.
  ENDSELECT.


  LOOP AT it_pcp00.
    CLEAR hrp1000.
    SELECT SINGLE ktext INTO it_pcp00-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_pcp00-zcost
                    AND datbi = '99991231'.
    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO it_pcp00-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
            AND ( zgrup = '1260' OR zgrup = '1270' )
            AND zctxt EQ  it_pcp00-zcost.
    ENDIF.

    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_pcp00-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    it_pcp00-zjobk = hrp1000-short.
    MODIFY it_pcp00. CLEAR it_pcp00.
  ENDLOOP.
*

  SORT it_pcp00 BY zcost zobjc.
* insert 05/25/2004
  PERFORM data_sorting.

  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE it_pcp00 LINES tc9000-lines.
ENDFORM.                    " SELECT_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE_BASICPAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_save_basicpay.
  DATA : wa_ansal LIKE zthr_pcp00-ansal,
         wa_houry LIKE zthr_pcp00-houry.


  LOOP AT it_pcp00.
    CLEAR: wa_ansal, wa_houry  .

    wa_ansal = it_pcp00-zsalc * 12 .
    wa_houry = wa_ansal / 2080 .
*Korea
    IF it_pcp00-zperg = '9' AND it_pcp00-zsubg = 'U2'.
      it_pcp00-act01 = it_pcp00-zsalc ."* it_pcp00-zhedc.
*Local
    ELSEIF it_pcp00-zperg = '1' AND
         ( it_pcp00-zsubg = 'U2' OR it_pcp00-zsubg = 'U3' ).
      it_pcp00-act02 = it_pcp00-zsalc ."* it_pcp00-zhedc.
*hourly
    ELSEIF it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U0'.
      it_pcp00-act03 = it_pcp00-zsalc ."* it_pcp00-zhedc.
*    ELSE.
*      MESSAGE i014 WITH text-001 it_pcp00-zperg '&' it_pcp00-zsubg .
    ENDIF.
    UPDATE zthr_pcp00
        SET: mthly = it_pcp00-zsalc
*             ansal = wa_ansal
*             houry = wa_houry
             act03 = it_pcp00-act03
             act02 = it_pcp00-act02
             act01 = it_pcp00-act01
      WHERE zyear = w_zyear
        AND zmons = w_zmons
        AND zvers = w_zvers
        AND zcost = it_pcp00-zcost
        AND zpera = it_pcp00-zpera
        AND zperg = it_pcp00-zperg
        AND zsubg = it_pcp00-zsubg
        AND zobjc = it_pcp00-zobjc
        AND zsenr = it_pcp00-zsenr.

  ENDLOOP.

ENDFORM.                    " DATA_SAVE_BASICPAY
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN_LOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_load_file.

  TABLES : cskt.
  DATA: BEGIN OF it_down OCCURS 0,
        zyear(40),
        zmons(40),
        zcost(40),
        ktext(40),
        zpera(40),
        zperg(40),
        zsubg(40),
        zjobk(40),
        zsenr(40),
        zhedc(40),
        zsaly(40),
        zsalc(40),
        END OF it_down.

  DATA : wa_filename    LIKE rlgrap-filename .

  CLEAR it_pcp00. REFRESH it_pcp00.
*
  CLEAR zthr_pcp00.
  SELECT zcost zpera zperg zsubg zobjc zsenr zhedc zsaly
         act01 act02 act03 zyear zmons mthly
    INTO (zthr_pcp00-zcost, zthr_pcp00-zpera, zthr_pcp00-zperg,
          zthr_pcp00-zsubg, zthr_pcp00-zobjc, zthr_pcp00-zsenr,
          zthr_pcp00-zhedc, zthr_pcp00-zsaly,
          zthr_pcp00-act01, zthr_pcp00-act02, zthr_pcp00-act03,
          zthr_pcp00-zyear, zthr_pcp00-zmons,zthr_pcp00-mthly)
    FROM zthr_pcp00 WHERE zyear = w_zyear
                      AND zvers = w_zvers.


    it_down-zyear = zthr_pcp00-zyear.
    it_down-zmons = zthr_pcp00-zmons.
    it_down-zcost = zthr_pcp00-zcost.
    it_down-zpera = zthr_pcp00-zpera.
    it_down-zperg = zthr_pcp00-zperg.
    it_down-zsubg = zthr_pcp00-zsubg.
    it_down-zsenr = zthr_pcp00-zsenr.
    it_down-zhedc = zthr_pcp00-zhedc.

*    IF ZTHR_PCP00-ZPERG = '9' AND ZTHR_PCP00-ZSUBG = 'U2'.
*      IT_DOWN-ZSALY = IT_DOWN-ZSALC = ZTHR_PCP00-ACT01.
*
*    ELSEIF ( ( ZTHR_PCP00-ZPERG = '1' AND ZTHR_PCP00-ZSUBG = 'U2' ) OR
*             ( ZTHR_PCP00-ZPERG = '1' AND ZTHR_PCP00-ZSUBG = 'U3' ) ).
*
*      IT_DOWN-ZSALY = IT_DOWN-ZSALC = ZTHR_PCP00-ACT02.
*    ELSE.
*      IT_DOWN-ZSALY = IT_DOWN-ZSALC = ZTHR_PCP00-ACT03.
*    ENDIF.
    it_down-zsaly = it_down-zsalc = zthr_pcp00-mthly.

    CLEAR hrp1000.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = zthr_pcp00-zcost
                  AND datbi = '99991231'.

    it_down-ktext = cskt-ktext.

    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = zthr_pcp00-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    it_down-zjobk = hrp1000-short.

    APPEND it_down. CLEAR it_down.
  ENDSELECT.
  SORT it_down BY zyear zmons zcost .

  it_down-zyear = 'Year'.
  it_down-zmons = 'Month'.
  it_down-zcost = 'Cost Center'.
  it_down-ktext = 'Cost Center name'.
  it_down-zpera = 'Per. area'.
  it_down-zperg = 'EE group'.
  it_down-zsubg = 'EE Subgroup'.
  it_down-zjobk = 'Job'.
  it_down-zsenr = 'Seniority'.
  it_down-zhedc = 'Head Count'.
  it_down-zsaly = 'Monthly Salary'.
  it_down-zsalc = 'Change Salary'.

  INSERT  it_down INDEX 1.

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

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " EXCEL_DOWN_LOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  data_sorting
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

  LOOP AT it_pcp00.
* OBJID
    READ TABLE it_sort WITH KEY val2 = it_pcp00-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_pcp00-zval1.
      MODIFY it_pcp00.
    ENDIF.
  ENDLOOP.
*SORT IT_AHC01  ASCENDING  BY ZCOST .
  SORT it_pcp00 BY zval1.
  SORT it_pcp00
                ASCENDING BY  zcost  zperg   DESCENDING
                                     zval1   ASCENDING
                                     zsenr   ASCENDING.

ENDFORM.                    " data_sorting
