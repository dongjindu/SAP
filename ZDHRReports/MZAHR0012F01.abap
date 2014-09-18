*----------------------------------------------------------------------*
***INCLUDE MZAHR0012F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_main_data.
  RANGES      r_attjo FOR zthr_pcp00-zobjc.

  CLEAR : r_attjo , r_attjo[].

  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1065' .

    r_attjo-sign   = 'I'.
    r_attjo-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_attjo-low.

    APPEND r_attjo.  CLEAR r_attjo.

  ENDSELECT.
  DELETE r_attjo WHERE sign = ''.
  IF r_attjo[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1065 Job Code'.
  ENDIF.

  SELECT zcost zobjc  zperg  zsubg  zsenr SUM( zhedc ) AS zhedc
         SUM( act04 ) AS amunt ancur
    INTO  CORRESPONDING FIELDS OF TABLE it_9000
    FROM zthr_pcp00
    WHERE zyear = w_zyear
      AND zvers = w_zvers
      AND zpera = w_werks
      AND zmons = w_zmons
      AND zobjc IN r_attjo
      AND NOT ( zperg = '9' AND zsubg = 'U2' )
*                ( ZPERG = '1' AND ZSUBG = 'U2' ) OR
*                ( ZPERG = '1' AND ZSUBG = 'U3' ) )
      GROUP by zcost zobjc  zperg  zsubg  zsenr  ancur.
* CODE BOOK SELECTION
  SELECT zval1 zcode
        INTO (zthr_pcp02-zval1,zthr_pcp02-zcode)
        FROM zthr_pcp02
        WHERE zmodl = '02'
         AND zgrup = '1070'
         AND zcode  IN ('10000','10010').

    CASE zthr_pcp02-zcode.
      WHEN '10000'.                                         " 01~06
        MOVE : zthr_pcp02-zval1 TO it_9000-zval1 .
      WHEN '10010' .
        MOVE : zthr_pcp02-zval1 TO it_9000-zval2.
    ENDCASE.
  ENDSELECT.

  MODIFY  it_9000  TRANSPORTING zval1  zval2  WHERE zval1 = space.

* TEXT SELECTION
  LOOP AT it_9000.
    it_9000-amunt = it_9000-amunt * it_9000-zhedc.

    SELECT SINGLE ktext INTO it_9000-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_9000-zcost
                  AND datbi = '99991231'.

    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO it_9000-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND zgrup EQ '1260'
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
  PERFORM data_sorting.

  REFRESH CONTROL 'TC_9000' FROM SCREEN 9000.
  DESCRIBE TABLE it_9000 LINES tc_9000-lines.
ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN_LOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_load.
  DATA : wa_filename    LIKE rlgrap-filename .
  DATA : BEGIN OF it_xx OCCURS 0 ,
         zyear LIKE zthr_pcp00-zyear,
         zmons LIKE zthr_pcp00-zmons.
          INCLUDE STRUCTURE it_9000.
  DATA : END OF it_xx.

  DATA: BEGIN OF it_down  OCCURS 0,
         zyear(40),
         zmons(40),
         zcost(40), " Cost Center
         ktext(40),
         zjobk(40),
         zperg(40),
         zsubg(40),
         zsenr(40),
         zhedc(40),
         zval1(40),
         zval2(40),
         amunt(40),
      END OF it_down.

  CLEAR : it_down , it_down[].

  SELECT zyear zmons zcost zobjc  zperg  zsubg
         zsenr SUM( zhedc ) AS zhedc
         SUM( act03 ) AS amunt ancur
    INTO  CORRESPONDING FIELDS OF TABLE it_xx
    FROM zthr_pcp00
    WHERE zyear = w_zyear
      AND zvers = w_zvers
      AND zpera = w_werks
*      AND ZMONS = W_ZMONS
      AND NOT ( ( zperg = '9' AND zsubg = 'U2' ) OR
                ( zperg = '1' AND zsubg = 'U2' ) OR
                ( zperg = '1' AND zsubg = 'U3' ) )

      GROUP by zyear zmons zcost zobjc  zperg
               zsubg  zsenr  ancur.


* RATE :


* CODE BOOK SELECTION
*
  SELECT zval1 zcode
        INTO (zthr_pcp02-zval1,zthr_pcp02-zcode)
        FROM zthr_pcp02
        WHERE zmodl = '02'
         AND zgrup = '1070'
         AND zcode  IN ('10000','10010').

    CASE zthr_pcp02-zcode.
      WHEN '10000'.                                         " 01~06
        MOVE : zthr_pcp02-zval1 TO it_xx-zval1 .
      WHEN '10010' .
        MOVE : zthr_pcp02-zval1 TO it_xx-zval2.
    ENDCASE.
  ENDSELECT.

  MODIFY  it_xx TRANSPORTING zval1  zval2  WHERE zval1 = space.

* TEXT SELECTION
  LOOP AT it_xx.

    SELECT SINGLE ktext INTO it_xx-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_xx-zcost
                  AND datbi = '99991231'.

    SELECT SINGLE short INTO it_xx-zjobk
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_xx-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    MODIFY it_xx .
  ENDLOOP.

  LOOP AT it_xx.
    MOVE-CORRESPONDING it_xx   TO it_down.
    APPEND it_down . CLEAR it_down.
  ENDLOOP.

  it_down-zyear  = 'YEAR'.
  it_down-zmons  = 'MONTH'.
  it_down-zcost  = 'Cost Center'.
  it_down-ktext  = 'Cost Center name'.
  it_down-zjobk  = 'Job'.
  it_down-zperg  = 'EE group '.
  it_down-zsubg  = 'EE subgroup '.
  it_down-zsenr  = 'Seniority'.
  it_down-zhedc  = 'Head count'.
  it_down-zval1  = 'Pay rate'.
  it_down-zval2  = 'Attendance rate'.
  it_down-amunt  = 'Amount'.
  INSERT it_down INDEX  1.

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





ENDFORM.                    " EXCEL_DOWN_LOAD
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

  LOOP AT it_9000.
* OBJID
    READ TABLE it_sort WITH KEY val2 = it_9000-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_9000-xx.
      MODIFY it_9000.
    ENDIF.
  ENDLOOP.
*SORT IT_9000  ASCENDING  BY ZCOST .
  SORT it_9000
                ASCENDING BY  zcost  zperg   DESCENDING
                                     xx      ASCENDING
                                     zsenr   ASCENDING.

ENDFORM.                    " data_sorting
