*----------------------------------------------------------------------*
***INCLUDE MZAHR0013F01 .
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
  DATA : wl_answer,
         w_line TYPE i.

*  DESCRIBE TABLE IT_9000 LINES W_LINE.
*
*  IF W_LINE > 0 .
*    CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*         EXPORTING
*              TEXTLINE1 = 'Do you want to'
*              TEXTLINE2 = 'continue? '
*              TITEL     = 'Data will be lost'
*         IMPORTING
*              ANSWER    = WL_ANSWER.
*    IF WL_ANSWER <> 'J'.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
  CLEAR: it_9000, it_9000[].

  SELECT
         zperg
         zobjc
         zsenr
         zhedc
         zsaly
         bonus
         rate1
         ezsaly AS act01
         erate1 AS rate2
         monbonus AS motha
         expbouns AS amunt
         INTO CORRESPONDING FIELDS OF TABLE it_9000
         FROM zthr_pcp06
         WHERE zyear = w_zyear
           AND zvers = w_zvers
           AND zpera = w_werks .
  IF sy-subrc EQ 0.
    PERFORM it_9000_descirtion.
  ELSE.
    PERFORM it_9000_collection.
  ENDIF.

  PERFORM sort_it_900.


* TABLE CONTROL
  REFRESH CONTROL 'TC_9000' FROM SCREEN 9000.
  DESCRIBE TABLE it_9000 LINES tc_9000-lines.

ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_ANNUAL_BUNUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_annual_bunus.




  PERFORM zt06_table_data_collection.
  PERFORM zt00_table_insert.

ENDFORM.                    " SAVE_DATA_ANNUAL_BUNUS
*&---------------------------------------------------------------------*
*&      Form  ZT06_TABLE_DATA_COLLECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zt06_table_data_collection.
  CLEAR :it_pcp06, it_pcp06.

  LOOP AT it_9000.
    MOVE :
          w_zyear      TO it_pcp06-zyear,
          w_zvers      TO it_pcp06-zvers,
          w_werks      TO it_pcp06-zpera,
          it_9000-zperg TO it_pcp06-zperg,
          it_9000-zobjc TO it_pcp06-zobjc,
          it_9000-zsenr TO it_pcp06-zsenr,
          it_9000-zhedc TO it_pcp06-zhedc,
          it_9000-zsaly TO it_pcp06-zsaly,
          it_9000-bonus TO it_pcp06-bonus,
          it_9000-rate1 TO it_pcp06-rate1,
          it_9000-act01 TO it_pcp06-ezsaly,
          it_9000-rate2 TO it_pcp06-erate1,
          it_9000-motha TO it_pcp06-monbonus,
          it_9000-amunt TO it_pcp06-expbouns,
          'USD'         TO it_pcp06-ancur,
          sy-datum      TO it_pcp06-erdat,
          sy-uname      TO it_pcp06-ernam,
          sy-uzeit      TO it_pcp06-erzet.

    APPEND it_pcp06 . CLEAR it_pcp06.

  ENDLOOP.

ENDFORM.                    " ZT06_TABLE_DATA_COLLECTION
*&---------------------------------------------------------------------*
*&      Form  ZT00_TABLE_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zt00_table_insert.
  DATA : it_00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.

  DATA : w_head TYPE i,
         w_head2 TYPE i.

  SELECT * INTO TABLE it_00
           FROM zthr_pcp00
           WHERE zpera = w_werks
             AND zyear = w_zyear
             AND zvers = w_zvers .
*Change date : 2004.06.21 request by NJJ
*  IF W_SAVE = 'X'.
*    MESSAGE E000(ZMHR) WITH 'Saved Bonus '.
*  ENDIF.
*****************************************
  IF sy-subrc NE 0.
    MESSAGE e000(zmhr) WITH 'NO date for update'.
  ENDIF.
  LOOP AT it_00.
    READ TABLE it_9000 WITH KEY zobjc  = it_00-zobjc
                                zsenr  = it_00-zsenr
                                zperg  = it_00-zperg .
    IF sy-subrc EQ 0.
      IF it_9000-zhedc <> 0.
*in case of RATE & Currency
        it_00-act05 = it_9000-amunt / 12.

*        it_00-act05 =
*          ( ( it_9000-amunt / it_9000-zhedc )  * it_00-zhedc )  / 12  .
      ENDIF.
    ENDIF.
    MODIFY it_00.

  ENDLOOP.
*
  MODIFY zthr_pcp00 FROM TABLE it_00.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
  ELSE.
    MODIFY zthr_pcp06 FROM TABLE it_pcp06.
    IF sy-subrc EQ 0.
      w_input = ''.
      w_save  = 'X'.
      COMMIT WORK.
      MESSAGE s001(zmhr) WITH 'Transaction was processed successfully'.

    ELSE.
      ROLLBACK WORK.
      MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
    ENDIF.
  ENDIF.

ENDFORM.                    " ZT00_TABLE_INSERT
*&---------------------------------------------------------------------*
*&      Form  IT_9000_COLLECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_9000_collection.
  CLEAR : it_9000[], it_9000.


**  SELECT ZOBJC
**         ZPERG
****         COUNT( * )   AS ZHEDC
***         SUM( ZSALY ) AS ZSALY
***         SUM( BONUS ) AS BONUS
**          ZSALY
**          BONUS
**         INTO CORRESPONDING FIELDS OF TABLE IT_PCPXX
**         FROM ZTHR_PCPXX
**         WHERE ZPERA = W_WERKS
**           AND ZYEAR = W_ZYEAR
**           AND ZVERS = W_ZVERS .
***           GROUP BY ZOBJC  ZPERG .
**
**  LOOP AT IT_PCPXX.
**    MOVE : IT_PCPXX-ZOBJC TO IT_9000-ZOBJC,
**           IT_PCPXX-ZPERG TO IT_9000-ZPERG,
***          IT_PCPXX-ZSALY TO IT_9000-ZSALY,
**           IT_PCPXX-BONUS TO IT_9000-BONUS.
***           IT_PCPXX-ZHEDC TO IT_9000-ZHEDC.
***          '1'             TO IT_9000-ZHEDC .
**
***
***   IT_9000-ZSALY = IT_9000-ZSALY * 12 .
**
**    COLLECT IT_9000. CLEAR IT_9000.
**  ENDLOOP.
**
**  SELECT ZOBJC
**         ZPERG
**         ZSUBG
**         SUM( ZHEDC ) AS ZHEDC
**         SUM( ACT01 ) AS ACT01
**         SUM( ACT02 ) AS ACT02
**         SUM( ACT03 ) AS ACT03
**         INTO CORRESPONDING FIELDS OF TABLE IT_PCP00
**         FROM ZTHR_PCP00
**         WHERE ZPERA = W_WERKS
**           AND ZYEAR = W_ZYEAR
**           AND ZVERS = W_ZVERS
**           GROUP BY ZOBJC  ZPERG  ZSUBG.
**
**  LOOP AT IT_PCP00.
**    MOVE : IT_PCP00-ZOBJC TO IT_9000-ZOBJC,
**           IT_PCP00-ZPERG TO IT_9000-ZPERG,
**           IT_PCP00-ZHEDC TO IT_9000-ZHEDC.
**
**    IF IT_PCP00-ZPERG = '9' AND IT_PCP00-ZSUBG = 'U2'.
**      MOVE : IT_PCP00-ACT01 TO IT_9000-ACT01.
**    ELSEIF ( IT_PCP00-ZPERG = '1' AND IT_PCP00-ZSUBG = 'U2' ) OR
**           ( IT_PCP00-ZPERG = '1' AND IT_PCP00-ZSUBG = 'U3' ) .
**      MOVE : IT_PCP00-ACT02 TO IT_9000-ACT01.
**    ELSE.
**      MOVE : IT_PCP00-ACT03 TO IT_9000-ACT01.
**    ENDIF.
**
**    IT_9000-ZHEDC = IT_9000-ZHEDC / 12 .
**
**    IT_9000-ZSALY = IT_9000-ACT01 / ( 104 / 100 ) * IT_9000-ZHEDC.
**    IT_9000-ACT01 = IT_9000-ACT01  .
**    COLLECT IT_9000 . CLEAR IT_9000.
**
**  ENDLOOP.
*
*  SELECT zobjc zperg zsenr "zsubg
*         SUM( zhedc ) AS zhedc
*         AVG( ansal ) AS ansal
*         INTO CORRESPONDING FIELDS OF TABLE it_pcp00
*         FROM zthr_pcp00
*         WHERE zpera = w_werks
*           AND zyear = w_zyear
*           AND zvers = w_zvers
*           and ZPERG = '1'
*           GROUP by zobjc  zperg zsenr ."zsubg.
*
*  LOOP AT it_pcp00.
*
*    SELECT SUM( bonus ) INTO it_pcp00-act05
*          FROM zthr_pcpxx
*          WHERE zpera = w_werks
*            AND zyear = w_zyear
*            AND zvers = w_zvers
*            AND zsenr = it_pcp00-zsenr
*            AND zobjc = it_pcp00-zobjc
*            AND zperg = it_pcp00-zperg.
*
*    MOVE : it_pcp00-zobjc TO it_9000-zobjc,
*           it_pcp00-zperg TO it_9000-zperg,
*           it_pcp00-zsenr TO it_9000-zsenr,
*           it_pcp00-ansal TO it_9000-zsaly,
*           it_pcp00-act05 TO it_9000-bonus,
*           it_pcp00-zhedc TO it_9000-zhedc.
**    IF it_9000-zhedc NE 1.
*      it_9000-zhedc = it_9000-zhedc / 12 .
**    ENDIF.
*    it_9000-zsaly = it_9000-zsaly  * it_9000-zhedc.
*    it_9000-act01 = it_9000-zsaly * ( 104 / 100 )  .
*
*    COLLECT it_9000 . CLEAR it_9000.
*
*
*  ENDLOOP.
*
*
*  LOOP AT it_9000.
*    IF it_9000-zsaly NE 0.
*      it_9000-rate1 = ( it_9000-bonus / it_9000-zsaly ) * 100 .
*    ENDIF.
*
*    SELECT SINGLE short INTO it_9000-zjobk
*    FROM hrp1000 WHERE plvar = '01'
*                   AND otype = 'C'
*                   AND objid = it_9000-zobjc
*                   AND istat = '1'
*                   AND endda = '99991231'
*                   AND langu = sy-langu.
*    MODIFY it_9000.
*  ENDLOOP.
*
  SELECT zobjc zperg zsenr "zsubg
           COUNT(*) AS zhedc
           AVG( ansal ) AS ansal
           INTO CORRESPONDING FIELDS OF TABLE it_pcp00
           FROM zthr_pcpxx
           WHERE zpera = w_werks
             AND zyear = w_zyear
             AND zvers = w_zvers
             AND zperg = '1'
             GROUP by zobjc  zperg zsenr ."zsubg.

  LOOP AT it_pcp00.

    SELECT SUM( bonus ) INTO it_pcp00-act05
          FROM zthr_pcpxx
          WHERE zpera = w_werks
            AND zyear = w_zyear
            AND zvers = w_zvers
            AND zsenr = it_pcp00-zsenr
            AND zobjc = it_pcp00-zobjc
            AND zperg = it_pcp00-zperg.

    MOVE : it_pcp00-zobjc TO it_9000-zobjc,
           it_pcp00-zperg TO it_9000-zperg,
           it_pcp00-zsenr TO it_9000-zsenr,
           it_pcp00-ansal TO it_9000-zsaly,
           it_pcp00-act05 TO it_9000-bonus,
           it_pcp00-zhedc TO it_9000-zhedc.
*    IF it_9000-zhedc NE 1.
*    it_9000-zhedc = it_9000-zhedc / 12 .
*    ENDIF.
    it_9000-zsaly = it_9000-zsaly  * it_9000-zhedc.
    it_9000-act01 = it_9000-zsaly * ( 104 / 100 )  .

    COLLECT it_9000 . CLEAR it_9000.

  ENDLOOP.

  LOOP AT it_9000.
    IF it_9000-zsaly NE 0.
      it_9000-rate1 = ( it_9000-bonus / it_9000-zsaly ) * 100 .
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

ENDFORM.                    " IT_9000_COLLECTION
*&---------------------------------------------------------------------*
*&      Form  IT_9000_DESCIRTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_9000_descirtion.
  LOOP AT it_9000.


    SELECT SINGLE short INTO it_9000-zjobk
    FROM hrp1000 WHERE plvar = '01'
                   AND otype = 'C'
                   AND objid = it_9000-zobjc
                   AND istat = '1'
                   AND endda = '99991231'
                   AND langu = sy-langu.

    MODIFY it_9000.
  ENDLOOP.

ENDFORM.                    " IT_9000_DESCIRTION
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

  DATA: BEGIN OF it_down OCCURS 0,
        zjobk(40),
        zperg(40),
        zsenr(40),
        zhedc(40),
        zsaly(40),
        bonus(40),
        rate1(40),
        act01(40),
        rate2(40),
        motha(40),
        amunt(40),
       END OF it_down.

  CLEAR : it_down, it_down[].

  LOOP AT it_9000.
    MOVE-CORRESPONDING it_9000 TO it_down.
    APPEND it_down. CLEAR it_down.

  ENDLOOP.

  it_down-zjobk = 'JOB'.
  it_down-zperg = 'Per. area'.
  it_down-zsenr = 'Seniority'.
  it_down-zhedc = 'Head count'.
  it_down-zsaly = 'Salery'.
  it_down-bonus = 'Bouns'.
  it_down-rate1 = 'Rate'.
  it_down-act01 = 'Salery'.
  it_down-rate2 = 'Rate'.
  it_down-motha = 'Monthly Bouns'.
  it_down-amunt = 'Bouns amount'.
  INSERT it_down INDEX 1.
  CLEAR it_down.
  it_down-zjobk = 'Previous Year'.
  it_down-act01 = 'Next year'.
  INSERT it_down INDEX 1.


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





ENDFORM.                    " EXCEL_DOWN_LOAD
*&---------------------------------------------------------------------*
*&      Form  sort_it_900
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_it_900.
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
    READ TABLE it_sort WITH KEY val2 = it_9000-zobjc .
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_9000-zval1.
      MODIFY it_9000.
    ENDIF.
  ENDLOOP.
*SORT IT_AHC01  ASCENDING  BY ZCOST .
  SORT it_9000
                ASCENDING BY   zperg zval1 zsenr DESCENDING.
ENDFORM.                    " sort_it_900
