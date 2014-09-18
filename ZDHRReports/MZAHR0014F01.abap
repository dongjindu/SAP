*----------------------------------------------------------------------*
***INCLUDE MZAHR0014F01 .
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
  DATA : wl_answer.

  SELECT  zcost zobjc zsenr zhedc zhouy annul rate1
          act08 AS amunt
          INTO CORRESPONDING FIELDS OF TABLE it_9000
          FROM zthr_pcp07
          WHERE zyear = w_zyear
            AND zpera = w_werks
            AND zvers = w_zvers
            AND zmons = w_zmons.

  IF sy-subrc EQ 0.
    PERFORM data_descrition_get.
  ELSE.
    PERFORM data_pcpxx_data_geting.
  ENDIF.

  PERFORM data_sorting .

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

  PERFORM pcp07_data_appending.
  PERFORM pcp00_data_collection.
  PERFORM data_saving.

ENDFORM.                    " SAVE_DATA_ANNUAL_BUNUS
*&---------------------------------------------------------------------*
*&      Form  JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_code_selection.
  CLEAR :  r_jobcode[],  r_jobcode.
  CLEAR :  it_val[], it_val.
**
  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1080' .

    r_jobcode-sign   = 'I'.
    r_jobcode-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_jobcode-low.
    APPEND r_jobcode.  CLEAR r_jobcode.

  ENDSELECT.
**
  SELECT zcode zctxt zval1 zval2
   INTO (zthr_pcp02-zcode,zthr_pcp02-zctxt, zthr_pcp02-zval1,
         zthr_pcp02-zval2)
     FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1085' .

    MOVE : zthr_pcp02-zcode TO it_val-zcode,
           zthr_pcp02-zval1 TO it_val-zval1,
           zthr_pcp02-zval2 TO it_val-zval2.
    APPEND it_val . CLEAR it_val.

  ENDSELECT.
ENDFORM.                    " JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  pcp07_data_appending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pcp07_data_appending.
  CLEAR : it_pcp07[], it_pcp07.
  LOOP AT it_9000.
    MOVE :
            w_zyear       TO it_pcp07-zyear,
            w_zvers       TO it_pcp07-zvers,
            w_werks       TO it_pcp07-zpera,
            w_zmons       TO it_pcp07-zmons,
            it_9000-zcost TO it_pcp07-zcost,
            it_9000-zobjc TO it_pcp07-zobjc,
            it_9000-zsenr TO it_pcp07-zsenr,
            it_9000-zhedc TO it_pcp07-zhedc,
            it_9000-zhouy TO it_pcp07-zhouy,
            it_9000-annul TO it_pcp07-annul,
            it_9000-rate1 TO it_pcp07-rate1,
            it_9000-amunt TO it_pcp07-act08,
            'USD'         TO it_pcp07-ancur,
            sy-datum      TO it_pcp07-erdat,
            sy-uzeit      TO it_pcp07-erzet,
            sy-uname      TO it_pcp07-ernam.
    APPEND it_pcp07 . CLEAR it_pcp07.
  ENDLOOP.

ENDFORM.                    " pcp07_data_appending
*&---------------------------------------------------------------------*
*&      Form  PCP00_DATA_COLLECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pcp00_data_collection.
  SELECT * INTO TABLE it_pcp00
       FROM zthr_pcp00
       WHERE zyear = w_zyear
         AND zvers = w_zvers
         AND zpera = w_werks .

  LOOP AT it_pcp00.
    READ TABLE it_9000 WITH KEY zcost = it_pcp00-zcost
                                zsenr = it_pcp00-zsenr
                                zobjc = it_pcp00-zobjc
                                zperg = '1'.

    IF sy-subrc EQ 0  AND it_9000-zhedc NE 0.

      it_pcp00-act08 =   it_9000-amunt / it_9000-zhedc .
      MODIFY it_pcp00 TRANSPORTING act08
        WHERE zyear = w_zyear
          AND zvers = w_zvers
          AND zmons = w_zmons
          AND zpera = w_werks
          AND zcost = it_pcp00-zcost
          AND zsenr = it_pcp00-zsenr
          AND zobjc = it_pcp00-zobjc
          AND zperg = '1'.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " PCP00_DATA_COLLECTION
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_saving.
  DATA : w_int TYPE i.
  DESCRIBE TABLE it_9000 LINES w_int.
  IF w_int <> 0.
    MODIFY zthr_pcp00 FROM TABLE it_pcp00.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
    ELSE.
      MODIFY zthr_pcp07 FROM TABLE it_pcp07.
      IF sy-subrc EQ 0.
        w_input = ''.
        w_save  = 'X'.
        COMMIT WORK.
       MESSAGE s001(zmhr) WITH 'Transaction was processed successfully'.
        REFRESH it_9000.
      ELSE.
        ROLLBACK WORK.
        MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
      ENDIF.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000(zmhr) WITH ' Dont Save !! Check entry ' .
  ENDIF.
ENDFORM.                    " DATA_SAVING
*&---------------------------------------------------------------------*
*&      Form  DATA_DESCRITION_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_descrition_get.
  PERFORM   job_code_selection.

  REFRESH :it_ahc01,it_count.

  SELECT  zscst zjobc zmons zhedc znewc zsenr COUNT( * ) AS zct
     INTO CORRESPONDING FIELDS OF TABLE it_count
          FROM zthr_ahc01
            WHERE zyear EQ w_zyear
              AND zvers EQ w_zvers
              AND zpera EQ w_werks
              AND zmons EQ w_zmons
              AND zcost NE space
              AND zperg NE '9'
*              AND zhedc EQ '0'
              AND znewc NE space
*              AND zsenr = '1'
              GROUP by zscst zjobc zmons zhedc znewc zsenr.

  SORT it_count BY zscst zjobc.

  SORT it_9000 BY zcost zobjc.
*New persons add at screen
  LOOP AT it_count WHERE zhedc EQ 0
                      AND zsenr = '1'.
    READ TABLE it_9000 WITH KEY zcost = it_count-zscst
                                zobjc = it_count-zjobc.
    IF sy-subrc NE 0.
      MOVE : it_count-zscst TO it_9000-zcost,
             it_count-zjobc TO it_9000-zobjc,
             '1'            TO it_9000-zsenr,
             it_count-znewc TO it_9000-zhedc.

      APPEND it_9000.
    ENDIF.
  ENDLOOP.
*Change persons
  LOOP AT it_count WHERE zhedc <> 0.
    s_people = it_count-zhedc + it_count-znewc.
    READ TABLE it_9000 WITH KEY zcost = it_count-zscst
                                zobjc = it_count-zjobc
                                zsenr = it_count-zsenr.
    IF sy-subrc EQ 0.
      IF s_people = 0.
        CLEAR it_9000-zhedc.
      ELSEIF s_people <> 0 AND it_count-znewc <> 0.
        it_9000-zhedc = it_count-znewc + it_count-zhedc.
        it_9000-zinc = 'X'.
      ENDIF.
      MODIFY it_9000 FROM it_9000 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  SORT it_9000 BY zcost zobjc zsenr .
  LOOP AT it_9000 .
* new personal
*    PERFORM new_entry_get_paid_update.
    IF it_9000-zinc = 'X'.
      PERFORM change_entry_get_paid .
    ELSE.
      PERFORM new_entry_get_paid .
    ENDIF.
    IF it_9000-zobjc IN r_jobcode.
      it_9000-divi = 'Manager'.
    ELSE.
      it_9000-divi = 'Staff'.
    ENDIF.

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
*When People is reduced
    IF it_9000-zhedc = 0.
      CLEAR : it_9000-zhedc,it_9000-zhouy,it_9000-annul,it_9000-rate1,
              it_9000-amunt,it_9000-zperg, it_9000-zval1.
    ENDIF.
    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " DATA_DESCRITION_GET
*&---------------------------------------------------------------------*
*&      Form  DATA_PCPXX_DATA_GETING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_pcpxx_data_geting.
  CLEAR : it_9000[], it_9000,s_people.
  TABLES : zthr_ahc01.
  SELECT zcost zobjc zsenr zperg
         COUNT( * )   AS zhedc
         SUM( zhouy ) AS zhouy
         SUM( ahday ) AS annul
         INTO CORRESPONDING FIELDS OF TABLE it_9000
         FROM zthr_pcpxx
         WHERE zpera = w_werks
           AND zyear = w_zyear
           AND zvers = w_zvers
           AND zperg NE '9'
*           and ZCOST = '0000011001'
           GROUP by zcost   zperg zsenr  zobjc.
*New : Cost center add
  PERFORM add_data_new_costcenter.

  PERFORM   job_code_selection.

  REFRESH :it_ahc01,it_count.

  SELECT  zscst zjobc zmons zhedc znewc zsenr COUNT( * ) AS zct
     INTO CORRESPONDING FIELDS OF TABLE it_count
          FROM zthr_ahc01
            WHERE zyear EQ w_zyear
              AND zvers EQ w_zvers
              AND zpera EQ w_werks
              AND zmons EQ w_zmons
              AND zscst NE space
              AND zperg NE '9'
*              AND zhedc EQ '0'
              AND znewc NE space
*              AND zsenr = '1'
              GROUP by zscst zjobc zmons zhedc znewc zsenr.

  SORT it_count BY zscst zjobc.
*New persons add at screen
  LOOP AT it_count WHERE zhedc EQ 0
                      AND zsenr = '1'.
    READ TABLE it_9000 WITH KEY zcost = it_count-zscst
                                zobjc = it_count-zjobc.
    IF sy-subrc NE 0.
      MOVE : it_count-zscst TO it_9000-zcost,
             it_count-zjobc TO it_9000-zobjc,
             '1'            TO it_9000-zsenr,
             it_count-znewc TO it_9000-zhedc.

      APPEND it_9000.
    ENDIF.
  ENDLOOP.
*Change persons
  LOOP AT it_count WHERE zhedc <> 0.
    s_people = it_count-zhedc + it_count-znewc.
    READ TABLE it_9000 WITH KEY zcost = it_count-zscst
                                zobjc = it_count-zjobc
                                zsenr = it_count-zsenr.
    IF sy-subrc EQ 0.
      IF s_people = 0.
        CLEAR it_9000-zhedc.
      ELSEIF s_people <> 0 AND it_count-znewc <> 0.
        it_9000-zhedc = it_count-znewc + it_count-zhedc.
        it_9000-zinc = 'X'.
      ENDIF.
      MODIFY it_9000 FROM it_9000 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  SORT it_9000 BY zcost zobjc zsenr .
  LOOP AT it_9000 .
    IF it_9000-zhedc <> 0 AND it_9000-zinc <> 'X' .
      it_9000-zhouy =  ( it_9000-zhouy * 8 ) / it_9000-zhedc .
    ELSEIF it_9000-zhedc <> 0 AND it_9000-zinc = 'X' .
      it_9000-zhouy =  ( it_9000-zhouy * 8 ).
      it_9000-annul = it_9000-annul * it_9000-zhedc.
    ENDIF.
* new personal
    IF it_9000-zinc = 'X'.
      PERFORM change_entry_get_paid .
    ELSE.
      PERFORM new_entry_get_paid .
    ENDIF.
    IF it_9000-zobjc IN r_jobcode.
      it_9000-divi = 'Manager'.
    ELSE.
      it_9000-divi = 'Staff'.
    ENDIF.

    SELECT SINGLE ktext INTO it_9000-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_9000-zcost
                  AND datbi = '99991231'.

    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO it_9000-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND ( zgrup = '1260' OR zgrup = '1270' )
             AND zctxt EQ  it_9000-zcost.
    ENDIF.
    SELECT SINGLE short INTO it_9000-zjobk
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_9000-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
*When People is reduced
    IF it_9000-zhedc = 0.
      CLEAR : it_9000-zhedc,it_9000-zhouy,it_9000-annul,it_9000-rate1,
              it_9000-amunt,it_9000-zperg,it_9000-zval1.
    ENDIF.
    MODIFY it_9000.
  ENDLOOP.

ENDFORM.                    " DATA_PCPXX_DATA_GETING
*&---------------------------------------------------------------------*
*&      Form  DOWN_LOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_load_excel_file.
  DATA : wa_filename    LIKE rlgrap-filename .

  DATA: BEGIN OF it_down OCCURS 0,
        zcost(20),
        ktext(30) ,
        zjobk(30) ,
        zsenr(30),                       " Seniority
        divi(10) ,                       " Division production
        zhedc(12)  ,
        annul(20) ,
        rate1(10) ,
        amunt(20) ,
       END OF it_down.

  LOOP AT it_9000.

    MOVE-CORRESPONDING it_9000 TO it_down.
    APPEND it_down . CLEAR it_down.

  ENDLOOP.

  it_down-zcost = 'Cost Center'.
  it_down-ktext = 'Name '.
  it_down-zjobk = 'Job name'.
  it_down-divi  = 'Division'.
  it_down-zsenr = 'Seniority'.
  it_down-zhedc = 'Head Count'.
  it_down-annul = 'Anual holiday'.
  it_down-rate1 = 'Rate'.
  it_down-amunt = 'Amount'.

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
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " DOWN_LOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  NEW_ENTRY_GET_PAID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_entry_get_paid .

  DATA : l_zhouy LIKE zthr_pcpxx-zhouy,
         l_ansal LIKE zthr_pcpxx-ansal,
         l_zmons LIKE zthr_pcp00-zmons.
  REFRESH it_man.

  SELECT zmons zscst zjobk zperg
         SUM( znewc ) AS znewc
        INTO TABLE it_man
        FROM zthr_ahc01
        WHERE zpera = w_werks
          AND zyear = w_zyear
          AND zvers = w_zvers
          AND zmons = w_zmons
          AND zscst = it_9000-zcost
          AND zjobc = it_9000-zobjc
          AND znewc > '0'
          AND zhedc = 0
          GROUP by zmons zscst zjobk zperg   .

  SORT it_man BY  zscst zjobk  zmons .

  CLEAR : l_zhouy ,l_zhouy.

  LOOP AT it_man.
    IF it_man-znewc NE 0.
      PERFORM new_person_annul .
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " NEW_ENTRY_GET_PAID
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
                ASCENDING BY  zcost  zval1   ASCENDING .

ENDFORM.                    " DATA_SORTING
*&---------------------------------------------------------------------*
*&      Form  new_entry_get_paid_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_entry_get_paid_update.
  DATA : BEGIN OF it_man OCCURS 0 ,
         zmons LIKE  zthr_ahc01-zmons,
         zscst LIKE  zthr_ahc01-zscst,
         zjobk LIKE  zthr_ahc01-zjobk,
         zperg LIKE  zthr_ahc01-zperg,
         znewc LIKE  zthr_ahc01-znewc,
   END OF it_man.

  DATA : l_zhouy LIKE zthr_pcpxx-zhouy,
         l_ansal LIKE zthr_pcpxx-ansal,
         l_zmons LIKE zthr_pcp00-zmons.

  SELECT zmons zscst zjobk zperg
         SUM( znewc ) AS znewc
        INTO TABLE it_man
        FROM zthr_ahc01
        WHERE zpera = w_werks
          AND zyear = w_zyear
          AND zvers = w_zvers
          AND zmons = w_zmons
          AND zscst = it_9000-zcost
          AND zjobc = it_9000-zobjc
          AND znewc > '0'
          AND zhedc = 0
          GROUP by zmons zscst zjobk zperg   .

  SORT it_man BY  zscst zjobk  zmons .

  CLEAR : l_zhouy ,l_zhouy.

  LOOP AT it_man.
    IF it_man-znewc NE 0.
      SELECT SINGLE zval5 INTO zthr_pcp02-zval5
                   FROM zthr_pcp02
                   WHERE zmodl = '02'
                      AND zgrup = '1250'
                      AND zval2 = '1' "it_9000-zperg
                      AND zval4 = it_9000-zobjc.

      MOVE : zthr_pcp02-zval5 TO l_ansal .

      l_zhouy = ( l_ansal / 2080 ) * 8 .
      it_9000-zhouy = l_zhouy ."+ it_9000-zhouy .
      it_9000-zsenr = 1.
      READ TABLE it_val WITH KEY zcode = '1' .
      READ TABLE r_jobcode WITH KEY low = it_man-zjobk.
      IF sy-subrc = 0.
        it_9000-annul =  it_val-zval1 / 12 * it_9000-zhedc.
      ELSE.
        it_9000-annul = it_val-zval2 / 12 * it_9000-zhedc.
      ENDIF.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " new_entry_get_paid_update
*&---------------------------------------------------------------------*
*&      Form  newperson_annul
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_person_annul.

  SELECT SINGLE zval5 INTO zthr_pcp02-zval5
               FROM zthr_pcp02
               WHERE zmodl = '02'
                  AND zgrup = '1250'
                  AND zval2 = '1' "it_9000-zperg
                  AND zval4 = it_9000-zobjc.

  MOVE : zthr_pcp02-zval5 TO l_ansal .

  l_zhouy = ( l_ansal / 2080 ) * 8 .
  it_9000-zhouy = l_zhouy ."+ it_9000-zhouy .
*      it_9000-zhedc = it_9000-zhedc + it_man-znewc .
  it_9000-zhedc = it_man-znewc .
  it_9000-zsenr = 1.
  READ TABLE it_val WITH KEY zcode = '1' .
  READ TABLE r_jobcode WITH KEY low = it_man-zjobk.
  IF sy-subrc = 0.
    it_9000-annul =  it_val-zval1 / 12 * it_9000-zhedc.
  ELSE.
    it_9000-annul = it_val-zval2 / 12 * it_9000-zhedc.
  ENDIF.

ENDFORM.                    " newperson_annul
*&---------------------------------------------------------------------*
*&      Form  CHANGE_entry_get_paid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_entry_get_paid.
  CLEAR : l_ansal ,l_zhouy.
  SELECT SINGLE zval5 INTO zthr_pcp02-zval5
               FROM zthr_pcp02
               WHERE zmodl = '02'
                  AND zgrup = '1250'
                  AND zval2 = '1' "it_9000-zperg
                  AND zval4 = it_9000-zobjc.

  MOVE : zthr_pcp02-zval5 TO l_ansal .

  l_zhouy = ( l_ansal / 2080 ) * 8 .
  it_9000-zhouy = l_zhouy ."+ it_9000-zhouy .
*      it_9000-zhedc = it_9000-zhedc + it_man-znewc .
*  it_9000-zhedc = 1 .
  READ TABLE it_val WITH KEY zcode = 1.
  READ TABLE r_jobcode WITH KEY low = it_9000-zobjc.
  IF sy-subrc = 0.
    it_9000-annul =  ( ( it_val-zval1 + it_9000-zsenr )
                       * it_9000-zhedc ) / 12.
  ELSE.
    it_9000-annul =  ( ( it_val-zval2 + it_9000-zsenr )
                       * it_9000-zhedc ) / 12.

  ENDIF.
ENDFORM.                    " CHANGE_entry_get_paid
*&---------------------------------------------------------------------*
*&      Form  add_data_New_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_data_new_costcenter.
  DATA : lt_pcp02 LIKE zthr_pcp02 OCCURS 0 WITH HEADER LINE,
         w_int TYPE i.
  RANGES: r_paid FOR pa0001-stell.

  CLEAR :  r_paid[],r_paid,lt_pcp02[],lt_pcp02.
  CLEAR : zthr_pcp02,w_int.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1080' .

    r_paid-sign   = 'I'.
    r_paid-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_paid-low.

    APPEND r_paid.  CLEAR r_paid.

  ENDSELECT.
  DELETE r_paid WHERE sign = ''.
  IF r_paid[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1080 Job Code'.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_pcp02
      FROM zthr_pcp02
       WHERE zmodl EQ '02'
        AND ( zgrup = '1260' OR zgrup = '1270' ).


  DELETE ADJACENT DUPLICATES FROM lt_pcp02 COMPARING zctxt.
  DESCRIBE TABLE lt_pcp02 LINES w_int.
  IF w_int <> 0.
    LOOP AT lt_pcp02.
      SELECT zcost zobjc zsenr zperg
             SUM( zhedc )   AS  zhedc
             SUM( houry ) AS  zhouy
*         SUM( ansal ) AS  ahday
             APPENDING CORRESPONDING FIELDS OF TABLE it_9000
             FROM zthr_pcp00
             WHERE zpera = w_werks
               AND zyear = w_zyear
               AND zvers = w_zvers
               AND zmons = w_zmons
               AND zperg NE '9'
               AND zcost EQ lt_pcp02-zctxt
               GROUP by zcost   zperg zsenr  zobjc.
    ENDLOOP.
    LOOP AT it_9000.
      READ TABLE lt_pcp02 WITH KEY zctxt = it_9000-zcost.
      IF sy-subrc = 0.
        READ TABLE r_paid WITH KEY low = it_9000-zobjc.
        IF sy-subrc = 0.
          it_9000-annul = ( 15 / 12 ) * it_9000-zhedc.
        ELSE.
          it_9000-annul = ( 10 / 12 ) * it_9000-zhedc.
        ENDIF.
        MODIFY it_9000 FROM it_9000 TRANSPORTING annul.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " add_data_New_costcenter
