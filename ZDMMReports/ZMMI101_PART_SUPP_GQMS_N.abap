************************************************************************
* Program Name      : ZMMI101_PART_SUPP_GQMS_N
* Author            : Furong Wang
* Creation Date     : 12/04/2007
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : GQMS (SARTS & SUPPLIER)
* Modification Logs
* Date       Developer    RequestNo    Description
* 03.20.2014 Victor       send data whenever the valuation class
*                         in the material master is changed.
* 08.27.2014 Victor       Country Key issue
************************************************************************
REPORT zmmi101_part_supp_gqms_n NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID zmpp.


DATA: it_data  LIKE TABLE OF ztmm_part_supp_n WITH HEADER LINE,
      it_data2 LIKE TABLE OF ztmm_part_supp_n WITH HEADER LINE.

CONSTANTS: c_dest(10) VALUE 'WMHR01'.   "Interface Destination.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_datum FOR sy-datum OBLIGATORY.
PARAMETERS: p_all(1).
SELECTION-SCREEN SKIP.
PARAMETERS: p_send(1) DEFAULT  'X'.
SELECTION-SCREEN END OF BLOCK bl1.
*SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-T02.
*PARAMETERS: P_DAYS TYPE I.
*SELECTION-SCREEN END OF BLOCK BL2.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.
  PERFORM read_data.
  PERFORM save_data.
  IF p_send IS INITIAL.
  ELSE.
    PERFORM send_rtn.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  DATA: BEGIN OF lt_data OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE it_data-lifnr,
        erdate LIKE it_data-erdate,
        udate LIKE it_data-udate,
        maktg LIKE it_data-maktg,
        name1 LIKE it_data-name1,
        END OF lt_data.

  DATA: BEGIN OF lt_mbew OCCURS 0,
        matnr LIKE mbew-matnr,
        bklas LIKE mbew-bklas,
        END OF lt_mbew.

  DATA: lt_cdhdr LIKE TABLE OF cdhdr WITH HEADER LINE,
        lt_data_del LIKE TABLE OF lt_data WITH HEADER LINE,
        lw_data LIKE it_data.

*-03.20.2014 Victor
  DATA : BEGIN OF lt_cdhdr2 OCCURS 0.
          INCLUDE STRUCTURE cdhdr.
  DATA :   value_new LIKE cdpos-value_new,
           part_no   LIKE ztmm_part_supp_n-part_no,
         END OF lt_cdhdr2.
  DATA : lt_cdpos LIKE cdpos OCCURS 0 WITH HEADER LINE.

  DATA: l_objectid_10(10),
        l_objectid_werks(4),
        l_index LIKE sy-tabix,
        l_index_d LIKE sy-tabix.

  IF p_all IS INITIAL.
    SELECT * INTO TABLE lt_cdhdr
      FROM cdhdr
      WHERE objectclas IN ('ORDERBUCH', 'INFOSATZ')
        AND udate IN s_datum.
    SORT lt_cdhdr BY objectclas objectid change_ind.
    DELETE ADJACENT DUPLICATES FROM lt_cdhdr
             COMPARING objectclas objectid change_ind.

    LOOP AT lt_cdhdr.
      l_objectid_10 = lt_cdhdr-objectid+0(10).
      l_objectid_werks = lt_cdhdr-objectid+18(4).
** Info record change
      IF lt_cdhdr-objectclas = 'INFOSATZ'.
        CASE lt_cdhdr-change_ind.
          WHEN 'I'.
            SELECT a~matnr a~lifnr a~erdat AS erdate maktg name1
              INTO CORRESPONDING FIELDS OF TABLE lt_data
              FROM eina AS a
              INNER JOIN mara AS b
              ON a~matnr = b~matnr
              INNER JOIN makt AS c
              ON b~matnr = c~matnr
              INNER JOIN lfa1 AS d
              ON a~lifnr = d~lifnr
              WHERE infnr = l_objectid_10
                AND loekz = ' '
                AND b~mtart = 'ROH' .
            SORT lt_data BY matnr lifnr.
            LOOP AT lt_data.
              MOVE-CORRESPONDING lt_data TO it_data.
              it_data-part_no = lt_data-matnr+0(15).
              it_data-matnr1 = lt_data-matnr+0(5).
              it_data-matnr2 = lt_data-matnr+5(10).
              it_data-usage0 = '*'.
              it_data-company = 'A'.
              it_data-crdate = sy-datum.
              APPEND it_data.
            ENDLOOP.
          WHEN 'U'.
            SELECT a~matnr a~lifnr maktg name1
              INTO CORRESPONDING FIELDS OF TABLE lt_data
              FROM eina AS a
              INNER JOIN mara AS b
              ON a~matnr = b~matnr
              INNER JOIN makt AS c
              ON b~matnr = c~matnr
              INNER JOIN lfa1 AS d
              ON a~lifnr = d~lifnr
              WHERE infnr = l_objectid_10
                AND loekz <> ' '
                AND b~mtart = 'ROH' .
            SORT lt_data BY matnr lifnr.
            LOOP AT lt_data.
              MOVE-CORRESPONDING lt_data TO it_data.
              it_data-part_no = lt_data-matnr+0(15).
              it_data-part_no = lt_data-matnr+0(15).
              it_data-matnr1 = lt_data-matnr+0(5).
              it_data-matnr2 = lt_data-matnr+5(10).
              it_data-usage0 = '*'.
              it_data-company = 'A'.
              it_data-udate = lt_cdhdr-udate.
              it_data-crdate = sy-datum.
              APPEND it_data.
            ENDLOOP.
        ENDCASE.
      ELSE.
** Source list change
        SELECT a~matnr a~lifnr maktg name1
        INTO CORRESPONDING FIELDS OF TABLE lt_data
        FROM eord AS a
        INNER JOIN mara AS b
        ON a~matnr = b~matnr
        INNER JOIN makt AS c
        ON b~matnr = c~matnr
        INNER JOIN lfa1 AS d
        ON a~lifnr = d~lifnr
        WHERE a~matnr = l_objectid_10
          AND a~werks = l_objectid_werks
          AND ( a~flifn = 'X' OR a~febel = 'X' )
          AND b~mtart = 'ROH'.
        IF sy-subrc = 0.
          SORT lt_data BY matnr lifnr.
          LOOP AT lt_data.
            MOVE-CORRESPONDING lt_data TO it_data.
            it_data-part_no = lt_data-matnr+0(15).
            it_data-matnr1 = lt_data-matnr+0(5).
            it_data-matnr2 = lt_data-matnr+5(10).
            it_data-usage0 = ' '.
            it_data-company = 'A'.
            it_data-udate = lt_cdhdr-udate.
            it_data-crdate = sy-datum.
            APPEND it_data.
*** get deleted source list
            SELECT a~matnr a~lifnr maktg name1
                INTO CORRESPONDING FIELDS OF TABLE lt_data_del
                FROM eina AS a
                INNER JOIN mara AS b
                ON a~matnr = b~matnr
                INNER JOIN makt AS c
                ON b~matnr = c~matnr
                INNER JOIN lfa1 AS d
                ON a~lifnr = d~lifnr
                WHERE a~matnr = lt_data-matnr
                  AND a~lifnr <> lt_data-lifnr
*                AND LOEKZ <> ' '
                  AND b~mtart = 'ROH'.
            LOOP AT lt_data_del.
              MOVE-CORRESPONDING lt_data_del TO it_data.
              it_data-part_no = lt_data-matnr+0(15).
              it_data-matnr1 = lt_data_del-matnr+0(5).
              it_data-matnr2 = lt_data_del-matnr+5(10).
              it_data-usage0 = '*'.
              it_data-company = 'A'.
              it_data-udate = lt_cdhdr-udate.
              it_data-crdate = sy-datum.
              APPEND it_data.
            ENDLOOP.
*** End
          ENDLOOP.
        ELSE.
          SELECT a~matnr a~lifnr maktg name1
               INTO CORRESPONDING FIELDS OF TABLE lt_data
               FROM eina AS a
               INNER JOIN mara AS b
               ON a~matnr = b~matnr
               INNER JOIN makt AS c
               ON b~matnr = c~matnr
               INNER JOIN lfa1 AS d
               ON a~lifnr = d~lifnr
               WHERE a~matnr = l_objectid_10
*                AND LOEKZ <> ' '
                 AND b~mtart = 'ROH' .
          SORT lt_data BY matnr lifnr.
          LOOP AT lt_data.
            MOVE-CORRESPONDING lt_data TO it_data.
            it_data-part_no = lt_data-matnr+0(15).
            it_data-matnr1 = lt_data-matnr+0(5).
            it_data-matnr2 = lt_data-matnr+5(10).
            it_data-usage0 = '*'.
            it_data-company = 'A'.
            it_data-udate = lt_cdhdr-udate.
            it_data-crdate = sy-datum.
            APPEND it_data.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
** All data
    SELECT a~matnr a~lifnr a~erdat AS erdate maktg name1
     INTO CORRESPONDING FIELDS OF TABLE lt_data
     FROM eord AS a
     INNER JOIN mara AS b
     ON a~matnr = b~matnr
     INNER JOIN makt AS c
     ON b~matnr = c~matnr
     INNER JOIN lfa1 AS d
     ON a~lifnr = d~lifnr
     WHERE b~mtart = 'ROH'
       AND ( a~flifn = 'X' OR a~febel = 'X' ).
    SORT lt_data BY matnr lifnr.
    LOOP AT lt_data.
      MOVE-CORRESPONDING lt_data TO it_data.
      it_data-part_no = lt_data-matnr+0(15).
      it_data-matnr1 = lt_data-matnr+0(5).
      it_data-matnr2 = lt_data-matnr+5(10).
      it_data-usage0 = ' '.
      it_data-company = 'A'.
      it_data-crdate = sy-datum.
      APPEND it_data.
    ENDLOOP.
  ENDIF.
  IF it_data[] IS INITIAL.
  ELSE.
    SELECT a~matnr bklas INTO TABLE lt_mbew
       FROM mbew AS a
       INNER JOIN eord AS b
      ON a~matnr = b~matnr
      AND a~bwkey = b~werks
      FOR ALL ENTRIES IN it_data
      WHERE a~matnr = it_data-part_no.
    SORT lt_mbew BY matnr.

    SORT it_data BY matnr1 matnr2 lifnr udate.
    LOOP AT it_data.
      l_index = sy-tabix.
      l_index_d  = l_index + 1.
      READ TABLE it_data INTO lw_data INDEX l_index_d.
      IF it_data-matnr1 = lw_data-matnr1 AND
         it_data-matnr2 = lw_data-matnr2 AND
         it_data-lifnr = lw_data-lifnr AND
*       IT_DATA-UDATE = ' ' AND
         lw_data-udate <> ' '.
        it_data-usage0 = lw_data-usage0.
        it_data-udate = lw_data-udate.
*        MODIFY it_data INDEX l_index.
        DELETE it_data INDEX l_index_d.
      ENDIF.

      CLEAR : lt_mbew.
      READ TABLE lt_mbew WITH KEY matnr = it_data-part_no
                                  BINARY SEARCH.
      IF lt_mbew-bklas = '3000' OR
         lt_mbew-bklas = '3002'.
        it_data-lpkd = 'K'.
*        it_data-land1 = '*'.
      ELSE.
        it_data-lpkd = 'L'.
*        it_data-land1 = ' '.
      ENDIF.

*-< Victor 08.27.2014 get Country key from Vendor
      SELECT SINGLE land1 INTO it_data-land1
      FROM lfa1
      WHERE lifnr = it_data-lifnr.
*->

      MODIFY it_data INDEX l_index.
    ENDLOOP.
  ENDIF.
*  DELETE ADJACENT DUPLICATES FROM IT_DATA.

**--Victor added logic from here 03.20.2014
*   get material which valuation class has been changed

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cdhdr2
  FROM cdhdr AS  a
   WHERE a~udate IN s_datum
     AND a~objectclas = 'MATERIAL'
     AND ( a~tcode = 'MM02' OR a~tcode = 'MM02(MASS)' ).

  CHECK  lt_cdhdr2[] IS NOT INITIAL.

  SORT lt_cdhdr2 BY objectclas objectid
                    udate DESCENDING
                    utime DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_cdhdr2
                          COMPARING objectclas objectid changenr.

  SELECT * INTO TABLE  lt_cdpos
  FROM cdpos AS b
    FOR ALL ENTRIES IN   lt_cdhdr2
  WHERE b~tabname = 'MBEW'
    AND b~fname   = 'BKLAS'
    AND b~objectclas = lt_cdhdr2-objectclas
    AND b~objectid   = lt_cdhdr2-objectid
    AND b~changenr   = lt_cdhdr2-changenr.

  SORT lt_cdpos BY objectclas objectid changenr.
  LOOP AT lt_cdhdr2.
    READ TABLE lt_cdpos  WITH KEY  objectclas = lt_cdhdr2-objectclas
                                   objectid   = lt_cdhdr2-objectid
                                   changenr   = lt_cdhdr2-changenr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      lt_cdhdr2-value_new = lt_cdpos-value_new.
      lt_cdhdr2-part_no   = lt_cdhdr2-objectid. "For entries
      MODIFY lt_cdhdr2.
    ELSE.
      DELETE lt_cdhdr2.
    ENDIF.
  ENDLOOP.

  SELECT * INTO TABLE it_data2
  FROM ztmm_part_supp_n
    FOR ALL ENTRIES IN lt_cdhdr2
  WHERE part_no = lt_cdhdr2-part_no.

  CLEAR : it_data.
  LOOP AT it_data2.
    READ TABLE it_data WITH KEY part_no = it_data2-part_no.
    CHECK sy-subrc <> 0.

    READ TABLE lt_cdhdr2 WITH KEY part_no = it_data2-part_no.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING it_data2 TO it_data.
    IF lt_cdhdr2-value_new = '3000' OR lt_cdhdr2-value_new = '3002'.
      it_data-lpkd  = 'K'.
*      it_data-land1 = '*'.
      it_data-udate = lt_cdhdr2-udate.
    ELSE.
      it_data-lpkd  = 'L'.
*      it_data-land1 = ''.
      it_data-udate = lt_cdhdr2-udate.
    ENDIF.

*-< Victor 08.27.2014 get Country key from Vendor
      SELECT SINGLE land1 INTO it_data-land1
      FROM lfa1
      WHERE lifnr = it_data2-lifnr.
*->

    APPEND it_data.
  ENDLOOP.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  send_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_rtn.
  DATA : l_msgtxt(100),
         l_result(1),
         l_date LIKE sy-datum.

  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'There is No Data'.
    STOP.
  ENDIF.

  CALL FUNCTION 'Z_FPP_PART_SUPPLIER'
    DESTINATION c_dest
    IMPORTING
      result                = l_result
    TABLES
      part_supp             = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc = 0.
    MESSAGE i001 WITH 'Data successfully sent out'.
  ELSE.
    MESSAGE i001 WITH l_msgtxt.
  ENDIF.

ENDFORM.                    " send_rtn
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  s_datum-sign = 'I'.
  s_datum-option = 'EQ'.
  s_datum-high = sy-datum.
  s_datum-low = sy-datum - 6.
  APPEND s_datum.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  CHECK it_data[] IS NOT INITIAL.

  MODIFY ztmm_part_supp_n FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH 'Table update error'.
  ENDIF.

ENDFORM.                    " SAVE_DATA
