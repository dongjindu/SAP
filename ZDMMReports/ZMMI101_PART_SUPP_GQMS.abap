************************************************************************
* Program Name      : ZMMI101_PART_SUPP_GQMS
* Author            : Furong Wang
* Creation Date     : 12/04/2007
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : GQMS (SARTS & SUPPLIER)
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZMMI101_PART_SUPP_GQMS NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID ZMPP.


DATA: IT_DATA LIKE TABLE OF ZTMM_PART_SUPP WITH HEADER LINE.

CONSTANTS: C_DEST(10) VALUE 'WMHR01'.   "Interface Destination.

SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM OBLIGATORY.
PARAMETERS: P_ALL(1).
SELECTION-SCREEN SKIP.
PARAMETERS: P_SEND(1) DEFAULT  'X'.
SELECTION-SCREEN END OF BLOCK BL1.
*SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-T02.
*PARAMETERS: P_DAYS TYPE I.
*SELECTION-SCREEN END OF BLOCK BL2.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM SAVE_DATA.
  IF P_SEND IS INITIAL.
  ELSE.
    PERFORM SEND_RTN.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: BEGIN OF LT_DATA OCCURS 0,
        MATNR LIKE MARA-MATNR,
        LIFNR LIKE IT_DATA-LIFNR,
        ERDATE LIKE IT_DATA-ERDATE,
        UDATE LIKE IT_DATA-UDATE,
        MAKTG LIKE IT_DATA-MAKTG,
        NAME1 LIKE IT_DATA-NAME1,
        END OF LT_DATA.
  DATA: LT_CDHDR LIKE TABLE OF CDHDR WITH HEADER LINE,
        LT_DATA_DEL LIKE TABLE OF LT_DATA WITH HEADER LINE,
        LW_DATA LIKE IT_DATA..
  DATA: L_OBJECTID_10(10),
        L_OBJECTID_WERKS(4),
        L_INDEX LIKE SY-TABIX,
        L_INDEX_D LIKE SY-TABIX.

  IF P_ALL IS INITIAL.
    SELECT * INTO TABLE LT_CDHDR
      FROM CDHDR
      WHERE OBJECTCLAS IN ('ORDERBUCH', 'INFOSATZ')
        AND UDATE IN S_DATUM.
    SORT LT_CDHDR BY OBJECTCLAS OBJECTID CHANGE_IND.
    DELETE ADJACENT DUPLICATES FROM LT_CDHDR
             COMPARING OBJECTCLAS OBJECTID CHANGE_IND.

    LOOP AT LT_CDHDR.
      L_OBJECTID_10 = LT_CDHDR-OBJECTID+0(10).
      L_OBJECTID_WERKS = LT_CDHDR-OBJECTID+18(4).
** Info record change
      IF LT_CDHDR-OBJECTCLAS = 'INFOSATZ'.
        CASE LT_CDHDR-CHANGE_IND.
          WHEN 'I'.
            SELECT A~MATNR A~LIFNR A~ERDAT AS ERDATE MAKTG NAME1
              INTO CORRESPONDING FIELDS OF TABLE LT_DATA
              FROM EINA AS A
              INNER JOIN MARA AS B
              ON A~MATNR = B~MATNR
              INNER JOIN MAKT AS C
              ON B~MATNR = C~MATNR
              INNER JOIN LFA1 AS D
              ON A~LIFNR = D~LIFNR
              WHERE INFNR = L_OBJECTID_10
                AND LOEKZ = ' '
                AND ( B~MTART = 'ROH' OR B~MTART = 'ROH1' ).
            SORT LT_DATA BY MATNR LIFNR.
            LOOP AT LT_DATA.
              MOVE-CORRESPONDING LT_DATA TO IT_DATA.
              IT_DATA-MATNR1 = LT_DATA-MATNR+0(5).
              IT_DATA-MATNR2 = LT_DATA-MATNR+5(10).
              IT_DATA-USAGE0 = '*'.
              IT_DATA-COMPANY = 'A'.
              IT_DATA-CRDATE = SY-DATUM.
              APPEND IT_DATA.
            ENDLOOP.
          WHEN 'U'.
            SELECT A~MATNR A~LIFNR MAKTG NAME1
              INTO CORRESPONDING FIELDS OF TABLE LT_DATA
              FROM EINA AS A
              INNER JOIN MARA AS B
              ON A~MATNR = B~MATNR
              INNER JOIN MAKT AS C
              ON B~MATNR = C~MATNR
              INNER JOIN LFA1 AS D
              ON A~LIFNR = D~LIFNR
              WHERE INFNR = L_OBJECTID_10
                AND LOEKZ <> ' '
                AND ( B~MTART = 'ROH' OR B~MTART = 'ROH1' ).
            SORT LT_DATA BY MATNR LIFNR.
            LOOP AT LT_DATA.
              MOVE-CORRESPONDING LT_DATA TO IT_DATA.
              IT_DATA-MATNR1 = LT_DATA-MATNR+0(5).
              IT_DATA-MATNR2 = LT_DATA-MATNR+5(10).
              IT_DATA-USAGE0 = '*'.
              IT_DATA-COMPANY = 'A'.
              IT_DATA-UDATE = LT_CDHDR-UDATE.
              IT_DATA-CRDATE = SY-DATUM.
              APPEND IT_DATA.
            ENDLOOP.
        ENDCASE.
      ELSE.
** Source list change
        SELECT A~MATNR A~LIFNR MAKTG NAME1
        INTO CORRESPONDING FIELDS OF TABLE LT_DATA
        FROM EORD AS A
        INNER JOIN MARA AS B
        ON A~MATNR = B~MATNR
        INNER JOIN MAKT AS C
        ON B~MATNR = C~MATNR
        INNER JOIN LFA1 AS D
        ON A~LIFNR = D~LIFNR
        WHERE A~MATNR = L_OBJECTID_10
          AND A~WERKS = L_OBJECTID_WERKS
          AND ( A~FLIFN = 'X' or A~FEBEL = 'X' )
          AND ( B~MTART = 'ROH' OR B~MTART = 'ROH1' ).
        IF SY-SUBRC = 0.
          SORT LT_DATA BY MATNR LIFNR.
          LOOP AT LT_DATA.
            MOVE-CORRESPONDING LT_DATA TO IT_DATA.
            IT_DATA-MATNR1 = LT_DATA-MATNR+0(5).
            IT_DATA-MATNR2 = LT_DATA-MATNR+5(10).
            IT_DATA-USAGE0 = ' '.
            IT_DATA-COMPANY = 'A'.
            IT_DATA-UDATE = LT_CDHDR-UDATE.
            IT_DATA-CRDATE = SY-DATUM.
            APPEND IT_DATA.
*** get deleted source list
            SELECT A~MATNR A~LIFNR MAKTG NAME1
                INTO CORRESPONDING FIELDS OF TABLE LT_DATA_DEL
                FROM EINA AS A
                INNER JOIN MARA AS B
                ON A~MATNR = B~MATNR
                INNER JOIN MAKT AS C
                ON B~MATNR = C~MATNR
                INNER JOIN LFA1 AS D
                ON A~LIFNR = D~LIFNR
                WHERE A~MATNR = LT_DATA-MATNR
                  AND A~LIFNR <> LT_DATA-LIFNR
*                AND LOEKZ <> ' '
                  AND ( B~MTART = 'ROH' OR B~MTART = 'ROH1' ).
            LOOP AT LT_DATA_DEL.
              MOVE-CORRESPONDING LT_DATA_DEL TO IT_DATA.
              IT_DATA-MATNR1 = LT_DATA_DEL-MATNR+0(5).
              IT_DATA-MATNR2 = LT_DATA_DEL-MATNR+5(10).
              IT_DATA-USAGE0 = '*'.
              IT_DATA-COMPANY = 'A'.
              IT_DATA-UDATE = LT_CDHDR-UDATE.
              IT_DATA-CRDATE = SY-DATUM.
              APPEND IT_DATA.
            ENDLOOP.
*** End
          ENDLOOP.
        ELSE.
          SELECT A~MATNR A~LIFNR MAKTG NAME1
               INTO CORRESPONDING FIELDS OF TABLE LT_DATA
               FROM EINA AS A
               INNER JOIN MARA AS B
               ON A~MATNR = B~MATNR
               INNER JOIN MAKT AS C
               ON B~MATNR = C~MATNR
               INNER JOIN LFA1 AS D
               ON A~LIFNR = D~LIFNR
               WHERE A~MATNR = L_OBJECTID_10
*                AND LOEKZ <> ' '
                 AND ( B~MTART = 'ROH' OR B~MTART = 'ROH1' ).
          SORT LT_DATA BY MATNR LIFNR.
          LOOP AT LT_DATA.
            MOVE-CORRESPONDING LT_DATA TO IT_DATA.
            IT_DATA-MATNR1 = LT_DATA-MATNR+0(5).
            IT_DATA-MATNR2 = LT_DATA-MATNR+5(10).
            IT_DATA-USAGE0 = '*'.
            IT_DATA-COMPANY = 'A'.
            IT_DATA-UDATE = LT_CDHDR-UDATE.
            IT_DATA-CRDATE = SY-DATUM.
            APPEND IT_DATA.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
** All data
    SELECT A~MATNR A~LIFNR A~ERDAT AS ERDATE MAKTG NAME1
     INTO CORRESPONDING FIELDS OF TABLE LT_DATA
     FROM EORD AS A
     INNER JOIN MARA AS B
     ON A~MATNR = B~MATNR
     INNER JOIN MAKT AS C
     ON B~MATNR = C~MATNR
     INNER JOIN LFA1 AS D
     ON A~LIFNR = D~LIFNR
     WHERE ( B~MTART = 'ROH' OR B~MTART = 'ROH1' )
       AND ( A~FLIFN = 'X' or A~FEBEL = 'X' ).
    SORT LT_DATA BY MATNR LIFNR.
    LOOP AT LT_DATA.
      MOVE-CORRESPONDING LT_DATA TO IT_DATA.
      IT_DATA-MATNR1 = LT_DATA-MATNR+0(5).
      IT_DATA-MATNR2 = LT_DATA-MATNR+5(10).
      IT_DATA-USAGE0 = ' '.
      IT_DATA-COMPANY = 'A'.
      IT_DATA-CRDATE = SY-DATUM.
      APPEND IT_DATA.
    ENDLOOP.
  ENDIF.
  SORT IT_DATA BY MATNR1 MATNR2 LIFNR UDATE.
  LOOP AT IT_DATA.
    L_INDEX = SY-TABIX.
    L_INDEX_D  = L_INDEX + 1.
    READ TABLE IT_DATA INTO LW_DATA INDEX L_INDEX_D.
    IF IT_DATA-MATNR1 = LW_DATA-MATNR1 AND
       IT_DATA-MATNR2 = LW_DATA-MATNR2 AND
       IT_DATA-LIFNR = LW_DATA-LIFNR AND
*       IT_DATA-UDATE = ' ' AND
       LW_DATA-UDATE <> ' '.
      IT_DATA-USAGE0 = LW_DATA-USAGE0.
      IT_DATA-UDATE = LW_DATA-UDATE.
      MODIFY IT_DATA INDEX L_INDEX.
      DELETE IT_DATA INDEX L_INDEX_D.
    ENDIF.
  ENDLOOP.
*  DELETE ADJACENT DUPLICATES FROM IT_DATA.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  send_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_RTN.
  DATA : L_MSGTXT(100),
         L_RESULT(1),
         L_DATE LIKE SY-DATUM.

  CALL FUNCTION 'Z_FPP_PART_SUPPLIER'
       DESTINATION C_DEST
   IMPORTING
     RESULT        = L_RESULT
    TABLES
      PART_SUPP       = IT_DATA
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF SY-SUBRC = 0.
    MESSAGE I001 WITH 'Data successfully sent out'.
  ELSE.
    MESSAGE I001 WITH L_MSGTXT.
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
FORM INIT_DATA.
  S_DATUM-SIGN = 'I'.
  S_DATUM-OPTION = 'EQ'.
  S_DATUM-HIGH = SY-DATUM.
  S_DATUM-LOW = SY-DATUM - 6.
  APPEND S_DATUM.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  MODIFY ZTMM_PART_SUPP FROM TABLE IT_DATA.
*    MODIFY ZTPP_PLAN_HMC FROM TABLE IT_PLAN_DAY.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE I000 WITH 'Table update error'.
  ENDIF.

ENDFORM.                    " SAVE_DATA
