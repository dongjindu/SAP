REPORT ZQM_SCARP_PQRE NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

TABLES: QMEL, VIQMEL, TJ30T, QMFE.
TYPE-POOLS: VRM.

DATA: BEGIN OF IT_ITAB OCCURS 0,
      QMNUM LIKE QMEL-QMNUM,
      MATNR LIKE QMEL-MATNR,
      OBJNR LIKE QMEL-OBJNR,
      MAKTX LIKE MAKT-MAKTX,
      RKMNG LIKE QMEL-RKMNG,
      LIFNUM LIKE QMEL-LIFNUM,
*      QWRNUM LIKE QMEL-QWRNUM,
      ERDAT LIKE QMEL-ERDAT,
      ERNAM LIKE QMEL-ERNAM,
      MZEIT LIKE QMEL-MZEIT,
      STAT LIKE JEST-STAT,
      INACT LIKE JEST-INACT,
      USNAM_JCDS LIKE JCDS-USNAM,
      UDATE_JCDS LIKE JCDS-UDATE,
      UTIME_JCDS LIKE JCDS-UTIME,
      DAY_DIFF TYPE P,
      TIME_DIFF TYPE P,
      MAWERK LIKE QMEL-MAWERK,
      END OF IT_ITAB.

DATA: XNAME    TYPE VRM_ID,
      NAME     TYPE VRM_ID    ,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      BEGIN OF YLIST     OCCURS 0,
         KEY(40) TYPE C,
         TEXT(80) TYPE C,
      END OF YLIST      .

** alv
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDNAME.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: gro_doc_container TYPE REF TO cl_gui_docking_container.


DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE I.
***

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_QMNUM FOR QMEL-QMNUM,
                 S_ERDAT FOR QMEL-ERDAT,
                 S_LIFNUM FOR QMEL-LIFNUM.

PARAMETERS: P_FEGRPN(8) AS LISTBOX VISIBLE LENGTH 15 OBLIGATORY.

SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN OUTPUT.
  PERFORM LIST_BOX_FEGRPN.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S009 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 0800.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA: LT_JCDS LIKE TABLE OF JCDS WITH HEADER LINE.
  DATA: L_CURR_DATE LIKE SY-DATUM,
        L_CURR_TIME LIKE SY-UZEIT,
        L_TIME_DIFF LIKE SY-UZEIT,
        L_DATEDIFF TYPE I,
        L_TIMEDIFF TYPE I,
        L_INDEX LIKE SY-TABIX,
        L_KALID LIKE KAKO-KALID,
        L_DATE_BE LIKE SY-DATUM.

  CONSTANTS: C_STAT LIKE JEST-STAT VALUE 'I0068'.

  SELECT A~QMNUM A~MATNR A~OBJNR MAKTX
         A~RKMNG A~LIFNUM A~QWRNUM
         A~ERDAT A~ERNAM A~MZEIT STAT INACT MAWERK
      INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
      FROM QMEL AS A
      INNER JOIN QMFE AS B
      ON A~QMNUM = B~QMNUM
*      INNER JOIN VIQMEL AS C
*      ON A~QMNUM = C~QMNUM
      INNER JOIN JEST AS D
      ON A~OBJNR = D~OBJNR
*      INNER JOIN TJ30 AS E
*      ON D~STAT = E~ESTAT
*      INNER JOIN TJ30T AS F
*      ON D~STAT = F~ESTAT
        INNER JOIN MAKT AS G
      ON A~MATNR = G~MATNR
      WHERE A~QMNUM IN S_QMNUM
        AND A~ERDAT IN S_ERDAT
*        AND C~QMGRP IN S_QMGRP
*        AND C~QMCOD IN S_QMCOD
        AND A~LIFNUM IN S_LIFNUM
         AND FEGRP = '0'
         AND STAT =  C_STAT
         AND INACT = ' '
*        AND F~STSMA = 'ZQNSCRP1'
*        AND F~SPRAS = 'EN'
        AND G~SPRAS = 'E'.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  SORT IT_ITAB BY QMNUM MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_ITAB COMPARING QMNUM MATNR.

  L_CURR_DATE = SY-DATUM.
  L_CURR_TIME = SY-UZEIT.

  SELECT A~OBJNR USNAM UDATE UTIME
      INTO CORRESPONDING FIELDS OF TABLE LT_JCDS
    FROM  JEST AS A
    INNER JOIN JCDS AS B
      ON A~OBJNR = B~OBJNR
      AND A~STAT = B~STAT
      AND A~STAT = B~STAT
    FOR ALL ENTRIES IN IT_ITAB
    WHERE A~OBJNR = IT_ITAB-OBJNR
     AND B~STAT = 'E0015'
     AND B~INACT = ' '.

  SORT LT_JCDS BY OBJNR.


  LOOP AT IT_ITAB.
    CLEAR: L_DATEDIFF, L_TIMEDIFF.

    CASE IT_ITAB-MAWERK.
      WHEN 'P001'.
        L_KALID = 'HM'.
      WHEN 'E001' OR 'E002'.
        L_KALID = 'HE'.
    ENDCASE.

    L_INDEX = SY-TABIX.
    READ TABLE LT_JCDS WITH KEY OBJNR = IT_ITAB-OBJNR BINARY SEARCH.
    IF SY-SUBRC = 0.
*      CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
*        EXPORTING
*          DATE1            = L_CURR_DATE
*          TIME1            = L_CURR_TIME
*          DATE2            = LT_JCDS-UDATE
*          TIME2            = LT_JCDS-UTIME
*        IMPORTING
*          DATEDIFF         = L_DATEDIFF
*          TIMEDIFF         = L_TIMEDIFF
**         EARLIEST         =
*        EXCEPTIONS
*          INVALID_DATETIME = 1
*          OTHERS           = 2.
*
*      IF SY-SUBRC <> 0.
*        CONTINUE.
*      ENDIF.

      L_DATE_BE = LT_JCDS-UDATE.
      IF L_CURR_DATE = L_DATE_BE.
        L_DATEDIFF = 0.
      L_TIMEDIFF = ( ( L_CURR_TIME - LT_JCDS-UTIME ) MOD 86400 ) / 3600
      .
        IF L_TIMEDIFF < 0.
          L_TIMEDIFF = 0.
        ENDIF.
      ELSE.
        WHILE L_CURR_DATE > L_DATE_BE.
          L_DATE_BE = L_DATE_BE + 1.

          CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
            EXPORTING
              CORRECT_OPTION               = '+'
              DATE                         = L_DATE_BE
              FACTORY_CALENDAR_ID          = L_KALID
            IMPORTING
              DATE                         = L_DATE_BE
            EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 1
              CORRECT_OPTION_INVALID       = 2
              DATE_AFTER_RANGE             = 3
              DATE_BEFORE_RANGE            = 4
              DATE_INVALID                 = 5
              FACTORY_CALENDAR_NOT_FOUND   = 6
              OTHERS                       = 7.

          IF L_DATE_BE <= L_CURR_DATE.
            L_DATEDIFF = L_DATEDIFF + 1.
          ENDIF.

        ENDWHILE.
        L_TIMEDIFF =  L_CURR_TIME - LT_JCDS-UTIME.
        IF L_TIMEDIFF < 0.
          L_TIMEDIFF =  L_TIMEDIFF MOD 86400 .
          L_DATEDIFF = L_DATEDIFF - 1.
        ENDIF.
        L_TIMEDIFF = L_TIMEDIFF / 3600.
      ENDIF.
      IF L_TIMEDIFF = 24.
        L_TIMEDIFF = 0.
        L_DATEDIFF = L_DATEDIFF + 1.
      ENDIF.

      IT_ITAB-USNAM_JCDS = LT_JCDS-USNAM.
      IT_ITAB-UDATE_JCDS = LT_JCDS-UDATE.
      IT_ITAB-UTIME_JCDS = LT_JCDS-UTIME.

      IT_ITAB-DAY_DIFF = L_DATEDIFF.
      IT_ITAB-TIME_DIFF = L_TIMEDIFF.
      MODIFY IT_ITAB INDEX L_INDEX.
    ELSE.
      DELETE IT_ITAB INDEX L_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
ENDFORM.                    " INIT_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XLIST    text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_VRM USING P_XLIST.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = P_XLIST.
ENDFORM.                    " CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_QMGRPN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_BOX_FEGRPN.
  DATA: NAME TYPE VRM_ID,   " Field Name
        XLIST TYPE VRM_VALUES,
        XVALUE LIKE LINE OF XLIST.

  MOVE 'ALL' TO XVALUE-KEY.
  MOVE 'For all' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '0' TO XVALUE-KEY.
  MOVE 'Supplier Defect' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '1' TO XVALUE-KEY.
  MOVE ' Stamping' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '2' TO XVALUE-KEY.
  MOVE 'Welding' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '3' TO XVALUE-KEY.
  MOVE 'Paint' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '4' TO XVALUE-KEY.
  MOVE 'Assembly' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '5' TO XVALUE-KEY.
  MOVE 'Engine' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '6' TO XVALUE-KEY.
  MOVE 'Quality' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '9' TO XVALUE-KEY.
  MOVE 'Administration' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '99' TO XVALUE-KEY.
  MOVE 'No Defect Found' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  NAME = 'P_FEGRPN'.

* LIST BOX SETTING
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = NAME  " list box
      VALUES          = XLIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.

  IF P_FEGRPN IS INITIAL.
    READ TABLE XLIST INTO XVALUE  INDEX 1.
    P_FEGRPN = XVALUE-KEY.
  ENDIF.

ENDFORM.                    " LIST_BOX_QMGRPN

INCLUDE ZQM_SCARP_DISPUTED_PARTS_LIO01.
