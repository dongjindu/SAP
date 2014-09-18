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
      ERDAT LIKE QMEL-ERDAT,
      ERNAM LIKE QMEL-ERNAM,
      MZEIT LIKE QMEL-MZEIT,
*      stat like jest-stat,
*      INACT LIKE JEST-INACT,
*      USNAM_JCDS LIKE JCDS-USNAM,
*      UDATE_JCDS LIKE JCDS-UDATE,
*      UTIME_JCDS LIKE JCDS-UTIME,
*      DAY_DIFF type p,
*      TIME_DIFF type p,
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

  CONSTANTS: C_STAT LIKE JEST-STAT VALUE 'E0016'.

  SELECT A~QMNUM A~MATNR A~OBJNR MAKTX
         A~RKMNG A~LIFNUM A~QWRNUM
         A~ERDAT A~ERNAM A~MZEIT stat INACT
      INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
      FROM QMEL AS A
      INNER JOIN QMFE AS B
      ON A~QMNUM = B~QMNUM
      INNER JOIN JEST AS D
      ON A~OBJNR = D~OBJNR
        INNER JOIN MAKT AS G
      ON A~MATNR = G~MATNR
      WHERE A~QMNUM IN S_QMNUM
        AND A~ERDAT IN S_ERDAT
        AND A~LIFNUM IN S_LIFNUM
         AND FEGRP = '0'
         AND STAT =  C_STAT
         AND INACT = ' '
        AND G~SPRAS = 'E'.

  SORT IT_ITAB BY QMNUM MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_ITAB COMPARING QMNUM MATNR.

ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------
**
*&      Form  CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XLIST  text
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

INCLUDE ZQM_SCARP_WITN_NO_PQ_REVIEWO01.
