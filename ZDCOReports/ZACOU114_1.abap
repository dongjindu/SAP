*----------------------------------------------------------------------
* Program ID        : ZACOU114
* Title             : [CO] Variance List
* Created on        : 10/13/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Report for Variance List
*----------------------------------------------------------------------
REPORT ZACOU114_1 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.

TABLES ZTCOU106.

TYPES: BEGIN OF TY_106,
         KOKRS  TYPE KOKRS,          " Cotrolling Area
         BDATJ  TYPE BDATJ,          " Fiscal Year
         ID     TYPE ZID1,           " ID
         KZUST1 TYPE ZKZUST1,                               " Reason1
         WERTN1 TYPE ZWERTN1,                               " Price1
         KZUST2 TYPE ZKZUST2,                               " Reason1
         WERTN2 TYPE ZWERTN2,                               " Price1
         KZUST3 TYPE ZKZUST3,                               " Reason1
         WERTN3 TYPE ZWERTN3,                               " Price1
         IDTEXT TYPE ZIDTEXT,        " ID Desc.
       END OF TY_106.

TYPES: BEGIN OF TY_ITAB,
         KOKRS     TYPE KOKRS,       " Cotrolling Area
         BDATJ     TYPE BDATJ,       " Fiscal Year
         ID        TYPE ZID1,        " ID
         IDTEXT    TYPE ZIDTEXT,     " ID Desc.
         KZUST     TYPE KZUST,       " Reason
         INCR      TYPE ZWERTN,      " Increase
         DECR      TYPE ZWERTN,      " Decrease
       END OF TY_ITAB.

TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ITAB.
TYPES:   TOT      TYPE ZWERTN,      " Total
         ZRESC	  TYPE ZCORSC,      " Responsible Category
         ZRESP	  TYPE ZCORSP,      " Responsible Area
         TEXT	  TYPE ZRTEXT,      " Description
         RTEXT    TYPE ZRTEXT,      " Reason Description
         TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
       END OF TY_OUT.

TYPES: BEGIN OF TY_KZUST,
         KZUST TYPE KZUST,
       END OF TY_KZUST.

DATA: GT_106   TYPE TABLE OF TY_106   WITH HEADER LINE,
      ITAB     TYPE TABLE OF TY_ITAB  WITH HEADER LINE,
      GT_OUT   TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_M01   TYPE TABLE OF ZTCOUM01 WITH HEADER LINE,
      GT_M02   TYPE TABLE OF ZTCOUM02 WITH HEADER LINE,
      GT_KZUST TYPE TABLE OF TY_KZUST WITH HEADER LINE.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME.
PARAMETERS: P_KOKRS LIKE ZTCOU106-KOKRS OBLIGATORY
                                        MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCOU106-BDATJ OBLIGATORY MEMORY ID BDTJ.
SELECT-OPTIONS: S_POPER FOR ZTCOU106-POPER OBLIGATORY,
                S_ID    FOR ZTCOU106-ID MATCHCODE OBJECT ZID,
                S_KZUST FOR ZTCOU106-KZUST.
SELECTION-SCREEN END OF BLOCK B0.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM DISP_RESULT.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Costing Data
*----------------------------------------------------------------------*
FORM GET_DATA.
* Get Calculate Variances Data
  CLEAR   GT_106.
  REFRESH GT_106.

  SELECT A~KOKRS A~BDATJ A~ID A~KZUST1 A~WERTN1
         A~KZUST2 A~WERTN2 A~KZUST3 A~WERTN3 B~IDTEXT
    INTO TABLE GT_106
    FROM ZTCOU106 AS A
    JOIN ZTCOU104 AS B
      ON B~KOKRS = A~KOKRS
     AND B~BDATJ = A~BDATJ
     AND B~KALKA = A~KALKA
     AND B~ID = A~ID
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = P_YEAR
     AND A~ID IN S_ID
     AND A~POPER IN S_POPER.

  SORT GT_106 BY KOKRS BDATJ ID.

  PERFORM GET_ITAB.

* Get Reason Info.
  PERFORM GET_REASON.

* Get Resp.
  PERFORM GET_RESP.

* Create Internal Tables for Variance List
  PERFORM GET_GT_OUT.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_RGRP
*&---------------------------------------------------------------------*
*       Create Internal Table for Reason
*----------------------------------------------------------------------*
FORM GET_REASON.
  CLEAR GT_M02.
  REFRESH GT_M02.

  SELECT * INTO TABLE GT_M02
    FROM ZTCOUM02
   WHERE GRP1 <> 'Z'.

  SORT GT_M02 BY RGRP2.

ENDFORM.                    " GET_REASON
*&---------------------------------------------------------------------*
*&      Form  GET_RESP
*&---------------------------------------------------------------------*
*       Get Responsible Area
*----------------------------------------------------------------------*
FORM GET_RESP.
  CLEAR GT_M01.
  REFRESH GT_M01.

  SELECT * INTO TABLE GT_M01
    FROM ZTCOUM01.

  SORT GT_M01 BY ZRESC.

ENDFORM.                    " GET_RESP
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  DATA L_RESP TYPE ZCORSP.

  CLEAR   GT_OUT.
  REFRESH GT_OUT.

  LOOP AT ITAB.
    MOVE-CORRESPONDING ITAB TO GT_OUT.
    GT_OUT-TOT = ITAB-INCR + ITAB-DECR.        " Total

*   Resp.
    READ TABLE GT_M02 WITH KEY RGRP2 = ITAB-KZUST BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_OUT-RTEXT = GT_M02-TEXT.       " Reason Desc.
    ENDIF.

    CLEAR L_RESP.
    READ TABLE GT_M01 WITH KEY ZRESC = GT_M02-ALU.

    IF SY-SUBRC = 0.
      PERFORM MOVE_RESP.
      CONTINUE.
    ELSE.
      READ TABLE GT_M01 WITH KEY ZRESC = GT_M02-ALD.
      IF SY-SUBRC = 0.
        PERFORM MOVE_RESP.
        CONTINUE.
      ELSE.
        READ TABLE GT_M01 WITH KEY ZRESC = GT_M02-ALE.
        IF SY-SUBRC = 0.
          PERFORM MOVE_RESP.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Review
*---------------------------------------------------------------------*
FORM DISP_RESULT.
  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_EVENTS, GS_VARIANT,
         GT_FIELDCAT[], GT_EVENTS[].

  PERFORM BUILD_FIELD_CATEGORY USING:
    'ID'         'X'  'ID'               10    'CHAR',
    'IDTEXT'     'X'  'Description'      25    'CHAR',
    'ZRESC'      ' '  'Resp.Category'    2     'CHAR',
    'ZRESP'      ' '  'Resp.Area'        1     'CHAR',
    'TEXT'       ' '  'Description'      30    'CHAR',
    'KZUST'      ' '  'Reason'           10    'CHAR',
    'RTEXT'      ' '  'Description'      25    'CHAR',
    'TOT'        ' '  'Total'            15    'CURR',
    'INCR'       ' '  'Increase'         15    'CURR',
    'DECR'       ' '  'Decrease'         15    'CURR'.

  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME  = 'TABCOLOR'.

  PERFORM SET_COLOR.
  PERFORM SET_LIST_HEADER USING GT_LIST_TOP_OF_PAGE.
  PERFORM SET_EVENTS CHANGING GT_EVENTS.

  GV_REPID = GS_VARIANT-REPORT = SY-REPID.

* Set variant
  GS_VARIANT-VARIANT = P_VARI.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GC_PF_STATUS_SET
            I_CALLBACK_USER_COMMAND  = GC_USER_COMMAND
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_EVENTS                = GT_EVENTS[]
            I_SAVE                   = 'A'
            IS_VARIANT               = GS_VARIANT
       TABLES
            T_OUTTAB                 = GT_OUT.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  SET_LIST_HEADER
*&---------------------------------------------------------------------*
FORM SET_LIST_HEADER USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
        L_CNT TYPE I.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.

  LS_LINE-KEY  = 'Controlling Area'.
  LS_LINE-INFO = P_KOKRS.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Fiscal Year'.
  LS_LINE-INFO = P_YEAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Period'.
  DESCRIBE TABLE S_POPER LINES L_CNT.
  IF L_CNT = 1.
    IF S_POPER-HIGH IS INITIAL.
      LS_LINE-INFO = S_POPER-LOW.
    ELSE.
      CONCATENATE S_POPER-LOW '~' S_POPER-HIGH INTO LS_LINE-INFO.
    ENDIF.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ELSEIF L_CNT > 1.
    CONCATENATE S_POPER-LOW '...' INTO LS_LINE-INFO.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

  IF NOT S_ID IS INITIAL.
    LS_LINE-KEY  = 'ID'.
    DESCRIBE TABLE S_ID LINES L_CNT.
    IF L_CNT = 1.
      IF S_ID-HIGH IS INITIAL.
        LS_LINE-INFO = S_ID-LOW.
      ELSE.
        CONCATENATE S_ID-LOW '~' S_ID-HIGH INTO LS_LINE-INFO.
      ENDIF.
      APPEND LS_LINE TO LT_TOP_OF_PAGE.
    ELSEIF L_CNT > 1.
      CONCATENATE S_ID-LOW '...' INTO LS_LINE-INFO.
      APPEND LS_LINE TO LT_TOP_OF_PAGE.
    ENDIF.
  ENDIF.

  IF NOT S_KZUST IS INITIAL.
    LS_LINE-KEY  = 'Reason'.
    DESCRIBE TABLE S_KZUST LINES L_CNT.
    IF L_CNT = 1.
      IF S_KZUST-HIGH IS INITIAL.
        LS_LINE-INFO = S_KZUST-LOW.
      ELSE.
        CONCATENATE S_KZUST-LOW '~' S_KZUST-HIGH INTO LS_LINE-INFO.
      ENDIF.
      APPEND LS_LINE TO LT_TOP_OF_PAGE.
    ELSEIF L_CNT > 1.
      CONCATENATE S_KZUST-LOW '...' INTO LS_LINE-INFO.
      APPEND LS_LINE TO LT_TOP_OF_PAGE.
    ENDIF.
  ENDIF.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
ENDFORM.                    " SET_LIST_HEADER
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
FORM SET_COLOR.
  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].

  GS_SPECIALCOL-FIELDNAME = 'TOT'.
  GS_SPECIALCOL-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_TOTAL.
  GS_SPECIALCOL-COLOR-INT = 0.
  APPEND GS_SPECIALCOL TO GT_SPECIALCOL.

  GS_SPECIALCOL-FIELDNAME = 'INCR'.
  GS_SPECIALCOL-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_POSITIVE.
  APPEND GS_SPECIALCOL TO GT_SPECIALCOL.

  GS_SPECIALCOL-FIELDNAME = 'DECR'.
  GS_SPECIALCOL-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_NEGATIVE.
  APPEND GS_SPECIALCOL TO GT_SPECIALCOL.

  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE SCREEN.

    WHEN 'DET'.
      READ TABLE GT_OUT INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        SUBMIT ZACOU115 WITH P_KOKRS = P_KOKRS
                        WITH P_YEAR = P_YEAR
                        WITH P_ID = GT_OUT-ID
                        WITH S_POPER IN S_POPER.
      ENDIF.
  ENDCASE.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  MOVE_RESP
*&---------------------------------------------------------------------*
FORM MOVE_RESP.
  GT_OUT-ZRESC = GT_M01-ZRESC.     " Responsible Category
  GT_OUT-ZRESP = GT_M01-ZRESP.     " Responsible Area
  GT_OUT-TEXT  = GT_M01-TEXT.      " Description

ENDFORM.                    " MOVE_RESP
*&---------------------------------------------------------------------*
*&      Form  GET_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_ITAB.
  TYPES: BEGIN OF TY_106,
           KOKRS  TYPE KOKRS,          " Cotrolling Area
           BDATJ  TYPE BDATJ,          " Fiscal Year
           ID     TYPE ZID1,           " ID
           IDTEXT TYPE ZIDTEXT,        " ID Desc.
           KZUST  TYPE ZKZUST1,        " Reason
           WERTN  TYPE ZWERTN1,        " Price
           CHK,
         END OF TY_106.

  DATA LT_106 TYPE TABLE OF TY_106 WITH HEADER LINE.
*
  CLEAR LT_106.
  REFRESH LT_106.

  LOOP AT GT_106.
    LT_106-KOKRS  = GT_106-KOKRS.
    LT_106-BDATJ  = GT_106-BDATJ.
    LT_106-ID     = GT_106-ID.
    LT_106-IDTEXT = GT_106-IDTEXT.

    LT_106-KZUST  = GT_106-KZUST1.
    LT_106-WERTN  = GT_106-WERTN1.
    COLLECT LT_106.

    LT_106-KZUST = GT_106-KZUST2.
    LT_106-WERTN = GT_106-WERTN2.
    COLLECT LT_106.

    LT_106-KZUST = GT_106-KZUST3.
    LT_106-WERTN = GT_106-WERTN3.
    COLLECT LT_106.
    CLEAR LT_106.
  ENDLOOP.

  IF NOT S_KZUST[] IS INITIAL.
    LT_106-CHK = 'X'.
    MODIFY LT_106 TRANSPORTING CHK WHERE KZUST IN S_KZUST.
    DELETE LT_106 WHERE CHK IS INITIAL.
  ENDIF.
*
  CLEAR ITAB.
  REFRESH ITAB.

  LOOP AT LT_106.
    ITAB-KOKRS  = LT_106-KOKRS.
    ITAB-BDATJ  = LT_106-BDATJ.
    ITAB-ID     = LT_106-ID.
    ITAB-IDTEXT = LT_106-IDTEXT.
    CONCATENATE LT_106-KZUST+0(1) LT_106-KZUST+2(1) INTO ITAB-KZUST.

    CASE LT_106-KZUST+1(1).
      WHEN 'U'.
        ITAB-INCR = LT_106-WERTN.
      WHEN 'D'.
        ITAB-DECR = LT_106-WERTN.
    ENDCASE.

    COLLECT ITAB.
  ENDLOOP.

ENDFORM.                    " GET_ITAB
