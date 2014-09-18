*----------------------------------------------------------------------
* Program ID        : ZACOU115
* Title             : [CO] Variance Report
* Created on        : 10/13/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Report for Variance
*----------------------------------------------------------------------
REPORT ZACOU115 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

TABLES: ZTCOU102,      " [CO] Costing Result
        ZTCOU106.      " [CO] Calculate Variances

TYPES: BEGIN OF TY_106,
         KOKRS  TYPE KOKRS,          " Cotrolling Area
         BDATJ  TYPE BDATJ,          " Fiscal Year
         KALKA  TYPE CK_KALKA,       " Costing Type
         ID     TYPE ZID1,           " ID
         POPER  TYPE POPER,          " Month
         UPGVC  TYPE ZUPGVC,         " UPG
         COMPN  TYPE IDNRK,          " Component
         KSTAR  TYPE KSTAR,          " Cost Element
         LIFNR  TYPE LIFNR,
         PROFL  TYPE ADGE_PROFL,     " Source
         KZUST1 TYPE ZKZUST1,        " Reason
         WERTN1 TYPE ZWERTN1_1,      " Price
         KZUST2 TYPE ZKZUST2,        " Reason
         WERTN2 TYPE ZWERTN2_1,      " Price
         KZUST3 TYPE ZKZUST3,        " Reason
         WERTN3 TYPE ZWERTN3,        " Price
         MAKTG  TYPE MAKTG,          " Desc.
* UD1K941594 by IG.MOON 9/18/07
* {
         ZRCLSS TYPE ZRCLSS,
         INFRSN TYPE ZKZUST1,
         EKGRP  TYPE EKGRP,
* }
       END OF TY_106.

TYPES: BEGIN OF TY_ITAB,
*         KOKRS  TYPE KOKRS,          " Cotrolling Area
*         BDATJ  TYPE BDATJ,          " Fiscal Year
*         KALKA  TYPE CK_KALKA,       " Costing Type
         ID     TYPE ZID1,           " ID
         POPER  TYPE POPER,          " Month
         UPGVC  TYPE ZUPGVC,         " UPG
         COMPN  TYPE IDNRK,          " Component
         KSTAR  TYPE KSTAR,          " Cost Element
         LIFNR  TYPE LIFNR,
         PROFL  TYPE ADGE_PROFL,     " Source
         KZUST  TYPE ZKZUST1,        " Reason
         MAKTG  TYPE MAKTG,          " Desc.

         WERTN  TYPE ZWERTN1_1,      " Price
* UD1K941594 by IG.MOON 9/18/07
* {
         ZRCLSS TYPE ZRCLSS,
         INFRSN TYPE ZKZUST1,
         EKGRP  TYPE EKGRP,
* }
       END OF TY_ITAB.

TYPES: BEGIN OF TY_UPG,
         IDNRK(8),                   " UPG
         UPGTXT TYPE MAKTG,          " UPG Text
       END OF TY_UPG.

TYPES: BEGIN OF TY_104,
         KALKA      TYPE CK_KALKA,   " Costing Type
         ID         TYPE ZID1,       " ID
         ABP_YEAR   TYPE ZABP_YEAR,  " ABP Year
         ABP_POPER  TYPE ZABP_POPER, " ABP Period
         BASE_YEAR  TYPE ZBASE_YEAR, " Base Year
         BASE_POPER TYPE ZBASE_POPER," Base Period
       END OF TY_104.

TYPES: BEGIN OF TY_102,
         BDATJ TYPE BDATJ,           " Fiscal Year
         POPER TYPE POPER,           " Period
         KALKA TYPE KALKA,           " Costing Type
         MATNR TYPE MATNR,           " Material
         EKGRP TYPE EKGRP,           " Pur.Grp
         WERTN TYPE ZWERTN,          " Price
         LIFNR TYPE LIFNR,           " Vendor
         ZTEXT TYPE KTEXT15,
*         MAKTG TYPE MAKTG,           " Desc.
       END OF TY_102.

TYPES: BEGIN OF TY_OUT,
         KOKRS  TYPE KOKRS,          " Control Area
         BDATJ  TYPE BDATJ,          " Fiscal Year
         POPER  TYPE POPER,          " Period
         UPGVC  TYPE MATNR,          " UPG
         UPGTXT TYPE MAKTG,          " UPG Text
         KSTAR  TYPE KSTAR,          " Cost Element
         COMPN  TYPE IDNRK,          " Component
         MAKTG  TYPE MAKTG,          " Desc.
         PROFL  TYPE PROFL,          " Source
         EKGRP  TYPE EKGRP,          " Purchasing group
         LIFNR  TYPE LIFNR,          " Vendor
*         FAMT   TYPE ZWERTN,         " Price of from period
*         TAMT   TYPE ZWERTN,         " Price of to period

         KZUST  TYPE KZUST ,
         ZTEXT  TYPE KTEXT15,
*         AMT    TYPE ZWERTN,
         AMT000 TYPE ZWERTN,
         AMT001 TYPE ZWERTN,
         AMT002 TYPE ZWERTN,
         AMT003 TYPE ZWERTN,
         AMT004 TYPE ZWERTN,
         AMT005 TYPE ZWERTN,
         AMT006 TYPE ZWERTN,
         AMT007 TYPE ZWERTN,
         AMT008 TYPE ZWERTN,
         AMT009 TYPE ZWERTN,
         AMT010 TYPE ZWERTN,
         AMT011 TYPE ZWERTN,
         AMT012 TYPE ZWERTN,
         AMTXXX TYPE ZWERTN,
* UD1K941594 by IG.MOON 9/18/07
* {
         ZRCLSS TYPE ZRCLSS,
         INFRSN TYPE ZKZUST1,
* }
       END OF TY_OUT.

DATA: GT_106  TYPE TABLE OF TY_106   WITH HEADER LINE,
      ITAB    TYPE TABLE OF TY_ITAB  WITH HEADER LINE,
      GT_UPG  TYPE TABLE OF TY_UPG   WITH HEADER LINE,
      GT_104  TYPE TABLE OF TY_104   WITH HEADER LINE,
      GT_102  TYPE TABLE OF TY_102   WITH HEADER LINE,
      GT_OUT  TYPE TABLE OF TY_OUT   WITH HEADER LINE.

DATA: GV_RCNT TYPE I,
      GV_CNT TYPE I.

INCLUDE ZACOUI00.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME.
PARAMETERS: P_KOKRS LIKE ZTCOU106-KOKRS OBLIGATORY
                                        MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCOU106-BDATJ OBLIGATORY MEMORY ID BDTJ,
            P_KALKA LIKE ZTCOU106-KALKA MEMORY ID KKA,
            P_ID    LIKE ZTCOU104-ID OBLIGATORY MEMORY ID ZCOUID
                                     MATCHCODE OBJECT ZID.
SELECT-OPTIONS : S_POPER FOR ZTCOU102-POPER OBLIGATORY NO-EXTENSION,
* UD1K941594 by IG.MOON 9/18/07
* {

                S_ZRCLSS FOR ZTCOU106-ZRCLSS,
                S_EKGRP FOR ZTCOU106-EKGRP.
* }

PARAMETER: P_ABP    AS CHECKBOX.
*           P_detail AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B0.

SELECT-OPTIONS:
  S_COMPN FOR ZTCOU106-COMPN,
  S_UPGVC FOR ZTCOU106-UPGVC,
  S_KZUST FOR ZTCOU106-KZUST.


* Layout
SELECTION-SCREEN BEGIN OF BLOCK B1S WITH FRAME TITLE TEXT-001.
PARAMETERS: P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B1S.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  READ TABLE S_POPER INDEX 1.
  IF S_POPER-HIGH IS INITIAL. S_POPER-HIGH = S_POPER-LOW. ENDIF.
  PERFORM GET_DATA.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH 'No data found.'.
    EXIT.
  ENDIF.

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
* Get reason code
  PERFORM GET_REASON_FOR_POSSIBLE_ENTRY.

  CLEAR   GT_106.
  REFRESH GT_106.

* Get Calculate Variances Data from Table ZTCOU106
  SELECT A~KOKRS A~BDATJ A~KALKA A~ID A~POPER A~UPGVC A~COMPN
         A~KSTAR A~LIFNR A~PROFL A~KZUST1 A~WERTN1 A~KZUST2 A~WERTN2
         A~KZUST3 A~WERTN3
         B~MAKTG
* UD1K941594 by IG.MOON 9/18/07
* {
           A~ZRCLSS
           D~KZUST1 AS INFRSN
           A~EKGRP
* }
          INTO CORRESPONDING FIELDS OF TABLE GT_106
    FROM ZTCOU106 AS A
    JOIN MAKT AS B
      ON B~MATNR = A~COMPN
     AND B~SPRAS = SY-LANGU
* UD1K941594 by IG.MOON 9/18/07
* {
      LEFT OUTER JOIN ZTCOU102 AS D
        ON  D~KOKRS EQ A~KOKRS
        AND D~BDATJ EQ A~BDATJ
        AND D~POPER EQ A~POPER
        AND D~KALKA EQ A~KALKA
        AND D~VER   EQ SPACE
        AND D~MATNR EQ A~COMPN
* }
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = P_YEAR
     AND A~KALKA = P_KALKA
     AND A~ID = P_ID
     AND A~POPER IN S_POPER
     AND A~COMPN IN S_COMPN
     AND A~UPGVC IN S_UPGVC
     AND A~KZUST IN S_KZUST
     AND A~ZRCLSS IN S_ZRCLSS
     AND A~EKGRP IN S_EKGRP.

  IF SY-SUBRC = 0.
    PERFORM GET_ITAB.
    PERFORM GET_UPGTXT.     " Get UPG Description
    PERFORM GET_GT_104.     " Get Variance Analysis Code Info.
    PERFORM GET_GT_102.     " Get Costing Result
    PERFORM GET_GT_OUT.     " Create internal table GT_OUT for display
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_ZTCOU104
*&---------------------------------------------------------------------*
*       Get Variance Analysis Code Info.
*----------------------------------------------------------------------*
FORM GET_GT_104.
  CLEAR GT_104.
  REFRESH GT_104.

  SELECT KALKA ID
*  ABP_YEAR
*  ABP_POPER
  BASE_YEAR BASE_POPER
    INTO TABLE GT_104
    FROM ZTCOU104
    WHERE KOKRS = P_KOKRS
      AND BDATJ = P_YEAR
      AND ID = P_ID.

  SORT GT_104 BY ID.

ENDFORM.                    " GET_GT_ZTCOU104
*&---------------------------------------------------------------------*
*&      Form  GET_GT_102
*&---------------------------------------------------------------------*
*       Get Costing Result
*----------------------------------------------------------------------*
FORM GET_GT_102.
* Select data from ZTCOU102 as applicable year
*  check p_detail = 'X'.

  CLEAR GT_102.
  REFRESH GT_102.

  SELECT A~BDATJ A~POPER A~KALKA A~MATNR
         A~EKGRP A~WERTN A~LIFNR A~ZTEXT
*        B~MAKTG
    INTO TABLE GT_102
    FROM ZTCOU102 AS A
*    JOIN MAKT AS B
*      ON B~MATNR = A~MATNR
*     AND B~SPRAS = SY-LANGU
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = P_YEAR
     AND A~KALKA = P_KALKA
     AND A~MATNR IN S_COMPN.

  SORT GT_102 BY BDATJ POPER MATNR.

ENDFORM.                    " GET_GT_102
*&---------------------------------------------------------------------*
*&      Form  GET_gt_out
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  DATA: L_YEAR  TYPE BDATJ,
        L_POPER TYPE POPER,
        L_FAMT  TYPE ZWERTN,
        L_TAMT  TYPE ZWERTN,
        L_AMT   TYPE ZWERTN,
        L_EKGRP TYPE EKGRP,
        L_LIFNR TYPE LIFNR.

  LOOP AT ITAB.

    MOVE-CORRESPONDING ITAB TO GT_OUT.
**   from Year/Period
**   . if checkd ABP, from period is ABP period
**   . if period is 1, calclate period is base period
*    CLEAR: L_YEAR, L_POPER, L_FAMT, L_TAMT, L_AMT.
*
    READ TABLE GT_104 WITH KEY ID = GT_106-ID BINARY SEARCH.
    CHECK SY-SUBRC = 0.

    IF P_ABP = 'X'.
      L_YEAR  = GT_104-ABP_YEAR.
      L_POPER = GT_104-ABP_POPER.
    ELSE.
      L_YEAR = P_YEAR.
      L_POPER = ITAB-POPER.
    ENDIF.

*---102 infomation
*    if p_detail = 'X'.
    READ TABLE GT_102 WITH KEY BDATJ = L_YEAR
                               POPER = ITAB-POPER
                               MATNR = ITAB-COMPN
                      BINARY SEARCH.
*    GT_OUT-EKGRP  = GT_102-EKGRP.                " Purchasing group
    GT_OUT-ZTEXT  = GT_102-ZTEXT.                " User Comments
*    endif.

*---106 information
    GT_OUT-MAKTG  = ITAB-MAKTG.
    GT_OUT-PROFL  = ITAB-PROFL.             " Source

    GT_OUT-LIFNR  = ITAB-LIFNR.                " Vendor


*    GT_OUT-KOKRS  = ITAB-KOKRS.             " Control Area
*    GT_OUT-BDATJ  = ITAB-BDATJ.             " Fiscal Year
    GT_OUT-UPGVC  = ITAB-UPGVC.             " UPG
    GT_OUT-KSTAR  = ITAB-KSTAR.             " Cost Element
    GT_OUT-COMPN  = ITAB-COMPN.             " Component

    IF ITAB-KZUST(1) = 'X'.
      GT_OUT-KZUST  = ITAB-KZUST.
    ELSE.
      CONCATENATE ITAB-KZUST(1) ITAB-KZUST+2(1) INTO GT_OUT-KZUST.
    ENDIF.


    PERFORM SPLIT_TO_PERIOD.

*    APPEND GT_OUT.
    COLLECT GT_OUT.

    CLEAR GT_OUT.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_OUT.

* UPG Desc.
  LOOP AT GT_UPG.
    GT_OUT-UPGTXT = GT_UPG-UPGTXT.
    MODIFY GT_OUT TRANSPORTING UPGTXT
      WHERE UPGVC+0(7) = GT_UPG-IDNRK+0(7).
  ENDLOOP.

ENDFORM.                    " GET_gt_out
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Review
*----------------------------------------------------------------------*
FORM DISP_RESULT.
  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_EVENTS, GS_LAYOUT,
         GT_FIELDCAT[], GT_EVENTS[].

  PERFORM BUILD_FIELD_CATEGORY USING:
    'UPGVC'      ' '  'UPG'                  18    'CHAR',
    'UPGTXT'     ' '  'Description'          25    'CHAR',
    'KSTAR'      ' '  'Cost E'               10    'CHAR',
    'COMPN'      ' '  'Component'            18    'CHAR',
    'MAKTG'      ' '  'Description'          25    'CHAR',
    'LIFNR'      ' '  'Vendor'               10    'CHAR',
    'EKGRP'      ' '  'PurG'                  3    'CHAR',
    'INFRSN'     ' '  'Info.'                2    'CHAR',
    'ZRCLSS'     ' '  'C'                    1    'CHAR',
    'PROFL'      ' '  'Source'               1     'CHAR',
*    'ZTEXT'      ' '  'Rsn.desc'             15    'CHAR',
*    'FAMT'       ' '  'Price of from period' 15    'CURR',
*    'TAMT'       ' '  'Price of to period'   15    'CURR',

    'KZUST'      ' '  'Reason'               02    'CHAR',
    'AMT000'     ' '  'Chg$-00'              12    'CURR',
    'AMT001'     ' '  'Chg$-01'              12    'CURR',
    'AMT002'     ' '  'Chg$-02'              12    'CURR',
    'AMT003'     ' '  'Chg$-03'              12    'CURR',
    'AMT004'     ' '  'Chg$-04'              12    'CURR',
    'AMT005'     ' '  'Chg$-05'              12    'CURR',
    'AMT006'     ' '  'Chg$-06'              12    'CURR',
    'AMT007'     ' '  'Chg$-07'              12    'CURR',
    'AMT008'     ' '  'Chg$-08'              12    'CURR',
    'AMT009'     ' '  'Chg$-09'              12    'CURR',
    'AMT010'     ' '  'Chg$-10'              12    'CURR',
    'AMT011'     ' '  'Chg$-11'              12    'CURR',
    'AMT012'     ' '  'Chg$-12'              12    'CURR',
    'AMTXXX'     ' '  'Chg$-Tot'             12    'CURR'.


  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    IF GS_FIELDCAT-FIELDNAME EQ 'ZRCLSS' or
        GS_FIELDCAT-FIELDNAME EQ 'EKGRP'.
      GS_FIELDCAT-REF_TABNAME   = 'ZTCOU106'.
      GS_FIELDCAT-REF_FIELDNAME   = GS_FCAT-FIELDNAME.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING REF_TABNAME
                                               REF_FIELDNAME
                     WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.
    ENDIF.

    IF GS_FIELDCAT-FIELDNAME EQ 'KZUST' OR
        GS_FIELDCAT-FIELDNAME EQ 'INFRSN'.
      GS_FIELDCAT-REF_TABNAME   = 'ZTCOUM02'.
      GS_FIELDCAT-REF_FIELDNAME   = 'RGRP2'.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING REF_TABNAME
                                               REF_FIELDNAME
                     WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.
    ENDIF.

    IF GS_FIELDCAT-FIELDNAME EQ 'ZRESC'.
      GS_FIELDCAT-REF_TABNAME   = 'ZTCOUM02'.
      GS_FIELDCAT-REF_FIELDNAME   = 'ALU'.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING REF_TABNAME
                                               REF_FIELDNAME
                     WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.
    ENDIF.

    IF GS_FIELDCAT-FIELDNAME EQ 'ZRESP'.
      GS_FIELDCAT-REF_TABNAME   = 'ZTCOUM01'.
      GS_FIELDCAT-REF_FIELDNAME   = 'ZRESP'.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING REF_TABNAME
                                               REF_FIELDNAME
                     WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.
    ENDIF.

  ENDLOOP.

  DATA : N(3), L_FIELD(5).
  CLEAR: N, L_FIELD.

*  LOOP AT GT_RSN.
*    CLEAR L_FIELD.
*    N = N + 1.
*    CONCATENATE 'AMT' N INTO L_FIELD.
*    GS_FIELDCAT-FIELDNAME = L_FIELD.
*    GS_FIELDCAT-SELTEXT_L = GT_RSN-KZUST.
*    GS_FIELDCAT-OUTPUTLEN = 15.
*    GS_FIELDCAT-DATATYPE  = 'CURR'.
*
*    APPEND GS_FIELDCAT TO GT_FIELDCAT.
*  ENDLOOP.

  GS_FIELDCAT-EMPHASIZE = 'C500'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING EMPHASIZE
         WHERE FIELDNAME+0(3) = 'AMT'.

  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME  = 'TABCOLOR'.

  PERFORM SET_LIST_HEADER USING GT_LIST_TOP_OF_PAGE.
  PERFORM SET_EVENTS CHANGING GT_EVENTS.

  GV_REPID = GS_VARIANT-REPORT = SY-REPID.

* Set variant
  GS_VARIANT-VARIANT = P_VARI.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GC_PF_STATUS_SET
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
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.

  LS_LINE-KEY  = 'Controlling Area'.
  LS_LINE-INFO = P_KOKRS.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Fiscal Year'.
  LS_LINE-INFO = P_YEAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'ID'.
  LS_LINE-INFO = P_ID.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Period'.
  CONCATENATE S_POPER-LOW '~' S_POPER-HIGH INTO LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                     " SET_LIST_HEADER
*&---------------------------------------------------------------------*
*&      Form  split_to_period
*&---------------------------------------------------------------------*
*       Get price by reason code
*----------------------------------------------------------------------*
FORM SPLIT_TO_PERIOD.
  DATA: L_KZUST TYPE KZUST,
        N3(3),
        N2(2),
        L_FNAME(15).

  FIELD-SYMBOLS <FS>.

  N3 = ITAB-POPER.

  CLEAR L_FNAME.
  CONCATENATE 'GT_OUT-AMT' N3 INTO L_FNAME.
  CONDENSE L_FNAME.

  ASSIGN (L_FNAME) TO <FS>.
  <FS> =  ITAB-WERTN.

*total
  GT_OUT-AMTXXX =  ITAB-WERTN.

ENDFORM.                    " split_to_period
*&---------------------------------------------------------------------*
*&      Form  GET_UPGTXT
*&---------------------------------------------------------------------*
*       Get UPG Text
*----------------------------------------------------------------------*
FORM GET_UPGTXT.
  CLEAR GT_UPG.
  REFRESH GT_UPG.

  LOOP AT ITAB.
    CONCATENATE ITAB-UPGVC+0(7) '%' INTO GT_UPG-IDNRK.
    APPEND GT_UPG.
    CLEAR GT_UPG.
  ENDLOOP.

  SORT GT_UPG BY IDNRK.
  DELETE ADJACENT DUPLICATES FROM GT_UPG.

  LOOP AT GT_UPG.
    SELECT SINGLE MAKTG INTO GT_UPG-UPGTXT
      FROM MAKT
     WHERE MATNR LIKE GT_UPG-IDNRK
       AND SPRAS = SY-LANGU.

    IF SY-SUBRC = 0.
      MODIFY GT_UPG.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_UPGTXT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_itab
*&---------------------------------------------------------------------*
FORM GET_ITAB.
  CLEAR ITAB.
  REFRESH ITAB.

  LOOP AT GT_106.
    MOVE-CORRESPONDING GT_106 TO ITAB.

    IF NOT GT_106-KZUST1 IS INITIAL AND
       NOT GT_106-WERTN1 IS INITIAL.
      ITAB-KZUST = GT_106-KZUST1.
      ITAB-WERTN = GT_106-WERTN1.
      COLLECT ITAB.
    ENDIF.

    IF NOT GT_106-KZUST2 IS INITIAL AND
       NOT GT_106-WERTN2 IS INITIAL.
      ITAB-KZUST = GT_106-KZUST2.
      ITAB-WERTN = GT_106-WERTN2.
      COLLECT ITAB.
    ENDIF.

    IF NOT GT_106-KZUST3 IS INITIAL AND
       NOT GT_106-WERTN3 IS INITIAL.
      ITAB-KZUST = GT_106-KZUST3.
      ITAB-WERTN = GT_106-WERTN3.
      COLLECT ITAB.
    ENDIF.
  ENDLOOP.

*  SORT ITAB BY KOKRS BDATJ KALKA ID UPGVC KSTAR COMPN.
  SORT ITAB BY ID UPGVC KSTAR COMPN.

ENDFORM.                    " get_itab
