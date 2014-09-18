*----------------------------------------------------------------------
* Program ID        : ZACOU109
* Title             : [CO] Cost Detail Report
* Created on        : 10/05/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Display Cost Detail
*----------------------------------------------------------------------
REPORT ZACOU109 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU109_TOP.

* UD1K941189 - by IG.MOON 8/1/2007 {
TYPE-POOLS:   CCS00,  CCS01,  CKMV0,  CKMV3,  VRM,  CKMD,  CKRU0.
TYPES:
   T_CKMLLACR_TYPE     TYPE STANDARD TABLE OF CKMLLACR
                            WITH KEY KALNR POPER BDATJ UNTPER CURTP.
DATA : GT_SORT_MOON            TYPE SLIS_T_SORTINFO_ALV,
       GS_SORT_MOON            TYPE SLIS_SORTINFO_ALV.

DATA: IT_PRKEKO       LIKE CKMLPRKEKO OCCURS 0 WITH HEADER LINE.
DATA: IT_PRKEPH       LIKE CKMLPRKEPH OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF GT_CC_AMT OCCURS 0 ,
        ELEMT    TYPE KSTAR,
        DMBTR    TYPE DMBTR,
        DMBTR_F  TYPE DMBTR,
       END OF GT_CC_AMT.


* }

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-000.
PARAMETERS: P_KOKRS LIKE KEKO-KOKRS OBLIGATORY
                                    MEMORY ID CAC
                                    MATCHCODE OBJECT FC_KOKRS,
            P_KALKA LIKE KEKO-KALKA OBLIGATORY MEMORY ID KKA,
            P_VER   LIKE ZTCOU103-VER,
            P_YEAR  LIKE KEKO-BDATJ OBLIGATORY MEMORY ID BDTJ.
SELECT-OPTIONS :
            S_POPER FOR ZTCOU103-POPER NO-EXTENSION
                        MEMORY ID POPR.

PARAMETERS: P_DLV  AS CHECKBOX,   " Exclude Delivery cost
            P_QTY  AS CHECKBOX,   " Quantity Comparison

            " Period compare - FSC 1EA
            P_MON  AS CHECKBOX USER-COMMAND UCOM,

            P_FSC  TYPE I OBLIGATORY DEFAULT 20,
            P_UNIT AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MTART FOR MARA-MTART,
                S_MATNR FOR ZTCOU103-ARTNR,
                S_UPG   FOR ZTCOU103-UPGVC,
                S_COMPN FOR ZTCOU103-COMPN.
SELECTION-SCREEN END OF BLOCK B1.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
PARAMETERS P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* At Selection-Screen
*----------------------------------------------------------------------*
* UD1K941189 - by IG.MOON {
AT SELECTION-SCREEN .
  CASE SSCRFIELDS-UCOMM.
    WHEN 'UCOM'.
      PERFORM MODIFY_SCREEN.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.
* }

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  READ TABLE S_POPER INDEX 1.
  IF S_POPER-HIGH IS INITIAL.
    S_POPER-HIGH = S_POPER-LOW.
  ENDIF.

  PERFORM GET_DATA.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM DISP_RESULT.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get cost detail information
*----------------------------------------------------------------------*
FORM GET_DATA.

  CLEAR G_ERROR.
* Get Material Ranges
  PERFORM GET_R_MATNR.
  CHECK G_ERROR EQ SPACE.

* Get cost detail information from table ZTCOU103
  PERFORM GET_103.

  PERFORM GET_VC_COMPONENT.

* Create Internal Table GT_VC for UPG Desc.
  PERFORM GET_UPGTXT.

* Create Internal Table GT_102 for Reason
  PERFORM GET_GT_102.

* Create Internal Table GT_STD for MAP, STD
  PERFORM GET_STPRS.

* UD1K941194 - by IG.MOON 8/1/2007 {
  PERFORM GET_103_MANIPULATION.
* }

* Create Internal Table GT_OUT for display
  PERFORM GET_GT_OUT.

  SORT GT_OUT BY UPGVC KSTAR COMPN.
  DELETE ADJACENT DUPLICATES FROM GT_OUT.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  LOOP AT GT_VCIT.  "GT_COST.
*    MOVE-CORRESPONDING GT_COST TO GT_OUT.
    MOVE-CORRESPONDING GT_VCIT TO GT_OUT.

    GV_IDX = GV_IDX + 1.
    GT_OUT-IDX = GV_IDX.     " Index

*   Get FSC Usage
    PERFORM GET_FSC_USAGE.

    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

* Apply FSC Usage by component & UPG
  PERFORM APPLY_FSC_USGE.

* Get InfoDetail
  PERFORM GET_INFO_DETAIL.

* Get UPG Desc.
  LOOP AT GT_VC.
    GT_OUT-UPGTXT = GT_VC-UPGTXT.
    MODIFY GT_OUT TRANSPORTING UPGTXT
        WHERE UPGVC = GT_VC-UPGVC.
*       WHERE UPGVC+0(7) = GT_VC-UPGVC+0(7).
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_UPGTXT
*&---------------------------------------------------------------------*
*       Get UPG Desc. & Material group, Src
*----------------------------------------------------------------------*
FORM GET_UPGTXT.
  SORT GT_VC BY UPGVC.

  DELETE ADJACENT DUPLICATES FROM GT_VC.

  LOOP AT GT_VC.
    SELECT SINGLE MAKTX INTO GT_VC-UPGTXT
      FROM MAKT
     WHERE MATNR LIKE GT_VC-UPGVC.
    IF SY-SUBRC = 0.
      MODIFY GT_VC.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_UPGTXT
*&---------------------------------------------------------------------*
*&      Form  GET_REASON
*&---------------------------------------------------------------------*
*       Get info detail
*----------------------------------------------------------------------*
*       Get the reason from Table ZTCOU102,
*       if it has no data, retrieve from info-record: Table A018, KONH
*----------------------------------------------------------------------*
FORM GET_INFO_DETAIL.
  DATA: L_IDX TYPE SY-INDEX.

*  SORT GT_STD BY MATNR.
  SORT GT_STD BY KALNR.

  LOOP AT GT_OUT.
    L_IDX = SY-TABIX.

    READ TABLE GT_102 WITH KEY MATNR = GT_OUT-COMPN BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_OUT-LIFNR = GT_102-LIFNR.          " Vendor
      GT_OUT-KZUST = GT_102-KZUST1.         " Reason

      GT_OUT-WERTN = GT_102-WERTN.         " Price
      GT_OUT-PEINH = GT_102-PEINH.         " Price Unit
      GT_OUT-PMEHT = GT_102-PMEHT.         " Price Qty Unit

    ENDIF.

*    READ TABLE GT_STD WITH KEY MATNR = GT_OUT-COMPN BINARY SEARCH.
    READ TABLE GT_STD WITH KEY KALNR = GT_OUT-KALNR BINARY SEARCH.

    IF SY-SUBRC = 0.
      GT_OUT-VERPR = GT_STD-PVPRS.      " Moving avg.price
      GT_OUT-STPRS = GT_STD-STPRS.      " Standard price
    ENDIF.

    MODIFY GT_OUT INDEX L_IDX TRANSPORTING
        LIFNR KZUST WERTN PEINH PMEHT VERPR STPRS.
  ENDLOOP.

ENDFORM.                    " GET_INFO_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_FSC_USAGE
*&---------------------------------------------------------------------*
*       Get FSC Usage & Price
*----------------------------------------------------------------------*
FORM GET_FSC_USAGE.
  DATA: L_MENGE(15)  TYPE P DECIMALS 3,
        L_WERTN      TYPE ZWERTN.

* Get price information from table ZTCOU103
  READ TABLE GT_103 WITH KEY COMPN = GT_OUT-COMPN
                             UPGVC = GT_OUT-UPGVC
                      BINARY SEARCH.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  CLEAR: GT_FSC, GT_MAT, L_MENGE, L_WERTN.

  LOOP AT GT_103 FROM SY-TABIX.
    IF GT_103-COMPN = GT_OUT-COMPN AND
       GT_103-UPGVC = GT_OUT-UPGVC.

*   when choose [Exclude Delivery cost]
*   : Info-price ... total price - ( duty  + freight + other)
      IF P_DLV = 'X'.
        GT_103-WERTN = GT_103-WERTN -
                       ( GT_103-DUTY + GT_103-FRG + GT_103-OTH ).
      ENDIF.

      L_MENGE = L_MENGE + GT_103-MENGE.
      L_WERTN = L_WERTN + GT_103-WERTN.

*    Create Internal Table GT_MAT for usage of FSC
      GT_MAT-ARTNR = GT_103-ARTNR.      " Product
      APPEND GT_MAT.

*    Create Internal Table GT_FSC for FSC
      GT_FSC-IDX   = GV_IDX.            " Index (UPG + COMPN)
      GT_FSC-ARTNR = GT_103-ARTNR.      " Product
      GT_FSC-MENGE = GT_103-MENGE.      " Qty
      GT_FSC-WERTN = GT_103-WERTN.      " Price
      COLLECT GT_FSC.

*     MOVE to MAIN TABLE
      MOVE-CORRESPONDING GT_103 TO GT_OUT.
* UD1K941189 - by IG.MOON 8/1/2007 {
      IF P_MON = 'X'.
        GT_OUT-ARTNR = GT_103-$ARTNR.
      ENDIF.
* }
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  SORT GT_FSC BY IDX ARTNR.

  GT_OUT-WERTT = L_WERTN.                " Total Amt
  GT_OUT-MENGT = L_MENGE.                " Total Qty.

ENDFORM.                    " GET_FSC_USAGE
*&---------------------------------------------------------------------*
*&      Form  GET_GT_102
*&---------------------------------------------------------------------*
*       Create Internal Table GT_102 for Reason
*----------------------------------------------------------------------*
FORM GET_GT_102.
  IF NOT GT_VCIT[] IS INITIAL.
    CLEAR GT_102.
    REFRESH GT_102.

    SELECT MATNR WERKS LIFNR KZUST1
           WERTN PEINH PMEHT
      INTO TABLE GT_102
      FROM ZTCOU102
       FOR ALL ENTRIES IN GT_VCIT
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND POPER = S_POPER-HIGH
       AND KALKA = P_KALKA
       AND MATNR = GT_VCIT-COMPN.
  ENDIF.

  SORT GT_102 BY MATNR.

ENDFORM.                    " GET_GT_102
*&---------------------------------------------------------------------*
*&      Form  GET_STPRS
*&---------------------------------------------------------------------*
*       Get MAP, STD
*----------------------------------------------------------------------*
FORM GET_STPRS.
* Get Price unit, MAP, STD
  IF NOT GT_VCIT[] IS INITIAL.
    CLEAR GT_STD.
    REFRESH GT_STD.

    SELECT MATNR BWKEY STPRS PVPRS B~KALNR
      INTO TABLE GT_STD
      FROM CKMLCR AS A
      JOIN CKMLHD AS B
        ON B~KALNR = A~KALNR
       FOR ALL ENTRIES IN GT_VCIT
     WHERE A~BDATJ = P_YEAR
       AND A~POPER = S_POPER-HIGH
       AND A~UNTPER = '000'
       AND A~CURTP = '10'
       AND B~MATNR = GT_VCIT-COMPN.
*      AND B~BWKEY = GT_COST-WERKS.
  ENDIF.

  SORT GT_STD BY MATNR BWKEY.

ENDFORM.                    " GET_STPRS
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Detail
*----------------------------------------------------------------------*
FORM DISP_RESULT.
  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_EVENTS, GS_VARIANT,
         GT_FIELDCAT[], GT_EVENTS[].

* UD1K941189 - by IG.MOON 8/1/2007 {
  CLEAR GT_SORT_MOON[].
  IF P_MON EQ 'X'.
    READ TABLE R_MATNR INDEX 2.
    IF SY-SUBRC EQ 0.
      PERFORM BUILD_FIELD_CATEGORY USING:
      'ARTNR'   'X'  'Product'          18  'CHAR'.
    ENDIF.
  ENDIF.

* }
  PERFORM BUILD_FIELD_CATEGORY USING:
    'UPGVC'   'X'  'UPG'          18  'CHAR',
    'UPGTXT'  'X'  'Description'  50  'CHAR',
    'KSTAR'   'X'  'Cost E'       10  'CHAR',
    'COMPN'   'X'  'Component'    18  'CHAR',
    'MAKTX'   'X'  'Descrption'   50  'CHAR',
    'PROFL'   ' '  'Src'          1   'CHAR',
    'EKGRP'   ' '  'Pur.Grp'      3   'CHAR',
    'LIFNR'   ' '  'Vendor'       10  'CHAR',
    'WERTN'   ' '  'Info-Price'   15  'CURR',
    'PEINH'   ' '  'Price unit'   5   'DEC',
    'PMEHT'   ' '  'UoM-PO'       5   'UNIT',
    'KZUST'   ' '  'RSN'          3   'CHAR',
    'VERPR'   ' '  'MAP'          11  'CURR',
    'STPRS'   ' '  'STD'          11  'CURR',
    'MEEHT'   ' '  'UoM'          5   'UNIT'.

  PERFORM MODIFY_GT_FIELDCAT.

  PERFORM SORT_BUILD        USING GT_SORT_MOON[].

  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME  = 'TABCOLOR'.

  PERFORM SET_COLOR.
  PERFORM SET_COMMENT USING GT_LIST_TOP_OF_PAGE.
  PERFORM SET_EVENTS CHANGING GT_EVENTS.

  GV_REPID = GS_VARIANT-REPORT = SY-REPID.

* Set variant
  GS_VARIANT-VARIANT = P_VARI.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = GV_REPID
            IS_LAYOUT          = GS_LAYOUT
            IT_FIELDCAT        = GT_FIELDCAT[]
            IT_EVENTS          = GT_EVENTS[]
            IT_SORT            = GT_SORT_MOON[]
            I_SAVE             = 'A'
            IS_VARIANT         = GS_VARIANT
       TABLES
            T_OUTTAB           = GT_OUT.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM MODIFY_GT_FIELDCAT.
  DATA: N(2), L_FIELD(15).

  CLEAR: N, L_FIELD.

  PERFORM BUILD_FIELD_CATEGORY USING:
      'WAVG'   'X'  'Avg.Cost'    15  'CURR'.

  IF P_QTY = 'X'.
    PERFORM BUILD_FIELD_CATEGORY USING:
      'MAVG'   'X'  'Avg.Qty'     15  'QUAN'.
  ENDIF.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'UPGVC'.
        GS_FIELDCAT-KEY = 'X'.
        MODIFY GT_FIELDCAT FROM GS_FIELDCAT
               TRANSPORTING KEY WHERE FIELDNAME = 'UPGVC'.

      WHEN 'PROFL' OR 'KZUST' OR 'EKGRP'.
        GS_FIELDCAT-JUST = 'C'.
        MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING JUST
         WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.

      WHEN 'STPRS' OR 'VERPR'.
        GS_FIELDCAT-REF_FIELDNAME = 'STPRS'.
        GS_FIELDCAT-REF_TABNAME = 'CKMLCR'.

        MODIFY GT_FIELDCAT FROM GS_FIELDCAT
               TRANSPORTING REF_FIELDNAME REF_TABNAME
         WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.

      WHEN 'PEINH' OR 'PMEHT'.
        GS_FIELDCAT-JUST = 'C'.
        GS_FIELDCAT-REF_FIELDNAME = GS_FIELDCAT-FIELDNAME.
        GS_FIELDCAT-REF_TABNAME = 'ZTCOU102'.

        MODIFY GT_FIELDCAT FROM GS_FIELDCAT
               TRANSPORTING JUST REF_FIELDNAME REF_TABNAME
         WHERE FIELDNAME = GS_FIELDCAT-FIELDNAME.
    ENDCASE.

    IF GS_FIELDCAT-DATATYPE = 'CURR'.
      GS_FIELDCAT-JUST = 'R'.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING JUST
         WHERE DATATYPE = 'CURR'.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_MAT TO GV_FSC  WHERE CNT < '31'.
    CLEAR L_FIELD.

    N = N + 1.

    IF P_QTY = 'X'.
      CONCATENATE 'MENGE' N INTO L_FIELD.
      GS_FIELDCAT-REF_FIELDNAME = 'MENGE'.
    ELSE.
      CONCATENATE 'WERTN' N INTO L_FIELD.
      GS_FIELDCAT-REF_FIELDNAME = 'WERTN'.
    ENDIF.

    GS_FIELDCAT-FIELDNAME    = L_FIELD.
    GS_FIELDCAT-SELTEXT_L    = GT_MAT-ARTNR.
    GS_FIELDCAT-REPTEXT_DDIC = GT_MAT-ARTNR.
    GS_FIELDCAT-DDICTXT      = 'L'.
    GS_FIELDCAT-JUST         = 'R'.
    GS_FIELDCAT-DO_SUM       = 'X'.
    GS_FIELDCAT-OUTPUTLEN    = 18.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " MODIFY_GT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_COMMENT
*&---------------------------------------------------------------------*
FORM SET_COMMENT USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.

  LS_LINE-KEY  = 'Controlling Area:'.
  LS_LINE-INFO = P_KOKRS.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Costing Type:'.
  LS_LINE-INFO = P_KALKA.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Year:'.
  LS_LINE-INFO = P_YEAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*  LS_LINE-KEY  = 'Period:'.
*  LS_LINE-INFO = S_POPER-LOW.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  IF P_DLV = 'X'.
    LS_LINE-KEY = '[Exclude Dlv.cost]'.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

  IF P_QTY = 'X'.
    LS_LINE-KEY  = '[Quantity Base]'.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

* UD1K941189 - by IG.MOON 8/1/2007 {
  IF P_MON = 'X'.
    READ TABLE R_MATNR INDEX 2.
    IF SY-SUBRC NE 0.
      LS_LINE-KEY  = 'Product:'.
      READ TABLE R_MATNR INDEX 1.
      LS_LINE-INFO = R_MATNR-LOW.
      APPEND LS_LINE TO LT_TOP_OF_PAGE.
    ENDIF.
  ENDIF.
*  }

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.


*  DATA L_TEXT(60).
*  DATA S_TEXT(60).
*
*  REFRESH GT_LIST_TOP_OF_PAGE.
*
*  L_TEXT = 'Cost Comparison Report.'.
*
*  PERFORM SET_HEADER_LINE USING:
*          'P' 'H' ''                 L_TEXT       '',
*          'P' 'S' 'Controlling Area' P_KOKRS      '',
*          'P' 'S' 'Costing Type'     P_KALKA      '',
*          'P' 'S' 'Year'             P_YEAR      '',
*          'S' 'S' 'Period'           S_POPER      '',
*          'P' 'S' 'Costing Type'     P_KALKA      ''.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*       EXPORTING
*            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
*
*
ENDFORM.                    " SET_COMMENT
*&---------------------------------------------------------------------*
*&      Form  APPLY_FSC_USGE
*&---------------------------------------------------------------------*
FORM APPLY_FSC_USGE.
  DATA: L_CNT TYPE I,
        L_IDX TYPE I,
        N(2),
        L_CHAR(15),
        L_MCNT TYPE I,
        L_WCNT TYPE I.

  FIELD-SYMBOLS: <FS1>,
                 <FS2>.

  CLEAR: GV_MCNT, L_CNT, N, L_CHAR, GV_FSC, L_IDX, N.

* Adjust FSC count
  SORT GT_MAT BY ARTNR.
  DELETE ADJACENT DUPLICATES FROM GT_MAT.

  LOOP AT GT_MAT.
    L_CNT = L_CNT + 1.
    GT_MAT-CNT = L_CNT.
    MODIFY GT_MAT.
  ENDLOOP.

  IF GV_MCNT < P_FSC.
    GV_FSC = L_CNT.
  ELSE.
    GV_FSC = P_FSC.
  ENDIF.

* Apply FSC, Qty, price by Component & UPG
  LOOP AT GT_OUT.
    CLEAR: L_MCNT, L_WCNT.
    L_IDX = L_IDX + 1.

    LOOP AT GT_MAT WHERE CNT < '31'.
      READ TABLE GT_FSC WITH KEY IDX = GT_OUT-IDX
                                 ARTNR = GT_MAT-ARTNR BINARY SEARCH.

      IF SY-SUBRC = 0.
        N = GT_MAT-CNT.

*       Usage
        CONCATENATE 'GT_OUT-MENGE' N INTO L_CHAR.
        ASSIGN (L_CHAR) TO <FS1>.
        <FS1> = GT_FSC-MENGE.

        IF GT_FSC-MENGE <> 0.
          L_MCNT = L_MCNT + 1.
        ENDIF.

*       Price
        CONCATENATE 'GT_OUT-WERTN' N INTO L_CHAR.
        ASSIGN (L_CHAR) TO <FS2>.
        <FS2> = GT_FSC-WERTN.

        IF GT_FSC-WERTN <> 0.
          L_WCNT = L_WCNT + 1.
        ENDIF.
      ENDIF.

      GT_OUT-MCNT = L_MCNT.
      GT_OUT-WCNT = L_WCNT.

    ENDLOOP.

*     Avg.Cost : FSC Usage * Info.Price / Total count of FSC ???
    IF GV_FSC <> 0.
      GT_OUT-MAVG = GT_OUT-MENGT / GV_FSC.
      GT_OUT-WAVG = GT_OUT-WERTT / GV_FSC.
    ENDIF.
*FIXME>>>>
*    if l_mcnt <> 0.
*      gt_out-mavg = gt_out-mengt / l_mcnt.
*    endif.
*    if l_wcnt <> 0.
*      gt_out-wavg = gt_out-wertt / l_wcnt.
*    endif.

    MODIFY GT_OUT INDEX L_IDX.

  ENDLOOP.

ENDFORM.                    " APPLY_FSC_USGE
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       Set Color
*----------------------------------------------------------------------*
FORM SET_COLOR.
  DATA: N(2),
        L_FNAM(7).

  CLEAR: N, GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].

  DO GV_FSC TIMES.
    N = N + 1.

    CLEAR L_FNAM.
    IF P_QTY = 'X'.
      CONCATENATE 'MENGE' N INTO L_FNAM.
    ELSE.
      CONCATENATE 'WERTN' N INTO L_FNAM.
    ENDIF.

    GS_SPECIALCOL-FIELDNAME = L_FNAM.
    GS_SPECIALCOL-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_POSITIVE.
    GS_SPECIALCOL-COLOR-INT = 0.
    APPEND GS_SPECIALCOL TO GT_SPECIALCOL.
  ENDDO.

  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  GET_R_MATNR
*&---------------------------------------------------------------------*
*       Get Material Range
*----------------------------------------------------------------------*
FORM GET_R_MATNR.
  CLEAR R_MATNR.
  REFRESH R_MATNR.

* UD1K941189 - commented by IG.MOON 8/1/2007 {
*  IF P_MON = 'X'.
*    P_FSC = 1.
*  ENDIF.
* }

  IF P_UNIT = 'X'.
    TYPES: BEGIN OF TY_MAT,
             MATNR TYPE MATNR,
           END OF TY_MAT.

    DATA LT_MAT TYPE TABLE OF TY_MAT WITH HEADER LINE.

    CLEAR LT_MAT.
    REFRESH LT_MAT.

    SELECT DISTINCT MATNR INTO TABLE LT_MAT
      FROM ZTCOU100 UP TO P_FSC ROWS
      WHERE KOKRS = P_KOKRS
        AND BDATJ = P_YEAR
        AND POPER IN S_POPER
        AND KALKA = P_KALKA
        AND MATNR IN S_MATNR.

    IF SY-SUBRC NE 0.
      MESSAGE S000 WITH 'No registered product was found in ZTCOU100'.
      G_ERROR = 'X'.
      EXIT.
    ENDIF.

    LOOP AT LT_MAT.
      R_MATNR-LOW = LT_MAT-MATNR.
      APPEND R_MATNR.
      CLEAR R_MATNR.
    ENDLOOP.

    R_MATNR-SIGN   = 'I'.
    R_MATNR-OPTION = 'EQ'.

    MODIFY R_MATNR TRANSPORTING SIGN OPTION WHERE SIGN IS INITIAL.

  ELSE.

    LOOP AT S_MATNR.
      MOVE S_MATNR TO R_MATNR.
      APPEND R_MATNR.
      CLEAR R_MATNR.
      IF SY-TABIX > P_FSC.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_R_MATNR
*&---------------------------------------------------------------------*
*&      Form  get_103
*&---------------------------------------------------------------------*
FORM GET_103.
  CLEAR:   GT_103.
  REFRESH: GT_103.

  SELECT A~WERKS ARTNR UPGVC KSTAR COMPN MENGE
         GPREIS WERTN DUTY FRG OTH A~PEINH A~MEEHT
         SPLNT BWDAT
         B~MAKTX C~MATKL C~PROFL C~MTART D~EKGRP
         A~POPER
* UD1K941189 - by IG.MOON 8/1/2007 {
         A~STKKZ A~KALNR
* }
    INTO CORRESPONDING FIELDS OF TABLE GT_103
    FROM ZTCOU103 AS A
    JOIN MAKT AS B
      ON B~MATNR = A~COMPN
     AND B~SPRAS = SY-LANGU
    JOIN MARA AS C
      ON C~MATNR = A~COMPN
    JOIN MARC AS D
      ON D~MATNR = A~COMPN
     AND D~WERKS = A~WERKS
   WHERE KOKRS = P_KOKRS
     AND BDATJ = P_YEAR
     AND POPER IN S_POPER
     AND KALKA = P_KALKA
     AND VER   = P_VER
     AND ARTNR IN R_MATNR
     AND UPGVC IN S_UPG
     AND COMPN IN S_COMPN
     AND C~MTART IN S_MTART.

ENDFORM.                                                    " get_103
*&---------------------------------------------------------------------*
*&      Form  get_vc_component
*&---------------------------------------------------------------------*
FORM GET_VC_COMPONENT.
  DATA L_IDX TYPE SY-TABIX.

* Get Cost Roll-Up
  CLEAR:   GT_VC, GT_FSC, GT_MAT, GT_OUT.
  REFRESH: GT_VC, GT_FSC, GT_MAT, GT_OUT.


  LOOP AT GT_103.
    L_IDX = SY-TABIX.

    IF P_MON = 'X'.
* UD1K941189 - by IG.MOON 8/1/2007 {
      GT_103-$ARTNR = GT_103-ARTNR.
* }
      GT_103-ARTNR = GT_103-POPER.
      MODIFY GT_103 INDEX L_IDX TRANSPORTING ARTNR $ARTNR.
    ENDIF.

*  FIXME
*   will be BOM as 7 digit
*   CONCATENATE GT_103-UPGVC+0(7) '%' INTO GT_VC-UPGVC.
    GT_VC-UPGVC = GT_103-UPGVC.

    READ TABLE GT_VC WITH KEY UPGVC = GT_VC-UPGVC.
    IF SY-SUBRC <> 0 AND GT_VC-UPGVC NE SPACE.
      APPEND GT_VC.
      CLEAR GT_VC.
    ENDIF.

    GT_VCIT-UPGVC = GT_103-UPGVC.
    GT_VCIT-COMPN = GT_103-COMPN.
    APPEND GT_VCIT.
  ENDLOOP.

  SORT GT_VCIT BY UPGVC COMPN.
  DELETE ADJACENT DUPLICATES FROM GT_VCIT.

  SORT GT_103  BY COMPN UPGVC ARTNR. "by werks compn upgvc artnr,

ENDFORM.                    " get_vc_component
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.
  LOOP AT SCREEN.
    CASE P_MON.
      WHEN 'X'.
        IF SCREEN-NAME = 'S_POPER-HIGH'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
      WHEN ' '.
        IF SCREEN-NAME = 'S_POPER-HIGH'.
          REFRESH S_POPER.
          CLEAR S_POPER.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING    FT_SORT TYPE SLIS_T_SORTINFO_ALV.

  DEFINE SORT_TAB.
    CLEAR GS_SORT_MOON.
    GS_SORT_MOON-FIELDNAME = &1.
    GS_SORT_MOON-SPOS      = &2.
    GS_SORT_MOON-UP        = &3.
    GS_SORT_MOON-GROUP     = &4.
    GS_SORT_MOON-COMP      = &5.
    APPEND GS_SORT_MOON TO FT_SORT.
  END-OF-DEFINITION.

  IF P_MON EQ 'X'.
    READ TABLE R_MATNR INDEX 2.
    IF SY-SUBRC EQ 0.
      SORT_TAB :
           'ARTNR'        ' ' 'X' 'X' 'X'.
    ENDIF.
  ENDIF.

  SORT_TAB :
             'UPGVC'        ' ' 'X' 'X' 'X',
             'UPGTXT'       ' ' 'X' 'X' 'X',
             'COMPN'        ' ' 'X' 'X' 'X',
             'MAKTX'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  GET_103_MANIPULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_103_MANIPULATION.

  DATA : LT_KALNR TYPE CKMV0_LAOBJ_TBL WITH HEADER LINE,
         IT_PRKEKO_TEMP      TYPE MLCCS_T_PRKEKO,
         IT_PRKEPH_TEMP      TYPE MLCCS_T_PRKEPH.

  RANGES :IR_PRTYP FOR MLPRKEPH-PRTYP,
          IR_CURTP FOR TKEL-CURTP.

  LOOP AT GT_103. " WHERE STKKZ EQ 'X'.
*    READ TABLE GT_STD WITH KEY MATNR = GT_103-COMPN
*                               BWKEY = GT_103-WERKS
*                               BINARY SEARCH.
*    CHECK SY-SUBRC = 0 .
*    LT_KALNR-KALNR  = GT_STD-KALNR .

    LT_KALNR-KALNR  = GT_103-KALNR .
    COLLECT LT_KALNR.  CLEAR LT_KALNR.
  ENDLOOP.

  SORT LT_KALNR.
  DELETE ADJACENT DUPLICATES FROM LT_KALNR.

  IR_PRTYP = 'IEQS'.
  APPEND IR_PRTYP.

  IR_PRTYP = 'IEQV'.
  APPEND IR_PRTYP.

  DATA : $STR(10),
         $CODE(2),
         $PROPER(3) TYPE N.

  __CLS : IT_PRKEKO,IT_PRKEPH.

  DO 12 TIMES.
    $PROPER = SY-INDEX.
    CHECK $PROPER IN S_POPER.

    __CLS : IT_PRKEKO_TEMP, IT_PRKEPH_TEMP.

    CALL FUNCTION 'MLCCS_READ_PR'
         EXPORTING
              I_USE_BUFFER            = SPACE
              I_BDATJ_1               = P_YEAR
              I_POPER_1               = $PROPER
         IMPORTING
              ET_PRKEKO               = IT_PRKEKO_TEMP
              ET_PRKEPH               = IT_PRKEPH_TEMP
         TABLES
              IT_KALNR                = LT_KALNR
              IR_PRTYP                = IR_PRTYP
              IR_CURTP                = IR_CURTP
         EXCEPTIONS
              NO_DATA_FOUND           = 1
              INPUT_DATA_INCONSISTENT = 2
              OTHERS                  = 3.

    APPEND LINES OF :
            IT_PRKEKO_TEMP TO IT_PRKEKO,
            IT_PRKEPH_TEMP TO IT_PRKEPH.

  ENDDO.


  PERFORM GET_MLCCS_PR.

ENDFORM.                    " GET_103_MANIPULATION
*&---------------------------------------------------------------------*
*&      Form  get_MLCCS_pr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MLCCS_PR.

  DATA : IT_TCKH3      LIKE TCKH3         OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE IT_TCKH3
      FROM TCKH3
        WHERE ELEHK = 'H1'.

  DATA : T_AMT TYPE DMBTR.

  __CLS : GT_STD.

  PERFORM GET_MLCCS_PR_TOTAL TABLES IT_TCKH3 CHANGING T_AMT.

ENDFORM.                    " get_MLCCS_pr
*&---------------------------------------------------------------------*
*&      Form  get_mlccs_pr_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MLCCS_PR_TOTAL TABLES IT_TCKH3 STRUCTURE TCKH3
                        CHANGING T_AMT.
  __CLS GT_CC_AMT.


  LOOP AT IT_PRKEPH WHERE PRTYP EQ 'S'
                      AND KKZST EQ ' '.

    __CLS GT_CC_AMT.
    CLEAR T_AMT.
    PERFORM DO_SUM TABLES IT_TCKH3
                          GT_CC_AMT
                 CHANGING T_AMT.

    GT_STD-KALNR = IT_PRKEPH-KALNR.
    GT_STD-STPRS = T_AMT.         " Standard price
    APPEND GT_STD.

  ENDLOOP.

  SORT GT_STD BY KALNR.

  LOOP AT IT_PRKEPH WHERE PRTYP EQ 'V'
                      AND KKZST EQ ' '.

    __CLS GT_CC_AMT.
    CLEAR T_AMT.
    PERFORM DO_SUM TABLES IT_TCKH3
                          GT_CC_AMT
                 CHANGING T_AMT.

    READ TABLE GT_STD WITH KEY KALNR = IT_PRKEPH-KALNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_STD-PVPRS = T_AMT.
      MODIFY GT_STD INDEX SY-TABIX.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " get_mlccs_pr_total
*&---------------------------------------------------------------------*
*&      Form  do_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DO_SUM TABLES  IT_TCKH3   STRUCTURE TCKH3
                    LT_CC_AMT  STRUCTURE GT_CC_AMT
            CHANGING T_AMT.

  FIELD-SYMBOLS: <F_FIELD> .

  DATA : L_CNT(3) TYPE N,
         L_FIELD(25),
         L_AMT TYPE P DECIMALS 6.


  DO 40 TIMES.
    L_CNT = L_CNT + 1.

    CONCATENATE 'IT_PRKEPH-KST' L_CNT INTO L_FIELD.
    ASSIGN  (L_FIELD)    TO   <F_FIELD> .
    CLEAR L_AMT.
    L_AMT = <F_FIELD>.
    CHECK NOT L_AMT IS INITIAL.

* Overall value
    CLEAR IT_TCKH3.
    READ TABLE IT_TCKH3 WITH KEY EL_HV = L_CNT.
    IF SY-SUBRC   =  0 .
      LT_CC_AMT-DMBTR   = L_AMT.
      T_AMT = T_AMT + LT_CC_AMT-DMBTR.
    ELSE.
* Fixed value
      READ TABLE IT_TCKH3 WITH KEY EL_HF = L_CNT.
      IF SY-SUBRC   = 0 .
        LT_CC_AMT-DMBTR_F = L_AMT.
      ENDIF.
    ENDIF.

    LT_CC_AMT-ELEMT = IT_TCKH3-ELEMT.

    COLLECT LT_CC_AMT. CLEAR LT_CC_AMT.

  ENDDO.

ENDFORM.                    " do_sum
