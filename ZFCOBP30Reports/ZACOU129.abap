* Program ID        : ZACOU129
* Title             : [CO] ABP Revision - Normal Efficiency Rate
* Created on        : 10/01/2007
* Created by        : IG.MOON
* Specifications By : Andy
* Description       : ABP Revision 2nd phase
*
* input:  man hour actual
* output: ztable-efficiency rate
*----------------------------------------------------------------------
REPORT ZACOU129 MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU129_TOP.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS     : P_KOKRS LIKE ZSCOU129-KOKRS OBLIGATORY
                 MEMORY ID CAC.
PARAMETERS     : P_PLNYR LIKE ZSCOU129-PLNYR OBLIGATORY
                 MEMORY ID ZPYR.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
PARAMETERS     : P_ACTYR LIKE ZSCOU129-ACTYR OBLIGATORY
                 MEMORY ID BDTJ.
SELECT-OPTIONS : S_POPER FOR ZTCO_MHV-POPER OBLIGATORY
                 MEMORY ID POPR.
SELECT-OPTIONS : S_MATNR FOR MARA-MATNR.
SELECTION-SCREEN END OF BLOCK BL2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK VIEW-RESULT WITH FRAME TITLE TEXT-T03.
SELECTION-SCREEN PUSHBUTTON  1(24) VSLT USER-COMMAND VSLT.
SELECTION-SCREEN END OF BLOCK VIEW-RESULT.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[CO] ABP Revision - Apply Normal Efficiency Rate'.
  PERFORM DEFAULT_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM :
            INITIALIZE            ,
            VALIDATE              ,
            GET_DATA              ,
            REFINE_ROW_ITAB       ,
            MOVE_OUT              .

*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'VSLT'.
      PERFORM VIEW_.
  ENDCASE.

END-OF-SELECTION.
  CHECK G_ERROR EQ SPACE .
  PERFORM SET_OUTPUT .
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OUTPUT.

  CHECK : G_ERROR IS INITIAL.
  CLEAR FLAG_DATA_CHANGED.

  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE.
  CLEAR G_ERROR.
ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDATE.
  CLEAR TKA01.

  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE S038 WITH P_KOKRS.
    G_ERROR = TRUE.
  ENDIF.

ENDFORM.                    " VALIDATE_
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_.

*  S_HKONT-SIGN = 'I'.
*  S_HKONT-OPTION = 'EQ'.
*  S_HKONT-LOW = '0000530180'.
*  APPEND S_HKONT.
*
*  S_HKONT-SIGN = 'I'.
*  S_HKONT-OPTION = 'EQ'.
*  S_HKONT-LOW = '0000532100'.
*  APPEND S_HKONT.
*
*  S_BKLAS-SIGN = 'I'.
*  S_BKLAS-OPTION = 'BT'.
*  S_BKLAS-LOW  = '3000'.
*  S_BKLAS-HIGH = '3005'.
*  APPEND S_BKLAS.

  WRITE:
          ICON_BIW_REPORT_VIEW AS ICON TO VSLT,
         'View saved data' TO VSLT+4(21).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_OUT.

  __PROCESS 'Preparing output...' '95'.

  __CLS : GT_OUT,$KOSTL.


  LOOP AT IT_ROW_TAB.
    $KOSTL-KOSTL = IT_ROW_TAB-KOSTL.
    COLLECT $KOSTL.
  ENDLOOP.

  PERFORM GET_ABTEI.

  LOOP AT IT_ROW_TAB.
    MOVE-CORRESPONDING IT_ROW_TAB TO GT_OUT.
    PERFORM CHK_RELEASED.
    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

  DATA $FLAG(1).
  LOOP AT GT_OUT.
    AT NEW KOSTL.
      $FLAG = TRUE.
    ENDAT.
    CHECK $FLAG EQ TRUE.
    CLEAR $FLAG.
    PERFORM CHK_NOR_R.
    MODIFY GT_OUT TRANSPORTING NOR_R
              WHERE KOSTL EQ GT_OUT-KOSTL.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  SAVE_z_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_Z_TABLE.

  CHECK : G_ERROR EQ FALSE.

  __PROCESS 'Saving data...' '90'.

ENDFORM.                    " SAVE_z_TABLE
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIEW_.

  __CLS IT_ROW_TAB.
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE IT_ROW_TAB
  FROM ZTCOU129
  WHERE KOKRS EQ P_KOKRS
*  AND   ACTYR EQ P_ACTYR
  AND   PLNYR EQ P_PLNYR
  AND   PERIO IN S_POPER
  AND   MATNR IN S_MATNR.

  PERFORM :
            INITIALIZE            ,
            VALIDATE              ,
            MOVE_OUT              .

  CHECK G_ERROR EQ SPACE .
  PERFORM SET_OUTPUT .

ENDFORM.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM POP_UP USING    P_TEXT P_TEXT2 P_CANC
            CHANGING P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TEXTLINE1      = P_TEXT
            TEXTLINE2      = P_TEXT2
            TITEL          = 'Check!'
            CANCEL_DISPLAY = P_CANC
       IMPORTING
            ANSWER         = P_ANSWER.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  PERFORM GET_PRODUCT_QTY.
  PERFORM GET_ROUTING.
  PERFORM GET_HR_MH.
  PERFORM GET_ROW_ITAB.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCT_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PRODUCT_QTY.

  __CLS IT_PRODQTY.
  PERFORM GET_PLANT.

  DATA : IT_PRODQTY_TEMP LIKE IT_PRODQTY OCCURS 0 WITH HEADER LINE.

  __PROCESS 'Read product quantity...' '10'.

  SELECT A~MATNR A~MTART INTO CORRESPONDING FIELDS OF TABLE IT_MARA
    FROM MARA AS A
    JOIN MARC AS B
    ON B~MATNR EQ A~MATNR
   WHERE A~MATNR IN S_MATNR
   AND ( A~MTART EQ 'FERT' OR A~MTART EQ 'HALB' ).
*   AND B~SOBSL EQ SPACE.

  SORT IT_MARA.

  CHECK NOT IT_MARA[] IS INITIAL.
  __PROCESS 'Read product quantity...' '20'.

  SELECT  B~BWKEY B~MATNR A~PERIO
          A~OUT_MENGE
          A~MEINH
    INTO CORRESPONDING FIELDS OF TABLE IT_PRODQTY_TEMP
    FROM
    ( ( CKMLMV003 AS A
    INNER JOIN CKMLMV001 AS B
       ON B~KALNR = A~KALNR_BAL )
    INNER JOIN CKMLMV013 AS C
       ON C~KALNR_PROC = A~KALNR_IN )
   WHERE A~MGTYP EQ '00001'
     AND A~GJAHR EQ P_ACTYR
     AND A~PERIO IN S_POPER
     AND A~WERKS IN R_BWKEY
     AND B~BTYP  =  'BF'
     AND C~FLG_WBWG = 'X'
     AND C~AUTYP = '05' .

  LOOP AT IT_PRODQTY_TEMP.
    MOVE-CORRESPONDING IT_PRODQTY_TEMP TO IT_PRODQTY.
    COLLECT IT_PRODQTY. CLEAR IT_PRODQTY.
  ENDLOOP.

  SORT IT_PRODQTY BY MATNR.

ENDFORM.                    " GET_PRODUCT_QTY
*&---------------------------------------------------------------------*
*&      Form  get_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ROUTING.
  CHECK G_ERROR IS INITIAL.
  DATA $IX LIKE SY-TABIX.

  __PROCESS 'Get Routing Info...' '40'.
* Rounting M/H(Labor) : by Cost center
  PERFORM SELECT_ROUTING_INFO.

  CHECK G_ERROR IS INITIAL.

  DATA : L_CNT(3) TYPE N,
         L_FIELD(40).

  __CLS TMPT.

  SORT IT_PLPO BY MATNR.

  LOOP AT IT_PRODQTY WHERE MATNR IN S_MATNR.

    READ TABLE IT_PLPO WITH KEY MATNR = IT_PRODQTY-MATNR BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    $IX = SY-TABIX.

*    LOOP AT IT_PLPO WHERE MATNR = IT_PRODQTY-MATNR .
    LOOP AT IT_PLPO FROM $IX.
      IF IT_PLPO-MATNR NE IT_PRODQTY-MATNR.
        EXIT.
      ENDIF.

      TMPT-PERIO   = IT_PRODQTY-PERIO.
      CLEAR IT_CRHD .
      READ TABLE IT_CRHD WITH KEY OBJID = IT_PLPO-ARBID BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        TMPT-KOSTL   = IT_CRHD-ARBPL.
        TMPT-ARBPL   = IT_PLPO-ARBID.
        COLLECT TMPT. CLEAR TMPT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT : TMPT BY ARBPL PERIO.

  __CLS ITAB.

  LOOP AT IT_PRODQTY WHERE MATNR IN S_MATNR.
    READ TABLE IT_PLPO WITH KEY MATNR = IT_PRODQTY-MATNR BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    $IX = SY-TABIX.

*    LOOP AT IT_PLPO WHERE MATNR = IT_PRODQTY-MATNR .
    LOOP AT IT_PLPO FROM $IX.
      IF IT_PLPO-MATNR NE IT_PRODQTY-MATNR.
        EXIT.
      ENDIF.
      ITAB-MATNR   = IT_PLPO-MATNR.
      ITAB-PERIO   = IT_PRODQTY-PERIO.
      ITAB-TYPE    = IT_PLPO-TYPE.
      ITAB-VGW01   = IT_PLPO-VGW01.    "Set
      ITAB-VGW02   = IT_PLPO-VGW02.    "Machine

      ITAB-VGE01   = IT_PLPO-VGE01.    "unit Set
      ITAB-VGE02   = IT_PLPO-VGE02.    "unit Machine
      ITAB-VGE03   = IT_PLPO-VGE03.    "unit MH

      CLEAR TMPT.
      READ TABLE TMPT WITH KEY ARBPL = IT_PLPO-ARBID
                               PERIO = IT_PRODQTY-PERIO
                               BINARY SEARCH.
      CHECK SY-SUBRC = 0 .
      ITAB-KOSTL = TMPT-KOSTL.

      ITAB-PP_MH = IT_PLPO-VGW03.
      ITAB-$PP_MH = IT_PLPO-VGW03. " not collect the unit value

      CHECK NOT ITAB-PP_MH IS INITIAL.
      COLLECT ITAB. CLEAR ITAB.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " get_routing
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROUTING_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_ROUTING_INFO.
  DATA $MATNR TYPE MATNR.
  __PROCESS 'Get Routing Info...' '50'.

  __CLS : IT_MIP,IT_MI,IT_MIP,IT_PLPO_TEMP,IT_PLPO, IT_CRHD.

  CLEAR G_STARTDT.
  CONCATENATE P_ACTYR '1215' INTO G_STARTDT. "Fix me
  CHECK NOT IT_PRODQTY[] IS INITIAL.

  DATA IT_FSC_MI LIKE ZSFSC_MI OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_MARA .
    IF  IT_MARA-MTART = 'FERT' .
      IT_FSC_MI-FSC = IT_MARA-MATNR.
      COLLECT IT_FSC_MI.
    ENDIF.
  ENDLOOP.

  SORT IT_FSC_MI BY FSC.
  DELETE ADJACENT DUPLICATES FROM IT_FSC_MI COMPARING FSC.

  CALL FUNCTION 'Z_CO_GET_MI_CODE_BATCH'
       TABLES
            IT_FSC_MI = IT_FSC_MI.

  LOOP AT IT_MARA .
    IF  IT_MARA-MTART = 'FERT' .

*      IT_MI-MATNR = IT_MARA-MATNR.
*      IT_MI-PLNNR = IT_MARA-MATNR+6(7) .
*      APPEND IT_MI. CLEAR IT_MI.
*
*      PERFORM GET_MI_CODE_SINGLE USING IT_MARA-MATNR
*                              CHANGING $MATNR.

      READ TABLE IT_FSC_MI WITH KEY IT_MARA-MATNR
                                    BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        $MATNR = IT_FSC_MI-MI.

        IT_MI-MATNR = IT_MARA-MATNR.
        IT_MI-PLNNR = $MATNR .
        APPEND IT_MI. CLEAR IT_MI.

      ELSE.
        CLEAR $MATNR.
      ENDIF.

      READ TABLE IT_PRODQTY WITH KEY MATNR = IT_MARA-MATNR
                                      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        IT_PRODQTY-$MATNR = $MATNR.
        MODIFY IT_PRODQTY INDEX SY-TABIX TRANSPORTING $MATNR.
      ENDIF.
    ELSE.
      IT_MIP-MATNR = IT_MARA-MATNR.
      APPEND IT_MIP. CLEAR IT_MIP.
    ENDIF.
  ENDLOOP.

* 1) MI Rounting (FERT)
  IF NOT IT_MI[]  IS INITIAL.
    PERFORM MAKE_MI_ROUTING.
  ENDIF.

* 2) MIP Rounting (HALB)
  IF NOT IT_MIP[]  IS INITIAL.
    PERFORM SELECT_MIP_MATNR.
* 2-1) MIP :Rate routing
    IF NOT IT_RATE[] IS INITIAL.
      PERFORM MAKE_MIP_ROUTING TABLES IT_RATE
                               USING  'R'.
    ENDIF.
* 2-2) MIP:Product routing
    IF NOT IT_PRODUCT[] IS INITIAL.
      PERFORM MAKE_MIP_ROUTING TABLES IT_PRODUCT
                               USING  'N'.
    ENDIF.
  ENDIF.

* For get work center mapping
  SELECT OBJID ARBPL INTO CORRESPONDING FIELDS OF TABLE IT_CRHD
     FROM CRHD
     FOR ALL ENTRIES IN IT_PLPO
     WHERE OBJTY = 'A'
       AND OBJID = IT_PLPO-ARBID.

  SORT IT_CRHD BY OBJID.

ENDFORM.                    " SELECT_ROUTING_INFO
*&---------------------------------------------------------------------*
*&      Form  MAKE_MI_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_MI_ROUTING.

* MI Rounting (FERT)
  SELECT
        C~PLNTY C~PLNNR C~PLNKN
        C~ZAEHL C~DATUV C~ARBID
        C~AENNR C~VORNR C~WERKS
        C~LAR01 C~VGE01 C~VGW01
        C~LAR02 C~VGE02 C~VGW02
        C~LAR03 C~VGE03 C~VGW03
        C~ZGR03
  INTO CORRESPONDING FIELDS OF TABLE IT_PLPO_TEMP
     FROM PLKO AS A
    INNER JOIN PLAS AS B
       ON B~PLNTY = A~PLNTY
      AND B~PLNNR = A~PLNNR
      AND B~PLNAL = A~PLNAL
    INNER JOIN PLPO AS C
       ON C~PLNTY = B~PLNTY
      AND C~PLNNR = B~PLNNR
      AND C~PLNKN = B~PLNKN
      FOR ALL ENTRIES IN IT_MI
     WHERE A~PLNTY = 'M'
       AND A~PLNNR = IT_MI-PLNNR
       AND A~VERWE = '1'             "Usage
       AND A~STATU IN ('3', '4')     "Status
       AND A~DATUV <= G_STARTDT      "Valid from
       AND A~DELKZ = ''              "Delete indicator
       AND B~LOEKZ = ''              "Delete indicator
       AND C~LOEKZ = '' .            "Delete indicator

  SORT IT_PLPO_TEMP BY PLNNR ARBID ASCENDING
                       DATUV       DESCENDING.

* delete old data; change number
  DELETE ADJACENT DUPLICATES FROM IT_PLPO_TEMP
      COMPARING PLNNR ARBID.

  DATA $IX LIKE SY-TABIX.

  LOOP AT IT_MI.
    READ TABLE IT_PLPO_TEMP WITH KEY PLNNR = IT_MI-PLNNR
                                     BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    $IX = SY-TABIX.

*    LOOP AT IT_PLPO_TEMP WHERE PLNNR = IT_MI-PLNNR.
    LOOP AT IT_PLPO_TEMP FROM $IX.
      IF IT_PLPO_TEMP-PLNNR NE IT_MI-PLNNR.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING IT_PLPO_TEMP TO IT_PLPO.
      IT_PLPO-MATNR = IT_MI-MATNR.
      IT_PLPO-TYPE = 'F'.
      COLLECT IT_PLPO.  CLEAR IT_PLPO.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " MAKE_MI_ROUTING
*&---------------------------------------------------------------------*
*&      Form  SELECT_MIP_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MIP_MATNR.

* MI Rounting (FERT)
  SELECT WERKS MATNR SAUFT
    INTO CORRESPONDING FIELDS OF TABLE IT_MARC
     FROM MARC
     FOR ALL ENTRIES IN IT_MIP
     WHERE MATNR = IT_MIP-MATNR.

  LOOP AT IT_MARC.
    IF IT_MARC-SAUFT = 'X'.
      IT_RATE-MATNR = IT_MARC-MATNR.
      APPEND IT_RATE. CLEAR IT_RATE.
    ELSE.
      IT_PRODUCT-MATNR = IT_MARC-MATNR.
      APPEND IT_PRODUCT. CLEAR IT_PRODUCT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SELECT_MIP_MATNR
*&---------------------------------------------------------------------*
*&      Form  MAKE_MIP_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RATE  text
*      -->P_1324   text
*----------------------------------------------------------------------*
FORM MAKE_MIP_ROUTING TABLES   P_TAB  STRUCTURE IT_RATE
                       USING   P_PLNTY.

  DATA : BEGIN OF IT_PLPO_TEMP2 OCCURS 0,
          WERKS  LIKE PLPO-WERKS,
          MATNR   LIKE MARA-MATNR,
          PLNTY   LIKE PLPO-PLNTY,
          PLNNR   LIKE PLPO-PLNNR,
          PLNKN   LIKE PLPO-PLNKN,
          ARBID   LIKE PLPO-ARBID,
          ZAEHL   LIKE PLPO-ZAEHL,
          DATUV   LIKE PLPO-DATUV,
          VGW01   LIKE PLPO-VGW01,    "Set
          VGW02   LIKE PLPO-VGW02,    "Machine
          VGW03   LIKE PLPO-VGW03,    "Labor
          VGE01   LIKE PLPO-VGE01,    "Set
          VGE02   LIKE PLPO-VGE02,    "Machine
          VGE03   LIKE PLPO-VGE03,    "Labor
        END OF IT_PLPO_TEMP2 .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PLPO_TEMP2
     FROM MAPL AS A
    INNER JOIN PLKO AS B
      ON  A~PLNTY = B~PLNTY
     AND  A~PLNNR = B~PLNNR
     AND  A~PLNAL = B~PLNAL
    INNER JOIN PLAS AS C
       ON B~PLNTY = C~PLNTY
      AND B~PLNNR = C~PLNNR
     AND  B~PLNAL = C~PLNAL
    INNER JOIN PLPO AS D
       ON C~PLNTY = D~PLNTY
      AND C~PLNNR = D~PLNNR
      AND C~PLNKN = D~PLNKN
    FOR ALL ENTRIES IN P_TAB
    WHERE A~PLNTY = P_PLNTY         "R:Rate routing N:Product
      AND A~MATNR = P_TAB-MATNR
      AND A~LOEKZ = ''
      AND B~VERWE = '1'             "Usage
      AND B~DATUV <= G_STARTDT      "Valid from
      AND D~DATUV <= G_STARTDT      "Valid from
      AND B~DELKZ = ''              "Delete indicator
      AND C~LOEKZ = ''              "Delete indicator
      AND D~LOEKZ = ''.

  SORT IT_PLPO_TEMP2 BY PLNNR ARBID ASCENDING
                  DATUV       DESCENDING.

* delete old data; change number
  DELETE ADJACENT DUPLICATES FROM IT_PLPO_TEMP2
      COMPARING PLNNR ARBID.

  LOOP AT IT_PLPO_TEMP2.
    MOVE-CORRESPONDING IT_PLPO_TEMP2 TO IT_PLPO.
    IF P_PLNTY = 'R'.
      IT_PLPO-TYPE = 'R'.
    ELSE.
      IT_PLPO-TYPE = 'N'.
    ENDIF.
    COLLECT IT_PLPO. CLEAR IT_PLPO.
  ENDLOOP.

ENDFORM.                    " make_mip_routing
*&---------------------------------------------------------------------*
*&      Form  GET_HR_MH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_HR_MH.
  CHECK G_ERROR IS INITIAL.
  __PROCESS 'Get HR M/H...' '60'.

  __CLS IT_ACT_HR_MH.

  SELECT POPER ARTNR SHOP KOSTL
         OS_DIR   FINAL_MH
    INTO CORRESPONDING FIELDS OF TABLE IT_ACT_HR_MH
    FROM ZTCO_MHV
   WHERE KOKRS = P_KOKRS
     AND BDATJ = P_ACTYR
     AND POPER IN S_POPER
     AND ARTNR IN S_MATNR
     AND LLV_MATNR EQ SPACE.

ENDFORM.                    " GET_HR_MH
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ROW_ITAB.
  CHECK G_ERROR IS INITIAL.

  __PROCESS 'Refine Data...' '70'.


  __CLS IT_ROW_TAB.

  SORT IT_ACT_HR_MH BY  POPER ARTNR KOSTL.

  LOOP AT ITAB.

    IT_ROW_TAB-PERIO = ITAB-PERIO.

    IT_ROW_TAB-MATNR  = ITAB-MATNR.
    IT_ROW_TAB-KOSTL  = ITAB-KOSTL.
    IT_ROW_TAB-VGE03 =  ITAB-VGE03.
    IT_ROW_TAB-PP_MH = ITAB-$PP_MH.
*    IT_ROW_TAB-$PP_MH = ITAB-$PP_MH. " not collect the unit value

    CLEAR IT_PRODQTY.

    READ TABLE IT_PRODQTY WITH KEY MATNR = ITAB-MATNR
                                     BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    IT_ROW_TAB-PRDQTY = IT_PRODQTY-OUT_MENGE.
    IT_ROW_TAB-MEINH  = IT_PRODQTY-MEINH.

    IT_ROW_TAB-TOT_PP_MH = IT_ROW_TAB-PP_MH * IT_ROW_TAB-PRDQTY.

    READ TABLE IT_ACT_HR_MH WITH KEY POPER = IT_ROW_TAB-PERIO
                                     ARTNR = ITAB-MATNR
                                     KOSTL = ITAB-KOSTL
                                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-HR_MH = IT_ACT_HR_MH-FINAL_MH.
      IT_ROW_TAB-TP_MH = IT_ACT_HR_MH-OS_DIR.
*      IT_ROW_TAB-SHR_MH = IT_ACT_HR_MH-FINAL_MH. " avoid collect
*      IT_ROW_TAB-STP_MH = IT_ACT_HR_MH-OS_DIR.  " avoid collect
      IT_ROW_TAB-$HR_MH = IT_ACT_HR_MH-FINAL_MH * IT_ROW_TAB-PRDQTY.
      IT_ROW_TAB-$TP_MH = IT_ACT_HR_MH-OS_DIR * IT_ROW_TAB-PRDQTY.
    ENDIF.

    IT_ROW_TAB-AT_MH = IT_ROW_TAB-HR_MH + IT_ROW_TAB-TP_MH.
    IT_ROW_TAB-TOT_AT_MH = IT_ROW_TAB-AT_MH * IT_ROW_TAB-PRDQTY.
    IF IT_PRODQTY-$MATNR NE SPACE.
      IT_ROW_TAB-MATNR = IT_PRODQTY-$MATNR.
    ENDIF.
    APPEND IT_ROW_TAB.
    CLEAR  IT_ROW_TAB.
  ENDLOOP.

ENDFORM.                    " GET_ROW_ITAB

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR '100'.
   *ZTCOU129-ACTYR = P_ACTYR.
   *ZTCOU129-PLNYR = P_PLNYR.

*   Exclude toolbar
  PERFORM EXCLUDE_FUNCTIONS.
  __CLS FTAB.
  IF SSCRFIELDS-UCOMM NE 'VSLT'.
    SET PF-STATUS '100' EXCLUDING FTAB.
  ELSE.
    FTAB-FCODE = 'CALC'.
    APPEND FTAB. CLEAR FTAB.
    FTAB-FCODE = 'UNDO'.
    APPEND FTAB. CLEAR FTAB.
    FTAB-FCODE = 'SAVE'.
    APPEND FTAB. CLEAR FTAB.
    SET PF-STATUS '100' EXCLUDING FTAB.
  ENDIF.

  PERFORM INFO_TEXT_SET USING TRUE.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  FLAG_DATA_CHANGED = TRUE.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

  DATA  $KOSTL TYPE KOSTL.
  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    IF SY-SUBRC = 0.
      $KOSTL = GT_OUT-KOSTL.
      CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
                EXPORTING I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                          I_VALUE     = LS_MOD_CELLS-VALUE.

      GT_OUT-NOR_R = LS_MOD_CELLS-VALUE.
      IF GT_OUT-NOR_R > 0.
        GT_OUT-ICON = ICON_PARAMETER.
        GT_OUT-FLAG = TRUE.
      ELSE.
        CLEAR : GT_OUT-ICON,GT_OUT-FLAG.
      ENDIF.

      MODIFY GT_OUT TRANSPORTING NOR_R ICON FLAG WHERE KOSTL EQ $KOSTL.

      IF GT_OUT-NOR_R > 0.
        GT_OUT-ICON = ICON_PARAMETER_IMPORT.
        MODIFY GT_OUT INDEX LS_MOD_CELLS-ROW_ID TRANSPORTING ICON.
      ENDIF.

    ENDIF.
  ENDLOOP.

  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_100 OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    PERFORM CREATE_AND_INIT_ALV.
*   Display alv grid
    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING IS_LAYOUT            = GS_LAYO
                   IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
                   I_SAVE               = GC_VAR_SAVE
                   IS_VARIANT           = GS_VARIANT
         CHANGING  IT_OUTTAB            = GT_OUT[]
                   IT_FIELDCATALOG      = GT_FCAT[]
                   IT_SORT              = GT_SORT[].
  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
  __FOCUS G_GRID.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_ALV.
*   Create object
  PERFORM CREATE_OBJECT.

*  Create Object to verify input values.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER : G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID.

*   Create field category
  PERFORM CREATE_FIELD_CATEGORY USING FALSE.

  CALL METHOD G_GRID->REGISTER_EDIT_EVENT
       EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD G_GRID->SET_READY_FOR_INPUT
     EXPORTING
            I_READY_FOR_INPUT = 0.

  PERFORM SORT_BUILD USING GT_SORT[].

*   Setting for layout
  PERFORM SET_LVC_LAYOUT.

*   Set colors
  PERFORM SET_COLOR.

*   Define cell attribute
  PERFORM BUILD_CELL_ATTR.

*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM CREATE_FIELD_CATEGORY USING MODE_EDIT.
  DATA: L_POS       TYPE I.
  DEFINE __CATALOG.
    L_POS = L_POS + 1.
    CLEAR GS_FCAT.
    GS_FCAT-COL_POS       = L_POS.
    GS_FCAT-KEY           = &1.
    GS_FCAT-FIELDNAME     = &2.
    GS_FCAT-COLTEXT       = &3.     " Column heading
    GS_FCAT-OUTPUTLEN     = &4.     " Column width
    GS_FCAT-DATATYPE      = &5.     " Data type
    GS_FCAT-EMPHASIZE     = &6.
    APPEND GS_FCAT TO GT_FCAT.
  END-OF-DEFINITION.

  __CATALOG :

    'X'  'KOSTL'    'C.C'               10  'CHAR' '',
    'X'  'PERIO'    'Period'             8  'CHAR' ''.

  __CATALOG :
    'X'  'MATNR'    'MI/MIP'            18  'CHAR' ''.

  __CATALOG :
    ' '  'PRDQTY'    'Prd.Qty'          15  'QUAN' '',
    ' '  'PP_MH'     'M/H'              15  'QUAN' '',
    ' '  'TOT_PP_MH' 'PP M/H'           15  'QUAN' '',
    ' '  'HR_MH'     'F.HR M/H'         15  'QUAN' '',
    ' '  'TP_MH'     'TP M/H'           15  'QUAN' '',
    ' '  'TOT_AT_MH' 'HR M/H'           15  'QUAN' '',
    ' '  'CAL_R'     'Rate(%)'           3  'NUMC' '',
    ' '  'NOR_R'     'Nor.Rate(%)'       3  'NUMC' '',
    ' '  'ICON'      'IC'                3  'ICON' '',
    ' '  'ICONR'     'R'                 3  'ICON' ''.

  "   ' '  'FLAG'    ''                  1  'CHAR' ''.

  LOOP AT GT_FCAT INTO GS_FCAT.
    CASE GS_FCAT-FIELDNAME.
      WHEN 'PRDQTY'.
        GS_FCAT-JUST = 'R'.
        GS_FCAT-QFIELDNAME = 'MEINH'.
      WHEN 'PP_MH' OR 'TOT_PP_MH' OR 'HR_MH'
           OR 'TP_MH' OR 'AT_MH' OR 'TOT_AT_MH'.
        GS_FCAT-JUST = 'R'.
        GS_FCAT-QFIELDNAME = 'VGE03'.
      WHEN 'NOR_R' OR 'CAL_R'.
        GS_FCAT-JUST = 'R'.
    ENDCASE.
    GS_FCAT-REF_TABLE = 'ZSCOU129'.
    GS_FCAT-REF_FIELD = GS_FIELDCAT-FIELDNAME.
    MODIFY GT_FCAT FROM GS_FCAT.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS.
  PERFORM APPEND_EXCLUDE_FUNCTIONS
           TABLES GT_EXCLUDE[]
           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
*                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.

ENDFORM.                    " EXCLUDE_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LVC_LAYOUT.

  CLEAR GS_LAYO.

  GS_LAYO-EDIT       = 'X'.
  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
  GS_LAYO-CWIDTH_OPT = 'X'.
  GS_LAYO-CTAB_FNAME = 'TABCOLOR'.
  GS_LAYO-STYLEFNAME = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_COLOR.
  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].

  DEFINE __COLOR.
    GS_SPECIALCOL-FIELDNAME = &1 .
    GS_SPECIALCOL-COLOR-COL = &2 .
    GS_SPECIALCOL-COLOR-INT = &3 .
    APPEND GS_SPECIALCOL TO GT_SPECIALCOL .
  END-OF-DEFINITION.

  __COLOR :
    'PERIO'        '1' 0.

  __COLOR :
    'MATNR'        '1' 0.

  __COLOR :
    'KOSTL'        '1' 0,
    'PRDQTY'       '2' 0,
    'PP_MH'        '1' 0,
    'TOT_PP_MH'    '3' 0,
    'HR_MH'        '1' 0,
    'TP_MH'        '1' 0,
    'TOT_AT_MH'    '3' 0,
    'CAL_R'        '3' 1,
    'NOR_R'        '7' 0.

  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING FT_SORT TYPE LVC_T_SORT.
  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-SUBTOT    = &5.
    GS_SORT-COMP      = &6.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
             'KOSTL'    '1' 'X' '' 'X' '',
             'PERIO'    '2' 'X' '' 'X' ''.
  SORT_TAB :
             'MATNR'    '3' 'X' '' 'X' ''.
ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CLEAR : G_ERROR.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      PERFORM FREE_CONTAINER.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM REALLY?.
      CHECK G_ERROR NE TRUE.
      PERFORM SAVE_TABLE.

    WHEN 'CALC'.
      PERFORM : RECALC,
                REFRESH_ALV.
      __FOCUS G_GRID.
    WHEN 'UNDO'.
      PERFORM : UNDO,
                REFRESH_ALV.
      __FOCUS G_GRID.

    WHEN 'SWITCH'.
      PERFORM SWITCH_EDIT_MODE.
      __FOCUS G_GRID.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_ALV.
  PERFORM SET_COLOR.
  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  SAVE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TABLE.

  DATA I_ZTCOU129 LIKE ZTCOU129 OCCURS 0 WITH HEADER LINE.
  DATA : $FLAG(1),$FLAG_M(1).

  DATA $SUBRC(1).
  DATA $LINES LIKE SY-TABIX.
  DATA $LINES_Z LIKE SY-TABIX.

  __CLS IT_SEL.

  LOOP AT GT_OUT WHERE FLAG EQ TRUE.
    MOVE-CORRESPONDING GT_OUT TO IT_SEL.
    IT_SEL-KOKRS = P_KOKRS.
    IT_SEL-ACTYR = P_ACTYR.
    IT_SEL-PLNYR = P_PLNYR.
    IT_SEL-LINE_NO = SY-TABIX.
    APPEND IT_SEL. CLEAR IT_SEL.
  ENDLOOP.

  READ TABLE IT_SEL INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE I000 WITH 'No simulated data was found!' ''.
    EXIT.
  ENDIF.

  READ TABLE IT_SEL WITH KEY RELEASED = TRUE.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH 'You can not apply with the released line(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

  DELETE IT_SEL WHERE RELEASED = TRUE.

  SORT IT_SEL BY KOKRS ACTYR PLNYR PERIO KOSTL MATNR.

  DATA $IT_SEL LIKE IT_SEL OCCURS 0 WITH HEADER LINE.

  DATA $PERIO LIKE IT_SEL-PERIO.
  DATA $KOSTL LIKE IT_SEL-KOSTL.

  $IT_SEL[] = IT_SEL[].

  LOOP AT IT_SEL.

    AT END OF KOSTL.
      $PERIO = IT_SEL-PERIO.
      $FLAG = TRUE.
    ENDAT.

    CHECK $FLAG EQ TRUE.
    CLEAR $FLAG.
    CHECK IT_SEL-KOSTL NE $KOSTL.

    DELETE $IT_SEL WHERE PERIO NE $PERIO
                     AND KOSTL EQ IT_SEL-KOSTL.

    $KOSTL = IT_SEL-KOSTL.
  ENDLOOP.

  CLEAR $FLAG.
  LOOP AT $IT_SEL.

    AT NEW KOSTL.
      CLEAR : *ZTCOU129.
    ENDAT.

    AT NEW MATNR.
      CLEAR : *ZTCOU129-PRDQTY.
    ENDAT.

    ADD :	$IT_SEL-$HR_MH     TO *ZTCOU129-HR_MH,
        	$IT_SEL-$TP_MH     TO *ZTCOU129-TP_MH,
        	$IT_SEL-TOT_PP_MH  TO *ZTCOU129-TOT_PP_MH,
        	$IT_SEL-TOT_AT_MH  TO *ZTCOU129-TOT_AT_MH.

    AT END OF KOSTL.
      $FLAG = TRUE.
    ENDAT.

    AT END OF MATNR.
      $FLAG_M = TRUE.
    ENDAT.

    IF $FLAG_M EQ TRUE.
      ADD  $IT_SEL-PRDQTY TO *ZTCOU129-PRDQTY.
      CLEAR $FLAG_M.
    ENDIF.

    CHECK $FLAG EQ TRUE. CLEAR $FLAG.

     *ZTCOU129-KOKRS = $IT_SEL-KOKRS.
     *ZTCOU129-ACTYR = $IT_SEL-ACTYR.
     *ZTCOU129-PLNYR = $IT_SEL-PLNYR.
     *ZTCOU129-PERIO = $IT_SEL-PERIO.
     *ZTCOU129-KOSTL = $IT_SEL-KOSTL.
     *ZTCOU129-CAL_R = $IT_SEL-CAL_R.
     *ZTCOU129-NOR_R = $IT_SEL-NOR_R.
     *ZTCOU129-MEINH = $IT_SEL-MEINH.
     *ZTCOU129-VGE03 = $IT_SEL-VGE03.
     *ZTCOU129-AEDAT = SY-DATUM.
     *ZTCOU129-AENAM = SY-UNAME.

    I_ZTCOU129 = *ZTCOU129.
    APPEND I_ZTCOU129.

  ENDLOOP.

*  PERFORM UPDATE_TC31A TABLES I_ZTCOU129
*                     CHANGING $SUBRC
*                              $LINES.
*
*  IF $SUBRC EQ 'E'.
*    ROLLBACK WORK.
*    MESSAGE S000 WITH ' Error was occured when data saving.'.
*    EXIT.
*  ELSE.
*    COMMIT WORK.
*  ENDIF.

  __CLS I_ZTCOU129.
  LOOP AT IT_SEL.
    MOVE-CORRESPONDING IT_SEL TO *ZTCOU129.
     *ZTCOU129-AEDAT = SY-DATUM.
     *ZTCOU129-AENAM = SY-UNAME.
    I_ZTCOU129 = *ZTCOU129.
    APPEND I_ZTCOU129.
  ENDLOOP.

  DESCRIBE TABLE I_ZTCOU129 LINES $LINES_Z.

  IF $LINES_Z > 0.

    DELETE FROM ZTCOU129
    WHERE KOKRS EQ P_KOKRS
    AND   PLNYR EQ P_PLNYR
    AND   MATNR IN S_MATNR.

    MODIFY ZTCOU129 FROM TABLE I_ZTCOU129.
    IF SY-SUBRC EQ 0.
      CASE $LINES.
        WHEN 1.
          MESSAGE S000 WITH $LINES
*          ' line has been saved into TC31A successfully. z:'
          ' line has been saved into z-table successfully.'
          $LINES_Z.
        WHEN OTHERS.
          MESSAGE S000 WITH $LINES
            ' lines have been saved into z-table successfully.'
            $LINES_Z.
      ENDCASE.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE S000 WITH ' Error was occured when data saving.'.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAVE_TABLE
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REALLY?.
  DATA $EXISTS(1).
  DATA L_ANSWER(1).

  PERFORM POP_UP USING
      'The existing data will be changed!'
      'Do you really want to apply?' ' '
                 CHANGING L_ANSWER.

  IF L_ANSWER NE 'J'.
    G_ERROR = TRUE.
    MESSAGE S000 WITH 'Processing was canceled by user.'.
  ENDIF.

ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM GET_SELECTED_DATA TABLES PT_ROWS STRUCTURE LVC_S_ROW
                             PT_ROW_NO STRUCTURE LVC_S_ROID.

  __CLS IT_SEL .

  CLEAR GT_OUT-CHK.
  MODIFY GT_OUT TRANSPORTING CHK WHERE CHK = 'X'.

* Selected Row by row selection
  LOOP AT PT_ROWS WHERE ROWTYPE IS INITIAL.
    READ TABLE GT_OUT INDEX PT_ROWS-INDEX.
    GT_OUT-CHK = TRUE .
    MODIFY GT_OUT INDEX PT_ROWS-INDEX .
  ENDLOOP.

  PERFORM SELECT_ROW_BY_SUBTOTAL TABLES PT_ROWS .

  READ TABLE GT_OUT WITH KEY RELEASED = TRUE
                             CHK = TRUE.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH 'You can not apply with the released line(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

  LOOP AT GT_OUT WHERE CHK EQ TRUE.
    CHECK GT_OUT-RELEASED NE TRUE.
    MOVE-CORRESPONDING GT_OUT TO IT_SEL.
    MOVE SY-TABIX TO IT_SEL-LINE_NO.
    CLEAR : IT_SEL-PERIO, IT_SEL-MATNR.
    APPEND IT_SEL. CLEAR IT_SEL.
  ENDLOOP.

ENDFORM.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_ROWS  text
*----------------------------------------------------------------------*
FORM SELECT_ROW_BY_SUBTOTAL TABLES P_PT_ROWS
                                   STRUCTURE LVC_S_ROW.

  DATA: TMPGRP TYPE LVC_T_GRPL, " For subtotal Selection .
       $TMPGRP TYPE LVC_S_GRPL.

  CALL METHOD G_GRID->GET_SUBTOTALS
          IMPORTING
            ET_GROUPLEVELS = TMPGRP.

* Selected Row by row selection ( Sub total )
  LOOP AT P_PT_ROWS WHERE NOT ROWTYPE IS INITIAL.
    READ TABLE TMPGRP INDEX P_PT_ROWS-INDEX INTO $TMPGRP.
    CHECK SY-SUBRC EQ 0 .

    LOOP AT GT_OUT FROM $TMPGRP-INDEX_FROM
                     TO $TMPGRP-INDEX_TO.
      GT_OUT-CHK = TRUE .
      MODIFY GT_OUT.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  GET_MI_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PRODQTY_TEMP_MATNR  text
*      <--P_IT_PRODQTY_MATNR  text
*----------------------------------------------------------------------*
FORM GET_MI_CODE_SINGLE  USING P_FSC
                      CHANGING P_MI.

  CALL FUNCTION 'Z_CO_GET_MI_CODE_SINGLE'
       EXPORTING
            P_FSC     = P_FSC
       IMPORTING
            P_MI      = P_MI
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
  IF SY-SUBRC <> 0.
  ENDIF.

ENDFORM.                    " GET_MI_CODE
*&---------------------------------------------------------------------*
*&      Form  REFINE_ROW_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFINE_ROW_ITAB.
  __PROCESS 'Refine Data...' '80'.

  DATA $IT_ROW_TAB LIKE IT_ROW_TAB OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_ROW_TAB.
    $IT_ROW_TAB = IT_ROW_TAB.
    COLLECT  $IT_ROW_TAB.
    CLEAR  $IT_ROW_TAB.
  ENDLOOP.
  __CLS IT_ROW_TAB.
  IT_ROW_TAB[] = $IT_ROW_TAB[].

  DATA T_MH TYPE ZHR_MH.
  DATA H_RATE  TYPE P.
  DATA T_RATE  TYPE P.

  LOOP AT IT_ROW_TAB.

    IT_ROW_TAB-PP_MH = IT_ROW_TAB-TOT_PP_MH / IT_ROW_TAB-PRDQTY.


    IT_ROW_TAB-HR_MH = IT_ROW_TAB-$HR_MH / IT_ROW_TAB-PRDQTY.
    IT_ROW_TAB-TP_MH = IT_ROW_TAB-$TP_MH / IT_ROW_TAB-PRDQTY.

*    T_MH = IT_ROW_TAB-$HR_MH + IT_ROW_TAB-$TP_MH.
*
*    H_RATE = IT_ROW_TAB-$HR_MH / T_MH.
*    T_RATE = IT_ROW_TAB-$TP_MH / T_MH.
*
*    IT_ROW_TAB-HR_MH = IT_ROW_TAB-TOT_AT_MH / IT_ROW_TAB-PRDQTY
*                      * H_RATE.
*    IT_ROW_TAB-TP_MH = IT_ROW_TAB-TOT_AT_MH / IT_ROW_TAB-PRDQTY
*                      * T_RATE.

    IF IT_ROW_TAB-AT_MH <> 0.
      IT_ROW_TAB-CAL_R = IT_ROW_TAB-TOT_PP_MH
                        / IT_ROW_TAB-TOT_AT_MH * 100.
    ENDIF.
    MODIFY IT_ROW_TAB INDEX SY-TABIX TRANSPORTING CAL_R PP_MH
                                                  HR_MH TP_MH.
  ENDLOOP.

  SORT IT_ROW_TAB BY PERIO MATNR KOSTL.

ENDFORM.                    " REFINE_ROW_ITAB
*&---------------------------------------------------------------------*
*&      Form  RECALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RECALC.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD G_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    MESSAGE E000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000 WITH 'Please select a data.'.
    EXIT.
  ENDIF.

  PERFORM GET_SELECTED_DATA TABLES LT_ROWS
                                   LT_ROW_NO .

  READ TABLE IT_SEL INDEX 1.
  IF SY-SUBRC EQ 0.
    PERFORM GET_NORMAL_RATE.
  ENDIF.

ENDFORM.                    " RECALC
*&---------------------------------------------------------------------*
*&      Form  get_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PLANT.

  TYPES: BEGIN OF TY_BWKEY,
           BWKEY TYPE BWKEY,
         END OF TY_BWKEY.

  DATA   LT_BWKEY TYPE TABLE OF TY_BWKEY WITH HEADER LINE.

  __CLS : LT_BWKEY, R_BWKEY.

  SELECT BWKEY INTO TABLE LT_BWKEY
    FROM T001K
   WHERE BUKRS = P_KOKRS.

  R_BWKEY-SIGN   = 'I'.
  R_BWKEY-OPTION = 'EQ'.

  LOOP AT LT_BWKEY.
    R_BWKEY-LOW = LT_BWKEY-BWKEY.
    APPEND R_BWKEY.
  ENDLOOP.

  CLEAR R_BWKEY.
*

ENDFORM.                    " get_plant
*&---------------------------------------------------------------------*
*&      Form  GET_NORMAL_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NORMAL_RATE.
  DATA : $TOT_PP_MH TYPE P DECIMALS 3,
         $TOT_AT_MH TYPE P DECIMALS 3,
         $FLAG(1).

  SORT IT_SEL BY KOSTL.
  LOOP AT IT_SEL.

    AT NEW KOSTL.
      CLEAR : $TOT_PP_MH, $TOT_AT_MH.
    ENDAT.

    ADD : IT_SEL-TOT_PP_MH  TO $TOT_PP_MH,
          IT_SEL-TOT_AT_MH  TO $TOT_AT_MH.

    AT END OF KOSTL.
      $FLAG = TRUE.
    ENDAT.

    CHECK $FLAG EQ TRUE.
    CLEAR $FLAG.
    IF $TOT_AT_MH <> 0.
      GT_OUT-NOR_R = $TOT_PP_MH / $TOT_AT_MH * 100.
      GT_OUT-ICON = ICON_PARAMETER.
      GT_OUT-FLAG = TRUE.
      MODIFY GT_OUT TRANSPORTING NOR_R ICON FLAG
                              WHERE KOSTL = IT_SEL-KOSTL.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_SEL.
    GT_OUT-ICON = ICON_PARAMETER_IMPORT.
    GT_OUT-FLAG = TRUE.
    MODIFY GT_OUT INDEX IT_SEL-LINE_NO TRANSPORTING ICON FLAG.
  ENDLOOP.

ENDFORM.                    " GET_NORMAL_RATE
*&---------------------------------------------------------------------*
*&      Form  UNDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UNDO.
  DATA $EXISTS(1).
  DATA L_ANSWER(1).

  PERFORM POP_UP USING
      'Data will be refreshed!'
      'Do you really want to refresh the simulated data?' ' '
                 CHANGING L_ANSWER.

  IF L_ANSWER NE 'J'.
    MESSAGE S000 WITH 'Processing was canceled by user.'.
    EXIT.
  ENDIF.

  PERFORM MOVE_OUT.

ENDFORM.                    " UNDO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TC31A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DEL_ROWS  text
*      -->P_I_ZTCOU128  text
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
*FORM UPDATE_TC31A TABLES P_ZTCOU129 STRUCTURE ZTCOU129
*                CHANGING P_$SUBRC P_$LINES.
*
*  DATA : $END_DATE TYPE SYDATUM,
*         $PERIO TYPE POPER,
*         $PERIO_128(6),
*         $ABTEI TYPE ABTEI.
*
*  LOOP AT P_ZTCOU129.
*    CLEAR : $ABTEI, $PERIO, $END_DATE.
*
*    READ TABLE $KOSTL_ABTEI WITH KEY KOSTL = P_ZTCOU129-KOSTL
*                            BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      $ABTEI = $KOSTL_ABTEI-ABTEI.
*    ELSE.
*      MESSAGE S000 WITH
*      ' Please setup the Department for .' P_ZTCOU129-KOSTL.
*      P_$SUBRC = 'E'.
*      EXIT.
*    ENDIF.
*
*    DO 12 TIMES.
*
*      $PERIO = SY-INDEX.
*      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
*           EXPORTING
*                I_GJAHR = P_PLNYR
*                I_PERIV = 'K0'
*                I_POPER = $PERIO
*           IMPORTING
*                E_DATE  = $END_DATE.
*      ADD 1 TO P_$LINES.
*
*      CLEAR *TC31A.
*      SELECT SINGLE * INTO *TC31A
*        FROM TC31A WHERE ZGRAD EQ $ABTEI
*                     AND DATUB EQ $END_DATE.
*      IF SY-SUBRC EQ 0.
*         *TC31A-ZGKAL = P_ZTCOU129-NOR_R.
*         *TC31A-ZGTER = P_ZTCOU129-NOR_R.
*        UPDATE TC31A FROM *TC31A.
*      ELSE.
*         *TC31A-ZGRAD = $ABTEI.
*         *TC31A-DATUB = $END_DATE.
*         *TC31A-ZGKAL = P_ZTCOU129-NOR_R.
*         *TC31A-ZGTER = P_ZTCOU129-NOR_R.
*        INSERT TC31A FROM *TC31A.
*      ENDIF.
*
*      IF SY-SUBRC NE 0.
*        MESSAGE S000 WITH
*        ' Error was occured when input the data for '
*          P_ZTCOU129-KOSTL $END_DATE.
*        P_$SUBRC = 'E'.
*        EXIT.
*      ENDIF.

*** ztcou128 {
**      CONCATENATE P_PLNYR $PERIO+1(2) INTO $PERIO_128.
**      CLEAR *ZTCOU128.
**      SELECT SINGLE * INTO *ZTCOU128
**        FROM ZTCOU128 WHERE KOKRS EQ P_KOKRS
**                        AND BDATJ EQ P_PLNYR
**                        AND KOSTL EQ P_ZTCOU129-KOSTL
**                        AND ABTEI EQ $ABTEI
**                        AND PERIO EQ $PERIO_128
**                        AND DATUB EQ $END_DATE.
**      IF SY-SUBRC EQ 0.
**
**        IF *ZTCOU128-RELEASED EQ TRUE.
**          MESSAGE E000 WITH
**          ' The rate was just released at another session!'
**          ' Please leave this program and start again!'.
**          P_$SUBRC = 'E'.
**          EXIT.
**        ENDIF.
**
***         *ZTCOU128-NOR_R = P_ZTCOU129-NOR_R.
**        UPDATE ZTCOU128 FROM *ZTCOU128.
**      ENDIF.
*** }

*    ENDDO.
*
*    IF P_$SUBRC EQ 'E'.
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.                    " UPDATE_TC31A
*&---------------------------------------------------------------------*
*&      Form  chk_released
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_RELEASED.

  DATA : $ABTEI LIKE CSKS-ABTEI,
         $PERIO(6).

  READ TABLE $KOSTL_ABTEI WITH KEY KOSTL = GT_OUT-KOSTL
                                   BINARY SEARCH.

  IF SY-SUBRC EQ 0.
    $ABTEI = $KOSTL_ABTEI-ABTEI.
  ENDIF.

  CONCATENATE P_PLNYR GT_OUT-PERIO+1(2) INTO $PERIO.
  CLEAR *ZTCOU128.
  SELECT SINGLE * INTO *ZTCOU128
    FROM ZTCOU128 WHERE KOKRS EQ P_KOKRS
                    AND BDATJ EQ P_PLNYR
                    AND KOSTL EQ GT_OUT-KOSTL
                    AND ABTEI EQ $ABTEI.
  IF SY-SUBRC EQ 0.
    GT_OUT-RELEASED = *ZTCOU128-RELEASED.
  ENDIF.

  IF GT_OUT-RELEASED EQ TRUE.
    GT_OUT-ICONR = ICON_RELEASE.
  ENDIF.

ENDFORM.                    " chk_released
*&---------------------------------------------------------------------*
*&      Form  GET_ABTEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ABTEI.

  __CLS $KOSTL_ABTEI.

* get department {
  SELECT KOSTL ABTEI INTO TABLE $KOSTL_ABTEI
  FROM CSKS
   FOR ALL ENTRIES IN $KOSTL
   WHERE KOKRS EQ P_KOKRS
    AND  KOSTL EQ $KOSTL-KOSTL.

  SORT $KOSTL_ABTEI BY KOSTL.
* }


ENDFORM.                    " GET_ABTEI
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FREE_CONTAINER.

  IF NOT G_EVENT_RECEIVER IS INITIAL.
    FREE G_EVENT_RECEIVER.
  ENDIF.

  IF NOT G_GRID IS INITIAL.
    CALL METHOD G_GRID->FREE.
  ENDIF.

  IF NOT G_CUSTOM_CONTAINER IS INITIAL.
    CALL METHOD G_CUSTOM_CONTAINER->FREE.
  ENDIF.

  FREE : G_GRID,G_CUSTOM_CONTAINER.

  CLEAR :  GS_LAYO,GT_EXCLUDE,GT_OUT[],GT_FCAT[],GT_SORT[].

ENDFORM.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  CHK_NOR_R
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_NOR_R.

  SELECT SINGLE * INTO *ZTCOU129
    FROM ZTCOU129 WHERE KOKRS EQ P_KOKRS
                    AND PLNYR EQ P_PLNYR
                    AND KOSTL EQ GT_OUT-KOSTL.
  IF SY-SUBRC EQ 0.
    GT_OUT-NOR_R = *ZTCOU129-NOR_R.
  ENDIF.

ENDFORM.                    " CHK_NOR_R
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SWITCH_EDIT_MODE.
  DATA ANSWER.
  IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 1.
    SET PF-STATUS '100'.
    PERFORM INFO_TEXT_SET USING TRUE.
  ELSE.
*    IF FLAG_DATA_CHANGED EQ TRUE.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*           EXPORTING
*                TEXTLINE1     = 'Data has not been saved yet.'
*                TEXTLINE2     = 'Do you want to continue anyway? '
*                TITEL         = 'Confirmation'
*                DEFAULTOPTION = 'N'
*           IMPORTING
*                ANSWER        = ANSWER.
*      CHECK ANSWER EQ 'J'.
*    ENDIF.
*    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM INFO_TEXT_SET USING FALSE.
  ENDIF.

  PERFORM BUILD_CELL_ATTR.
ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_ATTR.

  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR LT_CELLTAB.
  REFRESH LT_CELLTAB.

  CLEAR GS_FCAT.

  LOOP AT GT_FCAT INTO GS_FCAT.
    LS_CELLTAB-FIELDNAME = GS_FCAT-FIELDNAME.
    IF LS_CELLTAB-FIELDNAME = 'NOR_R'.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.
    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  CLEAR GT_OUT-CELLTAB.
  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE CELLTAB IS INITIAL.
  PERFORM BUILD_CELL_ATTR1_LOCK.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_ATTR1_LOCK.

  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR LT_CELLTAB.
  REFRESH LT_CELLTAB.

  __CLS GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE RELEASED EQ TRUE.

  CLEAR GS_FCAT.

  LOOP AT GT_FCAT INTO GS_FCAT.
    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE RELEASED EQ TRUE.

ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM INFO_TEXT_SET USING P_TRUE.

  IF P_TRUE EQ TRUE.
    INFO = TEXT-015.
  ELSE.
    INFO = TEXT-015.
  ENDIF.

ENDFORM.                    " info_text_set
