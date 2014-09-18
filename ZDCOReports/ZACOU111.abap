*----------------------------------------------------------------------
* Program ID        : ZACOU111
* Title             : [CO] Collect Price Variance
* Created on        : 09/05/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Adjust Price History
*----------------------------------------------------------------------
REPORT  ZACOU111_1 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU111_TOP.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.
PARAMETERS:     P_BUKRS  LIKE T001T-BUKRS DEFAULT 'H201' OBLIGATORY.
PARAMETERS:     P_MONTH  LIKE S001-SPMON OBLIGATORY.
SELECT-OPTIONS: S_MTART  FOR MARA-MTART,
                S_MATNR  FOR MARA-MATNR,
                S_BKLAS  FOR CKMLMV011-BKLAS.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_GR AS CHECKBOX DEFAULT 'X',
            P_IV AS CHECKBOX,
            P_RV AS CHECKBOX,
            P_IM AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.
PARAMETERS: P_TEST AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-008.
SELECT-OPTIONS S_HKONT FOR BSIS-HKONT.
SELECTION-SCREEN END OF BLOCK B3.

SELECT-OPTIONS: S_BELNR  FOR MLHD-BELNR,
                S_KJAHR  FOR MLHD-KJAHR MEMORY ID MLJ,
                S_AWREF  FOR MLHD-AWREF,
                S_AWORG  FOR MLHD-AWORG.
PARAMETERS: P_BREAK AS CHECKBOX.
*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM RANGES.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Imported from parallel program
  IMPORT: IT_MAT = GT_MAT FROM MEMORY ID 'ZMAT',
          P_BUKRS FROM MEMORY ID 'ZBUKRS',
          P_MONTH FROM MEMORY ID 'ZMONTH',
          P_GR    FROM MEMORY ID 'ZGT',
          P_IV    FROM MEMORY ID 'ZIV',
          P_RV    FROM MEMORY ID 'ZRV',
          P_IM    FROM MEMORY ID 'ZIM'.

* If GT_MAT is exist => It is imported from parallel program
  PERFORM GET_DATE.

  IF GT_MAT[] IS INITIAL.
    GV_PAR = 'X'.
    PERFORM GET_MATERIAL.
    PERFORM GET_DATA.
  ELSE.
    PERFORM GET_DATA.
  ENDIF.

  IF GT_OUT[] IS INITIAL.
    MESSAGE S000 WITH 'No data found.'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF P_TEST = 'X'.
    PERFORM DISP_RESULT.
  ELSE.
    PERFORM SAVE_DATA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SET_INITIALIZATION
*&---------------------------------------------------------------------*
FORM SET_INITIALIZATION.
  IF SY-DATUM+4(2) EQ '01'.
    P_MONTH(4)   = SY-DATUM(4) - 1.
    P_MONTH+4(2) = '12'.
  ELSE.
    P_MONTH = SY-DATUM(6) - 1.
  ENDIF.

ENDFORM.                    " SET_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
* Create Internal Table GT_RSN for Reason Codes
  CLEAR GT_ZTCOUM02.
  REFRESH GT_ZTCOUM02.

  SELECT RGRP2 TEXT INTO TABLE GT_ZTCOUM02
    FROM ZTCOUM02
   WHERE GRP1 = 'Z'.
  SORT GT_ZTCOUM02 BY RGRP2.

  IF P_GR = 'X'.
*   Get data for ABP
    PERFORM PROGRESS_IND USING '20' TEXT-007.
    PERFORM GET_ABP_DATA.

*   Get data for Info
    PERFORM PROGRESS_IND USING '25' TEXT-005.
    PERFORM GET_INFO_DATA.
  ENDIF.

* Get Data for GR/IV/Import
  PERFORM GET_MLDOC_RECEIPT.
  PERFORM GET_ITAB_FR_ML.
  PERFORM MOVE_GR_TO_GT_OUT.

* Get data for Revaluation
  IF P_RV = 'X'.
    PERFORM GET_RV_BSIS_BSEG.
    PERFORM GET_ITAB_FROM_FI.
    PERFORM MOVE_RV_TO_GT_OUT.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_R_BWKEY
*&---------------------------------------------------------------------*
FORM GET_R_BWKEY.
  DATA LT_T001K TYPE TABLE OF TY_T001K WITH HEADER LINE.

  CLEAR  : LT_T001K, R_BWKEY.
  REFRESH: LT_T001K, R_BWKEY.

  SELECT BWKEY INTO TABLE LT_T001K
    FROM T001K
   WHERE BUKRS = P_BUKRS.

  R_BWKEY-SIGN   = 'I'.
  R_BWKEY-OPTION = 'EQ'.

  LOOP AT LT_T001K.
    R_BWKEY-LOW = LT_T001K-BWKEY.
    APPEND R_BWKEY.
  ENDLOOP.

ENDFORM.                    " GET_R_BWKEY
*&---------------------------------------------------------------------*
*&      Form  GET_GT_MAT
*&---------------------------------------------------------------------*
FORM GET_GT_MAT.
  DATA L_ABRECHDAT LIKE CKMLHD-ABRECHDAT.

  DATA: L_DATE LIKE SY-DATUM.
  SELECT SINGLE LAST_DAY INTO L_DATE FROM CKMLRUNPERIOD
      INNER JOIN CKMLMV011
         ON  CKMLMV011~LAUFID = CKMLRUNPERIOD~RUN_ID
      WHERE GJAHR = P_MONTH(4)
        AND POPER = P_MONTH+4(2)
        AND BWKEY IN R_BWKEY.

  CHECK SY-SUBRC = 0.

  SELECT CKMLHD~KALNR
         CKMLMV011~MATNR CKMLMV011~WERKS
         CKMLMV011~MTART CKMLMV011~MATKL CKMLMV011~KALST
         CKMLMV011~BKLAS
         CKMLCR~STPRS
         CKMLCR~PVPRS AS VERPR
         CKMLCR~PEINH
         CKMLPP~MEINS
         CKMLPP~ZUKUMO  CKMLPP~EKKUMO
     INTO TABLE GT_MAT
           FROM CKMLMV011
                  INNER JOIN CKMLRUNPERIOD
                    ON  CKMLMV011~LAUFID = CKMLRUNPERIOD~RUN_ID
                  INNER JOIN CKMLHD
                    ON  CKMLMV011~KALNR = CKMLHD~KALNR
                  INNER JOIN CKMLPP
                    ON  CKMLPP~KALNR  = CKMLHD~KALNR
                   AND  CKMLPP~BDATJ  = CKMLRUNPERIOD~GJAHR
                   AND  CKMLPP~POPER  = CKMLRUNPERIOD~POPER
                   AND  CKMLPP~UNTPER = SPACE
                  INNER JOIN CKMLCR
                    ON  CKMLCR~KALNR  = CKMLPP~KALNR
                   AND  CKMLCR~BDATJ  = CKMLPP~BDATJ
                   AND  CKMLCR~POPER  = CKMLPP~POPER
                   AND  CKMLCR~CURTP  = '10'
                   AND  CKMLCR~UNTPER = SPACE
           WHERE CKMLRUNPERIOD~GJAHR = P_MONTH(4)
             AND CKMLRUNPERIOD~POPER = P_MONTH+4(2)
             AND CKMLMV011~BWKEY IN R_BWKEY
             AND CKMLMV011~BKLAS IN S_BKLAS
             AND CKMLMV011~MTART IN S_MTART
             AND CKMLMV011~MATNR IN S_MATNR
             AND ( CKMLPP~EKKUMO <> 0
                OR CKMLPP~ZUKUMO <> 0
                OR CKMLPP~PBPOPO <> 0 )  " GR/IV
    ORDER BY CKMLHD~KALNR.

  SORT GT_MAT BY KALNR.
*  DELETE ADJACENT DUPLICATES FROM GT_MAT.

  LOOP AT GT_MAT.
    GT_INFO-MATNR = GT_MAT-MATNR.

    APPEND GT_INFO.
    CLEAR GT_INFO.
  ENDLOOP.

  SORT GT_INFO.
  DELETE ADJACENT DUPLICATES FROM GT_INFO.

  CLEAR GT_105.
  REFRESH GT_105.

  SELECT TMATNR FMATNR
    INTO TABLE GT_105
    FROM ZTCOU105
     FOR ALL ENTRIES IN GT_INFO
     WHERE TMATNR = GT_INFO-MATNR.

  SORT GT_105 BY TMATNR.

  LOOP AT GT_105.
    GT_INFO-MATNR = GT_105-FMATNR.
    APPEND GT_INFO.
    CLEAR GT_INFO.
  ENDLOOP.

  SORT GT_INFO.
  DELETE ADJACENT DUPLICATES FROM GT_INFO.

  SORT GT_MAT BY MATNR.

ENDFORM.                       " GET_GT_MAT
*---------------------------------------------------------------------*
*       FORM GET_MLDOC_RECEIPT
*---------------------------------------------------------------------*
FORM GET_MLDOC_RECEIPT.
  DATA: LV_INDEX TYPE SYTABIX,
        L_REFKEY LIKE BKPF-AWKEY,
        L_XBLNR TYPE XBLNR,
        L_BKTXT TYPE BKTXT.

  PERFORM PROGRESS_IND USING '30' TEXT-004.

  CHECK NOT GT_MAT[] IS INITIAL.

  CLEAR   GT_MLDOC.
  REFRESH GT_MLDOC.

  CLEAR R_PTYP.
  REFRESH R_PTYP.

  R_PTYP-SIGN   = 'I'.      R_PTYP-OPTION = 'EQ'.
  R_PTYP-LOW    = 'B+'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'BB'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'VP'.     APPEND R_PTYP.

* GR / TRF
  IF P_GR = 'X'.
    PERFORM GET_GR_FROM_ML.
  ENDIF.

* Invoice
  IF P_IV = 'X'.
    PERFORM GET_IV_FROM_ML.
  ENDIF.

* Import
  IF P_IM = 'X'.
    PERFORM GET_IM_FROM_ML.
  ENDIF.

  SORT GT_MAT BY KALNR.
  LOOP AT GT_MAT.
    GT_MLDOC-CHK = 'X'.
    MODIFY GT_MLDOC TRANSPORTING CHK WHERE KALNR = GT_MAT-KALNR.
  ENDLOOP.
  SORT GT_MAT BY MATNR.

  SORT GT_MLDOC BY CHK.
  DELETE GT_MLDOC WHERE CHK IS INITIAL.

  CLEAR LV_INDEX.
  LOOP AT GT_MLDOC.
    LV_INDEX = SY-TABIX.

*   GR
    IF GT_MLDOC-AWTYP ='MKPF'.
      GT_MLDOC-ZEILE = GT_MLDOC-URZEILE+2(4).
      SELECT SINGLE LIFNR EBELN EBELP
          INTO (GT_MLDOC-LIFNR, GT_MLDOC-EBELN, GT_MLDOC-EBELP)
          FROM MSEG
          WHERE MBLNR = GT_MLDOC-AWREF
            AND MJAHR = GT_MLDOC-BDATJ
            AND ZEILE = GT_MLDOC-ZEILE.
      MODIFY GT_MLDOC INDEX LV_INDEX
                      TRANSPORTING LIFNR EBELN EBELP ZEILE.

*   Price change: IM
    ELSEIF GT_MLDOC-AWTYP ='PRCHG'.
      CLEAR: L_REFKEY, L_XBLNR, L_BKTXT.

      CONCATENATE GT_MLDOC-AWREF GT_MLDOC-KJAHR INTO L_REFKEY.

      SELECT SINGLE XBLNR BKTXT
          INTO (L_XBLNR, L_BKTXT)
          FROM BKPF
          WHERE AWKEY = L_REFKEY.

      IF SY-SUBRC = 0 AND L_BKTXT = 'IMPORT SETTLEMENT'.
        GT_MLDOC-EBELN = L_XBLNR+0(10).
        GT_MLDOC-EBELP = L_XBLNR+11(5).
        SELECT SINGLE LIFNR INTO GT_MLDOC-LIFNR FROM EKKO
            WHERE EBELN = GT_MLDOC-EBELN.
        MODIFY GT_MLDOC INDEX LV_INDEX
                        TRANSPORTING LIFNR EBELN EBELP.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                 " GET_MLDOC_RECEIPT
*&---------------------------------------------------------------------*
*&      Form  get_rv_bsis_bseg
*&---------------------------------------------------------------------*
FORM GET_RV_BSIS_BSEG.
  DATA L_BUZEI TYPE BUZEI.
  DATA: L_KEY(14), L_LAST(14).

  PERFORM PROGRESS_IND USING '35' TEXT-006.

  CLEAR  : GT_BSIS, GT_RV, L_BUZEI.
  REFRESH: GT_BSIS, GT_RV.

  IF NOT GT_MAT[] IS INITIAL.
    SELECT BUKRS GJAHR MONAT ZUONR BELNR BUZEI DMBTR BSCHL
      INTO TABLE GT_BSIS
      FROM BSIS
     WHERE BUKRS = P_BUKRS
       AND HKONT IN S_HKONT
       AND BUDAT BETWEEN GV_DATE_F AND GV_DATE_T.

    SORT GT_BSIS BY BELNR GJAHR.

    CLEAR: L_KEY, L_LAST.

    DATA $LIFNR LIKE BSAK-LIFNR.
    LOOP AT GT_BSIS.
      IF GT_BSIS-BSCHL = 'H'.
        GT_BSIS-DMBTR = - GT_BSIS-DMBTR.
      ENDIF.

      READ TABLE GT_MAT WITH KEY MATNR = GT_BSIS-ZUONR BINARY SEARCH.

      CHECK SY-SUBRC = 0.

      CONCATENATE GT_BSIS-BELNR GT_BSIS-GJAHR INTO L_KEY.
      MOVE-CORRESPONDING GT_BSIS TO GT_RV.

      SELECT SINGLE BWKEY EBELN EBELP MENGE MEINS PEINH
        INTO CORRESPONDING FIELDS OF GT_RV
        FROM BSEG
       WHERE BUKRS = GT_BSIS-BUKRS
         AND BELNR = GT_BSIS-BELNR
         AND GJAHR = GT_BSIS-GJAHR
         AND BUZEI = GT_BSIS-BUZEI.

      IF L_KEY <> L_LAST.
        CLEAR $LIFNR.
        SELECT SINGLE LIFNR INTO $LIFNR
          FROM BSIK
         WHERE BUKRS = GT_BSIS-BUKRS
           AND BELNR = GT_BSIS-BELNR
           AND GJAHR = GT_BSIS-GJAHR.

        IF SY-SUBRC <> 0.
          SELECT SINGLE LIFNR INTO $LIFNR
            FROM BSAK
           WHERE BUKRS = GT_BSIS-BUKRS
             AND BELNR = GT_BSIS-BELNR
             AND GJAHR = GT_BSIS-GJAHR.
        ENDIF.
      ENDIF.
      GT_RV-LIFNR = $LIFNR.
      APPEND GT_RV.
      CLEAR GT_RV.

      CONCATENATE GT_BSIS-BELNR GT_BSIS-GJAHR INTO L_LAST.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_RV_BSIS_BSEG
*&---------------------------------------------------------------------*
*&      Form  get_itab_fr_ml
*&---------------------------------------------------------------------*
FORM GET_ITAB_FR_ML.
  DATA: LT_LDC TYPE TABLE OF TY_OUT WITH HEADER LINE,
        L_LDC_AMT TYPE DMBTR,
        L_NET_AMT TYPE DMBTR.

  IF NOT GT_MLDOC[] IS INITIAL.
    CLEAR LT_LDC.
    REFRESH LT_LDC.

    LOOP AT GT_MLDOC.
      CHECK NOT GT_MLDOC-LIFNR IS INITIAL.

      ITAB-KOKRS = P_BUKRS.            " Controlling area
      ITAB-BDATJ = GT_MLDOC-BDATJ.     " Fiscal year
      ITAB-POPER = GT_MLDOC-POPER.     " Period
      ITAB-MATNR = GT_MLDOC-MATNR.     " Material
      ITAB-LIFNR = GT_MLDOC-LIFNR.     " Vendor
      ITAB-MENGE = GT_MLDOC-MENGE.     " GR Qty.
      ITAB-SALK3 = GT_MLDOC-SALK3.     " AMT
      ITAB-CHNGE = GT_MLDOC-PRDIF.
      ITAB-MEINS = GT_MLDOC-MEINS.     " UoM
*     itab-peinh = gt_mldoc-peinh.     "Fixme collect

*     Create internal table ITAB for GR
      IF P_GR = 'X' AND GT_MLDOC-AWTYP = 'MKPF'.
        CLEAR: L_LDC_AMT, L_NET_AMT.

        PERFORM GET_LDC TABLES LT_LDC
                        CHANGING L_LDC_AMT
                                 L_NET_AMT.

        ITAB-KZUST = C_GR.
        ITAB-SALK3 = ITAB-SALK3 - L_LDC_AMT.
*FIXME-ANDY
        ITAB-CHNGE = L_NET_AMT - ITAB-SALK3.  "PRDIF??
        COLLECT ITAB.
      ENDIF.

*     Create internal table ITAB for Invoice
      IF P_IV = 'X' AND GT_MLDOC-AWTYP = 'RMRP'.
        DATA L_KSCHL TYPE KSCHL.

        GT_MLDOC-ZEILE = GT_MLDOC-URZEILE.

        CLEAR L_KSCHL.
        SELECT SINGLE KSCHL MENGE INTO (L_KSCHL, ITAB-MENGE)
            FROM RSEG
            WHERE BELNR = GT_MLDOC-AWREF
              AND GJAHR = GT_MLDOC-BDATJ
              AND BUZEI = GT_MLDOC-ZEILE.

        IF L_KSCHL IN R_KSCHL.
          ITAB-KZUST = C_IV.
        ELSE.
          ITAB-KZUST = C_LDC.
        ENDIF.

        COLLECT ITAB.
      ENDIF.

*     Create internal table ITAB for Import expenses
      IF P_IM = 'X'                AND
         GT_MLDOC-AWTYP = 'PRCHG'  AND
         GT_MLDOC-FELDG = 'PBO'    AND
         GT_MLDOC-KATEGORIE = 'VP' AND
         GT_MLDOC-PTYP = 'DC'.
        ITAB-KZUST = C_IM.
        COLLECT ITAB.
      ENDIF.

*     exchange rate diff = GR + IV (gt_mldoc-prdif)
      IF GT_MLDOC-KRDIF <> 0.
        ITAB-KZUST = C_EXC.
        ITAB-CHNGE = GT_MLDOC-KRDIF.
        CLEAR ITAB-PRDIF.
        COLLECT ITAB.
      ENDIF.

    ENDLOOP.

    IF NOT LT_LDC[] IS INITIAL.
      APPEND LINES OF LT_LDC TO ITAB.
    ENDIF.

  ENDIF.

ENDFORM.                    " get_itab_fr_ml
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
FORM DISP_RESULT.
  REFRESH: GT_FIELDCAT, GT_EVENTS.
  CLEAR:   GT_FIELDCAT, GS_LAYOUT, GT_EVENTS.

  PERFORM BUILD_FIELD_CATEGORY USING:
*    'KOKRS'  'X'  'Controlling Area'   4    'CHAR',
    'BDATJ'  'X'  'Year'               4	  'CHAR',
    'POPER'  'X'  'Per'                3    'NUMC',
    'MATNR'  'X'  'Material'           18   'CHAR',
    'LIFNR'  'X'  'Vendor'             10   'CHAR',
    'GRP1'   'X'  'Cat'                1    'CHAR',
    'KZUST'  'X'  'Reason'             3    'CHAR',
    'PRICE'  ' '  'Base Price'         15   'CURR',
    'PEINH'  ' '  'Price unit'         15   'DEC',
    'PRDIF'  ' '  'Price diff'         10   'CURR',
    'CHNGE'  ' '  'Value Chg'          15   'CURR',
    'SALK3'  ' '  'Value STD'          15   'CURR',
    'EKPRD'  ' '  'Purch Diff'         10   'CURR',
    'MENGE'  ' '  'Quantity'           15   'QUAN',
    'MEINS'  ' '  'UoM'                3    'UNIT'.
  	
  GS_FIELDCAT-REF_FIELDNAME  = 'MEINS'.
  GS_FIELDCAT-REF_TABNAME = 'ZTCOU111'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT
    TRANSPORTING REF_FIELDNAME  REF_TABNAME
    WHERE FIELDNAME = 'MEINS'.

  GS_FIELDCAT-QFIELDNAME = 'MEINS'.
  MODIFY GT_FCAT FROM GS_FCAT
     TRANSPORTING QFIELDNAME WHERE FIELDNAME = 'MENGE'.

  PERFORM SET_LAYOUT USING  ' '  ' ' CHANGING GS_LAYOUT.
  PERFORM SET_EVENTS CHANGING GT_EVENTS.
  PERFORM SET_LIST_HEADER  USING GT_LIST_TOP_OF_PAGE[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = SY-CPROG
            I_CALLBACK_PF_STATUS_SET = GC_PF_STATUS_SET
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            I_SAVE                   = GC_VAR_SAVE
            IT_EVENTS                = GT_EVENTS[]
       TABLES
            T_OUTTAB                 = GT_OUT.

ENDFORM.                    " DISP_RESULT

*&---------------------------------------------------------------------*
*&      Form  SET_LIST_HEADER
*&---------------------------------------------------------------------*
FORM SET_LIST_HEADER USING  LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  CLEAR LS_LINE.

* Month
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Month :'.
  WRITE: P_MONTH TO LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

* Material
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Material :'.
  CONCATENATE S_MATNR-LOW '~' S_MATNR-HIGH INTO LS_LINE-INFO
              SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                     " SET_LIST_HEADER
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: LV_ANSWER,
        LV_CNT TYPE I,
        LV_CNT2 TYPE I,
        L_KZUST TYPE KZUST,
        LT_M02  TYPE TABLE OF ZTCOUM02 WITH HEADER LINE.

  CLEAR: LV_ANSWER, LV_CNT, LV_CNT2, LT_M02.
  REFRESH LT_M02.

  SELECT * INTO TABLE LT_M02
    FROM ZTCOUM02.

  IF GV_PAR = SPACE.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
              TEXT_QUESTION  = 'Do you want to delete existing data?'
         IMPORTING
              ANSWER         = LV_ANSWER
         EXCEPTIONS
              TEXT_NOT_FOUND = 1
              OTHERS         = 2.
  ELSE.
    LV_ANSWER = '1'.
  ENDIF.

  IF LV_ANSWER = '1'.
    SORT GT_OUT BY KOKRS BDATJ POPER MATNR.

    DELETE FROM ZTCOU111
     WHERE KOKRS = P_BUKRS
       AND BDATJ = GV_YEAR
       AND POPER = GV_POPER
       AND MATNR IN S_MATNR.

    LOOP AT GT_OUT.
      AT NEW MATNR.
        LV_CNT = 1.
      ENDAT.

      IF GT_OUT-KZUST = 'XXX'.
        GT_OUT-GRP1 = 'P'.
      ELSE.
        IF GT_OUT-KZUST+0(1) = 'X'.
          READ TABLE LT_M02 WITH KEY RGRP2 = GT_OUT-KZUST+1(2)
                            BINARY SEARCH.
        ELSE.
          READ TABLE LT_M02 WITH KEY RGRP2 = GT_OUT-KZUST+0(2)
                            BINARY SEARCH.
          IF SY-SUBRC <> 0.
            CLEAR L_KZUST.
            CONCATENATE GT_OUT-KZUST+0(1) GT_OUT-KZUST+2(1)
                   INTO L_KZUST.
            READ TABLE LT_M02 WITH KEY RGRP2 = L_KZUST
                            BINARY SEARCH.
          ENDIF.
        ENDIF.

        IF SY-SUBRC = 0.
          GT_OUT-GRP1 = LT_M02-GRP1.
        ENDIF.
      ENDIF.

      GT_OUT-AEDAT = SY-DATUM.           " Created on
      GT_OUT-AENAM = SY-UNAME.           " Created by
      GT_OUT-SEQNO = LV_CNT.

      INSERT ZTCOU111 FROM GT_OUT.

      IF SY-SUBRC = 0.
        LV_CNT = LV_CNT + 1.
        LV_CNT2 = LV_CNT2 + 1.
      ENDIF.
    ENDLOOP.

    MESSAGE S000 WITH 'You have saved;' LV_CNT2 'records.'.
  ENDIF.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL
*&---------------------------------------------------------------------*
FORM GET_MATERIAL.
  CLEAR GT_MAT.
  REFRESH GT_MAT.

  PERFORM GET_R_BWKEY.
  PERFORM PROGRESS_IND USING '20' TEXT-003.

  PERFORM GET_GT_MAT.

ENDFORM.                    " GET_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
FORM PROGRESS_IND USING    P_%
                           P_TEXT.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = P_%
            TEXT       = P_TEXT.

ENDFORM.                    " PROGRESS_IND
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_PRICE
*&---------------------------------------------------------------------*
FORM GET_INFO_PRICE.
  DATA L_DATE TYPE SYDATUM.

  READ TABLE GT_ABP WITH KEY MATNR = GT_MAT-MATNR
                             LIFNR = ITAB-LIFNR.

  IF SY-SUBRC = 0.
    ITAB-PRICE = GT_ABP-WERTN.

  ELSE.
    CLEAR L_DATE.
    CONCATENATE P_MONTH+0(4) '0101' INTO L_DATE.

    LOOP AT GT_CONDI WHERE MATNR = GT_MAT-MATNR
                       AND LIFNR = ITAB-LIFNR.
      READ TABLE GT_KNUMH WITH KEY KNUMH = GT_CONDI-KNUMH.

      IF SY-SUBRC = 0.
        IF GT_KNUMH-DATAB <= L_DATE AND
          GT_KNUMH-DATBI >= L_DATE.
          ITAB-PRICE = GT_CONDI-KBETR.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ITAB-PRICE IS INITIAL.
      LOOP AT GT_CONDI WHERE MATNR = GT_MAT-MATNR
                         AND LIFNR = ITAB-LIFNR.
        READ TABLE GT_KNUMH WITH KEY KNUMH = GT_CONDI-KNUMH.
        IF SY-SUBRC = 0.
          IF GT_KNUMH-DATAB > L_DATE.
            ITAB-PRICE = GT_CONDI-KBETR.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_INFO_PRICE
*&---------------------------------------------------------------------*
*&      Form  GET_DATE
*&---------------------------------------------------------------------*
FORM GET_DATE.
  DATA: L_DATE(8),
        L_GJAHR  TYPE BDATJ,
        L_POPER  TYPE POPER.

  CLEAR: GV_INFO_F, GV_DATE_F, GV_DATE_T, GV_DATE3.

  GV_YEAR = P_MONTH+0(4).
  GV_POPER = P_MONTH+4(2).

  CLEAR L_DATE.
  CONCATENATE '0101' P_MONTH+0(4)  INTO L_DATE.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            INPUT  = L_DATE
       IMPORTING
            OUTPUT = GV_INFO_F.

  CLEAR L_DATE.
  CONCATENATE P_MONTH+4(2) '01' P_MONTH+0(4)  INTO L_DATE.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            INPUT  = L_DATE
       IMPORTING
            OUTPUT = GV_DATE_F.

  CLEAR: L_GJAHR, L_POPER.
  L_GJAHR = P_MONTH+0(4).
  L_POPER = P_MONTH+4(2).

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = L_GJAHR
            I_PERIV = 'K0'
            I_POPER = L_POPER
       IMPORTING
            E_DATE  = GV_DATE_T.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = GV_DATE_T
            DAYS      = '00'
            MONTHS    = '01'
            SIGNUM    = '+'
            YEARS     = '00'
       IMPORTING
            CALC_DATE = GV_DATE3.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = GV_DATE3
       IMPORTING
            LAST_DAY_OF_MONTH = GV_DATE3.

ENDFORM.                    " GET_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_DATA
*&---------------------------------------------------------------------*
*       Get data for Info
*----------------------------------------------------------------------*
FORM GET_INFO_DATA.
  TYPES: BEGIN OF TY_LIFNR,
           LIFNR TYPE LIFNR,
         END OF TY_LIFNR.

  DATA: LT_LIFNR TYPE TABLE OF TY_LIFNR WITH HEADER LINE,
        LT_CONDI LIKE GT_CONDI OCCURS 0 WITH HEADER LINE.

  RANGES R_LIFNR FOR A018-LIFNR.
*
  CLEAR GT_CONDI.
  REFRESH GT_CONDI.

  LOOP AT GT_INFO.
    CLEAR:   GT_EINA, R_LIFNR, GT_KNUMH.
    REFRESH: GT_EINA, R_LIFNR,GT_KNUMH.

    READ TABLE GT_MAT WITH KEY MATNR = GT_INFO-MATNR BINARY SEARCH.

    IF SY-SUBRC <> 0.
      READ TABLE GT_105 WITH KEY TMATNR = GT_INFO-MATNR BINARY SEARCH.
      READ TABLE GT_MAT WITH KEY MATNR = GT_105-FMATNR BINARY SEARCH.
    ENDIF.

    SELECT A~MATNR LIFNR B~EKGRP
      INTO TABLE GT_EINA
      FROM EINA AS A
      JOIN EINE AS B
        ON A~INFNR = B~INFNR
     WHERE A~MATNR = GT_INFO-MATNR
       AND A~LOEKZ <> 'X'
       AND B~WERKS = ' '
       AND B~EKORG = C_EKORG
       AND B~LOEKZ <> 'X'.

    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    SORT GT_EINA BY MATNR LIFNR.
    DELETE ADJACENT DUPLICATES FROM GT_EINA.
*
    R_LIFNR-SIGN = 'I'.
    R_LIFNR-OPTION = 'EQ'.

    LOOP AT GT_EINA.
      R_LIFNR-LOW = GT_EINA-LIFNR.
      APPEND R_LIFNR.
    ENDLOOP.
*
    SELECT MATNR LIFNR KNUMH DATAB DATBI
      INTO TABLE GT_KNUMH
      FROM A018
     WHERE KAPPL = 'M'                " Purchasing
       AND KSCHL = 'PB00'             " ZTIR = PB00
       AND LIFNR IN R_LIFNR
       AND MATNR = GT_INFO-MATNR
       AND EKORG = C_EKORG
       AND ESOKZ = '0'                " Standard
       AND ( ( DATAB =< GV_INFO_F AND DATBI >= GV_INFO_F ) OR
             ( DATAB >= GV_INFO_F AND DATBI <= GV_DATE_T ) OR
             ( DATAB =< GV_DATE_T AND DATBI >= GV_DATE_T ) ).

    IF SY-SUBRC = 0.
      SORT GT_KNUMH BY DATAB DESCENDING.

      LOOP AT GT_KNUMH.
        READ TABLE GT_EINA WITH KEY MATNR = GT_KNUMH-MATNR
                                LIFNR = GT_KNUMH-LIFNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_KNUMH-EKGRP = GT_EINA-EKGRP.
          MODIFY GT_KNUMH INDEX 1 TRANSPORTING EKGRP.
        ENDIF.

        CLEAR LT_CONDI.
        REFRESH LT_CONDI.

        SELECT A~KNUMH A~DATAB A~DATBI A~KZUST B~KPEIN B~MEINS
               SUM( B~KBETR )
          INTO TABLE LT_CONDI
          FROM KONH AS A
          JOIN KONP AS B
            ON B~KNUMH = A~KNUMH
           AND B~KAPPL = A~KAPPL
           AND B~KSCHL = A~KSCHL
         WHERE A~KNUMH = GT_KNUMH-KNUMH
           AND A~KAPPL = 'M'
           AND A~KSCHL IN R_KSCHL
         GROUP BY A~KNUMH A~DATAB A~DATBI A~KZUST B~KPEIN B~MEINS.
*
        DATA: L_OUTPUT(10) TYPE P DECIMALS 3,
              L_IDX        TYPE SYTABIX.

        LOOP AT LT_CONDI.
          L_IDX = SY-TABIX.

          CLEAR L_OUTPUT.

          LT_CONDI-MATNR = GT_KNUMH-MATNR.
          LT_CONDI-LIFNR = GT_KNUMH-LIFNR.

          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
               EXPORTING
                    INPUT                = LT_CONDI-KPEIN
                    UNIT_IN              = LT_CONDI-MEINS
                    UNIT_OUT             = GT_MAT-MEINS
               IMPORTING
                    OUTPUT               = L_OUTPUT
               EXCEPTIONS
                    CONVERSION_NOT_FOUND = 1
                    DIVISION_BY_ZERO     = 2
                    INPUT_INVALID        = 3
                    OUTPUT_INVALID       = 4
                    OVERFLOW             = 5
                    TYPE_INVALID         = 6
                    UNITS_MISSING        = 7
                    UNIT_IN_NOT_FOUND    = 8
                    UNIT_OUT_NOT_FOUND   = 9.

          IF SY-SUBRC <> 0.
*           ALT UoM
            DATA L_UMREZ TYPE UMREZ.

            CLEAR L_UMREZ.
            SELECT SINGLE UMREZ INTO L_UMREZ FROM MARM
               WHERE MATNR = GT_MAT-MATNR
                 AND MEINH = LT_CONDI-MEINS.

            IF SY-SUBRC = 0.
              L_OUTPUT = LT_CONDI-KPEIN * L_UMREZ.
            ENDIF.
          ENDIF.

          IF SY-SUBRC <> 0.
            MESSAGE S000 WITH 'You cannot conversion UoM '
                              LT_CONDI-MEINS ' into '
                              GT_MAT-MEINS.
          ELSE.
            LT_CONDI-KBETR = LT_CONDI-KBETR * GT_MAT-PEINH / L_OUTPUT.
            LT_CONDI-KPEIN = GT_MAT-PEINH.

            MODIFY LT_CONDI INDEX L_IDX
                   TRANSPORTING MATNR LIFNR KBETR KPEIN.
          ENDIF.
        ENDLOOP.

        APPEND LINES OF LT_CONDI TO GT_CONDI.
        CLEAR GT_CONDI.

      ENDLOOP.
    ENDIF.

  ENDLOOP.

  SORT GT_CONDI BY MATNR LIFNR DATAB.

ENDFORM.                    " GET_INFO_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ABP_DATA
*&---------------------------------------------------------------------*
*       Get data for ABP
*----------------------------------------------------------------------*
FORM GET_ABP_DATA.
  CLEAR GT_ABP.
  REFRESH GT_ABP.

  SELECT KOKRS BDATJ POPER MATNR LIFNR KZUST1 WERTN PMEHT PEINH
    INTO TABLE GT_ABP
    FROM ZTCOU102
     FOR ALL ENTRIES IN GT_INFO
   WHERE KOKRS = P_BUKRS
     AND BDATJ = P_MONTH+0(4)
     AND POPER = '01'
     AND KALKA = 'BP'
     AND VER = 0
     AND MATNR = GT_INFO-MATNR.

  SORT GT_ABP BY MATNR.
*
  DATA: L_IDX TYPE SYTABIX,
        L_OUTPUT TYPE P DECIMALS 3.

  CLEAR L_IDX.

  LOOP AT GT_ABP.
    L_IDX = SY-TABIX.

    READ TABLE GT_MAT WITH KEY MATNR = GT_ABP-MATNR BINARY SEARCH.

    IF SY-SUBRC = 0.
      CLEAR L_OUTPUT.

      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                INPUT                = GT_ABP-PEINH
                UNIT_IN              = GT_ABP-PMEHT
                UNIT_OUT             = GT_MAT-MEINS
           IMPORTING
                OUTPUT               = L_OUTPUT
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                DIVISION_BY_ZERO     = 2
                INPUT_INVALID        = 3
                OUTPUT_INVALID       = 4
                OVERFLOW             = 5
                TYPE_INVALID         = 6
                UNITS_MISSING        = 7
                UNIT_IN_NOT_FOUND    = 8
                UNIT_OUT_NOT_FOUND   = 9.

      IF L_OUTPUT EQ 0.
        L_OUTPUT = 1.
      ENDIF.

      GT_ABP-WERTN = GT_ABP-WERTN * GT_MAT-PEINH / L_OUTPUT.

      MODIFY GT_ABP INDEX L_IDX TRANSPORTING WERTN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_ABP_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_INV
*&---------------------------------------------------------------------*
*       Get Invoice Information
*----------------------------------------------------------------------*
FORM GET_INV.
  ITAB-KZUST = C_IV.
  ITAB-CHNGE = GT_MLDOC-PRDIF.

  COLLECT ITAB.

ENDFORM.                    " GET_INV
*&---------------------------------------------------------------------*
*&      Form  GET_RV
*&---------------------------------------------------------------------*
*       Create internal table ITAB for Revalueation
*----------------------------------------------------------------------*
FORM MOVE_RV_TO_GT_OUT.

  SORT ITAB BY MATNR LIFNR.

* Create internal table GT_OUT for Adjust Price History & display
  DATA L_PRICE.
  CLEAR L_PRICE.

  LOOP AT ITAB.
    MOVE-CORRESPONDING ITAB TO GT_OUT.
    READ TABLE GT_MAT WITH KEY MATNR = ITAB-MATNR BINARY SEARCH.

    IF SY-SUBRC <> 0.
      IF P_BREAK = 'X'. BREAK-POINT. ENDIF.
      MESSAGE S000 WITH ITAB-MATNR ' - Not found in GT_MAT'.
    ENDIF.
    GT_OUT-PEINH = GT_MAT-PEINH.

    IF ITAB-MENGE = 0.
    ELSE.
      GT_OUT-PRDIF = ITAB-CHNGE / ITAB-MENGE.        " Price Diff.
    ENDIF.

    APPEND GT_OUT.
    CLEAR GT_OUT.

    L_PRICE = ITAB-PRICE.
  ENDLOOP.

ENDFORM.                    " MOVE_RV_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_ABP_FROM_INFO_RECORD
*&---------------------------------------------------------------------*
FORM GET_ABP_FROM_INFO_RECORD TABLES LT_OUT STRUCTURE ITAB
                              CHANGING P_ABP TYPE KBETR_KOND
                                       P_PRICE  TYPE ZPRICE3.

  DATA: L_IDX    TYPE SYTABIX,
        LT_CONDI TYPE TABLE OF TY_CONDI WITH HEADER LINE,
        W_CONDI  TYPE TY_CONDI.

  CLEAR: L_IDX, GT_105, LT_CONDI.
  REFRESH LT_CONDI.

  READ TABLE GT_105 WITH KEY TMATNR = ITAB-MATNR BINARY SEARCH.
  READ TABLE GT_ABP WITH KEY MATNR = ITAB-MATNR BINARY SEARCH.

  IF SY-SUBRC = 0.
    P_ABP = GT_ABP-WERTN.
  ELSE.
    IF NOT GT_105-TMATNR IS INITIAL.
      READ TABLE GT_ABP WITH KEY MATNR = GT_105-TMATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        P_ABP = GT_ABP-WERTN.
      ENDIF.
    ENDIF.
  ENDIF.

* check previous material
  IF NOT GT_105-FMATNR IS INITIAL.
    READ TABLE GT_CONDI WITH KEY MATNR = ITAB-MATNR
                                 LIFNR = ITAB-LIFNR BINARY SEARCH
                        INTO  W_CONDI.
    IF SY-SUBRC = 0 AND W_CONDI-DATAB > GV_INFO_F.
      LOOP AT GT_CONDI WHERE MATNR = GT_105-FMATNR
                         AND LIFNR = ITAB-LIFNR.
        IF GT_CONDI-DATBI < W_CONDI-DATAB
        OR ( GT_CONDI-DATAB < W_CONDI-DATAB AND
             GT_CONDI-DATBI > W_CONDI-DATAB ).
          APPEND GT_CONDI TO LT_CONDI.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  LOOP AT GT_CONDI WHERE MATNR = ITAB-MATNR
                     AND LIFNR = ITAB-LIFNR.
    APPEND GT_CONDI TO LT_CONDI.
  ENDLOOP.


  LOOP AT LT_CONDI.
    L_IDX = L_IDX + 1.
    LT_OUT-LIFNR = LT_CONDI-LIFNR.

    IF L_IDX = 1.
      IF NOT P_ABP IS INITIAL.
        LT_OUT-KZUST = C_ABP.
        LT_OUT-PRICE = P_ABP. "gt_condi-kbetr.
        LT_OUT-PRDIF = GT_CONDI-KBETR - P_ABP.
        LT_OUT-CHNGE = ITAB-MENGE * LT_OUT-PRDIF.
        P_PRICE = LT_OUT-PRICE.
        APPEND LT_OUT.

      ELSE.
        P_ABP = P_PRICE = LT_CONDI-KBETR.
      ENDIF.

    ELSE.
      IF LT_CONDI-KZUST = SPACE OR LT_CONDI-KZUST = 'ZLC'.
        LT_OUT-KZUST = 'XXX'.  "itab-kzust. "'Z1'.
      ELSE.
        LT_OUT-KZUST = LT_CONDI-KZUST.
      ENDIF.

      LT_OUT-PRICE = P_ABP. "gt_condi-kbetr.
      LT_OUT-PRDIF = LT_CONDI-KBETR - P_PRICE.
      LT_OUT-CHNGE = ITAB-MENGE * LT_OUT-PRDIF.
      P_PRICE = LT_CONDI-KBETR.
      APPEND LT_OUT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_ABP_FROM_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  MOVE_GR_TO_GT_OUT
*&---------------------------------------------------------------------*
FORM MOVE_GR_TO_GT_OUT.
  DATA: LT_OUT  LIKE ITAB OCCURS 0 WITH HEADER LINE,
        L_ABP   TYPE KBETR_KOND,     " Last price
        L_PRICE TYPE ZPRICE3,        " Last price
        L_SUM   TYPE ZPRICE3.

  CLEAR L_PRICE.

  LOOP AT ITAB.
    READ TABLE GT_MAT WITH KEY MATNR = ITAB-MATNR BINARY SEARCH.
    IF SY-SUBRC <> 0.
      IF P_BREAK = 'X'. BREAK-POINT. ENDIF.
      MESSAGE S000 WITH ITAB-MATNR ' - Not found in GT_MAT'.
    ENDIF.

    ITAB-PEINH = GT_MAT-PEINH.

*   When GR price is exist
    IF ITAB-KZUST = C_GR.
      IF ITAB-MENGE = 0 AND ITAB-CHNGE = 0.
        CONTINUE.
      ENDIF.

*     GR - Total
      MOVE-CORRESPONDING ITAB TO GT_OUT.
      GT_OUT-EKPRD = GT_OUT-CHNGE.
      CLEAR GT_OUT-CHNGE.
      APPEND GT_OUT.

      REFRESH LT_OUT.
      MOVE-CORRESPONDING ITAB TO LT_OUT.
*     1. Select ABP & Info prices
      CLEAR: L_ABP, L_PRICE.
      PERFORM GET_ABP_FROM_INFO_RECORD TABLES LT_OUT
                                       CHANGING  L_ABP
                                                 L_PRICE.

      CLEAR: LT_OUT, L_SUM.
      LOOP AT LT_OUT.
        APPEND LT_OUT TO GT_OUT.
        L_SUM = L_SUM + LT_OUT-CHNGE.
      ENDLOOP.

*     2. Select GR prices
      MOVE-CORRESPONDING ITAB TO GT_OUT.
      IF ITAB-MENGE = 0.
*       BREAK-POINT.
      ELSE.
        ITAB-PRICE = ITAB-PEINH * ( ITAB-SALK3 + ITAB-CHNGE )
                      / ITAB-MENGE.
      ENDIF.
      GT_OUT-PRDIF = ITAB-PRICE - L_PRICE.
      IF GT_OUT-PRDIF <> 0.
        GT_OUT-KZUST = C_PO.
        GT_OUT-PRICE = L_ABP.
        GT_OUT-CHNGE = ITAB-MENGE * GT_OUT-PRDIF.
        APPEND GT_OUT.
        L_SUM = L_SUM + GT_OUT-CHNGE.
      ENDIF.

*     3. Select Standard price
      READ TABLE GT_MAT WITH KEY MATNR = ITAB-MATNR
                        BINARY SEARCH.

      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING ITAB TO GT_OUT.
        GT_OUT-KZUST = C_STD.
        GT_OUT-PRICE = L_ABP.
        GT_OUT-PRDIF = L_ABP - GT_MAT-STPRS.
        GT_OUT-CHNGE = GT_OUT-MENGE * GT_OUT-PRDIF.
        APPEND GT_OUT.
        L_SUM = L_SUM + GT_OUT-CHNGE.
      ENDIF.

*     4. Rounding
      IF ITAB-CHNGE <> L_SUM.   "itab-prdif <>
        GT_OUT-KZUST = C_OTH.
        GT_OUT-PRICE = L_ABP.
        GT_OUT-CHNGE = ITAB-CHNGE - L_SUM.

        IF GT_OUT-MENGE <> 0.
          GT_OUT-PRDIF = GT_OUT-CHNGE / GT_OUT-MENGE.
        ENDIF.

        APPEND GT_OUT.
      ENDIF.

    ELSE.
      MOVE-CORRESPONDING ITAB TO GT_OUT.

      IF ITAB-MENGE <> 0.
        GT_OUT-PRDIF = ITAB-CHNGE / ITAB-MENGE.
      ENDIF.

      APPEND GT_OUT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " MOVE_GR_TO_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_EINA
*&---------------------------------------------------------------------*
*       Get Pur.Group
*----------------------------------------------------------------------*
FORM GET_GT_EINA.
  CLEAR GT_EINA.
  REFRESH GT_EINA.

  SELECT MATNR LIFNR EKGRP
    INTO TABLE GT_EINA
    FROM EINA AS A
    JOIN EINE AS B
      ON A~INFNR = B~INFNR
     FOR ALL ENTRIES IN GT_KNUMH
   WHERE A~MATNR = GT_KNUMH-MATNR
     AND A~LIFNR = GT_KNUMH-LIFNR
     AND A~LOEKZ <> 'X'
     AND B~WERKS = ' '
     AND B~EKORG = C_EKORG
     AND B~LOEKZ <> 'X'.

  SORT GT_EINA BY MATNR LIFNR.
  DELETE ADJACENT DUPLICATES FROM GT_EINA.

ENDFORM.                    " GET_GT_EINA
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_gr_from_ml
*&---------------------------------------------------------------------*
FORM GET_GR_FROM_ML.
  SELECT A~BELNR A~KJAHR A~VGART A~AWTYP A~AWREF A~BLDAT
       C~KATEGORIE C~PTYP
       C~URZEILE
       C~MATNR C~BWKEY C~MEINS
       C~KALNR C~BUKRS C~PSART E~WAERS E~SALK3 E~PEINH
       D~BDATJ D~POPER D~LBKUM AS MENGE
       F~FELDG F~PRDIF
       B~BUDAT
    INTO CORRESPONDING FIELDS OF TABLE GT_MLDOC
    FROM MLHD AS A
    INNER JOIN MKPF AS B
            ON B~MBLNR = A~AWREF
           AND B~MJAHR = A~AWORG
    INNER JOIN MLIT AS C
            ON C~BELNR = A~BELNR
           AND C~KJAHR = A~KJAHR
    INNER JOIN MLPP AS D
            ON D~BELNR = C~BELNR
           AND D~KJAHR = C~KJAHR
           AND D~POSNR = C~POSNR
           AND D~BDATJ = GV_YEAR
           AND D~POPER = GV_POPER
    INNER JOIN MLCR AS E
            ON E~BELNR = D~BELNR
           AND E~KJAHR = D~KJAHR
           AND E~POSNR = D~POSNR
           AND E~BDATJ = D~BDATJ
           AND E~POPER = D~POPER
    LEFT JOIN MLCRF AS F
           ON E~BELNR = F~BELNR
          AND E~KJAHR = F~KJAHR
          AND E~POSNR = F~POSNR
          AND E~BDATJ = F~BDATJ
          AND E~POPER = F~POPER
          AND E~CURTP = F~CURTP
          AND F~FELDG = 'ZUO'
   WHERE ( A~CPUDT >= GV_DATE_F AND A~CPUDT <  GV_DATE3 )
     AND A~VGART  = 'UP'
     AND A~AWTYP = 'MKPF'
     AND A~TCODE <> 'MB1B'          " exclude stock trf.
     AND ( B~BUDAT >= GV_DATE_F AND B~BUDAT <= GV_DATE_T )
     AND C~PSART = 'UP'             " ML update
     AND ( C~KATEGORIE = 'ZU' OR C~KATEGORIE = 'VP' )
     AND E~CURTP  = '10'
     AND A~AWREF IN S_AWREF
     AND A~AWORG IN S_AWORG
     AND A~BELNR IN S_BELNR
     AND A~KJAHR IN S_KJAHR.

ENDFORM.                    " get_gr_from_ml
*&---------------------------------------------------------------------*
*&      Form  GET_IV_FROM_ML
*&---------------------------------------------------------------------*
*       Get Invoice Data
*----------------------------------------------------------------------*
FORM GET_IV_FROM_ML.
  SELECT A~BELNR A~KJAHR A~VGART A~AWTYP A~AWREF A~BLDAT
         C~KATEGORIE C~PTYP C~URZEILE C~MATNR C~BWKEY C~MEINS
         C~KALNR C~BUKRS C~PSART E~WAERS E~SALK3 E~PEINH
         D~BDATJ D~POPER D~LBKUM AS MENGE
         F~FELDG F~PRDIF
         B~BUDAT B~LIFNR
    APPENDING CORRESPONDING FIELDS OF TABLE GT_MLDOC
         FROM ( MLHD AS A
         INNER JOIN RBKP AS B
            ON B~BELNR = A~AWREF
           AND B~GJAHR = A~AWORG
         INNER JOIN MLIT AS C
            ON C~BELNR = A~BELNR
           AND C~KJAHR = A~KJAHR
         INNER JOIN MLPP AS D
            ON D~BELNR = C~BELNR
           AND D~KJAHR = C~KJAHR
           AND D~POSNR = C~POSNR
           AND D~BDATJ  = GV_YEAR
           AND D~POPER  = GV_POPER
         INNER JOIN MLCR AS E
            ON E~BELNR = D~BELNR
           AND E~KJAHR = D~KJAHR
           AND E~POSNR = D~POSNR
           AND E~BDATJ = D~BDATJ
           AND E~POPER = D~POPER )
         INNER JOIN MLCRF AS F
            ON E~BELNR = F~BELNR
           AND E~KJAHR = F~KJAHR
           AND E~POSNR = F~POSNR
           AND E~BDATJ = F~BDATJ
           AND E~POPER = F~POPER
           AND E~CURTP = F~CURTP
     WHERE ( A~CPUDT >= GV_DATE_F AND A~CPUDT <  GV_DATE3 )
       AND A~VGART  = 'UP'
       AND A~AWTYP = 'RMRP'
       AND ( B~BUDAT >= GV_DATE_F AND B~BUDAT <= GV_DATE_T )
       AND C~PSART = 'UP'             " ML update
       AND ( C~KATEGORIE = 'ZU' OR C~KATEGORIE = 'VP' )
       AND E~CURTP  = '10'
       AND F~FELDG = 'ZUO'
       AND A~AWREF IN S_AWREF
       AND A~AWORG IN S_AWORG.

ENDFORM.                    " GET_IV_FROM_ML
*&---------------------------------------------------------------------*
*&      Form  GET_IM_FROM_ML
*&---------------------------------------------------------------------*
*       Get Import Data
*----------------------------------------------------------------------*
FORM GET_IM_FROM_ML.
  SELECT A~BELNR A~KJAHR A~VGART A~AWTYP A~AWREF A~BLDAT
         C~KATEGORIE C~PTYP C~URZEILE C~MATNR C~BWKEY C~MEINS
         C~KALNR C~BUKRS C~PSART C~LIFNR E~WAERS E~SALK3 E~PEINH
         D~BDATJ D~POPER D~LBKUM AS MENGE
         F~FELDG F~PRDIF
    APPENDING CORRESPONDING FIELDS OF TABLE GT_MLDOC
         FROM ( MLHD AS A
         INNER JOIN MLIT AS C
            ON C~BELNR = A~BELNR
           AND C~KJAHR = A~KJAHR
         INNER JOIN MLPP AS D
            ON D~BELNR = C~BELNR
           AND D~KJAHR = C~KJAHR
           AND D~POSNR = C~POSNR
           AND D~BDATJ  = GV_YEAR
           AND D~POPER  = GV_POPER
         INNER JOIN MLCR AS E
            ON E~BELNR = D~BELNR
           AND E~KJAHR = D~KJAHR
           AND E~POSNR = D~POSNR
           AND E~BDATJ = D~BDATJ
           AND E~POPER = D~POPER )
         INNER JOIN MLCRF AS F
            ON E~BELNR = F~BELNR
           AND E~KJAHR = F~KJAHR
           AND E~POSNR = F~POSNR
           AND E~BDATJ = F~BDATJ
           AND E~POPER = F~POPER
           AND E~CURTP = F~CURTP
     WHERE ( A~CPUDT >= GV_DATE_F AND A~CPUDT <  GV_DATE3 )
       AND A~VGART  = 'UP'
       AND A~AWTYP = 'PRCHG'
       AND E~CURTP  = '10'
       AND F~FELDG = 'PBO'.   "?????

ENDFORM.                    " GET_IM_FROM_ML
*&---------------------------------------------------------------------*
*&      Form  GET_ITAB_FROM_FI
*&---------------------------------------------------------------------*
*       Get Revalucation
*----------------------------------------------------------------------*
FORM GET_ITAB_FROM_FI.
  REFRESH ITAB.

  LOOP AT GT_RV.
    ITAB-KOKRS = P_BUKRS.                     " Controlling area
    ITAB-BDATJ = GT_RV-GJAHR.                 " Fiscal year
    ITAB-POPER = GT_RV-MONAT.                 " Period
    ITAB-MATNR = GT_RV-ZUONR.                 " Material
    ITAB-LIFNR = GT_RV-LIFNR.                 " Vendor
    ITAB-MENGE = GT_RV-MENGE.                 " Qty.
    ITAB-KZUST = C_RV.                        " Reason code
    ITAB-CHNGE = GT_RV-DMBTR.                 " Price
    ITAB-AEDAT = SY-DATUM.                    " Created on
    ITAB-AENAM = SY-UNAME.                    " Created by

    ITAB-MEINS = GT_RV-MEINS.

    COLLECT ITAB.
    CLEAR ITAB.
  ENDLOOP.

ENDFORM.                    " GET_ITAB_FROM_FI
*&---------------------------------------------------------------------*
*&      Form  GET_LDC
*&---------------------------------------------------------------------*
*       Get LDC Diff.
*----------------------------------------------------------------------*
FORM GET_LDC TABLES P_TAB STRUCTURE ITAB
             CHANGING F_LDC_AMT TYPE DMBTR
                      F_NET_AMT  TYPE DMBTR.

  DATA: L_DMBTR    TYPE DMBTR,             " PO Amount
        L_NETWR    TYPE BWERT,             " Net Value
        L_EFFWR    TYPE EFFWR,             " Effective Value
        L_DIF      TYPE BWERT,             " Diff
        L_LDC(10)  TYPE P DECIMALS 3,      " LDC Rate
        L_NET_GR   TYPE DMBTR,             " Net GR
        L_SHKZG    TYPE SHKZG,
        L_LDC_AMT2 TYPE DMBTR.             " LDC Amout calc

  CLEAR: L_DMBTR, L_NETWR, L_EFFWR, L_DIF, L_LDC, L_NET_GR,
         L_LDC_AMT2.

* Get Net GR
  SELECT SINGLE NETWR EFFWR
    INTO (L_NETWR, L_EFFWR)
    FROM EKPO
   WHERE EBELN = GT_MLDOC-EBELN
     AND EBELP = GT_MLDOC-EBELP
     AND BSTYP = 'F'       "PO only
     AND PSTYP = '0'.      "Standard

  L_DIF = L_EFFWR - L_NETWR.

  IF L_DIF <> 0.
* Get PO amount from EKBE or EKBEH
    SELECT SINGLE DMBTR SHKZG INTO (L_NET_GR, L_SHKZG)
    FROM EKBE
   WHERE EBELN = GT_MLDOC-EBELN
     AND EBELP = GT_MLDOC-EBELP
     AND ZEKKN = '00'
     AND VGABE = '1'
     AND GJAHR = GT_MLDOC-BDATJ
     AND BELNR = GT_MLDOC-AWREF
     AND BUZEI = GT_MLDOC-ZEILE.

    IF SY-SUBRC <> 0.
      SELECT SINGLE DMBTR SHKZG INTO (L_NET_GR, L_SHKZG)
        FROM EKBEH
       WHERE EBELN = GT_MLDOC-EBELN
         AND EBELP = GT_MLDOC-EBELP
         AND ZEKKN = '00'
         AND VGABE = '1'
         AND GJAHR = GT_MLDOC-BDATJ
         AND BELNR = GT_MLDOC-AWREF
         AND BUZEI = GT_MLDOC-ZEILE.
    ENDIF.

    IF L_SHKZG = 'H'.
      L_NET_GR = - L_NET_GR.
    ENDIF.

    IF L_NETWR <> 0.
      L_LDC = 100 * L_DIF / L_NETWR.              " LDC Rate
    ENDIF.

* Get LDC Diff. : ML_GR(act.ldc) - Net_GR(est.ldc)
    F_LDC_AMT = ( GT_MLDOC-SALK3 + GT_MLDOC-PRDIF ) + - L_NET_GR.
    L_LDC_AMT2 = L_NET_GR * L_LDC / 100.             " Net GR

    MOVE-CORRESPONDING ITAB TO P_TAB.
    P_TAB-KZUST = C_LDC.
    P_TAB-CHNGE = F_LDC_AMT - L_LDC_AMT2.
    CLEAR: P_TAB-PRDIF.

    F_NET_AMT = L_NET_GR.  "p_tab-chnge.
    COLLECT P_TAB.  "APPEND
    CLEAR P_TAB.

  ENDIF.

ENDFORM.                    " GET_LDC
*&---------------------------------------------------------------------*
*&      Form  RANGES
*&---------------------------------------------------------------------*
*       Get Condition type & Val.Class
*----------------------------------------------------------------------*
FORM RANGES.
  S_HKONT-SIGN = 'I'.
  S_HKONT-OPTION = 'EQ'.
  S_HKONT-LOW = '0000530180'.
  APPEND S_HKONT.

  S_HKONT-SIGN = 'I'.
  S_HKONT-OPTION = 'EQ'.
  S_HKONT-LOW = '0000532100'.
  APPEND S_HKONT.

  R_KSCHL-SIGN = 'I'.
  R_KSCHL-OPTION = 'EQ'.
  R_KSCHL-LOW = 'PB00'. APPEND R_KSCHL.
  R_KSCHL-LOW = 'ZTIR'. APPEND R_KSCHL.

  S_BKLAS-SIGN = 'I'.
  S_BKLAS-OPTION = 'BT'.
  S_BKLAS-LOW  = '3000'.
  S_BKLAS-HIGH = '3005'.
  APPEND S_BKLAS.

ENDFORM.                    " RANGES
