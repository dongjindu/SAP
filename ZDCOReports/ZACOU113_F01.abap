*----------------------------------------------------------------------*
***INCLUDE ZACOU113_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Material Information
*----------------------------------------------------------------------*
FORM GET_DATA.
* Get BDC Options
  PERFORM GET_OPT USING 'N'.

* Get plant
  PERFORM GET_PLANT.

* Get LDC
  PERFORM GET_LDC.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  EXEC_MATL_COST
*&---------------------------------------------------------------------*
*       Excute Material Costing
*----------------------------------------------------------------------*
* 1. Change costing lot size : BAPI Funv. BAPI_MATERIAL_SAVEDATA
*      Costing lot size = info price unit
*      If failed, fill error log tables (error type = 1)
* 2. Create or change material cost estimate : CK11
*      Check costing result
*      If ZERO, fill error log tables (error type = 2)
* 3. Change Price : MR21
*    If failed, fill error log tables (error type = 3)
* 4. Update price table ZTCOU102
* 5. Add flag
*----------------------------------------------------------------------*
FORM EXEC_MATL_COST.
  DATA: L_IDX LIKE SY-TABIX.

  IF NOT GT_MAT[] IS INITIAL.
    CLEAR: GV_CNT, GV_SCNT, GV_ECNT.

    LOOP AT GT_MAT.
      L_IDX = SY-TABIX.
      GV_CNT = GV_CNT + 1.

*     no info record
      IF GT_MAT-LIFNR = SPACE.
        GT_MAT-STAT = C_STAT0.
        GT_MAT-EFLAG = 'E'.
        GT_MAT-MSG = 'Info record not exist.'.
      ELSE.
        PERFORM CHANGE_COSTING_LOT_SIZE.
        PERFORM CREATE_COST_ESTIMATE.
        PERFORM EXEC_MR21 USING 'S'.
        PERFORM INSERT_ZTCOU102.
      ENDIF.

      GT_MAT-AEDAT = SY-DATUM.
      GT_MAT-AENAM = SY-UNAME.

      MODIFY GT_MAT INDEX L_IDX FROM GT_MAT
             TRANSPORTING LIFNR KBETR STAT EFLAG MSG AEDAT AENAM.

      IF GT_MAT-EFLAG = 'E'.
        GV_ECNT = GV_ECNT + 1.
        PERFORM SAVE_LOG.

        MOVE-CORRESPONDING GT_MAT TO GT_OUT.
        APPEND GT_OUT.
        CLEAR GT_OUT.
      ELSE.
        GV_SCNT = GV_SCNT + 1.
        PERFORM DELETE_LOG.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " EXEC_MATL_COST
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COSTING_LOT_SIZE
*&---------------------------------------------------------------------*
*       Change costing lot size
*----------------------------------------------------------------------*
FORM CHANGE_COSTING_LOT_SIZE.
  DATA: WA_HEAD       LIKE BAPIMATHEAD,   " Header Segment
        WA_PLANT      LIKE BAPI_MARC,     " Material Data at Plant Level
        WA_PLANTX     LIKE BAPI_MARCX,    " Checkbox Strc. for BAPI_MARC
        WA_MBEW       LIKE BAPI_MBEW,
        WA_MBEWX      LIKE BAPI_MBEWX.

  CHECK GT_MAT-STAT IS INITIAL.

  GT_MAT-STAT  = C_STAT1.

  CHECK GT_MAT-PEINH <> GT_MAT-KPEIN.
  GT_MAT-PEINH = GT_MAT-KPEIN.

  CLEAR:  GT_BAPIRET, WA_HEAD, WA_PLANT, WA_PLANTX, WA_MBEW, WA_MBEWX.
  REFRESH GT_BAPIRET.

  WA_HEAD-MATERIAL     = GT_MAT-MATNR.     " Material
  WA_HEAD-COST_VIEW    = 'X'.              " [Costing view]

  WA_PLANT-LOT_SIZE    = GT_MAT-PEINH.     " Lot Size
  WA_PLANT-PLANT       = GT_MAT-WERKS.     " Plant

  WA_PLANTX-PLANT      = GT_MAT-WERKS.     " Plant
  WA_PLANTX-LOT_SIZE   = 'X'.              " Flag of Lot Size

  WA_MBEW-VAL_AREA     = GT_MAT-WERKS.
  WA_MBEWX-VAL_AREA    = GT_MAT-WERKS.

  WA_MBEW-PRICE_UNIT   = GT_MAT-PEINH.
  WA_MBEWX-PRICE_UNIT  = 'X'.

* Call BAPI for Create and Change Material Master Data
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            HEADDATA       = WA_HEAD
            PLANTDATA      = WA_PLANT
            PLANTDATAX     = WA_PLANTX
            VALUATIONDATA  = WA_MBEW
            VALUATIONDATAX = WA_MBEWX
       TABLES
            RETURNMESSAGES = GT_BAPIRET.

  IF SY-SUBRC = 0.
    PERFORM MASTER_UPDATE_SUCCESS.
  ELSE.
    PERFORM MASTER_UPDATE_FAILED.
  ENDIF.

ENDFORM.                    " CHANGE_COSTING_LOT_SIZE
*&---------------------------------------------------------------------*
*&      Form  MASTER_UPDATE_FAILED
*&---------------------------------------------------------------------*
*       Case of material master chahge failed
*----------------------------------------------------------------------*
FORM MASTER_UPDATE_FAILED.
  CLEAR GT_MAT-MSG.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  CONCATENATE C_STEP1 GT_BAPIRET-MESSAGE INTO GT_MAT-MSG
              SEPARATED BY SPACE.

  GT_MAT-STAT = C_STEP1.
  GT_MAT-EFLAG = 'E'.

ENDFORM.                    " MASTER_UPDATE_FAILED_RTN
*&---------------------------------------------------------------------*
*&      Form  MASTER_UPDATE_SUCCESS
*&---------------------------------------------------------------------*
*       Case of material master chahge success
*----------------------------------------------------------------------*
FORM MASTER_UPDATE_SUCCESS.
  CLEAR: GT_BAPIRET, GT_MAT-EFLAG, GT_MAT-MSG.

* Execute external Commit when using BAPIs
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT   = 'X'
       IMPORTING
            RETURN = GT_BAPIRET.

  IF GT_BAPIRET-TYPE = 'E'.
    CONCATENATE C_STEP1 GT_BAPIRET-MESSAGE INTO GT_MAT-MSG
                SEPARATED BY SPACE.
    GT_MAT-EFLAG = 'E'.
    CONCATENATE C_STEP1 GT_BAPIRET-MESSAGE INTO GT_MAT-MSG
                SEPARATED BY SPACE.

  ELSE.
    GT_MAT-EFLAG = 'S'.
  ENDIF.

ENDFORM.                    " MASTER_UPDATE_SUCCESS
*&---------------------------------------------------------------------*
*&      Form  EXEC_MR21
*&---------------------------------------------------------------------*
*       Create or change material cost estimate
*----------------------------------------------------------------------*
* Check table CKMLPP (join with CKMLHD)
* If Status(CKMLPP-STATUS)
*  :10 --> Call Transaction MR21
*   30 --> IF total quantity of GR(CKMLPP-ZUKUMO) = 0 and
*             issues not affecting the price(CKMLPP-VNKUMO) = 0,
*          Call Transaction MR21 using 'LTPC'.
* Change material cost estimate by plant, ML status
* grouping by No of material in document
*----------------------------------------------------------------------*
FORM EXEC_MR21  USING F_MODE TYPE C.
  DATA: LV_ZUKUMO TYPE CK_ZUKUM,
        LV_VNKUMO TYPE CK_VNKUM.

  IF P_TEST = 'X'.
    GT_MAT-EFLAG = 'E'.
  ENDIF.

  CHECK GT_MAT-EFLAG <> 'E' OR F_MODE = 'M'.

  GT_MAT-STAT  = C_STAT3.   " Change material cost estimate

* Check Status
  CLEAR: LV_ZUKUMO, LV_VNKUMO.
  SELECT SINGLE A~ZUKUMO A~VNKUMO
    INTO (LV_ZUKUMO, LV_VNKUMO)
    FROM CKMLPP AS A
    JOIN CKMLHD AS B
      ON B~KALNR = A~KALNR
   WHERE A~BDATJ = SY-DATUM+0(4)
     AND A~POPER = SY-DATUM+4(2)
     AND A~STATUS = '30'
     AND B~MATNR = GT_MAT-MATNR
     AND B~BWKEY = GT_MAT-WERKS.

* If the status is '30' and
* total quantity of GR(CKMLPP-ZUKUMO) = 0 and
* Issues not affecting the price(CKMLPP-VNKUMO) = 0,
* Price Change(Call Transaction MR21) using 'LTPC'.
  IF SY-SUBRC = 0 AND LV_ZUKUMO = 0 AND LV_VNKUMO = 0.
    PERFORM BDC_MR21 USING 'X'.
  ELSE.
    PERFORM BDC_MR21 USING SPACE.
  ENDIF.

  CALL TRANSACTION 'MR21'  USING         GT_BDC
                           OPTIONS FROM  GS_OPT
                           MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = 'S'
                             MSGID = 'CKPRCH'
                             MSGNR = '019'.

  IF SY-SUBRC = 0.
    CLEAR GT_MAT-MSG.
    GT_MAT-EFLAG = 'S'.
  ELSE.
    DATA L_MSG(100).

    CLEAR L_MSG.

    LOOP AT GT_MSG.
      PERFORM GET_MESSAGE_TEXT.

      IF SY-TABIX = 1.
        CONCATENATE  C_STEP3 GT_MAT-MSG INTO L_MSG
                                       SEPARATED BY SPACE.
      ELSE.
        CONCATENATE L_MSG GT_MAT-MSG INTO L_MSG
                   SEPARATED BY  ';'.
      ENDIF.
    ENDLOOP.

    GT_MAT-MSG = L_MSG.
    GT_MAT-EFLAG = 'E'.

  ENDIF.

ENDFORM.                                                    " EXEC_MR21
*&---------------------------------------------------------------------*
*&      Form  BDC_MR21
*&---------------------------------------------------------------------*
*       Transaction MR21 BDC Processing
*----------------------------------------------------------------------*
FORM BDC_MR21 USING P_CHK.
  DATA: LV_BUDAT(10),
        LV_DMAMT(15),
        LV_PEINH(5),
        LV_BIDAT LIKE SY-DATUM.

  CLEAR: GT_BDC, LV_BUDAT, LV_DMAMT, LV_PEINH.
  REFRESH GT_BDC.

  PERFORM CONVERT_DATE(ZACOU101) USING    SY-DATUM
                                 CHANGING LV_BUDAT.
  LV_DMAMT = GT_MAT-KBETR.
  LV_PEINH = GT_MAT-PEINH.

  PERFORM DYNPRO USING:
    'X'   'SAPRCKM_MR21'     '0201',
    ' '   'MR21HEAD-BUDAT'    LV_BUDAT,
    ' '   'MR21HEAD-BUKRS'    C_BUKRS,
    ' '   'MR21HEAD-WERKS'    GT_MAT-WERKS,
    ' '   'BDC_OKCODE'        '=ENTR'.

  IF P_CHK = 'X'.
    PERFORM DYNPRO USING:
      'X'   'SAPRCKM_MR21'   '0201',
      ' '   'BDC_OKCODE'     '=LTPC'.
  ENDIF.

  PERFORM DYNPRO USING:
    'X'   'SAPRCKM_MR21'                 '0201',
    ' '   'CKI_MR21_0250-MATNR(01)'      GT_MAT-MATNR,
    ' '   'CKI_MR21_0250-NEWVALPR(01)'   LV_DMAMT,
    ' '   'CKI_MR21_0250-NEWPEINH(01)'   LV_PEINH,
    ' '   'BDC_OKCODE'                   '=SAVE'.

ENDFORM.                                                    " BDC_MR21
*&---------------------------------------------------------------------*
*&      Form  CREATE_COST_ESTIMATE
*&---------------------------------------------------------------------*
FORM CREATE_COST_ESTIMATE.
  DATA: L_VALDT(10),
        L_DATE TYPE SYDATUM,
        L_CSTDT(10).

  CHECK GT_MAT-EFLAG <> 'E'.

  CLEAR: L_VALDT, L_DATE, L_CSTDT.

  PERFORM CONVERT_DATE(ZACOU101) USING    SY-DATUM
                                 CHANGING L_VALDT.

  CONCATENATE SY-DATUM+0(6) '01' INTO L_DATE.

  PERFORM CONVERT_DATE(ZACOU101) USING    L_DATE
                                 CHANGING L_CSTDT.

  GT_MAT-STAT  = C_STAT2. " Create Cost Estimate for Mat

  REFRESH: GT_BDC, GT_MSG.
  CLEAR  : GT_BDC, GT_MSG.

  PERFORM DYNPRO USING:
     'X'  'SAPLCKDI'               '0111',
     ' '  'CKI64A-KLVAR'           'ZR01',
     ' '  'CKI64A-MATNR'           GT_MAT-MATNR,
     ' '  'CKI64A-WERKS'           GT_MAT-WERKS,

     'X'  'SAPLCKDI'               '0400',
     ' '  'BDC_OKCODE'	           '=ENTR',
     ' '  'CKI64A-KADAT'	     L_CSTDT,
     ' '  'CKI64A-ALDAT'	     L_VALDT,
     ' '  'CKI64A-BWDAT'	     L_VALDT,

     'X'  'SAPLCKDI'               '2100',
     ' '  'BDC_OKCODE'             '=BUCA'.

  CALL TRANSACTION 'CK11'  USING         GT_BDC
                           OPTIONS FROM  GS_OPT
                           MESSAGES INTO GT_MSG.

* Get Meassage
  READ TABLE GT_MSG WITH KEY MSGTYP = 'S'
                             MSGID  = 'CK'
                             MSGNR  = '039'.

  IF SY-SUBRC = 0.
    GT_MAT-EFLAG = 'S'.
    CLEAR GT_MAT-MSG.
  ELSE.
    DELETE GT_MSG
      WHERE ( MSGID = 'CK' AND MSGNR = '037' ) "lot size
         OR ( MSGTYP = 'S' ).

    DATA L_MSG(100).
    CLEAR L_MSG.

    LOOP AT GT_MSG.
      PERFORM GET_MESSAGE_TEXT.

      IF SY-TABIX = 1.
        CONCATENATE  C_STEP3 GT_MAT-MSG INTO L_MSG
                                       SEPARATED BY SPACE.
      ELSE.
        CONCATENATE L_MSG GT_MAT-MSG INTO L_MSG
                   SEPARATED BY  ';'.
      ENDIF.
    ENDLOOP.

    GT_MAT-MSG = L_MSG.
    GT_MAT-EFLAG = 'E'.
  ENDIF.

ENDFORM.                    " CREATE_COST_ESTIMATE
*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE_TEXT
*&---------------------------------------------------------------------*
*       Get Message
*----------------------------------------------------------------------*
FORM GET_MESSAGE_TEXT.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = GT_MSG-MSGID
            MSGNR               = GT_MSG-MSGNR
            MSGV1               = GT_MSG-MSGV1
            MSGV2               = GT_MSG-MSGV2
            MSGV3               = GT_MSG-MSGV3
            MSGV4               = GT_MSG-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = GT_MAT-MSG.

ENDFORM.                    " GET_MESSAGE_TEXT
*&---------------------------------------------------------------------*
*&      Form  INSERT_ZTCOU102
*&---------------------------------------------------------------------*
*       Update Table ZTCOU102
*----------------------------------------------------------------------*
FORM INSERT_ZTCOU102.
  CHECK GT_MAT-EFLAG <> 'E'.

  ZTCOU102-KOKRS  = P_KOKRS.
  ZTCOU102-BDATJ  = SY-DATUM+0(4).
  ZTCOU102-POPER  = SY-DATUM+4(2).
  ZTCOU102-KALKA  = 'R1'.
  ZTCOU102-MATNR  = GT_MAT-MATNR.
  ZTCOU102-WERKS  = GT_MAT-WERKS.
  ZTCOU102-MTART  = GT_MAT-MTART.
  ZTCOU102-EKGRP  = GT_MAT-EKGRP.
  ZTCOU102-PROFL  = GT_MAT-PROFL.
  ZTCOU102-BKLAS  = GT_MAT-BKLAS.
  ZTCOU102-PEINH  = GT_MAT-PEINH.
  ZTCOU102-PMEHT  = GT_MAT-PMEHT.

  ZTCOU102-WERTN  = GT_MAT-KBETR.
*
  ZTCOU102-DUTY   = GT_MAT-NETPR * ( GT_MAT-DUTY / 100 ).
  ZTCOU102-FRG    = GT_MAT-FRG.
  ZTCOU102-OTH    = GT_MAT-OTH.

  ZTCOU102-GRSPR  = ZTCOU102-WERTN + ZTCOU102-DUTY +
                    ZTCOU102-FRG + ZTCOU102-OTH.

  ZTCOU102-LIFNR  = GT_MAT-LIFNR.
  ZTCOU102-QTA    = 100.
  ZTCOU102-KZUST1 = GT_MAT-KZUST.
  ZTCOU102-WERTN1 = GT_MAT-KBETR.
  ZTCOU102-AEDAT  = SY-DATUM.
  ZTCOU102-AENAM  = SY-UNAME.

  INSERT ZTCOU102.

  MESSAGE S000 WITH '*** Done ***'  GT_MAT-MATNR GT_MAT-KBETR.

ENDFORM.                    " INSERT_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  GET_GT_MAT
*&---------------------------------------------------------------------*
*       Get Material Information
*----------------------------------------------------------------------*
FORM GET_GT_MAT.

  CLEAR GT_MAT.
  REFRESH GT_MAT.

* ZTCOU102-PMEHT = GT_MAT-PMEHT.
  SELECT A~MATNR A~MTART A~PROFL A~MEINS
         B~WERKS B~EKGRP E~MAKTX
         F~BKLAS F~PEINH
         B~STAWN
*        S~KBETR AS DUTY
    INTO CORRESPONDING FIELDS OF TABLE GT_MAT
    FROM MARA AS A
    JOIN MARC AS B
      ON B~MATNR = A~MATNR
    JOIN MAKT AS E
      ON E~MATNR = A~MATNR
     AND E~SPRAS = SY-LANGU
    JOIN MBEW AS F
      ON F~MATNR = B~MATNR
     AND F~BWKEY = B~WERKS
    INNER JOIN CKMLHD AS K
       ON K~MATNR = F~MATNR
      AND K~BWKEY = F~BWKEY

   WHERE A~MATNR IN S_MATNR
     AND A~MTART IN S_MTART
     AND A~LVORM <> 'X'       " Not deleted
     AND A~MSTAE IN S_MMSTA
     AND B~LVORM <> 'X'       " Not deleted
     AND B~DISPO IN S_DISPO
     AND B~BESKZ IN S_BESKZ   " F-default
     AND B~WERKS IN R_WERKS
     AND B~MMSTA IN S_MMSTA
     AND F~LVORM <> 'X'       " Not deleted
     AND F~LBKUM   = 0        " NO inventory
     AND F~STPRS   = 0
     AND K~XABRECH = SPACE
     AND K~MLAST   = '3'.

  IF SY-SUBRC = 0.
    PERFORM MODIFY_GT_MAT.
  ENDIF.

  SORT GT_MAT BY MATNR WERKS.

ENDFORM.                    " GET_GT_MAT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_MAT
*&---------------------------------------------------------------------*
*       Get Other Information : Vendor, Price, Reason...
*----------------------------------------------------------------------*
FORM MODIFY_GT_MAT.
  DATA: LV_KNUMH TYPE KNUMH,       " Condition No
        L_LAND1  TYPE LAND1_GP,
        L_ZOTH   TYPE ZOTH,
        L_ZOTI   TYPE ZOTI,
        L_IDX    LIKE SY-TABIX.

  CLEAR:   GT_EINA, GT_A018, LV_KNUMH.
  REFRESH: GT_EINA, GT_A018.

* Purchasing Info Record
  SELECT INFNR MATNR LIFNR
    INTO TABLE GT_EINA
    FROM EINA
     FOR ALL ENTRIES IN GT_MAT
   WHERE MATNR = GT_MAT-MATNR.

  SORT GT_EINA BY MATNR LIFNR.
  DELETE ADJACENT DUPLICATES FROM GT_EINA.

  IF NOT GT_EINA[] IS INITIAL.
* Material Info Record
    SELECT LIFNR MATNR DATBI DATAB KNUMH
      INTO TABLE GT_A018
      FROM A018
       FOR ALL ENTRIES IN GT_EINA
     WHERE KAPPL = 'M'                " Purchasing
       AND KSCHL = 'PB00'             " Gross price
       AND A018~LIFNR = GT_EINA-LIFNR
       AND MATNR = GT_EINA-MATNR
       AND ESOKZ = '0'                " Standard
       AND DATAB =< SY-DATUM
       AND DATBI >= SY-DATUM.
  ENDIF.

  SORT: GT_EINA BY MATNR LIFNR,
        GT_A018 ASCENDING BY MATNR DESCENDING DATAB.

* Modify internal table GT_MAT
  LOOP AT GT_MAT.
    L_IDX = SY-TABIX.

    GT_MAT-KOKRS = P_KOKRS.            " Controlling Area
    GT_MAT-KALKA = 'R1'.               " Costing type : Standard

    IF GT_MAT-STAWN <> SPACE.
      SELECT SINGLE S~KBETR INTO GT_MAT-DUTY
        FROM A902 AS T
        JOIN KONP AS S
          ON S~KNUMH =  T~KNUMH
        WHERE T~KAPPL = 'M'
          AND T~KSCHL = 'ZOA1'
          AND T~STAWN =  GT_MAT-STAWN.

*   FIXME...
      GT_MAT-DUTY = GT_MAT-DUTY / 10.
    ENDIF.

    READ TABLE GT_A018 WITH KEY MATNR = GT_MAT-MATNR.
    IF SY-SUBRC = 0.
      READ TABLE GT_EINA WITH KEY MATNR = GT_MAT-MATNR
                                  LIFNR = GT_A018-LIFNR
                              BINARY SEARCH.

      IF SY-SUBRC = 0.
        GT_MAT-INFNR = GT_EINA-INFNR.   " Info Record No.
      ENDIF.
      GT_MAT-LIFNR = GT_A018-LIFNR.
      LV_KNUMH  = GT_A018-KNUMH.        " Condition No.

*     Info Price
      SELECT SINGLE KZUST AVG( KPEIN ) SUM( KBETR )
        INTO (GT_MAT-KZUST, GT_MAT-KPEIN, GT_MAT-NETPR)
        FROM KONP
        INNER JOIN KONH
           ON KONH~KNUMH = KONP~KNUMH
       WHERE KONP~KNUMH = LV_KNUMH
         AND KONP~KAPPL = 'M'
         AND KONP~KSCHL IN R_KSCHL
       GROUP BY KZUST.

*     LDC check
      CLEAR L_LAND1.
      SELECT SINGLE LAND1 INTO L_LAND1 FROM LFA1
             WHERE LIFNR = GT_A018-LIFNR.


      READ TABLE GT_116 WITH KEY MATNR = GT_MAT-MATNR.
      IF SY-SUBRC <> 0.
        READ TABLE GT_116 WITH KEY LAND1 = L_LAND1.
      ENDIF.

      IF SY-SUBRC = 0.
        CLEAR: L_ZOTH, L_ZOTI.

*       Freight
        IF GT_116-FRA1 <> 0.
          GT_MAT-FRG = GT_MAT-NETPR * ( GT_116-FRA1 / 100 ).
        ENDIF.

*       Others
        IF GT_116-ZOTH <> 0.
          L_ZOTH = GT_MAT-NETPR * ( GT_116-ZOTH / 100 ).
        ENDIF.

        IF GT_116-ZOTI <> 0.
          L_ZOTI = GT_MAT-NETPR * ( GT_116-ZOTI / 100 ).
        ENDIF.

        GT_MAT-OTH = L_ZOTH + L_ZOTI.

*       Price
        GT_MAT-KBETR =
             GT_MAT-NETPR * ( 1 + ( GT_MAT-DUTY + GT_116-FRA1 +
                                GT_116-ZOTH + GT_116-ZOTI ) / 100 ).
      ELSE.
        GT_MAT-KBETR = GT_MAT-NETPR.
      ENDIF.
    ENDIF.

    MODIFY GT_MAT INDEX L_IDX FROM GT_MAT
        TRANSPORTING KOKRS KALKA LIFNR INFNR KZUST
                     KBETR NETPR DUTY OTH KPEIN.

  ENDLOOP.

ENDFORM.                    " MODIFY_GT_MAT
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
*       Save Log Table ZTCOU113
*----------------------------------------------------------------------*
FORM SAVE_LOG.
  SELECT COUNT(*) INTO SY-INDEX
         FROM ZTCOU113 WHERE KOKRS = GT_MAT-KOKRS
                         AND MATNR = GT_MAT-MATNR
                         AND WERKS = GT_MAT-WERKS.

  IF SY-SUBRC <> 0.
    INSERT INTO ZTCOU113 VALUES GT_MAT.

  ELSE.
    UPDATE ZTCOU113 SET LIFNR = GT_MAT-LIFNR
                        STAT  = GT_MAT-STAT
                        EFLAG = GT_MAT-EFLAG
                        MSG   = GT_MAT-MSG
                        AEDAT = SY-DATUM
                        AENAM = SY-UNAME
                  WHERE KOKRS = GT_MAT-KOKRS
                    AND MATNR = GT_MAT-MATNR
                    AND WERKS = GT_MAT-WERKS.
  ENDIF.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Results
*----------------------------------------------------------------------*
FORM DISP_RESULT.
  IF GV_ECNT = 0.
    WRITE: '* Processing result',
           / '  Total  :',  GV_CNT,
           / '  Succees:',  GV_SCNT,
           / '  Fail   :',  GV_ECNT.
  ELSE.
    CALL SCREEN 100.
  ENDIF.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  GET_LOG
*&---------------------------------------------------------------------*
*       Select Log
*----------------------------------------------------------------------*
FORM GET_LOG.
* Get log from table ZTCOU113
  CLEAR GT_MAT.
  REFRESH GT_MAT.

  SELECT KOKRS A~MATNR WERKS KALKA MTART MEINS
         EKGRP PROFL BKLAS INFNR KBETR   PEINH LIFNR KZUST
         STAT  EFLAG MSG   AEDAT A~AENAM B~MAKTX
    INTO CORRESPONDING FIELDS OF TABLE GT_MAT
    FROM ZTCOU113 AS A
    JOIN MAKT AS B
      ON B~MATNR = A~MATNR
     AND B~SPRAS = SY-LANGU
   WHERE A~MATNR IN S_MATNR
     AND WERKS   IN R_WERKS
     AND STAT = '0'.

  IF SY-SUBRC = 0.
    PERFORM MODIFY_GT_MAT.
  ENDIF.

  SELECT KOKRS A~MATNR WERKS KALKA MTART MEINS
         EKGRP PROFL BKLAS INFNR KBETR   PEINH LIFNR KZUST
         STAT  EFLAG MSG   AEDAT A~AENAM B~MAKTX
    APPENDING CORRESPONDING FIELDS OF TABLE GT_MAT
    FROM ZTCOU113 AS A
    JOIN MAKT AS B
      ON B~MATNR = A~MATNR
     AND B~SPRAS = SY-LANGU
   WHERE A~MATNR IN S_MATNR
     AND WERKS   IN R_WERKS
     AND STAT <> '0'.

  SORT GT_MAT BY MATNR WERKS.
  PERFORM GET_GT_OUT.

ENDFORM.                    " GET_LOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*       Create ALV Control
*----------------------------------------------------------------------*
FORM CREATE_ALV_CONTROL.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CLEAR: GS_LAYO, GT_EXCLUDE, GS_VARIANT, GT_FCAT[].

*   Create object
    PERFORM CREATE_OBJECT.

*   Exclude toolbar
    PERFORM EXCLUDE_FUNCTIONS USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM CREATE_FIELD_CATEGORY.

*   Setting for layout
    GS_LAYO-EDIT       = 'X'.
    GS_LAYO-CWIDTH_OPT = 'X'.
    GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
    GS_LAYO-STYLEFNAME = 'CELLTAB'.

*   Define editable field
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
         EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*   Setting for event
    CREATE OBJECT G_EVENT_RECEIVER.
    SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID.

*   Define cell attribute
    PERFORM BUILD_CELL_ATTR.

*   Define variant
    GS_VARIANT-REPORT = SY-REPID.

*   Display alv grid
    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING IS_LAYOUT            = GS_LAYO
                   IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
                   I_SAVE               = GC_VAR_SAVE
                   IS_VARIANT           = GS_VARIANT
         CHANGING  IT_OUTTAB            = GT_OUT[]
                   IT_FIELDCATALOG      = GT_FCAT[].

  ENDIF.

ENDFORM.                    " CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Exclude function code
*----------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS USING P_TABNAME.
  PERFORM APPEND_EXCLUDE_FUNCTIONS
          TABLES GT_EXCLUDE[]
           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create Field Category
*----------------------------------------------------------------------*
FORM CREATE_FIELD_CATEGORY.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZTCOU113'
       CHANGING
            CT_FIELDCAT      = GT_FCAT.

  PERFORM MODIFY_GT_FCAT.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_FCAT
*&---------------------------------------------------------------------*
*       Modify Field Category
*----------------------------------------------------------------------*
FORM MODIFY_GT_FCAT.
  DATA LV_CNT TYPE I.

  CLEAR LV_CNT.
  DELETE GT_FCAT WHERE FIELDNAME = 'MANDT'
                    OR FIELDNAME = 'KOKRS'
                    OR FIELDNAME = 'KALKA'.


  LOOP AT GT_FCAT INTO GS_FCAT.
    IF GS_FCAT-COL_POS < 7.
      GS_FCAT-COL_POS = GS_FCAT-COL_POS - 1.
    ENDIF.

    CASE GS_FCAT-FIELDNAME.
      WHEN 'KALKA'.
        GS_FCAT-COLTEXT = 'CstTy'.
      WHEN 'MTART'.
        GS_FCAT-COLTEXT = 'Mat.Typ'.
      WHEN 'EKGRP'.
        GS_FCAT-COLTEXT = 'P.Grp'.
      WHEN 'PROFL'.
        GS_FCAT-COLTEXT = 'Src'.
      WHEN 'LIFNR'.
        GS_FCAT1-F4AVAILABL = 'X'.
      WHEN 'KBETR'.
        GS_FCAT-COLTEXT = 'Gross Price'.
      WHEN 'PEINH'.
        GS_FCAT-COLTEXT = 'Price Unit'.
      WHEN 'KZUST'.
        GS_FCAT-COLTEXT = 'Reason'.
      WHEN 'FLAG'.
        GS_FCAT-COLTEXT = 'Flag'.
      WHEN 'DUTY'.
        GS_FCAT-COLTEXT = 'Duty%'.
    ENDCASE.

    MODIFY GT_FCAT FROM GS_FCAT.
  ENDLOOP.

  PERFORM FILL_FIELD_CATEGORY USING
      6   'MAKTX'     'Desc.'      50  'CHAR'.

ENDFORM.                    " MODIFY_GT_FCAT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE,
        L_FIELD TYPE LVC_FNAME.

  FIELD-SYMBOLS <FS> TYPE ANY.

  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_OUT INDEX LS_MOD_CELLS-ROW_ID.

    IF SY-SUBRC = 0.
      IF LS_MOD_CELLS-FIELDNAME = 'LIFNR'  OR
         LS_MOD_CELLS-FIELDNAME = 'KBETR'.
*        ls_mod_cells-fieldname = 'PEINH'.

        CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
               EXPORTING I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                         I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                         I_VALUE     = LS_MOD_CELLS-VALUE.
        MODIFY GT_MAT INDEX LS_MOD_CELLS-ROW_ID.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
FORM BUILD_CELL_ATTR.
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR: LT_CELLTAB, GS_FCAT.
  REFRESH LT_CELLTAB.

  LOOP AT GT_FCAT INTO GS_FCAT.
    LS_CELLTAB-FIELDNAME = GS_FCAT-FIELDNAME.

    IF LS_CELLTAB-FIELDNAME = 'LIFNR'  OR
       LS_CELLTAB-FIELDNAME = 'KBETR'.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  CLEAR GT_OUT-CELLTAB.
  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE CELLTAB IS INITIAL.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  CLEAR GT_OUT.
  REFRESH GT_OUT.

  LOOP AT GT_MAT.
    MOVE-CORRESPONDING GT_MAT TO GT_OUT.
    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTCOU102
*&---------------------------------------------------------------------*
FORM SAVE_ZTCOU102.
  DATA: LV_WERTN TYPE ZWERTN1,
        LV_DUTY  TYPE ZDUTY1,
        LV_FRG   TYPE ZFRG1,
        LV_OTH   TYPE ZOTH1,
        LV_DRATE TYPE ZWERTN1,
        LV_FRATE TYPE ZWERTN1,
        LV_ORATE TYPE ZWERTN1,
        LV_CNT(5),
        LV_MSG(50).

  CLEAR: GT_ROW[], GT_ROID[], LV_CNT, LV_MSG.

* Get selected rows
  CALL METHOD G_GRID->GET_SELECTED_ROWS
              IMPORTING ET_INDEX_ROWS = GT_ROW
                        ET_ROW_NO = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      CLEAR: LV_WERTN, LV_DUTY, LV_FRG, LV_OTH,
             LV_DRATE, LV_FRATE, LV_ORATE.

      SELECT SINGLE WERTN DUTY FRG OTH
        INTO (LV_WERTN, LV_DUTY, LV_FRG, LV_OTH)
        FROM ZTCOU102
       WHERE KOKRS = GT_OUT-KOKRS
         AND MATNR = GT_OUT-MATNR
         AND WERKS = GT_OUT-WERKS.

      IF LV_DUTY <> 0 AND LV_WERTN <> 0.
        LV_DRATE = LV_DUTY / LV_WERTN.
      ENDIF.

      IF LV_FRG <> 0 AND LV_WERTN <> 0.
        LV_FRATE = LV_FRG / LV_WERTN.
      ENDIF.

      IF LV_OTH <> 0 AND LV_WERTN <> 0.
        LV_ORATE = LV_OTH / LV_WERTN.
      ENDIF.

      UPDATE ZTCOU102 SET LIFNR = GT_OUT-LIFNR
                          WERTN = GT_OUT-KBETR
                          DUTY = LV_DUTY
                          FRG = LV_FRG
                          OTH = LV_OTH
                    WHERE KOKRS = GT_OUT-KOKRS
                      AND MATNR = GT_OUT-MATNR
                      AND WERKS = GT_OUT-WERKS
                      AND ZLOCK NE 'X'.

      IF SY-SUBRC = 0.
        LV_CNT = LV_CNT + 1.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF LV_CNT IS INITIAL.
    LV_CNT = '0'.
  ENDIF.

  CONDENSE LV_CNT.
  CONCATENATE 'Save to table ZTCOU102:' LV_CNT 'records.' INTO LV_MSG.
  MESSAGE S000 WITH LV_MSG.

ENDFORM.                    " SAVE_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  CALL_PRICE_HISTORY
*&---------------------------------------------------------------------*
*       Get Price History
*----------------------------------------------------------------------*
FORM CALL_PRICE_HISTORY.
  CLEAR: GT_ROW[], GT_ROID[], R_MATNR.
  REFRESH R_MATNR.

* Get selected rows
  CALL METHOD G_GRID->GET_SELECTED_ROWS
              IMPORTING ET_INDEX_ROWS = GT_ROW
                        ET_ROW_NO = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      R_MATNR-SIGN = 'I'.
      R_MATNR-OPTION = 'EQ'.
      R_MATNR-LOW = GT_OUT-MATNR.

      APPEND R_MATNR.
      CLEAR R_MATNR.
    ENDIF.
  ENDLOOP.

  IF NOT R_MATNR[] IS INITIAL.
    SUBMIT ZACOU112 WITH S_MATNR IN R_MATNR AND RETURN.
  ENDIF.

ENDFORM.                    " CALL_PRICE_HISTORY
*&---------------------------------------------------------------------*
*&      Form  REPROCESSING
*&---------------------------------------------------------------------*
*       Reprocessing
*----------------------------------------------------------------------*
*  1. Delete Log Table
*  2. Process remaining steps
*----------------------------------------------------------------------*
FORM REPROCESSING.
  DATA: L_IDX    LIKE SY-TABIX,
        LW_GT_MAT LIKE GT_MAT.

  CLEAR: GT_ROW[], GT_ROID[],
         GV_CNT, GV_SCNT, GV_ECNT.

* Get selected rows
  CALL METHOD G_GRID->GET_SELECTED_ROWS
              IMPORTING ET_INDEX_ROWS = GT_ROW
                        ET_ROW_NO = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.
    CHECK SY-SUBRC = 0.

*  1. Process remaining steps
    CLEAR GT_MAT.
    MOVE-CORRESPONDING GT_OUT TO GT_MAT.
    CLEAR: GT_MAT-EFLAG.

    CASE GT_OUT-STAT.
      WHEN C_STAT0 OR C_STAT1.
        PERFORM CHANGE_COSTING_LOT_SIZE.
        PERFORM CREATE_COST_ESTIMATE.
        PERFORM EXEC_MR21 USING 'S'.

      WHEN C_STAT2.
        PERFORM CREATE_COST_ESTIMATE.
        PERFORM EXEC_MR21 USING 'S'.

      WHEN C_STAT3.
        PERFORM EXEC_MR21 USING 'S'.
    ENDCASE.

    GT_MAT-AEDAT = SY-DATUM.
    GT_MAT-AENAM = SY-UNAME.

    READ TABLE GT_MAT INTO LW_GT_MAT
                      WITH KEY MATNR = GT_OUT-MATNR
                               WERKS = GT_OUT-WERKS
                      BINARY SEARCH.
    L_IDX = SY-TABIX.
    MODIFY GT_MAT INDEX L_IDX
           TRANSPORTING LIFNR KBETR STAT EFLAG MSG AEDAT AENAM.

    IF GT_MAT-EFLAG = 'E'.
      GV_ECNT = GV_ECNT + 1.
      PERFORM SAVE_LOG.

      GT_OUT-DEL = 'X'.
      MODIFY GT_OUT INDEX GS_ROW-INDEX TRANSPORTING DEL.
      MESSAGE S000 WITH '*** Error...'  GT_OUT-MATNR.

      CLEAR GT_OUT.
      MOVE-CORRESPONDING GT_MAT TO GT_OUT.
      APPEND GT_OUT.
      CLEAR GT_OUT.
    ELSE.
      GV_SCNT = GV_SCNT + 1.
      PERFORM INSERT_ZTCOU102.         " Update Table ZTCOU102
      PERFORM DELETE_LOG.

      GT_OUT-DEL = 'X'.
      MODIFY GT_OUT INDEX GS_ROW-INDEX TRANSPORTING DEL.
      MESSAGE S000 WITH 'Reprocessed...'  GT_OUT-MATNR.
    ENDIF.

  ENDLOOP.

  DELETE GT_OUT WHERE DEL = 'X'.
  PERFORM REFRESH_FIELD.

ENDFORM.                    " REPROCESSING
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
*       Get Field Category for price history
*----------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY1 USING P_POS   TYPE LVC_COLPOS
                                P_FNAME TYPE LVC_FNAME
                                P_TXT   TYPE LVC_TXTCOL
                                P_LEN   TYPE LVC_OUTLEN
                                P_TYPE  TYPE DATATYPE_D.

  CLEAR GS_FCAT1.

  GS_FCAT1-COL_POS   = P_POS.     " Column position
  GS_FCAT1-FIELDNAME = P_FNAME.   " Field name
  GS_FCAT1-COLTEXT   = P_TXT.     " Column heading
  GS_FCAT1-OUTPUTLEN = P_LEN.     " Column width
  GS_FCAT1-DATATYPE  = P_TYPE.    " Data type

  APPEND GS_FCAT1 TO GT_FCAT1.

ENDFORM.                    " FILL_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       Get Plant by Controlling area
*----------------------------------------------------------------------*
FORM GET_PLANT.
  TYPES: BEGIN OF TY_PLANT,
           BWKEY TYPE BWKEY,
         END OF TY_PLANT.

  DATA LT_PLANT TYPE TABLE OF TY_PLANT WITH HEADER LINE.

  CLEAR: LT_PLANT, R_WERKS.
  REFRESH: LT_PLANT, R_WERKS.

  SELECT BWKEY INTO TABLE LT_PLANT
    FROM T001K AS A
    JOIN TKA02 AS B
      ON B~BUKRS = A~BUKRS
   WHERE B~KOKRS = P_KOKRS.

  R_WERKS-SIGN = 'I'.
  R_WERKS-OPTION = 'EQ'.

  LOOP AT LT_PLANT.
    R_WERKS-LOW = LT_PLANT-BWKEY.
    APPEND R_WERKS.
  ENDLOOP.

ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  get_ldc
*&---------------------------------------------------------------------*
*       Get LDC
*----------------------------------------------------------------------*
FORM GET_LDC.
  CLEAR GT_116.
  REFRESH GT_116.

  SELECT * INTO TABLE GT_116
    FROM ZTCOU116
   WHERE KOKRS = P_KOKRS
     AND BDATJ = SY-DATUM+0(4)
     AND VER = 0.

ENDFORM.                    " get_ldc
