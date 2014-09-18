REPORT ZCO_ZXCKAU03 .

PARAMETERS: P_KLVAR LIKE KEKO-KLVAR,
            P_WERKS LIKE KEKO-WERKS,
            P_MATNR LIKE KEKO-MATNR,
            P_VERID LIKE KEKO-VERID,
            P_KADKY LIKE KEKO-KADKY.

PARAMETERS: P_SAVE AS CHECKBOX DEFAULT 'X'.

*---- ALV
TYPE-POOLS: SLIS.
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV,
      G_REPID     LIKE SY-REPID.
*---- ALV

*-------------------------------------------------------------
INCLUDE ZXCKAU00_DATA.
*-------------------------------------------------------------

DATA: F_CKIUSER LIKE CKIUSER.
DATA: T_KIS1  LIKE KIS1 OCCURS 0 WITH HEADER LINE.

DATA: LT_KEKO  LIKE KEKO OCCURS 0 WITH HEADER LINE.
DATA: LT_CKIS  LIKE CKIS OCCURS 0 WITH HEADER LINE.
*DATA: W_CKIS  LIKE CKIS.
*DATA: W_UKALN LIKE CKIS-UKALN.
*DATA: W_WERTN LIKE KIS1-WERTN.
*-------------------------------------------------------------
*
*Refer KEKO for Header, CKIS for Line
*

*FSC/MIP
SELECT SINGLE * INTO CORRESPONDING FIELDS OF F_CKIUSER
     FROM KEKO
     WHERE MATNR = P_MATNR
       AND WERKS = P_WERKS
       AND KLVAR = P_KLVAR
       AND KADKY = P_KADKY.
IF F_CKIUSER-SOBES = '7'. "Stock Trf.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF F_CKIUSER
       FROM KEKO
       WHERE MATNR = P_MATNR
         AND WERKS = F_CKIUSER-SOWRK
         AND KLVAR = P_KLVAR
         AND KADKY = P_KADKY.
ENDIF.

CHECK SY-SUBRC = 0.
*FSC Item
SELECT * INTO CORRESPONDING FIELDS OF TABLE T_KIS1
     FROM CKIS
     WHERE LEDNR = '00'             "Standard ledger
       AND BZOBJ = F_CKIUSER-BZOBJ
       AND KALNR = F_CKIUSER-KALNR
       AND KALKA = F_CKIUSER-KALKA
       AND KADKY = F_CKIUSER-KADKY
       AND BWVAR = F_CKIUSER-BWVAR
       and tvers = '01'.


*Component (inc. FSC)
SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_KEKO
     FROM KEKO
     FOR ALL ENTRIES IN T_KIS1
     WHERE BZOBJ = F_CKIUSER-BZOBJ
       AND KALNR = T_KIS1-UKALN
       AND KALKA = F_CKIUSER-KALKA
       AND KADKY = P_KADKY
       AND TVERS = F_CKIUSER-TVERS
       AND BWVAR = F_CKIUSER-BWVAR
       AND KLVAR = P_KLVAR
       and tvers = '01'.
SORT LT_KEKO BY KALNR.

*Component Item (exc. FSC)
SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_CKIS
     FROM CKIS
     FOR ALL ENTRIES IN LT_KEKO
     WHERE LEDNR = '00'             "Standard ledger
       AND BZOBJ = F_CKIUSER-BZOBJ
       AND ( KALNR <> F_CKIUSER-KALNR AND KALNR = LT_KEKO-KALNR )
       AND KALKA = F_CKIUSER-KALKA
       AND KADKY = P_KADKY
       AND TVERS = F_CKIUSER-TVERS
       AND BWVAR = F_CKIUSER-BWVAR
       and tvers = '01'.
SORT LT_CKIS BY KALNR HRKFT.


*if single item costing...
DESCRIBE TABLE T_KIS1 LINES SY-INDEX.
IF SY-INDEX = 0. APPEND T_KIS1. ENDIF.

*get 1st level BOM
SELECT IDNRK MAKTX INTO CORRESPONDING FIELDS OF TABLE LT_UPG
  FROM STPO AS S
  INNER JOIN MAKT AS T
     ON T~MATNR = S~IDNRK
    AND T~SPRAS = SY-LANGU
  WHERE STLTY = 'M'
    AND STLNR =  F_CKIUSER-STNUM
    AND DATUV <= F_CKIUSER-ALDAT.

CHECK SY-SUBRC = 0.

*read BOM explosion
*DATA L_CAPID TYPE CAPID.

CLEAR L_CAPID.
SELECT SINGLE A~CAPID INTO L_CAPID
  FROM TCK19A AS A
  JOIN TCK03 AS B
    ON B~AUFKZ = A~AUFKZ
 WHERE B~KLVAR = F_CKIUSER-KLVAR
   AND B~KALKA = F_CKIUSER-KALKA
   AND B~BWVAR = F_CKIUSER-BWVAR.

CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
     EXPORTING
          CAPID = L_CAPID
          DATUV = F_CKIUSER-ALDAT
          MTNRV = F_CKIUSER-MATNR
          WERKS = F_CKIUSER-WERKS
          STLAN = F_CKIUSER-STLAN
          STLAL = F_CKIUSER-STALT
          MEHRS = 'X'  "Multi-level explosion
          MMORY = '1'  "Memory use On(1)
          SANKA = 'X'  "Only Costing Relevency(inc.Phantom)
     TABLES
          STB   = STB.
LOOP AT STB.
  MOVE-CORRESPONDING STB TO LT_BOM. APPEND LT_BOM.
ENDLOOP.

*fill Itemization
LOOP AT T_KIS1 WHERE TYPPS = 'M'.
  CLEAR ITAB.
  MOVE-CORRESPONDING T_KIS1 TO ITAB.

  ITAB-KOKRS  = F_CKIUSER-KOKRS.
  ITAB-KLVAR  = F_CKIUSER-KLVAR.
  ITAB-ARTNR  = F_CKIUSER-MATNR.
  ITAB-WERKS  = F_CKIUSER-WERKS.
  ITAB-BDATJ  = F_CKIUSER-BDATJ.
  ITAB-POPER  = F_CKIUSER-POPER.
  ITAB-VERID  = F_CKIUSER-VERID. "Production version

  ITAB-BWDAT  = F_CKIUSER-BWDAT. "Val.date
  ITAB-ALDAT  = F_CKIUSER-ALDAT. "Qty.date
  ITAB-STALT  = F_CKIUSER-STALT. "Alt.BOM

  ITAB-COMPN = T_KIS1-MATNR.
  ITAB-REQQT = T_KIS1-MENGE.

*For Debugging------------------
*  TABLES: ztfi_ctl.
  SELECT COUNT( * ) INTO SY-DBCNT FROM ZTFI_CTL
    WHERE CATEG = 'ZXCKAU03' AND FLAG = 'X' AND ZUONR = T_KIS1-MATNR.
  IF SY-SUBRC = 0.
    BREAK-POINT.
  ENDIF.
*For Debugging------------------

* get component detail
  IF NOT T_KIS1-UKALN IS INITIAL.
    W_UKALN = T_KIS1-UKALN.

    READ TABLE LT_KEKO WITH KEY KALNR = W_UKALN.
    ITAB-SPLNT = LT_KEKO-WERKS.  "Def.Supply plant

*---Stock transfer, get source plant data
    IF LT_KEKO-SOBES = '7'. "Stock Trf
      ITAB-SPLNT = LT_KEKO-SOWRK.   "Trf From Plant

*.... get detail info from supply plant
      READ TABLE LT_CKIS INTO W_CKIS WITH KEY KALNR = T_KIS1-UKALN.
      W_UKALN = W_CKIS-UKALN.
      ITAB-STRAT = W_CKIS-STRAT.   "Pricing Strategy

      READ TABLE LT_KEKO WITH KEY KALNR = W_UKALN.
*---- select missing items
      IF SY-SUBRC <> 0.
        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE LT_KEKO
             FROM KEKO
             WHERE BZOBJ = F_CKIUSER-BZOBJ
               AND KALNR = W_UKALN
               AND KALKA = F_CKIUSER-KALKA
               AND KADKY = F_CKIUSER-KADKY
               AND TVERS = F_CKIUSER-TVERS
               AND BWVAR = F_CKIUSER-BWVAR
               AND KLVAR = F_CKIUSER-KLVAR
               AND WERKS = ITAB-SPLNT.
        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE LT_CKIS
             FROM CKIS
             WHERE LEDNR = '00'             "Standard ledger
               AND BZOBJ = F_CKIUSER-BZOBJ
               AND KALNR = W_UKALN
               AND KALKA = F_CKIUSER-KALKA
               AND KADKY = F_CKIUSER-KADKY
               AND TVERS = F_CKIUSER-TVERS
               AND BWVAR = F_CKIUSER-BWVAR.
        READ TABLE LT_KEKO WITH KEY KALNR = W_UKALN.
      ENDIF.
    ENDIF.

*---if end-item, get price detail..(EndItem - BOM usage blank)
    IF LT_KEKO-STLAN IS INITIAL.

      CLEAR W_WERTN.
      LOOP AT LT_CKIS WHERE KALNR = W_UKALN.
        IF LT_CKIS-LIFNR <> SPACE.
          ITAB-LIFNR = LT_CKIS-LIFNR.
          ITAB-INFNR = LT_CKIS-INFNR.
        ENDIF.
*-------- LDC rate / price unit (no BOM qty)
        CASE LT_CKIS-HRKFT.
          WHEN 'KD-D'.
            ITAB-AMTDT = T_KIS1-MENGE * LT_CKIS-WERTN / LT_CKIS-PEINH.
          WHEN 'KD-F'.
            ITAB-AMTFT = T_KIS1-MENGE * LT_CKIS-WERTN / LT_CKIS-PEINH.
          WHEN 'KD-O'.
            ITAB-AMTOT = T_KIS1-MENGE * LT_CKIS-WERTN / LT_CKIS-PEINH.
          WHEN OTHERS.
        ENDCASE.

        W_WERTN   = W_WERTN + LT_CKIS-WERTN.
      ENDLOOP.

*---- exeption case...why?..unknown.
      IF ITAB-WERTN = 0.
        ITAB-WERTN  = ITAB-REQQT * W_WERTN / LT_CKIS-PEINH.
        ITAB-GPREIS = W_WERTN.
      ENDIF.
    ENDIF.

  ENDIF.

  APPEND ITAB.
ENDLOOP.

* get vendor name
SELECT LIFNR NAME1 INTO CORRESPONDING FIELDS OF TABLE T_LFA1
                   FROM LFA1
                   FOR ALL ENTRIES IN ITAB
                   WHERE LIFNR = ITAB-LIFNR.
SORT T_LFA1 BY LIFNR.


* get Item info
*DATA L_INDEX TYPE SYTABIX.

SORT ITAB BY COMPN POSNR.

LOOP AT LT_BOM WHERE DUMPS = SPACE.
  READ TABLE ITAB WITH KEY COMPN = LT_BOM-IDNRK
                           UPGVC = SPACE
                           INDX = 0
                           CHK = SPACE.

  IF SY-SUBRC = 0.
    L_INDEX = SY-TABIX.

    MOVE-CORRESPONDING LT_BOM TO ITAB.
    ITAB-INDX  = LT_BOM-INDEX.
    ITAB-CHK = 'X'.

    READ TABLE T_LFA1 WITH KEY LIFNR = ITAB-LIFNR.
    IF SY-SUBRC = 0.
      ITAB-NAME1 = T_LFA1-NAME1.
    ENDIF.

    MODIFY ITAB INDEX L_INDEX TRANSPORTING INDX CHK.
  ENDIF.
ENDLOOP.

* get UPG
SORT LT_BOM BY INDEX STUFE ASCENDING.
LOOP AT LT_BOM.
  IF LT_BOM-STUFE = 1.
    W_UPG = LT_BOM.
  ENDIF.

  READ TABLE ITAB WITH KEY INDX = LT_BOM-INDEX.

  IF SY-SUBRC = 0.
    ITAB-UPGVC = W_UPG-IDNRK.
*...UPG text
    READ TABLE LT_UPG WITH KEY IDNRK = W_UPG-IDNRK.

    IF SY-SUBRC = 0.
      ITAB-UPGTX = LT_UPG-MAKTX.
    ENDIF.

    MODIFY ITAB TRANSPORTING UPGVC UPGTX WHERE INDX = LT_BOM-INDEX.
  ENDIF.
ENDLOOP.

DATA: IT_ZTCO_CK11 LIKE ZTCO_CK11 OCCURS 0 WITH HEADER LINE.
IF P_SAVE = 'X'.
  DELETE FROM ZTCO_CK11 WHERE
       KOKRS  = F_CKIUSER-KOKRS
   AND KLVAR  = F_CKIUSER-KLVAR
   AND BDATJ  = F_CKIUSER-BDATJ
   AND POPER  = F_CKIUSER-POPER
   AND WERKS  = F_CKIUSER-WERKS
   AND ARTNR  = F_CKIUSER-MATNR
   AND VERID  = F_CKIUSER-VERID.
  COMMIT WORK.
  LOOP AT ITAB.
    MOVE-CORRESPONDING ITAB TO IT_ZTCO_CK11.
    APPEND IT_ZTCO_CK11.
  ENDLOOP.
  INSERT ZTCO_CK11 FROM TABLE IT_ZTCO_CK11.
  WRITE:/ P_MATNR, '***Data saved: ', SY-DBCNT.
ELSE.

  PERFORM FIELD_SETTING(ZCOGSREV) TABLES GT_FIELDCAT USING :
  'KSTAR'     'Cost.Ele'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'TYPPS'     'Type'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'UPGVC'     'UPGVC'          '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'UPGTX'     'UPG Desc'       '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'COMPN'     'Component'      '19' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LTEXT'     'Component Desc' '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'REQQT'     'ReqQty'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MEEHT'     'UoM'            '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'WERTN'     'Amt'            '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'AMTDT'     'KD Duty'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'AMTFT'     'KD Freight'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'AMTOT'     'KD Other'       '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'GPREIS'    'Unit$ '         '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'PEINH'     'PrcUnit'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'STRAT'     'PS'             '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'SPLNT'     'SrcPlant'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LAND1'     'Country'        '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LIFNR'     'Vendor'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'NAME1'     'Name'           '35' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MENGE'     'Qty'            '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'STKKZ'     'Assy'           '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'AUSSS'     'Assy.Scrap'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'KAUSF'     'Comp.Scrap'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'BAUSF'     'Scrap%'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'INFNR'     'Info'           '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MTART'     'Mat type'       '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'XCHAR'     'BatchMgt'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MSTAE'     'Stat1'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MMSTA'     'Stat2'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'SOBSL'     'Sp.Prc'         '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'RGEKZ'     'BF ind'         '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LGPRO'     'S.Loc'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MATMK'     'Mat.Grp'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'STAWN'     'DutyCd'         '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'POSTP'     'Type'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'SORTF'     'SortStr'        '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  G_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = G_REPID
            IT_FIELDCAT        = GT_FIELDCAT
            I_SAVE             = 'A'
       TABLES
            T_OUTTAB           = ITAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.

ENDIF.
