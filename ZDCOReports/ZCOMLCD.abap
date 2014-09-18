REPORT ZMLCD      LINE-SIZE 230           MESSAGE-ID 0
                     NO STANDARD PAGE HEADING.

*REPORT YMLCD .
TABLES: MARA, MAKT, CKMLHD, CKMLPP, CKMLCR, MLCD, T001W,
        T001K, T001, " Valuation area, Currency
        T025, T134, T030, SKAT,
        MSEG, MLHD, MLIT, MLCR, MLPP,  " Ledget header/line
        MBEW,
        MLCRF, MLCRP.  "curr, price change...
TABLES: MLREPORT, CKMLMV009.

DATA: ZCKMLHD  LIKE CKMLHD  OCCURS 0 WITH HEADER LINE.
DATA: I_TYP    LIKE CKMLMV009 occurs 0 with header line.

DATA: BEGIN OF I_T030 OCCURS 0,
        BKLAS   LIKE T030-BKLAS,
        KONTS   LIKE T030-KONTS,
        TXT20   LIKE SKAT-TXT20,
      END OF I_T030.

TYPES: BEGIN OF MLCR_TYPE,
         BDATJ  LIKE MLCR-BDATJ,
         POPER  LIKE MLCR-POPER,
         SALK3  LIKE MLCR-SALK3,
       END OF MLCR_TYPE.

DATA: IMAKT LIKE MAKT  OCCURS 0 WITH HEADER LINE.
DATA: ZMLCR TYPE MLCR_TYPE  OCCURS 0 WITH HEADER LINE.
*
TYPES: BEGIN OF MLCD_TYPE,
         KALNR  LIKE MLCD-KALNR,
         BDATJ  LIKE MLCD-BDATJ,
         POPER  LIKE MLCD-POPER,
         CATEG  LIKE MLCD-CATEG,
         PTYP   LIKE MLCD-PTYP,
         BVALT  LIKE MLCD-BVALT,
         LBKUM  LIKE MLCD-LBKUM,
         SALK3  LIKE MLCD-SALK3,
         ESTPRD LIKE MLCD-SALK3, "price diff
         ESTKDM LIKE MLCD-SALK3, "exchg.diff
         MSTPRD LIKE MLCD-SALK3, "price diff(multi)
         MSTKDM LIKE MLCD-SALK3, "exchg.diff(multi)
       END OF MLCD_TYPE.

DATA: ZMLCD TYPE MLCD_TYPE  OCCURS 0 WITH HEADER LINE.
*
TYPES: BEGIN OF CKMLPP_TYPE,
         KALNR   LIKE CKMLPP-KALNR,
         BDATJ   LIKE CKMLPP-BDATJ,    "Year
         POPER   LIKE CKMLPP-POPER,    "Period
         ABKUMO  LIKE CKMLPP-ABKUMO,   "Begin Qty
         LBKUM   LIKE CKMLPP-LBKUM,    "End Qty
       END OF CKMLPP_TYPE.

DATA: ZCKMLPP TYPE CKMLPP_TYPE  OCCURS 0 WITH HEADER LINE.
*
TYPES: BEGIN OF CKMLCR_TYPE,
         KALNR     LIKE CKMLCR-KALNR,
         BDATJ     LIKE CKMLCR-BDATJ,    "Year
         POPER     LIKE CKMLCR-POPER,    "Period
         VPRSV     LIKE CKMLCR-VPRSV,    "standard/moving (S/V)
         PVPRS     LIKE CKMLCR-PVPRS,    "moving average
         ABSALK3   LIKE CKMLCR-ABSALK3,  "Begin Amt
         SALK3     LIKE CKMLCR-SALK3,    "End Amt
         SALKV     LIKE CKMLCR-SALKV,    "End Amt(Moving)
         PBPRD_O   LIKE CKMLCR-PBPRD_O , "receipt diff adj(pc)
         PBKDM_O   LIKE CKMLCR-PBKDM_O , "receipt diff adj(ex)
         ZUPRD_O   LIKE CKMLCR-ZUPRD_O , "receipt diff(pc)
         ZUKDM_O   LIKE CKMLCR-ZUKDM_O , "receipt diff(ex)
         VNPRD_EA  LIKE CKMLCR-VNPRD_EA, "no dist. (pc)
         VNKDM_EA  LIKE CKMLCR-VNKDM_EA, "no dist. (ex)
         VNPRD_MA  LIKE CKMLCR-VNPRD_MA, "no dist. (pc)-multi
         VNKDM_MA  LIKE CKMLCR-VNKDM_MA, "no dist. (ex)-multi
         EBPRD_EA  LIKE CKMLCR-EBPRD_EA, "settle (pc)
         EBKDM_EA  LIKE CKMLCR-EBKDM_EA, "settle (ex)
         EBPRD_MA  LIKE CKMLCR-EBPRD_MA, "settle (pc)-multi
         EBKDM_MA  LIKE CKMLCR-EBKDM_MA, "settle (ex)-multi
         ABPRD_O   LIKE CKMLCR-ABPRD_O , "initial diff(pc)
         ABKDM_O   LIKE CKMLCR-ABKDM_O , "initial diff(ex)
         ZUPRD_MO  LIKE CKMLCR-ZUPRD_MO,

       END OF CKMLCR_TYPE.

*DATA: I_CKMLCR TYPE CKMLCR_TYPE  OCCURS 0 WITH HEADER LINE.
DATA: I_CKMLCR LIKE CKMLCR  OCCURS 0 WITH HEADER LINE.


* internal table for summary of mlit
DATA: BEGIN OF I_MLITEM OCCURS 0,
        BWKEY   LIKE CKMLHD-BWKEY,     "Plant
        MATNR   LIKE MARA-MATNR,       "Meterial no

        BEWARTGRP  LIKE MLIT-BEWARTGRP  , "group
        LBKUM      LIKE MLPP-LBKUM      , "qty
        SALK3      LIKE MLCR-SALK3      , "new value

*       V1Amt   LIKE MLCD-SALK3,   "Sales Order:sales
        V2AMT   LIKE MLCD-SALK3,   "Sales Order:Internal consumption
        V3AMT   LIKE MLCD-SALK3,   "Sales Order:Free of charge
*       K1Amt   LIKE MLCD-SALK3,   "Cost obj:Internal consumption
        K2AMT   LIKE MLCD-SALK3,   "Cost obj:scrap
        K3AMT   LIKE MLCD-SALK3,   "Cost obj:Free of charge
        K4AMT   LIKE MLCD-SALK3,   "Cost obj:physical inventory (-)
        K5AMT   LIKE MLCD-SALK3,   "Cost obj:physical inventory (+)

        V1QTY   LIKE MLCD-LBKUM,   "Sales Order:sales
        V2QTY   LIKE MLCD-LBKUM,   "Sales Order:Internal consumption
        V3QTY   LIKE MLCD-LBKUM,   "Sales Order:Free of charge
        K1QTY   LIKE MLCD-LBKUM,   "Cost obj:Internal consumption
        K2QTY   LIKE MLCD-LBKUM,   "Cost obj:scrap
        K3QTY   LIKE MLCD-LBKUM,   "Cost obj:Free of charge
        K4QTY   LIKE MLCD-LBKUM,   "Cost obj:physical inventory (-)
        K5QTY   LIKE MLCD-LBKUM,   "Cost obj:physical inventory (+)
       END OF I_MLITEM.

DATA: BEGIN OF I_MLCD OCCURS 0,
        BKLAS   LIKE T030-BKLAS,       "?????
        BWKEY   LIKE CKMLHD-BWKEY,     "Plant
        MATNR   LIKE MARA-MATNR,       "Meterial no
        KALNR   LIKE CKMLHD-KALNR,     "Cost Estimate No
        MLAST   LIKE CKMLHD-MLAST,     "tran/single/multi
*       MAKTX   LIKE MAKT-MAKTX,       "Meterial Desc.

        IN1AMT  LIKE MLCD-SALK3,   "Normal:Purchase
        IN2AMT  LIKE MLCD-SALK3,   "Normal:production
        IN3AMT  LIKE MLCD-SALK3,   "Normal:subcontract
        IN4AMT  LIKE MLCD-SALK3,   "Other: return
        IN5AMT  LIKE MLCD-SALK3,   "Other: transfer
        IN6AMT  LIKE MLCD-SALK3,   "Normal:price change
        IN7AMT  LIKE MLCD-SALK3,   "Other: other receipt
        IN8AMT  LIKE MLCD-SALK3,   "Other: physical inv
        IN9AMT  LIKE MLCD-SALK3,   "Other: Mat to Mat(+/-)
        OU1AMT  LIKE MLCD-SALK3,   "Normal:Sales
        OU2AMT  LIKE MLCD-SALK3,   "Normal:Production
        OU3AMT  LIKE MLCD-SALK3,   "Normal:subcontract
        OU4AMT  LIKE MLCD-SALK3,   "Other: Internal
        OU5AMT  LIKE MLCD-SALK3,   "Other: Purch.return
        OU6AMT  LIKE MLCD-SALK3,   "Other: consumption
        OU7AMT  LIKE MLCD-SALK3,   "Other: free of charge
        OU8AMT  LIKE MLCD-SALK3,   "Other: other consume
        OU9AMT  LIKE MLCD-SALK3,   "Other: Mat to Mat -
        OUAAMT  LIKE MLCD-SALK3,   "Other: transfer

        IN1QTY  LIKE MLCD-LBKUM,
        IN2QTY  LIKE MLCD-LBKUM,
        IN3QTY  LIKE MLCD-LBKUM,
        IN4QTY  LIKE MLCD-LBKUM,
        IN5QTY  LIKE MLCD-LBKUM,
        IN6QTY  LIKE MLCD-LBKUM,
        IN7QTY  LIKE MLCD-LBKUM,
        IN8QTY  LIKE MLCD-LBKUM,
        IN9QTY  LIKE MLCD-LBKUM,
        OU1QTY  LIKE MLCD-LBKUM,
        OU2QTY  LIKE MLCD-LBKUM,
        OU3QTY  LIKE MLCD-LBKUM,
        OU4QTY  LIKE MLCD-LBKUM,
        OU5QTY  LIKE MLCD-LBKUM,
        OU6QTY  LIKE MLCD-LBKUM,
        OU7QTY  LIKE MLCD-LBKUM,
        OU8QTY  LIKE MLCD-LBKUM,
        OU9QTY  LIKE MLCD-LBKUM,
        OUAQTY  LIKE MLCD-LBKUM,


        INAMT   LIKE MLCD-SALK3,   "In  normal total
        IOAMT   LIKE MLCD-SALK3,   "In  other  total
        ONAMT   LIKE MLCD-SALK3,   "Out normal total
        OOAMT   LIKE MLCD-SALK3,   "Out other  total
        BEGAMT  LIKE CKMLCR-ABSALK3,  "Begin Amt

        BAJAMT  LIKE CKMLCR-ABSALK3,  "receipt settle
        NODAMT  LIKE CKMLCR-ABSALK3,  "No Distribution
        SETAMT  LIKE CKMLCR-ABSALK3,  "Settlement diff.
        EAJAMT  LIKE CKMLCR-ABSALK3,  "end settle

        ENDAMT  LIKE CKMLCR-ABSALK3,  "END

        INQTY   LIKE MLCD-LBKUM,   "In  normal total
        IOQTY   LIKE MLCD-LBKUM,   "In  other  total
        ONQTY   LIKE MLCD-LBKUM,   "Out normal total
        OOQTY   LIKE MLCD-LBKUM,   "Out other  total

        BEGQTY  LIKE CKMLPP-ABKUMO,   "Begin Amt
        ENDQTY  LIKE CKMLPP-ABKUMO,   "END

        DIFAMT  LIKE MLCD-SALK3,   "Diff Amt (exchg rateprice)

        ADJAMT   LIKE MLCD-SALK3,   "Begin adjust Amt
        CALCAMT  LIKE MLCD-SALK3,   "CALCULATE END
        CALCQTY  LIKE MLCD-LBKUM,   "CALCULATE END
       END OF I_MLCD.

* MLCD Internal Table
*DATA: I_MLCD  TYPE IT_MLCD OCCURS 1000 WITH HEADER LINE.

DATA: G_ZMOVE        TYPE CHAR1,
      G_MON_FR(7),
      G_MON_TO(7).

* MLIT Table
DATA: BEGIN OF I_MLIT OCCURS 1000,
        BELNR      LIKE MLIT-BELNR      ,
        KJAHR      LIKE MLIT-KJAHR      ,
        POSNR      LIKE MLPP-POSNR      ,
        BDATJ      LIKE MLPP-BDATJ      ,
        POPER      LIKE MLPP-POPER      ,
        CURTP      LIKE MLCR-CURTP      ,
        MATNR      LIKE MLIT-MATNR      ,
        KALNR      LIKE MLIT-KALNR      ,
        BWKEY      LIKE MLIT-BWKEY      ,
        MEINS      LIKE MLIT-MEINS      ,
        BEWARTGRP  LIKE MLIT-BEWARTGRP  ,
        PTYP_BVALT LIKE MLIT-PTYP_BVALT ,
        PTYP_PROC  LIKE MLIT-PTYP_PROC  ,
        LBKUM      LIKE MLPP-LBKUM      , "qty
        BKLAS      LIKE MLPP-BKLAS      , "class
        SALK3      LIKE MLCR-SALK3      , "new value
        SALK3_OLD  LIKE MLCR-SALK3_OLD  , "old
        PVPRS_OLD  LIKE MLCR-PVPRS_OLD  , "moving average
        WAERS      LIKE MLCR-WAERS      ,
        PSART      LIKE MLIT-PSART      ,
    END OF I_MLIT.

* MLCRF
DATA: IT_MLCRF LIKE MLCRF OCCURS 0 WITH HEADER LINE.

*DATA: I_MLIT LIKE MLIT  OCCURS 20  WITH HEADER LINE.
DATA: I_MOVE LIKE T156Q OCCURS 0 WITH HEADER LINE.


DATA: STRDAT TYPE D,
      ENDDAT TYPE D,
      TMPDAT TYPE D.

DATA: CHKBOX   TYPE C,
      AB_QTY   TYPE C,
      AB_AMT   TYPE C,
      CNT      TYPE P VALUE 0.

DATA: TBEGIN_QTY LIKE MLCD-LBKUM,
      TBEGIN_AMT LIKE MLCD-SALK3,
      TBEAD_QTY  LIKE MLCD-LBKUM,
      TBEAD_AMT  LIKE MLCD-SALK3.

RANGES: R_KALNR FOR CKMLHD-KALNR.

CONSTANTS : V VALUE '|',
            H_1(4)  VALUE 'Plt',
            H_2(20) VALUE 'Material',

            H_4(16) VALUE 'Begin',
            H_5(16) VALUE 'Incoming',
            H_6(16) VALUE 'In normal',
            H_7(16) VALUE 'In other',
            H_E(16) VALUE 'RecDiff',
            H_8(16) VALUE 'Outgoing',
            H_9(16) VALUE 'Out normal',
            H_A(16) VALUE 'Out other',
            H_D(16) VALUE 'MLNoDist',
            H_C(16) VALUE 'MLsettle',
            H_B(16) VALUE 'End',

            H_L(4)  VALUE '',
            H_M(20) VALUE '',
            H_N(16) VALUE '',
            H_O(16) VALUE '',
            H_P(16) VALUE '',
            H_Q(16) VALUE '',
            H_R(16) VALUE '',
            H_S(16) VALUE '',
            H_T(16) VALUE '',
            H_U(16) VALUE '',
            H_V(16) VALUE '',
            H_W(16) VALUE '',
            H_X(16) VALUE ''.


*
TYPE-POOLS: CKMD, CKMV0.

DATA: T_BELEGE TYPE CKMD_T_DOCUMENT_REPORT WITH HEADER LINE,
      G_W LIKE T001-WAERS.

*------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.
PARAMETERS:    P_BUKRS LIKE BKPF-BUKRS    OBLIGATORY memory id BUK.
PARAMETERS:    P_MJAHR LIKE MSEG-MJAHR    OBLIGATORY.    "Year
SELECT-OPTIONS P_MONAT FOR  MLCD-POPER. " OBLIGATORY.    "Period
SELECT-OPTIONS P_BWKEY FOR  T001W-WERKS. "Plant
SELECT-OPTIONS P_KONTS FOR  T030-KONTS DEFAULT '0112020100'.
*default '0112050100'.
*SELECT-OPTIONS P_BKLAS FOR  T025-BKLAS.
*SELECT-OPTIONS P_MTART FOR  MARA-MTART.
SELECT-OPTIONS P_MATNR FOR  MARA-MATNR.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-002.
PARAMETERS:    P_ERR     AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK 2.
PARAMETERS:    P_MLIT    AS CHECKBOX DEFAULT ' '.
PARAMETERS:    P_DIFF    AS CHECKBOX DEFAULT ' '.
*------------------------------------------------------------------

*-------------------------------------------------------------------
INITIALIZATION.
*-------------------------------------------------------------------
  MOVE  SY-DATUM+0(4)  TO  P_MJAHR.
  MOVE  SY-DATUM+4(2)  TO  P_MONAT-LOW.

*-------------------------------------------------------------------
START-OF-SELECTION.
*-------------------------------------------------------------------

  IF P_MONAT-LOW  = 0. P_MONAT-LOW  = SY-DATUM+4(2). ENDIF.
  IF P_MONAT-HIGH = 0. P_MONAT-HIGH = P_MONAT-LOW.   ENDIF.
  CONCATENATE P_MJAHR P_MONAT-LOW  INTO G_MON_FR.
  CONCATENATE P_MJAHR P_MONAT-HIGH INTO G_MON_TO.


* get other account movement group
  SELECT * FROM T156Q INTO TABLE I_MOVE
          WHERE MLBWG <> SPACE.
  PERFORM CURRENCY_RTN.

* get material
  PERFORM GET_MARA.
  SELECT * FROM CKMLMV009 INTO TABLE I_TYP.

* get ml data
  SELECT * FROM MLCD INTO CORRESPONDING FIELDS OF TABLE ZMLCD
      FOR ALL ENTRIES IN R_KALNR
      WHERE BDATJ =  P_MJAHR             "Year
        AND POPER BETWEEN P_MONAT-LOW AND P_MONAT-HIGH     "Period
        AND KALNR = R_KALNR-LOW.

  SELECT *  INTO TABLE IT_MLCRF
            FROM MLCRF
            WHERE BDATJ = P_MJAHR
            AND POPER BETWEEN P_MONAT-LOW AND P_MONAT-HIGH     "Period
            AND   FELDG = 'UMO'.
*            AND   feldg LIKE '%ZU%'.


  PERFORM COLLECT_MLCD.
  PERFORM REMOVE_ZERO_ROW.


*-------------------------------------------------------------------
END-OF-SELECTION.
*-------------------------------------------------------------------
  SORT    I_MLCD BY BWKEY MATNR.
  PERFORM DISPLAY_MLCD.

********************************************************************
***  (SHIFT+PF1) Recreate MLCD summary
********************************************************************
AT PF13.
  SUBMIT ML_MLCD_CREATE AND RETURN.
********************************************************************
***  (SHIFT+PF2) Check
********************************************************************
AT PF14.
  CALL TRANSACTION 'CKMC'.

********************************************************************
* AT LINE-SELECTION
********************************************************************
AT LINE-SELECTION.
  PERFORM AT_LINE_SELECTION.
*------------------------------------------------------------------
TOP-OF-PAGE.
*------------------------------------------------------------------
  WRITE: / 'YEAR:', P_MJAHR, 14 'PERIOD:',
            P_MONAT-LOW, '-', P_MONAT-HIGH.
  WRITE: 40 'PLANT:', P_BWKEY-LOW, '-', P_BWKEY-HIGH.
  ULINE.
  FORMAT COLOR COL_HEADING.
  WRITE :   V NO-GAP,
            H_1   NO-GAP, V NO-GAP,
            H_2   NO-GAP, V NO-GAP,
            H_4   NO-GAP, V NO-GAP,
            H_5   NO-GAP, '' NO-GAP,
            H_P   NO-GAP, '' NO-GAP,
            H_Q   NO-GAP, '' NO-GAP,
            H_X   NO-GAP, V NO-GAP,
            H_8   NO-GAP, '' NO-GAP,
            H_S   NO-GAP, '' NO-GAP,
            H_T   NO-GAP, '' NO-GAP,
            H_V   NO-GAP, V NO-GAP,
            H_C   NO-GAP, V NO-GAP,
            H_B   NO-GAP, V NO-GAP.
  WRITE:/  V NO-GAP,
           H_L   UNDER H_1   NO-GAP, V NO-GAP,
           H_M   UNDER H_2   NO-GAP, V NO-GAP,
           H_N   UNDER H_4   NO-GAP, V NO-GAP,
           H_O   UNDER H_5   NO-GAP, V NO-GAP,
           H_6   UNDER H_P   NO-GAP, V NO-GAP,
           H_7   UNDER H_Q   NO-GAP, V NO-GAP,
           H_E   UNDER H_X   NO-GAP, V NO-GAP,
           H_R   UNDER H_8   NO-GAP, V NO-GAP,
           H_9   UNDER H_S   NO-GAP, V NO-GAP,
           H_A   UNDER H_T   NO-GAP, V NO-GAP,
           H_U   UNDER H_C   NO-GAP, V NO-GAP,
           H_D   UNDER H_V   NO-GAP, V NO-GAP,
           H_W   UNDER H_C   NO-GAP, V NO-GAP,
           H_X   UNDER H_B   NO-GAP, V NO-GAP.

  FORMAT COLOR OFF.
  ULINE.
  SET LEFT SCROLL-BOUNDARY COLUMN 25.
********************************************************************

* ZCOV0003 (CKMLHD, MARA) CKMLHD-MATNR = MARA-MATNR
* MANDT      X  ?????
* KALNR      X  ?????? ????? ?? ????
* MATNR         ????
* MTART         ????
* BWKEY         ????
*-------------
FORM GET_MARA.
*-------------
  RANGES: P_MTART FOR T134-MTART,
          P_BKLAS FOR T030-BKLAS.
  DATA: I_T025 LIKE T025 OCCURS 0 WITH HEADER LINE.
  P_MTART-SIGN = 'I'.
  P_MTART-OPTION = 'EQ'.

  P_BKLAS-SIGN = 'I'.
  P_BKLAS-OPTION = 'EQ'.
  SELECT * FROM T030 INTO CORRESPONDING FIELDS OF TABLE I_T030
           WHERE KTOPL = T001-KTOPL
             AND KTOSL = 'BSX'
             AND KONTS IN P_KONTS.
  LOOP AT I_T030.
    SELECT SINGLE TXT20 FROM SKAT INTO I_T030-TXT20
           WHERE SPRAS = SY-LANGU
             AND KTOPL = T001-KTOPL
             AND SAKNR = I_T030-KONTS.
    MODIFY I_T030.
    P_BKLAS-LOW = I_T030-BKLAS.  APPEND P_BKLAS.
  ENDLOOP.

  SELECT DISTINCT * FROM T025 WHERE BKLAS IN P_BKLAS.
    SELECT * FROM T134 WHERE KKREF = T025-KKREF.
      P_MTART-LOW = T134-MTART. APPEND P_MTART.
    ENDSELECT.
  ENDSELECT.

  REFRESH: I_MLCD.

  SELECT * FROM MAKT INTO TABLE IMAKT    "???
      WHERE MATNR IN P_MATNR
        AND SPRAS = SY-LANGU.

*  SELECT DISTINCT MARA~MATNR ckmlhd~bwkey ckmlhd~kalnr ckmlhd~mlast
*           into corresponding fields of table I_MLCD
*           FROM ( MARA inner join ckmlhd
*                       on mara~matnr = ckmlhd~matnr )
*           WHERE mara~MTART in p_MTART
*             and mara~MATNR IN P_MATNR
*             and ckmlhd~bwkey in p_bwkey
*           order by mara~matnr.
  SELECT DISTINCT MBEW~BKLAS MBEW~MATNR MBEW~BWKEY
                  CKMLHD~KALNR CKMLHD~MLAST
           INTO CORRESPONDING FIELDS OF TABLE I_MLCD
           FROM ( MBEW INNER JOIN CKMLHD
                       ON  MBEW~MATNR = CKMLHD~MATNR
                       AND MBEW~BWKEY = CKMLHD~BWKEY )
           WHERE MBEW~MATNR IN P_MATNR
             AND MBEW~BKLAS IN P_BKLAS
             AND CKMLHD~BWKEY IN P_BWKEY
           ORDER BY MBEW~BWKEY MBEW~MATNR.

* for select...
  REFRESH R_KALNR.
  R_KALNR-SIGN = 'I'.
  R_KALNR-OPTION = 'EQ'.
  LOOP AT I_MLCD.
    R_KALNR-LOW = I_MLCD-KALNR.  APPEND R_KALNR.
  ENDLOOP.
ENDFORM.
*---------------------------
FORM COLLECT_MLCD.
*---------------------------
  DATA: F_ABPRD LIKE CKMLCR-ABPRD_O.
  LOOP AT I_MLCD.

    CLEAR G_ZMOVE .

    PERFORM GET_MLCD_DATA.



* Adjust sales (using sales order for consumption)
*      I_MLCD-Ou6Qty = I_MLITEM-V2Qty. " Consumption
*      I_MLCD-Ou6Amt = I_MLITEM-V2Amt.
*      I_MLCD-Ou7Qty = I_MLITEM-V3Qty. " Free of charge
*      I_MLCD-Ou7Amt = I_MLITEM-V3Amt.
*      I_MLCD-Ou1Qty = I_MLCD-Ou1Qty - I_MLCD-Ou6Qty - I_MLCD-Ou7Qty.
*      I_MLCD-Ou1Amt = I_MLCD-Ou1Amt - I_MLCD-Ou6Amt - I_MLCD-Ou7Amt.


* Calc total
    I_MLCD-INAMT = I_MLCD-IN1AMT + I_MLCD-IN2AMT + I_MLCD-IN3AMT
                 + I_MLCD-IN6AMT. "price change
    I_MLCD-IOAMT = I_MLCD-IN4AMT + I_MLCD-IN5AMT + I_MLCD-IN7AMT
                 + I_MLCD-IN8AMT + I_MLCD-IN9AMT.
    I_MLCD-ONAMT = I_MLCD-OU1AMT + I_MLCD-OU2AMT + I_MLCD-OU3AMT.
    I_MLCD-OOAMT = I_MLCD-OU4AMT + I_MLCD-OU5AMT + I_MLCD-OU6AMT
                 + I_MLCD-OU7AMT + I_MLCD-OU8AMT + I_MLCD-OU9AMT
                 + I_MLCD-OUAAMT.

    I_MLCD-INQTY = I_MLCD-IN1QTY + I_MLCD-IN2QTY + I_MLCD-IN3QTY
                 + I_MLCD-IN6QTY. "price change
    I_MLCD-IOQTY = I_MLCD-IN4QTY + I_MLCD-IN5QTY + I_MLCD-IN7QTY
                 + I_MLCD-IN8QTY + I_MLCD-IN9QTY.
    I_MLCD-ONQTY = I_MLCD-OU1QTY + I_MLCD-OU2QTY + I_MLCD-OU3QTY.
    I_MLCD-OOQTY = I_MLCD-OU4QTY + I_MLCD-OU5QTY + I_MLCD-OU6QTY
                 + I_MLCD-OU7QTY + I_MLCD-OU8QTY + I_MLCD-OU9QTY
                 + I_MLCD-OUAQTY.

* begin & end ...
* end = begin + in/out + ML
    PERFORM GET_BEGIN_END.


    I_MLCD-CALCAMT = I_MLCD-BEGAMT
                   + ( I_MLCD-INAMT  + I_MLCD-IOAMT )
                   - ( I_MLCD-ONAMT  + I_MLCD-OOAMT + I_MLCD-NODAMT )
                    + I_MLCD-SETAMT.
    I_MLCD-CALCQTY = I_MLCD-BEGQTY
                      + I_MLCD-INQTY  + I_MLCD-IOQTY
                      - I_MLCD-ONQTY  - I_MLCD-OOQTY.

    I_MLCD-ADJAMT  = I_MLCD-ENDAMT - I_MLCD-CALCAMT.

    COLLECT I_MLCD.

  ENDLOOP.

ENDFORM.

*------------------
FORM GET_MLCD_DATA.
*------------------
* Category
* ZU : Receipt -> effect price
* VN : Consumption -> no effect price
* VP : other receipt/consumption -> effect price
*
*AB     Beginning inventory
*PC     Price changes
*ZU     Receipts
*VP     Other receipts/consumption
*VN     Consumption
*EB     Ending inventory
*NV     Not distributed
*KB     Cumulative inventory
*NC     Not allocated


*CKMLMV009
*Process cat. Partner-ProcCat Description
*---------------------------------------------------------
*B+                           Procurement
*B++                          Masking (procurement)
*BB                           Purchase order
*BBK          VBK             Purchase order (grp)
*BBV                          Procurement (change involving stocks)
*BF           VF              Production
*BKA                          Sales order
*BL           VL              Subcontracting
*BNAW                         Procurement (inactive plant)
*BU           VU              Stock transfer
*BUBM                         Material tansfer posting
*BUBS         VUBS            Transfer posting - special stock
*V+                           Consumption
*V++                          Masked (consumption)
*VA                           Fixed asset
*VBK          BBK             Purchase order (grp)
*VEAU                         Consumption for single-level orders
*VF           BF              Production
*VHP                          Cost object
*VK                           Cost center
*VKA                          Sales order
*VL           BL              Subcontracting
*VNAW                         Consumption (inactive plant)
*VP                           Project
*VU           BU              Stock transfer
*VUBM                         Material tansfer posting
*VUBS         BUBS            Transfer posting - special stock

  LOOP AT ZMLCD WHERE KALNR = I_MLCD-KALNR.    "Cost estimate no
    I_MLCD-SETAMT = I_MLCD-SETAMT
            + ZMLCD-ESTPRD + ZMLCD-ESTKDM
            + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.

    G_ZMOVE = 'X'.  "?? move? ???.
*    case zmlcd-categ.
*      when 'AB'.
*      when 'ZU' or 'VP'. "????/?? - ????/??
*      when 'VN'.
*      when 'PC'.  " ??.
*      when others. "????? ??...
*    endcase.

    IF ZMLCD-CATEG = 'AB'. "Initial Stock :Begin...
      I_MLCD-BEGAMT = I_MLCD-BEGAMT + ZMLCD-SALK3.
      I_MLCD-BEGQTY = I_MLCD-BEGQTY + ZMLCD-LBKUM.
    ENDIF.


    IF  ZMLCD-CATEG = 'ZU'.
      CASE ZMLCD-PTYP.
        WHEN 'B+' OR 'BB' OR 'BBK' OR 'BBV'. "purchase/procurement
          I_MLCD-IN1QTY = I_MLCD-IN1QTY + ZMLCD-LBKUM.
          I_MLCD-IN1AMT = I_MLCD-IN1AMT + ZMLCD-SALK3.
          IF P_DIFF = 'X'.
            I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                       + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                       + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
          ENDIF.
        WHEN 'BF' OR 'BKA'. "production/sales order
          I_MLCD-IN2QTY = I_MLCD-IN2QTY + ZMLCD-LBKUM.
          I_MLCD-IN2AMT = I_MLCD-IN2AMT + ZMLCD-SALK3.
          IF P_DIFF = 'X'.
            I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                          + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                          + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
          ENDIF.
        WHEN 'BL'.          "subcontracting
          I_MLCD-IN3QTY = I_MLCD-IN3QTY + ZMLCD-LBKUM.
          I_MLCD-IN3AMT = I_MLCD-IN3AMT + ZMLCD-SALK3.
          IF P_DIFF = 'X'.
            I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                          + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                          + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
          ENDIF.
* RETURN
* Material to Material (+)
        WHEN 'BUBM'.        "material transfer:
          I_MLCD-IN4QTY = I_MLCD-IN4QTY + ZMLCD-LBKUM.
          I_MLCD-IN4AMT = I_MLCD-IN4AMT + ZMLCD-SALK3.
          IF P_DIFF = 'X'.
            I_MLCD-BAJAMT = I_MLCD-BAJAMT +
              + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
              + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
          ENDIF.
* TRANSFER
        WHEN 'BU' OR 'BUBS'."stock transfer/transfer special
          I_MLCD-IN5QTY = I_MLCD-IN5QTY + ZMLCD-LBKUM.
          I_MLCD-IN5AMT = I_MLCD-IN5AMT + ZMLCD-SALK3.
          IF P_DIFF = 'X'.
            I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                          + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                          + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
          ENDIF.
      ENDCASE.
    ENDIF.

*///// Other
* Price Change (+/-)
    IF  ZMLCD-CATEG = 'PC'.
      I_MLCD-IN6QTY = I_MLCD-IN6QTY + ZMLCD-LBKUM.
      I_MLCD-IN6AMT = I_MLCD-IN6AMT + ZMLCD-SALK3.
      IF P_DIFF = 'X'.
        I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                      + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                      + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
      ENDIF.
    ENDIF.

* VP : Other receipts/consumption
* Material Debit/Credit? ???? ??? ? ? ???...
    IF ZMLCD-CATEG = 'VP'. " ZMLCD-SALK3 > 0. +/1
      I_MLCD-IN7QTY = I_MLCD-IN7QTY + ZMLCD-LBKUM.
      I_MLCD-IN7AMT = I_MLCD-IN7AMT + ZMLCD-SALK3.
      IF P_DIFF = 'X'.
        I_MLCD-BAJAMT = I_MLCD-BAJAMT +
                      + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
                      + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????
      ENDIF.
    ENDIF.

* Physical QTY??? FIXME
*    IF  ZMLCD-CATEG = 'VN' AND ZMLCD-PTYP  = 'V+'
*    AND ZMLCD-SALK3 > 0.       " 000000510362'.
*      I_MLCD-In8QTY = I_MLCD-In8QTY + ZMLCD-LBKUM.
*      I_MLCD-In8AMT = I_MLCD-In8AMT + ZMLCD-SALK3.
*    ENDIF.



******************************************************
** Outgoing
******************************************************


    IF ZMLCD-CATEG = 'VN'.
      CASE ZMLCD-PTYP.
* Sales Order ...
        WHEN 'V+'.
          I_MLCD-OU1QTY = I_MLCD-OU1QTY + ZMLCD-LBKUM.
          I_MLCD-OU1AMT = I_MLCD-OU1AMT + ZMLCD-SALK3.
* Single-level order/Productoin/sales order
        WHEN 'VEAU' OR 'VF' OR 'VKA'.
          I_MLCD-OU2QTY = I_MLCD-OU2QTY + ZMLCD-LBKUM.
          I_MLCD-OU2AMT = I_MLCD-OU2AMT + ZMLCD-SALK3.
*                        + ZMLCD-ESTPRD  + ZMLCD-ESTKDM
*                        + ZMLCD-MSTPRD  + ZMLCD-MSTKDM.  "?????

* subcontracting
        WHEN 'VL'.
          I_MLCD-OU3QTY = I_MLCD-OU3QTY + ZMLCD-LBKUM.
          I_MLCD-OU3AMT = I_MLCD-OU3AMT + ZMLCD-SALK3.

* 5 - Internal Consumption, Scrap : VK - 522430, 523120
* 6 - Free of charge : V+ - 523010
* 7 - Physical inventory : 510362
* Internal Consumption(Cost Center/Project/Fixed Asset)
        WHEN 'VK' OR 'VP' OR 'VA'.
          I_MLCD-OU4QTY = I_MLCD-OU4QTY + ZMLCD-LBKUM.
          I_MLCD-OU4AMT = I_MLCD-OU4AMT + ZMLCD-SALK3.

* TRANSFER
        WHEN 'VU' OR 'VUBS'. "stock transfer/transfer special
          I_MLCD-OU3QTY = I_MLCD-OU3QTY + ZMLCD-LBKUM.
          I_MLCD-OU3AMT = I_MLCD-OU3AMT + ZMLCD-SALK3.

* RETURN P/O
        WHEN 'VBK'.        "Purchase return
          I_MLCD-IN4QTY = I_MLCD-IN4QTY + ZMLCD-LBKUM.
          I_MLCD-IN4AMT = I_MLCD-IN4AMT + ZMLCD-SALK3.

* Material to Material
        WHEN 'VUBM'.
          I_MLCD-OU9QTY = I_MLCD-OU9QTY + ZMLCD-LBKUM.
          I_MLCD-OU9AMT = I_MLCD-OU9AMT + ZMLCD-SALK3.

      ENDCASE.
    ENDIF.

* VP: Other receipt/consumption
*   IF ZMLCD-CATEG = 'VP' and ZMLCD-SALK3 < 0.
*     I_MLCD-Ou8Qty = I_MLCD-Ou8Qty + ZMLCD-LBKUM.
*     I_MLCD-Ou8Amt = I_MLCD-Ou8Amt + ZMLCD-SALK3.
*   ENDIF.

  ENDLOOP.

ENDFORM.

*------------------------------------------------------------------
* FORM Calc_BeginAmt
*------------------------------------------------------------------
FORM CALC_BEGINAMT CHANGING CALCBEGAMT LIKE MLCR-SALK3.
  DATA: FIRST_FLAG,
        T_SALK3    LIKE MLCR-SALK3,
        TT_SALK3   LIKE MLCR-SALK3,
*       CalcBegAmt LIKE MLCR-SALK3,
        L_BDATJ     LIKE MLCR-BDATJ,
        L_POPER     LIKE MLCR-POPER,
        L_MONAT    LIKE CKMLPP-POPER.

  CALCBEGAMT = 0.

  L_MONAT = P_MONAT-LOW.
  WHILE ( L_MONAT <= P_MONAT-HIGH ).

    CALL FUNCTION 'CKMD_PRICE_HISTORY_READ'
         EXPORTING
              I_KALNR           = I_MLCD-KALNR
              I_BDATJ           = P_MJAHR
              I_POPER           = L_MONAT
              I_UNTPER          = '000'
              I_REFRESH_BUFFER  = SPACE
              I_ONLINE          = ' '
         TABLES
              T_PH              = T_BELEGE
         EXCEPTIONS
              NO_DOCUMENT_FOUND = 1
              OTHERS            = 2.

    IF SY-SUBRC <> 0.
    ENDIF.

    T_SALK3   = 0.

* AB : initial stock
    LOOP AT T_BELEGE WHERE CATEG = 'AB'.

      REFRESH ZMLCR.
      SELECT * FROM MLCR INTO CORRESPONDING FIELDS OF TABLE ZMLCR
        WHERE BELNR = T_BELEGE-BELNR
          AND KJAHR = T_BELEGE-KJAHR
          AND POSNR = T_BELEGE-POSNR
          AND CURTP = T_BELEGE-CURTP
        ORDER BY BDATJ DESCENDING
                 POPER DESCENDING.

      FIRST_FLAG = ''.
      TT_SALK3  = 0.
      L_BDATJ = ''.
      L_POPER = ''.

      LOOP AT ZMLCR.
        IF FIRST_FLAG = ''.
          T_SALK3 = ZMLCR-SALK3.
          FIRST_FLAG = '*'.
          L_BDATJ = ZMLCR-BDATJ.
          L_POPER = ZMLCR-POPER.
          CONTINUE.
        ENDIF.

        TT_SALK3 = T_SALK3 - ZMLCR-SALK3.
        EXIT.
      ENDLOOP.

      CALCBEGAMT = CALCBEGAMT + TT_SALK3.

*     get difference amount
      SELECT SINGLE * FROM MLCRF   WHERE BELNR = T_BELEGE-BELNR
                                     AND KJAHR = T_BELEGE-KJAHR
                                     AND POSNR = T_BELEGE-POSNR
                                     AND BDATJ = L_BDATJ
                                     AND POPER = L_POPER
                                     AND CURTP = T_BELEGE-CURTP
                                     AND FELDG = 'UMO'.
      CHECK SY-SUBRC = 0.
      CALCBEGAMT = CALCBEGAMT + MLCRF-PRDIF + MLCRF-KRDIF.

    ENDLOOP.

    L_MONAT = L_MONAT + 1.
  ENDWHILE.

*  I_MLCD-BeAdjAmt = CalcBegAmt.

ENDFORM.

*&------------------------------------------------------------------
FORM DISPLAY_MLCD.
*&------------------------------------------------------------------
  FORMAT COLOR 2.
  DATA:MODE LIKE SY-TABIX,
       SV_P TYPE I VALUE 21.

  LOOP AT I_MLCD.

    AT NEW BKLAS.
      NEW-PAGE.
      FORMAT COLOR 3.
      READ TABLE I_T030 WITH KEY BKLAS = I_MLCD-BKLAS.
      WRITE:/ 'Account : ', I_MLCD-BKLAS, I_T030-KONTS, I_T030-TXT20.
    ENDAT.

    AT NEW BWKEY.
      SKIP.
      ULINE.
    ENDAT.

    FORMAT COLOR 1.
    MODE = SY-TABIX MOD 2.
    PERFORM SET_MODE USING MODE.
    PERFORM WRITE_LINE USING 'D'.

    AT END OF BWKEY.
      SUM.
      ULINE.
      FORMAT COLOR 4.
      PERFORM WRITE_LINE USING 'S'.
    ENDAT.

    SET LEFT SCROLL-BOUNDARY COLUMN 39.

    AT LAST.
      SUM.
      ULINE.
      FORMAT COLOR 5.
      PERFORM WRITE_LINE USING 'S'.
    ENDAT.
  ENDLOOP.
ENDFORM.

*clear: i_mlcd-bwkey, i_mlcd-matnr.


*&------------------------------------------------------------------
*& 	FORM REMOVE_ZERO_ROW.
*&------------------------------------------------------------------
FORM REMOVE_ZERO_ROW.
  LOOP AT I_MLCD.
    IF  I_MLCD-BEGQTY = 0 AND I_MLCD-ENDQTY = 0
    AND I_MLCD-INQTY  = 0 AND I_MLCD-IOQTY  = 0
    AND I_MLCD-ONQTY  = 0 AND I_MLCD-OOQTY  = 0.
      DELETE I_MLCD INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&------------------------------------------------------------------
*&      Form  SET_MODE
*&------------------------------------------------------------------
FORM SET_MODE USING    P_MODE.
  IF P_MODE EQ 0.
    FORMAT INTENSIFIED ON.
  ELSE.
    FORMAT INTENSIFIED OFF.
  ENDIF.

ENDFORM.                    " SET_MODE
*&------------------------------------------------------------------
*&      Form  CURRENCY_RTN
*&------------------------------------------------------------------
FORM CURRENCY_RTN.
  SELECT SINGLE * FROM T001K
   WHERE BWKEY IN P_BWKEY.

  IF SY-SUBRC <> 0.
    G_W = 'KRW'.
  ENDIF.

  SELECT SINGLE * FROM T001
   WHERE BUKRS = P_BUKRS.
  G_W = T001-WAERS.
ENDFORM.                    " CURRENCY_RTN
*&------------------------------------------------------------------
*&   Form  get_begin_end
*&------------------------------------------------------------------
FORM GET_BEGIN_END.

  DATA: L_BDATJ   LIKE CKMLPP-BDATJ,
        L_POPER   LIKE CKMLPP-POPER,
        L_BDATJ2  LIKE CKMLPP-BDATJ,
        L_POPER2  LIKE CKMLPP-POPER,
        SV_BDATJ  LIKE CKMLPP-BDATJ,
        SV_POPER  LIKE CKMLPP-POPER,
        L_MONYR(7),
        L_SALK3   LIKE MLCR-SALK3,
        L_AMT_ML  LIKE CKMLCR-SALK3,
        L_ML_MOV(1)  TYPE C,
        L_ML_SET(1)  TYPE C,
        L_ABPRD LIKE CKMLCR-ZUPRD_A,
        L_ZUPRD LIKE CKMLCR-ZUPRD_A.




  CLEAR: I_MLCD-BEGQTY, I_MLCD-BEGAMT.

* BEGIN QTY UP TO 1 ROWS
* get latest period.
  SELECT * FROM CKMLPP
                       WHERE KALNR = I_MLCD-KALNR
                         AND BDATJ <= P_MJAHR
     ORDER BY BDATJ DESCENDING
              POPER DESCENDING.
    CONCATENATE CKMLPP-BDATJ CKMLPP-POPER INTO L_MONYR.
    CHECK L_MONYR < G_MON_FR.
    MOVE CKMLPP-BDATJ TO L_BDATJ.  "Get max year and period
    MOVE CKMLPP-POPER TO L_POPER.
    EXIT.
  ENDSELECT.

* Begin Qty
  SELECT SINGLE SUM( LBKUM ) INTO I_MLCD-BEGQTY   FROM CKMLPP
    WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
      AND BDATJ = L_BDATJ              "Year
      AND POPER = L_POPER.             "Period

* BEGIN Amt
  SELECT SINGLE *  FROM CKMLCR
     INTO CORRESPONDING FIELDS OF I_CKMLCR
     WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
       AND BDATJ = L_BDATJ              "Year
       AND POPER = L_POPER.             "Period
  IF SY-SUBRC = 0.
    IF I_CKMLCR-VPRSV = 'V' AND I_MLCD-MLAST = '3'.
      I_MLCD-BEGAMT = I_CKMLCR-SALKV.
      L_AMT_ML      = I_CKMLCR-SALK3. "save
    ELSE.
      I_MLCD-BEGAMT = I_CKMLCR-SALK3.
      L_AMT_ML      = I_CKMLCR-SALKV. "save
    ENDIF.
  ENDIF.

  L_ABPRD = I_CKMLCR-ABPRD_O.
  L_ZUPRD = I_CKMLCR-ZUPRD_MO + I_CKMLCR-ABPRD_MO.
  L_ML_MOV  = I_CKMLCR-VPRSV.

* ENDING ******************************************

  I_MLCD-ENDQTY = 0.
  I_MLCD-ENDAMT = 0.
* ENDING AMT
  SELECT * FROM CKMLCR  WHERE KALNR = I_MLCD-KALNR
                            AND BDATJ <= P_MJAHR
       ORDER BY BDATJ DESCENDING
                POPER DESCENDING.

    CONCATENATE CKMLCR-BDATJ CKMLCR-POPER INTO L_MONYR.
    CHECK L_MONYR <= G_MON_TO.
    MOVE CKMLCR-BDATJ TO L_BDATJ2.  "Get max year and period
    MOVE CKMLCR-POPER TO L_POPER2.
    EXIT.
  ENDSELECT.
* ML: moving -> standard ( salkv -> salk3 )
*  SELECT SINGLE SUM( SALK3 ) INTO I_MLCD-ENDAmt   FROM CKMLCR
*      WHERE KALNR = I_MLCD-KALNR        "Cost estimate no
*        AND BDATJ = L_BDATJ             "Year
*        AND POPER = L_POPER2.           "Period

* if l_bdatj <> l_bdatj2. l_poper2 = l_poper2 - 1. endif.
  SELECT SINGLE SUM( LBKUM ) INTO I_MLCD-ENDQTY   FROM CKMLPP
      WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
        AND BDATJ = L_BDATJ2           "Year
        AND POPER = L_POPER2.          "Period

* check if ml settle is run...
  IF I_MLCD-MLAST = '3'.
    SELECT SINGLE * FROM CKMLCR
        WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
          AND BDATJ >= L_BDATJ2          "Year
          AND POPER >= L_POPER2          "Period
          AND VPRSV = 'V'.
    IF SY-SUBRC = 0.
      L_ML_SET = 'X'.
    ELSE.
      L_ML_SET = ' '.
    ENDIF.
  ENDIF.

* Get End Amt
  SELECT SINGLE * FROM CKMLCR
     INTO CORRESPONDING FIELDS OF I_CKMLCR
      WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
        AND BDATJ = L_BDATJ2           "Year
        AND POPER = L_POPER2.          "Period
  IF SY-SUBRC = 0.

*    if i_mlcd-mlast = '3'.
*      if i_ckmlcr-vprsv = 'V'.   l_ml_set = 'X'.
*      else.                      l_ml_set = ' '.   endif.
*    endif.

    IF I_MLCD-MLAST = '3'.
      IF L_ML_SET = 'X'.
        I_MLCD-ENDAMT = I_CKMLCR-SALKV.
*        clear f_abprd.

        IF L_ML_MOV = 'S'.
          I_MLCD-BEGAMT = L_AMT_ML.
        ENDIF.
      ELSE.
        IF I_CKMLCR-VPRSV = 'V'.
          I_MLCD-ENDAMT = I_CKMLCR-SALKV.
        ELSE.
          I_MLCD-ENDAMT = I_CKMLCR-SALK3.
        ENDIF.
      ENDIF.
* mlast = 2
    ELSE.
      I_MLCD-ENDAMT = I_CKMLCR-SALK3.
    ENDIF.
  ENDIF.

* ?? ??? ??? ??? ???...
  IF G_ZMOVE = SPACE.
    I_MLCD-ENDAMT = I_MLCD-BEGAMT.
    I_MLCD-ENDQTY = I_MLCD-BEGQTY.
    CLEAR: I_MLCD-SETAMT, I_MLCD-NODAMT.
    EXIT.
  ENDIF.


*  clear i_mlcd-bajamt.

* ????=0??... ????... ???..0??...
  IF I_CKMLCR-VPRSV = 'S' AND I_CKMLCR-STPRS = 0.
    PERFORM CLEAR_MLCD.
    EXIT.
  ENDIF.

* ?? ??? ???, ???? ??.
  IF G_ZMOVE = SPACE. EXIT. ENDIF.

* from~to settle/no dist amount
  REFRESH I_CKMLCR.
  SELECT * FROM CKMLCR
      INTO CORRESPONDING FIELDS OF TABLE I_CKMLCR
      WHERE KALNR = I_MLCD-KALNR       "Cost estimate no
        AND BDATJ = L_BDATJ2           "Year
*         AND POPER BETWEEN P_MONAT-LOW AND P_MONAT-HIGH.
        AND POPER = L_POPER2.
  LOOP AT I_CKMLCR.
    I_MLCD-NODAMT = I_MLCD-NODAMT
                  + I_CKMLCR-VNPRD_EA + I_CKMLCR-VNKDM_EA
                  + I_CKMLCR-VNPRD_MA + I_CKMLCR-VNKDM_MA.
*    I_MLCD-SETAmt = I_MLCD-SETAmt
*                  + i_ckmlcr-ebPRD_EA + i_ckmlcr-ebKDM_EA
*                  + i_ckmlcr-ebPRD_MA + i_ckmlcr-ebKDM_MA.
* CL settlement
    IF I_MLCD-MLAST = '3' AND L_ML_SET = 'X'.
      SELECT SINGLE * FROM MLREPORT
               WHERE KALNR = I_MLCD-KALNR
                 AND BDATJ = L_BDATJ2
                 AND POPER = L_POPER2
                 AND PSART = 'CL'.
      IF SY-SUBRC = 0.
        L_POPER = MLREPORT-POPER - 1.
        IF L_POPER = 0.
          L_BDATJ = MLREPORT-BDATJ - 1. L_POPER = 12.
        ENDIF.

        SELECT SINGLE * FROM MLCR
                        WHERE BELNR = MLREPORT-BELNR
                          AND KJAHR = MLREPORT-KJAHR
                          AND POSNR = MLREPORT-POSNR
                          AND BDATJ = L_BDATJ
                          AND POPER = L_POPER.
        IF SY-SUBRC = 0.
          L_SALK3 = MLCR-SALKV_OLD - MLCR-SALK3_OLD.
        ENDIF.
      ENDIF.
*      if i_mlcd-SETamt > 0.
*        write:/ i_mlcd-matnr, 'ssssssssssssssssssssssssssssss'.
*      endif.
      I_MLCD-SETAMT = I_MLCD-SETAMT + L_SALK3.
    ENDIF.

  ENDLOOP.

* no settlement yet...
* ???? ???? + BOM ????
* BOM????? ??? ???? ...
* FIXME
  IF I_MLCD-MLAST = '3'.
    I_MLCD-SETAMT = -1 * ( I_MLCD-BEGAMT
               + ( I_MLCD-INAMT  + I_MLCD-IOAMT + I_MLCD-BAJAMT )
               - ( I_MLCD-ONAMT  + I_MLCD-OOAMT + I_MLCD-NODAMT )
               - I_MLCD-ENDAMT ).
  ELSE.
    I_MLCD-SETAMT = -1 * ( I_MLCD-BEGAMT
               + ( I_MLCD-INAMT  + I_MLCD-IOAMT + I_MLCD-BAJAMT )
               - ( I_MLCD-ONAMT  + I_MLCD-OOAMT + I_MLCD-NODAMT )
               - I_MLCD-ENDAMT ).
  ENDIF.


* I_MLCD-BAJAmt = I_MLCD-SETAmt  - I_MLCD-BAJAmt
*                 + i_MLCD-nodamt.

ENDFORM.                    " get_begin_end

*&------------------------------------------------------------------
*&      Form  WRITE_LINE
*&------------------------------------------------------------------
FORM WRITE_LINE USING L_MODE TYPE C.
  DATA: L_MAKTX   LIKE MAKT-MAKTX.
  DATA: L_INAMT   LIKE MLCD-SALK3,
        L_INQTY   LIKE MLCD-LBKUM,
        L_OUAMT   LIKE MLCD-SALK3,
        L_OUQTY   LIKE MLCD-LBKUM.

  IF L_MODE = 'D'.
    READ TABLE IMAKT WITH KEY MATNR = I_MLCD-MATNR.
    L_MAKTX = IMAKT-MAKTX.
  ENDIF.

  L_INAMT = I_MLCD-INAMT + I_MLCD-IOAMT + I_MLCD-BAJAMT.
  L_INQTY = I_MLCD-INQTY + I_MLCD-IOQTY.
  L_OUAMT = I_MLCD-ONAMT + I_MLCD-OOAMT + I_MLCD-NODAMT.
  L_OUQTY = I_MLCD-ONQTY + I_MLCD-OOQTY.

  IF L_MODE = 'D'.  " Data
    FORMAT COLOR 2.
  ENDIF.

  IF I_MLCD-ADJAMT <> 0.
    FORMAT COLOR COL_NEGATIVE.  " Problem occur...
  ELSE.
    IF P_ERR = 'X'.  EXIT. ENDIF.
  ENDIF.

*for test
  IF I_MLCD-DIFAMT <> 0.
    FORMAT COLOR COL_TOTAL. " diff amount ...
  ENDIF.

  FORMAT INTENSIFIED ON.
  WRITE:/ V NO-GAP,
          I_MLCD-BWKEY  UNDER H_1  NO-GAP, V NO-GAP,
     (20) I_MLCD-MATNR  UNDER H_2  NO-GAP, V NO-GAP,
*         ' Qty'        UNDER H_3  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) I_MLCD-BEGQTY UNDER H_4  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) L_INQTY       UNDER H_5  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) I_MLCD-INQTY  UNDER H_6  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) I_MLCD-IOQTY  UNDER H_7  DECIMALS 0 NO-GAP, V NO-GAP,
           H_X   UNDER H_E   NO-GAP, V NO-GAP,
     (16) L_OUQTY       UNDER H_8  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) I_MLCD-ONQTY  UNDER H_9  DECIMALS 0 NO-GAP, V NO-GAP,
     (16) I_MLCD-OOQTY  UNDER H_A  DECIMALS 0 NO-GAP, V NO-GAP,
           H_V   UNDER H_D   NO-GAP, V NO-GAP,
           H_U   UNDER H_C   NO-GAP, V NO-GAP,
     (16) I_MLCD-ENDQTY UNDER H_B  DECIMALS 0 NO-GAP, V NO-GAP.

  HIDE: I_MLCD-BWKEY, I_MLCD-MATNR.

  FORMAT INTENSIFIED OFF.
  WRITE:/ V NO-GAP,
          '    '        UNDER H_1  DECIMALS 0 NO-GAP, V NO-GAP,
     (20) L_MAKTX       UNDER H_2  NO-GAP, V NO-GAP,
*    (20) I_MLCD-KALNR  UNDER H_2  NO-GAP, V NO-GAP,
     (16) I_MLCD-BEGAMT UNDER H_4 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) L_INAMT       UNDER H_5 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-INAMT  UNDER H_6 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-IOAMT  UNDER H_7 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-BAJAMT UNDER H_E CURRENCY G_W NO-GAP, V NO-GAP,
     (16) L_OUAMT       UNDER H_8 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-ONAMT  UNDER H_9 CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-OOAMT  UNDER H_A CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-SETAMT UNDER H_C CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-NODAMT UNDER H_D CURRENCY G_W NO-GAP, V NO-GAP,
     (16) I_MLCD-ENDAMT UNDER H_B CURRENCY G_W NO-GAP, V NO-GAP.

  ULINE.
ENDFORM.
*&------------------------------------------------------------------
*&      Form  AT_LINE_SELECTION
*&-----------------------------------------------------------------
FORM AT_LINE_SELECTION.
  TABLES: CKMLMV010T.  " group text
  CHECK I_MLCD-MATNR <> SPACE.

  READ TABLE I_MLCD WITH KEY BWKEY = I_MLCD-BWKEY
                             MATNR = I_MLCD-MATNR.
  CHECK SY-SUBRC = 0.

* ??...
  SELECT SINGLE * FROM MBEW
                  WHERE MATNR = I_MLCD-MATNR
                   AND BWKEY = I_MLCD-BWKEY.

  WRITE:/  I_MLCD-BWKEY, ' - ', I_MLCD-MATNR.
  WRITE:  '(', I_MLCD-KALNR, ')', MBEW-VPRSV, MBEW-BKLAS.
  WRITE:/ 'Qty:', MBEW-LBKUM,
          'Val:', MBEW-SALK3 CURRENCY G_W,
          'MAP:', MBEW-VERPR CURRENCY G_W,
          'STD:', MBEW-STPRS CURRENCY G_W.
  ULINE.

* other account data from mseg
* except cost center consumption
* run only if checked...(for test)
  PERFORM GET_MLIT_INFO.
  PERFORM CALC_MLIT_AMT.

  SORT I_MLITEM BY LBKUM.
  DATA: I1 TYPE I.

  LOOP AT I_MLITEM.
    AT FIRST.
    WRITE:/ 'Category',
          40  'Qty: ',
          80  'Amt: '.
    ENDAT.

    I1 = I_MLITEM-BEWARTGRP MOD 2.
    IF I1 = 0. "??
      FORMAT COLOR 3.
    ELSE.
      FORMAT COLOR OFF.
    ENDIF.

    SELECT SINGLE * FROM CKMLMV010T WHERE SPRAS = SY-LANGU
                                      AND MLBWG = I_MLITEM-BEWARTGRP.
    WRITE:/ I_MLITEM-BEWARTGRP, CKMLMV010T-KTEXT,
          40  I_MLITEM-LBKUM,
          80  I_MLITEM-SALK3 CURRENCY G_W COLOR 2.
    AT LAST.
      SUM.
      FORMAT COLOR 4.
      WRITE:/ 'Total',
            40  I_MLITEM-LBKUM,
            80  I_MLITEM-SALK3 CURRENCY G_W COLOR 1.
    ENDAT.
  ENDLOOP.
*  WRITE:/ I_MLITEM-V2AMT,   'Sales Order:Internal consumption'.
*  WRITE:/ I_MLITEM-V3AMT,   'Sales Order:Free of charge      '.
*  WRITE:/ I_MLITEM-K2AMT,   'Cost obj:scrap                  '.
*  WRITE:/ I_MLITEM-K3AMT,   'Cost obj:Free of charge         '.
*  WRITE:/ I_MLITEM-K4AMT,   'Cost obj:physical inventory (-) '.
*  WRITE:/ I_MLITEM-K5AMT,   'Cost obj:physical inventory (+) '.
********************************************************
  EXIT.

  WRITE:/ 'Normal:Purchase       ',
       		I_MLCD-IN1AMT  CURRENCY G_W, I_MLCD-IN1QTY.
  WRITE:/ 'Normal:production     ',
  			I_MLCD-IN2AMT  CURRENCY G_W, I_MLCD-IN2QTY.
  WRITE:/ 'Normal:subcontract    ',
  			I_MLCD-IN3AMT  CURRENCY G_W, I_MLCD-IN3QTY.
  WRITE:/ 'Normal:price change   ',
  			I_MLCD-IN6AMT  CURRENCY G_W, I_MLCD-IN6QTY.
  WRITE:/ 'Other: return         ',
  			I_MLCD-IN4AMT  CURRENCY G_W, I_MLCD-IN4QTY.
  WRITE:/ 'Other: transfer       ',
  			I_MLCD-IN5AMT  CURRENCY G_W, I_MLCD-IN5QTY.
  WRITE:/ 'Other: other receipt  ',
  			I_MLCD-IN7AMT  CURRENCY G_W, I_MLCD-IN7QTY.
  WRITE:/ 'Other: physical inv   ',
  			I_MLCD-IN8AMT  CURRENCY G_W, I_MLCD-IN8QTY.
  WRITE:/ 'Other: Mat to Mat(+/-)',
  			I_MLCD-IN9AMT  CURRENCY G_W, I_MLCD-IN9QTY.
  WRITE:/ 'Normal:Sales          ',
  			I_MLCD-OU1AMT  CURRENCY G_W, I_MLCD-OU1QTY.
  WRITE:/ 'Normal:Production     ',
  			I_MLCD-OU2AMT  CURRENCY G_W, I_MLCD-OU2QTY.
  WRITE:/ 'Normal:subcontract    ',
  			I_MLCD-OU3AMT  CURRENCY G_W, I_MLCD-OU3QTY.
  WRITE:/ 'Other: Internal       ',
  			I_MLCD-OU4AMT  CURRENCY G_W, I_MLCD-OU4QTY.
  WRITE:/ 'Other: Purch.return   ',
  			I_MLCD-OU5AMT  CURRENCY G_W, I_MLCD-OU5QTY.
  WRITE:/ 'Other: consumption    ',
  			I_MLCD-OU6AMT  CURRENCY G_W, I_MLCD-OU6QTY.
  WRITE:/ 'Other: free of charge ',
  			I_MLCD-OU7AMT  CURRENCY G_W, I_MLCD-OU7QTY.
  WRITE:/ 'Other: other consume  ',
  		    I_MLCD-OU8AMT  CURRENCY G_W, I_MLCD-OU8QTY.
  WRITE:/ 'Other: Mat to Mat -   ',
  			I_MLCD-OU9AMT  CURRENCY G_W, I_MLCD-OU9QTY.
  WRITE:/ 'Other: transfer       ', 	
  			I_MLCD-OUAAMT  CURRENCY G_W, I_MLCD-OUAQTY.

  ULINE.
  WRITE:/ 'Begin ', I_MLCD-BEGAMT  CURRENCY G_W, I_MLCD-BEGQTY.
  WRITE:/ 'MLSet ', I_MLCD-SETAMT  CURRENCY G_W.
  WRITE:/ 'End   ', I_MLCD-ENDAMT  CURRENCY G_W, I_MLCD-ENDQTY.
  WRITE:/ 'Diff. ', I_MLCD-DIFAMT  CURRENCY G_W.
  WRITE:/ 'Adj.  ', I_MLCD-ADJAMT  CURRENCY G_W.
  WRITE:/ 'End(C)', I_MLCD-CALCAMT CURRENCY G_W, I_MLCD-CALCQTY.

  ULINE.
  WRITE:/ '***MLIT List***' COLOR 2.
  LOOP AT I_MLITEM WHERE BWKEY = I_MLCD-BWKEY
                     AND MATNR = I_MLCD-MATNR.
    WRITE:/ 'Sales:Internal consumption   ',
            	I_MLITEM-V2AMT  CURRENCY G_W, I_MLITEM-V2QTY.
    WRITE:/ 'Sales:Free of charge         ',
            	I_MLITEM-V3AMT  CURRENCY G_W, I_MLITEM-V3QTY.
    WRITE:/ 'Cost :Free of charge         ',
            	I_MLITEM-K3AMT  CURRENCY G_W, I_MLITEM-K3QTY.
    WRITE:/ 'Cost :physical inventory (-) ',
            	I_MLITEM-K4AMT  CURRENCY G_W, I_MLITEM-K4QTY.
    WRITE:/ 'Cost :physical inventory (+) ',
            	I_MLITEM-K5AMT  CURRENCY G_W, I_MLITEM-K5QTY.
  ENDLOOP.

  ULINE.

  WRITE:/ '***Diff List***' COLOR 2.
  PERFORM GET_MLIT_INFO.
  LOOP AT I_MLIT.
    SELECT SINGLE * FROM MLCRF
                   WHERE  BELNR = I_MLIT-BELNR
                     AND  KJAHR = I_MLIT-KJAHR
                     AND  POSNR = I_MLIT-POSNR
                     AND  BDATJ = I_MLIT-BDATJ
                     AND  POPER = I_MLIT-POPER
                     AND  CURTP = I_MLIT-CURTP.
    CHECK SY-SUBRC = 0.
    WRITE:/ MLCRF-BELNR,
	     MLCRF-KJAHR,
	     MLCRF-POSNR,
	     MLCRF-BDATJ,
	     MLCRF-POPER,
        MLCRF-CURTP,
	     MLCRF-FELDG,
	     MLCRF-PRDIF CURRENCY G_W,
	     MLCRF-KRDIF CURRENCY G_W.
  ENDLOOP.
ENDFORM.                    " AT_LINE_SELECTION
*&------------------------------------------------------------------
*&      Form  GET_MLIT_INFO.
*&------------------------------------------------------------------
FORM GET_MLIT_INFO.
  REFRESH I_MLIT.
  RANGES: P_DATUM FOR SY-DATUM.
  DATA: I2 TYPE I.

  P_DATUM-SIGN = 'I'.
  P_DATUM-OPTION = 'BT'.
  P_DATUM-LOW+0(04) = P_MJAHR.
  P_DATUM-LOW+4(02) = P_MONAT-LOW+1(2).
  P_DATUM-LOW+6(02) = '01'.

  P_DATUM-HIGH+0(04) = P_MJAHR.
  I2 = P_MONAT-HIGH+1(2).
  I2 = I2 + 1.
  IF I2 > 12.
  P_DATUM-HIGH+4(04) = '1231'.
  ELSE.
  P_DATUM-HIGH+4(02) = I2.
  P_DATUM-HIGH+6(02) = '01'.
  P_DATUM-HIGH = P_DATUM-HIGH - 1.
  ENDIF.

*FIXME : reverse doc????
  SELECT  MLIT~BELNR MLIT~KJAHR MLPP~POSNR MLPP~BDATJ
          MLPP~POPER MLCR~CURTP MLIT~MATNR MLIT~KALNR
          MLIT~BWKEY MLIT~MEINS MLIT~BEWARTGRP
          MLIT~PTYP_BVALT       MLIT~PTYP_PROC
          MLPP~LBKUM MLPP~BKLAS MLCR~SALK3 MLCR~WAERS MLIT~PSART
       INTO CORRESPONDING FIELDS OF TABLE I_MLIT
       FROM ( MLIT INNER JOIN MLPP
                           ON MLIT~BELNR = MLPP~BELNR
                              AND MLIT~KJAHR = MLPP~KJAHR
                              AND MLIT~POSNR = MLPP~POSNR
                   INNER JOIN MLCR
                           ON  MLPP~BELNR = MLCR~BELNR
                           AND MLPP~KJAHR = MLCR~KJAHR
                           AND MLPP~POSNR = MLCR~POSNR
                           AND MLPP~BDATJ = MLCR~BDATJ
                           AND MLPP~POPER = MLCR~POPER
                    INNER JOIN MLHD
                           ON  MLIT~BELNR = MLHD~BELNR
                           AND MLIT~KJAHR = MLHD~KJAHR )
       WHERE MLIT~MATNR = I_MLCD-MATNR
         AND MLIT~BWKEY = I_MLCD-BWKEY
         AND MLIT~KJAHR = P_MJAHR
*         AND MLIT~BEWARTGRP BETWEEN '40' AND '79'.
*         AND MLIT~BEWARTGRP <> SPACE
         AND MLHD~CPUDT BETWEEN P_DATUM-LOW AND P_DATUM-HIGH.

* CPU ??? ????...???..
ENDFORM.
*&------------------------------------------------------------------
*&      Form  calc_mlit_amt
*&------------------------------------------------------------------
FORM CALC_MLIT_AMT.
  I_MLITEM-BWKEY = I_MLCD-BWKEY.
  I_MLITEM-MATNR = I_MLCD-MATNR.

*Sales Order:sales (default)
*Sales Order:Internal consumption
*Sales Order:Free of charge
*Cost obj:Internal consumption (default)
*Cost obj:Free of charge
*Cost obj:scrap
*Cost obj:physical inventory +/-

  LOOP AT I_MLIT.
    CLEAR I_MLITEM.
    MOVE-CORRESPONDING I_MLIT TO I_MLITEM.
    COLLECT I_MLITEM.
  ENDLOOP.

  EXIT.
*FIXME
  LOOP AT I_MLIT.
    CLEAR I_MLITEM.
    MOVE-CORRESPONDING I_MLIT TO I_MLITEM.

    CASE I_MLIT-BEWARTGRP.
* Internal Use ??, ??, ????, ???? ?
      WHEN '50' OR '52' OR '54' OR '56' OR '58'
        OR '62' OR '64' OR '66' OR '70'.
        I_MLITEM-V2QTY = I_MLITEM-V2QTY + I_MLIT-LBKUM.
        I_MLITEM-V2AMT = I_MLITEM-V2AMT + I_MLIT-SALK3.
* Free of charge ????,??,???
      WHEN '48' OR '50' OR '68'.
        I_MLITEM-V3QTY = I_MLITEM-V3QTY + I_MLIT-LBKUM.
        I_MLITEM-V3AMT = I_MLITEM-V3AMT + I_MLIT-SALK3.

* Scrap
      WHEN '46'.
        I_MLITEM-K3QTY = I_MLITEM-K3QTY + I_MLIT-LBKUM.
        I_MLITEM-K3AMT = I_MLITEM-K3AMT + I_MLIT-SALK3.

* Physical Inventory / Backflush adj.
      WHEN '28' OR '30'.
        IF I_MLIT-LBKUM < 0.
          I_MLITEM-K4QTY = I_MLITEM-K4QTY + I_MLIT-LBKUM.
          I_MLITEM-K4AMT = I_MLITEM-K4AMT + I_MLIT-SALK3.
        ELSE.
          I_MLITEM-K5QTY = I_MLITEM-K5QTY + I_MLIT-LBKUM.
          I_MLITEM-K5AMT = I_MLITEM-K5AMT + I_MLIT-SALK3.
        ENDIF.
    ENDCASE.

*    collect i_mlitem.
*FIXME later
    PERFORM GET_MLCRF_AMT.
    CHECK SY-SUBRC = 0.
    I_MLCD-DIFAMT = I_MLCD-DIFAMT + IT_MLCRF-PRDIF + IT_MLCRF-KRDIF.

  ENDLOOP.

*  append i_mlitem.
ENDFORM.                    " calc_mlit_amt
*&------------------------------------------------------------------
*&      Form  get_mlcrf_amt
*&------------------------------------------------------------------
FORM GET_MLCRF_AMT.
* get diff. value
  READ TABLE IT_MLCRF WITH KEY BELNR = I_MLIT-BELNR
                               KJAHR = I_MLIT-KJAHR
                               POSNR = I_MLIT-POSNR
                               BDATJ = I_MLIT-BDATJ
                               POPER = I_MLIT-POPER
                               CURTP = I_MLIT-CURTP.
ENDFORM.                    " get_mlcrf_amt

* Single/Multi Level
*  data: h_run_id TYPE ckml_run_id,
*        h_error_belnr LIKE mlhd-belnr,
*        h_error_kjahr LIKE mlhd-kjahr,
*        h_reduced_bom TYPE ck_modif.
*  DATA: lh_error_in_settlement TYPE ck_xabrerr.
*  data: lt_docs type ckmd_t_document_report with header line.
*  refresh lt_docs.

*  IF I_MLCD-mlast = '3'.
** for calc. begin...
*    refresh lt_docs.
*    CALL FUNCTION 'CKMD_DOCUMENT_REPORT'
*         EXPORTING
*              i_kalnr               = I_MLCD-KALNR
*              i_bdatj               = L_BDATJ
*              i_poper               = P_MONAT-HIGH
**             i_untper              = h_untper
*              i_run_id              = h_run_id
*              i_only_not_mlcd_docs  = 'X'
**             I_NO_BUFFER           =
*              i_refresh_buffer      = space
*              i_online              = space
*         IMPORTING
*              e_error_in_settlement = lh_error_in_settlement
*              e_error_belnr         = h_error_belnr
*              e_error_kjahr         = h_error_kjahr
*              e_reduced_bom         = h_reduced_bom
*         TABLES
*              ot_docs               = lt_docs
*         EXCEPTIONS
*              no_document_found     = 1
*              OTHERS                = 2.
** EB : ???? ????
** AB :  "????????
*
*&--------------------------------------------------------------------
*&      Form  clear_mlcd
*&--------------------------------------------------------------------
FORM CLEAR_MLCD.
  I_MLCD-BAJAMT = 0.
  I_MLCD-BEGAMT = 0.
  I_MLCD-ENDAMT = 0.
  I_MLCD-SETAMT = 0.
  I_MLCD-NODAMT = 0.
  I_MLCD-INAMT = 0.
  I_MLCD-IOAMT = 0.
  I_MLCD-ONAMT = 0.
  I_MLCD-OOAMT = 0.

ENDFORM.                    " clear_mlcd
