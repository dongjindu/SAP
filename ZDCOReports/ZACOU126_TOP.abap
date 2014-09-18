*
*   INCLUDE ZACOU126_TOP                                               *
*----------------------------------------------------------------------*
TABLES : KEKO,
         ZTCOU103,
         ZTCO_SHOP_SUM,
         ZTCO_SHOP_CC,
         TKA01,
         MARA,
         ZTCOU105.

DATA : IT_ZTCOU103 LIKE TABLE OF ZTCOU103 WITH HEADER LINE.

DATA: BEGIN OF IT_ACTUAL OCCURS 0,
              KOKRS		LIKE ZTCO_SHOP_SUM-KOKRS,
              BDATJ		LIKE ZTCO_SHOP_SUM-BDATJ,	
              POPER		LIKE ZTCO_SHOP_SUM-POPER,
              ARTNR		LIKE ZTCO_SHOP_SUM-ARTNR,
              PAR_WERKS       LIKE ZTCO_SHOP_SUM-PAR_WERKS,
              LLV_MATNR		LIKE ZTCO_SHOP_SUM-LLV_MATNR,
*              KSTAR		LIKE ZTCO_SHOP_SUM-KSTAR,
              FEVOR		LIKE ZTCO_SHOP_SUM-FEVOR,
              BWKEY		LIKE ZTCO_SHOP_SUM-BWKEY,
              BWTAR		LIKE ZTCO_SHOP_SUM-BWTAR,
              KOSTL		LIKE ZTCO_SHOP_SUM-KOSTL,
              KALST		LIKE ZTCO_SHOP_SUM-KALST,
              MEEHT		LIKE ZTCO_SHOP_SUM-MEEHT,
              STPRS		LIKE ZTCO_SHOP_SUM-STPRS,
              VERPR		LIKE ZTCO_SHOP_SUM-VERPR,
              PEINH		LIKE ZTCO_SHOP_SUM-PEINH,
              MANU_AMT		LIKE ZTCO_SHOP_SUM-MANU_AMT,
              MANU_QTY		LIKE ZTCO_SHOP_SUM-MANU_QTY,
             $NET_A           LIKE ZTCO_SHOP_SUM-MANU_AMT, " for debug
              SINGLE_QTY      LIKE ZTCO_SHOP_SUM-SINGLE_QTY,
*             GR_QTY          LIKE ZTCO_SHOP_SUM-GR_QTY,
              PP_GR_QTY       LIKE ZTCO_SHOP_SUM-GR_QTY,

              ACT_QTY         LIKE ZTCO_SHOP_SUM-MANU_QTY,
              ACT_AMT         LIKE ZTCO_SHOP_SUM-MANU_AMT,
              ACT_S_QTY       LIKE ZTCO_SHOP_SUM-MANU_QTY, " Single Qty
              ACT_S_AMT       LIKE ZTCO_SHOP_SUM-MANU_AMT, " Single Amt
              VERID type VERID,
              CHK(1),

              ADD_MBGBTR type ZMENGE5,
              mbgbtr2 type  ZMENGE5,

      END OF IT_ACTUAL.

DATA IT_ACTUAL_MANU LIKE IT_ACTUAL OCCURS 0 WITH HEADER LINE .

TYPES: BEGIN OF TY_ROW_TAB,
              KOKRS		LIKE ZTCO_SHOP_SUM-KOKRS,
              BDATJ		LIKE ZTCO_SHOP_SUM-BDATJ,	
              POPER		LIKE ZTCO_SHOP_SUM-POPER,
              WERKS		LIKE ZTCOU103-WERKS,
              ARTNR           LIKE ZTCOU103-ARTNR,
              UPGVC           LIKE ZTCOU103-UPGVC,
              COMPN           LIKE ZTCOU103-COMPN,
              COMPN_A         LIKE ZTCOU103-COMPN, " Actual COMPN
              KSTAR           LIKE ZTCOU103-KSTAR,
              LIFNR           LIKE LFA1-LIFNR,
              VNDR_NAME       LIKE LFA1-NAME1,
              MEEHT           LIKE ZTCOU103-MEEHT,
              WAERS           LIKE TKA01-WAERS,
              PLN_QTY         LIKE ZTCOU103-MENGE,
              PLN_UNP         LIKE ZTCOU103-GPREIS,
              PLN_AMT         LIKE ZTCOU103-WERTN,
              T_PLN_Q         LIKE ZTCOU103-MENGE,

              ACT_QTY         LIKE ZTCO_SHOP_SUM-MANU_QTY,
              ACT_UNP         LIKE ZTCO_SHOP_SUM-VERPR,
              ACT_AMT         LIKE ZTCO_SHOP_SUM-MANU_AMT,

              ACT_S_QTY       LIKE ZTCO_SHOP_SUM-MANU_QTY, " Single Qty
              ACT_S_UNP       LIKE ZTCO_SHOP_SUM-VERPR,
              ACT_S_AMT       LIKE ZTCO_SHOP_SUM-MANU_AMT, " Single Amt

*             GR_QTY          LIKE ZTCO_SHOP_SUM-GR_QTY,
              PP_GR_QTY       LIKE ZTCO_SHOP_SUM-GR_QTY,

              DIF_QTY         LIKE ZTCO_SHOP_SUM-MANU_QTY,
              DIF_AMT         LIKE ZTCO_SHOP_SUM-MANU_AMT,
              UPGVC_T         LIKE MAKT-MAKTX,
              COMPN_T         LIKE MAKT-MAKTX,
* Value for analysis {
* AQ = Actual Quantity
* AP = Actual Price
* PP = Planed Price
* PQ = Planed Quantity
* PV = ( AQ * AP ) - ( AQ * PP )
* QV = ( PQ - AQ ) * PP
* RV = ( AQ - PQ ) * PP
* OV = Diff. - PV - QV - RV
              PV              LIKE ZTCO_SHOP_SUM-MANU_QTY,
              QV              LIKE ZTCO_SHOP_SUM-MANU_QTY,
              RV              LIKE ZTCO_SHOP_SUM-MANU_QTY,
              OV              LIKE ZTCO_SHOP_SUM-MANU_QTY,
* }
              MANU_AMT		LIKE ZTCO_SHOP_SUM-MANU_AMT,
              MANU_QTY		LIKE ZTCO_SHOP_SUM-MANU_QTY,

              MISS_ACT(1),
              MISS_PLN(1),
              VERID type VERID,

              MATNR2         TYPE MATNR,                    "UD1K949919
              MATNR1         TYPE MATNR,                    "UD1K949919
              MAKTX          TYPE MAKTX,                    "UD1K949919
              ZCATX          TYPE ZCATX,                    "UD1K949919

       END   OF  TY_ROW_TAB.

DATA : BEGIN OF IT_CKMLMV003 OCCURS 0,
         BWKEY      LIKE CKMLMV001-BWKEY,
         MATNR      LIKE CKMLMV001-MATNR,
         AUFNR      LIKE CKMLMV013-AUFNR,
         VERID_ND   LIKE CKMLMV001-VERID_ND,
         MEINH      LIKE CKMLMV003-MEINH,
         OUT_MENGE  LIKE CKMLMV003-OUT_MENGE,
       END OF  IT_CKMLMV003.

DATA : BEGIN OF IT_COLOR OCCURS 0,
         MATNR1 TYPE MATNR,
         MATNR2 TYPE MATNR,
       END OF IT_COLOR.

DATA : BEGIN OF IT_MATERIAL OCCURS 0,
         MATNR TYPE MATNR,
         WERKS LIKE MARC-WERKS,
         MTYPE(1) type c,
       END OF IT_MATERIAL.

DATA : BEGIN OF IT_MAT_TXT  OCCURS 0,
         MATNR      LIKE MARC-MATNR,
         WERKS      LIKE MARC-WERKS,
         RAUBE      LIKE MARA-RAUBE, "shop
         FEVOR      LIKE MARC-FEVOR, "PP schedule
         MTART      LIKE MARA-MTART,
         MATKL      LIKE MARA-MATKL,
         BISMT      LIKE MARA-BISMT,
         VSPVB      LIKE MARC-VSPVB,
         PRCTR      LIKE MARC-PRCTR,
         MAKTG      LIKE MAKT-MAKTG,
       END OF IT_MAT_TXT.

DATA : BEGIN OF IT_VND_MAT  OCCURS 0,
         MATNR LIKE S013-MATNR,
         LIFNR LIKE S013-LIFNR,
         WERKS LIKE MARC-WERKS,
       END OF IT_VND_MAT .

DATA : BEGIN OF IT_VENDOR  OCCURS 0,
         LIFNR LIKE S013-LIFNR,
         WERKS LIKE MARC-WERKS,
       END OF IT_VENDOR .

DATA : BEGIN OF IT_VNDR_NAME  OCCURS 0,
         LIFNR LIKE LFA1-LIFNR,
         NAME1 LIKE LFA1-NAME1,
       END OF IT_VNDR_NAME.

* Type for ALV
TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ROW_TAB.
TYPES: END OF TY_OUT.

DATA  : IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
        GT_OUT     TYPE TABLE OF TY_OUT     WITH HEADER LINE .

* for possible entry
DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA: DYNAME         TYPE PROGNAME,
      DYNUMB         TYPE SYCHAR04,
      EXC_EXCTAB     TYPE SLIS_T_EXTAB,
      POPUP_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      F              TYPE SLIS_FIELDCAT_ALV,
      SELFIELD       TYPE SLIS_SELFIELD,
      EXITFIELD,
      COLOR_ACTIVE(3)  VALUE 'C50',
      TABIX LIKE SY-TABIX.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DEFINE __PROCESS.
  PERFORM SHOW_PROGRESS USING &1 &2.
END-OF-DEFINITION.

DEFINE __MESSAGE.
  CALL FUNCTION 'POPUP_TO_INFORM'
       EXPORTING
            TITEL = &1
            TXT1  = &2
            TXT2  = SY-SUBRC.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

*----------------------------------------------------------------------*
* Others
*----------------------------------------------------------------------*
* ETC.
DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID.

DATA FROM_MATNR LIKE IT_ROW_TAB-COMPN. " for Following Parts.
DATA TO_MATNR LIKE IT_ROW_TAB-COMPN. " for Following Parts.

*&---------------------------------------------------------------------*
*&      Form  POPUP_KALKA
*&---------------------------------------------------------------------*
*       Possible Enter for Costing types
*----------------------------------------------------------------------*
FORM POPUP_KALKA USING PA_KALKA     TYPE CK_KALKA
                       P_FIELDNAME TYPE DYNFNAM.
  DATA: BEGIN OF LT_TCK02 OCCURS 0,
           KALKA TYPE CK_KALKA,
           TXKLA TYPE CK_TXKLA,
         END OF LT_TCK02.

  DATA: BEGIN OF FIELDS_TAB OCCURS 1,
            KALKA TYPE CK_KALKA,
            TXKLA TYPE CK_TXKLA,
            COLOR(3),
         END OF FIELDS_TAB.

  CLEAR: DYNPFIELDS, DYNAME, DYNUMB, EXC_EXCTAB, POPUP_FIELDCAT,
         F, SELFIELD, EXITFIELD, COLOR_ACTIVE, TABIX, FIELDS_TAB.
  REFRESH: DYNPFIELDS, FIELDS_TAB.

  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  APPEND DYNPFIELDS.

  DYNAME = SY-REPID.
  DYNUMB = SY-DYNNR.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME             = DYNAME
            DYNUMB             = DYNUMB
            TRANSLATE_TO_UPPER = 'X'
       TABLES
            DYNPFIELDS         = DYNPFIELDS
       EXCEPTIONS
            OTHERS             = 9.

  CLEAR LT_TCK02.
  REFRESH LT_TCK02.

  SELECT KALKA TXKLA INTO TABLE LT_TCK02
    FROM TCK02
   WHERE SPRAS = SY-LANGU.

  DELETE LT_TCK02
    WHERE NOT ( KALKA+0(1) = 'U' OR
                KALKA+0(1) = 'M' OR
                KALKA+0(1) = 'B' OR
                KALKA+0(1) = 'R' ).

  SORT LT_TCK02 BY KALKA.

  F-REPTEXT_DDIC  = 'Costing Type'.
  F-FIELDNAME = 'KALKA'.
  F-OUTPUTLEN = 2.
  APPEND F TO POPUP_FIELDCAT.
  CLEAR F.

  F-REPTEXT_DDIC = 'Desc.'.
  F-FIELDNAME = 'TXKLA'.

  DESCRIBE FIELD FIELDS_TAB-TXKLA LENGTH F-OUTPUTLEN.
  APPEND F TO POPUP_FIELDCAT.

* Excluding-Table
  APPEND: '%SC ' TO EXC_EXCTAB,       " Search
          '%SC+' TO EXC_EXCTAB,       " Search+
          '&OUP' TO EXC_EXCTAB,       " Sort Up
          '&ODN' TO EXC_EXCTAB,       " Sort Dn
          '&ILT' TO EXC_EXCTAB,       " Filter
          '&OL0' TO EXC_EXCTAB.

* Popup
  TABIX = SY-TABIX.

  LOOP AT LT_TCK02.
    FIELDS_TAB-KALKA = LT_TCK02-KALKA.
    FIELDS_TAB-TXKLA = LT_TCK02-TXKLA.
    APPEND FIELDS_TAB.
    CLEAR FIELDS_TAB.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            I_LINEMARK_FIELDNAME    = 'COLOR'
            I_TABNAME               = 'FIELDS_TAB'
            IT_FIELDCAT             = POPUP_FIELDCAT
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND_POPUP_LIGHTS_N'
            I_CALLBACK_PROGRAM      = DYNAME
            IT_EXCLUDING            = EXC_EXCTAB
       IMPORTING
            ES_SELFIELD             = SELFIELD
            E_EXIT                  = EXITFIELD
       TABLES
            T_OUTTAB                = FIELDS_TAB.

  READ TABLE FIELDS_TAB INDEX TABIX.
  CLEAR FIELDS_TAB-COLOR.
  MODIFY FIELDS_TAB INDEX TABIX.

  IF EXITFIELD IS INITIAL.
    READ TABLE FIELDS_TAB INDEX SELFIELD-TABINDEX.
    PA_KALKA = FIELDS_TAB-KALKA.

    DYNPFIELDS-FIELDNAME = P_FIELDNAME.
    DYNPFIELDS-FIELDVALUE = FIELDS_TAB-KALKA.
    APPEND DYNPFIELDS.

    DYNAME = SY-REPID.
    DYNUMB = SY-DYNNR.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME     = DYNAME
              DYNUMB     = DYNUMB
         TABLES
              DYNPFIELDS = DYNPFIELDS.

  ENDIF.

ENDFORM.                    " POPUP_KALKA
