*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120031469 0000234174                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZML_VALUE_FLOW_ANALYZER
*& Object Header   PROG ZML_VALUE_FLOW_ANALYZER
*&-------------------------------------------------------------------*
*& REPORT ZML_VALUE_FLOW_ANALYZER
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report           ZML_VALUE_FLOW_ANALYZER                         *
*&                                                                     *
*&---------------------------------------------------------------------*

*include zmlvfatop.
*&---------------------------------------------------------------------*
*& Include ZMLVFATOP                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT ZACO11U_MLVA MESSAGE-ID C+ .

TYPE-POOLS: SLIS, CKMV0.

TABLES: MLKEY, MARA, MARV, MARC, MBEW, TCURM, T001, T001K, CKMLHD,
        CKMLPP, CKMLRUNPERIOD, SSCRFIELDS.

RANGES: R_BWKEY FOR CKMLHD-BWKEY.      "Hilfsrange Bewertungskreis

TYPES:
* Material/valuated sales order/project stock data
  BEGIN OF S_MATS,
     KALNR TYPE CKMLHD-KALNR,
     MATNR TYPE CKMLHD-MATNR,
     BWKEY TYPE CKMLHD-BWKEY,
     BWTAR TYPE CKMLHD-BWTAR,
     SOBKZ TYPE CKMLHD-SOBKZ,
     VBELN TYPE CKMLHD-VBELN,
     POSNR TYPE CKMLHD-POSNR,
     PSPNR TYPE CKMLHD-PSPNR,
     BKLAS TYPE MBEW-BKLAS,
     MTART TYPE MARA-MTART,
     MATKL TYPE MARA-MATKL,
     SPART TYPE MARA-SPART,
     PRCTR TYPE MARC-PRCTR,
     MEINS TYPE MARA-MEINS,
  END OF S_MATS,
  TY_MATS TYPE STANDARD TABLE OF S_MATS WITH KEY KALNR,
* Output
  BEGIN OF S_OUT,
     KALNR TYPE CKMLHD-KALNR,
     BDATJ TYPE CKMLPP-BDATJ,
     POPER TYPE CKMLPP-POPER,
     UNTPER TYPE CKMLPP-UNTPER,
     CURTP TYPE CKMLCR-CURTP,
     MATNR TYPE CKMLHD-MATNR,
     BWKEY TYPE CKMLHD-BWKEY,
     BWTAR TYPE CKMLHD-BWTAR,
     VBELN TYPE CKMLHD-VBELN,
     POSNR TYPE CKMLHD-POSNR,
     PSPNR TYPE CKMLHD-PSPNR,
     BKLAS TYPE MBEW-BKLAS,
     MTART TYPE MARA-MTART,
     MATKL TYPE MARA-MATKL,
     SPART TYPE MARA-SPART,
     PRCTR TYPE MARC-PRCTR,
     MEINS TYPE CKMLPP-MEINS,
     STATUS TYPE CKMLPP-STATUS,
     LBKUM TYPE CKMLPP-LBKUM,
     MENGE TYPE KKB_ML_MENGE,
     PBPOPO TYPE CKMLPP-PBPOPO,
     SALK3 TYPE CKMLCR-SALK3,
     WERT TYPE KKB_ML_BEWER,
     STPRS TYPE CKMLCR-STPRS,
     PVPRS TYPE CKMLCR-PVPRS,
     PEINH TYPE CKMLCR-PEINH,
     WAERS TYPE CKMLCR-WAERS,
     PBPRD_O TYPE CKMLCR-PBPRD_O,
     PBKDM_O TYPE CKMLCR-PBKDM_O,
     ESTPRD TYPE CKML_ESTPRD,
     ESTKDM TYPE CKML_ESTKDM,
     MSTPRD TYPE CKML_MSTPRD,
     MSTKDM TYPE CKML_MSTKDM,
     ESTDIF TYPE CK_SINGLELEVEL_DIF,
     MSTDIF TYPE CK_MULTILEVEL_DIF,
     PRDIF TYPE CK_SUM_PRDIF,
     KRDIF TYPE CK_SUM_KRDIF,
     SUMDIF TYPE CK_SUM_DIF,
     POS_TYPE(3),
     POS_TYPE_TEXT(40),
     COLOR(3) TYPE C,
   END OF S_OUT,
   TY_OUT TYPE STANDARD TABLE OF S_OUT WITH KEY KALNR.

*----------------------------------------------------------------------*
*     Tabellen
*----------------------------------------------------------------------*
DATA: T_T001K LIKE T001K OCCURS 0 WITH HEADER LINE,
      T_OUT TYPE TY_OUT,
      T_MATS TYPE TY_MATS,
      T_MATS_PORTION TYPE TY_MATS,
      T_CKMLPP TYPE STANDARD TABLE OF CKMLPP
               WITH KEY KALNR BDATJ POPER
               WITH HEADER LINE,
      T_CKMLCR TYPE STANDARD TABLE OF CKMLCR
               WITH KEY KALNR BDATJ POPER CURTP
               WITH HEADER LINE,
      T_MLCD TYPE STANDARD TABLE OF MLCD
               WITH KEY KALNR BDATJ POPER UNTPER CATEG PTYP BVALT CURTP
               WITH HEADER LINE,
      T_MLCD_NOT_ALLOC TYPE STANDARD TABLE OF MLCD
               WITH KEY KALNR BDATJ POPER UNTPER CATEG PTYP BVALT CURTP
               WITH HEADER LINE,
      T_PLANTS TYPE CKML_RUN_T_PLANT.

*----------------------------------------------------------------------*
*     Feldleisten                                                      *

*----------------------------------------------------------------------*
DATA: S_RUNPERIOD TYPE CKML_RUN_PERIOD_DATA,
      S_MATS TYPE S_MATS.

*----------------------------------------------------------------------*
*     Globale Hilfsfelder                                              *
*----------------------------------------------------------------------*
DATA: H_LAST_BWKEY TYPE BWKEY,
      H_SELE_LAUF TYPE BOOLE_D,
      H_EXPAN TYPE BOOLE_D,
      H_LINES TYPE I,
      H_INDEX TYPE SY-TABIX,
      H_COUNTER TYPE I,
      H_PORTION_SIZE TYPE I VALUE 100.


INCLUDE LCKM0TOP_STATUS.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*

*include zmlvfa_para.
SELECTION-SCREEN SKIP 2.

SELECT-OPTIONS: R_MATNR FOR MLKEY-MATNR ,
                R_BUKRS FOR MLKEY-BUKRS NO-EXTENSION NO INTERVALS ,
                R_WERKS FOR MLKEY-WERKS NO-EXTENSION NO INTERVALS ,
                R_BWTAR FOR MLKEY-BWTAR NO-EXTENSION NO INTERVALS .

SELECTION-SCREEN SKIP .
SELECTION-SCREEN BEGIN OF BLOCK SELE WITH FRAME TITLE TEXT-011.
* Lauf
SELECTION-SCREEN BEGIN OF BLOCK LAUF WITH FRAME TITLE TEXT-001.
PARAMETERS: P_LAUF LIKE CKMLRUNPERIOD-RUN_TYPE
                        MODIF ID LAU.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) TEXT-009 FOR FIELD P_LPOP MODIF ID LAU.
PARAMETERS: P_LPOP LIKE CKMLRUNPERIOD-POPER  MODIF ID LAU,
            P_LGJA LIKE CKMLRUNPERIOD-GJAHR  MODIF ID LAU.
SELECTION-SCREEN COMMENT 40(1) TEXT-010 FOR FIELD P_LPOP MODIF ID LAU.
PARAMETERS: P_APPL LIKE CKMLRUNPERIOD-APPL MODIF ID LAU.
SELECTION-SCREEN END OF LINE.
PARAMETERS: P_LDAY LIKE CKMLRUNPERIOD-LAST_DAY NO-DISPLAY MODIF ID LAU.
SELECTION-SCREEN END OF BLOCK LAUF.
* Periode
SELECTION-SCREEN BEGIN OF BLOCK PERIODE WITH FRAME TITLE TEXT-003.
PARAMETERS: P_POPER LIKE CKI_DOC_ML-SL_PERIODE MODIF ID PER,
            P_BDATJ LIKE MLKEY-BDATJ           MODIF ID PER.
SELECTION-SCREEN END OF BLOCK PERIODE.
SELECTION-SCREEN PUSHBUTTON /1(25) KNOPF USER-COMMAND SELE.
SELECTION-SCREEN END OF BLOCK SELE.

SELECTION-SCREEN BEGIN OF BLOCK RADIO WITH FRAME TITLE TEXT-004.
PARAMETERS: P_NOTDIS RADIOBUTTON GROUP G DEFAULT 'X',
            P_NOTINC RADIOBUTTON GROUP G,
            P_ALL RADIOBUTTON GROUP G.
SELECTION-SCREEN END OF BLOCK RADIO.

SELECTION-SCREEN PUSHBUTTON /1(30) EXPAN USER-COMMAND EXPAN.
SELECTION-SCREEN BEGIN OF BLOCK EXTRA WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS: R_VBELN FOR  CKMLHD-VBELN  MODIF ID PUK,
                R_POSNR FOR  CKMLHD-POSNR  MODIF ID PUK,
                R_PSPNR FOR  CKMLHD-PSPNR  MODIF ID PUK,
                R_BKLAS FOR  MBEW-BKLAS    MODIF ID PUK,
                R_MTART FOR  MARA-MTART    MODIF ID PUK,
                R_MATKL FOR  MARA-MATKL    MODIF ID PUK,
                R_SPART FOR  MARA-SPART    MODIF ID PUK,
                R_PRCTR FOR  MARC-PRCTR    MODIF ID PUK.
SELECTION-SCREEN END OF BLOCK EXTRA.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*

***************************
AT SELECTION-SCREEN OUTPUT.
***************************

*    LOOP AT SCREEN.
*       IF SCREEN-GROUP1 = 'LAU'.
*         SCREEN-INVISIBLE = '0'.
*         SCREEN-ACTIVE    = '1'.
*         MODIFY SCREEN.
*       ENDIF.
*    ENDLOOP.
*

*  IF TCURM-BWKRS_CUS IS INITIAL.
*    READ TABLE TCURM.
*  ENDIF.
** Display Select-Option R_WERKS or R_BUKRS
*  IF TCURM-BWKRS_CUS = '3'.            "Bewertungsebene BURKS
*    LOOP AT SCREEN.
*      IF SCREEN-NAME CS 'R_WERKS'.
*        SCREEN-INVISIBLE = '1'.
*        SCREEN-ACTIVE    = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSE.                                                     "sonst
*    LOOP AT SCREEN.
*      IF SCREEN-NAME CS 'R_BUKRS'.
*        SCREEN-INVISIBLE = '1'.
*        SCREEN-ACTIVE    = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*  IF NOT H_SELE_LAUF IS INITIAL.
*    KNOPF = TEXT-003.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'PER'.
*        SCREEN-INVISIBLE = '1'.
*        SCREEN-ACTIVE    = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    KNOPF = TEXT-001.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'LAU'.
*        SCREEN-INVISIBLE = '1'.
*        SCREEN-ACTIVE    = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*  IF NOT H_EXPAN IS INITIAL.
*    CONCATENATE TEXT-013 TEXT-002
*                INTO EXPAN SEPARATED BY SPACE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'PUK'.
*        SCREEN-INVISIBLE = '0'.
*        SCREEN-ACTIVE    = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    REFRESH: R_VBELN, R_POSNR, R_PSPNR, R_BKLAS, R_MTART, R_MATKL,
*             R_SPART, R_PRCTR.
*    CONCATENATE TEXT-012 TEXT-002
*                INTO EXPAN SEPARATED BY SPACE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'PUK'.
*        SCREEN-INVISIBLE = '1'.
*        SCREEN-ACTIVE    = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

********************
AT SELECTION-SCREEN.
********************
  IF NOT R_BUKRS[] IS INITIAL.
    R_BWKEY[] = R_BUKRS[].
  ELSE.
    R_BWKEY[] = R_WERKS[].
  ENDIF.
  IF SSCRFIELDS-UCOMM = 'SELE'.
    IF H_SELE_LAUF IS INITIAL.
      H_SELE_LAUF = 'X'.
    ELSE.
      CLEAR: H_SELE_LAUF, P_LAUF.
    ENDIF.
  ENDIF.
  IF SSCRFIELDS-UCOMM = 'EXPAN'.
    IF H_EXPAN IS INITIAL.
      H_EXPAN = 'X'.
    ELSE.
      CLEAR: H_EXPAN.
      REFRESH: R_VBELN, R_POSNR, R_PSPNR, R_BKLAS, R_MTART, R_MATKL,
               R_SPART, R_PRCTR.
    ENDIF.
  ENDIF.

*******************
START-OF-SELECTION.
*******************

  IF NOT P_LAUF IS INITIAL.
    CALL FUNCTION 'CKML_RUN_PERIOD_GET'
      EXPORTING
*       I_RUN_ID               =
        I_RUN_TYPE             = P_LAUF
*       I_LAST_DAY             = p_lday
*       I_LANGU                = SY-LANGU
        I_POPER                = P_LPOP
        I_GJAHR                = P_LGJA
        I_APPL                 = P_APPL
      IMPORTING
        ES_RUNPERIOD           = S_RUNPERIOD
      EXCEPTIONS
        RUN_NOT_EXISTENT       = 1
        OTHERS                 = 2.
    IF SY-SUBRC <> 0.
      CLEAR: S_RUNPERIOD.
      MESSAGE E112(CKMLRUN) WITH P_LAUF P_LPOP P_LGJA P_APPL.
*      SUBMIT ZML_VALUE_FLOW_ANALYZER VIA SELECTION-SCREEN.
    ELSE.
      P_BDATJ = S_RUNPERIOD-GJAHR.
      P_POPER = S_RUNPERIOD-POPER.
      CALL FUNCTION 'CKML_RUN_PLANTS_GET'
        EXPORTING
          I_RUN_ID               = S_RUNPERIOD-RUN_ID
*         I_RUN_TYPE             =
*         I_LAST_DAY             =
*         I_POPER                =
*         I_GJAHR                =
*         I_APPL                 = CKRU0_CO_APPL_ACT
        IMPORTING
          ET_PLANTS              = T_PLANTS
        EXCEPTIONS
          RUN_NOT_EXISTENT       = 1
          NO_PLANTS              = 2
          OTHERS                 = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH: T_T001K.

* Bewertungskreise bestimmen
* ACHTUNG! Funktioniert so nicht mehr, wenn Bewertung auf Buchungskreis!
  IF NOT T_PLANTS IS INITIAL.
    SELECT BWKEY BUKRS MLBWA FROM T001K
                             INTO CORRESPONDING FIELDS OF TABLE T_T001K
                             FOR ALL ENTRIES IN T_PLANTS
                             WHERE BWKEY = T_PLANTS-WERKS
                             AND   BWKEY IN R_BWKEY.
  ELSE.
    SELECT BWKEY BUKRS MLBWA FROM T001K
                             INTO CORRESPONDING FIELDS OF TABLE T_T001K
                             WHERE BWKEY IN R_BWKEY.
  ENDIF.
  LOOP AT T_T001K.
    REFRESH: T_MATS, T_MATS_PORTION, T_CKMLPP, T_CKMLCR.
    IF NOT T_T001K-MLBWA IS INITIAL.
      PERFORM GET_MATERIALS USING T_T001K
                            CHANGING P_BDATJ
                                     P_POPER
                                     T_MATS.
*     Portionieren!
      DESCRIBE TABLE T_MATS LINES H_LINES.
      LOOP AT T_MATS INTO S_MATS.
        H_INDEX = SY-TABIX.
        H_COUNTER = H_COUNTER + 1.
        APPEND S_MATS TO T_MATS_PORTION.
        IF H_COUNTER >= H_PORTION_SIZE OR H_INDEX = H_LINES.
          PERFORM GET_MATERIAL_PERIODS USING T_MATS_PORTION
                                             T_CKMLPP[]
                                             T_CKMLCR[].
          PERFORM GET_MLCD_DATA USING T_MATS_PORTION
                                CHANGING T_MLCD[]
                                         T_MLCD_NOT_ALLOC[].
          PERFORM FIND_BAD_BOYS USING T_MATS_PORTION
                                      T_CKMLPP[]
                                      T_CKMLCR[]
                                      T_MLCD[]
                                      T_MLCD_NOT_ALLOC[]
                                CHANGING T_OUT[].
          CALL FUNCTION 'CKMS_BUFFER_REFRESH_COMPLETE'.
          REFRESH: T_MATS_PORTION, T_CKMLPP, T_CKMLCR, T_MLCD,
                   T_MLCD_NOT_ALLOC.
          CLEAR: H_COUNTER.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

* Ausgabe
*  IF NOT T_OUT[] IS INITIAL.
*     perform ausgabe.
*  ELSE.
*    MESSAGE E154.
*    IF SY-BATCH IS INITIAL.
*     SUBMIT ZML_VALUE_FLOW_ANALYZER VIA SELECTION-SCREEN.
*    ENDIF.
*  ENDIF.
  FREE MEMORY ID 'HLV'.
  EXPORT T_OUT   = T_OUT
         TO MEMORY ID 'HLV'.

*----------------------------------------------------------------------*
************************************************************************
*    FORM-Routinen                                                     *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
FORM GET_MATERIALS USING    PF_T001K TYPE T001K
                   CHANGING PL_BDATJ TYPE BDATJ
                            PL_POPER TYPE POPER
                            PT_MATS LIKE T_MATS[].

  DATA: LT_KALNR TYPE CKMV0_MATOBJ_TBL,
        LS_MATS TYPE S_MATS,
        LS_KALNR TYPE CKMV0_MATOBJ_STR.

* Aktuelle Periode als Default / Hausw?rung lesen
  IF PL_BDATJ IS INITIAL OR PL_POPER IS INITIAL.
    IF MARV-BUKRS <> PF_T001K-BUKRS.
      SELECT SINGLE * FROM MARV WHERE BUKRS = PF_T001K-BUKRS.
      IF SY-SUBRC <> 0.
        CLEAR: MARV.
      ENDIF.
    ENDIF.
    IF NOT MARV IS INITIAL.
      PL_BDATJ = MARV-LFGJA.
      IF PL_POPER IS INITIAL.
        PL_POPER = MARV-LFMON.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'T001_SINGLE_READ'
      EXPORTING
*       KZRFB            = ' '
*       MAXTZ            = 0
        BUKRS            = PF_T001K-BUKRS
      IMPORTING
        WT001            = T001
      EXCEPTIONS
        NOT_FOUND        = 1
        WRONG_CALL       = 2
        OTHERS           = 3.
    IF SY-SUBRC <> 0.
      CLEAR: T001.
    ENDIF.
  ENDIF.
  REFRESH: PT_MATS.
  IF NOT R_VBELN IS INITIAL.
    SELECT H~BWKEY H~KALNR H~MATNR H~BWTAR
           H~SOBKZ H~VBELN H~POSNR H~PSPNR
           M~MTART M~MATKL M~SPART M~MEINS C~PRCTR B~BKLAS
       INTO CORRESPONDING FIELDS OF TABLE PT_MATS
       FROM ( ( CKMLHD AS H JOIN MARA AS M
              ON H~MATNR = M~MATNR )
              JOIN MARC AS C
              ON H~MATNR = C~MATNR AND H~BWKEY = C~WERKS )
            JOIN EBEW AS B
            ON H~MATNR = B~MATNR AND H~BWKEY = B~BWKEY
            AND H~BWTAR = B~BWTAR AND H~SOBKZ = B~SOBKZ
            AND H~POSNR = B~POSNR
       WHERE H~BWKEY = PF_T001K-BWKEY
       AND   H~MATNR IN R_MATNR
       AND   H~BWTAR IN R_BWTAR
       AND   H~VBELN IN R_VBELN
       AND   H~POSNR IN R_POSNR
       AND   H~PSPNR IN R_PSPNR
       AND   H~MLAST = '3'
       AND   B~BKLAS IN R_BKLAS
       AND   M~MTART IN R_MTART
       AND   M~MATKL IN R_MATKL
       AND   M~SPART IN R_SPART
       AND   C~PRCTR IN R_PRCTR.
  ELSEIF NOT R_PSPNR IS INITIAL.
    SELECT H~BWKEY H~KALNR H~MATNR H~BWTAR
           H~SOBKZ H~VBELN H~POSNR H~PSPNR
           M~MTART M~MATKL M~SPART M~MEINS C~PRCTR B~BKLAS
       INTO CORRESPONDING FIELDS OF TABLE PT_MATS
       FROM ( ( CKMLHD AS H JOIN MARA AS M
              ON H~MATNR = M~MATNR )
              JOIN MARC AS C
              ON H~MATNR = C~MATNR AND H~BWKEY = C~WERKS )
            JOIN QBEW AS B
            ON H~MATNR = B~MATNR AND H~BWKEY = B~BWKEY
            AND H~BWTAR = B~BWTAR AND H~SOBKZ = B~SOBKZ
            AND H~PSPNR = B~PSPNR
       WHERE H~BWKEY = PF_T001K-BWKEY
       AND   H~MATNR IN R_MATNR
       AND   H~BWTAR IN R_BWTAR
       AND   H~VBELN IN R_VBELN
       AND   H~POSNR IN R_POSNR
       AND   H~PSPNR IN R_PSPNR
       AND   H~MLAST = '3'
       AND   B~BKLAS IN R_BKLAS
       AND   M~MTART IN R_MTART
       AND   M~MATKL IN R_MATKL
       AND   M~SPART IN R_SPART
       AND   C~PRCTR IN R_PRCTR.
  ELSE.
    SELECT H~BWKEY H~KALNR H~MATNR H~BWTAR
           H~SOBKZ H~VBELN H~POSNR H~PSPNR
           M~MTART M~MATKL M~SPART M~MEINS C~PRCTR B~BKLAS
       INTO CORRESPONDING FIELDS OF TABLE PT_MATS
       FROM ( ( CKMLHD AS H JOIN MARA AS M
              ON H~MATNR = M~MATNR )
              JOIN MARC AS C
              ON H~MATNR = C~MATNR AND H~BWKEY = C~WERKS )
            JOIN MBEW AS B
            ON H~MATNR = B~MATNR AND H~BWKEY = B~BWKEY
            AND H~BWTAR = B~BWTAR
       WHERE H~BWKEY = PF_T001K-BWKEY
       AND   H~MATNR IN R_MATNR
       AND   H~BWTAR IN R_BWTAR
       AND   H~VBELN IN R_VBELN
       AND   H~POSNR IN R_POSNR
       AND   H~PSPNR IN R_PSPNR
       AND   H~MLAST = '3'
       AND   B~BKLAS IN R_BKLAS
       AND   M~MTART IN R_MTART
       AND   M~MATKL IN R_MATKL
       AND   M~SPART IN R_SPART
       AND   C~PRCTR IN R_PRCTR.
  ENDIF.

ENDFORM.                               " get_materials
*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
FORM FIND_BAD_BOYS USING PT_MATS LIKE T_MATS[]
                         PT_CKMLPP LIKE T_CKMLPP[]
                         PT_CKMLCR LIKE T_CKMLCR[]
                         PT_MLCD LIKE T_MLCD[]
                         PT_MLCD_NOT_ALLOC LIKE T_MLCD[]
                   CHANGING PT_OUT LIKE T_OUT[].

  DATA: LS_OUT_NDI TYPE S_OUT,
        LS_OUT_CUM TYPE S_OUT,
        LS_OUT_NIN TYPE S_OUT,
        LS_MATS TYPE S_MATS,
        LS_CKMLPP TYPE CKMLPP,
        LS_CKMLCR TYPE CKMLCR,
        LS_MLCD TYPE MLCD,
        LS_MLCD_NOT_ALLOC TYPE MLCD,
        L_COLOR(3) TYPE C,
        L_AB_MENGE LIKE MLCD-LBKUM,
        L_NIN TYPE BOOLE_D,
        L_KALNR_OLD LIKE MLCD-KALNR.

  LOOP AT PT_MATS INTO LS_MATS.
    READ TABLE PT_CKMLPP INTO LS_CKMLPP
                         WITH KEY KALNR = LS_MATS-KALNR
                                  BDATJ = P_BDATJ
                                  POPER = P_POPER
                                  UNTPER = S_RUNPERIOD-UNTPER
                                  BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    CLEAR: LS_OUT_NDI, LS_OUT_CUM, LS_OUT_NIN.
    MOVE-CORRESPONDING LS_CKMLPP TO LS_OUT_NDI.
    MOVE-CORRESPONDING LS_CKMLPP TO LS_OUT_CUM.
    MOVE-CORRESPONDING LS_CKMLPP TO LS_OUT_NIN.
    IF LS_CKMLPP-STATUS >= Y_EINSTUFIG_ABGERECHNET.
      READ TABLE PT_CKMLCR WITH KEY KALNR = LS_MATS-KALNR
                                    BDATJ = P_BDATJ
                                    POPER = P_POPER
                                    UNTPER = S_RUNPERIOD-UNTPER
                                    BINARY SEARCH
                                    TRANSPORTING NO FIELDS.
      LOOP AT PT_CKMLCR INTO LS_CKMLCR FROM SY-TABIX.
        IF LS_CKMLCR-KALNR <> LS_CKMLPP-KALNR OR
           LS_CKMLCR-BDATJ <> LS_CKMLPP-BDATJ OR
           LS_CKMLCR-POPER <> LS_CKMLPP-POPER OR
           LS_CKMLCR-UNTPER <> LS_CKMLPP-UNTPER.
          EXIT.
        ENDIF.
*       Kumulierter Bestand
        MOVE-CORRESPONDING LS_CKMLCR TO LS_OUT_NDI.
        MOVE-CORRESPONDING LS_CKMLCR TO LS_OUT_CUM.
        MOVE-CORRESPONDING LS_CKMLCR TO LS_OUT_NIN.
        LS_OUT_CUM-ESTPRD = LS_CKMLCR-ABPRD_O + LS_CKMLCR-ZUPRD_O +
                            LS_CKMLCR-VPPRD_O.
        LS_OUT_CUM-ESTKDM = LS_CKMLCR-ABKDM_O + LS_CKMLCR-ZUKDM_O +
                            LS_CKMLCR-VPKDM_O.
*       ACHTUNG: Bei Status "Abschluss storniert" k?nte das falsch
*       sein, da evtl. nur einstufig preisermittelt wurde!
        IF LS_CKMLPP-STATUS >= Y_MEHRSTUFIG_ABGERECHNET.
          LS_OUT_CUM-MSTPRD = LS_CKMLCR-ABPRD_MO + LS_CKMLCR-ZUPRD_MO.
          LS_OUT_CUM-MSTKDM = LS_CKMLCR-ABKDM_MO + LS_CKMLCR-ZUKDM_MO.
        ELSE.
          LS_OUT_CUM-MSTPRD = LS_CKMLCR-ABPRD_MO.
          LS_OUT_CUM-MSTKDM = LS_CKMLCR-ABKDM_MO.
        ENDIF.
*       Gibt's nicht verteilte Differenzen?
        LS_OUT_NDI-ESTPRD = LS_OUT_CUM-ESTPRD.
        LS_OUT_NDI-ESTKDM = LS_OUT_CUM-ESTKDM.
        LS_OUT_NDI-MSTPRD = LS_OUT_CUM-MSTPRD.
        LS_OUT_NDI-MSTKDM = LS_OUT_CUM-MSTKDM.
        LS_OUT_NDI-ESTPRD = LS_OUT_NDI-ESTPRD -
                            ( LS_CKMLCR-VNPRD_EA + LS_CKMLCR-EBPRD_EA ).
        LS_OUT_NDI-ESTKDM = LS_OUT_NDI-ESTKDM -
                            ( LS_CKMLCR-VNKDM_EA + LS_CKMLCR-EBKDM_EA ).
        LS_OUT_NDI-MSTPRD = LS_OUT_NDI-MSTPRD -
                            ( LS_CKMLCR-VNPRD_MA + LS_CKMLCR-EBPRD_MA ).
        LS_OUT_NDI-MSTKDM = LS_OUT_NDI-MSTKDM -
                            ( LS_CKMLCR-VNKDM_MA + LS_CKMLCR-EBKDM_MA ).
        LS_OUT_NDI-SUMDIF = LS_OUT_NDI-ESTPRD + LS_OUT_NDI-ESTKDM +
                            LS_OUT_NDI-MSTPRD + LS_OUT_NDI-MSTKDM.
*       Gibt's eine 'Nicht verrechnet'-Zeile?
        READ TABLE PT_MLCD_NOT_ALLOC INTO LS_MLCD_NOT_ALLOC
                                     WITH KEY KALNR = LS_CKMLCR-KALNR
                                              BDATJ = LS_CKMLCR-BDATJ
                                              POPER = LS_CKMLCR-POPER
                                             UNTPER = LS_CKMLCR-UNTPER
                                              CURTP = LS_CKMLCR-CURTP
                                              BINARY SEARCH.
        IF SY-SUBRC = 0.
          L_NIN = 'X'.
        ELSE.
          CLEAR: L_NIN.
        ENDIF.
        IF NOT LS_OUT_NDI-SUMDIF IS INITIAL OR
           NOT L_NIN IS INITIAL.
          IF LS_OUT_NDI-KALNR <> L_KALNR_OLD.
            L_KALNR_OLD = LS_OUT_NDI-KALNR.
            IF L_COLOR = 'C21'.
              L_COLOR = 'C20'.
            ELSE.
              L_COLOR = 'C21'.
            ENDIF.
          ENDIF.
          READ TABLE PT_MATS INTO LS_MATS
                             WITH KEY KALNR = LS_OUT_NDI-KALNR.
          IF SY-SUBRC = 0.
            MOVE-CORRESPONDING LS_MATS TO LS_OUT_CUM.
            MOVE-CORRESPONDING LS_MATS TO LS_OUT_NDI.
            MOVE-CORRESPONDING LS_MATS TO LS_OUT_NIN.
          ENDIF.
          IF ( NOT P_NOTDIS IS INITIAL OR NOT P_ALL IS INITIAL ) AND
             NOT LS_OUT_NDI-SUMDIF IS INITIAL.
            LS_OUT_NDI-POS_TYPE = 'NDI'.
            LS_OUT_NDI-COLOR = L_COLOR.
            LS_OUT_NDI-POS_TYPE_TEXT = TEXT-006.
            CLEAR: LS_OUT_NDI-MENGE, LS_OUT_NDI-WERT.
            LS_OUT_NDI-PRDIF = LS_OUT_NDI-ESTPRD + LS_OUT_NDI-MSTPRD.
            LS_OUT_NDI-KRDIF = LS_OUT_NDI-ESTKDM + LS_OUT_NDI-MSTKDM.
            LS_OUT_NDI-ESTDIF = LS_OUT_NDI-ESTPRD + LS_OUT_NDI-ESTKDM.
            LS_OUT_NDI-MSTDIF = LS_OUT_NDI-MSTPRD + LS_OUT_NDI-MSTKDM.
            APPEND LS_OUT_NDI TO PT_OUT.
          ENDIF.
          IF NOT P_ALL IS INITIAL.
            LS_OUT_CUM-POS_TYPE = 'CUM'.
            LS_OUT_CUM-COLOR = L_COLOR.
            LS_OUT_CUM-POS_TYPE_TEXT = TEXT-008.
*           Anfangsbestands-Menge (R?kbuchungen) aus MLCD
            CLEAR: L_AB_MENGE, LS_OUT_CUM-PBPOPO.
            READ TABLE PT_MLCD TRANSPORTING NO FIELDS
                               WITH KEY KALNR = LS_CKMLCR-KALNR
                                        BDATJ = LS_CKMLCR-BDATJ
                                        POPER = LS_CKMLCR-POPER
                                        UNTPER = LS_CKMLCR-UNTPER
                                        CATEG = 'AB'
                                        CURTP = LS_CKMLCR-CURTP
                                        BINARY SEARCH.
            IF SY-SUBRC = 0.
              LOOP AT PT_MLCD FROM SY-TABIX INTO LS_MLCD.
                IF LS_MLCD-KALNR <> LS_CKMLCR-KALNR OR
                   LS_MLCD-BDATJ <> LS_CKMLCR-BDATJ OR
                   LS_MLCD-POPER <> LS_CKMLCR-POPER OR
                   LS_MLCD-UNTPER <> LS_CKMLCR-UNTPER OR
                   LS_MLCD-CATEG <> 'AB' OR
                   LS_MLCD-CURTP <> LS_CKMLCR-CURTP.
                  EXIT.
                ENDIF.
                L_AB_MENGE = L_AB_MENGE + LS_MLCD-LBKUM.
              ENDLOOP.
            ENDIF.
            LS_OUT_CUM-PRDIF = LS_OUT_CUM-ESTPRD + LS_OUT_CUM-MSTPRD.
            LS_OUT_CUM-KRDIF = LS_OUT_CUM-ESTKDM + LS_OUT_CUM-MSTKDM.
            LS_OUT_CUM-ESTDIF = LS_OUT_CUM-ESTPRD + LS_OUT_CUM-ESTKDM.
            LS_OUT_CUM-MSTDIF = LS_OUT_CUM-MSTPRD + LS_OUT_CUM-MSTKDM.
            LS_OUT_CUM-SUMDIF = LS_OUT_CUM-ESTPRD + LS_OUT_CUM-ESTKDM +
                                  LS_OUT_CUM-MSTPRD + LS_OUT_CUM-MSTKDM.
            LS_OUT_CUM-MENGE = LS_CKMLPP-ABKUMO + LS_CKMLPP-ZUKUMO +
                               LS_CKMLPP-VPKUMO + L_AB_MENGE.
            LS_OUT_CUM-WERT = LS_OUT_CUM-MENGE * LS_OUT_CUM-STPRS /
                              LS_OUT_CUM-PEINH.
            APPEND LS_OUT_CUM TO PT_OUT.
          ENDIF.
          IF ( NOT P_NOTINC IS INITIAL OR NOT P_ALL IS INITIAL ) AND
             NOT L_NIN IS INITIAL.
            LS_OUT_NIN-POS_TYPE = 'NIN'.
            LS_OUT_NIN-COLOR = L_COLOR.
            LS_OUT_NIN-POS_TYPE_TEXT = TEXT-007.
            CLEAR: LS_OUT_NIN-MENGE, LS_OUT_NIN-WERT.
            LS_OUT_NIN-ESTPRD = LS_MLCD_NOT_ALLOC-ESTPRD.
            LS_OUT_NIN-ESTKDM = LS_MLCD_NOT_ALLOC-ESTKDM.
            LS_OUT_NIN-MSTPRD = LS_MLCD_NOT_ALLOC-MSTPRD.
            LS_OUT_NIN-MSTKDM = LS_MLCD_NOT_ALLOC-MSTKDM.
            LS_OUT_NIN-PRDIF = LS_OUT_NIN-ESTPRD + LS_OUT_NIN-MSTPRD.
            LS_OUT_NIN-KRDIF = LS_OUT_NIN-ESTKDM + LS_OUT_NIN-MSTKDM.
            LS_OUT_NIN-ESTDIF = LS_OUT_NIN-ESTPRD + LS_OUT_NIN-ESTKDM.
            LS_OUT_NIN-MSTDIF = LS_OUT_NIN-MSTPRD + LS_OUT_NIN-MSTKDM.
            LS_OUT_NIN-SUMDIF = LS_OUT_NIN-ESTPRD + LS_OUT_NIN-ESTKDM +
                                  LS_OUT_NIN-MSTPRD + LS_OUT_NIN-MSTKDM.
            APPEND LS_OUT_NIN TO PT_OUT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
*   Da kommt noch was!
    ENDIF.
  ENDLOOP.


ENDFORM.                               " find_bad_boys
*&---------------------------------------------------------------------*
*&      Form  ausgabe
*&---------------------------------------------------------------------*
FORM AUSGABE.

  DATA: LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        LT_SORT TYPE SLIS_T_SORTINFO_ALV,
        LT_FILTER TYPE SLIS_T_FILTER_ALV,
        LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        LS_LAYOUT TYPE SLIS_LAYOUT_ALV,
        LS_VARIANT LIKE DISVARIANT,
        LS_SORT TYPE SLIS_SORTINFO_ALV,
        LS_FILTER TYPE SLIS_FILTER_ALV,
        L_INDEX TYPE I,
        L_REPID TYPE SY-REPID,
        L_BWTAR_SPACE LIKE CKMLHD-BWTAR,
        L_VBELN_SPACE LIKE CKMLHD-VBELN,
        L_POSNR_SPACE LIKE CKMLHD-POSNR,
        L_PSPNR_SPACE LIKE CKMLHD-PSPNR.


  CLEAR: L_BWTAR_SPACE, L_VBELN_SPACE, L_POSNR_SPACE,
         L_PSPNR_SPACE.

  REFRESH LT_FIELDCAT. CLEAR LT_FIELDCAT.

  IF NOT P_ALL IS INITIAL.
    CLEAR LS_FIELDCAT.
    LS_FIELDCAT-FIELDNAME = 'POS_TYPE_TEXT'.
    LS_FIELDCAT-TABNAME = 'T_OUT'.
    LS_FIELDCAT-REF_TABNAME = 'CKML_MULTI_TREE'.
    LS_FIELDCAT-REF_FIELDNAME = 'LTEXT'.
    L_INDEX = L_INDEX + 1.
    LS_FIELDCAT-COL_POS = L_INDEX.
*   ls_fieldcat-key = y_x.
    APPEND LS_FIELDCAT TO LT_FIELDCAT.
  ENDIF.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MATNR'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'BWKEY'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  IF TCURM-BWKRS_CUS = '1'.          "Bewertungsebene = Werk
    LS_FIELDCAT-REF_TABNAME = 'MLKEY'.
    LS_FIELDCAT-REF_FIELDNAME = 'WERKS'.
  ENDIF.
  IF TCURM-BWKRS_CUS = '3'.          "Bewertungsebene = BUKRS
    LS_FIELDCAT-REF_TABNAME = 'MLKEY'.
    LS_FIELDCAT-REF_FIELDNAME = 'BUKRS'.
  ENDIF.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'BWTAR'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LOOP AT T_OUT TRANSPORTING NO FIELDS
                WHERE NOT BWTAR = L_BWTAR_SPACE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'VBELN'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LOOP AT T_OUT TRANSPORTING NO FIELDS
                WHERE NOT VBELN = L_VBELN_SPACE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'POSNR'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LOOP AT T_OUT TRANSPORTING NO FIELDS
                WHERE NOT POSNR = L_POSNR_SPACE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PSPNR'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLHD'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LOOP AT T_OUT TRANSPORTING NO FIELDS
                WHERE NOT PSPNR = L_PSPNR_SPACE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'BKLAS'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MBEW'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MTART'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MARA'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MATKL'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MARA'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'SPART'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MARA'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PRCTR'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MARC'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'SUMDIF'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKI_DOC_ML'.
  LS_FIELDCAT-REF_FIELDNAME = 'SUM_DIF'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PRDIF'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKI_DOC_ML'.
  LS_FIELDCAT-REF_FIELDNAME = 'SUM_PRDIF'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'KRDIF'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKI_DOC_ML'.
  LS_FIELDCAT-REF_FIELDNAME = 'SUM_KRDIF'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'ESTDIF'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKI_DOC_ML'.
  LS_FIELDCAT-REF_FIELDNAME = 'SINGLELEVEL_DIF'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MSTDIF'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKI_DOC_ML'.
  LS_FIELDCAT-REF_FIELDNAME = 'MULTILEVEL_DIF'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'ESTPRD'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MLCD'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'ESTKDM'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MLCD'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MSTPRD'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MLCD'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MSTKDM'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'MLCD'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'WERT'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'KKB_ML_POS'.
  LS_FIELDCAT-REF_FIELDNAME = 'BEWER'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  IF P_ALL IS INITIAL.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'SALK3'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLCR'.
  LS_FIELDCAT-CTABNAME = 'T_OUT'.
  LS_FIELDCAT-CFIELDNAME = 'WAERS'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'CURTP'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLCR'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'WAERS'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLCR'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MENGE'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'KKB_ML_POS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  IF P_ALL IS INITIAL.
    LS_FIELDCAT-NO_OUT = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PBPOPO'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLPP'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
  LS_FIELDCAT-NO_ZERO = 'X'.
* ls_fieldcat-key = y_x.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'LBKUM'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLPP'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  LS_FIELDCAT-NO_ZERO = 'X'.
  LS_FIELDCAT-NO_OUT = 'X'.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MEINS'.
  LS_FIELDCAT-TABNAME = 'T_OUT'.
  LS_FIELDCAT-REF_TABNAME = 'CKMLPP'.
  L_INDEX = L_INDEX + 1.
  LS_FIELDCAT-COL_POS = L_INDEX.
* ls_fieldcat-key = y_x.
  APPEND LS_FIELDCAT TO LT_FIELDCAT.

* Varianten
  CLEAR LS_VARIANT.
  LS_VARIANT-REPORT = 'ML_VALUE_FLOW_MONITOR'.

* Sortierung
  REFRESH LT_SORT.
  CLEAR: LT_SORT, L_INDEX.

  CLEAR LS_SORT.
  L_INDEX = L_INDEX + 1.
  LS_SORT-SPOS = L_INDEX.
  LS_SORT-FIELDNAME = 'MATNR'.
  LS_SORT-TABNAME = 'T_OUT'.
  LS_SORT-UP = 'X'.
  APPEND LS_SORT TO LT_SORT.

  CLEAR LS_SORT.
  L_INDEX = L_INDEX + 1.
  LS_SORT-SPOS = L_INDEX.
  LS_SORT-FIELDNAME = 'BWKEY'.
  LS_SORT-TABNAME = 'T_OUT'.
  LS_SORT-UP = 'X'.
  APPEND LS_SORT TO LT_SORT.

  CLEAR LS_SORT.
  L_INDEX = L_INDEX + 1.
  LS_SORT-SPOS = L_INDEX.
  LS_SORT-FIELDNAME = 'BWTAR'.
  LS_SORT-TABNAME = 'T_OUT'.
  LS_SORT-UP = 'X'.
  APPEND LS_SORT TO LT_SORT.

* Filter nach lokaler W?rung (CURTP = 10)
  REFRESH: LT_FILTER.
  CLEAR: LS_FILTER.
  LS_FILTER-FIELDNAME = 'CURTP'.
  LS_FILTER-TABNAME = 'T_OUT'.
  LS_FILTER-VALUF = '10'.
  LS_FILTER-VALUF_INT = '10'.
  LS_FILTER-SIGN0 = 'I'.
  LS_FILTER-OPTIO = 'EQ'.
  APPEND LS_FILTER TO LT_FILTER.

* Layout
  CLEAR LS_LAYOUT.
  LS_LAYOUT-F2CODE = 'DETA'.
  LS_LAYOUT-KEY_HOTSPOT = 'X'.
  LS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LS_LAYOUT-ZEBRA = 'X'.
  LS_LAYOUT-INFO_FIELDNAME = 'COLOR'.

  L_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
      I_BUFFER_ACTIVE                   = 'X'
      I_CALLBACK_PROGRAM                = L_REPID
*      i_callback_pf_status_set          = 'ALV_PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'
      I_CALLBACK_TOP_OF_PAGE            = 'ALV_TOP_OF_LIST'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
      I_STRUCTURE_NAME                  = 'T_OUT'
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = LS_LAYOUT
      IT_FIELDCAT                       = LT_FIELDCAT
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      IT_SORT                           = LT_SORT
      IT_FILTER                         = LT_FILTER
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
      I_SAVE                            = 'A'
      IS_VARIANT                        = LS_VARIANT
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_ADD_FIELDCAT                   =
*     IT_HYPERLINK                      =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = T_OUT
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                                                    " ausgabe
*&---------------------------------------------------------------------*
*&      Form  alv_top_of_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_TOP_OF_LIST.

  DATA: LT_COMMENT TYPE SLIS_T_LISTHEADER,
        LS_COMMENT TYPE SLIS_LISTHEADER,
        L_JAHRPER TYPE JAHRPER.

  CLEAR: LS_COMMENT.
  LS_COMMENT-TYP = 'H'.
  LS_COMMENT-INFO = TEXT-005.
  APPEND LS_COMMENT TO LT_COMMENT.

  CLEAR: LS_COMMENT.
  LS_COMMENT-TYP = 'S'.
  LS_COMMENT-KEY = TEXT-014.
  CONCATENATE P_BDATJ P_POPER INTO L_JAHRPER.
  WRITE L_JAHRPER TO LS_COMMENT-INFO.
  APPEND LS_COMMENT TO LT_COMMENT.

  IF P_ALL IS INITIAL.
    CLEAR: LS_COMMENT.
    LS_COMMENT-TYP = 'S'.
    LS_COMMENT-KEY = TEXT-004.
    IF NOT P_NOTDIS IS INITIAL.
      WRITE TEXT-006 TO LS_COMMENT-INFO.
    ELSEIF NOT P_NOTINC IS INITIAL.
      WRITE TEXT-007 TO LS_COMMENT-INFO.
    ENDIF.
    APPEND LS_COMMENT TO LT_COMMENT.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           i_logo             = 'ML_LADDERS'
            IT_LIST_COMMENTARY = LT_COMMENT.


ENDFORM.                    " alv_top_of_list
*&---------------------------------------------------------------------*
*&      Form  get_mlcd_data
*&---------------------------------------------------------------------*
FORM GET_MLCD_DATA USING PT_MATS LIKE T_MATS[]
                   CHANGING PT_MLCD LIKE T_MLCD[]
                            PT_MLCD_NOT_ALLOC LIKE T_MLCD[].

  DATA: LT_KALNR TYPE CKMV0_MATOBJ_TBL,
        LS_MATS TYPE S_MATS,
        LS_KALNR TYPE CKMV0_MATOBJ_STR.

  REFRESH: LT_KALNR.
  LOOP AT PT_MATS INTO LS_MATS.
    CLEAR: LS_KALNR.
    LS_KALNR-KALNR = LS_MATS-KALNR.
    LS_KALNR-BWKEY = LS_MATS-BWKEY.
    APPEND LS_KALNR TO LT_KALNR.
  ENDLOOP.
  CALL FUNCTION 'CKMCD_MLCD_READ'
      EXPORTING
        I_FROM_BDATJ            = P_BDATJ
        I_FROM_POPER            = P_POPER
*       I_TO_BDATJ              =
*       I_TO_POPER              =
        I_UNTPER                = S_RUNPERIOD-UNTPER
*       I_RUN_ID                =
*       I_NO_BUFFER             =
*       I_REFRESH_BUFFER        =
        I_ONLINE                = ' '
*       I_NO_MLCD_CREATE        =
      TABLES
        IT_KALNR                = LT_KALNR
        OT_MLCD                 = PT_MLCD
        OT_MLCD_NOT_ALLOC       = PT_MLCD_NOT_ALLOC
      EXCEPTIONS
        DATA_ERROR              = 1
        OTHERS                  = 2
              .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT PT_MLCD BY KALNR BDATJ POPER UNTPER CATEG CURTP.
  SORT PT_MLCD_NOT_ALLOC BY KALNR BDATJ POPER UNTPER CURTP.

ENDFORM.                    " get_mlcd_data
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LT_OUT TYPE TY_OUT,
        LS_OUT TYPE S_OUT,
        L_KALNR_OLD TYPE CKMLHD-KALNR,
        L_PBPOPO_SPACE TYPE CKMLPP-PBPOPO,
        L_COUNTER TYPE I,
        L_ANSWER TYPE C.

  CASE R_UCOMM.
    WHEN 'DELPL'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        EXPORTING
          DEFAULTOPTION        = 'N'
          DIAGNOSETEXT1        = TEXT-016
          DIAGNOSETEXT2        = TEXT-017
*         DIAGNOSETEXT3        = ' '
          TEXTLINE1            = TEXT-018
*         textline2            =
          TITEL                = TEXT-015
*         START_COLUMN         = 25
*         START_ROW            = 6
          CANCEL_DISPLAY       = ' '
        IMPORTING
          ANSWER               = L_ANSWER
                .
      IF L_ANSWER = 'J'.
        CLEAR: L_PBPOPO_SPACE.
        LT_OUT = T_OUT.
        SORT LT_OUT BY POS_TYPE KALNR.
        READ TABLE LT_OUT WITH KEY POS_TYPE = 'NDI'
                          BINARY SEARCH
                          TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          LOOP AT LT_OUT INTO LS_OUT FROM SY-TABIX.
            IF LS_OUT-POS_TYPE <> 'NDI'.
              EXIT.
            ENDIF.
            CHECK: LS_OUT-STATUS < Y_ABSCHLUSSBUCHUNG_ERFOLGT.
            IF LS_OUT-KALNR <> L_KALNR_OLD.
              L_KALNR_OLD = LS_OUT-KALNR.
              UPDATE CKMLPP SET PBPOPO = L_PBPOPO_SPACE
                                STATUS = Y_MENGEN_UND_WERTE_ERFASST
                            WHERE KALNR = LS_OUT-KALNR
                            AND   BDATJ = LS_OUT-BDATJ
                            AND   POPER = LS_OUT-POPER
                            AND  UNTPER = LS_OUT-UNTPER.
              L_COUNTER = L_COUNTER + 1.
              IF L_COUNTER = 50.
                CLEAR: L_COUNTER.
                COMMIT WORK.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        COMMIT WORK.
      ENDIF.
  ENDCASE.


ENDFORM.                    " ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM ALV_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

*  SET PF-STATUS 'STANDARD_ALV' EXCLUDING RT_EXTAB.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_material_periods
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_PERIODS USING PT_MATS LIKE T_MATS[]
                                PT_CKMLPP LIKE T_CKMLPP[]
                                PT_CKMLCR LIKE T_CKMLCR[].

  DATA: LT_KALNR TYPE CKMV0_MATOBJ_TBL,
        LS_MATS TYPE S_MATS,
        LS_KALNR TYPE CKMV0_MATOBJ_STR.

  IF PT_MATS[] IS INITIAL.
    REFRESH: PT_CKMLPP, PT_CKMLCR.
    EXIT.
  ENDIF.
* Periodens?ze lesen
  REFRESH: LT_KALNR.
  LOOP AT PT_MATS INTO LS_MATS.
    CLEAR: LS_KALNR.
    LS_KALNR-KALNR = LS_MATS-KALNR.
    LS_KALNR-BWKEY = LS_MATS-BWKEY.
    APPEND LS_KALNR TO LT_KALNR.
  ENDLOOP.
  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
*     I_REFRESH_BUFFER                =
*     I_READ_ONLY_BUFFER              = ' '
*     I_USE_BUFFER                    = 'X'
*     I_BUILD_SMBEW                   =
      I_BDATJ_1                       = P_BDATJ
      I_POPER_1                       = P_POPER
*     I_BDATJ_2                       =
*     I_POPER_2                       =
*     I_BDATJ_3                       =
*     I_POPER_3                       =
*     I_BETWEEN_1_AND_2               =
      I_UNTPER                        = S_RUNPERIOD-UNTPER
      I_CALL_BY_REPORTING             = 'X'
      I_NO_CHK_PERIODS_COMPLETE       = 'X'
    TABLES
      T_KALNR                         = LT_KALNR
      T_CKMLPP                        = PT_CKMLPP
      T_CKMLCR                        = PT_CKMLCR
*     T_MISS_CKMLPP                   =
*     T_MISS_CKMLCR                   =
    EXCEPTIONS
      NO_DATA_FOUND                   = 1
      INPUT_DATA_INCONSISTENT         = 2
      BUFFER_INCONSISTENT             = 3
      OTHERS                          = 4.
  IF SY-SUBRC <> 0 AND
     NOT ( SY-SUBRC = 1 AND
           NOT ( PT_CKMLPP[] IS INITIAL AND PT_CKMLPP[] IS INITIAL ) ).
*   Probleme
    REFRESH: PT_MATS, PT_CKMLPP, PT_CKMLCR.
    EXIT.
  ENDIF.
  SORT: PT_CKMLPP, PT_CKMLCR.

ENDFORM.                    " get_material_periods
*>>>> END OF INSERTION <<<<<<
