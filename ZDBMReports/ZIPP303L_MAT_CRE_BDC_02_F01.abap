*----------------------------------------------------------------------*
*   INCLUDE ZIPP303L_MAT_CRE_BDC_01_F01                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM ZTBM_ABYMMCDT
       INTO TABLE IT_AMMC
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    WRITE: / TEXT-001 COLOR 6 ON.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM CHECK_MATERIAL_SAME_DATA.
  PERFORM COLLECT_LT_MATNR_READ_MARA.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-002, WA_LINE_IDX.
  WRITE: / TEXT-003, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-004.
    FORMAT COLOR OFF.
    WRITE: /(10)  TEXT-005,
            (20)  TEXT-006,
            (20)  TEXT-007,
            (40)  TEXT-008,
            (10)  TEXT-009,
            (03)  TEXT-010,
            (06)  TEXT-011,
            (12)  TEXT-012,
            (18)  TEXT-013,
            (10)  TEXT-014,
            (07)  TEXT-015,
            (07)  TEXT-016,
            (07)  TEXT-017,
            (07)  TEXT-018,
            (10)  TEXT-019,
            (10)  TEXT-020,
            (18)  TEXT-021,
            (10)  TEXT-022,
            (10)  TEXT-023,
            (11)  TEXT-024,
                  TEXT-025.

    FORMAT COLOR OFF.
    LOOP AT IT_AMMC WHERE ZRESULT EQ 'E'.
      WRITE: /(10) IT_AMMC-PLNT,
              (20) IT_AMMC-MTYP,
              (20) IT_AMMC-MTNO,
              (40) IT_AMMC-ZDESC,
              (10) IT_AMMC-INDU,
              (03) IT_AMMC-UNIT,
              (06) IT_AMMC-MGRP,
              (12) IT_AMMC-GICA,
              (18) IT_AMMC-SOUR,
              (10) IT_AMMC-MTCN,
              (07) IT_AMMC-MRPY,
              (07) IT_AMMC-MRPC,
              (07) IT_AMMC-LOTS,
              (07) IT_AMMC-PRTY,
              (10) IT_AMMC-SMKY,
              (10) IT_AMMC-AVCK,
              (18) IT_AMMC-CNMT,
              (10) IT_AMMC-SLMD,
              (10) IT_AMMC-INCO,
              (11) IT_AMMC-VESN,
                   IT_AMMC-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
FORM COLLECT_LT_MATNR_READ_MARA.
  DATA L_TABIX TYPE SY-TABIX.
  DATA LT_AMMC TYPE ZTBM_ABYMMCDT OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_MATNR OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END   OF LT_MATNR.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
          MTART TYPE MARA-MTART,
        END   OF LT_MARC.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_AMMC LINES WA_LINE_IDX.

  LOOP AT IT_AMMC.
    LT_MATNR-MATNR = IT_AMMC-MTNO.
    LT_MATNR-WERKS = IT_AMMC-PLNT.
    COLLECT LT_MATNR.
    CLEAR: LT_MATNR, IT_AMMC.
  ENDLOOP.

  IF NOT LT_MATNR[] IS INITIAL.
    SELECT A~MATNR
           A~WERKS
           B~MTART
         FROM MARC AS A INNER JOIN MARA AS B
                        ON A~MATNR EQ B~MATNR
         INTO TABLE LT_MARC
         FOR ALL ENTRIES IN LT_MATNR
         WHERE A~MATNR EQ LT_MATNR-MATNR
         AND   A~WERKS EQ LT_MATNR-WERKS.
    IF SY-SUBRC EQ 0.
*     SORTING
      SORT LT_MARC BY MATNR WERKS MTART.
*     ERROR CHECK
      LOOP AT IT_AMMC.
        L_TABIX = SY-TABIX.
        READ TABLE LT_MARC WITH KEY MATNR = IT_AMMC-MTNO
                                    WERKS = IT_AMMC-PLNT
*                                    MTART = IT_AMMC-MTYP
                           BINARY SEARCH
                           TRANSPORTING MTART.
        IF SY-SUBRC EQ 0.
*          P_CHECK = 'X'.
          LT_AMMC = IT_AMMC.
          LT_AMMC-ZRESULT = 'L'.
*         MATERIAL TYPE CHECKING
          IF LT_MARC-MTART EQ IT_AMMC-MTYP.
            LT_AMMC-ZMSG = 'MATERIAL does exist'.
          ELSE.
            LT_AMMC-ZMSG =
                      'MATERIAL does exist OR MATERIAL TYPE unequal '.
          ENDIF.
          PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                                IT_AMMC-ZEDAT
                                                IT_AMMC-ZETIM
                                       CHANGING LT_AMMC-ZBDAT
                                                LT_AMMC-ZBTIM
                                                LT_AMMC-ZBNAM
                                                LT_AMMC-ZMODE.
          APPEND LT_AMMC.
          DELETE IT_AMMC INDEX L_TABIX.
        ENDIF.
        CLEAR: IT_AMMC, LT_MARC, LT_AMMC.
      ENDLOOP.
    ENDIF.
  ENDIF.
  DESCRIBE TABLE LT_AMMC LINES WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-026, WA_LINE_IDX.
  WRITE: / TEXT-027, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  PERFORM WRITE_ERROR TABLES   LT_AMMC
                      USING    'L'.
  PERFORM UPDATE_ZTBM_ABYMMCDT TABLES   LT_AMMC.
ENDFORM.                    " COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
*&      Form  ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
FORM ZSBM_IF_TIME_CHANGE USING     P_CHK
                                   P_ZEDAT
                                   P_ZETIM
                          CHANGING P_ZBDAT
                                   P_ZBTIM
                                   P_ZBNAM
                                   P_ZMODE.
* BDC EXECUTE DATE
  P_ZBDAT = SY-DATUM.
  P_ZBNAM = SY-UNAME.
  P_ZMODE = 'C'.
* If error becomes, do not input time.
  IF P_CHK EQ 'S'.
    P_ZBTIM = SY-UZEIT.
  ENDIF.
ENDFORM.                    " ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
*&      Form  ztbm_abymmcdt_MARA_UPDATE
*&---------------------------------------------------------------------*
FORM ZTBM_ABYMMCDT_MARA_UPDATE.
* ztbm_abymmcdt UPDATE
  UPDATE ZTBM_ABYMMCDT FROM TABLE IT_AMMC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


ENDFORM.                    " ztbm_abymmcdt_MARA_UPDATE
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  P_ZEDAT = SY-DATUM.
  CLEAR: P_ZBTIM.
* BDC MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
FORM WRITE_ERROR TABLES    PT_AMMC STRUCTURE IT_AMMC
                 USING     ZRESULT.
  IF NOT PT_AMMC[] IS INITIAL.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: /(10)  TEXT-005,
            (20)  TEXT-006,
            (20)  TEXT-007,
            (40)  TEXT-008,
            (10)  TEXT-009,
            (03)  TEXT-010,
            (06)  TEXT-011,
            (12)  TEXT-012,
            (18)  TEXT-013,
            (10)  TEXT-014,
            (07)  TEXT-015,
            (07)  TEXT-016,
            (07)  TEXT-017,
            (07)  TEXT-018,
            (10)  TEXT-019,
            (10)  TEXT-020,
            (18)  TEXT-021,
            (10)  TEXT-022,
            (10)  TEXT-023,
            (11)  TEXT-024,
            (10)  TEXT-028,
                  TEXT-025.
    FORMAT COLOR OFF.
    LOOP AT PT_AMMC WHERE ZRESULT EQ ZRESULT.
      WRITE: /(10) PT_AMMC-PLNT,
              (20) PT_AMMC-MTYP,
              (20) PT_AMMC-MTNO,
              (40) PT_AMMC-ZDESC,
              (10) PT_AMMC-INDU,
              (03) PT_AMMC-UNIT,
              (06) PT_AMMC-MGRP,
              (12) PT_AMMC-GICA,
              (18) PT_AMMC-SOUR,
              (10) PT_AMMC-MTCN,
              (07) PT_AMMC-MRPY,
              (07) PT_AMMC-MRPC,
              (07) PT_AMMC-LOTS,
              (07) PT_AMMC-PRTY,
              (10) PT_AMMC-SMKY,
              (10) PT_AMMC-AVCK,
              (18) PT_AMMC-CNMT,
              (10) PT_AMMC-SLMD,
              (10) PT_AMMC-INCO,
              (11) PT_AMMC-VESN,
              (10) PT_AMMC-ZRESULT,
                   PT_AMMC-ZMSG.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ztbm_abymmcdt
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABYMMCDT TABLES   PT_AMMC STRUCTURE IT_AMMC.
* ERROR DATA UPDATE
  UPDATE ZTBM_ABYMMCDT FROM TABLE PT_AMMC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ztbm_abymmcdt
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: LT_AMMC LIKE IT_AMMC OCCURS 0 WITH HEADER LINE.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_AMMC LINES WA_LINE_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-026, WA_LINE_IDX.
  FORMAT COLOR OFF.

  PERFORM BDC_EXECUTION.

  REFRESH: IT_BDC, IT_MESS.
* CONFIGURABLE 'X'
  PERFORM MM02_BDC_EXECTION.

  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
* ERROR LINES
  LOOP AT IT_AMMC.
    CASE IT_AMMC-ZRESULT.
      WHEN 'E'.
        LT_AMMC = IT_AMMC.
        APPEND LT_AMMC.
      WHEN OTHERS.
        WA_LINE_IDX = WA_LINE_IDX + 1.
    ENDCASE.
    CLEAR: IT_AMMC, LT_AMMC.
  ENDLOOP.
  DESCRIBE TABLE LT_AMMC LINES WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-002, WA_LINE_IDX.
  WRITE: / TEXT-003, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  PERFORM WRITE_ERROR TABLES   LT_AMMC
                      USING    'E'.
* ztbm_abymmcdt UPDATE
  PERFORM UPDATE_ZTBM_ABYMMCDT TABLES   IT_AMMC.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
FORM CHECK_MATERIAL_SAME_DATA.
  DATA: LT_AMMC LIKE IT_AMMC OCCURS 0 WITH HEADER LINE,
        MT_AMMC LIKE IT_AMMC OCCURS 0 WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX,
        L_COUNT TYPE I.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_AMMC LINES WA_LINE_IDX.
  LT_AMMC[] = IT_AMMC[].
  CLEAR: LT_AMMC, IT_AMMC.

  LOOP AT IT_AMMC.
    L_TABIX = SY-TABIX.
    LOOP AT LT_AMMC WHERE MTNO EQ IT_AMMC-MTNO.
      L_COUNT = L_COUNT + 1.
    ENDLOOP.
    IF L_COUNT GT 2.
*      P_CHECK = 'X'.
      MT_AMMC = IT_AMMC.
      MT_AMMC-ZRESULT = 'L'.
      MT_AMMC-ZMSG = 'MATERIAL does exist that PLANT is unequal'.
      PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                             MT_AMMC-ZEDAT
                                             MT_AMMC-ZETIM
                                    CHANGING MT_AMMC-ZBDAT
                                             MT_AMMC-ZBTIM
                                             MT_AMMC-ZBNAM
                                             MT_AMMC-ZMODE.
      APPEND MT_AMMC.
      DELETE IT_AMMC INDEX L_TABIX.
    ENDIF.
    CLEAR: L_COUNT, IT_AMMC, LT_AMMC, MT_AMMC.
  ENDLOOP.
  IF NOT MT_AMMC[] IS INITIAL.
    DESCRIBE TABLE MT_AMMC LINES WA_ERRO_IDX.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    WRITE: / TEXT-026, WA_LINE_IDX.
   WRITE: / 'MATERIAL does exist that PLANT is unequal : ', WA_ERRO_IDX.
    FORMAT COLOR OFF.
    PERFORM WRITE_ERROR TABLES   MT_AMMC
                        USING    'L'.
    PERFORM UPDATE_ZTBM_ABYMMCDT TABLES   MT_AMMC.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM FERT_SELECTION_VIEW.
  DATA: L_VKORG TYPE RMMG1-VKORG,
        L_PRODH TYPE T179T-PRODH,  "Product hierarchy
** Added By Tonkey on 05/21/2004.
        L_MTPOS      TYPE MVKE-MTPOS,   "Item category group from M/M
        L_NATN_C     TYPE ZTBM_ABXTRTDT-NATN_C,
        L_DELR_C     TYPE ZTBM_ABXTRTDT-DELR_C,
        L_BLNK_CLOC4 TYPE ZTBM_ABXTRTDT-BLNK_CLOC4,
*    2004.03.15 CHANGE
        L_LGORT TYPE RMMG1-LGORT,
        L_VTWEG TYPE RMMG1-VTWEG,
        L_SPART TYPE MARA-SPART,
        L_DWERK TYPE MVKE-DWERK,
        L_KTGRM TYPE MVKE-KTGRM,
        L_TRAGR TYPE MARA-TRAGR,
        L_LADGR TYPE MARC-LADGR,
        L_DISGR TYPE MARC-DISGR,
        L_STRGR TYPE MARC-STRGR,
        L_VRMOD TYPE MARC-VRMOD,
        L_SAUFT TYPE MARC-SAUFT,       "Repetitive Mfg Indicator
        L_SFEPR TYPE MARC-SFEPR,       "REM profile
        L_PROFIL TYPE MARC-PROFIL,     "Backfl.Profile
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler
        L_UNETO TYPE MARC-UNETO,     "Underdely tol.
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1),                  "Costing lot size
*-----start wskim 02/11/2005
        L_BRGEW(16),
        L_NTGEW(16),
        L_GEWEI(3),
        Z_ORG(5),
        Z_SPEC(4),
        Z_ITEM(18).

  DATA : IT_INF LIKE ZTBM_FSC_CRE_INF OCCURS 0 WITH HEADER LINE.


* by mOOn 7/19/2007
* {
  DATA : $DEAL_RET(2) .
* }

*-----end
************************
** Changed By Tonkey 05/21/2004.
  MOVE: IT_AMMC-MTNO+01(03) TO L_NATN_C,
        IT_AMMC-MTNO+04(01) TO L_DELR_C.

* by mOOn 7/19/2007
* {
  CLEAR : $DEAL_RET .
  SELECT SINGLE OLD_DEALER INTO $DEAL_RET
                 FROM ZTEBPP_DEAL_CONV
                 WHERE NEW_DEALER EQ L_DELR_C.

  IF $DEAL_RET IS INITIAL.
    MESSAGE  E001 WITH 'Dealer conversion error'.
  ENDIF.

* }

  REFRESH IT_INF. CLEAR :Z_ORG,Z_ITEM.

  CONCATENATE L_NATN_C $DEAL_RET INTO Z_ORG.

  SELECT * INTO TABLE IT_INF
          FROM ZTBM_FSC_CRE_INF.
**Sales org.
  CONCATENATE 'SA_' Z_ORG INTO Z_ITEM.

  READ TABLE IT_INF WITH KEY ITEM = Z_ITEM.
  IF SY-SUBRC = 0.
    MOVE : IT_INF-VALU2 TO L_VKORG,
           IT_INF-VALU3 TO L_VTWEG.
  ELSE.
    CASE IT_AMMC-MTNO+1(3).
      WHEN 'B28'.
        L_VKORG = 'D100'.  "Sales organization
      WHEN OTHERS.
        L_VKORG = 'E100'.  "Sales organization
    ENDCASE.

    L_VTWEG   = '10'   .  "Distribution level

    MESSAGE I001 WITH 'Sales org Data missing'
                      'Check : T-code ZBM_FINF'.
  ENDIF.

*Division
  CLEAR : Z_SPEC, Z_ITEM.
  MOVE : IT_AMMC-MTNO+5(2)  TO Z_SPEC.

  CONCATENATE 'DI_' Z_SPEC INTO Z_ITEM.

  READ TABLE IT_INF WITH KEY ITEM = Z_ITEM.
  IF SY-SUBRC = 0.
    MOVE : IT_INF-VALU1 TO L_SPART.
  ELSE.
    L_SPART   = '10'   .  "Division
    MESSAGE I001 WITH 'Division Data missing' 'Check : T-code ZBM_FINF'.
  ENDIF.

*Weight
  CLEAR :Z_SPEC,Z_ITEM.

  MOVE : IT_AMMC-MTNO+5(2)  TO Z_SPEC,
         IT_AMMC-MTNO+9(2) TO Z_SPEC+2(2).

  CONCATENATE 'WE_' Z_SPEC INTO Z_ITEM.

  READ TABLE IT_INF WITH KEY ITEM = Z_ITEM.
  IF SY-SUBRC = 0.
    MOVE : IT_INF-VALU1 TO L_BRGEW,
           IT_INF-VALU2 TO L_NTGEW,
           IT_INF-VALU3 TO L_GEWEI.
  ELSE.
    MESSAGE I001 WITH 'Weight Data missing' 'Check : T-code ZBM_FINF'.
  ENDIF.

  L_LGORT   = 'F001' .  "Storage location
  L_DWERK   = 'P001' .  "Delivering plant
  L_KTGRM   = '10'   .  "Acct assignment grp
  L_TRAGR   = '0001' .  "Trans. grp
  L_LADGR   = 'P100' .  "LoadingGrp
  L_DISGR   = '0001' .  "MRP group
  L_STRGR   = '56'   .  "Strategy group
  L_VRMOD   = '1'    .  "Consumption mode
  L_SAUFT   = 'X'    .  "Repetitive Mfg
  L_SFEPR   = 'VEHI' .  "REM profile
  L_PROFIL  = 'SAP2' .  "Backfl.Profile
  L_FEVOR   = '001'  .  "Production scheduler
  L_BKLAS   = '7920' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
*
** Unactivated By Tonkey on 05/21/2004.
*  it_ammc-gica = '0002'.  "Item category group
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-MRPY = 'PD'.    "MRP type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'V01'.   "MRP controller
**end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'E'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
**************************
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
*}
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS

  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT,  "PLANT
*    2004.03.15 CHANGE
     ' ' 'RMMG1-LGORT' L_LGORT,       "Storage location
     ' ' 'RMMG1-VTWEG' L_VTWEG,       "Distribution level
*******************************
     ' ' 'RMMG1-VKORG' L_VKORG,       "Sales organization
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM READ_T179T USING    IT_AMMC-MTYP
                              IT_AMMC-MTNO+5(2)
                     CHANGING L_PRODH.
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      IT_AMMC-ZDESC,  "Description
     ' ' 'MARA-MEINS'      IT_AMMC-UNIT,   "Unit
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,   "GenItemCatGroup
     ' ' 'MARA-PRDHA'      L_PRODH,        "Prod.hierarchy
*    2004.03.15 CHANGE
     ' ' 'MARA-SPART'      L_SPART,        "Division
*-----start wskim 02/11/2005
     ' ' 'MARA-BRGEW'      L_BRGEW,        "Gross weight
     ' ' 'MARA-NTGEW'      L_NTGEW,        "net weight
     ' ' 'MARA-GEWEI'      L_GEWEI,        "unit
*-----end
**********************************
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
*-----start wskim 02/11/2005 " default 'M' Choi
*     ' ' 'MARA-PROFL' it_ammc-sour,        "LP/KD/MIP
     ' ' 'MARA-PROFL' 'M',        "LP/KD/MIP
*----end
     ' ' 'BDC_OKCODE'  '=SP04'.
* Classification
* N/A
* Sales: Sales Org. Data 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*-----start 02/16/05 by wskim
*     ' ' 'MARA-SPART' ' ',                 "Division
      ' ' 'MARA-SPART' L_SPART,
*-----end
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' L_DWERK,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
*-----start wskim 02/11/2005  change from 1 to 0
* 03/15/2005  change From 1 to 0
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
*-----END
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.


  CONCATENATE 'EC_' Z_SPEC INTO Z_ITEM.

  READ TABLE IT_INF WITH KEY ITEM = Z_ITEM.
  IF SY-SUBRC = 0.
    MOVE : IT_INF-VALU1 TO L_BRGEW.
  ELSE.
    L_BRGEW = ''.
  ENDIF.

* Sales: Sales Org. Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  L_KTGRM,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      L_PRODH,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      IT_AMMC-MTNO+13(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      L_BRGEW,          " by IG.MOON 8/27/2007
     ' ' 'MVKE-MVGR5'      IT_AMMC-MGRP,     " by IG.MOON 8/27/2007
     ' ' 'MVKE-MTPOS'      IT_AMMC-GICA,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,   "Availability check
*******************************
     ' ' 'MARA-TRAGR'  L_TRAGR,  "Trans. grp
*    2004.03.15 CHANGE
     ' ' 'MARC-LADGR'  L_LADGR,  "LoadingGrp
*******************************
     ' ' 'BDC_OKCODE'  '=SP12'.

* Foreign Trade: Export Data

* Sales Text

* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISGR'  L_DISGR,       "MRP group
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,  "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,  "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-LGPRO'  L_LGORT,        "Issue stor. location
     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY,  "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY,  "SchedMargin key
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,    "Availability check
     ' ' 'MARC-STRGR'  L_STRGR,         "Strategy group
*    2004.03.15 CHANGE
     ' ' 'MARC-VRMOD'  L_VRMOD,         "Consumption mode
*******************************

     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'   IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'   IT_AMMC-INCO,  "Individual/coll.
     ' ' 'MARC-SAUFT'   L_SAUFT,       "Repetitive Mfg Indicator
     ' ' 'MARC-SFEPR'   L_SFEPR,       "REM profile
     ' ' 'MARC-PROFIL'  L_PROFIL,      "Backfl.Profile
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting

* Work Scheduling
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  L_FEVOR,   " Production scheduler
*     ' ' 'MARC-UNETO'  L_UNETO,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.

* Warehouse Management 1

* Warehouse Management 2

* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  L_BKLAS,          "Valuation class
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.                    " FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM HALB_SELECTION_VIEW.
  DATA: L_PRODH LIKE T179T-PRODH,    "Product hierarchy
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler

*---start wskim 03/15/2005
*        l_uneto TYPE marc-uneto,     "Underdely tol.
** on 01/20/12
*        L_UNETO(5) TYPE C,           "Underdely tol.
        l_uneto TYPE marc-uneto,     "Underdely tol.
** end on 01/20/12
*---end
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

* 2004.03.25 CHANGE
  L_RGEKZ = '1'. "Backflush
  L_FEVOR   = '001'  .  "Production scheduler
** on 01/20/12
*  L_UNETO   = '10.0' .  "Underdely tol
** end on 01/20/12
  L_BKLAS   = '7900' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'X'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
*******************
* START BDC
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
*}
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1

* 2004.03.25 CHANGE
*  PERFORM READ_T179T USING    IT_AMMC-MTYP
*                              IT_AMMC-MTNO+6(2)
*                     CHANGING L_PRODH.
  L_PRODH = '00002'.   "Product hierarchy
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MARA-PRDHA'      L_PRODH,        "Prod.hierarchy
     ' ' 'MAKT-MAKTX'      IT_AMMC-ZDESC,  "Description
     ' ' 'MARA-MEINS'      IT_AMMC-UNIT,   "Unit
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,   "GenItemCatGroup
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL'  IT_AMMC-SOUR,   "LP/KD/MIP
     ' ' 'BDC_OKCODE'  '=SP12'.
** Classification

* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,   "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,   "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY, "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  L_FEVOR,   "Production scheduler
** on 01/20/12
*     ' ' 'MARC-UNETO'  L_UNETO,   "Underdely tol.
** End
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'                '5000',
    ' ' 'MBEW-BKLAS'               L_BKLAS,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'              '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM HALB_PHAN_SELECTION_VIEW.
  DATA: L_PRODH LIKE T179T-PRODH,      "Product hierarchy
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler
        L_UNETO TYPE MARC-UNETO,     "Underdely tol.
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

* 2004.03.25 CHANGE
  L_RGEKZ = '1'. "Backflush
  L_FEVOR   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  L_BKLAS   = '7900' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'X'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
*******************
* START BDC
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
*}
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1

* 2004.03.25 CHANGE
*  PERFORM READ_T179T USING    IT_AMMC-MTYP
*                              IT_AMMC-MTNO+6(2)
*                     CHANGING L_PRODH.
  L_PRODH = '00002'.   "Product hierarchy
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MARA-PRDHA'      L_PRODH,        "Prod.hierarchy
     ' ' 'MAKT-MAKTX'      IT_AMMC-ZDESC,  "Description
     ' ' 'MARA-MEINS'      IT_AMMC-UNIT,   "Unit
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,   "GenItemCatGroup
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL'  IT_AMMC-SOUR,   "LP/KD/MIP
     ' ' 'BDC_OKCODE'  '=SP12'.
** Classification

* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  'ND',           "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,   "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  'X',          "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'MARC-SOBSL'  '50',         "Special procurement
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* Work Scheduling

* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
    ' ' 'MBEW-BKLAS'  L_BKLAS,          "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'MARC-NCOST'  'X',     "No costing
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM ROH_SELECTION_VIEW.
  DATA: L_MATKL TYPE MARA-MATKL,
        L_MSTAE LIKE MARA-MSTAE,     "X-plant matl status
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(5),                "Price unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*      it_ammc-lots = 'WB'.    "Lot size(Default : EX)
*    WHEN 'V'.
*      l_bklas = '3001'.  "Valuation class
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*    WHEN OTHERS.
*      l_bklas = '3000'.  "Valuation class
*      CLEAR: it_ammc-sour.
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  ENDCASE.

  CLEAR: IT_AMMC-SOUR.
  L_BKLAS      = '3000'.
  IT_AMMC-LOTS = 'EX'.

  L_MATKL      = 'INIT'.  "Material Group
  L_MSTAE      = '11'.    "X-plant matl status
  L_MSTAE      = '11'.    "X-plant matl status
  L_RGEKZ      = '1'.     "Backflush
  L_PEINH_1    = '1'.     "Price unit
  L_EKALR      = 'X'.     "with qty structure
  L_LOSGR      = '1'.     "Costing lot size
  IT_AMMC-UNIT = 'EA'.    "Unit of measure
  IT_AMMC-GICA = ' '.     "GenItemCatGroup
  IT_AMMC-MRPY = 'PD'.    "MRP type
  IT_AMMC-PRTY = 'F'.     "Procurement type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'C01'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = ' '.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.

* 2004.03.25 CHANGE
*  l_mstae = '11'      .  "X-plant matl status
*  l_rgekz = '1'      .  "Backflush
*  l_peinh_1 = '1'    .  "Price unit
*  clear l_matkl.
*  perform read_t023t changing l_matkl.  "Material  Group
*
*  clear l_bklas.
*  case it_ammc-sour.
*    when 'K'.
*      l_bklas = '3000'.  "Valuation class
*    when others.
*      l_bklas = '3001'.  "Valuation class
*  endcase.
*
*  l_ekalr   = 'X'    .  "with qty structure
*  l_losgr   = '1'    .  "Costing lot size

*  it_ammc-gica = ' '.     "GenItemCatGroup
*  it_ammc-mrpc = 'C01'.   "MRP controller
*  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  it_ammc-smky = '000'.   "SchedMargin key
*  it_ammc-avck = 'KP'.    "Availability check
*  it_ammc-slmd = ' '.     "Selection method
*  it_ammc-inco = '2'.     "Individual/coll.
*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI

  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
*}
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT,  "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX' IT_AMMC-ZDESC,  "Description
     ' ' 'MARA-MEINS' IT_AMMC-UNIT,   "Unit
     ' ' 'MARA-MATKL' L_MATKL,        "Material  Group
     ' ' 'MARA-MSTAE' L_MSTAE,        "X-plant matl status
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA, "GenItemCatGroup
     ' ' 'BDC_OKCODE'  '=SP02'.
* Basic Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL' IT_AMMC-SOUR,  "MIP/LP/KD
     ' ' 'BDC_OKCODE'  '=SP09'.
** Classification

* Purchasing View
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MMSTA'  L_MSTAE,
     ' ' 'MARC-KORDB'  'X',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,  "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,  "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY, "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'              L_BKLAS,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING CHANGING P_MSG.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  BDC_EXECUTION
*&---------------------------------------------------------------------*
FORM BDC_EXECUTION.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MTART TYPE MARA-MTART,
        L_MESSG LIKE CFGNL-MSGLIN.

  LOOP AT IT_AMMC.
    L_TABIX = SY-TABIX.
    SELECT SINGLE MTART
            FROM MARA
            INTO L_MTART
            WHERE MATNR EQ IT_AMMC-MTNO.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
                  FROM MARC
                  WHERE MATNR EQ IT_AMMC-MTNO
                  AND   WERKS EQ IT_AMMC-PLNT.
      IF SY-SUBRC EQ 0.
*       Material already maintained for this transaction/event
        PERFORM ERROR_LOG_MODIFY USING L_TABIX.
      ELSE.
*   Create material that there does not exist to 'MARC' being to 'MARA'.
        IF L_MTART EQ IT_AMMC-MTYP.
          PERFORM MARC_DOES_NOT_EXIST_MATERIAL.
          PERFORM CALL_TRANSACTION_MM01 USING L_TABIX.
        ELSE.
          PERFORM ERROR_LOG_MAT_TYPE USING L_TABIX.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM MARA_DOES_NOT_EXIST_MATERIAL.
      PERFORM CALL_TRANSACTION_MM01 USING L_TABIX.
    ENDIF.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_AMMC.
  ENDLOOP.
ENDFORM.                    " BDC_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
FORM MM02_BDC_EXECTION.
  DATA: L_MESSG LIKE CFGNL-MSGLIN,
        L_TABIX LIKE SY-TABIX.
  LOOP AT IT_AMMC WHERE ZRESULT NE 'E'
                  AND   MTCN    EQ 'X'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR'  IT_AMMC-MTNO,
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)'  'X',
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  IT_AMMC-MTCN,
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'   USING IT_BDC
                     OPTIONS  FROM WA_OPT
                     MESSAGES INTO IT_MESS.
    IT_AMMC-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_MESSG.
    IF IT_AMMC-ZRESULT EQ 'E'.
      IT_AMMC-ZMSG = L_MESSG.
      MODIFY IT_AMMC INDEX L_TABIX TRANSPORTING ZRESULT ZMSG.
    ENDIF.
    CLEAR: L_MESSG, IT_AMMC.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.
ENDFORM.                    " MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
*&      Form  READ_T179T
*&---------------------------------------------------------------------*
FORM READ_T179T USING    P_MTART
                         P_CARTY
                CHANGING P_PRODH.
  DATA: L_PRODH TYPE T179T-PRODH,
        L_VTEXT TYPE T179T-VTEXT.
  CASE P_MTART.
    WHEN 'FERT'.
      CONCATENATE P_MTART '%' INTO L_VTEXT.
      SELECT SINGLE PRODH
                  FROM T179T
                  INTO L_PRODH
                  WHERE VTEXT LIKE L_VTEXT.
      CLEAR L_VTEXT.

      CONCATENATE L_PRODH '%' INTO L_PRODH.
      CONCATENATE P_CARTY '%' INTO L_VTEXT.
      SELECT SINGLE PRODH
           FROM T179T
           INTO P_PRODH
           WHERE PRODH LIKE L_PRODH
           AND   VTEXT LIKE L_VTEXT.
    WHEN 'HALB'.
      CONCATENATE P_MTART '%' INTO L_VTEXT.
      SELECT SINGLE PRODH
                  FROM T179T
                  INTO P_PRODH
                  WHERE VTEXT LIKE L_VTEXT.
      CLEAR L_VTEXT.
  ENDCASE.
ENDFORM.                    " READ_T179T
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
FORM ERROR_LOG_MODIFY USING    P_TABIX.
  IT_AMMC-ZRESULT = 'E'.
  IT_AMMC-ZBDAT = SY-DATUM.
  IT_AMMC-ZBNAM = SY-UNAME.
  IT_AMMC-ZMODE = 'C'.
  IT_AMMC-ZMSG =
  'Material already maintained for this transaction/event'.
* MODIFY IT_AMMC
  MODIFY IT_AMMC INDEX P_TABIX TRANSPORTING ZRESULT
                                            ZBDAT
                                            ZBNAM
                                            ZMODE
                                            ZMSG.
ENDFORM.                    " ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM MARC_DOES_NOT_EXIST_MATERIAL.
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' IT_AMMC-MTNO,    "
     ' ' 'RMMG1-MBRSH' IT_AMMC-INDU,    "
     ' ' 'RMMG1-MTART' IT_AMMC-MTYP,    "
     ' ' 'BDC_OKCODE'  '/00'.
* MATERIAL TYPE
  CASE IT_AMMC-MTYP.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM FERT_SELECTION_VIEW1.
    WHEN 'HALB'.
      CASE IT_AMMC-SOUR.
*       MIP
        WHEN 'M'.
*         SELECTION VIEW( HALB(MIP) : BDC TAB )
          PERFORM HALB_SELECTION_VIEW1.
*       PHANTOM
        WHEN OTHERS.
*         SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          PERFORM HALB_PHAN_SELECTION_VIEW1.
      ENDCASE.

    WHEN 'ROH'.
*     SELECTION VIEW( ROH : BDC TAB )
      PERFORM ROH_SELECTION_VIEW1.
  ENDCASE.
ENDFORM.                    " MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM FERT_SELECTION_VIEW1.
  DATA: L_VKORG TYPE RMMG1-VKORG,
        L_PRODH TYPE T179T-PRODH,  "Product hierarchy
** Added By Tonkey on 05/21/2004.
        L_NATN_C     TYPE ZTBM_ABXTRTDT-NATN_C,
        L_DELR_C     TYPE ZTBM_ABXTRTDT-DELR_C,
        L_BLNK_CLOC4 TYPE ZTBM_ABXTRTDT-BLNK_CLOC4,
**
*    2004.03.15 CHANGE
        L_LGORT TYPE RMMG1-LGORT,
        L_VTWEG TYPE RMMG1-VTWEG,
        L_DWERK TYPE MVKE-DWERK,
        L_KTGRM TYPE MVKE-KTGRM,
        L_TRAGR TYPE MARA-TRAGR,
        L_LADGR TYPE MARC-LADGR,
        L_DISGR TYPE MARC-DISGR,
        L_STRGR TYPE MARC-STRGR,
        L_VRMOD TYPE MARC-VRMOD,
        L_SAUFT TYPE MARC-SAUFT,       "Repetitive Mfg Indicator
        L_SFEPR TYPE MARC-SFEPR,       "REM profile
        L_PROFIL TYPE MARC-PROFIL,     "Backfl.Profile
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler
        L_UNETO TYPE MARC-UNETO,     "Underdely tol.
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size
************************
** Changed By Tonkey on 05/21/2004.
  MOVE: IT_AMMC-MTNO+01(03) TO L_NATN_C,
        IT_AMMC-MTNO+04(02) TO L_DELR_C.
*
  SELECT SINGLE BLNK_CLOC4
    INTO L_BLNK_CLOC4
    FROM ZTBM_ABXTRTDT
    WHERE NATN_C = L_NATN_C AND  "Nation
          DELR_C = L_DELR_C   .  "Dealer
*
  CASE L_BLNK_CLOC4.
    WHEN 'D'.
      L_VKORG      = 'D100'.
      IT_AMMC-GICA = '0002'.
    WHEN 'E'.
      L_VKORG      = 'E100'.
      IT_AMMC-GICA = 'Z002'.
  ENDCASE.

*    2004.03.15 CHANGE
  PERFORM READ_T179T USING    IT_AMMC-MTYP
                              IT_AMMC-MTNO+5(2)
                     CHANGING L_PRODH.

  L_LGORT   = 'F001' .  "Storage location
  L_VTWEG   = '10'   .  "Distribution level
  L_DWERK   = 'P001' .  "Delivering plant
  L_KTGRM   = '10'   .  "Acct assignment grp
  L_TRAGR   = '0001' .  "Trans. grp
  L_LADGR   = 'P100' .  "LoadingGrp
  L_DISGR   = '0001' .  "MRP group
  L_STRGR   = '56'   .  "Strategy group
  L_VRMOD   = '1'    .  "Consumption mode
  L_SAUFT   = 'X'    .  "Repetitive Mfg
  L_SFEPR   = 'VEHI' .  "REM profile
  L_PROFIL  = 'SAP2' .  "Backfl.Profile
  L_FEVOR   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  L_BKLAS   = '7920' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
*
** Inactivated By Tonkey on 05/21/2004.
*  it_ammc-gica = '0002'.  "Item category group
**
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-MRPY = 'PD'.    "MRP type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'V01'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'E'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
**************************

  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
*}
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT,  "PLANT
*    2004.03.15 CHANGE
     ' ' 'RMMG1-LGORT' L_LGORT,       "Storage location
     ' ' 'RMMG1-VTWEG' L_VTWEG,       "Distribution level
*******************************
     ' ' 'RMMG1-VKORG' L_VKORG,       "Sales organization
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Sales: Sales Org. Data 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',                 "Division
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' L_DWERK,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  L_KTGRM,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      L_PRODH,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      IT_AMMC-MTNO+12(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      IT_AMMC-MTNO+10(01),  "Material group 4
     ' ' 'MVKE-MVGR5'      IT_AMMC-MTNO+09(01),  "Material group 5
     ' ' 'MVKE-MTPOS'      IT_AMMC-GICA,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',                 "Division
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' L_DWERK,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  L_KTGRM,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' IT_AMMC-GICA,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      L_PRODH,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      IT_AMMC-MTNO+12(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      IT_AMMC-MTNO+10(01),  "Material group 4
     ' ' 'MVKE-MVGR5'      IT_AMMC-MTNO+09(01),  "Material group 5
     ' ' 'MVKE-MTPOS'      IT_AMMC-GICA,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,   "Availability check
*******************************
     ' ' 'MARA-TRAGR'  L_TRAGR,  "Trans. grp
*    2004.03.15 CHANGE
     ' ' 'MARC-LADGR'  L_LADGR,  "LoadingGrp
*******************************
     ' ' 'BDC_OKCODE'  '=SP12'.

* Foreign Trade: Export Data

* Sales Text

* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISGR'  L_DISGR,       "MRP group
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,  "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,  "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-LGPRO'  L_LGORT,        "Issue stor. location
     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY,  "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY,  "SchedMargin key
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,    "Availability check
     ' ' 'MARC-STRGR'  L_STRGR,         "Strategy group
*    2004.03.15 CHANGE
     ' ' 'MARC-VRMOD'  L_VRMOD,         "Consumption mode
*******************************

     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'   IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'   IT_AMMC-INCO,  "Individual/coll.
     ' ' 'MARC-SAUFT'   L_SAUFT,       "Repetitive Mfg Indicator
     ' ' 'MARC-SFEPR'   L_SFEPR,       "REM profile
     ' ' 'MARC-PROFIL'  L_PROFIL,      "Backfl.Profile
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting

* Work Scheduling
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  L_FEVOR,   " Production scheduler
     ' ' 'MARC-UNETO'  L_UNETO,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.

* Warehouse Management 1

* Warehouse Management 2

* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  L_BKLAS,          "Valuation class
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.


ENDFORM.                    " FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM HALB_SELECTION_VIEW1.
  DATA: L_PRODH LIKE T179T-PRODH,      "Product hierarchy
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler
        L_UNETO TYPE MARC-UNETO,     "Underdely tol.
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

* 2004.03.25 CHANGE
  L_RGEKZ = '1'. "Backflush
  L_FEVOR   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  L_BKLAS   = '7900' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
  L_PRODH   = '00002'.  "Product hierarchy
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'X'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
*******************
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
** Changed on 01/19/12 'BW view
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
** end on 01/19/12
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,   "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,   "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY, "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  L_FEVOR,   "Production scheduler
*     ' ' 'MARC-UNETO'  L_UNETO,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'                '5000',
    ' ' 'MBEW-BKLAS'               L_BKLAS,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'              '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM HALB_PHAN_SELECTION_VIEW1.
  DATA: L_PRODH LIKE T179T-PRODH,      "Product hierarchy
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_FEVOR TYPE MARC-FEVOR,     "Production scheduler
        L_UNETO TYPE MARC-UNETO,     "Underdely tol.
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(06),    "Price unitPrice unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

* 2004.03.25 CHANGE
  L_RGEKZ = '1'. "Backflush
  L_FEVOR   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  L_BKLAS   = '7900' .  "Valuation class
  L_PEINH_1 = '1'    .  "Price unit
  L_EKALR   = 'X'    .  "with qty structure
  L_LOSGR   = '1'    .  "Costing lot size
  L_PRODH   = '00002'.  "Product hierarchy
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-PRTY = 'X'.     "Procurement type
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = '2'.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.
*******************

*******************
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
*}
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.

* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  'ND',           "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,   "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  'X',          "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'MARC-SOBSL'  '50',         "Special procurement
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* Work Scheduling

* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
    ' ' 'MBEW-BKLAS'  L_BKLAS,          "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'MARC-NCOST'  'X',     "No costing
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM ROH_SELECTION_VIEW1.
  DATA: L_MATKL TYPE MARA-MATKL,
        L_MSTAE LIKE MARA-MSTAE,     "X-plant matl status
        L_RGEKZ LIKE MARC-RGEKZ,     "Backflush
        L_BKLAS TYPE MBEW-BKLAS,     "Valuation class
        L_PEINH_1(5),                "Price unit
        L_EKALR TYPE MBEW-EKALR,     "with qty structure
        L_LOSGR(1).                  "Costing lot size

*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*      it_ammc-lots = 'WB'.    "Lot size(Default : EX)
*    WHEN 'V'.
*      l_bklas = '3001'.  "Valuation class
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*    WHEN OTHERS.
*      l_bklas = '3000'.  "Valuation class
*      CLEAR: it_ammc-sour.
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  ENDCASE.

  CLEAR: IT_AMMC-SOUR.
  L_BKLAS = '3000'.
  IT_AMMC-LOTS = 'EX'.

  L_MATKL      = 'INIT'.  "Material Group
  L_MSTAE      = '11'.    "X-plant matl status
  L_RGEKZ      = '1'.     "Backflush
  L_PEINH_1    = '1'.     "Price unit
  L_EKALR      = 'X'.     "with qty structure
  L_LOSGR      = '1'.     "Costing lot size
  IT_AMMC-UNIT = 'EA'.    "Unit of measure
  IT_AMMC-GICA = ' '.     "GenItemCatGroup
  IT_AMMC-MRPY = 'PD'.    "MRP type
  IT_AMMC-PRTY = 'F'.     "Procurement type
** change by Furong on 12/19/2005
*  it_ammc-mrpc = 'C01'.   "MRP controller
** end of change
  IT_AMMC-LOTS = 'EX'.    "Lot size(Default : EX)
  IT_AMMC-SMKY = '000'.   "SchedMargin key
  IT_AMMC-AVCK = 'KP'.    "Availability check
  IT_AMMC-SLMD = ' '.     "Selection method
  IT_AMMC-INCO = '2'.     "Individual/coll.

** 2004.03.25 CHANGE
*  l_rgekz = '1'      .  "Backflush
*  l_peinh_1 = '1'    .  "Price unit
*  CLEAR l_matkl.
*  PERFORM read_t023t CHANGING l_matkl.  "Material  Group
*
*  CLEAR l_bklas.
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*    WHEN OTHERS.
*      l_bklas = '3001'.  "Valuation class
*  ENDCASE.
*
*  l_ekalr   = 'X'    .  "with qty structure
*  l_losgr   = '1'    .  "Costing lot size
*
*  it_ammc-gica = ' '.     "GenItemCatGroup
*  it_ammc-mrpc = 'C01'.   "MRP controller
*  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  it_ammc-smky = '000'.   "SchedMargin key
*  it_ammc-avck = 'KP'.    "Availability check
*  it_ammc-slmd = ' '.     "Selection method
*  it_ammc-inco = '2'.     "Individual/coll.
*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI

  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
*{ 09/28/11 Paul Comment
*     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
*}
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' IT_AMMC-PLNT,  "Plant
     ' ' 'BDC_OKCODE'  '=ENTR'.

* Purchasing View
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MMSTA'  L_MSTAE,
     ' ' 'MARC-KORDB'  'X',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  IT_AMMC-MRPY,  "MRP type
     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,  "MRP controller
     ' ' 'MARC-DISLS'  IT_AMMC-LOTS,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY, "Procurement type
     ' ' 'MARC-FHORI'  IT_AMMC-SMKY, "SchedMargin key
     ' ' 'MARC-RGEKZ'  L_RGEKZ,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  IT_AMMC-AVCK,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM DYNPRO USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,  "Selection method
     ' ' 'MARC-SBDKZ'  IT_AMMC-INCO,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'              L_BKLAS,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  L_PEINH_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  L_EKALR, "with qty structure
     ' ' 'MARC-LOSGR'  L_LOSGR, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_MM01 USING    P_TABIX.
  DATA L_MESSG LIKE CFGNL-MSGLIN.

  CALL TRANSACTION 'MM01'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.
  IT_AMMC-ZRESULT = SY-MSGTY.

  PERFORM RKC_MSG_STRING CHANGING L_MESSG.

  IT_AMMC-ZMSG = L_MESSG.
  PERFORM ZSBM_IF_TIME_CHANGE USING     IT_AMMC-ZRESULT
                                        IT_AMMC-ZEDAT
                                        IT_AMMC-ZETIM
                               CHANGING IT_AMMC-ZBDAT
                                        IT_AMMC-ZBTIM
                                        IT_AMMC-ZBNAM
                                        IT_AMMC-ZMODE.
*   MODIFY IT_AMMC
  MODIFY IT_AMMC INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBTIM
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_AMMC.
ENDFORM.                    " CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
FORM ERROR_LOG_MAT_TYPE USING    P_TABIX.
  IT_AMMC-ZRESULT = 'E'.
  IT_AMMC-ZBDAT = SY-DATUM.
  IT_AMMC-ZBNAM = SY-UNAME.
  IT_AMMC-ZMODE = 'C'.
  IT_AMMC-ZMSG = 'MATERIAL TYPE is Mismatch'.
* MODIFY IT_AMMC
  MODIFY IT_AMMC INDEX P_TABIX TRANSPORTING ZRESULT
                                            ZBDAT
                                            ZBNAM
                                            ZMODE
                                            ZMSG.
ENDFORM.                    " ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
*&      Form  MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM MARA_DOES_NOT_EXIST_MATERIAL.
  PERFORM DYNPRO USING:
      'X' 'SAPLMGMM'    '0060',
      ' ' 'RMMG1-MATNR' IT_AMMC-MTNO,   "
      ' ' 'RMMG1-MBRSH' IT_AMMC-INDU,   "
      ' ' 'RMMG1-MTART' IT_AMMC-MTYP,   "
      ' ' 'BDC_OKCODE'  '/00'.
*   MATERIAL TYPE
  CASE IT_AMMC-MTYP.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM FERT_SELECTION_VIEW.
    WHEN 'HALB'.
      CASE IT_AMMC-SOUR.
*       MIP
        WHEN 'M'.
          PERFORM HALB_SELECTION_VIEW.
*       PHANTOM
        WHEN OTHERS.
*         SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          PERFORM HALB_PHAN_SELECTION_VIEW.
      ENDCASE.

    WHEN 'ROH'.
      PERFORM ROH_SELECTION_VIEW.
  ENDCASE.
ENDFORM.                    " MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_T023T
*&---------------------------------------------------------------------*
FORM READ_T023T CHANGING P_MATKL.
  SELECT SINGLE MATKL
              FROM T023T
              INTO P_MATKL.
ENDFORM.                    " READ_T023T
