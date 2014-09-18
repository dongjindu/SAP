*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_MAT_CONFI_CHK_F01                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
* SELECTION CONDITION
  P_ZEDAT = SY-DATUM.
*  P_ZBTIM
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_CLPT' OR 'P_UPCT'.
        SCREEN-INPUT = 0.
    ENDCASE.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_ZTBM_ABXEBMDT.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXEBMDT.
  SELECT *
       FROM ZTBM_ABXEBMDT
       INTO TABLE IT_AEBM
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM
       AND   CLPT  EQ P_CLPT
       AND   UPCT  EQ P_UPCT.
  IF SY-SUBRC NE 0.
    WRITE: / 'BOM SELECTION NO DATA' COLOR 6.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.

* TOTAL SELECTION COUNT
  DESCRIBE TABLE IT_AEBM LINES WA_LINE_IDX.
* INTERFACE LEGACY DATA ERROR COUNT
  WA_ERRO_IDX = 0.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'TOTAL SELECTION COUNT :  ', WA_LINE_IDX.
  WRITE: / 'INTERFACE LEGACY DATA ERROR COUNT :  ', WA_ERRO_IDX.
  FORMAT COLOR OFF.

* BOM EXPLODED.
  PERFORM BOM_EXPLODED TABLES IT_BOM_EXPLODED.

* MARA CONFIGURABLE MATERIAL CHECK
  IF NOT IT_BOM_EXPLODED[] IS INITIAL.
    PERFORM MARA_CONFIGURABLE_MATERIAL TABLES IT_BOM_EXPLODED.
  ENDIF.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
FORM READ_BOM TABLES P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED
              USING P_IDNRK
                    P_WERKS
                    P_DATUV
              CHANGING P_LAST.

  DATA : TEMP_ISTPOV LIKE STPOV OCCURS 0 WITH HEADER LINE.
* REFRESH: IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.
* CLEAR : IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            DATUB                      = P_DATUV
            DATUV                      = P_DATUV
            MATNR                      = P_IDNRK
            WERKS                      = P_WERKS
       IMPORTING
            TOPMAT                     = IT_MC29S
       TABLES
            WULTB                      = IT_STPOV
            EQUICAT                    = IT_CSCEQUI
            KNDCAT                     = IT_CSCKND
            MATCAT                     = IT_CSCMAT
            STDCAT                     = IT_CSCSTD
            TPLCAT                     = IT_CSCTPL
       EXCEPTIONS
            CALL_INVALID               = 1
            MATERIAL_NOT_FOUND         = 2
            NO_WHERE_USED_REC_FOUND    = 3
            NO_WHERE_USED_REC_SELECTED = 4
            NO_WHERE_USED_REC_VALID    = 5
            OTHERS                     = 6.

  TEMP_ISTPOV[] = IT_STPOV[].
  IF SY-SUBRC <> 0.
    P_LAST = 'X'.
  ELSE.
    LOOP AT TEMP_ISTPOV.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = TEMP_ISTPOV-MATNR
                                         WERKS = TEMP_ISTPOV-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                         USING TEMP_ISTPOV-MATNR
                               TEMP_ISTPOV-WERKS
                               P_DATUV
                         CHANGING P_LAST.
        READ TABLE P_BOM_EXPLODED WITH KEY MATNR = TEMP_ISTPOV-MATNR
                                           WERKS = TEMP_ISTPOV-WERKS
                                  BINARY SEARCH.
        IF SY-SUBRC NE 0.
          P_BOM_EXPLODED-MATNR = TEMP_ISTPOV-MATNR.
          P_BOM_EXPLODED-WERKS = TEMP_ISTPOV-WERKS.
          APPEND P_BOM_EXPLODED. CLEAR P_BOM_EXPLODED.
          CLEAR P_LAST.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
FORM READ_MARA TABLES PT_MARC.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT,
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR LIKE MARA-MATNR, "MATERIAL
          MTART LIKE MARA-MTART, "MATERIAL TYPE
        END   OF LT_MARA.
  DATA: L_TABIX TYPE SY-TABIX.
* INTERNAL TABLE MOVE
  LT_MARC[] = PT_MARC[].
* REFRESH
  REFRESH PT_MARC.
  SELECT MATNR
         MTART
       FROM MARA
       INTO TABLE LT_MARA
       FOR ALL ENTRIES IN LT_MARC
       WHERE MATNR EQ LT_MARC-MATNR.
  IF SY-SUBRC EQ 0.
*   SORTING
    SORT LT_MARA BY MATNR MTART.
*   MODIFY MTART(Material type)
    LOOP AT LT_MARC.
      L_TABIX = SY-TABIX.
      READ TABLE LT_MARA WITH KEY MATNR = LT_MARC-MATNR
                         BINARY SEARCH TRANSPORTING MTART.
      IF SY-SUBRC EQ 0.
        LT_MARC-MTART = LT_MARA-MTART.
        MODIFY LT_MARC INDEX L_TABIX TRANSPORTING MTART.
      ENDIF.
      CLEAR: LT_MARC, LT_MARA.
    ENDLOOP.
  ENDIF.
* INTERNAL TABLE MOVE
  PT_MARC[] = LT_MARC[].
ENDFORM.                    " READ_MARA
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM BOM_EXPLODED TABLES   P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT,
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
* MATERIAL & COMPENENT COLLECT
  LOOP AT IT_AEBM.
    LT_MARC-MATNR = IT_AEBM-MTNO.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    LT_MARC-ZEDAT = IT_AEBM-ZEDAT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC.
    LT_MARC-MATNR = IT_AEBM-COMP.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    LT_MARC-ZEDAT = IT_AEBM-ZEDAT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC, IT_AEBM.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
*   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
*                            LT_MARC-KZKFG(Configurable Material)
    PERFORM READ_MARA TABLES LT_MARC.

    LOOP AT LT_MARC WHERE MTART EQ 'FERT'.
      P_BOM_EXPLODED-MATNR = LT_MARC-MATNR.
      P_BOM_EXPLODED-WERKS = LT_MARC-WERKS.
      APPEND P_BOM_EXPLODED.
      CLEAR: P_BOM_EXPLODED, LT_MARC.
    ENDLOOP.

    LOOP AT LT_MARC WHERE MTART NE 'FERT'.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = LT_MARC-MATNR
                                         WERKS = LT_MARC-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       BOM EXPLODED.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                        USING LT_MARC-MATNR
                              LT_MARC-WERKS
                              LT_MARC-ZEDAT   "IDNRK WERKS DATUV
                        CHANGING WA_LAST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
FORM MARA_CONFIGURABLE_MATERIAL TABLES   P_BOM_EXPLODED
                                              STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
          KZKFG TYPE MARA-KZKFG,  "Configurable Material
        END   OF LT_MARA.
  DATA: L_TABIX TYPE SY-TABIX.
  SELECT MATNR
         KZKFG
       FROM MARA
       INTO TABLE LT_MARA
       FOR ALL ENTRIES IN P_BOM_EXPLODED
       WHERE MATNR EQ P_BOM_EXPLODED-MATNR.
  IF SY-SUBRC EQ 0.
*   SORTING
    SORT LT_MARA BY MATNR.
    LOOP AT P_BOM_EXPLODED.
      L_TABIX = SY-TABIX.
      READ TABLE LT_MARA WITH KEY MATNR = P_BOM_EXPLODED-MATNR
                         BINARY SEARCH TRANSPORTING KZKFG.
      IF SY-SUBRC EQ 0.
        P_BOM_EXPLODED-KZKFG = LT_MARA-KZKFG.
        MODIFY P_BOM_EXPLODED INDEX L_TABIX TRANSPORTING KZKFG.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.

  DATA: L_MSG   LIKE CFGNL-MSGLIN,
        L_TABIX TYPE SY-TABIX.

  LOOP AT IT_BOM_EXPLODED WHERE KZKFG NE 'X'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_BOM_EXPLODED-MATNR,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '4004',
       ' ' 'MARA-KZKFG'  'X',   "
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
             OPTIONS FROM WA_OPT
             MESSAGES INTO IT_MESS.
    IT_BOM_EXPLODED-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
    IT_BOM_EXPLODED-ZMSG = L_MSG.
    MODIFY IT_BOM_EXPLODED INDEX L_TABIX TRANSPORTING ZRESULT
                                                      ZMSG.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
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
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BDC EXECUTION COUNT :  ', WA_LINE_IDX.
  FORMAT COLOR OFF.

* SORTING
  SORT IT_BOM_EXPLODED BY MATNR WERKS.

  CLEAR: WA_ERRO_IDX.

  LOOP AT IT_AEBM.
    L_TABIX = SY-TABIX.
    READ TABLE IT_BOM_EXPLODED WITH KEY MATNR = IT_AEBM-MTNO
                                        WERKS = IT_AEBM-PLNT
                               BINARY SEARCH
                               TRANSPORTING ZRESULT.
    IF SY-SUBRC EQ 0.
      IF IT_BOM_EXPLODED-ZRESULT EQ 'E'.
        WA_ERRO_IDX = WA_ERRO_IDX + 1.
        IT_AEBM-ZRESULT = IT_BOM_EXPLODED-ZRESULT.
        IT_AEBM-ZMSG    = IT_BOM_EXPLODED-ZMSG.
        MODIFY IT_AEBM INDEX L_TABIX TRANSPORTING ZRESULT
                                                  ZMSG.
      ENDIF.
    ENDIF.
    CLEAR: IT_AEBM, IT_BOM_EXPLODED.
  ENDLOOP.

  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BDC SUCCESS COUNT :  ', WA_LINE_IDX.
  WRITE: / 'BDC EXECUTION ERROR COUNT :  ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE '1'.
    PERFORM WRITE_AEBM TABLES IT_AEBM
                       USING 'E'.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_AEBM
*&---------------------------------------------------------------------*
FORM WRITE_AEBM TABLES   PT_AEBM STRUCTURE IT_AEBM
                USING    L_ZRESULT.
  WRITE: /(18) 'Material number',
          (06) 'Plant',
          (10) 'BOM usage',
          (10) 'Alter BOM',
          (10) 'item no',
          (18) 'BOM component',
          (10) 'item text1',
          (10) 'SEQUENCE',
          (12) 'change no',
          (17) 'confi quantity' LEFT-JUSTIFIED,
          (04) 'Unit',
          (06) 'Stutas',
          (10) 'Catagory',
          (17) 'COMPON/QTY' LEFT-JUSTIFIED,
          (10) 'STRUCTURE',
          (04) 'Unit',
          (12) 'Procurement',
          (10) 'item text2',
          (10) 'COLOR PART',
          (30) 'DEPENDENCY',
          (10) 'UPDATE/CON',
          (15) 'UPG-VC',
          (10) 'DATE',
          (08) 'TIME',
          (12) 'BDC User ID',
          (10) 'FLAG',
          (10) 'RESULT',
          (220) 'MESSAGE'.
  LOOP AT PT_AEBM WHERE ZRESULT EQ L_ZRESULT.
    WRITE: /(18) PT_AEBM-MTNO,
            (06) PT_AEBM-PLNT,
            (10) PT_AEBM-USAG,
            (10) PT_AEBM-ALTN,
            (10) PT_AEBM-PREF,
            (18) PT_AEBM-COMP,
            (10) PT_AEBM-SUFF,
            (10) PT_AEBM-SEQU,
            (12) PT_AEBM-EONO,
            (17) PT_AEBM-BQTY,
            (04) PT_AEBM-HUNT,
            (06) PT_AEBM-STAT,
            (10) PT_AEBM-ITCA,
            (17) PT_AEBM-QNTY,
            (10) PT_AEBM-STGB,
            (04) PT_AEBM-UNIT,
            (12) PT_AEBM-SPPR,
            (10) PT_AEBM-EITM,
            (10) PT_AEBM-CLPT,
            (30) PT_AEBM-DPID,
            (10) PT_AEBM-UPCT,
            (15) PT_AEBM-UPGN,
            (10) PT_AEBM-ZBDAT,
            (08) PT_AEBM-ZBTIM,
            (12) PT_AEBM-ZBNAM,
            (10) PT_AEBM-ZMODE,
            (10) PT_AEBM-ZRESULT,
            (220) PT_AEBM-ZMSG.

  ENDLOOP.
ENDFORM.                    " WRITE_AEBM
