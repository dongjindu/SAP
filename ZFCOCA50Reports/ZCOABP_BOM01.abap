************************************************************************
* Program Name      : ZCOABP_BOM01
* Author            : Andy Choi
* Creation Date     : 2004.01.27.
* Specifications By : Andy Choi
************************************************************************
REPORT ZCOABP_BOM01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: PBIM,
        ZTCO_EBUSPLANBOM,
        MARA,
        MARC,
        MAST,
        AENR,
        STPO.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_CBOM OCCURS 0,
        GJAHR     TYPE ZTCO_EBUSPLANBOM-GJAHR,    "Fiscal year
        BOMTYPE   TYPE ZTCO_EBUSPLANBOM-BOMTYPE,  "Plan Type
        MATNR     TYPE ZTCO_EBUSPLANBOM-MATNR,    "Material number
        WERKS     TYPE ZTCO_EBUSPLANBOM-WERKS,    "Plant
        DATUV     TYPE ZTCO_EBUSPLANBOM-DATUV,    "Valid-from date
        MATNR_CHG TYPE ZTCO_EBUSPLANBOM-MATNR_CHG, "Material number
        CHK_EXIST TYPE ZTCO_EBUSPLANBOM-CHK_EXIST, "Check for Existance
     END OF IT_CBOM.
DATA: BEGIN OF ITAB OCCURS 0,
        MATNR     TYPE ZTCO_EBUSPLANBOM-MATNR,    "Material number
        WERKS     TYPE ZTCO_EBUSPLANBOM-WERKS,    "Plant
        MATNR_CHG TYPE ZTCO_EBUSPLANBOM-MATNR_CHG, "Material number
        DATUV     TYPE ZTCO_EBUSPLANBOM-DATUV,    "Valid-from date
        FDATU TYPE SY-DATUM,
        TDATU TYPE SY-DATUM,
        STLAN TYPE MAST-STLAN,  "BOM usage
        STLAL TYPE MAST-STLAL,  "Alternative BOM
        STATU,
        MSGTY TYPE SY-MSGTY,
        ZMSG  LIKE CFGNL-MSGLIN,
      END OF ITAB.
DATA: BEGIN OF IT_LIST OCCURS 0,
        ZMODE(02),
        MATNR TYPE PBIM-MATNR,
        EDATU TYPE SY-DATUM,
        WERKS TYPE PBIM-WERKS,
        CMATN TYPE MARA-MATNR,
        NMATN TYPE MARA-MATNR,
        FDATU TYPE SY-DATUM,
        TDATU TYPE SY-DATUM,
        CWERK TYPE T001W-WERKS,
        STLAN TYPE MAST-STLAN,
        STLAL TYPE MAST-STLAL,
        STATU,
      END OF IT_LIST.
DATA: BEGIN OF IT_BOML OCCURS 0,
        MATNR     TYPE ZTCO_EBUSPLANBOM-MATNR,    "Material number
        WERKS     TYPE ZTCO_EBUSPLANBOM-WERKS,    "Plant
        MATNR_CHG TYPE ZTCO_EBUSPLANBOM-MATNR_CHG, "Material number
        DATUV     TYPE ZTCO_EBUSPLANBOM-DATUV,    "Valid-from date
        FDATU TYPE SY-DATUM,
        TDATU TYPE SY-DATUM,
        STLAN TYPE MAST-STLAN,
        STLAL TYPE MAST-STLAL,
        STATU,
        MSGTY TYPE SY-MSGTY,
        ZMSG  LIKE CFGNL-MSGLIN,
      END OF IT_BOML.

*----------------------------------------------------------------------*
* DATAS
*----------------------------------------------------------------------*
DATA: WA_AENNR_STA TYPE AENR-AENNR,
      WA_AENNR_END TYPE AENR-AENNR,
      WA_CHK,
      WA_LINES TYPE I,
      WA_ERROR TYPE I,
      WA_SUCCE TYPE I,
      WA_STLA6 TYPE MAST-STLAN VALUE '6',
      WA_STLA1 TYPE MAST-STLAN VALUE '1'.

DATA G_CNT TYPE I.

*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_GJAHR LIKE ZTCO_EBUSPLANBOM-GJAHR.
PARAMETERS: P_BTYPE LIKE ZTCO_EBUSPLANBOM-BOMTYPE DEFAULT 'P'.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
                                   USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1,
            P_RDO3 RADIOBUTTON GROUP R1,
            P_DALL     AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
*PARAMETERS: P_DATUV LIKE ZTCO_EBUSPLANBOM-DATUV DEFAULT SY-DATUM.
SELECT-OPTIONS: S_MATNR FOR ZTCO_EBUSPLANBOM-MATNR.

PARAMETERS: P_TCODE LIKE TSTC-TCODE DEFAULT 'CS01' NO-DISPLAY.
PARAMETERS: P_MOD      TYPE CTU_MODE DEFAULT 'E'.

SELECTION-SCREEN END   OF BLOCK B1.
PARAMETERS: P_LOG      AS CHECKBOX.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.
*
*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.
*
START-OF-SELECTION.
  WA_OPT-DISMODE = P_MOD.

  PERFORM CHANGE_NUMBER_CONCATENATE CHANGING WA_CHK.
  PERFORM CHCEK_MAST_STPO_USAGE CHANGING WA_CHK.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.

  IF P_RDO1 EQ 'X'.
*   DISPLAY MODE
    PERFORM WRITE_PROCESS.
  ELSE.
    IF WA_CHK EQ 'X'.
      WRITE: / TEXT-011 COLOR 6.
    ELSE.
*   COSTING BOM BDC
      IF P_RDO3 = 'X'.
        PERFORM BOM_USAGE_6_DELETE.
      ELSE.
        PERFORM BOM_PROCESS.
        PERFORM BOM_WRITE_PROCESS.
      ENDIF.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.

END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
  P_GJAHR = SY-DATUM(04) + 1.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF SCREEN-NAME EQ 'P_TCODE' OR SCREEN-NAME EQ 'P_BTYPE'.
      SCREEN-INPUT = 0.
    ENDIF.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_ZTCO_EBUSPLANBOM.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_EBUSPLANBOM
*&---------------------------------------------------------------------*
FORM READ_ZTCO_EBUSPLANBOM.
  SELECT GJAHR
         BOMTYPE
         MATNR
         WERKS
         DATUV
         MATNR_CHG
         CHK_EXIST
       FROM ZTCO_EBUSPLANBOM
       INTO TABLE IT_CBOM
       WHERE  GJAHR      EQ P_GJAHR
       AND    BOMTYPE    EQ P_BTYPE
       AND    MATNR      IN S_MATNR.
*       AND    DATUV      EQ P_DATUV.
  IF SY-SUBRC NE 0.
    WRITE: / TEXT-003.
  ENDIF.

ENDFORM.                    " READ_ZTCO_EBUSPLANBOM
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
* IT_CBOM-MATNR_CHG INITIAL <== IT_CBOM-MATNR
  PERFORM MATNR_CHG_INITIAL_CHECK.
  PERFORM DATA_CHECK_MAST_STPO.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MATNR_CHG_INITIAL_CHECK
*&---------------------------------------------------------------------*
FORM MATNR_CHG_INITIAL_CHECK.
  DATA: L_TABIX TYPE SY-TABIX.
  LOOP AT IT_CBOM.
    L_TABIX = SY-TABIX.
    IF IT_CBOM-MATNR_CHG IS INITIAL.
      DELETE IT_CBOM INDEX L_TABIX.
*      IT_CBOM-MATNR_CHG = IT_CBOM-MATNR.
*      MODIFY IT_CBOM INDEX L_TABIX TRANSPORTING MATNR_CHG.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MATNR_CHG_INITIAL_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
FORM DATA_CHECK_MAST_STPO.
  DATA: L_STLNR TYPE MAST-STLNR,
        L_STLAL TYPE MAST-STLAL,
        L_STLKN TYPE STPO-STLKN,
        L_DATUV TYPE STPO-DATUV,
        L_TABIX TYPE SY-TABIX.
  REFRESH IT_LIST. CLEAR IT_LIST.

  LOOP AT IT_CBOM.
    ITAB-MATNR     = IT_CBOM-MATNR.
    ITAB-WERKS     = IT_CBOM-WERKS.
    ITAB-MATNR_CHG = IT_CBOM-MATNR_CHG.
    ITAB-DATUV     = IT_CBOM-DATUV.
    ITAB-FDATU     = WA_AENNR_STA(8).
    ITAB-TDATU     = WA_AENNR_END(8).

* check ABP BOM
    SELECT SINGLE      STLNR
                  MAX( STLAL )
                FROM MAST
                INTO (L_STLNR, L_STLAL)
                WHERE MATNR EQ ITAB-MATNR
                AND   WERKS EQ ITAB-WERKS
                AND   STLAN EQ WA_STLA6                     "wa_stla1
                GROUP BY STLNR.

    IF SY-SUBRC EQ 0.
*.... what is this checking logic ? -- Andy
      SELECT SINGLE A~STLKN
           FROM STPO AS A INNER JOIN STAS AS B
                          ON A~STLNR EQ B~STLNR
           INTO L_STLKN
           WHERE A~STLNR EQ L_STLNR
           AND   B~STLAL EQ L_STLAL.
*           AND   A~DATUV EQ P_FDATU.
      IF SY-SUBRC EQ 0.
        ITAB-STLAN = WA_STLA6.
        ITAB-STLAL = L_STLAL.
      ENDIF.
    ENDIF.
    PERFORM READ_MAST USING    L_DATUV
                               L_STLNR
                               L_STLAL
                      CHANGING ITAB-STATU.
    APPEND ITAB.
    CLEAR: ITAB, L_STLNR, L_STLAL.
  ENDLOOP.


ENDFORM.                    " DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
*&      Form  READ_MAST
*&---------------------------------------------------------------------*
FORM READ_MAST USING    P_GJAHR
                        P_STLNR
                        P_STLAL
               CHANGING P_STATU.
  DATA L_DATUV TYPE STPO-DATUV.
  SELECT SINGLE DATUV
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO L_DATUV
          WHERE A~MATNR EQ ITAB-MATNR
          AND   A~WERKS EQ ITAB-WERKS
*          AND   A~STLNR EQ P_STLNR
          AND   A~STLAN EQ WA_STLA6
          AND   A~STLAL EQ P_STLAL
          AND   B~AENNR EQ WA_AENNR_STA.
  IF SY-SUBRC EQ 0.
    P_STATU = 'Y'.
  ELSE.
    P_STATU = 'N'.
  ENDIF.

ENDFORM.                    " READ_MAST
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  SORT ITAB BY MATNR.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  DESCRIBE TABLE ITAB LINES WA_LINES.

  LOOP AT ITAB.
    WRITE: /(24) ITAB-MATNR COLOR COL_GROUP,
            (06) ITAB-WERKS COLOR COL_GROUP,
            (20) ITAB-DATUV,
            (20) ITAB-MATNR_CHG,
            (12) ITAB-FDATU,
            (12) ITAB-TDATU,
            (06) ITAB-STLAN,
            (06) ITAB-STATU.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM BOM_WRITE_PROCESS.
  SORT ITAB BY MATNR.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  CLEAR: WA_ERROR, WA_LINES.
  DESCRIBE TABLE ITAB LINES WA_LINES.
  LOOP AT ITAB WHERE MSGTY EQ 'E'.
    WA_ERROR = WA_ERROR + 1.
  ENDLOOP.
  WA_SUCCE = WA_LINES - WA_ERROR.
  LOOP AT ITAB.
    WRITE: /(24) ITAB-MATNR COLOR COL_GROUP,
            (06) ITAB-WERKS COLOR COL_GROUP,
            (20) ITAB-DATUV,
            (20) ITAB-MATNR_CHG,
            (12) ITAB-FDATU,
            (12) ITAB-TDATU,
            (06) ITAB-STLAN,
            (06) ITAB-STATU,
            (15) ITAB-MSGTY,
            (100) ITAB-ZMSG.
  ENDLOOP.
*  LOOP AT IT_BOML.
*    WRITE: /(24) IT_BOML-MATNR COLOR COL_GROUP,
*            (06) IT_BOML-WERKS COLOR COL_GROUP,
*            (20) IT_BOML-DATUV,
*            (20) IT_BOML-MATNR_CHG,
*            (12) IT_BOML-FDATU,
*            (12) IT_BOML-TDATU,
*            (06) IT_BOML-STLAN,
*            (06) IT_BOML-STATU,
*            (15) IT_BOML-MSGTY,
*            (100) IT_BOML-ZMSG.
*  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  IF P_RDO1 EQ 'X'.
    WRITE: / TEXT-022 COLOR 3, P_GJAHR COLOR 3.
    WRITE: / TEXT-024 , WA_LINES COLOR 3.
    WRITE: /(142) SY-ULINE.
    WRITE: /(24)  TEXT-012,
            (06)  TEXT-013,
            (20)  TEXT-014,
            (20)  TEXT-015,
            (12)  TEXT-016,
            (12)  TEXT-017,
            (06)  TEXT-018,
            (06)  TEXT-019.
    WRITE: /(142) SY-ULINE.
  ELSE.
    WRITE: / TEXT-023 COLOR 3, P_GJAHR COLOR 3.
    WRITE: / TEXT-025 , WA_LINES COLOR 3.
    WRITE: / TEXT-026 , WA_SUCCE COLOR 3.
    WRITE: / TEXT-027 , WA_ERROR COLOR 3.
    WRITE: /(142) SY-ULINE.
    WRITE: /(24)  TEXT-012,
            (06)  TEXT-013,
            (20)  TEXT-014,
            (20)  TEXT-015,
            (12)  TEXT-016,
            (12)  TEXT-017,
            (06)  TEXT-018,
            (06)  TEXT-019,
            (15)  TEXT-020,
            (100)  TEXT-021.
    WRITE: /(142) SY-ULINE.
  ENDIF.
  FORMAT COLOR OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BOM_PROCESS
*&---------------------------------------------------------------------*
FORM BOM_PROCESS.
  DATA: L_AENNR(12),
        L_TABIX TYPE SY-TABIX,
        L_ZMSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY,
        L_LINES TYPE I,
        L_CHECK.

  CLEAR G_CNT.

*  CONCATENATE P_GJAHR '0101-001' INTO L_AENNR.
  LOOP AT ITAB.
    L_TABIX = SY-TABIX.
    CLEAR L_CHECK.

    PERFORM READ_MAST_BOM_CHECK USING   ITAB-MATNR
                                        ITAB-WERKS
                                        '6'
                                        ITAB-STLAL
                                        WA_AENNR_STA
                                CHANGING L_CHECK.
    IF L_CHECK EQ 'X'.
      ITAB-MSGTY = 'S'.
      CONCATENATE 'S000 BOM is already created:'
                   ITAB-MATNR INTO ITAB-ZMSG SEPARATED BY SPACE.
      APPEND IT_BOML.
      CLEAR IT_BOML.
      MODIFY ITAB INDEX L_TABIX TRANSPORTING MSGTY ZMSG.

    ELSE.
      DATA: FR_MATNR LIKE MAST-MATNR.

      IF ITAB-MATNR = ITAB-MATNR_CHG.
        FR_MATNR = ITAB-MATNR.
      ELSE.
        FR_MATNR = ITAB-MATNR_CHG.
      ENDIF.

      PERFORM GET_RECENT_BOM USING    FR_MATNR  ITAB-WERKS
                             CHANGING ITAB-STLAL.

      PERFORM READ_MAST_BOM_CHECK USING FR_MATNR
                                        ITAB-WERKS
                                        '1'
                                        ITAB-STLAL
                                        WA_AENNR_STA
                               CHANGING L_CHECK.

      IF L_CHECK = 'X'.
        REFRESH: IT_BDC. CLEAR: IT_BDC.
        PERFORM BOM_COPY_CREATE USING    ITAB-MATNR
                                         ITAB-WERKS
                                         ITAB-STLAN
                                         ITAB-STLAL
                                         FR_MATNR
                                         WA_AENNR_STA
                                         ITAB-DATUV.
*     CALL TRANSACTION
        read table IT_BDC index 1.
        if sy-subrc eq 0.
          CALL TRANSACTION 'CS01'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.

          L_MSGTY = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
        endif.

        MOVE-CORRESPONDING ITAB TO IT_BOML.
        ITAB-MSGTY = IT_BOML-MSGTY = L_MSGTY.
        ITAB-ZMSG  = IT_BOML-ZMSG  = L_ZMSG.
        APPEND IT_BOML.
        CLEAR IT_BOML.

        MODIFY ITAB INDEX L_TABIX TRANSPORTING MSGTY ZMSG.

        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS.

        PERFORM CS_BOM_EXPL_MAT_V2 USING    ITAB-MATNR
                                            ITAB-WERKS
                                            WA_STLA1
                                            ITAB-STLAL
                                            ITAB-DATUV
                                   CHANGING L_LINES.

        PERFORM BOM_OUT_POINT_OF_TIME USING    ITAB-MATNR
                                               ITAB-WERKS
                                               ITAB-STLAL
                                               WA_AENNR_END.
*                                               L_LINES.
*     CALL TRANSACTION
*        WA_OPT-DISMODE = 'A'.

        WA_OPT-RACOMMIT = 'X'.
        WA_OPT-NOBINPT = 'X'.

        read table it_bdc index 1.
        if sy-subrc eq 0.
          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
          REFRESH: IT_BDC, IT_MESS.
        endif.
        CLEAR: IT_BDC, IT_MESS, WA_OPT-RACOMMIT, WA_OPT-NOBINPT.
      ENDIF.
    ENDIF.
    CLEAR ITAB.
  ENDLOOP.
ENDFORM.                    " BOM_PROCESS
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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BOM_COPY_CREATE
*&---------------------------------------------------------------------*
FORM BOM_COPY_CREATE USING    P_MATNR
                              P_WERKS
                              P_STLAN
                              P_STLAL
                              P_CMATN
                              P_AENNR
                              P_DATUV.
  DATA L_DATUV(10).
  WRITE: P_DATUV TO L_DATUV.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MATNR,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_WERKS,    "PLANT
     ' ' 'RC29N-STLAN' WA_STLA6,   "BOM usage
     ' ' 'RC29N-STLAL' P_STLAL,    "ALT BOM
     ' ' 'RC29N-AENNR' P_AENNR,    "Change number
     ' ' 'BDC_OKCODE'  '=VOKO'.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0105',
     ' ' 'RC29N-MATNR' P_CMATN,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_WERKS,    "PLANT
     ' ' 'RC29N-STLAN' WA_STLA1,   "BOM usage
     ' ' 'RC29N-STLAL' P_STLAL,    "ALT BOM
     ' ' 'RC29N-DATUV' L_DATUV,    "Valid-from date
     ' ' 'BDC_OKCODE'  '=CLWI'.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0156',
     ' ' 'BDC_OKCODE'  '=MALL'.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0156',
     ' ' 'BDC_OKCODE'  '=FCUE'.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

ENDFORM.                    " BOM_COPY_CREATE
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2 USING    P_MATNR
                                 P_WERKS
                                 P_STLAN
                                 P_STLAL
                                 P_DATUV
                        CHANGING P_LINES.

  DATA: LT_STB    LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE,
        L_EXIST,
        L_CHK,
        L_ZMSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY,
        L_LINES TYPE I,
        L_DATUV(10).

  WRITE: P_DATUV TO L_DATUV.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG                 = 0
            CAPID                 = 'PP01'
            CUOBJ                 = 000000000000000
            CUOVS                 = 0
            CUOLS                 = ' '
            DATUV                 = P_DATUV
            EMENG                 = 1
            MKTLS                 = 'X'
            MEHRS                 = ' '
            MTNRV                 = P_MATNR
            STLAL                 = P_STLAL
            STLAN                 = P_STLAN
            SVWVO                 = 'X'
            WERKS                 = P_WERKS
            VRSVO                 = 'X'
*       IMPORTING
*            TOPMAT                =
*            DSTST                 =
       TABLES
            STB                   = LT_STB
            MATCAT                = LT_MATCAT
       EXCEPTIONS
            ALT_NOT_FOUND         = 1
            CALL_INVALID          = 2
            MATERIAL_NOT_FOUND    = 3
            MISSING_AUTHORIZATION = 4
            NO_BOM_FOUND          = 5
            NO_PLANT_DATA         = 6
            NO_SUITABLE_BOM_FOUND = 7
            CONVERSION_ERROR      = 8
            OTHERS                = 9.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DESCRIBE TABLE LT_STB LINES P_LINES.

  LOOP AT LT_STB. " WHERE XISDT EQ 'X'.
    CLEAR L_CHK.
    IF LT_STB-XISDT EQ 'X'.
      PERFORM CHECK_MAST_STPO USING    LT_STB-IDNRK
                                       LT_STB-WERKS
                                       LT_STB-XTLTY
                                       LT_STB-XTLNR
                                       LT_STB-XTLAL
                              CHANGING L_CHK.
      IF L_CHK EQ SPACE.
        PERFORM BOM_COPY_CREATE USING    LT_STB-IDNRK
                                         LT_STB-WERKS
                                         LT_STB-XTLAN
                                         LT_STB-XTLAL
                                         LT_STB-IDNRK
                                         WA_AENNR_STA
                                         P_DATUV.
*       CALL TRANSACTION

        read table IT_BDC index 1.
        if sy-subrc eq 0.
        CALL TRANSACTION 'CS01'  USING IT_BDC
                                 OPTIONS FROM WA_OPT
                                 MESSAGES INTO IT_MESS.
        L_MSGTY = SY-MSGTY.
        PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
        endif.

        IT_BOML-MATNR = LT_STB-IDNRK.
        IT_BOML-WERKS = LT_STB-WERKS.
        IT_BOML-STLAN = LT_STB-XTLAN.
        IT_BOML-STLAL = LT_STB-XTLAL.
        IT_BOML-FDATU = WA_AENNR_STA(8).
        IT_BOML-TDATU = WA_AENNR_END(8).
        IT_BOML-MSGTY = L_MSGTY.
        IT_BOML-ZMSG  = L_ZMSG.
        APPEND IT_BOML. CLEAR IT_BOML.

        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS.

        PERFORM CS_BOM_EXPL_MAT_V2 USING    LT_STB-IDNRK
                                            LT_STB-WERKS
                                            LT_STB-XTLAN
                                            LT_STB-XTLAL
                                            P_DATUV
                                   CHANGING L_LINES.

        PERFORM BOM_OUT_POINT_OF_TIME USING    LT_STB-IDNRK
                                               LT_STB-WERKS
                                               LT_STB-XTLAL
                                               WA_AENNR_END.
*                                               L_LINES.
*       CALL TRANSACTION
        WA_OPT-RACOMMIT = 'X'.
        WA_OPT-NOBINPT = 'X'.

        read table it_bdc index 1.
        if sy-subrc eq 0.

          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
          REFRESH: IT_BDC, IT_MESS.
        endif.

        CLEAR: IT_BDC, IT_MESS, WA_OPT-RACOMMIT, WA_OPT-NOBINPT.
      ENDIF.

    ELSE.
      IF LT_STB-ZINFO EQ 'ENG'.
        LT_STB-WERKS = 'E001'.
        PERFORM CHECK_MAST_STPO USING    LT_STB-IDNRK
                                         LT_STB-WERKS
                                         LT_STB-XTLTY
                                         LT_STB-XTLNR
                                         LT_STB-XTLAL
                                CHANGING L_CHK.
        IF L_CHK EQ SPACE.
*--------- Update Request by IY Choi at 2004-04-06.---------------------
*--------- Check the Engine's BOM. if Not exist, Skip the process.------
          PERFORM CHECK_EXIST_BOM USING    LT_STB-IDNRK
                                           LT_STB-WERKS
                                           LT_STB-XTLTY
                                           LT_STB-XTLNR
                                           LT_STB-XTLAL
                                  CHANGING L_EXIST  .
          IF L_EXIST = SPACE .
            CONTINUE        .
          ENDIF.

*--------- End of Update Request by IY Choi at 2004-04-06.--------------
          PERFORM BOM_COPY_CREATE USING    LT_STB-IDNRK
                                           LT_STB-WERKS
                                           LT_STB-XTLAN
                                           LT_STB-XTLAL
                                           LT_STB-IDNRK
                                           WA_AENNR_STA
                                           P_DATUV.
*         CALL TRANSACTION

        read table IT_BDC index 1.
        if sy-subrc eq 0.
          CALL TRANSACTION 'CS01'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
          L_MSGTY = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
        endif.
          IT_BOML-MATNR = LT_STB-IDNRK.
          IT_BOML-WERKS = LT_STB-WERKS.
          IT_BOML-STLAN = LT_STB-XTLAN.
          IT_BOML-STLAL = LT_STB-XTLAL.
          IT_BOML-FDATU = WA_AENNR_STA(8).
          IT_BOML-TDATU = WA_AENNR_END(8).
          IT_BOML-MSGTY = L_MSGTY.
          IT_BOML-ZMSG  = L_ZMSG.
          APPEND IT_BOML. CLEAR IT_BOML.

          REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
          PERFORM CS_BOM_EXPL_MAT_V2 USING    LT_STB-IDNRK
                                              LT_STB-WERKS
                                              LT_STB-XTLAN
                                              LT_STB-XTLAL
                                              P_DATUV
                                     CHANGING L_LINES.

          PERFORM BOM_OUT_POINT_OF_TIME USING    LT_STB-IDNRK
                                                 LT_STB-WERKS
                                                 LT_STB-XTLAL
                                                 WA_AENNR_END.
*                                                 L_LINES.
*         CALL TRANSACTION
          WA_OPT-RACOMMIT = 'X'.
          WA_OPT-NOBINPT = 'X'.

          read table it_bdc index 1.
          if sy-subrc eq 0.
            CALL TRANSACTION 'CS02'  USING IT_BDC
                                     OPTIONS FROM WA_OPT
                                     MESSAGES INTO IT_MESS.
            REFRESH: IT_BDC, IT_MESS.
          endif.

          CLEAR: IT_BDC, IT_MESS, WA_OPT-RACOMMIT, WA_OPT-NOBINPT.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  CHANGE_NUMBER_CONCATENATE
*&---------------------------------------------------------------------*
FORM CHANGE_NUMBER_CONCATENATE CHANGING P_CHK.
  RANGES RT_AENNR FOR AENR-AENNR.
  DATA: BEGIN OF LT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
        END   OF LT_AENR.

  CONCATENATE P_GJAHR '0101-001' INTO WA_AENNR_STA.
  CONCATENATE P_GJAHR '1231-001' INTO WA_AENNR_END.

  RT_AENNR-LOW = WA_AENNR_STA.
  RT_AENNR-SIGN = 'I'.
  RT_AENNR-OPTION = 'EQ'.
  APPEND RT_AENNR.

  RT_AENNR-LOW = WA_AENNR_END.
  RT_AENNR-SIGN = 'I'.
  RT_AENNR-OPTION = 'EQ'.
  APPEND RT_AENNR.

  SELECT AENNR
         FROM AENR
         INTO TABLE LT_AENR
         WHERE AENNR IN RT_AENNR.
  IF SY-SUBRC EQ 0.
    SORT LT_AENR BY AENNR.
    READ TABLE LT_AENR WITH KEY AENNR = WA_AENNR_STA
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      P_CHK = 'X'.
    ENDIF.

    READ TABLE LT_AENR WITH KEY AENNR = WA_AENNR_END
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      P_CHK = 'X'.
    ENDIF.
  ELSE.

    P_CHK = 'X'.

  ENDIF.
ENDFORM.                    " CHANGE_NUMBER_CONCATENATE
*&---------------------------------------------------------------------*
*&      Form  CHCEK_MAST_STPO_USAGE
*&---------------------------------------------------------------------*
FORM CHCEK_MAST_STPO_USAGE CHANGING P_CHK.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR TYPE MAST-MATNR,
          WERKS TYPE MAST-WERKS,
          STLAN TYPE MAST-STLAN,
          STLAL TYPE MAST-STLAL,
          DATUV TYPE STPO-DATUV,
          AENNR TYPE STPO-AENNR,
        END   OF LT_MAST.

*  IF P_RDO1 = 'X' OR P_RDO3 = 'X'.
  SELECT  A~MATNR
          A~WERKS
          A~STLAN
          A~STLAL
          B~DATUV
          B~AENNR
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO  TABLE LT_MAST
          WHERE A~STLAN EQ WA_STLA6
          AND   B~AENNR EQ WA_AENNR_STA.
*  ELSE.
*    SELECT  A~MATNR
*            A~WERKS
*            A~STLAN
*            A~STLAL
*            B~DATUV
*            B~AENNR
*            FROM MAST AS A INNER JOIN STPO AS B
*                           ON A~STLNR EQ B~STLNR
*            INTO  TABLE LT_MAST
*            WHERE A~STLAN EQ WA_STLA6
*              AND A~MATNR IN S_MATNR.
*  ENDIF.

  IF SY-SUBRC EQ 0.
*    P_CHK = 'X'.
*    FORMAT RESET.
*    SORT LT_MAST BY MATNR WERKS STLAN STLAL.
*    WRITE: / TEXT-004.
*    WRITE: /(20)  TEXT-005,
*            (10)  TEXT-006,
*            (10)  TEXT-007,
*            (10)  TEXT-008,
*            (10)  TEXT-009,
*            (12)  TEXT-010.
*    LOOP AT LT_MAST.
*      WRITE: /(20) LT_MAST-MATNR,
*              (10) LT_MAST-WERKS,
*              (10) LT_MAST-STLAN,
*              (10) LT_MAST-STLAL,
*              (10) LT_MAST-DATUV,
*              (12) LT_MAST-AENNR.
*    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHCEK_MAST_STPO_USAGE
*&---------------------------------------------------------------------*
*&      Form  CHECK_MAST_STPO
*&---------------------------------------------------------------------*
FORM CHECK_MAST_STPO USING    P_IDNRK
                              P_WERKS
                              P_XTLTY
                              P_XTLNR
                              P_XTLAL
                     CHANGING P_CHK.
  DATA L_MATNR LIKE MAST-MATNR.
  SELECT SINGLE A~MATNR
*          A~WERKS
*          A~STLAN
*          A~STLAL
*          B~DATUV
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO  L_MATNR
          WHERE A~MATNR EQ P_IDNRK
          AND   A~WERKS EQ P_WERKS
          AND   A~STLAN EQ WA_STLA6
          AND   A~STLAL EQ P_XTLAL
          AND   B~STLTY EQ P_XTLTY
          AND   B~DATUV EQ WA_AENNR_STA.
  IF SY-SUBRC EQ 0.
    P_CHK = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_MAST_STPO
*&---------------------------------------------------------------------*
*&      Form  BOM_OUT_POINT_OF_TIME
*&---------------------------------------------------------------------*
FORM BOM_OUT_POINT_OF_TIME USING    P_IDNRK
                                    P_WERKS
                                    P_XTLAL
                                    P_AENNR.
*                                    P_LINES.
*  DATA: L_TIMES TYPE I,
*        L_COUNT TYPE I.
*  L_TIMES = P_LINES DIV 5.
*  L_COUNT = P_LINES MOD 5.
*  L_TIMES = L_TIMES + 1.
  DATA L_CNT TYPE I.

  CLEAR L_CNT.

  IF G_CNT > 0.
    PERFORM CHECK_EXIST_ITEM CHANGING L_CNT.
  ENDIF.

  IF L_CNT = 0.
    PERFORM DYNPRO USING:
       'X'  'SAPLCSDI'    '0100',
       ' '  'RC29N-MATNR' P_IDNRK,    "NEXT MATERIAL
       ' '  'RC29N-WERKS' P_WERKS,    "PLANT
       ' '  'RC29N-STLAN' WA_STLA6,   "BOM usage
       ' '  'RC29N-STLAL' P_XTLAL,    "ALT BOM
       ' '  'RC29N-AENNR' P_AENNR,    "Change number
       ' '  'BDC_OKCODE'  '=FCPU'.

    PERFORM DYNPRO USING:
       'X'  'SAPLCSDI'    '2150',
       ' '  'BDC_OKCODE'  'MALL',

       'X'  'SAPLCSDI'    '2150',
       ' '  'BDC_OKCODE'  '=FCDL',

       'X'  'SAPLSPO1'    '0100',
       ' '  'BDC_OKCODE'  '=YES',


       'X'  'SAPLCSDI'    '2150',
       ' '  'BDC_OKCODE'  '=FCBU'.

  ENDIF.

  G_CNT = G_CNT + 1.

*  DO L_TIMES TIMES.
*    IF L_TIMES EQ SY-INDEX.
*      CASE L_COUNT.
*        WHEN '0'.
*          PERFORM DYNPRO USING:
*             'X' 'SAPLCSDI'        '0150',
*             ' ' 'RC29P-AUSKZ(01)' 'X',    "
*             ' ' 'RC29P-AUSKZ(02)' 'X',    "
*             ' ' 'RC29P-AUSKZ(03)' 'X',    "
*             ' ' 'RC29P-AUSKZ(04)' 'X',    "
*             ' ' 'RC29P-AUSKZ(05)' 'X',    "
*             ' ' 'BDC_OKCODE'  '=FCDL'.
*        WHEN '1'.
*          PERFORM DYNPRO USING:
*             'X' 'SAPLCSDI'        '0150',
*             ' ' 'RC29P-AUSKZ(01)' 'X',    "
*             ' ' 'BDC_OKCODE'  '=FCDL'.
*        WHEN '2'.
*          PERFORM DYNPRO USING:
*             'X' 'SAPLCSDI'        '0150',
*             ' ' 'RC29P-AUSKZ(01)' 'X',    "
*             ' ' 'RC29P-AUSKZ(02)' 'X',    "
*             ' ' 'BDC_OKCODE'  '=FCDL'.
*        WHEN '3'.
*          PERFORM DYNPRO USING:
*             'X' 'SAPLCSDI'        '0150',
*             ' ' 'RC29P-AUSKZ(01)' 'X',    "
*             ' ' 'RC29P-AUSKZ(02)' 'X',    "
*             ' ' 'RC29P-AUSKZ(03)' 'X',    "
*             ' ' 'BDC_OKCODE'  '=FCDL'.
*        WHEN '4'.
*          PERFORM DYNPRO USING:
*             'X' 'SAPLCSDI'        '0150',
*             ' ' 'RC29P-AUSKZ(01)' 'X',    "
*             ' ' 'RC29P-AUSKZ(02)' 'X',    "
*             ' ' 'RC29P-AUSKZ(03)' 'X',    "
*             ' ' 'RC29P-AUSKZ(04)' 'X',    "
*             ' ' 'BDC_OKCODE'  '=FCDL'.
*      ENDCASE.
*    ELSE.
*      PERFORM DYNPRO USING:
*         'X' 'SAPLCSDI'        '0150',
*         ' ' 'RC29P-AUSKZ(01)' 'X',    "
*         ' ' 'RC29P-AUSKZ(02)' 'X',    "
*         ' ' 'RC29P-AUSKZ(03)' 'X',    "
*         ' ' 'RC29P-AUSKZ(04)' 'X',    "
*         ' ' 'RC29P-AUSKZ(05)' 'X',    "
*         ' ' 'BDC_OKCODE'  '=FCDL'.
*    ENDIF.
*  ENDDO.
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCSDI'    '0150',
*     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " BOM_OUT_POINT_OF_TIME
*&---------------------------------------------------------------------*
*&      Form  BOM_USAGE_6_DELETE
*&---------------------------------------------------------------------*
FORM BOM_USAGE_6_DELETE.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR TYPE MATNR,
          WERKS TYPE WERKS_D,
          STLAN TYPE STLAN,
          STLAL TYPE STLAL,
          STLNR TYPE STNUM,
          DATUV TYPE DATUV,
        END OF LT_MAST.

  DATA L_DATUM(10).

  CLEAR LT_MAST.
  REFRESH LT_MAST.

* Physical BOM deletion
  IF P_DALL = 'X'.
    SELECT A~MATNR A~WERKS A~STLAN A~STLAL A~STLNR B~DATUV
     INTO TABLE LT_MAST
     FROM MAST AS A
     JOIN STKO AS B
       ON A~STLNR = B~STLNR
    WHERE A~STLAN = WA_STLA6
      AND B~STLTY = 'M'
      AND A~MATNR IN S_MATNR.

  ELSE.
    SELECT A~MATNR A~WERKS A~STLAN A~STLAL A~STLNR
     INTO CORRESPONDING FIELDS OF TABLE LT_MAST
     FROM MAST AS A
     JOIN STPO AS B
       ON A~STLNR = B~STLNR
    WHERE A~STLAN = WA_STLA6
      AND B~STLTY = 'M'
      AND A~MATNR IN S_MATNR.
  ENDIF.

  SORT LT_MAST BY MATNR WERKS STLAN STLAL.
  DELETE ADJACENT DUPLICATES FROM LT_MAST COMPARING ALL FIELDS.

  IF NOT LT_MAST[] IS INITIAL.
* Physical BOM deletion
    IF P_DALL = 'X'.
      WRITE:/ '*** Usage 6 Costing is being deleted.'.

      LOOP AT LT_MAST.
        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS, L_DATUM.

        CONCATENATE LT_MAST-DATUV+4(4) LT_MAST-DATUV+0(4) INTO L_DATUM.

        PERFORM DYNPRO USING:
           'X' 'SAPLCSDI'    '0100',
           ' ' 'RC29N-MATNR' LT_MAST-MATNR,  "NEXT MATERIAL
           ' ' 'RC29N-WERKS' LT_MAST-WERKS,  "PLANT
           ' ' 'RC29N-STLAN' LT_MAST-STLAN,  "BOM usage
           ' ' 'RC29N-STLAL' LT_MAST-STLAL,  "ALT BOM
           ' ' 'RC29N-DATUV' L_DATUM,
           ' ' 'BDC_OKCODE'  '=KALL',

           'X' 'SAPLCSDI'    '2110',
           ' ' 'BDC_OKCODE'  '=FCLO'.

        read table it_bdc index 1.
        if sy-subrc eq 0.
          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
          IF P_LOG = 'X'.
            WRITE:/ 'RC:', SY-SUBRC,
                    'Material:', LT_MAST-MATNR, '...processed'.
          ENDIF.
        endif.
      ENDLOOP.

* Logical BOM deletion
    ELSE.
      LOOP AT LT_MAST.
        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS.

        PERFORM BOM_OUT_POINT_OF_TIME USING LT_MAST-MATNR
                                            LT_MAST-WERKS
                                            LT_MAST-STLAL
                                            WA_AENNR_STA.

        WA_OPT-RACOMMIT = 'X'.
        WA_OPT-NOBINPT = 'X'.

        read table it_bdc index 1.
        if sy-subrc eq 0.
          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.

          IF P_LOG = 'X'.
            WRITE:/ 'RC:', SY-SUBRC,
                    'Material:', LT_MAST-MATNR, '...processed'.
          ENDIF.
        endif.

        CLEAR: WA_OPT-RACOMMIT, WA_OPT-NOBINPT.
      ENDLOOP.
    ENDIF.

  ENDIF.

*  IF P_DALL = 'X'.
*    SELECT A~MATNR
*           A~WERKS
*           A~STLAN
*           A~STLAL
*           A~STLNR
*         FROM MAST AS A INNER JOIN STPO AS B
*                        ON A~STLNR EQ B~STLNR
*         INTO TABLE LT_MAST
*         WHERE A~STLAN EQ WA_STLA6
**        AND   b~aennr EQ wa_aennr_sta
*           AND   B~STLTY EQ 'M'.
*  ELSE.
*    SELECT A~MATNR
*           A~WERKS
*           A~STLAN
*           A~STLAL
*           A~STLNR
*         FROM MAST AS A INNER JOIN STPO AS B
*                        ON A~STLNR EQ B~STLNR
*         INTO TABLE LT_MAST
*         WHERE A~STLAN EQ WA_STLA6
**        AND   b~aennr EQ wa_aennr_sta
*           AND   B~STLTY EQ 'M'
*           AND A~MATNR IN S_MATNR.
*
*  ENDIF.
*
*  IF SY-SUBRC EQ 0.
*    REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
*    SORT LT_MAST BY MATNR WERKS STLAN STLAL.
*    DELETE ADJACENT DUPLICATES FROM LT_MAST COMPARING ALL FIELDS.
*
*    WRITE:/ '*** Usage 6 Costing is being deleted.'.
*
*    LOOP AT LT_MAST.
*      SELECT SINGLE * FROM STPO WHERE STLTY = 'M'
*                                  AND STLNR = LT_MAST-STLNR.
*
*      WRITE: STPO-DATUV TO L_DATUM.
*
*      PERFORM DYNPRO USING:
*         'X' 'SAPLCSDI'    '0100',
*         ' ' 'RC29N-MATNR' LT_MAST-MATNR,  "NEXT MATERIAL
*         ' ' 'RC29N-WERKS' LT_MAST-WERKS,  "PLANT
*         ' ' 'RC29N-STLAN' LT_MAST-STLAN,  "BOM usage
*         ' ' 'RC29N-STLAL' LT_MAST-STLAL,  "ALT BOM
*         ' ' 'RC29N-DATUV' L_DATUM,        "Change number
*         ' ' 'BDC_OKCODE'  '/00',
*
*         'X' 'SAPLCSDI'    '0150',
*         ' ' 'BDC_OKCODE'  '=FCLO'.
*
*      CALL TRANSACTION 'CS02'  USING IT_BDC
*                               OPTIONS FROM WA_OPT
*                               MESSAGES INTO IT_MESS.
*      IF P_LOG = 'X'.
*        WRITE:/ 'RC:', SY-SUBRC,
*                'Material:', LT_MAST-MATNR, '...processed'.
*      ENDIF.
*      REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
*      CLEAR LT_MAST.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " BOM_USAGE_6_DELETE
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_BOM_CHECK
*&---------------------------------------------------------------------*
FORM READ_MAST_BOM_CHECK USING    P_MATNR
                                  P_WERKS
                                  P_STLAN
                                  P_STLAL
                                  P_AENNR
                         CHANGING P_CHECK.
  DATA L_MATNR TYPE MAST-MATNR.

  IF P_STLAN = '6'.
    SELECT SINGLE A~MATNR
         FROM MAST AS A INNER JOIN STPO AS B
                        ON A~STLNR EQ B~STLNR
         INTO  L_MATNR
         WHERE A~MATNR EQ P_MATNR
         AND   A~WERKS EQ P_WERKS
         AND   A~STLAN EQ P_STLAN
         AND   A~STLAL EQ P_STLAL
         AND   B~AENNR EQ P_AENNR
         AND   B~STLTY EQ 'M'.
  ELSE.
    SELECT SINGLE A~MATNR
         FROM MAST AS A INNER JOIN STPO AS B
                        ON A~STLNR EQ B~STLNR
         INTO  L_MATNR
         WHERE A~MATNR EQ P_MATNR
         AND   A~WERKS EQ P_WERKS
         AND   A~STLAN EQ P_STLAN
         AND   A~STLAL EQ P_STLAL
         AND   B~STLTY EQ 'M'.
  ENDIF.

  IF SY-SUBRC EQ 0.
    CLEAR L_MATNR.
    P_CHECK = 'X'.
  ENDIF.
ENDFORM.                    " READ_MAST_BOM_CHECK

*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM CHECK_EXIST_BOM USING    PA_MATNR  PA_WERKS  PA_XTLTY
                              PA_XTLNR  PA_XTLAL
                     CHANGING PA_EXIST  .
  DATA L_MATNR LIKE MAST-MATNR.

  CLEAR: PA_EXIST.
  SELECT SINGLE A~MATNR
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO  L_MATNR
          WHERE A~MATNR EQ PA_MATNR
          AND   A~WERKS EQ PA_WERKS
          AND   A~STLAN EQ WA_STLA1
          AND   A~STLAL EQ PA_XTLAL
          AND   B~STLTY EQ PA_XTLTY
          AND   B~DATUV EQ WA_AENNR_STA.
  IF SY-SUBRC EQ 0.
    PA_EXIST = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_EXIST_BOM
*&---------------------------------------------------------------------*
*&      Form  get_recent_bom
*&---------------------------------------------------------------------*
FORM GET_RECENT_BOM USING    F_MATNR
                             F_WERKS
                    CHANGING F_STLAL.

  SELECT SINGLE MAX( STLAL ) INTO F_STLAL
          FROM MAST
              WHERE MATNR EQ F_MATNR
              AND   WERKS EQ F_WERKS
              AND   STLAN EQ WA_STLA1
              GROUP BY STLNR.
  IF SY-SUBRC EQ 0.

  ENDIF.
ENDFORM.                    " get_recent_bom
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST_ITEM
*&---------------------------------------------------------------------*
*       Check Exist Item
*----------------------------------------------------------------------*
FORM CHECK_EXIST_ITEM CHANGING P_CNT TYPE I.
  SELECT COUNT(*) INTO P_CNT
    FROM MAST AS A
    JOIN STKO AS B
      ON B~STLNR = A~STLNR
     AND B~STLAL = A~STLAL
    JOIN STAS AS C
      ON C~STLTY = B~STLTY
     AND C~STLNR = B~STLNR
     AND C~STLAL = B~STLAL
     AND C~LKENZ <> 'X'
    JOIN STPO AS D
      ON D~STLTY = C~STLTY
     AND D~STLNR = C~STLNR
     AND D~STLKN = C~STLKN
   WHERE A~MATNR = ITAB-MATNR
     AND A~WERKS = ITAB-WERKS
     AND A~STLAL = ITAB-STLAL
     AND B~AENNR = WA_AENNR_STA.

ENDFORM.                    " CHECK_EXIST_ITEM
