************************************************************************
* Program Name      : ZEPP311E_COSTING_BOM
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.18.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K904261
* Addl Documentation:
* Description       : Costing BOM for annual planning
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZEPP311E_COSTING_BOM
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: PBIM,
        ZTCO_BUSPLANBOM,
        MARA,
        MARC,
        MAST,
        AENR,
        STPO.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_PBIM OCCURS 0,
        MATNR TYPE PBIM-MATNR,
        WERKS TYPE PBIM-WERKS,
      END OF IT_PBIM.
DATA: BEGIN OF IT_ZTCO OCCURS 0,
        MATNR     TYPE ZTCO_BUSPLANBOM-MATNR,
        WERKS     TYPE ZTCO_BUSPLANBOM-WERKS,
        MATNR_CHG TYPE ZTCO_BUSPLANBOM-MATNR_CHG,
      END OF IT_ZTCO.
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
        MSGTY TYPE SY-MSGTY,
        ZMSG  LIKE CFGNL-MSGLIN,
      END OF IT_BOML.

*----------------------------------------------------------------------*
* DATAS
*----------------------------------------------------------------------*
DATA: WA_AENNR_STA TYPE AENR-AENNR,
      WA_AENNR_END TYPE AENR-AENNR,
      WA_CHK,
      WA_STLA6 TYPE MAST-STLAN VALUE '7',
      WA_STLA1 TYPE MAST-STLAN VALUE '1'.
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
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
                                   USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1.
PARAMETERS: P_DATUM LIKE SY-DATUM(4).
PARAMETERS: P_FDATU LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CS01'.
SELECTION-SCREEN END   OF BLOCK B1.
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

  PERFORM CHANGE_NUMBER_CONCATENATE CHANGING WA_CHK.
  PERFORM CHCEK_MAST_STPO_USAGE CHANGING WA_CHK.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  IF P_RDO1 EQ 'X'.
*   DISPLAY MODE
    PERFORM WRITE_PROCESS.
  ELSE.
    IF WA_CHK EQ 'X'.
      WRITE: / 'Change number dose not exist' COLOR 6.
    ELSE.
*   COSTING BOM BDC
      PERFORM BOM_PROCESS.
      PERFORM BOM_WRITE_PROCESS.
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
  P_DATUM = SY-DATUM(04) + 1.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF SCREEN-NAME EQ 'P_TCODE'.
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
  PERFORM READ_PBIM.
  PERFORM READ_ZTCO_BUSPLANBOM.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PBIM
*&---------------------------------------------------------------------*
FORM READ_PBIM.
  SELECT MATNR
         WERKS
       FROM PBIM
       INTO TABLE IT_PBIM
       WHERE VERSB EQ 'Y1'.
  IF SY-SUBRC NE 0.
    WRITE: / 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_PBIM
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_BUSPLANBOM
*&---------------------------------------------------------------------*
FORM READ_ZTCO_BUSPLANBOM.
  SELECT MATNR
         WERKS
         MATNR_CHG
       FROM ZTCO_BUSPLANBOM
       INTO TABLE IT_ZTCO.
ENDFORM.                    " READ_ZTCO_BUSPLANBOM
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  REFRESH IT_LIST. CLEAR IT_LIST.

  PERFORM DATA_UNITE.

  REFRESH: IT_PBIM, IT_ZTCO. CLEAR: IT_PBIM, IT_ZTCO.

  PERFORM DATA_CHECK_MAST_STPO.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_UNITE
*&---------------------------------------------------------------------*
FORM DATA_UNITE.

  SORT IT_PBIM BY MATNR WERKS.
  SORT IT_ZTCO BY MATNR WERKS MATNR_CHG.
  LOOP AT IT_PBIM.
    MOVE-CORRESPONDING IT_PBIM TO IT_LIST.
    IT_LIST-EDATU = SY-DATUM.
    IT_LIST-STLAN = WA_STLA6.
    CONCATENATE P_DATUM '01' '01' INTO IT_LIST-FDATU.
    CONCATENATE P_DATUM '12' '31' INTO IT_LIST-TDATU.
    READ TABLE IT_ZTCO WITH KEY MATNR = IT_PBIM-MATNR
                                WERKS = IT_PBIM-WERKS
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_LIST-CMATN = IT_ZTCO-MATNR_CHG.
      IT_LIST-NMATN = IT_ZTCO-MATNR.
      IT_LIST-CWERK = IT_ZTCO-WERKS.
      IT_LIST-ZMODE = 'N'.
    ELSE.
      IT_LIST-NMATN = IT_PBIM-MATNR.
      IT_LIST-CWERK = IT_PBIM-WERKS.
    ENDIF.
    APPEND IT_LIST.
    CLEAR: IT_LIST, IT_PBIM, IT_ZTCO.
  ENDLOOP.

ENDFORM.                    " DATA_UNITE
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
FORM DATA_CHECK_MAST_STPO.
  DATA: L_STLNR TYPE MAST-STLNR,
        L_STLAL TYPE MAST-STLAL,
        L_STLKN TYPE STPO-STLKN,
        L_DATUV TYPE STPO-DATUV,
        L_TABIX TYPE SY-TABIX.
*  CONCATENATE P_DATUM '01' '01' INTO L_DATUV.
  LOOP AT IT_LIST.
    L_TABIX = SY-TABIX.
    SELECT SINGLE      STLNR
                  MAX( STLAL )
                FROM MAST
                INTO (L_STLNR, L_STLAL)
                WHERE MATNR EQ IT_LIST-MATNR
                AND   WERKS EQ IT_LIST-WERKS
                AND   STLAN EQ WA_STLA1
                GROUP BY STLNR.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE A~STLKN
           FROM STPO AS A INNER JOIN STAS AS B
                          ON A~STLNR EQ B~STLNR
           INTO L_STLKN
           WHERE A~STLNR EQ L_STLNR
           AND   B~STLAL EQ L_STLAL.
*           AND   A~DATUV EQ P_FDATU.
      IF SY-SUBRC EQ 0.
        IT_LIST-STLAL = L_STLAL.
        IT_LIST-ZMODE = 'R'.
      ELSE.
        IT_LIST-ZMODE = 'NM'.
      ENDIF.

    ELSE.
      IT_LIST-ZMODE = 'NM'.
    ENDIF.
    PERFORM READ_MAST USING    L_DATUV
                               L_STLNR
                               L_STLAL
                      CHANGING IT_LIST-STATU.
    MODIFY IT_LIST INDEX L_TABIX TRANSPORTING ZMODE
                                              STLAL
                                              STLAN
                                              STATU.
    CLEAR: IT_LIST, L_STLNR, L_STLAL.
  ENDLOOP.

ENDFORM.                    " DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
*&      Form  READ_MAST
*&---------------------------------------------------------------------*
FORM READ_MAST USING    P_DATUV
                        P_STLNR
                        P_STLAL
               CHANGING P_STATU.
  DATA L_DATUV TYPE STPO-DATUV.
  SELECT SINGLE DATUV
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO L_DATUV
          WHERE A~MATNR EQ IT_LIST-MATNR
          AND   A~WERKS EQ IT_LIST-WERKS
          AND   A~STLNR EQ P_STLNR
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
  SORT IT_LIST BY ZMODE STATU MATNR.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT IT_LIST.
    WRITE: /(05) IT_LIST-ZMODE,
            (20) IT_LIST-MATNR COLOR COL_GROUP,
            (15) IT_LIST-EDATU COLOR COL_GROUP,
            (06) IT_LIST-WERKS COLOR COL_GROUP,
            (20) IT_LIST-CMATN,
            (20) IT_LIST-NMATN,
            (12) IT_LIST-FDATU,
            (12) IT_LIST-TDATU,
            (10) IT_LIST-CWERK,
            (06) IT_LIST-STLAN,
            (06) IT_LIST-STATU.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM BOM_WRITE_PROCESS.
  SORT IT_BOML BY ZMODE STATU MATNR.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT IT_BOML.
    WRITE: /(05) IT_BOML-ZMODE,
            (20) IT_BOML-MATNR COLOR COL_GROUP,
            (15) IT_BOML-EDATU COLOR COL_GROUP,
            (06) IT_BOML-WERKS COLOR COL_GROUP,
            (20) IT_BOML-CMATN,
            (20) IT_BOML-NMATN,
            (12) IT_BOML-FDATU,
            (12) IT_BOML-TDATU,
            (10) IT_BOML-CWERK,
            (06) IT_BOML-STLAN,
            (06) IT_BOML-STATU,
            (15) IT_BOML-MSGTY,
            (100) IT_BOML-ZMSG.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*  IF WA_CHK EQ 'X'.
*    WRITE: / 'Can not execute if there is USAGE 6'.
*    WRITE: /(20) 'MATERIAL',
*            (10) 'PLANT',
*            (10) 'USAGE',
*            (10) 'Alt.BOM',
*            (10) 'FROM DATE'.
*  ENDIF.
  IF P_RDO1 EQ 'X'.
    WRITE: / 'Annual planning  :  ' COLOR 3, P_DATUM COLOR 3.
    WRITE: /(142) SY-ULINE.
    WRITE: /(05) 'MODE',
            (20) 'Planning FSC(P.I.R)',
            (15) 'Offective date',
            (06) 'Plant',
            (20) 'CO maintain FSC',
            (20) 'Costing FSC',
            (12) 'Form date',
            (12) 'To date',
            (10) 'Plant',
            (06) 'Usage',
            (06) 'Status'.
    WRITE: /(142) SY-ULINE.
  ELSE.
    WRITE: / 'Annual planning BDC :  ' COLOR 3, P_DATUM COLOR 3.
    WRITE: /(142) SY-ULINE.
    WRITE: /(05) 'MODE',
            (20) 'Planning FSC(P.I.R)',
            (15) 'Offective date',
            (06) 'Plant',
            (20) 'CO maintain FSC',
            (20) 'Costing FSC',
            (12) 'Form date',
            (12) 'To date',
            (10) 'Plant',
            (06) 'Usage',
            (06) 'Status',
            (15) 'Message Type',
            (100) 'Message'.
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
        L_LINES TYPE I.
*  CONCATENATE P_DATUM '0101-001' INTO L_AENNR.
  LOOP AT IT_LIST WHERE ZMODE NE 'NM'.
*                  OR    STATU EQ 'N'.
    REFRESH: IT_BDC. CLEAR: IT_BDC.
    IF IT_LIST-CMATN IS INITIAL.
      PERFORM BOM_COPY_CREATE USING    IT_LIST-MATNR
                                       IT_LIST-WERKS
                                       IT_LIST-STLAN
                                       IT_LIST-STLAL
                                       IT_LIST-NMATN
                                       WA_AENNR_STA.
    ELSE.
      PERFORM BOM_COPY_CREATE USING    IT_LIST-MATNR
                                       IT_LIST-WERKS
                                       IT_LIST-STLAN
                                       IT_LIST-STLAL
                                       IT_LIST-CMATN
                                       WA_AENNR_STA.
    ENDIF.
*   CALL TRANSACTION
    CALL TRANSACTION 'CS01'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    L_MSGTY = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
    MOVE-CORRESPONDING IT_LIST TO IT_BOML.
    IT_BOML-FDATU = WA_AENNR_STA(8).
    IT_BOML-TDATU = WA_AENNR_END(8).
    IT_BOML-MSGTY = L_MSGTY.
    IT_BOML-ZMSG  = L_ZMSG.
    APPEND IT_BOML. CLEAR IT_BOML.

    REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.

    PERFORM CS_BOM_EXPL_MAT_V2 USING    IT_LIST-MATNR
                                        IT_LIST-WERKS
                                        WA_STLA1
                                        IT_LIST-STLAL
                               CHANGING L_LINES.

    PERFORM BOM_OUT_POINT_OF_TIME USING    IT_LIST-MATNR
                                           IT_LIST-WERKS
                                           IT_LIST-STLAL
                                           WA_AENNR_END
                                           L_LINES.
*     CALL TRANSACTION
    CALL TRANSACTION 'CS02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.

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
                              P_AENNR.
  DATA L_DATUV(10).
  WRITE: P_FDATU TO L_DATUV.
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
                        CHANGING P_LINES.
  DATA: LT_STB    LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE.
  DATA: L_CHK,
        L_ZMSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY,
        L_LINES TYPE I,
        L_DATUV(10).
  WRITE: P_FDATU TO L_DATUV.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG                 = 0
            CAPID                 = 'PP01'
            CUOBJ                 = 000000000000000
            CUOVS                 = 0
            CUOLS                 = ' '
            DATUV                 = P_FDATU
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
  LOOP AT LT_STB WHERE XISDT EQ 'X'.
    CLEAR L_CHK.
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
                                       WA_AENNR_STA.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS01'  USING IT_BDC
                               OPTIONS FROM WA_OPT
                               MESSAGES INTO IT_MESS.
      L_MSGTY = SY-MSGTY.
      PERFORM RKC_MSG_STRING CHANGING L_ZMSG.

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
                                 CHANGING L_LINES.


      PERFORM BOM_OUT_POINT_OF_TIME USING    LT_STB-IDNRK
                                             LT_STB-WERKS
                                             LT_STB-XTLAL
                                             WA_AENNR_END
                                             L_LINES.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS02'  USING IT_BDC
                               OPTIONS FROM WA_OPT
                               MESSAGES INTO IT_MESS.
      REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
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
  CONCATENATE P_DATUM '0101-001' INTO WA_AENNR_STA.
  CONCATENATE P_DATUM '1231-001' INTO WA_AENNR_END.
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
        END   OF LT_MAST.
  SELECT  A~MATNR
          A~WERKS
          A~STLAN
          A~STLAL
          B~DATUV
          FROM MAST AS A INNER JOIN STPO AS B
                         ON A~STLNR EQ B~STLNR
          INTO  TABLE LT_MAST
          WHERE A~STLAN EQ WA_STLA6
          AND   B~DATUV EQ WA_AENNR_STA.
  IF SY-SUBRC EQ 0.
    P_CHK = 'X'.
    FORMAT RESET.
    SORT LT_MAST BY MATNR WERKS STLAN STLAL.
    WRITE: / 'Can not execute USAGE 6'.
    WRITE: /(20) 'MATERIAL',
            (10) 'PLANT',
            (10) 'USAGE',
            (10) 'Alt.BOM',
            (10) 'FROM DATE'.
    LOOP AT LT_MAST.
      WRITE: /(20) LT_MAST-MATNR,
              (10) LT_MAST-WERKS,
              (10) LT_MAST-STLAN,
              (10) LT_MAST-STLAL,
              (10) LT_MAST-DATUV.
    ENDLOOP.
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
                                    P_AENNR
                                    P_LINES.
  DATA: L_TIMES TYPE I,
        L_COUNT TYPE I.
  L_TIMES = P_LINES DIV 5.
  L_COUNT = P_LINES MOD 5.
  L_TIMES = L_TIMES + 1.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_IDNRK,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_WERKS,    "PLANT
     ' ' 'RC29N-STLAN' WA_STLA6,   "BOM usage
     ' ' 'RC29N-STLAL' P_XTLAL,    "ALT BOM
     ' ' 'RC29N-AENNR' P_AENNR,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU'.

  DO L_TIMES TIMES.
    IF L_TIMES EQ SY-INDEX.
      CASE L_COUNT.
        WHEN '0'.
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'RC29P-AUSKZ(04)' 'X',    "
             ' ' 'RC29P-AUSKZ(05)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '1'.
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '2'.
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '3'.
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '4'.
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'RC29P-AUSKZ(04)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
      ENDCASE.
    ELSE.
      PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'        '0150',
         ' ' 'RC29P-AUSKZ(01)' 'X',    "
         ' ' 'RC29P-AUSKZ(02)' 'X',    "
         ' ' 'RC29P-AUSKZ(03)' 'X',    "
         ' ' 'RC29P-AUSKZ(04)' 'X',    "
         ' ' 'RC29P-AUSKZ(05)' 'X',    "
         ' ' 'BDC_OKCODE'  '=FCDL'.
    ENDIF.
  ENDDO.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " BOM_OUT_POINT_OF_TIME
