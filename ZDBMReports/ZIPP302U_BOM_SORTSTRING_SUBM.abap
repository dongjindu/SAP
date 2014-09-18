************************************************************************
* Program Name      : ZIPP302U_BOM_SORTSTRING_SUBM
* Author            : Bongsoo, Kim
* Creation Date     : 2004.02.12.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K907295
* Addl Documentation:
* Description       : SUB Material Sortstring Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP302U_BOM_SORTSTRING_SUBM
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: MARA,
        MARC,
        PLPO,
        ZTBM_SUB_BOM_VEL.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SUB OCCURS 0,
        WERKS TYPE ZTBM_SUB_BOM_VEL-WERKS,
        MATNR TYPE ZTBM_SUB_BOM_VEL-MATNR,
      END OF IT_SUB.

DATA: BEGIN OF IT_LIST OCCURS 0,
        MATNR TYPE MAST-MATNR,  "Material number
        WERKS TYPE MAST-WERKS,  "Plant
        STLAN TYPE MAST-STLAN,  "BOM usage
        STLAL TYPE MAST-STLAL,  "Alternative BOM
        STLKN TYPE STPO-STLKN,  "BOM item node number
        AENNR TYPE STPO-AENNR,  "Change number
        IDNRK TYPE STPO-IDNRK,  "BOM component
        SORTF TYPE STPO-SORTF,  "Sort string
        ZRESULT TYPE ZTBM_ABXEBMDT-ZRESULT,
        ZMSG  TYPE ZTBM_ABXEBMDT-ZMSG,
      END   OF IT_LIST.
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
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_NONC_TOT TYPE I,
      WA_COLO_TOT TYPE I,
      WA_CHECK.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
SELECT-OPTIONS: S_WERKS FOR ZTBM_SUB_BOM_VEL-WERKS,
                S_MATNR FOR ZTBM_SUB_BOM_VEL-MATNR.
PARAMETERS: P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  DATA L_TABIX TYPE SY-TABIX.
  SELECT WERKS
         MATNR
       FROM ZTBM_SUB_BOM_VEL
       INTO TABLE IT_SUB
       WHERE WERKS IN S_WERKS
       AND   MATNR IN S_MATNR.
  IF SY-SUBRC EQ 0.
    DESCRIBE TABLE IT_SUB LINES L_TABIX.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-001, L_TABIX.
    FORMAT COLOR OFF.
  ELSE.
    WRITE: / TEXT-002.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.

  PERFORM CS_BOM_EXPL_MAT_V2.

  PERFORM SORT_STRING_SELECTION.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PLPO
*&---------------------------------------------------------------------*
FORM READ_PLPO USING    P_VSPVB
                        P_WERKS
               CHANGING P_USR01.
  SELECT SINGLE USR01
              FROM PLPO
              INTO P_USR01
              WHERE PLNTY EQ 'M'
              AND   PLNNR EQ 'RP'
              AND   WERKS EQ P_WERKS
              AND   USR00 EQ P_VSPVB.
ENDFORM.                                                    " READ_PLPO
*&---------------------------------------------------------------------*
*&      Form  SORT_STRING_SELECTION
*&---------------------------------------------------------------------*
FORM SORT_STRING_SELECTION.
  DATA: L_TABIX TYPE SY-TABIX,
        L_USR01 TYPE PLPO-USR01.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
          VSPVB TYPE MARC-VSPVB,
        END OF LT_MARC.
  DATA: BEGIN OF LT_MARC_CHCK OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC_CHCK.
  DESCRIBE TABLE IT_LIST LINES L_TABIX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-010, L_TABIX.
  FORMAT COLOR OFF.
  LOOP AT IT_LIST.
    LT_MARC_CHCK-MATNR = IT_LIST-IDNRK.
    LT_MARC_CHCK-WERKS = IT_LIST-WERKS.
    COLLECT LT_MARC_CHCK.
    CLEAR: IT_LIST, LT_MARC_CHCK.
  ENDLOOP.

  SORT LT_MARC_CHCK BY MATNR WERKS.
  IF NOT LT_MARC_CHCK[] IS INITIAL.
    SELECT MATNR
           WERKS
           VSPVB
         FROM MARC
         INTO TABLE LT_MARC
         FOR ALL ENTRIES IN LT_MARC_CHCK
         WHERE MATNR EQ LT_MARC_CHCK-MATNR
         AND   WERKS EQ LT_MARC_CHCK-WERKS.
  ENDIF.

  SORT LT_MARC BY MATNR WERKS.

  LOOP AT IT_LIST.
    L_TABIX = SY-TABIX.
    READ TABLE LT_MARC WITH KEY MATNR = IT_LIST-IDNRK
                                WERKS = IT_LIST-WERKS
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF LT_MARC-VSPVB IS INITIAL.
        IT_LIST-SORTF = '18'.
        MODIFY IT_LIST INDEX L_TABIX TRANSPORTING SORTF.
      ELSE.
        CLEAR L_USR01.
        PERFORM READ_PLPO USING    LT_MARC-VSPVB
                                   IT_LIST-WERKS
                          CHANGING L_USR01.
        IT_LIST-SORTF = L_USR01.

        MODIFY IT_LIST INDEX L_TABIX TRANSPORTING SORTF.

      ENDIF.
    ENDIF.
    CLEAR: IT_LIST, LT_MARC.
  ENDLOOP.
ENDFORM.                    " SORT_STRING_SELECTION
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.

* NON COLOR PART BDC
  PERFORM SUB_MATERIAL_SORTSTRING_BDC.

ENDFORM.                    " BDC_PROCESS
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
*&      Form  SUB_MATERIAL_SORTSTRING_BDC
*&---------------------------------------------------------------------*
FORM SUB_MATERIAL_SORTSTRING_BDC.
  DATA: L_TABIX TYPE SY-TABIX,
        L_STLKN TYPE STPO-STLKN,
        L_ZMSG LIKE CFGNL-MSGLIN.
* REFRESH IT_BDC. IT_MESS.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
* DELETE
  DELETE IT_LIST WHERE SORTF IS INITIAL.

  LOOP AT IT_LIST.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0100',
       ' ' 'RC29N-MATNR' IT_LIST-MATNR,   "NEXT MATERIAL
       ' ' 'RC29N-WERKS' IT_LIST-WERKS,   "PLANT
       ' ' 'RC29N-STLAN' IT_LIST-STLAN,   "BOM usage
       ' ' 'RC29N-STLAL' IT_LIST-STLAL,   "ALT BOM
       ' ' 'RC29N-AENNR' IT_LIST-AENNR,   "Change number
       ' ' 'BDC_OKCODE'  '=FCPU',

       'X' 'SAPLCSDI'    '0150',
       ' ' 'BDC_OKCODE'  '=SETP',

       'X' 'SAPLCSDI'    '0708',
       ' ' 'RC29P-SELPI' IT_LIST-STLKN,    "
       ' ' 'BDC_OKCODE'  '=CLWI'.

    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0150',
       ' ' 'RC29P-AUSKZ(01)' 'X',
       ' ' 'RC29P-SORTF(01)' IT_LIST-SORTF,    "SORT STRING
       ' ' 'BDC_OKCODE'  '=FCBU',

       'X' 'SAPLCSDI'    '0130',
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCSDI'    '0131',
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCSDI'    '0138',
       ' ' 'BDC_OKCODE'  '/00'.
*   CALL TRANSACTION
    CALL TRANSACTION 'CS02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    IT_LIST-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
    IT_LIST-ZMSG = L_ZMSG.
*   REFRESH IT_BDC. IT_MESS.
    MODIFY IT_LIST INDEX L_TABIX TRANSPORTING ZRESULT ZMSG.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS.
    CLEAR: IT_LIST.
  ENDLOOP.

* ERROR LIST WRITE
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_LIST LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_LIST WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
    CLEAR: IT_LIST.
  ENDLOOP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-003, WA_LINE_IDX.
  WRITE: / TEXT-004, WA_ERRO_IDX.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_DATA_WRITE TABLES IT_LIST
                             USING  'E'.
  ENDIF.

ENDFORM.                    " SUB_MATERIAL_SORTSTRING_BDC
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
*&      Form  ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_DATA_WRITE TABLES   PT_LIST STRUCTURE IT_LIST
                      USING    P_ZRESULT.
  WRITE: /(20)  TEXT-006,
          (10)  TEXT-007,
          (10)  TEXT-008,
          (18)  TEXT-009,
*          (10)  TEXT-010,
          (20)  TEXT-011,
          (20)  TEXT-012,
          (15)  TEXT-013,
          (15)  TEXT-014,
          (100) TEXT-015.
  LOOP AT PT_LIST WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(20) PT_LIST-MATNR,
            (10) PT_LIST-WERKS,
            (10) PT_LIST-STLAN,
            (18) PT_LIST-STLAL,
            (20) PT_LIST-AENNR,
            (20) PT_LIST-IDNRK,
            (15) PT_LIST-SORTF,
            (15) PT_LIST-ZRESULT,
            (100) PT_LIST-ZMSG.
  ENDLOOP.

ENDFORM.                    " ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2.
  DATA: LT_STB LIKE STPOX OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_SUB.
    REFRESH LT_STB. CLEAR LT_STB.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
     EXPORTING
       CAPID                       = 'PP01'
       DATUV                       = P_DATUM
       MKTLS                       = 'X'
       MEHRS                       = 'X'
       MTNRV                       = IT_SUB-MATNR
       SVWVO                       = 'X'
       WERKS                       = IT_SUB-WERKS
       VRSVO                       = 'X'
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
      TABLES
        STB                         = LT_STB
*       MATCAT                      =
     EXCEPTIONS
       ALT_NOT_FOUND               = 1
       CALL_INVALID                = 2
       MATERIAL_NOT_FOUND          = 3
       MISSING_AUTHORIZATION       = 4
       NO_BOM_FOUND                = 5
       NO_PLANT_DATA               = 6
       NO_SUITABLE_BOM_FOUND       = 7
       CONVERSION_ERROR            = 8
       OTHERS                      = 9.
    IF SY-SUBRC EQ 0.
      LOOP AT LT_STB.
        IT_LIST-MATNR = IT_SUB-MATNR.  "Material number
        IT_LIST-WERKS = LT_STB-WERKS.  "Plant
        IT_LIST-STLAN = LT_STB-STLAN.  "BOM usage
        IT_LIST-STLAL = LT_STB-STLAL.  "Alternative BOM
        IT_LIST-STLKN = LT_STB-STLKN.  "BOM item node number
        IT_LIST-AENNR = LT_STB-AENNR.  "Change number
        IT_LIST-IDNRK = LT_STB-IDNRK.  "BOM component
*        IT_LIST-SORTF = LT_STB-SORTF.  "Sort string
        APPEND IT_LIST. CLEAR IT_LIST.
        CLEAR LT_STB.
      ENDLOOP.
      REFRESH LT_STB. CLEAR LT_STB.
    ELSE.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CS_BOM_EXPL_MAT_V2
