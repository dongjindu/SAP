************************************************************************
* Program Name      : ZEPP308U_SUB_BOM_FSC_ASSIG
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.13.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : Sub material BOM FSC Assignment
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.03.26  ZDBM         UD1K907658
*
*
************************************************************************
REPORT ZEPP308U_SUB_BOM_FSC_ASSIG
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: MARA,
        ZTBM_ABXEBMDT,
        ZTBM_MODEL_VAL,
        ZTBM_SUB_BOM_VEL.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BMDT OCCURS 0,
        MATNR TYPE ZTBM_ABXEBMDT-MTNO,
        WERKS TYPE ZTBM_ABXEBMDT-PLNT,
        STLAN TYPE ZTBM_ABXEBMDT-USAG,
        STLAL TYPE ZTBM_ABXEBMDT-ALTN,
        ZMODE,
        ZMSG  LIKE CFGNL-MSGLIN,
      END OF IT_BMDT.

DATA: BEGIN OF IT_SUBM OCCURS 0,
        MATNR LIKE MAST-MATNR,
        WERKS LIKE MAST-WERKS,
        STLAN LIKE MAST-STLAN,
        STLAL LIKE MAST-STLAL,
      END   OF IT_SUBM.
DATA: IT_VEL TYPE ZTBM_SUB_BOM_VEL OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_TOT TYPE I,
      WA_LINE_ERO TYPE I.
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
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS: P_ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT DEFAULT SY-DATUM,
            P_ZBTIM LIKE ZTBM_ABXEBMDT-ZBTIM.
PARAMETERS: P_SUBMIT NO-DISPLAY.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM WRITE_PROCESS.

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
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  REFRESH IT_SUBM. CLEAR IT_SUBM.
  IF P_SUBMIT EQ 'X'.
    IMPORT IT_SUBM FROM MEMORY ID 'BOM_SUB_DATA'.
    DELETE ADJACENT DUPLICATES FROM IT_SUBM COMPARING ALL FIELDS.
  ELSE.
    SELECT MTNO
           PLNT
           USAG
           ALTN
         FROM ZTBM_ABXEBMDT
         INTO TABLE IT_SUBM
         WHERE ZEDAT EQ P_ZEDAT
         AND   ZBTIM EQ P_ZBTIM
         GROUP BY MTNO PLNT USAG ALTN.
    IF SY-SUBRC EQ 0.
      DELETE ADJACENT DUPLICATES FROM IT_SUBM COMPARING ALL FIELDS.
    ELSE.
      WRITE: / TEXT-001 COLOR 4.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM READ_MARA_CHECK_FERT.
  PERFORM READ_ZTBM_SUB_BOM_VEL.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_CHECK_FERT
*&---------------------------------------------------------------------*
FORM READ_MARA_CHECK_FERT.
  LOOP AT IT_SUBM.
    SELECT SINGLE *
                FROM MARA
                WHERE MATNR EQ IT_SUBM-MATNR
                AND   MTART EQ 'FERT'.
    IF SY-SUBRC EQ 0.
      IT_BMDT = IT_SUBM.
      APPEND IT_BMDT.
    ENDIF.
    CLEAR: IT_SUBM, IT_BMDT.
  ENDLOOP.
ENDFORM.                    " READ_MARA_CHECK_FERT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
FORM READ_ZTBM_SUB_BOM_VEL.

  SELECT *
       FROM ZTBM_SUB_BOM_VEL
       INTO TABLE IT_VEL.


ENDFORM.                    " READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: LT_VEL LIKE IT_VEL OCCURS 0 WITH HEADER LINE.
  DATA: L_TABIX TYPE SY-TABIX.
  DATA: LA_BMDT LIKE IT_BMDT.
  DATA: BEGIN OF LT_BOM OCCURS 0,
          IDNRK TYPE STPO-IDNRK,
        END OF LT_BOM.

  DESCRIBE TABLE IT_BMDT LINES WA_LINE_TOT.
  LOOP AT IT_BMDT.
    L_TABIX = SY-TABIX.
    LA_BMDT = IT_BMDT.
    AT NEW MATNR.
      REFRESH LT_VEL. CLEAR LT_VEL.
      REFRESH LT_BOM. CLEAR LT_BOM.
      PERFORM CS_BOM_EXPL_MAT_V2 TABLES   LT_BOM
                                 USING    LA_BMDT-MATNR
                                          LA_BMDT-WERKS
                                          LA_BMDT-STLAN
                                          LA_BMDT-STLAL.
      PERFORM READ_IT_VEL TABLES LT_VEL
                          USING LA_BMDT-WERKS.
    ENDAT.
    IF NOT LT_VEL[] IS INITIAL.
      PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'       '0100',
         ' ' 'RC29N-MATNR'     LA_BMDT-MATNR,    "NEXT MATERIAL
         ' ' 'RC29N-WERKS'     LA_BMDT-WERKS,   "PLANT
         ' ' 'RC29N-STLAN'     LA_BMDT-STLAN,   "BOM usage
         ' ' 'RC29N-STLAL'     LA_BMDT-STLAL,   "ALT BOM
         ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
         ' ' 'BDC_OKCODE'      '=FCPU',

            'X' 'SAPLCSDI'        '0150',
           ' ' 'BDC_OKCODE'      '=FCNP'.
      LOOP AT LT_VEL.
        READ TABLE LT_BOM WITH KEY IDNRK = LT_VEL-MATNR
                          BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC EQ 0.
          CLEAR LT_VEL.
          CONTINUE.
        ELSE.

          PERFORM DYNPRO USING:

             'X' 'SAPLCSDI'        '0140',
             ' ' 'RC29P-AUSKZ(02)' 'X'   ,        "CHECK
             ' ' 'RC29P-IDNRK(02)' LT_VEL-MATNR,    "BOM compenent
             ' ' 'RC29P-MENGE(02)' '1',          "Compenent quantity
             ' ' 'RC29P-POSTP(02)' 'L'         ,    "Item category
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0130',
             ' ' 'RC29P-ITSOB'     '50',            "procurement type
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0131',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0138',
            ' ' 'ZSTGB'            'F'    ,     "STRUCTURE TYPE
            ' ' 'ZSUFF'            '0000',      "SUFFIX NO
            ' ' 'ZSEQU'            '0001',      "SEQUENCE NO
             ' ' 'BDC_OKCODE'      '/00',
             'X' 'SAPLCSDI'        '0140',
             ' ' 'BDC_OKCODE'      '=FCNP'.
        ENDIF.
        CLEAR LT_VEL.
      ENDLOOP.

      PERFORM DYNPRO USING:
              'X' 'SAPLCSDI'    '0140',
              ' ' 'BDC_OKCODE'  '=FCBU'.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS02'  USING IT_BDC
                               OPTIONS FROM WA_OPT
                               MESSAGES INTO IT_MESS.
      IT_BMDT-ZMODE = SY-MSGTY.
      PERFORM RKC_MSG_STRING CHANGING IT_BMDT-ZMSG.
      IF IT_BMDT-ZMODE EQ 'E'.
        WA_LINE_ERO = WA_LINE_ERO + 1.
      ENDIF.
      REFRESH: IT_BDC, IT_MESS.

    ELSE.
      WA_LINE_ERO = WA_LINE_ERO + 1.
      IT_BMDT-ZMODE = 'E'.

      IT_BMDT-ZMSG = 'Sub material NO ITEM'.
    ENDIF.
    MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING ZMODE
                                              ZMSG.
    CLEAR: IT_BMDT, LA_BMDT.
  ENDLOOP.
*  READ TABLE IT_BMDT WITH KEY ZMODE = 'E'.
*  IF SY-SUBRC NE 0.
*    FORMAT RESET.
*    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
*    WRITE: / TEXT-002,
*    WA_LINE_TOT.
*    WRITE: / TEXT-003,
*     WA_LINE_ERO.
*    FORMAT COLOR OFF.
*  ENDIF.
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
*&---------------------------------------------------------------------*
*&      Form  READ_IT_VEL
*&---------------------------------------------------------------------*
FORM READ_IT_VEL TABLES PT_VEL STRUCTURE IT_VEL
                 USING  P_WERKS.
  DATA: MT_VEL LIKE IT_VEL OCCURS 0 WITH HEADER LINE.
  DATA: L_ZVALUE TYPE ZTBM_MODEL_VAL-ZVALUE,
        L_Z_NATION TYPE ZTBM_SUB_BOM_VEL-Z_NATION.
  LOOP AT IT_VEL WHERE WERKS EQ P_WERKS
                 AND (  Z_CAR EQ IT_BMDT-MATNR+6(2)
                      OR Z_CAR EQ '*' ).
    PT_VEL = IT_VEL.
    APPEND PT_VEL.
    CLEAR: IT_VEL, PT_VEL.
  ENDLOOP.

  LOOP AT PT_VEL WHERE Z_YEAR EQ IT_BMDT-MATNR(1)
                 OR    Z_YEAR EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.

  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  SELECT SINGLE ZVALUE
              FROM ZTBM_MODEL_VAL
              INTO L_ZVALUE
              WHERE ZFIELD EQ '03'
              AND   ZVALNM EQ IT_BMDT-MATNR+1(3).
  IF SY-SUBRC EQ 0.
    CLEAR L_Z_NATION.
    L_Z_NATION = L_ZVALUE.
  ENDIF.
  LOOP AT PT_VEL WHERE Z_NATION EQ L_Z_NATION
                 OR    Z_NATION EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT PT_VEL WHERE Z_BT EQ IT_BMDT-MATNR+8(1)
                 OR    Z_BT EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT PT_VEL WHERE Z_TL EQ IT_BMDT-MATNR+9(1)
                 OR    Z_TL EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT PT_VEL WHERE Z_EC EQ IT_BMDT-MATNR+10(1)
                 OR    Z_EC EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT PT_VEL WHERE Z_FT EQ IT_BMDT-MATNR+11(1)
                 OR    Z_FT EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT PT_VEL WHERE Z_TM EQ IT_BMDT-MATNR+12(1)
                 OR    Z_TM EQ '*'.
    MT_VEL = PT_VEL.
    APPEND MT_VEL.
    CLEAR: MT_VEL, PT_VEL.
  ENDLOOP.
  REFRESH PT_VEL. CLEAR PT_VEL.
  PT_VEL[] = MT_VEL[].
ENDFORM.                    " READ_IT_VEL
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
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA L_MAKTX LIKE MAKT-MAKTX.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT IT_BMDT WHERE ZMODE EQ 'E'.
    CLEAR L_MAKTX.
    SELECT SINGLE MAKTX
                FROM MAKT
                INTO L_MAKTX
                WHERE MATNR EQ IT_BMDT-MATNR.

    WRITE: /(20) IT_BMDT-MATNR,
            (40) L_MAKTX,
            (06) IT_BMDT-WERKS,
            (05) IT_BMDT-STLAN,
            (15) IT_BMDT-STLAL,
            (10) IT_BMDT-ZMODE,
            (100) IT_BMDT-ZMSG.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  WRITE: / TEXT-004, WA_LINE_TOT.
  WRITE: / TEXT-005, WA_LINE_ERO.
  FORMAT COLOR OFF.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /(20)  TEXT-006,
          (40)  TEXT-007,
          (06)  TEXT-008,
          (05)  TEXT-009,
          (15)  TEXT-010,
          (10)  TEXT-011,
          (100)  TEXT-012.
  FORMAT COLOR OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2 TABLES   PT_BOM
                        USING    P_MATNR
                                 P_WERKS
                                 P_STLAN
                                 P_STLAL.
  DATA: BEGIN OF LT_BOM OCCURS 0,
          IDNRK TYPE STPO-IDNRK,
        END OF LT_BOM.

  DATA: BEGIN OF SELPOOL OCCURS 0.
          INCLUDE STRUCTURE CSTMAT.
  DATA: END OF SELPOOL.
  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD.
  CLEAR: L_CUOBJ.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = 'PP01'
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_ZEDAT
            EMENG  = '1'
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = P_MATNR
            STPST  = 0
            STLAN  = P_STLAN
            STLAL  = P_STLAL
            SVWVO  = 'X'
            WERKS  = P_WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = SELPOOL
 EXCEPTIONS
   ALT_NOT_FOUND               = 1
   CALL_INVALID                = 2
   MATERIAL_NOT_FOUND          = 3
   MISSING_AUTHORIZATION       = 4
   NO_BOM_FOUND                = 5
   NO_PLANT_DATA               = 6
   NO_SUITABLE_BOM_FOUND       = 7
   CONVERSION_ERROR            = 8
   OTHERS                      = 9
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_STB.
    LT_BOM-IDNRK = LT_STB-IDNRK.
    APPEND LT_BOM.
    CLEAR LT_BOM.
  ENDLOOP.
  SORT LT_BOM BY IDNRK.
  PT_BOM[] = LT_BOM[].
ENDFORM.                    " CS_BOM_EXPL_MAT_V2
