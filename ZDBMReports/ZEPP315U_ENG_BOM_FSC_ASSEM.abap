************************************************************************
* Program Name      : ZEPP315U_ENG_BOM_FSC_ASSEM
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.13.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : Engine Assembly ALC code Link to FSC Material BOM
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.03.25  ZDBM         UD1K907658
*
*
************************************************************************
REPORT ZEPP315U_ENG_BOM_FSC_ASSEM
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
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_TOT TYPE I,
      WA_LINE_ERO TYPE I,
      WA_TOP_PAGE.
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
PARAMETERS  P_SUBMIT NO-DISPLAY.
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
*       AND   ZBTIM EQ P_ZBTIM
         GROUP BY MTNO PLNT USAG ALTN.
*         WHERE MTNO EQ '2B06AADAFDH6B 3941'.
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

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_CHECK_FERT
*&---------------------------------------------------------------------*
FORM READ_MARA_CHECK_FERT.
  DATA: L_TABIX TYPE SY-TABIX,
        L_WORKO(40),
        L_STLAL(03).
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
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  DATA: LA_BMDT LIKE IT_BMDT,
        L_IDNRK LIKE MARA-MATNR,
        M_IDNRK LIKE MARA-MATNR,
        L_ZINFO LIKE STPO-ZINFO,
        L_SELID LIKE RC29P-SELID,       "Component
        L_SELPI LIKE RC29P-SELPI,       "Item ID
        L_CHK.
  LOOP AT IT_BMDT.
    L_TABIX = SY-TABIX.
    LA_BMDT = IT_BMDT.
    AT NEW STLAL.
      CLEAR L_IDNRK.
      PERFORM SEARCH_ENGINE_MATERIAL USING    LA_BMDT-MATNR
                                              LA_BMDT-STLAL
                                     CHANGING L_IDNRK.
    ENDAT.
    IF NOT L_IDNRK IS INITIAL.
      CLEAR: L_CHK.
      PERFORM CHECK_COMPONENT USING    LA_BMDT-MATNR
                                       LA_BMDT-WERKS
                                       LA_BMDT-STLAN
                                       LA_BMDT-STLAL
                                       L_IDNRK
                              CHANGING L_CHK
                                       M_IDNRK
                                       L_ZINFO
                                       L_SELPI.
*     L_CHK EQ A ==> HC90  EQ HC90
*     L_CHK EQ B ==> HC89  NE HC90
*     L_CHK EQ C ==> SPACE
      CASE L_CHK.
        WHEN 'A'.
*       SUCCESS MESSAGE
          IF L_ZINFO IS INITIAL.
*       SUCCESS MESSAGE
            PERFORM DYNPRO USING:
               'X' 'SAPLCSDI'       '0100',
               ' ' 'RC29N-MATNR'     LA_BMDT-MATNR,   "NEXT MATERIAL
               ' ' 'RC29N-WERKS'     LA_BMDT-WERKS,   "PLANT
               ' ' 'RC29N-STLAN'     LA_BMDT-STLAN,   "BOM usage
               ' ' 'RC29N-STLAL'     LA_BMDT-STLAL,   "ALT BOM
               ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
               ' ' 'BDC_OKCODE'      '=FCPU',

                  'X' 'SAPLCSDI'        '0150',
                 ' ' 'BDC_OKCODE'      '=SETP'.

            PERFORM DYNPRO USING:
               'X' 'SAPLCSDI'        '0708',
               ' ' 'RC29P-SELID'     L_IDNRK,      "Component
               ' ' 'RC29P-SELPI'     L_SELPI,       "Item ID
               ' ' 'BDC_OKCODE'      '=CLWI',

               'X' 'SAPLCSDI'        '0150',
               ' ' 'RC29P-AUSKZ(01)' 'X',            "procurement type
               ' ' 'BDC_OKCODE'      '=PALL',

               'X' 'SAPLCSDI'        '2130',
               ' ' 'BDC_OKCODE'      '=+002',

               'X' 'SAPLCSDI'        '2130',
              ' ' 'ZINFO'            'ENG',    "INFO
               ' ' 'BDC_OKCODE'      '=FCBU'.
*           CALL TRANSACTION
            CALL TRANSACTION 'CS02'  USING IT_BDC
                                     OPTIONS FROM WA_OPT
                                     MESSAGES INTO IT_MESS.
            IT_BMDT-ZMODE = SY-MSGTY.
            PERFORM RKC_MSG_STRING CHANGING IT_BMDT-ZMSG.
            IF IT_BMDT-ZMODE EQ 'E'.
              WA_LINE_ERO = WA_LINE_ERO + 1.
            ELSE.
              IT_BMDT-ZMODE = 'S'.
              CONCATENATE 'Next Material' LA_BMDT-MATNR
                          'Compenent' L_IDNRK 'already exists'
                                   INTO IT_BMDT-ZMSG SEPARATED BY SPACE.

            ENDIF.
          ENDIF.
        WHEN 'B'.
          WA_LINE_ERO = WA_LINE_ERO + 1.
          IT_BMDT-ZMODE = 'E'.
          CONCATENATE 'Next Material' LA_BMDT-MATNR
                      'Compenent' M_IDNRK 'Chaging Compenent' L_IDNRK
                      'Unequal ' INTO IT_BMDT-ZMSG SEPARATED BY SPACE.

        WHEN 'C'.
*       SUCCESS MESSAGE
          PERFORM DYNPRO USING:
             'X' 'SAPLCSDI'       '0100',
             ' ' 'RC29N-MATNR'     LA_BMDT-MATNR,   "NEXT MATERIAL
             ' ' 'RC29N-WERKS'     LA_BMDT-WERKS,   "PLANT
             ' ' 'RC29N-STLAN'     LA_BMDT-STLAN,   "BOM usage
             ' ' 'RC29N-STLAL'     LA_BMDT-STLAL,   "ALT BOM
             ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
             ' ' 'BDC_OKCODE'      '=FCPU',

                'X' 'SAPLCSDI'        '0150',
               ' ' 'BDC_OKCODE'      '=FCNP'.

          PERFORM DYNPRO USING:

             'X' 'SAPLCSDI'        '0140',
             ' ' 'RC29P-AUSKZ(02)' 'X'   ,        "CHECK
             ' ' 'RC29P-IDNRK(02)' L_IDNRK,       "BOM compenent
             ' ' 'RC29P-MENGE(02)' '1',           "Compenent quantity
             ' ' 'RC29P-POSTP(02)' 'L'         ,  "Item category
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0130',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0131',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0138',
            ' ' 'ZSTGB'            'F'    ,     "STRUCTURE TYPE
            ' ' 'ZSUFF'            '0000',      "SUFFIX NO
            ' ' 'ZSEQU'            '0001',      "SEQUENCE NO
            ' ' 'ZINFO'               'ENG',    "INFO
            ' ' 'BDC_OKCODE'      '/00'.

          PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'BDC_OKCODE'  '=FCBU'.
*       CALL TRANSACTION
          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
          IT_BMDT-ZMODE = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING IT_BMDT-ZMSG.
          IF IT_BMDT-ZMODE EQ 'E'.
            WA_LINE_ERO = WA_LINE_ERO + 1.
          ELSE.
            IT_BMDT-ZMODE = 'S'.
            CONCATENATE 'Next Material' LA_BMDT-MATNR
                        'Compenent' L_IDNRK 'Created'
                                   INTO IT_BMDT-ZMSG SEPARATED BY SPACE.
          ENDIF.
          REFRESH: IT_BDC, IT_MESS.
      ENDCASE.
    ELSE.
*     ERROR MESSAGE
      WA_LINE_ERO = WA_LINE_ERO + 1.
      IT_BMDT-ZMODE = 'E'.
      IT_BMDT-ZMSG = 'Engine Assembly  ALC code NO Item'.
    ENDIF.
    MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING ZMODE
                                              ZMSG.
    CLEAR: IT_BMDT, LA_BMDT.
  ENDLOOP.
  SORT IT_BMDT BY ZMODE MATNR.
  DESCRIBE TABLE IT_BMDT LINES WA_LINE_TOT.
*  FORMAT RESET.
*  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
*  WRITE: / TEXT-002, WA_LINE_TOT.
*  WRITE: / TEXT-003, WA_LINE_ERO.
*  FORMAT COLOR OFF.
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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
FORM SEARCH_ENGINE_MATERIAL USING    P_MATNR
                                     P_STLAL
                            CHANGING P_IDNRK.
  DATA: L_WORKO(40),
        L_STLAL(03),
        L_MATNR LIKE MARA-MATNR,
        L_ATINN LIKE CABN-ATINN,
        L_ATWRT LIKE AUSP-ATWRT.
  CONCATENATE '0' P_STLAL INTO L_STLAL.
  CONCATENATE P_MATNR L_STLAL INTO L_WORKO SEPARATED BY SPACE.

  SELECT SINGLE A~MATNR
              FROM MARA AS A INNER JOIN MAKT AS B
                             ON A~MATNR EQ B~MATNR
              INTO L_MATNR
              WHERE A~MTART EQ 'WOHD'
              AND   B~MAKTX EQ L_WORKO
              AND   B~SPRAS EQ SY-LANGU.
  IF SY-SUBRC EQ 0.
    SELECT SINGLE ATINN
              FROM CABN
              INTO L_ATINN
              WHERE ATNAM EQ 'P_ALC_U_1'.

    SELECT SINGLE ATWRT
                FROM AUSP
                INTO L_ATWRT
                WHERE OBJEK EQ L_MATNR
                AND   ATINN EQ L_ATINN.
    IF SY-SUBRC EQ 0.
      P_IDNRK = L_ATWRT.
    ELSE.
      CLEAR P_IDNRK.
    ENDIF.
  ENDIF.

ENDFORM.                    " SEARCH_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CHECK_COMPONENT
*&---------------------------------------------------------------------*
FORM CHECK_COMPONENT USING    P_MATNR
                              P_WERKS
                              P_STLAN
                              P_STLAL
                              P_IDNRK
                     CHANGING P_CHK
                              Q_IDNRK
                              P_ZINFO
                              P_SELPI.
  SELECT SINGLE B~IDNRK
                B~STLKN
                B~ZINFO
              FROM MAST AS A INNER JOIN STPO AS B
                             ON A~STLNR EQ B~STLNR
              INTO  (Q_IDNRK, P_SELPI, P_ZINFO)
              WHERE A~MATNR EQ P_MATNR
              AND   A~WERKS EQ P_WERKS
              AND   A~STLAN EQ P_STLAN
              AND   A~STLAL EQ P_STLAL
              AND   B~IDNRK EQ P_IDNRK.
  IF SY-SUBRC EQ 0.
    P_CHK = 'A'.
  ELSE.
    SELECT SINGLE B~IDNRK
                  B~ZINFO
                FROM MAST AS A INNER JOIN STPO AS B
                               ON A~STLNR EQ B~STLNR
                INTO  (Q_IDNRK, P_ZINFO)
                WHERE A~MATNR EQ P_MATNR
                AND   A~WERKS EQ P_WERKS
                AND   A~STLAN EQ P_STLAN
                AND   A~STLAL EQ P_STLAL
                AND   B~ZINFO EQ 'ENG'.
    IF SY-SUBRC EQ 0.
      P_CHK = 'B'.
    ELSE.
      P_CHK = 'C'.

    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_COMPONENT
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA L_MAKTX LIKE MAKT-MAKTX.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WA_TOP_PAGE = 'X'.
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
  WA_TOP_PAGE = ' '.
  LOOP AT IT_BMDT WHERE ZMODE EQ 'S'.
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
  WRITE: / TEXT-002, WA_LINE_TOT.

  WRITE: / TEXT-003, WA_LINE_ERO.

  FORMAT COLOR OFF.

  FORMAT RESET.
  IF WA_TOP_PAGE EQ 'X'.
    WRITE: / 'ERROR CODE DISPLAY'.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: /(20)  TEXT-004,
            (40)  TEXT-005,
            (06)  TEXT-006,
            (05)  TEXT-007,
            (15)  TEXT-008,
            (10)  TEXT-009,
            (100)  TEXT-010.
    FORMAT COLOR OFF.
  ELSE.
    WRITE: / 'SUCCESS CODE DISPLAY'.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: /(20)  TEXT-004,
            (40)  TEXT-005,
            (06)  TEXT-006,
            (05)  TEXT-007,
            (15)  TEXT-008,
            (10)  TEXT-009,
            (100)  TEXT-010.
    FORMAT COLOR OFF.
  ENDIF.
ENDFORM.                    " TOP_OF_PAGE
