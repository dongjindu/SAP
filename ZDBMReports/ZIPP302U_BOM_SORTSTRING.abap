************************************************************************
* Program Name      : ZIPP302U_BOM_SORTSTRING
* Author            : Bongsoo, Kim
* Creation Date     : 2003.12.18.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K905178
* Addl Documentation:
* Description       : BOM SORT STRING ADDITION
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP302U_BOM_SORTSTRING
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
        ZTBM_ABXEBMDT.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BMDT OCCURS 0.
        INCLUDE STRUCTURE ZTBM_ABXEBMDT.
DATA:   USR01 TYPE PLPO-USR01,
      END   OF  IT_BMDT.
DATA: BEGIN OF IT_BMCO OCCURS 0,
        MANDT TYPE ZTBM_ABXEBMDT-MANDT,
        UPCT TYPE ZTBM_ABXEBMDT-UPCT,
        MTNO TYPE ZTBM_ABXEBMDT-MTNO,
        PLNT TYPE ZTBM_ABXEBMDT-PLNT,
        USAG TYPE ZTBM_ABXEBMDT-USAG,
        ALTN TYPE ZTBM_ABXEBMDT-ALTN,
        PREF TYPE ZTBM_ABXEBMDT-PREF,
        COMP TYPE ZTBM_ABXEBMDT-COMP,
        SUFF TYPE ZTBM_ABXEBMDT-SUFF,
        SEQC TYPE ZTBM_ABXEBMDT-SEQC,
        SEQU TYPE ZTBM_ABXEBMDT-SEQU,
        EONO TYPE ZTBM_ABXEBMDT-EONO,
        BQTY TYPE ZTBM_ABXEBMDT-BQTY,
        HUNT TYPE ZTBM_ABXEBMDT-HUNT,
        STAT TYPE ZTBM_ABXEBMDT-STAT,
        ITCA TYPE ZTBM_ABXEBMDT-ITCA,
        QNTY TYPE ZTBM_ABXEBMDT-QNTY,
        STGB TYPE ZTBM_ABXEBMDT-STGB,
        UNIT TYPE ZTBM_ABXEBMDT-UNIT,
        SPPR TYPE ZTBM_ABXEBMDT-SPPR,
        EITM TYPE ZTBM_ABXEBMDT-EITM,
        CLPT TYPE ZTBM_ABXEBMDT-CLPT,
        DPID TYPE ZTBM_ABXEBMDT-DPID,
        UPGN TYPE ZTBM_ABXEBMDT-UPGN,
        ZUSER TYPE ZTBM_ABXEBMDT-ZUSER,
        ZSDAT TYPE ZTBM_ABXEBMDT-ZSDAT,
        ZSTIM TYPE ZTBM_ABXEBMDT-ZSTIM,
        ZEDAT TYPE ZTBM_ABXEBMDT-ZEDAT,
        ZETIM TYPE ZTBM_ABXEBMDT-ZETIM,
        ZBDAT TYPE ZTBM_ABXEBMDT-ZBDAT,
        ZBTIM TYPE ZTBM_ABXEBMDT-ZBTIM,
        ZBNAM TYPE ZTBM_ABXEBMDT-ZBNAM,
        ZMODE TYPE ZTBM_ABXEBMDT-ZMODE,
        ZRESULT TYPE ZTBM_ABXEBMDT-ZRESULT,
        ZMSG TYPE ZTBM_ABXEBMDT-ZMSG,
        USR01 TYPE PLPO-USR01,
      END OF IT_BMCO.
DATA: WA_BMCO LIKE IT_BMCO.
DATA: BEGIN OF IT_BMNC OCCURS 0,
        MANDT TYPE ZTBM_ABXEBMDT-MANDT,
        UPCT TYPE ZTBM_ABXEBMDT-UPCT,
        EONO TYPE ZTBM_ABXEBMDT-EONO,
        MTNO TYPE ZTBM_ABXEBMDT-MTNO,
        PLNT TYPE ZTBM_ABXEBMDT-PLNT,
        USAG TYPE ZTBM_ABXEBMDT-USAG,
        ALTN TYPE ZTBM_ABXEBMDT-ALTN,
        PREF TYPE ZTBM_ABXEBMDT-PREF,
        COMP TYPE ZTBM_ABXEBMDT-COMP,
        SUFF TYPE ZTBM_ABXEBMDT-SUFF,
        SEQC TYPE ZTBM_ABXEBMDT-SEQC,
        SEQU TYPE ZTBM_ABXEBMDT-SEQU,
        BQTY TYPE ZTBM_ABXEBMDT-BQTY,
        HUNT TYPE ZTBM_ABXEBMDT-HUNT,
        STAT TYPE ZTBM_ABXEBMDT-STAT,
        ITCA TYPE ZTBM_ABXEBMDT-ITCA,
        QNTY TYPE ZTBM_ABXEBMDT-QNTY,
        STGB TYPE ZTBM_ABXEBMDT-STGB,
        UNIT TYPE ZTBM_ABXEBMDT-UNIT,
        SPPR TYPE ZTBM_ABXEBMDT-SPPR,
        EITM TYPE ZTBM_ABXEBMDT-EITM,
        CLPT TYPE ZTBM_ABXEBMDT-CLPT,
        DPID TYPE ZTBM_ABXEBMDT-DPID,
        UPGN TYPE ZTBM_ABXEBMDT-UPGN,
        ZUSER TYPE ZTBM_ABXEBMDT-ZUSER,
        ZSDAT TYPE ZTBM_ABXEBMDT-ZSDAT,
        ZSTIM TYPE ZTBM_ABXEBMDT-ZSTIM,
        ZEDAT TYPE ZTBM_ABXEBMDT-ZEDAT,
        ZETIM TYPE ZTBM_ABXEBMDT-ZETIM,
        ZBDAT TYPE ZTBM_ABXEBMDT-ZBDAT,
        ZBTIM TYPE ZTBM_ABXEBMDT-ZBTIM,
        ZBNAM TYPE ZTBM_ABXEBMDT-ZBNAM,
        ZMODE TYPE ZTBM_ABXEBMDT-ZMODE,
        ZRESULT TYPE ZTBM_ABXEBMDT-ZRESULT,
        ZMSG TYPE ZTBM_ABXEBMDT-ZMSG,
        USR01 TYPE PLPO-USR01,
      END OF IT_BMNC.
DATA: WA_BMNC LIKE IT_BMNC.
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
PARAMETERS: P_ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT DEFAULT SY-DATUM,
            P_ZBTIM LIKE ZTBM_ABXEBMDT-ZBTIM.
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
  SELECT *
       FROM ZTBM_ABXEBMDT
       INTO TABLE IT_BMDT
       WHERE ZEDAT EQ P_ZEDAT
       AND   PLNT  EQ 'P001'
       AND   UPCT  EQ '1'
       AND   ZRESULT NE 'E'.
  IF SY-SUBRC EQ 0.
    DESCRIBE TABLE IT_BMDT LINES L_TABIX.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / 'BOM BDC TOTAL LINES : ', L_TABIX.
    FORMAT COLOR OFF.
  ELSE.
    WRITE: / TEXT-001.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.

  PERFORM SORT_STRING_SELECTION.


* COLOR PART & NON COLOR PART PARTITION
  PERFORM COLOR_PART_PARTITION.

  PERFORM NOT_SORTSTRING_VALUE_DELETE.

  PERFORM COLOR_PART_DUPLICATES_DELETE.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PLP0
*&---------------------------------------------------------------------*
FORM READ_PLP0 USING    P_VSPVB
                        P_PLNT
               CHANGING P_USR01.
  SELECT SINGLE USR01
              FROM PLPO
              INTO P_USR01
              WHERE PLNTY EQ 'M'
              AND   PLNNR EQ 'RP'
              AND   WERKS EQ P_PLNT
              AND   USR00 EQ P_VSPVB.
ENDFORM.                                                    " READ_PLP0
*&---------------------------------------------------------------------*
*&      Form  SORT_STRING_SELECTION
*&---------------------------------------------------------------------*
FORM SORT_STRING_SELECTION.
  DATA: L_TABIX TYPE SY-TABIX.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
          VSPVB TYPE MARC-VSPVB,
        END OF LT_MARC.
  DATA: BEGIN OF LT_MARC_CHCK OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC_CHCK.
  DATA: BEGIN OF LT_MARA_CHCK OCCURS 0,
          MATNR TYPE MARC-MATNR,
        END OF LT_MARA_CHCK.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
          MTART TYPE MARA-MTART,
        END OF LT_MARA.
  LOOP AT IT_BMDT.
    LT_MARC_CHCK-MATNR = LT_MARA_CHCK-MATNR = IT_BMDT-COMP.
    LT_MARC_CHCK-WERKS = IT_BMDT-PLNT.
    COLLECT LT_MARA_CHCK.
    COLLECT LT_MARC_CHCK.
    CLEAR: IT_BMDT, LT_MARA_CHCK, LT_MARC_CHCK.
  ENDLOOP.

  SORT LT_MARA_CHCK BY MATNR.
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

  IF NOT LT_MARA_CHCK[] IS INITIAL.
    SELECT MATNR
           MTART
         FROM MARA
         INTO TABLE LT_MARA
         FOR ALL ENTRIES IN LT_MARA_CHCK
         WHERE MATNR EQ LT_MARA_CHCK-MATNR.
  ENDIF.
  SORT LT_MARA BY MATNR.
  SORT LT_MARC BY MATNR WERKS.
* BOM UPDATE CONTROL TYPE '2' DELETE
*  DELETE IT_BMDT WHERE UPCT EQ '2'.

  LOOP AT IT_BMDT.
    L_TABIX = SY-TABIX.
    READ TABLE LT_MARA WITH KEY MATNR = IT_BMDT-COMP
                       BINARY SEARCH TRANSPORTING MTART.
    IF SY-SUBRC EQ 0.
      CASE LT_MARA-MTART.
        WHEN 'HALB' OR 'ROH'.
          IF IT_BMDT-SPPR NE '50'. "PHANTOM CHECK
            READ TABLE LT_MARC WITH KEY MATNR = IT_BMDT-COMP
                                        WERKS = IT_BMDT-PLNT
                               BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              IF LT_MARC-VSPVB IS INITIAL.
                IT_BMDT-USR01 = '18'.
                MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING USR01.
              ELSE.
                PERFORM READ_PLP0 USING    LT_MARC-VSPVB
                                           IT_BMDT-PLNT
                                  CHANGING IT_BMDT-USR01.
                MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING USR01.

              ENDIF.
            ELSE.
*              DELETE IT_BMDT INDEX L_TABIX.
            ENDIF.
          ELSE.
*            DELETE IT_BMDT INDEX L_TABIX.
          ENDIF.
        WHEN OTHERS.
*          DELETE IT_BMDT INDEX L_TABIX.
      ENDCASE.
    ELSE.
*      DELETE IT_BMDT INDEX L_TABIX.
    ENDIF.
    CLEAR: IT_BMDT, LT_MARC, LT_MARA.
  ENDLOOP.
ENDFORM.                    " SORT_STRING_SELECTION
*&---------------------------------------------------------------------*
*&      Form  NOT_SORTSTRING_VALUE_DELETE
*&---------------------------------------------------------------------*
FORM NOT_SORTSTRING_VALUE_DELETE.

  DELETE IT_BMCO WHERE USR01 IS INITIAL.

  DELETE IT_BMNC WHERE USR01 IS INITIAL.
*  DELETE IT_BMDT WHERE CLPT NE 'C'.
ENDFORM.                    " NOT_SORTSTRING_VALUE_DELETE
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
FORM COLOR_PART_PARTITION.
  DATA L_TABIX TYPE SY-TABIX.
  LOOP AT IT_BMDT.
    IF IT_BMDT-CLPT EQ 'C'.
      MOVE-CORRESPONDING IT_BMDT TO IT_BMCO.
      CLEAR: IT_BMCO-ZRESULT, IT_BMCO-ZMSG.
      APPEND IT_BMCO. CLEAR IT_BMCO.
    ELSE.
      MOVE-CORRESPONDING IT_BMDT TO IT_BMNC.
      CLEAR: IT_BMNC-ZRESULT, IT_BMNC-ZMSG.
      APPEND IT_BMNC. CLEAR IT_BMNC.
    ENDIF.
    CLEAR IT_BMDT.
  ENDLOOP.
  REFRESH IT_BMDT. CLEAR IT_BMDT.
** BOM SORT STRING NON COLOR PART BDC TOTAL LINES
*  DESCRIBE TABLE IT_BMNC LINES WA_NONC_TOT.
** BOM SORT STRING COLOR PART BDC TOTAL LINES
*  DESCRIBE TABLE IT_BMNC LINES WA_COLO_TOT.
ENDFORM.                    " COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.

* NON COLOR PART BDC
  PERFORM NON_COLOR_PART_BDC.
* COLOR PART BDC
  PERFORM COLOR_PART_BDC.

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
*&      Form  COLOR_PART_DUPLICATES_DELETE
*&---------------------------------------------------------------------*
FORM COLOR_PART_DUPLICATES_DELETE.
* SORTING
  SORT IT_BMCO BY MTNO PLNT USAG ALTN PREF
                  COMP SUFF SEQU UPCT EONO SEQC.
  DELETE ADJACENT DUPLICATES FROM IT_BMCO COMPARING
                    MTNO PLNT USAG ALTN PREF COMP SUFF SEQU UPCT.

* BOM SORT STRING NON COLOR PART BDC TOTAL LINES
  DESCRIBE TABLE IT_BMNC LINES WA_NONC_TOT.
* BOM SORT STRING COLOR PART BDC TOTAL LINES
  DESCRIBE TABLE IT_BMCO LINES WA_COLO_TOT.
ENDFORM.                    " COLOR_PART_DUPLICATES_DELETE
*&---------------------------------------------------------------------*
*&      Form  NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM NON_COLOR_PART_BDC.
  DATA: L_TABIX TYPE SY-TABIX,
        L_STLKN TYPE STPO-STLKN,
        L_ZMSG LIKE CFGNL-MSGLIN.
* REFRESH IT_BDC. IT_MESS.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
* SORTING
  SORT IT_BMNC BY MTNO PLNT USAG ALTN PREF
                  COMP SUFF SEQU UPCT EONO.
  LOOP AT IT_BMNC.
    L_TABIX = SY-TABIX.
    WA_BMNC = IT_BMNC.
    CLEAR: L_STLKN.
*   BOM CHECK & BOM HEADER CREATED
    PERFORM MAST_STPO_STLKN USING    WA_BMNC
                            CHANGING L_STLKN.
    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0100',
       ' ' 'RC29N-MATNR' WA_BMNC-MTNO,    "NEXT MATERIAL
       ' ' 'RC29N-WERKS' WA_BMNC-PLNT,    "PLANT
       ' ' 'RC29N-STLAN' WA_BMNC-USAG,    "BOM usage
       ' ' 'RC29N-STLAL' WA_BMNC-ALTN,    "ALT BOM
       ' ' 'RC29N-AENNR' WA_BMNC-EONO,    "Change number
       ' ' 'BDC_OKCODE'  '=FCPU',

       'X' 'SAPLCSDI'    '0150',
       ' ' 'BDC_OKCODE'  '=SETP',

       'X' 'SAPLCSDI'    '0708',
       ' ' 'RC29P-SELPI' L_STLKN,    "
       ' ' 'BDC_OKCODE'  '=CLWI'.

    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0150',
       ' ' 'RC29P-AUSKZ(01)' 'X',
       ' ' 'RC29P-SORTF(01)' WA_BMNC-USR01,    "SORT STRING
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
    IT_BMNC-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
    IT_BMNC-ZMSG = L_ZMSG.
*   REFRESH IT_BDC. IT_MESS.
    MODIFY IT_BMNC INDEX L_TABIX TRANSPORTING ZRESULT ZMSG.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS.
    CLEAR: IT_BMNC.
  ENDLOOP.

* ERROR LIST WRITE
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_BMDT. CLEAR IT_BMDT.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_BMNC LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_BMNC WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
    MOVE-CORRESPONDING IT_BMNC TO IT_BMDT.
    APPEND IT_BMDT.
    CLEAR: IT_BMDT, IT_BMNC.
  ENDLOOP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-002,
           WA_NONC_TOT.
  WRITE: / TEXT-003,
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
  REFRESH IT_BMNC. CLEAR IT_BMNC.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_DATA_WRITE TABLES IT_BMDT
                             USING  'E'.
  ENDIF.

ENDFORM.                    " NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM COLOR_PART_BDC.
  DATA: L_TABIX TYPE SY-TABIX,
        L_STLKN TYPE STPO-STLKN,
        L_ZMSG LIKE CFGNL-MSGLIN.
* REFRESH IT_BDC. IT_MESS.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
* SORTING
  SORT IT_BMCO BY MTNO PLNT USAG ALTN PREF
                  COMP SUFF SEQU UPCT EONO SEQC.
  LOOP AT IT_BMCO.
    L_TABIX = SY-TABIX.
    WA_BMCO = IT_BMCO.
    CLEAR: L_STLKN.
*   BOM CHECK & BOM HEADER CREATED
    PERFORM MAST_STPO_STLKN_COLOR USING    WA_BMCO
                                  CHANGING L_STLKN.
    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0100',
       ' ' 'RC29N-MATNR' WA_BMCO-MTNO,    "NEXT MATERIAL
       ' ' 'RC29N-WERKS' WA_BMCO-PLNT,    "PLANT
       ' ' 'RC29N-STLAN' WA_BMCO-USAG,    "BOM usage
       ' ' 'RC29N-STLAL' WA_BMCO-ALTN,    "ALT BOM
       ' ' 'RC29N-AENNR' WA_BMCO-EONO,    "Change number
       ' ' 'BDC_OKCODE'  '=FCPU',

       'X' 'SAPLCSDI'    '0150',
       ' ' 'BDC_OKCODE'  '=SETP',

       'X' 'SAPLCSDI'    '0708',
       ' ' 'RC29P-SELPI' L_STLKN,    "
       ' ' 'BDC_OKCODE'  '=CLWI'.

    PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0150',
       ' ' 'RC29P-AUSKZ(01)' 'X',
       ' ' 'RC29P-SORTF(01)' WA_BMCO-USR01,    "SORT STRING
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
    IT_BMCO-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
    IT_BMCO-ZMSG = L_ZMSG.
*   REFRESH IT_BDC. IT_MESS.
    MODIFY IT_BMCO INDEX L_TABIX TRANSPORTING ZRESULT ZMSG.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS.
    CLEAR: IT_BMCO.
  ENDLOOP.

* ERROR LIST WRITE
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_BMDT. CLEAR IT_BMDT.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_BMCO LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_BMCO WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
    MOVE-CORRESPONDING IT_BMCO TO IT_BMDT.
    APPEND IT_BMDT.
    CLEAR: IT_BMDT, IT_BMCO.
  ENDLOOP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-004,
           WA_COLO_TOT.
  WRITE: / TEXT-005,
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
  REFRESH IT_BMCO. CLEAR IT_BMCO.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_DATA_WRITE TABLES IT_BMDT
                             USING  'E'.
  ENDIF.


ENDFORM.                    " COLOR_PART_BDC
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
*&      Form  MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM MAST_STPO_STLKN USING    PA_BMNC STRUCTURE WA_BMNC
                     CHANGING P_STLKN.
  DATA: L_SEQU TYPE STPO-SEQU.
*  L_SEQU = PA_BMNC-SEQC.
  L_SEQU = PA_BMNC-SEQU.
  SELECT SINGLE B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO   P_STLKN
       WHERE A~MATNR EQ PA_BMNC-MTNO
       AND   A~WERKS EQ PA_BMNC-PLNT
       AND   A~STLAN EQ PA_BMNC-USAG
       AND   A~STLAL EQ PA_BMNC-ALTN
       AND   B~POSNR EQ PA_BMNC-PREF
       AND   B~IDNRK EQ PA_BMNC-COMP
       AND   B~SUFF  EQ PA_BMNC-SUFF
       AND   B~SEQU  EQ L_SEQU.
ENDFORM.                    " MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_STLKN_COLOR
*&---------------------------------------------------------------------*
FORM MAST_STPO_STLKN_COLOR USING    PA_BMCO STRUCTURE WA_BMCO
                     CHANGING P_STLKN.
  DATA: L_SEQU TYPE STPO-SEQU.
*  L_SEQU = PA_BMCO-SEQC.
  L_SEQU = PA_BMCO-SEQU.
  SELECT SINGLE B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO   P_STLKN
       WHERE A~MATNR EQ PA_BMCO-MTNO
       AND   A~WERKS EQ PA_BMCO-PLNT
       AND   A~STLAN EQ PA_BMCO-USAG
       AND   A~STLAL EQ PA_BMCO-ALTN
       AND   B~POSNR EQ PA_BMCO-PREF
       AND   B~IDNRK EQ PA_BMCO-COMP
       AND   B~SUFF  EQ PA_BMCO-SUFF
       AND   B~SEQU  EQ L_SEQU.
ENDFORM.                    " MAST_STPO_STLKN_COLOR
*&---------------------------------------------------------------------*
*&      Form  ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_DATA_WRITE TABLES   PT_BMDT STRUCTURE IT_BMDT
                      USING    P_ZRESULT.
  WRITE: /(20)  TEXT-006,
          (10)  TEXT-007,
          (10)  TEXT-008,
          (10)  TEXT-009,
          (10)  TEXT-010,
          (20)  TEXT-011,
          (10)  TEXT-012,
          (10)  TEXT-013,
          (15)  TEXT-014,
          (15)  TEXT-015,
          (20)  TEXT-016,
          (10)  TEXT-017,
          (10)  TEXT-018,
          (10)  TEXT-019,
          (20)  TEXT-020,
          (15)  TEXT-021,
          (10)  TEXT-022,
          (15)  TEXT-023,
          (10)  TEXT-024,
          (10)  TEXT-025,
          (30)  TEXT-026,
          (15)  TEXT-027,
          (20)  TEXT-028,
          (10)  TEXT-029,
          (15)  TEXT-030,
         (220)  TEXT-031.
  LOOP AT PT_BMDT WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(20) PT_BMDT-MTNO,
            (10) PT_BMDT-PLNT,
            (10) PT_BMDT-USAG,
            (10) PT_BMDT-ALTN,
            (10) PT_BMDT-PREF,
            (20) PT_BMDT-COMP,
            (10) PT_BMDT-SUFF,
            (10) PT_BMDT-SEQU,
            (15) PT_BMDT-SEQC,
            (15) PT_BMDT-EONO,
            (20) PT_BMDT-BQTY UNIT PT_BMDT-HUNT,
            (10) PT_BMDT-HUNT,
            (10) PT_BMDT-STAT,
            (10) PT_BMDT-ITCA,
            (20) PT_BMDT-QNTY UNIT PT_BMDT-UNIT,
            (15) PT_BMDT-STGB,
            (10) PT_BMDT-UNIT,
            (15) PT_BMDT-SPPR,
            (10) PT_BMDT-EITM,
            (10) PT_BMDT-CLPT,
            (30) PT_BMDT-DPID,
            (15) PT_BMDT-UPCT,
            (20) PT_BMDT-UPGN,
            (10) PT_BMDT-ZMODE,
            (15) PT_BMDT-ZRESULT,
            (220) PT_BMDT-ZMSG.
  ENDLOOP.

ENDFORM.                    " ERROR_DATA_WRITE
