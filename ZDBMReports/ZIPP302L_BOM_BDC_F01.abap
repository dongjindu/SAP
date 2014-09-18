*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_BDC_F01                                       *
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
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    WRITE: / 'BOM SELECTION NO DATA' COLOR 6.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS USING PA_CHECK.
* READ TABLE MARC CHECK Material number does exist .
  PERFORM READ_MARC_CHECK CHANGING PA_CHECK.

  IF PA_CHECK NE 'X'.
    PERFORM READ_MAST_STKO_STPO_CHECK CHANGING PA_CHECK.
*   REFRESH CLEAR
    REFRESH IT_MAST. CLEAR IT_MAST.
  ELSE.
    PERFORM DATA_PROCESS_ERROR_WRITE.
  ENDIF.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MARC_CHECK
*&---------------------------------------------------------------------*
FORM READ_MARC_CHECK CHANGING PA_CHECK.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC.

  LOOP AT IT_AEBM.
    LT_MARC-MATNR = IT_AEBM-MTNO.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    COLLECT LT_MARC.
    LT_MARC-MATNR = IT_AEBM-COMP.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    COLLECT LT_MARC.
  ENDLOOP.
* CLEAR
  CLEAR: WA_CHECK.
  IF NOT LT_MARC[] IS INITIAL.
    SELECT A~MATNR
           A~WERKS
           B~MTART
         FROM MARC AS A INNER JOIN MARA AS B
                        ON A~MATNR EQ B~MATNR
         INTO TABLE IT_MARC
         FOR ALL ENTRIES IN LT_MARC
         WHERE A~MATNR EQ LT_MARC-MATNR
         AND   A~WERKS EQ LT_MARC-WERKS.
    IF SY-SUBRC EQ 0.
      PERFORM MARC_CHECK_MATERIAL TABLES    IT_MARC
                                  CHANGING  WA_CHECK.
    ELSE.
*     Material number does not exist
*     LEAVE TO PROGRAM
      IT_AEBM-ZBDAT = SY-DATUM.
      IT_AEBM-ZBNAM = SY-UNAME.
      IT_AEBM-ZMODE = 'C'.
      IT_AEBM-ZRESULT = 'L'.
      IT_AEBM-ZMSG = 'Material number does not exist'.
      MODIFY       IT_AEBM TRANSPORTING ZBDAT
                                        ZBNAM
                                        ZMODE
                                        ZRESULT
                                        ZMSG
                           WHERE MTNO GE SPACE
                           AND   PLNT GE SPACE
                           AND   USAG GE SPACE
                           AND   ALTN GE SPACE
                           AND   PREF GE SPACE
                           AND   COMP GE SPACE
                           AND   SUFF GE SPACE
                           AND   SEQU GE SPACE.
      PA_CHECK = 'X'.
    ENDIF.

  ELSE.
*   Material number does not exist
*   LEAVE TO PROGRAM
    IT_AEBM-ZBDAT = SY-DATUM.
    IT_AEBM-ZBNAM = SY-UNAME.
    IT_AEBM-ZMODE = 'C'.
    IT_AEBM-ZRESULT = 'L'.
    IT_AEBM-ZMSG = 'Material number does not exist'.
    MODIFY       IT_AEBM TRANSPORTING ZBDAT
                                      ZBNAM
                                      ZMODE
                                      ZRESULT
                                      ZMSG
                           WHERE MTNO GE SPACE
                           AND   PLNT GE SPACE
                           AND   USAG GE SPACE
                           AND   ALTN GE SPACE
                           AND   PREF GE SPACE
                           AND   COMP GE SPACE
                           AND   SUFF GE SPACE
                           AND   SEQU GE SPACE.
    PA_CHECK = 'X'.
  ENDIF.
* REFRESH
  REFRESH IT_MARC. CLEAR IT_MARC.
ENDFORM.                    " READ_MARC_CHECK
*&---------------------------------------------------------------------*
*&      Form  MARC_CHECK_MATERIAL
*&---------------------------------------------------------------------*
FORM MARC_CHECK_MATERIAL TABLES    PT_MARC STRUCTURE IT_MARC
                          CHANGING PA_CHECK.
  DATA: L_TABIX TYPE SY-TABIX,
        L_ZMSG LIKE CFGNL-MSGLIN.
* SORTING
  SORT PT_MARC BY MATNR WERKS.

  LOOP AT IT_AEBM.
    L_TABIX = SY-TABIX.
    READ TABLE PT_MARC WITH KEY MATNR = IT_AEBM-MTNO
                                WERKS = IT_AEBM-PLNT
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      READ TABLE PT_MARC WITH KEY MATNR = IT_AEBM-COMP
                                  WERKS = IT_AEBM-PLNT
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
*     Except 'FERT', 'ALTERNATIVE BOM' can not become more than '01'.
        IF PT_MARC-MTART NE 'FERT'.
          IF IT_AEBM-ALTN NE '01'.
*           MESSAGE TEXT
            CONCATENATE 'Execpt  ' PT_MARC-MTART
                        '  ALTERNATIVE BOM can not become more than 01'
                     INTO L_ZMSG.
            PERFORM ERROR_MATERIAL_MODIFY USING L_ZMSG
                                                L_TABIX.
            PA_CHECK = 'X'.
          ENDIF.
        ENDIF.
      ELSE.
        L_ZMSG = 'BOM COMPONENT Material number does not exist'.
        PERFORM ERROR_MATERIAL_MODIFY USING L_ZMSG
                                            L_TABIX.
        PA_CHECK = 'X'.
      ENDIF.
    ELSE.
      READ TABLE PT_MARC WITH KEY MATNR = IT_AEBM-COMP
                                  WERKS = IT_AEBM-PLNT
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        L_ZMSG = 'NEXT Material number does not exist'.
        PERFORM ERROR_MATERIAL_MODIFY USING L_ZMSG
                                            L_TABIX.
        PA_CHECK = 'X'.
      ELSE.
        L_ZMSG = 'NEXT Material & BOM COMPONENT does not exist'.
        PERFORM ERROR_MATERIAL_MODIFY USING L_ZMSG
                                            L_TABIX.
        PA_CHECK = 'X'.
      ENDIF.
    ENDIF.
    CLEAR: IT_AEBM, PT_MARC.
  ENDLOOP.

ENDFORM.                    " MARC_CHECK_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_STKO_STPO_CHECK
*&---------------------------------------------------------------------*
FORM READ_MAST_STKO_STPO_CHECK CHANGING PA_CHECK.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR TYPE MAST-MATNR,  "MATNRIAL NO
          WERKS TYPE MAST-WERKS,  "PLANT
          STLAN TYPE MAST-STLAN,  "BOM USAGE
          STLAL TYPE MAST-STLAL,  "ALTERNATIVE BOM
          BMENG(17),  " TYPE STKO-BMENG,
          STLST(02),  " TYPE STKO-STLST,
*          POSNR TYPE STPO-POSNR,  "BOM item number(PREFIX)
*          IDNRK TYPE STPO-IDNRK,  "BOM component
*          POTX1 TYPE STPO-POTX1,  "BOM item text1(SUFFIX)
        END OF LT_MAST.

  LOOP AT IT_AEBM.
    LT_MAST-MATNR = IT_AEBM-MTNO.  "MATNRIAL NO
    LT_MAST-WERKS = IT_AEBM-PLNT.  "PLANT
    LT_MAST-STLAN = IT_AEBM-USAG.  "BOM USAGE
    LT_MAST-STLAL = IT_AEBM-ALTN.  "ALTERNATIVE BOM
    WRITE: IT_AEBM-BQTY TO LT_MAST-BMENG LEFT-JUSTIFIED,
           IT_AEBM-STAT TO LT_MAST-STLST.
*    LT_MAST-POSNR = IT_AEBM-PREF.  "BOM item number(PREFIX)
*    LT_MAST-IDNRK = IT_AEBM-COMP.  "BOM component
*    LT_MAST-POTX1 = IT_AEBM-SUFF.  "BOM item text1(SUFFIX)
    COLLECT LT_MAST.
    CLEAR: IT_AEBM, LT_MAST.
  ENDLOOP.

  IF NOT LT_MAST[] IS INITIAL.
    SELECT A~MATNR
           A~WERKS
           A~STLAN
           A~STLAL
           B~POSNR
           B~IDNRK
           B~POTX1
           A~STLNR
           B~STLKN
         FROM MAST AS A INNER JOIN STPO AS B
                        ON    B~STLTY EQ 'M'
                        AND   A~STLNR EQ B~STLNR
         INTO TABLE IT_MAST
         FOR ALL ENTRIES IN LT_MAST
         WHERE A~MATNR EQ LT_MAST-MATNR
         AND   A~WERKS EQ LT_MAST-WERKS
         AND   A~STLAN EQ LT_MAST-STLAN
         AND   A~STLAL EQ LT_MAST-STLAL.
*         AND   B~POSNR EQ LT_MAST-POSNR
*         AND   B~IDNRK EQ LT_MAST-IDNRK
*         AND   B~POTX1 EQ LT_MAST-POTX1.
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_IT_AEBM_FROM_IT_MAST CHANGING PA_CHECK.
      IF PA_CHECK NE 'X'.
        LOOP AT LT_MAST.
          REFRESH: IT_BDC, IT_MESS.
          PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                          LT_MAST-WERKS  "Plant
                                          LT_MAST-STLAN  "BOM usage
                                          LT_MAST-STLAL  "Alternat BOM
                                          LT_MAST-BMENG  "Confirmed qua
                                          LT_MAST-STLST  "BOM Status
                                    CHANGING PA_CHECK.
          CLEAR: LT_MAST.
        ENDLOOP.
        IF PA_CHECK EQ 'X'.
          PERFORM HEDAER_BOM_PROCESS_ERROR_WRITE.
        ENDIF.
      ELSE.
        PERFORM DATA_PROCESS_ERROR_WRITE.
      ENDIF.
    ELSE.
      LOOP AT LT_MAST.
        PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                        LT_MAST-WERKS  "Plant
                                        LT_MAST-STLAN  "BOM usage
                                        LT_MAST-STLAL  "Alternat BOM
                                        LT_MAST-BMENG  "Confirmed qua
                                        LT_MAST-STLST  "BOM Status
                                  CHANGING PA_CHECK.
        CLEAR: LT_MAST.
      ENDLOOP.
    ENDIF.
  ELSE.
*   BOM does not exist
    PA_CHECK = 'X'.
  ENDIF.
ENDFORM.                    " READ_MAST_STKO_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_AEBM_FROM_IT_MAST
*&---------------------------------------------------------------------*
FORM CHECK_IT_AEBM_FROM_IT_MAST CHANGING PA_CHECK.
  DATA: L_TABIX TYPE SY-TABIX,
        L_ZMSG LIKE CFGNL-MSGLIN.
* SORTING
  SORT IT_MAST BY MATNR WERKS STLAN STLAL POSNR IDNRK POTX1 STLNR STLKN.

  LOOP AT IT_AEBM WHERE UPCT EQ '1'.
    L_TABIX = SY-TABIX.
    READ TABLE IT_MAST WITH KEY MATNR = IT_AEBM-MTNO
                                WERKS = IT_AEBM-PLNT
                                STLAN = IT_AEBM-USAG
                                STLAL = IT_AEBM-ALTN
                                POSNR = IT_AEBM-PREF
                                IDNRK = IT_AEBM-COMP
                                POTX1 = IT_AEBM-SUFF
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      L_ZMSG = 'BOM does exist'.
      PERFORM ERROR_MATERIAL_MODIFY USING L_ZMSG
                                          L_TABIX.
      PA_CHECK = 'X'.
*    ELSE.
*      IF IT_AEBM-UPCT EQ '2'.
**       UPDATE CONTROL TYPE 1 : CREATE    2: DELETE
*        IT_AEBM-ZMODE = 'C'.
**       L : LEGACY DATA ERROR
*        IT_AEBM-ZRESULT = 'L'.
**       MESSAGE TEXT
*        IT_AEBM-ZMSG = 'BOM does not exist'.
**       ERROR DATE
*        IT_AEBM-ZBDAT = SY-DATUM.
**       USER
*        IT_AEBM-ZBNAM = SY-UNAME.
**       BOM does not exist
**       LEAVE TO PROGRAM
*        PA_CHECK = 'X'.
*      ENDIF.
    ENDIF.
    CLEAR: IT_AEBM, IT_MAST.
  ENDLOOP.
ENDFORM.                    " CHECK_IT_AEBM_FROM_IT_MAST
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: BEGIN OF LT_AEBM OCCURS 0,
          UPCT TYPE ZTBM_ABXEBMDT-UPCT,
          MANDT TYPE ZTBM_ABXEBMDT-MANDT,
          MTNO TYPE ZTBM_ABXEBMDT-MTNO,
          PLNT TYPE ZTBM_ABXEBMDT-PLNT,
          USAG TYPE ZTBM_ABXEBMDT-USAG,
          ALTN TYPE ZTBM_ABXEBMDT-ALTN,
          PREF TYPE ZTBM_ABXEBMDT-PREF,
          COMP TYPE ZTBM_ABXEBMDT-COMP,
          SUFF TYPE ZTBM_ABXEBMDT-SUFF,
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
        END OF LT_AEBM.
  DATA: MT_AEBM LIKE LT_AEBM,
        NT_AEBM LIKE IT_AEBM OCCURS 0 WITH HEADER LINE,
        L_MODE,
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_QNTY(17),
        L_ZRESULT TYPE ZTBM_ABXEBMDT-ZRESULT,
        L_ZMSG  LIKE CFGNL-MSGLIN,
        L_STLKN TYPE STPO-STLKN.
  LOOP AT IT_AEBM.
    MOVE-CORRESPONDING IT_AEBM TO LT_AEBM.
    APPEND LT_AEBM.
    CLEAR: IT_AEBM, LT_AEBM.
  ENDLOOP.

  SORT LT_AEBM BY MTNO PLNT USAG ALTN PREF COMP SUFF SEQU UPCT.

  LOOP AT LT_AEBM.
    L_TABIX = SY-TABIX.
    MT_AEBM = LT_AEBM.
    WRITE: MT_AEBM-QNTY TO L_QNTY.
    SHIFT L_QNTY LEFT DELETING LEADING SPACE.
    IF MT_AEBM-UPCT EQ '2'.
      PERFORM READ_MAST_STPO_STLKN USING MT_AEBM-MTNO  "Material number
                                         MT_AEBM-PLNT  "Plant
                                         MT_AEBM-USAG  "BOM usage
                                         MT_AEBM-ALTN  "Alternative BOM
                                         MT_AEBM-PREF  "BOM item number
                                         MT_AEBM-COMP  "BOM component
                                         MT_AEBM-SUFF  "SUFFIX
                                         MT_AEBM-SEQU  "SEQUENCE
                                CHANGING L_MODE
                                         L_STLKN.

      CASE L_MODE.
        WHEN '3'.
          IF NOT L_STLKN IS INITIAL.
            PERFORM CHANGE_HEADER_DE USING MT_AEBM-MTNO "Materialnumber
                                           MT_AEBM-PLNT "Plant
                                           MT_AEBM-USAG "BOM usage
                                           MT_AEBM-ALTN "AlternativeBOM
                                           MT_AEBM-EONO. "Change number
            PERFORM CHANGE_BADY_ITEM_DELETE USING MT_AEBM-PREF
                                                  MT_AEBM-COMP
                                                  L_STLKN.
            PERFORM CHANGE_END_ITEM_DELETE.
          ENDIF.
          CALL TRANSACTION 'CS02'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.

          L_ZRESULT = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
          CASE L_ZRESULT.
            WHEN 'E'.
              LT_AEBM-ZBDAT = SY-DATUM.
              LT_AEBM-ZBNAM = SY-UNAME.
              LT_AEBM-ZMODE = 'D'.
            WHEN OTHERS.
              LT_AEBM-ZBDAT = SY-DATUM.
              LT_AEBM-ZBTIM = SY-UZEIT.
              LT_AEBM-ZBNAM = SY-UNAME.
              LT_AEBM-ZMODE = 'D'.
          ENDCASE.
          MODIFY LT_AEBM INDEX L_TABIX TRANSPORTING ZBDAT
                                                    ZBTIM
                                                    ZBNAM
                                                    ZMODE
                                                    ZRESULT
                                                    ZMSG.
          CLEAR LT_AEBM.
        WHEN '4'.
*          MODIFY MT_AEBM
          LT_AEBM-ZBDAT = SY-DATUM.
          LT_AEBM-ZBNAM = SY-UNAME.
          LT_AEBM-ZMODE = 'D'.
          LT_AEBM-ZRESULT = 'E'.
          LT_AEBM-ZMSG    = 'BOM ITEM to delete does not exist.'.
          MODIFY LT_AEBM INDEX L_TABIX TRANSPORTING ZBDAT
                                                    ZBNAM
                                                    ZMODE
                                                    ZRESULT
                                                    ZMSG.
          CLEAR LT_AEBM.
      ENDCASE.

      REFRESH: IT_BDC, IT_MESS.
      CLEAR: IT_BDC, IT_MESS.
    ELSEIF MT_AEBM-UPCT EQ '1'.
*     AT NEW
      AT NEW ALTN.
        L_TANEW = L_TABIX.
*       CLEAR
        CLEAR L_MODE.
        L_MODE = '1'.
        IF L_MODE EQ '1'.  "CHANGE(ITEM CREATE)
          PERFORM CHANGE_HEADER_BOM USING MT_AEBM-MTNO  "Material number
                                          MT_AEBM-PLNT  "Plant
                                          MT_AEBM-USAG  "BOM usage
                                          MT_AEBM-ALTN  "Alternative BOM
                                          MT_AEBM-EONO. "Change number

        ENDIF.
      ENDAT.

*     'UPDATE CONTROL MODE' different BDC
      IF L_MODE EQ '1'.
        PERFORM CHANGE_BADY_ITEM_CREATE1
                            USING MT_AEBM-PREF  "BOM item number
                                  MT_AEBM-COMP  "BOM compenent
                                  L_QNTY        "Compenent quantity
                                  MT_AEBM-ITCA  "Item category
                                  MT_AEBM-SPPR  "procurement type item
                                  MT_AEBM-EITM  "END ITEM TYPE
                                  MT_AEBM-SUFF  "SUFFIX NO
                                  MT_AEBM-STGB  "STRUCTURE TYPE
                                  MT_AEBM-SEQU  "SEQUENCE NO
                                  MT_AEBM-UPGN  "UPG
*                                    MT_AEBM-ZINFO  "INFO
                                  MT_AEBM-CLPT   "COLOR PART
                                  MT_AEBM-DPID.  "DEPENDENCY ID
      ENDIF.


*     AT END OF
      AT END OF ALTN.

        L_TAEND = L_TABIX.

        IF L_MODE EQ '1'.
          PERFORM CHANGE_END_ITEM_CREATE.
          CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
          MT_AEBM-ZRESULT = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.

          IF MT_AEBM-UPCT EQ '1'.
            MT_AEBM-ZMODE = 'C'.
          ELSEIF MT_AEBM-UPCT EQ '2'.
            MT_AEBM-ZMODE = 'D'.
          ENDIF.
          PERFORM MODIFY_AEBM_MESSAGE USING MT_AEBM-ZMODE
                                            MT_AEBM-ZRESULT
                                            L_TANEW
                                            L_TAEND
                                            L_ZMSG.
        ENDIF.
        REFRESH: IT_BDC, IT_MESS.
      ENDAT.
    ENDIF.
    CLEAR: LT_AEBM, MT_AEBM.
  ENDLOOP.

  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_AEBM. CLEAR IT_AEBM.
* TOTAL LINE COUNT
  DESCRIBE TABLE LT_AEBM LINES WA_LINE_IDX.
  LOOP AT LT_AEBM.
    IF LT_AEBM-ZRESULT EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      MOVE-CORRESPONDING LT_AEBM TO NT_AEBM.
      APPEND NT_AEBM.
    ELSE.
      MOVE-CORRESPONDING LT_AEBM TO IT_AEBM.
      APPEND IT_AEBM.
    ENDIF.
    CLEAR: IT_AEBM, LT_AEBM, NT_AEBM.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM BDC TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM BDC ERROR TOTAL LINES : ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE '1'.
    PERFORM UPDATE_ZTBM_ABXEBMDT TABLES NT_AEBM.
    PERFORM WRITE_AEBM TABLES NT_AEBM
                       USING 'E'.
  ENDIF.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER_DE
*&---------------------------------------------------------------------*
FORM CHANGE_HEADER_DE USING P_MTNO
                            P_PLNT
                            P_USAG
                            P_ALTN
                            P_EONO.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_PLNT,    "PLANT
     ' ' 'RC29N-STLAN' P_USAG,    "BOM usage
     ' ' 'RC29N-STLAL' P_ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' P_EONO,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP'.

ENDFORM.                    " CHANGE_HEADER_DE
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
*&      Form  BOM_HEADER_CREATE
*&---------------------------------------------------------------------*
FORM BOM_HEADER_CREATE USING P_MTNO
                             P_PLNT
                             P_USAG
                             P_ALTN
                             P_BQTY
                             P_STAT
                       CHANGING PA_CHECK.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG   LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_PLNT,    "PLANT
     ' ' 'RC29N-STLAN' P_USAG,    "BOM usage
     ' ' 'RC29N-STLAL' P_ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' '19590101-001',    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0110',
     ' ' 'RC29K-BMENG' P_BQTY,    "Confirmed quantity
     ' ' 'RC29K-STLST' P_STAT,    "BOM STATUS
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0111',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
  CALL TRANSACTION 'CS01'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
  L_MSGTY = SY-MSGTY.
  PERFORM RKC_MSG_STRING CHANGING L_MSG.
  IF L_MSGTY EQ 'E'.
    LOOP AT IT_AEBM WHERE MTNO EQ P_MTNO
                    AND   PLNT EQ P_PLNT
                    AND   USAG EQ P_USAG
                    AND   ALTN EQ P_ALTN.

      L_TABIX = SY-TABIX.

      IT_AEBM-ZBDAT = SY-DATUM.
      IT_AEBM-ZBNAM = SY-UNAME.
      IT_AEBM-ZMODE = 'C'.
      IT_AEBM-ZRESULT = 'E'.
      IT_AEBM-ZMSG = L_MSG.
      MODIFY IT_AEBM INDEX L_TABIX TRANSPORTING ZBDAT
                                                ZBNAM
                                                ZMODE
                                                ZRESULT
                                                ZMSG .
    ENDLOOP.

    PA_CHECK = 'X'.
  ENDIF.
ENDFORM.                    " BOM_HEADER_CREATE
*&---------------------------------------------------------------------*
*&      Form  HEADER_BOM_PROCESS
*&---------------------------------------------------------------------*
FORM HEADER_BOM_PROCESS CHANGING PA_CHECK.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR TYPE MAST-MATNR,
          WERKS TYPE MAST-WERKS,
          STLAN TYPE MAST-STLAN,
          STLAL TYPE MAST-STLAL,
          BMENG(17),  " TYPE STKO-BMENG,
          STLST(02),  " TYPE STKO-STLST,
        END OF LT_MAST.
  DATA: MT_MAST LIKE LT_MAST OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_AEBM.
    LT_MAST-MATNR = IT_AEBM-MTNO.
    LT_MAST-WERKS = IT_AEBM-PLNT.
    LT_MAST-STLAN = IT_AEBM-USAG.
    LT_MAST-STLAL = IT_AEBM-ALTN.
    WRITE: IT_AEBM-BQTY TO LT_MAST-BMENG LEFT-JUSTIFIED,
           IT_AEBM-STAT TO LT_MAST-STLST.
    COLLECT LT_MAST.
    CLEAR: IT_AEBM, LT_MAST.
  ENDLOOP.

  IF NOT LT_MAST[] IS INITIAL.
    SELECT MATNR
           WERKS
           STLAN
           STLAL
         FROM MAST
         INTO TABLE MT_MAST
         FOR ALL ENTRIES IN LT_MAST
         WHERE MATNR EQ LT_MAST-MATNR
         AND   WERKS EQ LT_MAST-WERKS
         AND   STLAN EQ LT_MAST-STLAN
         AND   STLAL EQ LT_MAST-STLAL.
    IF SY-SUBRC EQ 0.
      SORT MT_MAST BY MATNR WERKS STLAN STLAL.
      LOOP AT LT_MAST.
        READ TABLE MT_MAST WITH KEY MATNR = LT_MAST-MATNR
                                    WERKS = LT_MAST-WERKS
                                    STLAN = LT_MAST-STLAN
                                    STLAL = LT_MAST-STLAL
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                          LT_MAST-WERKS  "Plant
                                          LT_MAST-STLAN  "BOM usage
                                          LT_MAST-STLAL  "Alternat BOM
                                          LT_MAST-BMENG  "Confirmed qua
                                          LT_MAST-STLST  "BOM Status
                                    CHANGING PA_CHECK.
        ENDIF.
        CLEAR: LT_MAST, MT_MAST.
      ENDLOOP.
    ELSE.
      LOOP AT LT_MAST.
        PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                        LT_MAST-WERKS  "Plant
                                        LT_MAST-STLAN  "BOM usage
                                        LT_MAST-STLAL  "Alternat BOM
                                        LT_MAST-BMENG  "Confirmed qua
                                        LT_MAST-STLST  "BOM Status
                                    CHANGING PA_CHECK.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " HEADER_BOM_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
FORM CHANGE_HEADER_BOM USING P_MTNO
                             P_PLNT
                             P_USAG
                             P_ALTN
                             P_EONO.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_PLNT,    "PLANT
     ' ' 'RC29N-STLAN' P_USAG,    "BOM usage
     ' ' 'RC29N-STLAL' P_ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' P_EONO,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM CHANGE_BADY_ITEM_CREATE1 USING P_PREF     "BOM item number
                                    P_COMP     "BOM compenent
                                    P_QNTY     "Compenent quantity
                                    P_ITCA     "Item category
                                    P_SPPR     "procurement type item
                                    P_EITM     "END ITEM TYPE
                                    P_SUFF     "SUFFIX NO
                                    P_STGB     "STRUCTURE TYPE
                                    P_SEQU     "SEQUENCE NO
                                    P_UPGN     "UPG
*                                    P_INFO     "INFO
                                    P_CLPT     "COLOR PART
                                    P_DPID.    "DEPENDENCY ID

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' P_PREF,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' P_COMP,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' P_QNTY,    "Compenent quantity
     ' ' 'RC29P-POSTP(02)' P_ITCA,    "Item category
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' P_SPPR,    "Special procurement type for BOM item
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
*     ' ' 'RC29P-POTX1' P_SUFF,    "BOM item text(like 1)
*     ' ' 'RC29P-POTX2' P_EITM,    "BOM item text(like 2)
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       P_EITM,    "END ITEM TYPE
     ' ' 'ZSTGB'       P_STGB,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       P_SUFF,    "SUFFIX NO
     ' ' 'ZSEQU'       P_SEQU,    "SEQUENCE NO
     ' ' 'ZUPGN'       P_UPGN,    "UPG
*     ' ' 'ZINFO'       P_SUFF,    "INFO
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
FORM CHANGE_END_ITEM_CREATE.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BADY_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM CHANGE_BADY_ITEM_DELETE USING P_PREF
                                   P_COMP
                                   P_STLKN.
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCSDI'    '0150',
*     ' ' 'BDC_OKCODE'  '=SETP',
*
*     'X' 'SAPLCSDI'    '0708',
*     ' ' 'RC29P-SELPO' P_PREF,                "
*     ' ' 'RC29P-SELID' P_COMP,                "
*     ' ' 'RC29P-SELPI' P_STLKN,                "
*     ' ' 'BDC_OKCODE'  '=CLWI',
*
*     'X' 'SAPLCSDI'    '0150',
*     ' ' 'RC29P-AUSKZ(01)' 'X',                "
*     ' ' 'BDC_OKCODE'  '=FCDL'.
  PERFORM DYNPRO USING:
*     'X' 'SAPLCSDI'    '0150',
*     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
*     ' ' 'RC29P-SELPO' P_POSNR,                "
*     ' ' 'RC29P-SELID' P_IDNRK,                "
     ' ' 'RC29P-SELPI' P_STLKN,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL'.

ENDFORM.                    " CHANGE_BADY_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_AEBM_MESSAGE
*&---------------------------------------------------------------------*
FORM MODIFY_AEBM_MESSAGE USING    P_ZMODE
                                  P_ZRESULT
                                  P_TANEW
                                  P_TAEND
                                  P_MSG.
  DATA: L_TABIX TYPE SY-TABIX.
  LOOP AT IT_AEBM FROM P_TANEW TO P_TAEND.
    L_TABIX = SY-TABIX.
    IT_AEBM-ZMODE   = P_ZMODE.
    IT_AEBM-ZRESULT = P_ZRESULT.
    IT_AEBM-ZMSG    = P_MSG.
    IF IT_AEBM-ZRESULT EQ 'E'.
      IT_AEBM-ZBDAT =  SY-DATUM.
      IT_AEBM-ZBNAM =  SY-UNAME.
    ELSE.
      IT_AEBM-ZBDAT =  SY-DATUM.
      IT_AEBM-ZBTIM =  SY-UZEIT.
      IT_AEBM-ZBNAM =  SY-UNAME.

    ENDIF.

    MODIFY IT_AEBM INDEX L_TABIX TRANSPORTING ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE
                                              ZRESULT
                                              ZMSG.
    CLEAR IT_AEBM.
  ENDLOOP.
ENDFORM.                    " MODIFY_AEBM_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM CHANGE_END_ITEM_DELETE.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " CHANGE_END_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM READ_MAST_STPO_STLKN USING    P_MATNR
                                   P_WERKS
                                   P_STLAN
                                   P_STLAL
                                   P_POSNR
                                   P_IDNRK
                                   P_ZSUFF
                                   P_ZSEQU
                          CHANGING P_MODE
                                   P_STLKN.
  DATA: L_ZSEQU(4) TYPE N,
        L_STLNR TYPE MAST-STLNR,
        L_CNT TYPE I.
  L_ZSEQU = P_ZSEQU.
  L_ZSEQU = L_ZSEQU - 1.

  SELECT SINGLE A~STLNR
                B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
*                      INNER JOIN STAS AS C
*                      ON    C~STLTY EQ 'M'
*                      AND   A~STLNR EQ C~STLNR
*                      AND   A~STLAL EQ C~STLAL
       INTO  (L_STLNR, P_STLKN)
       WHERE A~MATNR EQ P_MATNR
       AND   A~WERKS EQ P_WERKS
       AND   A~STLAN EQ P_STLAN
       AND   A~STLAL EQ P_STLAL
       AND   B~POSNR EQ P_POSNR
       AND   B~IDNRK EQ P_IDNRK
       AND   B~SUFF  EQ P_ZSUFF
       AND   B~SEQU  EQ L_ZSEQU.
  IF SY-SUBRC EQ 0.
    SELECT COUNT( DISTINCT STASZ )
         FROM STAS
         INTO L_CNT
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR
         AND   STLAL EQ P_STLAL
         AND   STLKN EQ P_STLKN.
    IF L_CNT GT 1.
      P_MODE = '4'.
    ELSE.
      P_MODE = '3'.
    ENDIF.
  ELSE.
    P_MODE = '4'.
  ENDIF.
ENDFORM.                    " READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_ERROR_WRITE
*&---------------------------------------------------------------------*
FORM DATA_PROCESS_ERROR_WRITE.
* TOTAL LINE
  DESCRIBE TABLE IT_AEBM LINES WA_LINE_IDX.

  LOOP AT IT_AEBM WHERE ZRESULT EQ 'L'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM SELECTION TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM SELECT LEGACY ERROR LINES : ', WA_ERRO_IDX.
  WRITE: / 'Material number does not exist & BOM does exist'.
  FORMAT COLOR OFF.
  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES IT_AEBM.
  PERFORM WRITE_AEBM TABLES IT_AEBM
                     USING 'L'.
ENDFORM.                    " DATA_PROCESS_ERROR_WRITE
*&---------------------------------------------------------------------*
*&      Form  HEDAER_BOM_PROCESS_ERROR_WRITE
*&---------------------------------------------------------------------*
FORM HEDAER_BOM_PROCESS_ERROR_WRITE.
* TOTAL LINE
  DESCRIBE TABLE IT_AEBM LINES WA_LINE_IDX.

  LOOP AT IT_AEBM WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM SELECTION TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM SELECT LEGACY ERROR LINES : ', 0.
  WRITE: / 'BOM HEADER BDC ERROR LINES : ', WA_ERRO_IDX.
  WRITE: / 'BOM HEADER BDC ERROR'.
  FORMAT COLOR OFF.
  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES IT_AEBM.
  PERFORM WRITE_AEBM TABLES IT_AEBM
                     USING 'E'.
ENDFORM.                    " HEDAER_BOM_PROCESS_ERROR_WRITE
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
* TOTAL LINE
  DESCRIBE TABLE IT_AEBM LINES WA_LINE_IDX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM SELECTION TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM SELECT LEGACY ERROR LINES : ', 0.
  WRITE: / 'BOM ITEM BDC LINES : ', WA_LINE_IDX.
  FORMAT COLOR OFF.

  LOOP AT IT_AEBM WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM ITEM BDC ERROR LINES : ', WA_ERRO_IDX.
  WRITE: / 'BOM ITEM BDC ERROR'.
  FORMAT COLOR OFF.
  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES IT_AEBM.
  PERFORM WRITE_AEBM TABLES IT_AEBM
                     USING 'E'.
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
*&---------------------------------------------------------------------*
*&      Form  ERROR_MATERIAL_MODIFY
*&---------------------------------------------------------------------*
FORM ERROR_MATERIAL_MODIFY USING    P_ZMSG
                                    P_TABIX.
*     UPDATE CONTROL TYPE 1 : CREATE    2: DELETE
  CASE IT_AEBM-UPCT.
    WHEN '1'.
      IT_AEBM-ZMODE = 'C'.
    WHEN '2'.
      IT_AEBM-ZMODE = 'D'.
  ENDCASE.
*     L : LEGACY DATA ERROR
  IT_AEBM-ZRESULT = 'L'.
*     MESSAGE TEXT
  IT_AEBM-ZMSG = P_ZMSG.
*     ERROR DATE
  IT_AEBM-ZBDAT = SY-DATUM.
*     USER
  IT_AEBM-ZBNAM = SY-UNAME.

  MODIFY IT_AEBM INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.
ENDFORM.                    " ERROR_MATERIAL_MODIFY
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM BOM_EXPLODED TABLES   P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
  DATA L_TABIX TYPE SY-TABIX.
* MATERIAL & COMPENENT COLLECT
  LOOP AT IT_AEBM WHERE CLPT EQ 'C'.
    LT_MARC-MATNR = IT_AEBM-MTNO.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC.
    LT_MARC-MATNR = IT_AEBM-COMP.
    LT_MARC-WERKS = IT_AEBM-PLNT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC, IT_AEBM.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
*   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
*                            LT_MARC-KZKFG(Configurable Material)
    PERFORM READ_MARA TABLES LT_MARC.

    LOOP AT LT_MARC WHERE MTART EQ 'FERT'.
      L_TABIX = SY-TABIX.
      P_BOM_EXPLODED-MATNR = LT_MARC-MATNR.
      P_BOM_EXPLODED-WERKS = LT_MARC-WERKS.
      APPEND P_BOM_EXPLODED.
      DELETE LT_MARC INDEX L_TABIX.
      CLEAR: P_BOM_EXPLODED, LT_MARC.
    ENDLOOP.

    LOOP AT LT_MARC.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = LT_MARC-MATNR
                                         WERKS = LT_MARC-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       BOM EXPLODED.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                        USING LT_MARC-MATNR
                              LT_MARC-WERKS
                              SY-DATUM   "IDNRK WERKS DATUV
                        CHANGING WA_LAST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXEBMDT TABLES   PT_AEBM STRUCTURE IT_AEBM.
  UPDATE ZTBM_ABXEBMDT FROM TABLE PT_AEBM.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    WRITE: / 'UPDATE SUCCESS' COLOR 4 .
  ELSE.
    ROLLBACK WORK.
    WRITE: / 'UPDATE ERROR' COLOR 4 .
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
FORM READ_MARA TABLES PT_MARC.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
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
      CLEAR TEMP_ISTPOV.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_BOM
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
*&      Form  MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
FORM MM02_CONFIGURABLE_MATERIAL_BDC.

  DATA: L_MSG   LIKE CFGNL-MSGLIN,
        L_TABIX TYPE SY-TABIX.
  DATA : BEGIN OF LT_BOM_EXPLODED OCCURS 0,
          MATNR      LIKE   STPOV-MATNR,   "Material
          WERKS      LIKE   STPOV-WERKS,   "Plant
          KZKFG      TYPE   MARA-KZKFG,    "Configurable Material
          ZRESULT    LIKE   IT_AEBM-ZRESULT,
          ZMSG       LIKE   IT_AEBM-ZMSG,
         END   OF LT_BOM_EXPLODED.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR  : IT_BDC, IT_MESS.
  CLEAR  : WA_LINE_IDX, WA_ERRO_IDX.
  WRITE: / 'CONFIGURABLE MATERIAL' COLOR 4.
  LOOP AT IT_BOM_EXPLODED WHERE KZKFG NE 'X'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_BOM_EXPLODED-MATNR,   "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'  '=BILD',

       'X' 'SAPLMGMM'    '4004',
       ' ' 'MARA-KZKFG'  'X',   "
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
             OPTIONS FROM WA_OPT
             MESSAGES INTO IT_MESS.
    MOVE-CORRESPONDING IT_BOM_EXPLODED TO LT_BOM_EXPLODED.

    LT_BOM_EXPLODED-ZRESULT = SY-MSGTY.
    IF LT_BOM_EXPLODED-ZRESULT EQ 'E'.
      WA_ERRO_IDX =  WA_ERRO_IDX + 1.
    ENDIF.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
    LT_BOM_EXPLODED-ZMSG = L_MSG.
    APPEND LT_BOM_EXPLODED.
*    MODIFY IT_BOM_EXPLODED INDEX L_TABIX TRANSPORTING ZRESULT
*                                                      ZMSG.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR  : IT_BDC, IT_MESS.
  ENDLOOP.

* TOTAL LINE
  DESCRIBE TABLE LT_BOM_EXPLODED LINES WA_LINE_IDX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'CONFIGURABLE MATERIAL CHECK TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'CONFIGURABLE MATERIAL CHECK BDC ERROR LINES: ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  PERFORM CONFIGURABLE_MATERIAL_WRITE TABLES LT_BOM_EXPLODED.

ENDFORM.                    " MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_SELECT_STLKN_CS02BDC
*&---------------------------------------------------------------------*
FORM MAST_STPO_SELECT_STLKN_CS02BDC.
  DATA: L_STLKN LIKE STPO-STLKN,
        L_TABIX TYPE SY-TABIX,
        L_ZMSG  LIKE CFGNL-MSGLIN.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR : IT_BDC, IT_MESS.
  CLEAR  : WA_LINE_IDX, WA_ERRO_IDX.
  LOOP AT IT_AEBM WHERE CLPT EQ 'C'.
    L_TABIX = SY-TABIX.
    SELECT SINGLE B~STLKN
         FROM MAST AS A INNER JOIN STPO AS B
                        ON A~STLNR EQ B~STLNR
         INTO L_STLKN
         WHERE A~MATNR EQ IT_AEBM-MTNO
         AND   A~WERKS EQ IT_AEBM-PLNT
         AND   A~STLAN EQ IT_AEBM-USAG
         AND   A~STLAL EQ IT_AEBM-ALTN
         AND   B~POSNR EQ IT_AEBM-PREF
         AND   B~IDNRK EQ IT_AEBM-COMP
         AND   B~SUFF  EQ IT_AEBM-SUFF
         AND   B~SEQU  EQ IT_AEBM-SEQU.
    IF SY-SUBRC EQ 0.
      WA_LINE_IDX = WA_LINE_IDX + 1.
      PERFORM COLOR_PART_DEPENDENCY USING IT_AEBM-MTNO "Material number
                                          IT_AEBM-PLNT "Plant
                                          IT_AEBM-USAG "BOM usage
                                          IT_AEBM-ALTN "Alternative BOM
                                          IT_AEBM-EONO  "Change number
                                          IT_AEBM-DPID   "
                                          L_STLKN.
      CALL TRANSACTION 'CS02'  USING IT_BDC
                               OPTIONS FROM WA_OPT
                               MESSAGES INTO IT_MESS.
      IT_AEBM-ZRESULT = SY-MSGTY.
      PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
      IF IT_AEBM-ZRESULT EQ 'E'.
        CONCATENATE IT_AEBM-ZMSG L_ZMSG 'COLOR PART ERROR'
                    INTO IT_AEBM-ZMSG.
        IT_AEBM-ZBDAT = SY-DATUM.
        IT_AEBM-ZBNAM = SY-UNAME.
        CLEAR IT_AEBM-ZBTIM.
      ELSE.
        CONCATENATE IT_AEBM-ZMSG L_ZMSG INTO IT_AEBM-ZMSG.
        IT_AEBM-ZBDAT = SY-DATUM.
        IT_AEBM-ZBNAM = SY-UNAME.
        IT_AEBM-ZBTIM = SY-UZEIT.
      ENDIF.
      MODIFY IT_AEBM INDEX L_TABIX TRANSPORTING ZBDAT
                                                ZBTIM
                                                ZBNAM
                                                ZRESULT
                                                ZMSG.

      REFRESH: IT_BDC, IT_MESS.
      CLEAR: IT_BDC, IT_MESS.
    ENDIF.
    CLEAR IT_AEBM.
  ENDLOOP.

  LOOP AT IT_AEBM WHERE CLPT EQ 'C'
                  AND   ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM COLOR PART TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM COLOR PART ERROR LINES : ', WA_ERRO_IDX.
  WRITE: / 'BOM COLOR PART BDC ERROR'.
  FORMAT COLOR OFF.
  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES IT_AEBM.
  PERFORM WRITE_AEBM TABLES IT_AEBM
                     USING 'E'.

ENDFORM.                    " MAST_STPO_SELECT_STLKN_CS02BDC
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_DEPENDENCY
*&---------------------------------------------------------------------*
FORM COLOR_PART_DEPENDENCY USING P_MTNO
                                 P_PLNT
                                 P_USAG
                                 P_ALTN
                                 P_EONO
                                 P_DPID
                                 P_STLKN.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_PLNT,    "PLANT
     ' ' 'RC29N-STLAN' P_USAG,    "BOM usage
     ' ' 'RC29N-STLAL' P_ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' P_EONO,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,    "NODE number
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "CHECK
     ' ' 'BDC_OKCODE'  '=WIZU',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'RCUKD-KNNAM(01)' P_DPID, "DEPENDENCY
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'BDC_OKCODE'  '=BACK',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " COLOR_PART_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CONFIGURABLE_MATERIAL_WRITE
*&---------------------------------------------------------------------*
FORM CONFIGURABLE_MATERIAL_WRITE TABLES   PT_BOM_EXPLODED.
  DATA : BEGIN OF LA_BOM_EXPLODED,
          MATNR      LIKE   STPOV-MATNR,   "Material
          WERKS      LIKE   STPOV-WERKS,   "Plant
          KZKFG      TYPE   MARA-KZKFG,    "Configurable Material
          ZRESULT    LIKE   IT_AEBM-ZRESULT,
          ZMSG       LIKE   IT_AEBM-ZMSG,
         END   OF LA_BOM_EXPLODED.
  WRITE: /(18) 'Material',
          (06) 'PLANT',
          (30) 'Configurable Material',
          (10) 'TYPE',
          (220) 'MESSAGE'.
  LOOP AT PT_BOM_EXPLODED INTO LA_BOM_EXPLODED.
    WRITE: /(18) LA_BOM_EXPLODED-MATNR,
            (06) LA_BOM_EXPLODED-WERKS,
            (30) LA_BOM_EXPLODED-KZKFG,
            (10) LA_BOM_EXPLODED-ZRESULT,
            (220) LA_BOM_EXPLODED-ZMSG.
  ENDLOOP.
ENDFORM.                    " CONFIGURABLE_MATERIAL_WRITE
