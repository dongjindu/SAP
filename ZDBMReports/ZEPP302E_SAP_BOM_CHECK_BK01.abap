************************************************************************
* Program Name      : ZEPP302E_SAP_BOM_CHECK
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.25.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K901973
* Addl Documentation:
* Description       : Read available BOM in present visual point.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.03.18               UD1K908313
*
*
************************************************************************
REPORT ZEPP302E_SAP_BOM_CHECK
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: MAST,
        STPO,
        STKO,
        STAS,
        MARA,
        ZTBM_ABXBMODT,
        ZSBM_ABXBMODT_RFC.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: IT_BMOT TYPE ZTBM_ABXBMODT OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_MAST OCCURS 0,
        MATNR TYPE MAST-MATNR,
        WERKS TYPE MAST-WERKS,
        STLAN TYPE MAST-STLAN,
        STLST TYPE STKO-STLST,
        STLAL TYPE MAST-STLAL,
      END OF IT_MAST.
*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
DATA : C_DEST(10) VALUE 'WMBOM01'.   "Outbound Interface Destination
DATA: WA_BRFC TYPE ZSBM_ABXBMODT_RFC.
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_LINES TYPE SY-TABIX,
      WA_DIV TYPE I VALUE '500'.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR MAST-MATNR,
                S_WERKS FOR MAST-WERKS.
PARAMETERS: P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END   OF BLOCK B2.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
*INITIALIZATION.
*  PERFORM INITIALIZATION.
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.
*
*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
*  PERFORM WRITE_PROCESS.
*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_MAST.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MAST
*&---------------------------------------------------------------------*
FORM READ_MAST.
  SELECT A~MATNR
         A~WERKS
         A~STLAN
         B~STLST
     MAX( A~STLAL )
       FROM MAST AS A INNER JOIN STKO AS B
                      ON  A~STLNR EQ B~STLNR
                      AND A~STLAL EQ B~STLAL
                      AND B~STLTY EQ 'M'
       INTO TABLE IT_MAST
       WHERE A~MATNR IN S_MATNR
       AND   A~WERKS IN S_WERKS
       AND   A~STLAN EQ '1'
       GROUP BY A~MATNR
                A~WERKS
                A~STLAN
                B~STLST.
  IF SY-SUBRC NE 0.
    WRITE: / 'NO DATA'.
  ENDIF.

ENDFORM.                    " READ_MAST
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM CHECK_MATERIAL_TYPE_FERT.
  PERFORM APPEND_IT_BMOT.
  PERFORM DELETE_ADJACENT_DUPLICATES.
  PERFORM INTERFACE_TIME_STAMP.
  PERFORM DELETE_AND_INSERT_ABXEBMOT.
  PERFORM OUTBOUND_RFC_FUNCTION.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2.
  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD,
        L_TABIX   LIKE  SY-TABIX.
  DATA: BEGIN OF LT_CUKB OCCURS 0,
         KNNAM LIKE CUKB-KNNAM,
  END   OF LT_CUKB.

  CLEAR: L_CUOBJ.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = 'PP01'  "
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUM
            EMENG  =  1      "
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = IT_MAST-MATNR
            STPST  = 0
            STLAN  = IT_MAST-STLAN
            STLAL  = IT_MAST-STLAL
            SVWVO  = 'X'
            WERKS  = IT_MAST-WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = LT_MATCAT
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
*  READ TABLE LT_STB WITH KEY MTART = 'ROH1'.
*  IF SY-SUBRC NE 0.
  LOOP AT LT_STB WHERE MTART NE 'ROH1'.
    IF LT_STB-XISDT EQ 'X'.
      PERFORM CS_BOM_EXPL_MAT_V2_V2 USING   LT_STB-IDNRK
                                            LT_STB-WERKS
                                            LT_STB-XTLAN
                                            LT_STB-XTLAL.
    ENDIF.
    IT_BMOT-MTNO = IT_MAST-MATNR.
    IT_BMOT-PLNT = IT_MAST-WERKS.
    IT_BMOT-USAG = IT_MAST-STLAN.
    IT_BMOT-ALTN = IT_MAST-STLAL.
    IT_BMOT-PREF = LT_STB-POSNR.
    IT_BMOT-COMP = LT_STB-IDNRK.
    IT_BMOT-SUFF = LT_STB-SUFF.
*    IT_BMOT-SEQC = '0000'.  "LT_STB-SEQU.
    IT_BMOT-EONO = LT_STB-AENNR.
    IT_BMOT-BQTY = 1.       "LT_STB-MNGLG .
    IT_BMOT-HUNT = LT_STB-MMEIN.
    IT_BMOT-STAT = IT_MAST-STLST.
    IT_BMOT-ITCA = LT_STB-POSTP.
    IT_BMOT-QNTY = LT_STB-MENGE.
    IT_BMOT-STGB = LT_STB-STGB.
    IT_BMOT-UNIT = LT_STB-MEINS.
    IT_BMOT-SPPR = LT_STB-ITSOB.
    IT_BMOT-EITM = LT_STB-EITM.
*      IT_BMOT-CLPT = LT_STB-CLPT.
*      IT_BMOT-UPCT = LT_STB-UPCT.
    IT_BMOT-UPGN = LT_STB-UPGN.
    IT_BMOT-DATUV = LT_STB-DATUV.
    IT_BMOT-DATUB = LT_STB-DATUB.
    IF NOT LT_STB-KNOBJ IS INITIAL.
      PERFORM SEARCH_DEPENDENCIES TABLES   LT_CUKB
                                  USING    LT_STB-KNOBJ.
      LOOP AT LT_CUKB.
        L_TABIX = SY-TABIX.
        IF L_TABIX EQ '1'.
          IT_BMOT-DPID = LT_CUKB-KNNAM.
          IT_BMOT-SEQC = 1.  "LT_STB-SEQU.
        ELSE.
          IT_BMOT-DPID = LT_CUKB-KNNAM.  "
          IT_BMOT-SEQC = IT_BMOT-SEQC + 1.  "LT_STB-SEQU.

        ENDIF.
        APPEND IT_BMOT.

      ENDLOOP.

    ELSE.
      IT_BMOT-DPID = ' '.  "
      IT_BMOT-SEQC = '0000'.  "LT_STB-SEQU.
      APPEND IT_BMOT.
    ENDIF.
    CLEAR IT_BMOT.
*    CLEAR: IT_BMOT, LT_STB.
*    IF NOT LT_STB-KNOBJ IS INITIAL.
*      PERFORM SEARCH_DEPENDENCIES TABLES   LT_CUKB
*                                  USING    LT_STB-KNOBJ.
*    ENDIF.
*    APPEND IT_BMOT.
*    CLEAR: IT_BMOT, LT_STB.
  ENDLOOP.
*  ENDIF.
ENDFORM.                    " CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  SEARCH_DEPENDENCIES
*&---------------------------------------------------------------------*
FORM SEARCH_DEPENDENCIES TABLES   PT_CUKB
                         USING    P_KNOBJ.
  DATA: L_KNNUM    LIKE CUOB-KNNUM,
        L_KNNAM    LIKE CUKB-KNNAM.
  DATA: BEGIN OF LT_CUOB OCCURS 0,
         KNNUM LIKE CUOB-KNNUM,
  END   OF LT_CUOB.
  DATA: BEGIN OF LT_CUKB OCCURS 0,
         KNNAM LIKE CUKB-KNNAM,
  END   OF LT_CUKB.
  SELECT  KNNUM
              FROM CUOB
              INTO TABLE LT_CUOB
              WHERE KNTAB EQ 'STPO'
              AND   KNOBJ EQ P_KNOBJ.
  SELECT KNNAM
              FROM CUKB
              INTO TABLE LT_CUKB
              FOR ALL ENTRIES IN LT_CUOB
              WHERE KNNUM EQ LT_CUOB-KNNUM.
  IF SY-SUBRC EQ 0.
    PT_CUKB[] = LT_CUKB[].
  ENDIF.
ENDFORM.                    " SEARCH_DEPENDENCIES
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_BMOT
*&---------------------------------------------------------------------*
FORM APPEND_IT_BMOT.
  LOOP AT IT_MAST.
    PERFORM CS_BOM_EXPL_MAT_V2.
    CLEAR: IT_BMOT, IT_MAST.
  ENDLOOP.
ENDFORM.                    " APPEND_IT_BMOT
*&---------------------------------------------------------------------*
*&      Form  DELETE_AND_INSERT_ABXEBMOT
*&---------------------------------------------------------------------*
FORM DELETE_AND_INSERT_ABXEBMOT.
  DATA: LT_BMOT TYPE ZTBM_ABXBMODT OCCURS 0 WITH HEADER LINE.
  SELECT *
       FROM ZTBM_ABXBMODT
       INTO TABLE LT_BMOT.
  IF SY-SUBRC EQ 0.
    DELETE ZTBM_ABXBMODT FROM TABLE LT_BMOT.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
      INSERT ZTBM_ABXBMODT FROM TABLE IT_BMOT.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    INSERT ZTBM_ABXBMODT FROM TABLE IT_BMOT.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DELETE_AND_INSERT_ABXEBMOT
*&---------------------------------------------------------------------*
*&      Form  INTERFACE_TIME_STAMP
*&---------------------------------------------------------------------*
FORM INTERFACE_TIME_STAMP.
  IT_BMOT-ZUSER = SY-UNAME.
  IT_BMOT-ZSDAT = P_DATUM.
  IT_BMOT-ZSTIM = SY-UZEIT.
*  IT_BMOT-ZEDAT = SY-DATUM.
*  IT_BMOT-ZETIM = SY-UZEIT.
  IT_BMOT-ZMODE = 'C'.
  IT_BMOT-ZRESULT = 'S'.
  MODIFY IT_BMOT TRANSPORTING ZUSER
                              ZSDAT
                              ZSTIM
*                              ZEDAT
*                              ZETIM
                              ZMODE
                              ZRESULT
                        WHERE MTNO GE SPACE
                        AND   PLNT GE SPACE
                        AND   USAG GE SPACE
                        AND   ALTN GE SPACE
                        AND   PREF GE SPACE
                        AND   COMP GE SPACE
                        AND   SUFF GE SPACE
                        AND   SEQC GE SPACE.

ENDFORM.                    " INTERFACE_TIME_STAMP
*&---------------------------------------------------------------------*
*&      Form  OUTBOUND_RFC_FUNCTION
*&---------------------------------------------------------------------*
FORM OUTBOUND_RFC_FUNCTION.
  DATA: LT_BMOT TYPE ZTBM_ABXBMODT OCCURS 0 WITH HEADER LINE.
  DATA: LT_BRFC TYPE ZSBM_ABXBMODT_RFC OCCURS 0 WITH HEADER LINE.
  DATA: MT_BRFC TYPE ZSBM_ABXBMODT_RFC OCCURS 0 WITH HEADER LINE.
  DATA: L_MSGTXT(200),
        L_TABIX TYPE SY-TABIX,
        L_TIMES TYPE I,
        L_MOD   TYPE I,
        L_TOTAL TYPE NUM9,
        L_INDEX TYPE SY-INDEX,
        L_FROM TYPE I VALUE '0',
        L_TO TYPE I VALUE '0',
        L_CURR TYPE NUM9.
  SELECT *
       FROM ZTBM_ABXBMODT
       INTO TABLE LT_BMOT.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_BMOT.
      MOVE-CORRESPONDING LT_BMOT TO LT_BRFC.
      APPEND LT_BRFC.
      CLEAR: LT_BMOT, LT_BRFC.
    ENDLOOP.
  ENDIF.
***************
  IF NOT LT_BRFC[] IS INITIAL.
    DESCRIBE TABLE LT_BRFC LINES WA_LINES.
    L_TIMES = WA_LINES DIV WA_DIV.
    L_MOD   = WA_LINES MOD WA_DIV.
    IF NOT L_MOD IS INITIAL.
      L_TIMES = L_TIMES + 1.
    ENDIF.
    L_TOTAL = L_TIMES.
    DO L_TIMES TIMES.
      L_INDEX = SY-INDEX.

      L_CURR = L_INDEX.

      L_FROM = L_TO + 1.
      L_TO   = WA_DIV * L_INDEX.

      IF L_TIMES EQ L_INDEX.
        L_TO = WA_LINES.
      ENDIF.
      REFRESH MT_BRFC. CLEAR MT_BRFC.
      LOOP AT LT_BRFC FROM L_FROM TO L_TO.
        MT_BRFC = LT_BRFC.
        APPEND MT_BRFC.
        CLEAR: LT_BRFC, MT_BRFC.
      ENDLOOP.
      CALL FUNCTION 'Z_FBM_ABXBMODT'
        DESTINATION  C_DEST
        IMPORTING
          TOTAL_C  = L_TOTAL
          CURR_C   = L_CURR
        TABLES
          T_ABXBMODT       = MT_BRFC
     EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

      IF SY-SUBRC <> 0.
        MESSAGE S000 WITH L_MSGTXT.
        WRITE: / L_MSGTXT.
        EXIT.
      ELSE.
        LOOP AT MT_BRFC.
          L_TABIX = SY-TABIX.
          L_TABIX = L_FROM - 1 + L_TABIX.
          LT_BRFC = MT_BRFC.
          MODIFY LT_BRFC INDEX L_TABIX TRANSPORTING ZEDAT
                                                    ZETIM
                                                    ZMSG
                                                    ZZRET.
          CLEAR: LT_BRFC, MT_BRFC.
        ENDLOOP.
        READ TABLE MT_BRFC WITH KEY ZZRET = 'E'.
        IF SY-SUBRC EQ 0.
          MESSAGE S000 WITH MT_BRFC-ZMSG.
          WRITE: / L_MSGTXT.
          EXIT.
        ELSE.
          MESSAGE S000 WITH '  Successful in transmission to EAI'.
        ENDIF.
      ENDIF.

    ENDDO.


    LOOP AT LT_BRFC.
      IF LT_BRFC-ZZRET EQ 'E'.
        WA_ERRO_IDX = WA_ERRO_IDX + 1.
      ELSE.
        WA_LINE_IDX = WA_LINE_IDX + 1.
      ENDIF.
    ENDLOOP.

    IF WA_ERRO_IDX GE 1.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / 'BOM OUTBOUND TOTAL LINES : ', WA_LINE_IDX.
      WRITE: / 'BOM OUTBOUND ERROR TOTAL LINES : ', WA_ERRO_IDX.
      FORMAT COLOR OFF.
*      SORT LT_BRFC BY ZZRET MTNO PLNT USAG ALTN PREF SEQU EONO.
      WRITE: /(20) 'MATERIAL(NEXT)',
              (10) 'PLANT',
              (10) 'BOM USAGE',
              (10) 'AlternaBOM',
              (10) 'PREFIX',
              (20) 'COMPONENT',
              (10) 'SUFFIX',
              (10) 'SEQUENCE',
              (15) 'COL.SEQUENCE',
              (15) 'Change number',
              (20) 'NEXT-Material QTY',
              (10) 'UNIT',
              (10) 'STATUS',
              (10) 'Category',
              (20) 'Component QTY',
              (15) 'STRUCTURE TYPE',
              (10) 'COM.UNIT',
              (15) 'procurement',
              (10) 'END ITEM',
              (10) 'COLORPART',
              (30) 'OBJECT DEPENDENCY',
              (15) 'CONTROL TYPE',
              (20) 'UPG',
*          (10) 'MODE',
              (30) 'Interface Return Value',
             (220) 'MESSAGE'.
      LOOP AT LT_BRFC WHERE ZZRET EQ 'E'.
        PERFORM WRITE_ERROR_LOG USING    WA_BRFC.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " OUTBOUND_RFC_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR_LOG
*&---------------------------------------------------------------------*
FORM WRITE_ERROR_LOG USING    PA_BRFC STRUCTURE WA_BRFC.
  WRITE: /(20) PA_BRFC-MTNO,
          (10) PA_BRFC-PLNT,
          (10) PA_BRFC-USAG,
          (10) PA_BRFC-ALTN,
          (10) PA_BRFC-PREF,
          (20) PA_BRFC-COMP,
          (10) PA_BRFC-SUFF,
*          (10) PA_BRFC-SEQU,
          (15) PA_BRFC-SEQC,
          (15) PA_BRFC-EONO,
          (20) PA_BRFC-BQTY UNIT PA_BRFC-HUNT,
          (10) PA_BRFC-HUNT,
          (10) PA_BRFC-STAT,
          (10) PA_BRFC-ITCA,
          (20) PA_BRFC-QNTY UNIT PA_BRFC-UNIT,
          (15) PA_BRFC-STGB,
          (10) PA_BRFC-UNIT,
          (15) PA_BRFC-SPPR,
          (10) PA_BRFC-EITM,
          (10) PA_BRFC-CLPT,
          (30) PA_BRFC-DPID,
          (15) PA_BRFC-UPCT,
          (20) PA_BRFC-UPGN,
*            (10) PA_BRFC-ZMODE,
          (30) PA_BRFC-ZZRET,
          (220) PA_BRFC-ZMSG.
ENDFORM.                    " WRITE_ERROR_LOG
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_TYPE_FERT
*&---------------------------------------------------------------------*
FORM CHECK_MATERIAL_TYPE_FERT.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MTART TYPE MARA-MTART.
  LOOP AT IT_MAST.
    L_TABIX = SY-TABIX.
    SELECT SINGLE MTART
                FROM MARA
                INTO L_MTART
                WHERE MATNR EQ IT_MAST-MATNR
                AND   MTART EQ 'FERT'.
    IF SY-SUBRC EQ 0.
      CLEAR L_MTART.
    ELSE.
      DELETE IT_MAST INDEX L_TABIX.
    ENDIF.
    CLEAR IT_MAST.
  ENDLOOP.
ENDFORM.                    " CHECK_MATERIAL_TYPE_FERT
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2_V2 USING    P_MATNR
                                    P_WERKS
                                    P_STLAN
                                    P_STLAL.
  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD,
        L_TABIX   LIKE  SY-TABIX.

  DATA: BEGIN OF LT_CUKB OCCURS 0,
         KNNAM LIKE CUKB-KNNAM,
  END   OF LT_CUKB.
  CLEAR: L_CUOBJ.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = 'PP01'  "
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUM
            EMENG  =  1      "
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
            MATCAT = LT_MATCAT
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
  LOOP AT LT_STB WHERE MTART NE 'ROH1'.
    IF LT_STB-XISDT EQ 'X'.
      PERFORM CS_BOM_EXPL_MAT_V2_V2 USING   LT_STB-IDNRK
                                            LT_STB-WERKS
                                            LT_STB-XTLAN
                                            LT_STB-XTLAL.
    ENDIF.
    IT_BMOT-MTNO = P_MATNR.
    IT_BMOT-PLNT = P_WERKS.
    IT_BMOT-USAG = P_STLAN.
    IT_BMOT-ALTN = P_STLAL.
    IT_BMOT-PREF = LT_STB-POSNR.
    IT_BMOT-COMP = LT_STB-IDNRK.
    IT_BMOT-SUFF = LT_STB-SUFF.
*    IT_BMOT-SEQU = LT_STB-SEQU.
    IT_BMOT-EONO = LT_STB-AENNR.
    IT_BMOT-BQTY = 1.       "LT_STB-MNGLG .
    IT_BMOT-HUNT = LT_STB-MMEIN.
    IT_BMOT-STAT = IT_MAST-STLST.
    IT_BMOT-ITCA = LT_STB-POSTP.
    IT_BMOT-QNTY = LT_STB-MENGE.
    IT_BMOT-STGB = LT_STB-STGB.
    IT_BMOT-UNIT = LT_STB-MEINS.
    IT_BMOT-SPPR = LT_STB-ITSOB.
    IT_BMOT-EITM = LT_STB-EITM.
*      IT_BMOT-CLPT = LT_STB-CLPT.
*      IT_BMOT-UPCT = LT_STB-UPCT.
    IT_BMOT-UPGN = LT_STB-UPGN.
    IT_BMOT-DATUV = LT_STB-DATUV.
    IT_BMOT-DATUB = LT_STB-DATUB.
    IF NOT LT_STB-KNOBJ IS INITIAL.
      PERFORM SEARCH_DEPENDENCIES TABLES   LT_CUKB
                                  USING    LT_STB-KNOBJ.
      LOOP AT LT_CUKB.
        L_TABIX = SY-TABIX.
        IF L_TABIX EQ '1'.
          IT_BMOT-DPID = LT_CUKB-KNNAM.
          IT_BMOT-SEQC = 1.  "LT_STB-SEQU.
        ELSE.
          IT_BMOT-DPID = LT_CUKB-KNNAM.  "
          IT_BMOT-SEQC = IT_BMOT-SEQC + 1.  "LT_STB-SEQU.

        ENDIF.
        APPEND IT_BMOT.
      ENDLOOP.
    ELSE.
      IT_BMOT-DPID = ' '.  "
      IT_BMOT-SEQC = '0000'.  "LT_STB-SEQU.
      APPEND IT_BMOT.
    ENDIF.
    CLEAR: IT_BMOT, LT_STB.
  ENDLOOP.

ENDFORM.                    " CS_BOM_EXPL_MAT_V2_V2
*&---------------------------------------------------------------------*
*&      Form  DELETE_ADJACENT_DUPLICATES
*&---------------------------------------------------------------------*
FORM DELETE_ADJACENT_DUPLICATES.
*  SORT IT_BMOT BY MTNO PLNT USAG ALTN PREF COMP SUFF SEQC SEQU .
  DELETE ADJACENT DUPLICATES FROM IT_BMOT COMPARING MTNO
                                                    PLNT
                                                    USAG
                                                    ALTN
                                                    PREF
                                                    COMP
                                                    SUFF
                                                    SEQC.
*                                                    SEQU.


ENDFORM.                    " DELETE_ADJACENT_DUPLICATES
