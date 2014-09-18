************************************************************************
* PROGRAM NAME      : ZBMR_901_BOM_NO_CHILD
* AUTHOR            : YONGPING
* CREATION DATE     :
* SPECIFICATIONS BY :
* PATTERN           :
* DEVELOPMENT REQUEST NO :
* Addl Documentation:
* DESCRIPTION       : CHECK THE FSC METERIALS THAT HAVE NO ENGINE OR
*                     TRANSMISSION
*
* MODIFICATION LOGS
* DATE       DEVELOPER    REQUEST/NO    DESCRIPTION
************************************************************************


REPORT ZBMR902_BOM_NO_ENG_TM  MESSAGE-ID ZMPP NO STANDARD PAGE HEADING.


***********************************************************************
*GLOBAL DATA
***********************************************************************
TABLES: MARA, MAST, STPO.

DATA: C_ENGINE(3) TYPE C VALUE 'ENG'." ENGINE INDICATOR
DATA: C_TM(2) TYPE C VALUE 'TM'.     " TRANSMISSIION INDICATOR
DATA: C_FERT(4) TYPE C VALUE 'FERT'. " FSC MATERIAL TYPE
DATA: P_CAPID LIKE RC29L-CAPID VALUE 'PP01'."APPLICATION ID:TABLE TC04
DATA: P_DATUV LIKE SY-DATUM.
DATA: P_EMENG LIKE RC29L-EMENG VALUE '1'.
DATA: S_NOT_EXIST(1)."'N': NOT A FERT MATERIAL,'X': NO MATERIAL EXIST
DATA  S_CHK(1).  " INITIAL: FOUND ENGINE/TM ERROR 'X':EXPLOSION ERROR
                 " 'N': NOT FOUND ENGINE/TM ERROR
***********************************************************************
*INTERNAL TABLES
***********************************************************************
DATA : BEGIN OF IT_MATERIAL OCCURS 0,
          MATNR LIKE MARA-MATNR,  "MATERIAL NUMBER
          STLNR LIKE MAST-STLNR,  "MATERIAL NUMBER
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
          WERKS LIKE MAST-WERKS,  "MATERIAL PLANT
          STLAN LIKE MAST-STLAN,  "BOM USAGE
          STLAL LIKE MAST-STLAL,  "ALTERNATIVE BOM NO
          ENGINE(1),              " ENGINE: 'Y' OR 'N'
          TM(1),                  "TRANSMISSION: 'Y' OR'N'

       END OF IT_MATERIAL.
DATA: BEGIN OF IT_RESULT OCCURS 0,
          MATNR LIKE MARA-MATNR,  "MATERIAL NUMBER
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
          WERKS LIKE MAST-WERKS,  "MATERIAL PLANT
          STLAN LIKE MAST-STLAN,  "BOM USAGE
          STLAL LIKE MAST-STLAL,  "ALTERNATIVE BOM NO
          ENGINE(1),              " ENGINE: 'Y' OR 'N'
          TM(1),                  "TRANSMISSION: 'Y' OR'N'
          ENGNO LIKE MARA-MATNR,  "ENGINE NUMBER
          TMNO LIKE MARA-MATNR,   "TM NUMBER
          COMMENT(50),            "OTHER ERROR RECORDING
       END OF IT_RESULT.


DATA: BEGIN OF SELPOOL OCCURS 0.
        INCLUDE STRUCTURE CSTMAT.
DATA: END OF SELPOOL.


*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case1         RADIOBUTTON  GROUP rg   DEFAULT 'X'    .
SELECTION-SCREEN COMMENT  (40) text-002 FOR FIELD p_case1        .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-005 FOR FIELD p_matnr       .
select-options: p_matnr  for  mara-matnr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case2         RADIOBUTTON  GROUP rg    .
SELECTION-SCREEN COMMENT  (40) text-003 FOR FIELD p_case2       .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-005 FOR FIELD p_date1     .
select-options: p_date1  for sy-datum default sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN END   OF BLOCK b1.

AT SELECTION-SCREEN .


*****************************************************************
*END OF SELECTION-SCREEN
*****************************************************************



START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM INITAL_RESULT.
  IF S_NOT_EXIST IS INITIAL.
    PERFORM CHECK_ENGINE_TM CHANGING S_CHK.
  ENDIF.

END-OF-SELECTION.

  PERFORM DISPLAY_RESULT.



*****************************************************************
*******  FORMS **************************************************
*****************************************************************


*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       READING SOURCE DATA FROM BOM HEADER TABLE
*----------------------------------------------------------------------*
*  -->  p1        INPUT USER INPUT FROM SELECTION SCREEN
*  <--  p2        OUTPUT THE FSC MATERIAL LIST TABLE IT_MATERIAL
*----------------------------------------------------------------------*
FORM READ_DATA.
   DATA: LT_MATERIAL LIKE IT_MATERIAL OCCURS 0 WITH HEADER LINE.
   DATA: L_TABIX LIKE SY-TABIX.
   DATA:  L_LINES TYPE I.


 CASE 'X'.
   WHEN P_CASE1.
*     INPUT MATERIAL NUMBER RANGE, READ DATA FROM MAST
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MATERIAL
        FROM MAST WHERE MATNR IN P_MATNR .
      IF SY-SUBRC <> 0.
        S_NOT_EXIST = 'X'.
      ELSE.
*       READ THE MATETIAL TYPE
        SELECT MATNR MTART
         INTO CORRESPONDING FIELDS OF TABLE LT_MATERIAL
         FROM MARA
         FOR ALL ENTRIES IN IT_MATERIAL
          WHERE MATNR = IT_MATERIAL-MATNR.
         IF SY-SUBRC = 0.
*           STORE THE TYPE INTO IT_MATERIAL
            LOOP AT IT_MATERIAL.
              L_TABIX = SY-TABIX.
              READ TABLE LT_MATERIAL WITH KEY MATNR = IT_MATERIAL-MATNR.
              IF LT_MATERIAL-MTART = C_FERT.
                  IT_MATERIAL-MTART = LT_MATERIAL-MTART.
                  MODIFY IT_MATERIAL.
              ELSE.
                  DELETE IT_MATERIAL INDEX L_TABIX.
              ENDIF.
            ENDLOOP.
*           CHECK IF FERT EXIST IN THE MATERIAL INTERNAL TABLE
            DESCRIBE TABLE IT_MATERIAL LINES L_LINES.
            IF L_LINES = 0.
              S_NOT_EXIST = 'N'. "THESE MATERIAL ARE NOT FERT MATERIAL
            ENDIF.
         ENDIF.

      ENDIF.


   WHEN P_CASE2.
*     INPUT FSC BOM CREATION/CHANGE DATE, READ DATA FROM MAST
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MATERIAL
        FROM STKO
          WHERE AEDAT IN P_DATE1 OR
                ANDAT IN P_DATE1 .

      IF SY-SUBRC <> 0.
        S_NOT_EXIST = 'X'.
      ELSE.
*       READ THE BOM LINK TABLE TO FIND THE MATERIAL
        LT_MATERIAL[] = IT_MATERIAL[].
        REFRESH IT_MATERIAL[].
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MATERIAL
         FROM MAST
          FOR ALL ENTRIES IN LT_MATERIAL
           WHERE STLNR = LT_MATERIAL-STLNR.
*       READ THE MATETIAL TYPE
        SELECT MATNR MTART
         INTO CORRESPONDING FIELDS OF TABLE LT_MATERIAL
         FROM MARA
         FOR ALL ENTRIES IN IT_MATERIAL
          WHERE MATNR = IT_MATERIAL-MATNR.
         IF SY-SUBRC = 0.
*           STORE THE TYPE INTO IT_MATERIAL
            LOOP AT IT_MATERIAL.
              L_TABIX = SY-TABIX.
              READ TABLE LT_MATERIAL WITH KEY MATNR = IT_MATERIAL-MATNR.
              IF LT_MATERIAL-MTART = C_FERT.
                  IT_MATERIAL-MTART = LT_MATERIAL-MTART.
                  MODIFY IT_MATERIAL.
              ELSE.
                  DELETE IT_MATERIAL INDEX L_TABIX.
              ENDIF.
            ENDLOOP.
*           CHECK IF FERT EXIST IN THE MATERIAL INTERNAL TABLE
            DESCRIBE TABLE IT_MATERIAL LINES L_LINES.
            IF L_LINES = 0.
              S_NOT_EXIST = 'N'. "THESE MATERIAL ARE NOT FERT MATERIAL
            ENDIF.
         ENDIF.

      ENDIF.

 ENDCASE.
     IF SY-SUBRC = 0.
     ENDIF.


ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENGINE_TM
*&---------------------------------------------------------------------*
*       CHECK THE FSC MATERIAL ENGINE AND TRANSMISSION
*----------------------------------------------------------------------*
*  -->  p1        INPUT MATERIAL LIST INTERNAL TABLE IT_MATERIAL
*  <--  p2        OUTPUT THE CHECKING RESULT LIST IT_RESULT
*----------------------------------------------------------------------*
FORM CHECK_ENGINE_TM  CHANGING P_STATUS TYPE C.
  DATA: LT_MATERIAL LIKE IT_MATERIAL OCCURS 0 WITH HEADER LINE.
  DATA: LT_COM_EXPLODED LIKE STPOX OCCURS 0 WITH HEADER LINE.
  DATA: L_STATUS(1).
  DATA: L_TABIX LIKE SY-TABIX.



*  LOOP THE MATERIAL INTERNAL TABLE TO CHECK ONE BY ONE.
   LOOP AT IT_MATERIAL.
     L_TABIX = SY-TABIX.
     PERFORM MAT_EXPLODE_SINGLE TABLES LT_COM_EXPLODED
                                USING  IT_MATERIAL-MATNR
                                       IT_MATERIAL-STLAN
                                       IT_MATERIAL-STLAL
                                       IT_MATERIAL-WERKS
                                CHANGING L_STATUS.
      IF L_STATUS IS INITIAL.
*       CHECK IF ENGINE AND TRANSMISSION EXIST.

        PERFORM CHECK_ENGINE_TM_INFO TABLES LT_COM_EXPLODED .

      ELSE.
*       HAVING BOM EXPLOSION ERROR, STORE INFO
        MOVE-CORRESPONDING IT_MATERIAL TO IT_RESULT.
        IT_RESULT-COMMENT = 'BOM EXPLOSION ERROR'.
        MODIFY IT_RESULT.
*        DELETE LT_MATERIAL INDEX L_TABIX.
      ENDIF.

   ENDLOOP.


ENDFORM.                    " CHECK_ENGINE_TM
*&---------------------------------------------------------------------*
*&      Form  MAT_EXPLODE_SINGLE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->PT_COM_EXPLODED  text
*      -->P_MATNR  " MATERIAL NUMBER
*      -->P_STLAN  "BOM USAGE
*      -->P_STLAL  "ALTERNATIVE BOM
*      -->P_WERKS  "PLANT
*      <--P_P_SATUS " STATUS
*----------------------------------------------------------------------*
FORM MAT_EXPLODE_SINGLE TABLES   PT_COM_EXPLODED STRUCTURE STPOX
                        USING    P_MATNR LIKE MARA-MATNR
                                 P_STLAN LIKE MAST-STLAN
                                 P_STLAL LIKE MAST-STLAL
                                 P_WERKS LIKE MAST-WERKS
                        CHANGING P_STATUS TYPE C.

 DATA: LT_STB LIKE STPOX OCCURS 0 WITH HEADER LINE.
 DATA: LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE.
 DATA: LT_TOPMAT LIKE  CSTMAT.
 DATA: L_CUOBJ   LIKE  MARC-CUOBJ.

*   CALL FUNCTION TO EXPLODE THE BOM IN SINGLE LEVEL

     P_DATUV = SY-DATUM.
     CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
            AUMNG  = 0
            CAPID  = P_CAPID
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUV
            EMENG  = P_EMENG
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = P_MATNR
            STPST  = 0
            STLAN  = P_STLAN
            STLAL  = P_STLAL
            SVWVO  = 'X'
            WERKS  = P_WERKS
            VRSVO  = 'X'
*     IMPORTING
*          TOPMAT         = LT_TOPMAT
*          DSTST          = LT_DSTST
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
      P_STATUS = 'X'.  "BOM EXPLOSION ERROR
  ELSE.
     PT_COM_EXPLODED[] = LT_STB[].
  ENDIF.

 ENDFORM.                    " MAT_EXPLODE_SINGLE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENGINE_TM_INFO
*&---------------------------------------------------------------------*
*       SEARCH THE ENGINE AND TRANSMISSION PARTS IN THE
*       EXPLODED COMPONENT LIST
*----------------------------------------------------------------------*
*      -->P_STATUS  "STATUS
*----------------------------------------------------------------------*
FORM CHECK_ENGINE_TM_INFO TABLES PT_COM_EXPLODED.

    DATA: LT_COM_EXPLODED LIKE STPOX OCCURS 0 WITH HEADER LINE.
    DATA: BEGIN OF LT_COM_ZINFO OCCURS 0,
            STLTY LIKE STPO-STLTY,  "CATEGORY
            STLNR LIKE STPO-STLNR,  "BILL OF MATERIAL
            STLKN LIKE STPO-STLKN,  "BOM ITEM NODE NUMBER
            STPOZ LIKE STPO-STPOZ,  "INTERNAL COUNTER
            IDNRK LIKE STPO-IDNRK,  "COMPONENT NUMBER
            ZINFO LIKE STPO-ZINFO,  "ENGINE/TM INFO FIELD
          END OF LT_COM_ZINFO.

    LT_COM_EXPLODED[] = PT_COM_EXPLODED[].

*   READ ZINFO FROM BOM COMPONENT TABLE
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_COM_ZINFO
      FROM STPO
       FOR ALL ENTRIES IN LT_COM_EXPLODED
        WHERE IDNRK = LT_COM_EXPLODED-IDNRK AND
              STLTY = LT_COM_EXPLODED-STLTY AND
              STLNR = LT_COM_EXPLODED-STLNR AND
              STPOZ = LT_COM_EXPLODED-STPOZ AND
              STLKN = LT_COM_EXPLODED-STLKN.

*   LOOP THE COMPONENT TO CHECK IF ENGINE EXIST

    LOOP AT LT_COM_EXPLODED.
      READ TABLE LT_COM_ZINFO WITH KEY IDNRK = LT_COM_EXPLODED-IDNRK
                                       STLTY = LT_COM_EXPLODED-STLTY
                                       STLNR = LT_COM_EXPLODED-STLNR
                                       STPOZ = LT_COM_EXPLODED-STPOZ
                                       STLKN = LT_COM_EXPLODED-STLKN.
*      CHECK IF ENGINE AND TM EXIST
       IF LT_COM_ZINFO-ZINFO <> 'ENG'.
          IF LT_COM_ZINFO-ZINFO <> 'TM'.
             CONTINUE.
          ELSE.
*            FOUND TRANSMISSION PART AND STORE IT
             MOVE-CORRESPONDING IT_MATERIAL TO IT_RESULT.
             IT_RESULT-TM = 'Y'.
             IT_RESULT-TMNO = LT_COM_ZINFO-IDNRK.
             MODIFY IT_RESULT FROM IT_RESULT
                       TRANSPORTING TM TMNO
                       WHERE MATNR = IT_MATERIAL-MATNR AND
                             WERKS = IT_MATERIAL-WERKS AND
                             STLAN = IT_MATERIAL-STLAN AND
                             STLAL = IT_MATERIAL-STLAL.
          ENDIF.
       ELSE.
*          FOUND THE ENGINE PART AND STORE IT
           MOVE-CORRESPONDING IT_MATERIAL TO IT_RESULT.
           IT_RESULT-ENGINE = 'Y'.
           IT_RESULT-ENGNO = LT_COM_ZINFO-IDNRK.
           MODIFY IT_RESULT FROM IT_RESULT
                       TRANSPORTING ENGINE ENGNO
                       WHERE MATNR = IT_MATERIAL-MATNR AND
                             WERKS = IT_MATERIAL-WERKS AND
                             STLAN = IT_MATERIAL-STLAN AND
                             STLAL = IT_MATERIAL-STLAL.


       ENDIF.

    ENDLOOP.

ENDFORM.                    " CHECK_ENGINE_TM_INFO
*&---------------------------------------------------------------------*
*&      Form  INITAL_RESULT
*&---------------------------------------------------------------------*
*       STORE ALL THE MATERIAL THAT NEED TO CHECK IN IT_RESULT
*       AS HEADER DATA
*----------------------------------------------------------------------*
*  -->  p1       INPUT MATERIAL TABLE IT_MATERIAL
*  <--  p2       OUTPUT THE RESULT TABLE IT_RESULT
*----------------------------------------------------------------------*
FORM INITAL_RESULT.
 DATA: I_LINES TYPE I.

   DESCRIBE TABLE IT_MATERIAL LINES I_LINES.
   IF I_LINES <> 0.
     LOOP AT IT_MATERIAL.
        MOVE-CORRESPONDING IT_MATERIAL TO IT_RESULT.
        IT_RESULT-ENGINE = 'N'.
        IT_RESULT-TM = 'N'.
        APPEND IT_RESULT.
     ENDLOOP.
   ENDIF.
ENDFORM.                    " INITAL_RESULT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       DISPLAY THE CHECKING RESULT TABLE IT_RESULT
*----------------------------------------------------------------------*
*  -->  p1        INPUT THE INTERNAL TABLE IT_RESULT
*  <--  p2        DISPLAY ON SCREEN
*----------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  DATA: I_LINES TYPE I.
  DATA: I_LINE_GROUP TYPE I.
  DATA: I_SKIP TYPE I.
*CHECK MATERIAL-EXIST INDICATOR
   IF S_NOT_EXIST IS INITIAL.

     DESCRIBE TABLE it_result lines I_LINES.
      WRITE: / SY-ULINE.
      IF I_LINES > 0.
        WRITE: /0(18) TEXT-008, 22(8) TEXT-009,
                31(10) TEXT-010, 42(10) TEXT-011,
                53(5) TEXT-012, 59(5) TEXT-013,
                65(18) TEXT-014,84(18) TEXT-015.
        WRITE: / SY-ULINE.
*       INITIALIZE THE COUNTER
        I_LINE_GROUP = 1.
        I_LINES = 0.

        LOOP AT IT_RESULT.
          I_LINES = I_LINES + 1.
          WRITE: /0(18) IT_RESULT-MATNR, 22(8) IT_RESULT-WERKS,
                  31(10) IT_RESULT-STLAN,42(10) IT_RESULT-STLAL,
                  53(5) IT_RESULT-ENGINE, 59(5) IT_RESULT-TM,
                  65(18) IT_RESULT-ENGNO,84(18) IT_RESULT-TMNO .
          I_SKIP = 5 * I_LINE_GROUP.
          IF I_LINES EQ I_SKIP.
            SKIP 1.
            I_LINE_GROUP = I_LINE_GROUP + 1.
          ENDIF.
       ENDLOOP.
     ELSE.
        WRITE: / TEXT-019.
     ENDIF.

   ELSEIF S_NOT_EXIST = 'X'.
     WRITE: / TEXT-006.
   ELSEIF S_NOT_EXIST = 'N'.
     WRITE: / TEXT-007.

   ENDIF.

ENDFORM.                    " DISPLAY_RESULT
