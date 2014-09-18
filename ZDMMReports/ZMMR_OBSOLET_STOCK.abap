************************************************************************
* Program Name      : ZMMR_OBSOLET_STOCK
* Author            : Furong Wang
* Creation Date     : 05/26/2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZMMR_OBSOLET_STOCK MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, ZTMM_OBS_STOCK.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_OBS_STOCK OCCURS 0.
        INCLUDE STRUCTURE ZTMM_OBS_STOCK.
DATA: END OF IT_OBS_STOCK.

DATA: BEGIN OF IT_MBEW OCCURS 0,
        MATNR LIKE MBEW-MATNR,
        WERKS LIKE MBEW-BWKEY,
        BKLAS LIKE MBEW-BKLAS,
        LBKUM LIKE MBEW-LBKUM,
        SALK3 LIKE MBEW-SALK3,
        MTART LIKE MARA-MTART,
*        MSTAE LIKE MARA-MSTAE,
        DISPO LIKE MARC-DISPO,
*        LGPRO LIKE MARC-LGPRO,
        MATKL LIKE MARA-MATKL,
        END OF IT_MBEW.

*DATA: BEGIN OF IT_STOCK OCCURS 0,
*        MATNR LIKE MBEW-MATNR,
*        LBKUM LIKE MBEW-LBKUM,
*        SALK3 LIKE MBEW-SALK3,
*        END OF IT_STOCK.

*
*DATA: W_MATNR LIKE MBEW-MATNR,
*      W_WERKS LIKE MBEW-BWKEY.
** ALV
*DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
*       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.
*
*DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
*       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.
*
*DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
*      IT_EXCLUDE TYPE UI_FUNCTIONS.
*
*DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
*      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
*      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*
*DATA: WA_CUSTOM_CONTROL_800 TYPE SCRFNAME VALUE 'ALV_CONTAINER_800',
*      ALV_GRID_800          TYPE REF TO CL_GUI_ALV_GRID,
*      GRID_CONTAINER_800    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*
*DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.
*
*DATA: OK_CODE LIKE SY-UCOMM,
*      W_CODE LIKE SY-UCOMM,
*      W_OLD_CODE LIKE SY-UCOMM,
*      W_CNT   TYPE   I,
*      W_BASE_DATE LIKE SY-DATUM,
*      W_BASE_TIME LIKE SY-UZEIT,
*      W_REPID LIKE SY-REPID,
*      W_DYNNR LIKE SY-DYNNR.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:   P_SAVE(1) DEFAULT 'X'.

SELECT-OPTIONS: S_MATRN FOR MARA-MATNR.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
INITIALIZATION.
*  PERFORM INIT_DATA.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM CHECKING_BATCH_JOB.
  PERFORM GET_DATA.
*  PERFORM PROCESS_DATA.

  IF P_SAVE = 'X'.
    PERFORM SAVE_TO_TABLE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM INIT_DATA.
*  W_REPID = SY-REPID.
*  W_DYNNR = SY-DYNNR.
*ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_TABLE.

  DELETE FROM ZTMM_OBS_STOCK CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

*  IF SY-SUBRC = 0.
  INSERT ZTMM_OBS_STOCK FROM TABLE IT_OBS_STOCK.
*  ELSE.
*    MESSAGE E000(ZZ) WITH 'Error: Z-Table deletion (ZTMM_HOUR_SHORT)'.
*  ENDIF.
ENDFORM.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Form  checking_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CHECKING_BATCH_JOB.
  DATA: L_BACKJOB LIKE  SY-REPID,
        LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
  L_BACKJOB = SY-REPID.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
       EXPORTING
            ABAP_PROGRAM_NAME             = L_BACKJOB
            DIALOG                        = 'N'
            STATUS                        = 'R'
       TABLES
            JOBLIST                       = LT_JOBLIST
       EXCEPTIONS
            NO_JOBS_FOUND                 = 1
            PROGRAM_SPECIFICATION_MISSING = 2
            INVALID_DIALOG_TYPE           = 3
            JOB_FIND_CANCELED             = 4
            OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999(PP) WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
*  ELSE.
*    READ TABLE LT_JOBLIST INDEX 1.
*    IF SY-SUBRC EQ 0.
*      MESSAGE E999(PP) WITH TEXT-M01.
*    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA: L_INDEX LIKE SY-TABIX,
        L_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT,
        L_COUNT_TOTAL LIKE ZTMM_OBS_STOCK-SEQ,
        LGR_DATE LIKE SY-DATUM,
        LBF_DATE LIKE SY-DATUM,
        L_PRE_FIRST_DATE LIKE SY-DATUM.

  DATA: BEGIN OF LT_MARA OCCURS 0,
        MATNR LIKE MARA-MATNR,
        MTART LIKE MARA-MTART,
        MSTDE LIKE MARA-MSTDE,
        MATKL LIKE MARA-MATKL,
        END OF LT_MARA.

  DATA:    L_LINES TYPE I.

  DATA: BEGIN OF LT_LGPRO OCCURS 4,
        MATNR LIKE MARC-MATNR,
        WERKS LIKE MARC-WERKS,
        LGPRO LIKE MARC-LGPRO,
        DISPO LIKE MARC-DISPO,
        END OF LT_LGPRO.

  DATA: IT_MBEW_TEMP LIKE TABLE OF IT_MBEW WITH HEADER LINE.


  L_DATE = SY-DATUM.
  L_TIME = SY-UZEIT.

  SELECT MATNR MTART MSTDE MATKL INTO TABLE LT_MARA
    FROM MARA
    WHERE MSTAE ='OB'
    AND MATNR IN S_MATRN.

  IF SY-SUBRC <> 0.
    MESSAGE E000(ZZ) WITH 'No Data'.
  ENDIF.

*  SELECT A~MATNR BWKEY AS WERKS BKLAS LBKUM SALK3 DISPO
*    INTO CORRESPONDING FIELDS OF TABLE IT_MBEW
*    FROM MBEW AS A
*    INNER JOIN MARC AS C
*    ON A~MATNR = C~MATNR
*    AND A~BWKEY = C~WERKS
*    FOR ALL ENTRIES IN LT_MARA
*    WHERE A~MATNR = LT_MARA-MATNR.

  SELECT A~MATNR BWKEY AS WERKS BKLAS LBKUM SALK3    " DISPO
     INTO CORRESPONDING FIELDS OF TABLE IT_MBEW_TEMP
     FROM MBEW AS A
     INNER JOIN MARC AS C
     ON A~MATNR = C~MATNR
     AND A~BWKEY = C~WERKS
     FOR ALL ENTRIES IN LT_MARA
     WHERE A~MATNR = LT_MARA-MATNR
     AND LBKUM > 0.

  SORT IT_MBEW_TEMP BY MATNR.
  LOOP AT IT_MBEW_TEMP.
    IT_MBEW = IT_MBEW_TEMP.
    CLEAR: IT_MBEW-WERKS.
    COLLECT IT_MBEW.
  ENDLOOP.

  LOOP AT IT_MBEW.
    L_INDEX = SY-TABIX.
    REFRESH LT_LGPRO.
    SELECT MATNR WERKS LGPRO DISPO INTO TABLE LT_LGPRO
     FROM MARC
     WHERE MATNR = IT_MBEW-MATNR
       AND LGPRO <> ' '.

** Changed on on 03/11/11
*    CLEAR: L_LINES.
*    DESCRIBE TABLE LT_LGPRO LINES L_LINES.
*    IF L_LINES > 1.
*      READ TABLE LT_LGPRO WITH KEY WERKS = 'E001'.
*      IT_MBEW-WERKS = 'E001'.
*      IT_MBEW-DISPO = LT_LGPRO-DISPO.
*    ELSE.
*      READ TABLE LT_LGPRO INDEX 1.
*      IT_MBEW-WERKS = LT_LGPRO-WERKS.
*      IT_MBEW-DISPO = LT_LGPRO-DISPO.
*    ENDIF.

    IF SY-SUBRC = 0.
      CLEAR: L_LINES.
      DESCRIBE TABLE LT_LGPRO LINES L_LINES.
      IF L_LINES > 1.
        READ TABLE LT_LGPRO WITH KEY WERKS = 'E001'.
      ELSE.
        READ TABLE LT_LGPRO INDEX 1.
      ENDIF.
      IT_MBEW-WERKS = LT_LGPRO-WERKS.
      IT_MBEW-DISPO = LT_LGPRO-DISPO.
    ELSE.
      READ TABLE IT_MBEW_TEMP WITH KEY MATNR = IT_MBEW-MATNR
                                  WERKS = 'E001'
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_MBEW-WERKS = IT_MBEW_TEMP-WERKS.
        IT_MBEW-DISPO = IT_MBEW_TEMP-DISPO.
      ELSE.
        READ TABLE IT_MBEW_TEMP WITH KEY MATNR = IT_MBEW-MATNR
                                   WERKS = 'P001'
                                   BINARY SEARCH.
        IF SY-SUBRC = 0.
          IT_MBEW-WERKS = IT_MBEW_TEMP-WERKS.
          IT_MBEW-DISPO = IT_MBEW_TEMP-DISPO.
        ELSE.
          IT_MBEW-WERKS = 'P001'.
          IT_MBEW-DISPO = IT_MBEW_TEMP-DISPO.
        ENDIF.
      ENDIF.
    ENDIF.
** End of change on 03/11/11

    MODIFY IT_MBEW INDEX L_INDEX.
  ENDLOOP.

  SORT IT_MBEW BY MATNR WERKS.

  LOOP AT IT_MBEW.
    MOVE-CORRESPONDING IT_MBEW TO IT_OBS_STOCK.

    READ TABLE LT_MARA WITH KEY MATNR = IT_MBEW-MATNR.
*                                  BINARY SEARCH.

    IT_OBS_STOCK-MTART = LT_MARA-MTART.
    IT_OBS_STOCK-OBS_DATE = LT_MARA-MSTDE.
    IT_OBS_STOCK-MATKL = LT_MARA-MATKL.

*    READ TABLE IT_STOCK WITH KEY MATNR = IT_DATA-MBEW
*                                  BINARY SEARCH.
*
*    IT_OBS_STOCK-LBKUM = IT_STOCK-LBKUM.
*    IT_OBS_STOCK-SALK3 = IT_STOCK-SALK3.
*
    SELECT SINGLE LIFNR INTO IT_OBS_STOCK-LIFNR
     FROM MARA AS A
     INNER JOIN EORD AS D
     ON A~MATNR = D~MATNR
     WHERE A~MATNR = IT_MBEW-MATNR
       AND WERKS = 'P001'
       AND VDATU <= SY-DATUM
       AND BDATU >= SY-DATUM.

    IT_OBS_STOCK-ZSDAT = L_DATE.
    IT_OBS_STOCK-ZSTIM = L_TIME.
    APPEND IT_OBS_STOCK.
    CLEAR: IT_OBS_STOCK.
  ENDLOOP.

** Changed by Furong on 10/27/10
  SORT IT_OBS_STOCK BY SALK3 DESCENDING.
  L_COUNT_TOTAL = '00001'.

  CONCATENATE SY-DATUM+0(6) '01' INTO L_PRE_FIRST_DATE.
  L_PRE_FIRST_DATE = L_PRE_FIRST_DATE - 1.
  CONCATENATE L_PRE_FIRST_DATE+0(6) '01' INTO L_PRE_FIRST_DATE.


  LOOP AT IT_OBS_STOCK.
** Changed by Furong on 11/23/10
*    IF IT_OVERSTOCK-WERKS = 'P001' AND L_COUNT_P001 <= '00010'.
*      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LGR_DATE
*      FROM MSEG
*      WHERE MATNR = IT_OBS_STOCK-MATNR
**        AND WERKS = IT_OVERSTOCK-WERKS
*        AND BWART = '101'.
*      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LBF_DATE
*       FROM MSEG
*       WHERE MATNR = IT_OBS_STOCK-MATNR
**         AND WERKS = IT_OVERSTOCK-WERKS
*         AND BWART = '261'.
    CLEAR: LGR_DATE, LBF_DATE.
    SELECT SINGLE LGR_DATE LBF_DATE INTO (LGR_DATE, LBF_DATE)
     FROM ZTMM_OBS_STOCK
     WHERE MATNR = IT_OBS_STOCK-MATNR
     AND WERKS = IT_OBS_STOCK-WERKS
     AND MTART = IT_OBS_STOCK-MTART.

    IF LGR_DATE IS INITIAL.
      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LGR_DATE
        FROM MSEG
        WHERE MATNR = IT_OBS_STOCK-MATNR
         AND BWART = '101'.
    ELSE.
*      IF LGR_DATE < L_PRE_FIRST_DATE.
*        LGR_DATE = L_PRE_FIRST_DATE.
*      ENDIF.
      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LGR_DATE
        FROM MSEG
        WHERE ZBUDAT BETWEEN LGR_DATE AND SY-DATUM
          AND MATNR = IT_OBS_STOCK-MATNR
          AND BWART = '101'.
      IF SY-SUBRC <> 0.
        IT_OBS_STOCK-LGR_DATE = LGR_DATE.
      ENDIF.
    ENDIF.
    IF LBF_DATE IS INITIAL.
      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LBF_DATE
         FROM MSEG
         WHERE MATNR = IT_OBS_STOCK-MATNR

         AND BWART = '261'.
    ELSE.
*      IF LBF_DATE < L_PRE_FIRST_DATE.
*        LBF_DATE = L_PRE_FIRST_DATE.
*      ENDIF.
      SELECT SINGLE MAX( ZBUDAT ) INTO IT_OBS_STOCK-LBF_DATE
         FROM MSEG
         WHERE ZBUDAT BETWEEN LBF_DATE AND SY-DATUM
         AND MATNR = IT_OBS_STOCK-MATNR
         AND BWART = '261'.
      IF SY-SUBRC <> 0.
        IT_OBS_STOCK-LBF_DATE = LBF_DATE.
      ENDIF.
    ENDIF.
** End of change on 11/23/10
    IT_OBS_STOCK-SEQ =     L_COUNT_TOTAL.
    L_COUNT_TOTAL =     L_COUNT_TOTAL + 1.
    MODIFY IT_OBS_STOCK.
    CLEAR: IT_OBS_STOCK.
  ENDLOOP.
** End of change


ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  FINAL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM FINAL_PROCESS.
*  DATA: L_DATE LIKE SY-DATUM,
*         L_TIME LIKE SY-UZEIT,
*         L_MATKL LIKE MARA-MATKL,
*         L_MTART LIKE MARA-MTART.
*
*  L_DATE = SY-DATUM.
*  L_TIME = SY-UZEIT.
*
*  SORT IT_DATA BY MATNR DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM IT_DATA COMPARING MATNR.
*
*  LOOP AT IT_DATA.
*    MOVE-CORRESPONDING IT_DATA TO IT_OBS_STOCK.
*
*    READ TABLE IT_MBEW WITH KEY MATNR = IT_DATA-MATNR
*                                WERKS = IT_DATA-WERKS
*                                BINARY SEARCH.
*
*    IT_OBS_STOCK-BKLAS = IT_MBEW-BKLAS.
*    IT_OBS_STOCK-MTART = IT_MBEW-MTART.
**    IT_OBS_STOCK-MSTAE = IT_MBEW-MSTAE.
*    IT_OBS_STOCK-DISPO = IT_MBEW-DISPO.
*    IT_OBS_STOCK-MATKL = IT_MBEW-MATKL.
*
*    READ TABLE IT_STOCK WITH KEY MATNR = IT_DATA-MATNR
*                                  BINARY SEARCH.
*
*    IT_OBS_STOCK-LBKUM = IT_STOCK-LBKUM.
*    IT_OBS_STOCK-SALK3 = IT_STOCK-SALK3.
*
*    SELECT SINGLE LIFNR INTO IT_OBS_STOCK-LIFNR
*     FROM MARA AS A
*     INNER JOIN EORD AS D
*     ON A~MATNR = D~MATNR
*     WHERE A~MATNR = IT_DATA-MATNR
*       AND WERKS = 'P001'
*       AND VDATU <= SY-DATUM
*       AND BDATU >= SY-DATUM.
*
*    IT_OBS_STOCK-ZSDAT = L_DATE.
*    IT_OBS_STOCK-ZSTIM = L_TIME.
*    APPEND IT_OBS_STOCK.
*    CLEAR: IT_OBS_STOCK.
*  ENDLOOP.
*
*ENDFORM.                    " FINAL_PROCESS
**&---------------------------------------------------------------------
**
**&      Form  PROCESS_DATA_NO_BOM
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM PROCESS_DATA.
*  DESCRIBE TABLE IT_MARA LINES W_LINES.
*
*  IF W_LINES > 0.
*
*    LOOP AT IT_MARA.
*      MOVE-CORRESPONDING IT_MARA TO IT_DATA.
*      APPEND IT_DATA.
*    ENDLOOP.
*
*    PERFORM FINAL_PROCESS.
*  ENDIF.
*
*ENDFORM.                    " PROCESS_DATA
