*&-------------------------------------------------------------*
*& Report ZTMM_STL_LOG_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_STL_LOG (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_STL_LOG_W .

***** Include TOP
INCLUDE ZTMM_STL_LOG_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS S_LOGNO FOR ZTMM_STL_LOG-LOGNO_H.
SELECT-OPTIONS S_MATNR FOR ZTMM_STL_LOG-MATNR.
SELECT-OPTIONS S_SDATE FOR ZTMM_STL_LOG-SDATE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_STL_L' NO-DISPLAY .
SELECTION-SCREEN SKIP 1.
PARAMETERS: COMMENT   LIKE ADMI_RUN-COMMENTS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

***** Main login - common routine of include
PERFORM ARCHIVE_PROCESS.

***** Common routine
INCLUDE ZITARCW.

***** History for each object,
***** processing required for each part defined,
FORM OPEN_CURSOR_FOR_DB.
  OPEN CURSOR WITH HOLD G_CURSOR FOR
SELECT * FROM ZTMM_STL_LOG
WHERE LOGNO_H IN S_LOGNO
AND MATNR IN S_MATNR
AND SDATE IN S_SDATE.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
