*&-------------------------------------------------------------*
*& Report ZTPP_RPID_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_RPID (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_RPID_W .

***** Include TOP
INCLUDE ZTPP_RPID_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BODY_N FOR ZTPP_RPID-BODY_NO.
SELECT-OPTIONS S_MODEL FOR ZTPP_RPID-MODEL_CODE.
SELECT-OPTIONS S_BODY_N FOR ZTPP_RPID-BODY_NO.

SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_RPID' NO-DISPLAY .
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
SELECT * FROM ZTPP_RPID
*WHERE BODY_NO IN S_BODY_N
WHERE MODEL_CODE IN S_MODEL
AND BODY_NO IN S_BODY_N.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
