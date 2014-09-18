*&-------------------------------------------------------------*
*& Report ZTPPVR_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPPVR_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPPVR_BK_W .

***** Include TOP
INCLUDE ZTPPVR_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_FLAG FOR ZTPPVR_BK-FLAG.
*SELECT-OPTIONS S_P_BODY FOR ZTPPVR_BK-P_BODY_SERIAL.
*SELECT-OPTIONS S_P_MODE FOR ZTPPVR_BK-P_MODEL.
*SELECT-OPTIONS S_P_RP_S FOR ZTPPVR_BK-P_RP_SERIAL.
*SELECT-OPTIONS S_P_STAT FOR ZTPPVR_BK-P_STATUS.
SELECT-OPTIONS S_ZEDAT FOR ZTPPVR_BK-ZEDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPVR_BK' NO-DISPLAY .
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
SELECT * FROM ZTPPVR_BK
*WHERE FLAG IN S_FLAG
*AND P_BODY_SERIAL IN S_P_BODY
*AND P_MODEL IN S_P_MODE
*AND P_RP_SERIAL IN S_P_RP_S
*AND P_STATUS IN S_P_STAT
WHERE ZEDAT IN S_ZEDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
