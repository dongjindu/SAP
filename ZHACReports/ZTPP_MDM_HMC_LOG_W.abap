*&-------------------------------------------------------------*
*& Report ZTPP_MDM_HMC_LOG_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_MDM_HMC_LOG (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_MDM_HMC_LOG_W .

***** Include TOP
INCLUDE ZTPP_MDM_HMC_LOG_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_COMP FOR ZTPP_MDM_HMC_LOG-COMP.
*SELECT-OPTIONS S_PROD_D FOR ZTPP_MDM_HMC_LOG-PROD_DATE.
*SELECT-OPTIONS S_VIN FOR ZTPP_MDM_HMC_LOG-VIN.
SELECT-OPTIONS S_ZEDAT FOR ZTPP_MDM_HMC_LOG-ZEDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_MDM_L' NO-DISPLAY .
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
SELECT * FROM ZTPP_MDM_HMC_LOG
*WHERE COMP IN S_COMP
*AND PROD_DATE IN S_PROD_D
*AND VIN IN S_VIN.
WHERE ZEDAT IN S_ZEDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
