*&-------------------------------------------------------------*
*& Report ZTPP_WOSUM2_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_WOSUM2_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_WOSUM2_BK_W .

***** Include TOP
INCLUDE ZTPP_WOSUM2_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_CR_MON FOR ZTPP_WOSUM2_BK-CR_MONTH.
SELECT-OPTIONS S_CR_DAT FOR ZTPP_WOSUM2_BK-CR_DATE.
*SELECT-OPTIONS S_DEALER FOR ZTPP_WOSUM2_BK-DEALER.
*SELECT-OPTIONS S_EXTC FOR ZTPP_WOSUM2_BK-EXTC.
*SELECT-OPTIONS S_INTC FOR ZTPP_WOSUM2_BK-INTC.
*SELECT-OPTIONS S_NATION FOR ZTPP_WOSUM2_BK-NATION.
*SELECT-OPTIONS S_WO_SER FOR ZTPP_WOSUM2_BK-WO_SER.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_WOSUM' NO-DISPLAY .
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
SELECT * FROM ZTPP_WOSUM2_BK
*WHERE CR_DATE IN S_CR_DAT
*WHERE CR_MONTH IN S_CR_MON
 WHERE CR_DATE IN S_CR_DAT.
*AND DEALER IN S_DEALER
*AND EXTC IN S_EXTC
*AND INTC IN S_INTC
*AND NATION IN S_NATION
*AND WO_SER IN S_WO_SER.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
