*&-------------------------------------------------------------*
*& Report ZTPP_RESB_B06_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_RESB_B06 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_RESB_B06_W .

***** Include TOP
INCLUDE ZTPP_RESB_B06_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BODY FOR ZTPP_RESB_B06-BODY.
*SELECT-OPTIONS S_MATNR FOR ZTPP_RESB_B06-MATNR.
*SELECT-OPTIONS S_RSPOS FOR ZTPP_RESB_B06-RSPOS.
*SELECT-OPTIONS S_SORTF FOR ZTPP_RESB_B06-SORTF.
*SELECT-OPTIONS S_RPSHOP FOR ZTPP_RESB_B06-RP_SHOP_DATE.
SELECT-OPTIONS S_CR_DAT FOR ZTPP_RESB_B06-CR_DATE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPRESBB0' NO-DISPLAY .
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
SELECT * FROM ZTPP_RESB_B06
*WHERE BODY IN S_BODY
*AND MATNR IN S_MATNR
*AND RSPOS IN S_RSPOS
*AND SORTF IN S_SORTF
*AND RP_SHOP_DATE IN S_RPSHOP.
WHERE CR_DATE IN S_CR_DAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
