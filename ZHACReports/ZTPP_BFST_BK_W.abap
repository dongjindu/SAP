*&-------------------------------------------------------------*
*& Report ZTPP_BFST_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_BFST_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_BFST_BK_W .

***** Include TOP
INCLUDE ZTPP_BFST_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BODY_S FOR ZTPP_BFST_BK-BODY_SER.
*SELECT-OPTIONS S_MODEL FOR ZTPP_BFST_BK-MODEL.
*SELECT-OPTIONS S_PLANT FOR ZTPP_BFST_BK-PLANT.
*SELECT-OPTIONS S_PLAN_O FOR ZTPP_BFST_BK-PLAN_ORD.
SELECT-OPTIONS S_BFP18 FOR ZTPP_BFST_BK-BFP18_DAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPBFSTBK' NO-DISPLAY .
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
SELECT * FROM ZTPP_BFST_BK
*WHERE BODY_SER IN S_BODY_S
*AND MODEL IN S_MODEL
*AND PLANT IN S_PLANT
*AND PLAN_ORD IN S_PLAN_O
WHERE BFP18_DAT in S_BFP18.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
