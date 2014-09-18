*&-------------------------------------------------------------*
*& Report ZTMM_MOD_PRI_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_MOD_PRI_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_MOD_PRI_BK_W .

***** Include TOP
INCLUDE ZTMM_MOD_PRI_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_COMP FOR ZTMM_MOD_PRI_BK-COMP.
*SELECT-OPTIONS S_LIFNR FOR ZTMM_MOD_PRI_BK-LIFNR.
*SELECT-OPTIONS S_MATNR FOR ZTMM_MOD_PRI_BK-MATNR.
SELECT-OPTIONS S_RUN_DA FOR ZTMM_MOD_PRI_BK-RUN_DATE.
*SELECT-OPTIONS S_RUN_TI FOR ZTMM_MOD_PRI_BK-RUN_TIME.
*SELECT-OPTIONS S_UPGVC FOR ZTMM_MOD_PRI_BK-UPGVC.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_MOD_P' NO-DISPLAY .
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
SELECT * FROM ZTMM_MOD_PRI_BK
*WHERE COMP IN S_COMP
*AND LIFNR IN S_LIFNR
*AND MATNR IN S_MATNR
WHERE RUN_DATE IN S_RUN_DA.
*AND RUN_TIME IN S_RUN_TI
*AND UPGVC IN S_UPGVC.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
