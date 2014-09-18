*&-------------------------------------------------------------*
*& Report ZTMM_EAI_INV_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_EAI_INV (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_EAI_INV_W .

***** Include TOP
INCLUDE ZTMM_EAI_INV_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_EBIN_N FOR ZTMM_EAI_INV-EBIN_NO.
*SELECT-OPTIONS S_ELOC_T FOR ZTMM_EAI_INV-ELOC_TP.
*SELECT-OPTIONS S_EPART_ FOR ZTMM_EAI_INV-EPART_NO.
*SELECT-OPTIONS S_ESTRG_ FOR ZTMM_EAI_INV-ESTRG_UT.
SELECT-OPTIONS S_TAIT_T FOR ZTMM_EAI_INV-TAIT_TARG_D.
*SELECT-OPTIONS S_TAIT_T FOR ZTMM_EAI_INV-TAIT_TARG_T.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_EAI_I' NO-DISPLAY .
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
SELECT * FROM ZTMM_EAI_INV
*WHERE EBIN_NO IN S_EBIN_N
*AND ELOC_TP IN S_ELOC_T
*AND EPART_NO IN S_EPART_
*AND ESTRG_UT IN S_ESTRG_
WHERE TAIT_TARG_D IN S_TAIT_T.
*AND TAIT_TARG_T IN S_TAIT_T.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
