*&-------------------------------------------------------------*
*& Report ZTMM_EAI_ASN_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_EAI_ASN (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_EAI_ASN_W .

***** Include TOP
INCLUDE ZTMM_EAI_ASN_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_EBL_NO FOR ZTMM_EAI_ASN-EBL_NO.
*SELECT-OPTIONS S_ECASE_ FOR ZTMM_EAI_ASN-ECASE_NO.
*SELECT-OPTIONS S_ECONT_ FOR ZTMM_EAI_ASN-ECONT_NO.
*SELECT-OPTIONS S_EINBD_ FOR ZTMM_EAI_ASN-EINBD_NO.
*SELECT-OPTIONS S_EORDER FOR ZTMM_EAI_ASN-EORDER_NO.
*SELECT-OPTIONS S_EPART_ FOR ZTMM_EAI_ASN-EPART_NO.
SELECT-OPTIONS S_TAIT_T FOR ZTMM_EAI_ASN-TAIT_TARG_D.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_EAI_A' NO-DISPLAY .
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
SELECT * FROM ZTMM_EAI_ASN
*WHERE EBL_NO IN S_EBL_NO
*AND ECASE_NO IN S_ECASE_
*AND ECONT_NO IN S_ECONT_
*AND EINBD_NO IN S_EINBD_
*AND EORDER_NO IN S_EORDER
*AND EPART_NO IN S_EPART_
WHERE TAIT_TARG_D IN S_TAIT_T.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
