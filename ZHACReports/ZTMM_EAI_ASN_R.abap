*&-------------------------------------------------------------*
*& Report ZTMM_EAI_ASN_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_EAI_ASN (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_EAI_ASN_R .

***** Include TOP
INCLUDE ZTMM_EAI_ASN_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_EAI_A' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-EBL_NO,
T_ITAB-ECASE_NO,
T_ITAB-ECONT_NO,
T_ITAB-EINBD_NO,
T_ITAB-EORDER_NO,
T_ITAB-EPART_NO,
T_ITAB-TAIT_TARG_D.
ENDFORM.
