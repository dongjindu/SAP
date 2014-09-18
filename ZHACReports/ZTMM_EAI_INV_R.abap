*&-------------------------------------------------------------*
*& Report ZTMM_EAI_INV_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_EAI_INV (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_EAI_INV_R .

***** Include TOP
INCLUDE ZTMM_EAI_INV_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_EAI_I' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-EBIN_NO,
T_ITAB-ELOC_TP,
T_ITAB-EPART_NO,
T_ITAB-ESTRG_UT,
T_ITAB-TAIT_TARG_D,
T_ITAB-TAIT_TARG_T.
ENDFORM.
