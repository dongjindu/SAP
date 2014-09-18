*&-------------------------------------------------------------*
*& Report ZTMM_MOD_PRI_BK_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_MOD_PRI_BK (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_MOD_PRI_BK_R .

***** Include TOP
INCLUDE ZTMM_MOD_PRI_BK_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_MOD_P' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-COMP,
T_ITAB-LIFNR,
T_ITAB-MATNR,
T_ITAB-RUN_DATE,
T_ITAB-RUN_TIME,
T_ITAB-UPGVC.
ENDFORM.
