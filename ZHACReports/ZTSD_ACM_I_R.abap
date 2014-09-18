*&-------------------------------------------------------------*
*& Report ZTSD_ACM_I_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTSD_ACM_I (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTSD_ACM_I_R .

***** Include TOP
INCLUDE ZTSD_ACM_I_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTSD_ACM_I' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-ZACLN,
T_ITAB-ZCDLR,
T_ITAB-ZCDST,
T_ITAB-ZCSER,
T_ITAB-ZLINE.
ENDFORM.
