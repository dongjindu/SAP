*&-------------------------------------------------------------*
*& Report ZTHR_BHISTHMMA_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTHR_BHISTHMMA (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTHR_BHISTHMMA_R .

***** Include TOP
INCLUDE ZTHR_BHISTHMMA_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTHR_BHIST' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-BADGE,
T_ITAB-RDATE,
T_ITAB-READERID,
T_ITAB-RTIME.
ENDFORM.
