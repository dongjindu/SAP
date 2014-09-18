*&-------------------------------------------------------------*
*& Report ZTFI_RECLAIM_LOG_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTFI_RECLAIM_LOG (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTFI_RECLAIM_LOG_R .

***** Include TOP
INCLUDE ZTFI_RECLAIM_LOG_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTFI_RECLL' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-CORP,
T_ITAB-DOEX,
T_ITAB-ERZET,
T_ITAB-FLDT,
T_ITAB-ISSU,
T_ITAB-RONO,
T_ITAB-SERL,
T_ITAB-VNDR.
ENDFORM.
