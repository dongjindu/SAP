*&-------------------------------------------------------------*
*& Report ZTFI_RECLAIM_DAT_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTFI_RECLAIM_DAT (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTFI_RECLAIM_DAT_R .

***** Include TOP
INCLUDE ZTFI_RECLAIM_DAT_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTFI_RECLA' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-CORP,
T_ITAB-DOEX,
T_ITAB-ISSU,
T_ITAB-RONO,
T_ITAB-SERL,
T_ITAB-VNDR.
ENDFORM.
