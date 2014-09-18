*&-------------------------------------------------------------*
*& Report ZMMT0041_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0041 (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0041_R .

***** Include TOP
INCLUDE ZMMT0041_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0041' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-EXIDV,
T_ITAB-PKKEY,
T_ITAB-REVERSED,
T_ITAB-RSNUM,
T_ITAB-RSPOS,
T_ITAB-SAEDT,
T_ITAB-SAEUZ.
ENDFORM.
