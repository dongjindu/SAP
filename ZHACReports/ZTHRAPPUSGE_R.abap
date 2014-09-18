*&-------------------------------------------------------------*
*& Report ZTHRAPPUSGE_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTHRAPPUSGE (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTHRAPPUSGE_R .

***** Include TOP
INCLUDE ZTHRAPPUSGE_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTHRAPPUSG' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-ACCOUNT,
T_ITAB-INSTANCE,
T_ITAB-LDATE,
T_ITAB-TCODE.
ENDFORM.
