*&-------------------------------------------------------------*
*& Report ZMMT0047_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0047 (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0047_R .

***** Include TOP
INCLUDE ZMMT0047_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0047' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-LGORT,
T_ITAB-MATNR,
T_ITAB-ZCRTDT,
T_ITAB-ZCRTIM.
ENDFORM.
