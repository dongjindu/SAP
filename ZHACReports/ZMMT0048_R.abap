*&-------------------------------------------------------------*
*& Report ZMMT0048_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0048 (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0048_R .

***** Include TOP
INCLUDE ZMMT0048_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0048' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-EXIDV,
T_ITAB-LGORT,
T_ITAB-LIFNR,
T_ITAB-MATNR,
T_ITAB-MBLNR,
T_ITAB-POSNR,
T_ITAB-VBELN,
T_ITAB-ZFLAG.
ENDFORM.
