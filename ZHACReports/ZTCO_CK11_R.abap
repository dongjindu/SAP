*&-------------------------------------------------------------*
*& Report ZTCO_CK11_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_CK11 (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_CK11_R .

***** Include TOP
INCLUDE ZTCO_CK11_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCO_CK11' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-ARTNR,
T_ITAB-BDATJ,
T_ITAB-COMPN,
T_ITAB-INDX,
T_ITAB-KLVAR,
T_ITAB-KOKRS,
T_ITAB-POPER,
T_ITAB-REFDT,
T_ITAB-VERID,
T_ITAB-WERKS.
ENDFORM.
