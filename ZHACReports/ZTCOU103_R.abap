*&-------------------------------------------------------------*
*& Report ZTCOU103_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCOU103 (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCOU103_R .

***** Include TOP
INCLUDE ZTCOU103_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCOU103' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-ARTNR,
T_ITAB-BDATJ,
T_ITAB-INDX,
T_ITAB-KALKA,
T_ITAB-KOKRS,
T_ITAB-POPER,
T_ITAB-VER,
T_ITAB-WERKS.
ENDFORM.
