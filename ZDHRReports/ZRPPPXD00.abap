
*----------------------------------------------------------------------*
*  R/3 data descriptions for PCL1- and PCL2-buffer, respectively
*----------------------------------------------------------------------*
TABLES: PCL3,
        PCL4.
DATA: SAVE-SGART(2).

DATA: MAIN-SUBRC      LIKE SY-SUBRC,
      READ-SUBRC      LIKE SY-SUBRC,
      READ-DIR-SUBRC  LIKE SY-SUBRC,
      READ-PCLX-SUBRC LIKE SY-SUBRC,
      DIR-TABIX       LIKE SY-TABIX,
      TBUFF-TABIX     LIKE SY-TABIX.

DATA: AUX_SRTF2 LIKE PCL1-SRTF2,
      AUX_ONUXT LIKE PCL1-SRTF2,
      AUX_NTABX LIKE SY-TABIX,
      AUX_OTABX LIKE SY-TABIX.
DATA: DEL-COUNTER TYPE P.
DATA: NTABX-LOW  LIKE SY-TABIX,
      NTABX-HIGH LIKE SY-TABIX,
      OTABX-LOW  LIKE SY-TABIX,
      OTABX-HIGH LIKE SY-TABIX.
DATA: PPPPP TYPE P.

DATA: SEQ-INPUT.
"***********************************************************************
