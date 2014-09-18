*----------------------------------------------------------------------*
*   INCLUDE ZISD09U_MOBIS_ETD_TOP                                      *
*----------------------------------------------------------------------*
TABLES : LIKP, LIPS, VBFA, VBAK, VBAP, VBKD,
         NAST.


DATA : BEGIN OF IT_HEADER OCCURS 0,
       RECORD(26),
       END OF IT_HEADER.

DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
       RECORD(94),
       END OF IT_DOWNFILE.

DATA : W_NUMC_OR(7) TYPE N,
       W_NUMC_DL(7) TYPE N,
       W_NUMC_OP(7) TYPE N,
       W_DSN(90). " VALUE '/usr/sap/EDI_SAP/'.
