*----------------------------------------------------------------------*
*   INCLUDE ZSSD05U_HMA_COMMERCIAL_INV_TOP                             *
*----------------------------------------------------------------------*
TABLES : ZSSD_INVOICE_H, ZSSD_INVOICE_I,
         NAST,
         VBRK, VBRP, LIKP, TVROT, KNA1, T005T,
         ZTSD_VEH_MOD, VBAK.

DATA : BEGIN OF IT_LIST OCCURS 0,
       MODEL(20),
       MONTH(4),
       MATNR LIKE VBRP-MATNR,
       ARKTX LIKE VBRP-ARKTX,
       FKIMG LIKE VBRP-FKIMG,
       VRKME LIKE VBRP-VRKME,
       NETWR LIKE VBRP-NETWR,
       END OF IT_LIST.

DATA : BEGIN OF IT_LIST_MODEL OCCURS 0,
       MODEL(20),
       FKIMG LIKE VBRP-FKIMG,
       VRKME LIKE VBRP-VRKME,
       NETWR LIKE VBRP-NETWR,
       END OF IT_LIST_MODEL.

DATA : BEGIN OF IT_LIST_MONTH OCCURS 0,
       MODEL(20),
       MONTH(4),
       FKIMG LIKE VBRP-FKIMG,
       VRKME LIKE VBRP-VRKME,
       NETWR LIKE VBRP-NETWR,
       END OF IT_LIST_MONTH.

DATA : BEGIN OF IT_INV_H OCCURS 0.
       INCLUDE STRUCTURE ZSSD_INVOICE_H.
DATA : END OF IT_INV_H.

DATA : BEGIN OF IT_INV_I OCCURS 0.
       INCLUDE STRUCTURE ZSSD_INVOICE_I.
DATA : END OF IT_INV_I.

DATA : W_PRICE LIKE VBRP-NETWR,
       W_FIRST(1),
       W_CNT(2) TYPE N,
       W_EDATE(11),
       W_EDATE2(11),
       W_CHK(1),
       W_SIGN(1),
       H9(10),
       H9_D LIKE SY-DATUM.
DATA: L_BSTNK  LIKE VBAK-BSTNK.

* smart form
DATA : FUNC_MOD_NAME TYPE RS38L_FNAM.
DATA : CONTROL_PARAMETERS TYPE SSFCTRLOP.
DATA : OUTPUT_OPTIONS TYPE SSFCOMPOP.
DATA : JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA : W_SPOOLID TYPE TSFSPOOLID WITH HEADER LINE.


DATA : PDF LIKE TLINE OCCURS 100 WITH HEADER LINE.
DATA : NUMBYTES TYPE I.
DATA : SRCSPOOLID LIKE TSP01-RQIDENT.
DATA : PDFSPOOLID LIKE TSP01-RQIDENT.
DATA : W_FILE LIKE RLGRAP-FILENAME.