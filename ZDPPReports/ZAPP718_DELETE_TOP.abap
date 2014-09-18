*----------------------------------------------------------------------*
*   INCLUDE ZAPP718_DELETE_TOP                                         *
*----------------------------------------------------------------------*
REPORT  ZAPP718_BACKFLUSH_DELETE MESSAGE-ID ZMPP.

TABLES : PLAF,RESB,ZTPP_STATUS.

DATA  : IT_BFST      LIKE  ZTPP_BFST  OCCURS 0  WITH HEADER LINE,
        WA_BFST      LIKE  IT_BFST,
        IT_DELETE_C  LIKE  ZTPP_BFST  OCCURS 0  WITH HEADER LINE,
        IT_DELETE_S  LIKE  ZTPP_BFST  OCCURS 0  WITH HEADER LINE.
DATA:   IT_BFST_BK   LIKE  ZTPP_BFST  OCCURS 0  WITH HEADER LINE.
DATA : BEGIN OF IT_DELETE OCCURS 0,
         PLNUM       LIKE PLAF-PLNUM,
         PLAF_COUNT  TYPE I,
         RESB_COUNT  TYPE I,
       END OF IT_DELETE .

DATA : IT_RETURN     LIKE BAPIRETURN1 OCCURS 0  WITH HEADER LINE.

DATA : PLAF_COUNT    TYPE I,
       RESB_COUNT    TYPE I,
       W_INT         TYPE I,
       W_BACKUP      TYPE I,
       TO_DATE       LIKE SY-DATUM,
       BE_DATE       LIKE SY-DATUM,
       WK_GUI_MSG(40),
       W_ARBPL       LIKE CRHD-ARBPL,
       W_RP_POINT    LIKE ZTPP_STATUS-RP_POINT VALUE '17',
       W_WERKS       LIKE T001W-WERKS VALUE 'P001',
       W_KALID       LIKE KAKO-KALID,                  "Calender ID
       W_MOSID       LIKE KAKO-MOSID,                  "Schedule group
       W_KAPID       LIKE KAKO-KAPID.                  "Capacity ID
