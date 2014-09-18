*----------------------------------------------------------------------*
*   INCLUDE ZACO11L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : MLCD_KEY, MLCD, MLIT, MLCR, MLPP.
TABLES : MARA, MBEW, MBEWH, CKMLKALNR.
TABLES : DB_TKA01_TKA02.
TABLES : T156Q, CKMLMV010, CKMLMV009T, CKMLMV009.
TABLES : ZTCO_MATLEDGER, ZVCO_MLXXV.

** Internal Tables
* The Group of Movement Types
DATA : IT_CKMLMV010      LIKE STANDARD TABLE OF CKMLMV010
                         WITH HEADER LINE .
* For General Information (Org.)
DATA : IT_DB_TKA01_TKA02 LIKE DB_TKA01_TKA02 OCCURS 0
                         WITH HEADER LINE.
DATA : IT_ORG  TYPE  BWKEYS_WERKS_TAB
               WITH  HEADER LINE .
* For Process Catagory
DATA : IT_CKMLMV009      LIKE STANDARD TABLE OF CKMLMV009
                         WITH HEADER LINE .
* For MBEW
DATA : IT_MBEW           LIKE STANDARD TABLE OF MBEW
                         WITH HEADER LINE .
* For Material / KALNR - Cost estimate Number
DATA : BEGIN OF IT_MATNR_KEY,
         MATNR  LIKE MBEW-MATNR,
         BWKEY  LIKE MBEW-BWKEY,
         BWTAR  LIKE MBEW-BWTAR,
       END OF  IT_MATNR_KEY.
DATA : BEGIN OF IT_MATNR OCCURS 500.
        INCLUDE STRUCTURE CKMLKALNR.
        INCLUDE STRUCTURE IT_MATNR_KEY AS KEY.
DATA :  WERKS   LIKE T001W-WERKS.
DATA : END OF  IT_MATNR.
* For Material Ledger
DATA : IT_MLCD  LIKE STANDARD TABLE OF MLCD
                WITH HEADER LINE .
* For CBO Table
DATA : IT_ZTCO_MATLEDGER LIKE STANDARD TABLE OF ZTCO_MATLEDGER
                         WITH HEADER LINE.
* JOB
DATA  : IT_L_TMP LIKE TABLE OF ZTCO_MATLEDGER.
DATA  :
    SND_JOBS TYPE I ,  "Sent jobs
    RCV_JOBS TYPE I .  "Received replies


*----------------------------------------------------------------------*
*   Macro                                                *
*----------------------------------------------------------------------*
DEFINE READ_KALNR.
WHEN '&1'.
  SELECT SINGLE *  INTO CORRESPONDING FIELDS OF  IT_MATNR-KEY
                   FROM &1
                  WHERE &2 = IT_MATNR-KALNR.
  IF SY-SUBRC = 0.
    MODIFY IT_MATNR.
  ENDIF.
*        IF SY-SUBRC NE 0.
*          PERFORM CHECK_OTHER_WAY USING    IT_MATNR
*                                  CHANGING NOT_FOUND
*                                           INCONSISTENT.
*        ENDIF.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS     MEMORY ID CAC  OBLIGATORY
                                         VALUE CHECK,
             P_BDATJ LIKE MLCD_KEY-BDATJ MEMORY ID BDTJ OBLIGATORY,
             P_POPER LIKE MLCD_KEY-POPER MEMORY ID POPR OBLIGATORY,
             P_CURTP LIKE MLCD-CURTP     DEFAULT '10'   OBLIGATORY,
             P_PRONO(1) TYPE N           DEFAULT '3'.
SELECTION-SCREEN END OF BLOCK BL1.
