* ZMMR_SA_REVAL_GET_INV
* This program is the copy of zrfi013 for download data to
* ZMMR_SA_REVAL
REPORT ZRFI013  MESSAGE-ID ZMCO.

TABLES: T001,EKBE, EKBEH, MARA, MAKT, MBEW, EKKO, EKPO, KONP.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS: P_BUKRS LIKE EKKO-BUKRS  MEMORY ID BUK.
SELECT-OPTIONS S_LIFNR FOR EKKO-LIFNR MEMORY ID LIF OBLIGATORY.

PARAMETERS: P_CURR  AS CHECKBOX DEFAULT 'X'.
PARAMETERS: P_PAST  AS CHECKBOX DEFAULT 'X'.
PARAMETERS: P_DUE   AS CHECKBOX DEFAULT ' '.
PARAMETERS: P_AUGDT LIKE BSAK-AUGDT.
PARAMETERS: P_REVAL AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK B1.
PARAMETERS: P_SHORT AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS S_MATNR FOR EKBE-MATNR MEMORY ID MAT.
SELECT-OPTIONS S_REFDT FOR SY-DATUM.

SELECT-OPTIONS S_EBELN FOR EKBE-EBELN MEMORY ID BES.
SELECT-OPTIONS S_EBELP FOR EKBE-EBELP.

SELECT-OPTIONS S_GJAHR FOR EKBE-GJAHR MEMORY ID MJA.
SELECT-OPTIONS S_BELNR FOR EKBE-BELNR MEMORY ID MBN.

SELECT-OPTIONS S_BUDAT FOR EKBE-BUDAT.
SELECT-OPTIONS S_CPUDT FOR EKBE-CPUDT.

SELECT-OPTIONS S_LFGJA FOR EKBE-LFGJA.
SELECT-OPTIONS S_LFBNR  FOR EKBE-LFBNR.

SELECT-OPTIONS S_BSART FOR EKKO-BSART MEMORY ID BSA.
SELECT-OPTIONS S_EKGRP FOR EKKO-EKGRP MEMORY ID EKG.

SELECT-OPTIONS S_MATKL FOR MARA-MATKL MEMORY ID MKL.
SELECT-OPTIONS S_MTART FOR MARA-MTART MEMORY ID MTA.
SELECT-OPTIONS S_PROFL FOR MARA-PROFL.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETER P_VARI    LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B3.

SELECT-OPTIONS S_BEWTP FOR EKBE-BEWTP.
PARAMETERS: P_SUMM  AS CHECKBOX DEFAULT ' '.
PARAMETERS: P_DIFF  AS CHECKBOX DEFAULT ' '.
PARAMETERS: P_ALV  AS CHECKBOX DEFAULT 'X'.

DATA: GV_ANSWER(1).
DATA: W_DISPLAY_ALV(1) VALUE 'X'.
DEFINE __POPUP.
  PERFORM POP_UP USING
  &1 &2 &3
  CHANGING GV_ANSWER.
  CHECK GV_ANSWER EQ 'J'.
END-OF-DEFINITION.

*-------------------------------------------------------------*
* DATA
*--------------------------------------------------------------*
DATA: BEGIN OF ITAB OCCURS 1000, " WITH HEADER LINE,
        MATNR LIKE EKBE-MATNR,
        EBELN LIKE EKBE-EBELN,
        EBELP LIKE EKBE-EBELP,
        BEWTP LIKE EKBE-BEWTP,  "PO history category

        GJAHR LIKE EKBE-GJAHR,
        BELNR LIKE EKBE-BELNR,
        BUDAT LIKE EKBE-BUDAT,
        CPUDT LIKE EKBE-CPUDT,
        SHKZG LIKE EKBE-SHKZG,
        MENGE LIKE EKBE-MENGE,
        DMBTR LIKE EKBE-DMBTR,
        REEWR LIKE EKBE-REEWR,   "Inv.Value

        LFGJA LIKE EKBE-LFGJA,
        LFBNR LIKE EKBE-LFBNR,

        XBLNR LIKE EKBE-XBLNR,

        BSART LIKE EKKO-BSART,   "PO type
        BSTYP LIKE EKKO-BSTYP,   "PO category
        EKGRP LIKE EKKO-EKGRP,
        LIFNR LIKE EKKO-LIFNR,

        PEINH LIKE EKPO-PEINH,
        UEBTK LIKE EKPO-UEBTK,
        ELIKZ LIKE EKPO-ELIKZ,
        EREKZ LIKE EKPO-EREKZ,
        LOEKZ LIKE EKPO-LOEKZ,

        MTART LIKE MARA-MTART,
        PROFL LIKE MARA-PROFL,
        MAKTX LIKE MAKT-MAKTX,
        INFNR LIKE EINA-INFNR,

        REFDT     LIKE EKBE-BUDAT,
        YYYYMM(6) TYPE C,
        ZVBELN    LIKE LIKP-VBELN,
        ASN       LIKE LIKP-BORGR_GRP,
        ZBELNR    LIKE BSIS-BELNR,
        DUEDT     LIKE BSIS-ZFBDT,
        CLRDT     LIKE BSIS-AUGDT,
        AUGBL     LIKE BSIS-AUGBL,
        BLART     LIKE BSIS-BLART,

        ZMENGE LIKE EKBE-MENGE,  "sign
        ZDMBTR LIKE EKBE-DMBTR,  "sign
        IVPRC  LIKE EKBE-DMBTR,  "IV price

        SAPRC  LIKE EKBE-DMBTR,  "SA $
        SAUNT  LIKE KONP-KPEIN,  "SA unit
        SAVAL  LIKE EKBE-DMBTR,  "SA value
        SANO(1) TYPE C,          "No SA price

        IFPRC  LIKE EKBE-DMBTR,  "info $
        IFUNT  LIKE KONP-KPEIN,  "info unit
        IFVAL  LIKE EKBE-DMBTR,  "info value

        DIFFA  LIKE EKBE-DMBTR,  "SA-IV
        DIFFB  LIKE EKBE-DMBTR,  "Info-SA
END OF ITAB.
DATA:       C_ID              LIKE SY-REPID        VALUE 'SUBMFE'.
DATA: BEGIN OF IT_MKPF OCCURS 0,
         MBLNR  TYPE MBLNR,
         MJAHR  TYPE MJAHR,
         XBLNR  TYPE XBLNR,
         BUDAT  TYPE BUDAT,
      END OF IT_MKPF.

DATA: BEGIN OF IT_RBKP OCCURS 0,
         BELNR  TYPE RE_BELNR,
         GJAHR  TYPE GJAHR,
         LIFNR  TYPE LIFNR,
         EMPFB  TYPE EMPFB,
      END OF IT_RBKP.

DATA: BEGIN OF IT_BKPF OCCURS 0,
         AWKEY  TYPE AWKEY,
         BELNR  TYPE RE_BELNR,
         BLART  TYPE BLART,
      END OF IT_BKPF.

DATA: BEGIN OF GT_LFA1 OCCURS 0,
         LIFNR  TYPE LIFNR,
         LAND1  TYPE LAND1,
      END OF GT_LFA1.
RANGES: R_LFA1_US FOR LFA1-LIFNR,
        R_LFA1_FR FOR LFA1-LIFNR.

DATA: ALV_TAB LIKE ITAB OCCURS 0 WITH HEADER LINE.

RANGES: R_BEWTP FOR EKBE-BEWTP.

*data: begin of i_eina occurs 0,
*         INFNR  like eina-infnr,
*         MATNR  like eina-matnr,
*      end of i_eina.


*--- ALV
TYPE-POOLS: SLIS.
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER,
      X_LAYOUT TYPE DISVARIANT,
      L_VARIANT    TYPE DISVARIANT,  "Display Variant
      L_LAYOUT     TYPE SLIS_LAYOUT_ALV.  "List layout specifications


DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV,
      G_REPID     LIKE SY-REPID.
*---- ALV

*&---------------------------------------------------------------------*
REFRESH R_BEWTP. CLEAR R_BEWTP.
DESCRIBE TABLE S_BEWTP LINES SY-INDEX.
IF SY-INDEX > 0.
  R_BEWTP[] = S_BEWTP[].
ELSE.
  R_BEWTP-OPTION = 'EQ'.  R_BEWTP-SIGN = 'I'.
  IF P_REVAL = 'X'.
    R_BEWTP-LOW    = 'W'.  APPEND R_BEWTP.
    R_BEWTP-LOW    = 'N'.  APPEND R_BEWTP.
  ELSE.
    R_BEWTP-LOW    = 'Q'.  APPEND R_BEWTP.
    R_BEWTP-LOW    = 'R'.  APPEND R_BEWTP.
    R_BEWTP-LOW    = 'W'.  APPEND R_BEWTP.
    R_BEWTP-LOW    = 'N'.  APPEND R_BEWTP.
  ENDIF.
ENDIF.
*------------------------------------------------------------*
*  At Selection-Screen
*-------------------------------------------------------------*
CLEAR X_LAYOUT.
X_LAYOUT-REPORT = SY-REPID.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT,
        NOF4 TYPE C.

  CLEAR NOF4.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_VARI'.
      IF SCREEN-INPUT = 0.
        NOF4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.
  IF SY-SUBRC = 0 AND NOF4 EQ SPACE.
    P_VARI = RS_VARIANT-VARIANT.

  ENDIF.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SELECT_VENDORS.

  IF P_CURR = 'X'.
    PERFORM SELECT_CURRENT_HISTORY.
    IF SY-DBCNT > 5000.
      __POPUP '' 'Too many data selected. Do you want to proceed?' ''.
      IF GV_ANSWER = 'N'. LEAVE PROGRAM.  ENDIF.
    ENDIF.
  ENDIF.
  IF P_PAST = 'X'.
    PERFORM SELECT_PAST_HISTORY.
    IF SY-DBCNT > 5000.
      __POPUP '' 'Too many data selected. Do you want to proceed?' ''.
      IF GV_ANSWER = 'N'. LEAVE PROGRAM.  ENDIF.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE ITAB LINES SY-TABIX.
  IF SY-TABIX = 0.
    MESSAGE S000 WITH 'No record found !'.
  ELSE.
    PERFORM SELECT_MKPF_REF.
    PERFORM SELECT_BKPF_REF.
    PERFORM GET_DETAIL_INFO.
** do not have to display data.
    IF P_ALV IS INITIAL.
    ELSE.
      PERFORM PREPARE_ALV.
      PERFORM SHOW_ALV.
    ENDIF.
** end of change
  ENDIF.
*----------------------------------------------------------------------*
* End-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

*  IF P_DIFF = 'X'.
*    DELETE ALV_TAB WHERE DIFFA = 0 AND DIFFB = 0.
*  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  select_current_history
*&---------------------------------------------------------------------*
FORM SELECT_CURRENT_HISTORY.
  DESCRIBE TABLE R_LFA1_US LINES SY-TABIX.
  IF SY-TABIX > 0.
    SELECT HIST~BELNR HIST~BEWTP HIST~BUDAT HIST~CPUDT HIST~DMBTR
           HIST~EBELN HIST~EBELP HIST~GJAHR HIST~LFBNR HIST~LFGJA
           HIST~MATNR HIST~MENGE HIST~SHKZG HIST~REEWR
           HIST~XBLNR
           EKKO~BSART EKKO~BSTYP EKKO~EKGRP EKKO~LIFNR
           EKPO~PEINH EKPO~UEBTK EKPO~ELIKZ EKPO~EREKZ EKPO~LOEKZ
           MARA~MTART MARA~PROFL
           MAKT~MAKTX
           EINA~INFNR
    INTO CORRESPONDING FIELDS OF TABLE ITAB
    FROM ( EKBE  AS HIST
           INNER JOIN MARA
           ON MARA~MATNR = HIST~MATNR
           INNER JOIN EKKO
           ON EKKO~EBELN = HIST~EBELN
           INNER JOIN EKPO
           ON EKPO~EBELN = HIST~EBELN
           AND EKPO~EBELP = HIST~EBELP
           INNER JOIN EINA
           ON  EINA~MATNR = EKPO~MATNR
           AND EINA~LIFNR = EKKO~LIFNR
           INNER JOIN MAKT
           ON MAKT~MATNR = HIST~MATNR
* UD1K941198 - by IG.MOON 08/01/2007 {
           INNER JOIN MKPF AS C
           ON  C~MBLNR = HIST~LFBNR
           AND C~MJAHR = HIST~LFGJA
* }
           )
           WHERE HIST~EBELN IN S_EBELN
             AND HIST~EBELP IN S_EBELP
             AND HIST~MATNR IN S_MATNR
             AND HIST~BEWTP IN R_BEWTP
             AND HIST~BUDAT IN S_BUDAT
             AND HIST~CPUDT IN S_CPUDT
             AND HIST~BELNR IN S_BELNR
             AND HIST~BELNR <> SPACE
             AND HIST~GJAHR IN S_GJAHR
             AND HIST~LFGJA IN S_LFGJA
             AND HIST~LFBNR IN S_LFBNR
             AND HIST~LFBNR IN S_LFBNR
             AND MARA~MATKL IN S_MATKL
             AND MARA~MTART IN S_MTART
             AND MARA~PROFL IN S_PROFL
             AND EKKO~BSART IN S_BSART
             AND EKKO~EKGRP IN S_EKGRP
             AND EKKO~LIFNR IN R_LFA1_US  "S_LIFNR
             AND EKKO~BUKRS = P_BUKRS
             AND MAKT~SPRAS = SY-LANGU
* UD1K941198 - by IG.MOON 08/01/2007 {
             AND C~BUDAT    IN S_REFDT.
* }

  ENDIF.

  DESCRIBE TABLE R_LFA1_FR LINES SY-TABIX.
  IF SY-TABIX > 0.
    SELECT HIST~BELNR HIST~BEWTP HIST~BUDAT HIST~CPUDT HIST~DMBTR
           HIST~EBELN HIST~EBELP HIST~GJAHR HIST~LFBNR HIST~LFGJA
           HIST~MATNR HIST~MENGE HIST~SHKZG HIST~REEWR
           HIST~XBLNR
           EKKO~BSART EKKO~BSTYP EKKO~EKGRP EKKO~LIFNR
           EKPO~PEINH EKPO~UEBTK EKPO~ELIKZ EKPO~EREKZ EKPO~LOEKZ
           MARA~MTART MARA~PROFL
           MAKT~MAKTX
           EINA~INFNR
    APPENDING CORRESPONDING FIELDS OF TABLE ITAB
    FROM ( EKBE  AS HIST
           INNER JOIN MARA
           ON MARA~MATNR = HIST~MATNR
           INNER JOIN EKKO
           ON EKKO~EBELN = HIST~EBELN
           INNER JOIN EKPO
           ON EKPO~EBELN = HIST~EBELN
           AND EKPO~EBELP = HIST~EBELP
           INNER JOIN EINA
           ON  EINA~MATNR = EKPO~MATNR
           AND EINA~LIFNR = EKKO~LIFNR
           INNER JOIN MAKT
           ON MAKT~MATNR = HIST~MATNR
           )
           WHERE HIST~EBELN IN S_EBELN
             AND HIST~EBELP IN S_EBELP
             AND HIST~MATNR IN S_MATNR
             AND HIST~BEWTP IN R_BEWTP
             AND HIST~BUDAT IN S_BUDAT
             AND HIST~CPUDT IN S_CPUDT
             AND HIST~BELNR IN S_BELNR
             AND HIST~BELNR <> SPACE
             AND HIST~GJAHR IN S_GJAHR
             AND HIST~LFGJA IN S_LFGJA
             AND HIST~LFBNR IN S_LFBNR
             AND HIST~LFBNR IN S_LFBNR
             AND MARA~MATKL IN S_MATKL
             AND MARA~MTART IN S_MTART
             AND MARA~PROFL IN S_PROFL
             AND EKKO~BSART IN S_BSART
             AND EKKO~EKGRP IN S_EKGRP
             AND EKKO~LIFNR IN R_LFA1_FR  "S_LIFNR
             AND EKKO~BUKRS = P_BUKRS
             AND MAKT~SPRAS = SY-LANGU.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  select_past_history
*&---------------------------------------------------------------------*
FORM SELECT_PAST_HISTORY.
  DESCRIBE TABLE R_LFA1_US LINES SY-TABIX.
  IF SY-TABIX > 0.
    SELECT HIST~BELNR HIST~BEWTP HIST~BUDAT HIST~CPUDT HIST~DMBTR
           HIST~EBELN HIST~EBELP HIST~GJAHR HIST~LFBNR HIST~LFGJA
           HIST~MATNR HIST~MENGE HIST~SHKZG HIST~REEWR
           HIST~XBLNR
           EKKO~BSART EKKO~BSTYP EKKO~EKGRP EKKO~LIFNR
           EKPO~PEINH EKPO~UEBTK EKPO~ELIKZ EKPO~EREKZ EKPO~LOEKZ
           MARA~MTART MARA~PROFL
           MAKT~MAKTX
           EINA~INFNR
    APPENDING CORRESPONDING FIELDS OF TABLE ITAB
    FROM ( EKBEH  AS HIST
           INNER JOIN MARA
           ON MARA~MATNR = HIST~MATNR
           INNER JOIN EKKO
           ON EKKO~EBELN = HIST~EBELN
           INNER JOIN EKPO
           ON EKPO~EBELN = HIST~EBELN
           AND EKPO~EBELP = HIST~EBELP
           INNER JOIN EINA
           ON  EINA~MATNR = EKPO~MATNR
           AND EINA~LIFNR = EKKO~LIFNR
           INNER JOIN MAKT
           ON MAKT~MATNR = HIST~MATNR
* UD1K941198 - by IG.MOON 08/01/2007 {
           INNER JOIN MKPF AS C
           ON  C~MBLNR = HIST~LFBNR
           AND C~MJAHR = HIST~LFGJA
* }
           )
           WHERE HIST~EBELN IN S_EBELN
             AND HIST~EBELP IN S_EBELP
             AND HIST~MATNR IN S_MATNR
             AND HIST~BEWTP IN R_BEWTP
             AND HIST~BUDAT IN S_BUDAT
             AND HIST~CPUDT IN S_CPUDT
             AND HIST~BELNR IN S_BELNR
             AND HIST~BELNR <> SPACE
             AND HIST~GJAHR IN S_GJAHR
             AND HIST~LFGJA IN S_LFGJA
             AND HIST~LFBNR IN S_LFBNR
             AND HIST~LFBNR IN S_LFBNR
             AND MARA~MATKL IN S_MATKL
             AND MARA~MTART IN S_MTART
             AND MARA~PROFL IN S_PROFL
             AND EKKO~BSART IN S_BSART
             AND EKKO~EKGRP IN S_EKGRP
             AND EKKO~LIFNR IN R_LFA1_US  "S_LIFNR
             AND EKKO~BUKRS = P_BUKRS
             AND MAKT~SPRAS = SY-LANGU
* UD1K941198 - by IG.MOON 08/01/2007 {
             AND C~BUDAT    IN S_REFDT.
* }
  ENDIF.

  DESCRIBE TABLE R_LFA1_FR LINES SY-TABIX.
  IF SY-TABIX > 0.
    SELECT HIST~BELNR HIST~BEWTP HIST~BUDAT HIST~CPUDT HIST~DMBTR
           HIST~EBELN HIST~EBELP HIST~GJAHR HIST~LFBNR HIST~LFGJA
           HIST~MATNR HIST~MENGE HIST~SHKZG HIST~REEWR
           HIST~XBLNR
           EKKO~BSART EKKO~BSTYP EKKO~EKGRP EKKO~LIFNR
           EKPO~PEINH EKPO~UEBTK EKPO~ELIKZ EKPO~EREKZ EKPO~LOEKZ
           MARA~MTART MARA~PROFL
           MAKT~MAKTX
           EINA~INFNR
    APPENDING CORRESPONDING FIELDS OF TABLE ITAB
    FROM ( EKBEH  AS HIST
           INNER JOIN MARA
           ON MARA~MATNR = HIST~MATNR
           INNER JOIN EKKO
           ON EKKO~EBELN = HIST~EBELN
           INNER JOIN EKPO
           ON EKPO~EBELN = HIST~EBELN
           AND EKPO~EBELP = HIST~EBELP
           INNER JOIN EINA
           ON  EINA~MATNR = EKPO~MATNR
           AND EINA~LIFNR = EKKO~LIFNR
           INNER JOIN MAKT
           ON MAKT~MATNR = HIST~MATNR
* UD1K941198 - by IG.MOON 08/01/2007 {
           INNER JOIN MKPF AS C
           ON  C~MBLNR = HIST~LFBNR
           AND C~MJAHR = HIST~LFGJA
* }
           )
           WHERE HIST~EBELN IN S_EBELN
             AND HIST~EBELP IN S_EBELP
             AND HIST~MATNR IN S_MATNR
             AND HIST~BEWTP IN R_BEWTP
             AND HIST~BUDAT IN S_BUDAT
             AND HIST~CPUDT IN S_CPUDT
             AND HIST~BELNR IN S_BELNR
             AND HIST~BELNR <> SPACE
             AND HIST~GJAHR IN S_GJAHR
             AND HIST~LFGJA IN S_LFGJA
             AND HIST~LFBNR IN S_LFBNR
             AND HIST~LFBNR IN S_LFBNR
             AND MARA~MATKL IN S_MATKL
             AND MARA~MTART IN S_MTART
             AND MARA~PROFL IN S_PROFL
             AND EKKO~BSART IN S_BSART
             AND EKKO~EKGRP IN S_EKGRP
             AND EKKO~LIFNR IN R_LFA1_FR  "S_LIFNR
             AND EKKO~BUKRS = P_BUKRS
             AND MAKT~SPRAS = SY-LANGU
* UD1K941198 - by IG.MOON 08/01/2007 {
             AND C~BUDAT    IN S_REFDT.
* }
  ENDIF.
ENDFORM.                    " select_past_history
*&---------------------------------------------------------------------*
*&      Form  get_asn
*&---------------------------------------------------------------------*
FORM GET_ASN.

  DATA: LS_BORGR     LIKE  LIKP-BORGR_GRP.

  CLEAR IT_MKPF.
  READ TABLE IT_MKPF WITH KEY MBLNR = ITAB-LFBNR
                              MJAHR = ITAB-LFGJA
                     BINARY SEARCH.

  IF SY-SUBRC <> 0.
    ITAB-REFDT = ITAB-BUDAT.
  ELSE.
    ITAB-REFDT = IT_MKPF-BUDAT.

* get asn
    IF IT_MKPF-XBLNR(3) = 'JIS' OR IT_MKPF-XBLNR(3) = SPACE.
    ELSE.
      ITAB-ZVBELN  = IT_MKPF-XBLNR(10).
      SELECT SINGLE BORGR_GRP INTO LS_BORGR
        FROM LIKP  WHERE VBELN = ITAB-ZVBELN.
      ITAB-ASN = LS_BORGR.
    ENDIF.
  ENDIF.

  ITAB-YYYYMM = ITAB-REFDT(6).

ENDFORM.                    " get_asn
*&---------------------------------------------------------------------*
*&      Form  get_ref_date
*&---------------------------------------------------------------------*
*FORM GET_REF_DATE.
*  ITAB-REFDT = ITAB-BUDAT.
*  DATA: L_DATE LIKE MKPF-BUDAT.
*  SELECT SINGLE BUDAT INTO L_DATE
*      FROM MKPF
*      WHERE MJAHR = ITAB-LFGJA
*        AND MBLNR = ITAB-LFBNR.
*  IF SY-SUBRC = 0.
*    ITAB-REFDT = L_DATE.
*  ENDIF.
*  ITAB-YYYYMM = ITAB-REFDT(6).
*ENDFORM.                    " get_ref_date
*&---------------------------------------------------------------------*
*&      Form  get_due_date
*&---------------------------------------------------------------------*
FORM GET_DUE_DATE.

  CHECK P_DUE = 'X'.

  DATA: L_BELNR LIKE BKPF-BELNR,
        L_AWKEY LIKE BKPF-AWKEY.

  CONCATENATE ITAB-BELNR ITAB-GJAHR INTO L_AWKEY.
  READ TABLE IT_BKPF WITH KEY AWKEY = L_AWKEY BINARY SEARCH.
  ITAB-BLART = IT_BKPF-BLART.

  SELECT SINGLE ZFBDT AUGDT AUGBL
    INTO (ITAB-DUEDT, ITAB-CLRDT, ITAB-AUGBL)
    FROM BSAK
    WHERE BUKRS = P_BUKRS
      AND LIFNR = ITAB-LIFNR
      AND BELNR = IT_BKPF-BELNR
      AND GJAHR = ITAB-GJAHR.

  IF SY-SUBRC <> 0.
    DATA: L_ZBD1T LIKE BSIK-ZBD1T.
    SELECT SINGLE ZFBDT ZBD1T
     INTO (ITAB-DUEDT, L_ZBD1T)
     FROM BSIK
     WHERE BUKRS = P_BUKRS
       AND LIFNR = ITAB-LIFNR
      AND BELNR = IT_BKPF-BELNR
       AND GJAHR = ITAB-GJAHR.
    ITAB-DUEDT = ITAB-DUEDT + L_ZBD1T.
  ENDIF.

ENDFORM.                    " get_due_date
*&---------------------------------------------------------------------*
*&      Form  get_invoice_party
*&---------------------------------------------------------------------*
FORM GET_INVOICE_PARTY.

  READ TABLE IT_RBKP WITH KEY BELNR = ITAB-BELNR
                              GJAHR = ITAB-GJAHR
                     BINARY SEARCH.

  IF IT_RBKP-EMPFB IS INITIAL.
    ITAB-LIFNR = IT_RBKP-LIFNR.
  ELSE.
    ITAB-LIFNR = IT_RBKP-EMPFB.
  ENDIF.

ENDFORM.                    " get_invoice_party
*&---------------------------------------------------------------------*
*&      Form  get_info_price
*&---------------------------------------------------------------------*
FORM GET_INFO_PRICE.

*  TABLES: konp.
  DATA: BEGIN OF IT_KNUMH OCCURS 0,
          KNUMH  LIKE KONH-KNUMH,
          DATAB  LIKE KONH-DATAB, "Valid-from date
          DATBI  LIKE KONH-DATBI,
          LIFNR  LIKE LFA1-LIFNR,
          KSTBMT TYPE KSTBMT,                               "3 digit
          KBETR  LIKE KONP-KBETR, "rate
          KPEIN  LIKE KONP-KPEIN, "pricing unit
          EKORG  LIKE EINE-EKORG,
          INFNR  LIKE EINA-INFNR,
        END   OF IT_KNUMH.

  REFRESH IT_KNUMH.
  CLEAR   IT_KNUMH.

  SELECT KNUMH DATAB LIFNR EKORG
    INTO CORRESPONDING FIELDS OF TABLE IT_KNUMH
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  ITAB-MATNR
     AND LIFNR =  ITAB-LIFNR
*      and ekorg =  c_ekorg
     AND ESOKZ =  '0'
     AND DATBI >=  ITAB-REFDT   "Valid To
     AND DATAB <=  ITAB-REFDT.  "Valid from
  CHECK SY-SUBRC = 0.

  READ TABLE IT_KNUMH INDEX 1.
  SELECT SINGLE * FROM KONP
   WHERE KNUMH = IT_KNUMH-KNUMH
     AND KAPPL = 'M'
     AND KSCHL = 'PB00'.
  IF SY-SUBRC = 0.
    ITAB-IFPRC = KONP-KBETR.
    ITAB-IFUNT = KONP-KPEIN.
  ENDIF.

ENDFORM.                    " get_info_price
*&---------------------------------------------------------------------*
*&      Form  get_sa_price
*&---------------------------------------------------------------------*
FORM GET_SA_PRICE.
  DATA: L_KNUMH LIKE A018-KNUMH.

*GR date, Get SA price
  SELECT SINGLE KNUMH INTO L_KNUMH
    FROM A016
     WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND EVRTN = ITAB-EBELN
     AND EVRTP = ITAB-EBELP
     AND DATBI >= ITAB-REFDT
     AND DATAB <= ITAB-REFDT.

  IF SY-SUBRC = 0.
* KONH, KONP
    SELECT SINGLE KBETR KPEIN INTO (ITAB-SAPRC, ITAB-SAUNT)
       FROM ZVMM_INFO_CONDI
       WHERE KNUMH = L_KNUMH
         AND LOEVM_KO = ' '.

* if normal PO, get from order history
  ELSE.
    TABLES: EIPA, EINA.
*    SELECT SINGLE * FROM eina
*       WHERE lifnr = itab-lifnr
*         AND matnr = itab-matnr.
    SELECT SINGLE PREIS PEINH INTO (ITAB-SAPRC, ITAB-SAUNT)
       FROM EIPA
       WHERE INFNR = ITAB-INFNR
         AND EBELN = ITAB-EBELN
         AND EBELP = ITAB-EBELP.
    ITAB-SANO = 'X'.
  ENDIF.

ENDFORM.                    " get_sa_price
*&---------------------------------------------------------------------*
*&      Form  collect_alv_tab
*&---------------------------------------------------------------------*
FORM COLLECT_ALV_TAB.

  ALV_TAB-LIFNR   = ITAB-LIFNR.
  ALV_TAB-MATNR   = ITAB-MATNR.
  ALV_TAB-EBELN   = ITAB-EBELN.
  ALV_TAB-EBELP   = ITAB-EBELP.
  ALV_TAB-ZMENGE  = ITAB-ZMENGE.
  ALV_TAB-ZDMBTR  = ITAB-ZDMBTR.
  ALV_TAB-SAVAL   = ITAB-SAVAL.
  ALV_TAB-IFVAL   = ITAB-IFVAL.
  ALV_TAB-DIFFA   = ITAB-DIFFA.
  ALV_TAB-DIFFB   = ITAB-DIFFB.

  COLLECT ALV_TAB.

ENDFORM.                    " collect_alv_tab
*&---------------------------------------------------------------------*
*&      Form  get_detail_info
*&---------------------------------------------------------------------*
FORM GET_DETAIL_INFO.
  DATA: L_IDX LIKE SY-TABIX.
  REFRESH: ALV_TAB.

  LOOP AT ITAB.
    L_IDX = SY-TABIX.

    PERFORM GET_DUE_DATE.
    IF NOT P_AUGDT IS INITIAL
    AND NOT ITAB-CLRDT IS INITIAL
    AND P_AUGDT <> ITAB-CLRDT.
      CONTINUE.
    ENDIF.

* UD1K941198 - by IG.MOON 08/01/2007 {
*    PERFORM get_ref_date.
* }

    PERFORM GET_ASN.
    CHECK ITAB-REFDT IN S_REFDT.

    PERFORM GET_INVOICE_PARTY.


    ITAB-ZMENGE = ITAB-MENGE.
    ITAB-ZDMBTR = ITAB-REEWR.
*---change sign of amount/qty
    IF ITAB-SHKZG = 'H'.
      ITAB-ZDMBTR = - ITAB-ZDMBTR.
*...qty is positive in case of reval.
      IF ITAB-BEWTP NA 'WN'.
        ITAB-ZMENGE = - ITAB-ZMENGE.
      ENDIF.
    ENDIF.

* IV unit$  (converted to SA unit)
    IF ITAB-MENGE <> 0.
      ITAB-IVPRC = ITAB-PEINH * ITAB-ZDMBTR / ITAB-MENGE.
    ENDIF.


*...Reval/Subseq.
    IF ITAB-BEWTP CA 'WN'.
      CLEAR ITAB-ZMENGE.
*-----ANDY
*     PERFORM GET_SA_PRICE.
      CLEAR: ITAB-SAPRC.

*...Normal IV
    ELSE.
      IF P_DIFF = 'X'.
        PERFORM GET_INFO_PRICE.
        PERFORM GET_SA_PRICE.
        ITAB-IFVAL = ITAB-ZMENGE * ITAB-IFPRC / ITAB-IFUNT.
        ITAB-SAVAL = ITAB-ZMENGE * ITAB-SAPRC / ITAB-SAUNT.

        IF ITAB-SHKZG = 'H'.
          ITAB-IFPRC = - ITAB-IFPRC.
          ITAB-SAPRC = - ITAB-SAPRC.
        ENDIF.

* Calc. Diff. (Reval=Diff,...)
        ITAB-DIFFA = ITAB-ZDMBTR - ITAB-SAVAL.
        ITAB-DIFFB = ITAB-SAVAL  - ITAB-IFVAL.
      ENDIF.
    ENDIF.

    IF P_SUMM = 'X'.
      PERFORM COLLECT_ALV_TAB.
    ELSE.
      APPEND ITAB  TO ALV_TAB.
    ENDIF.

    CLEAR ITAB.
  ENDLOOP.

  EXPORT ALV_TAB TO MEMORY ID C_ID.
*    export itab to MEMORY ID C_ID.
ENDFORM.                    " get_detail_info
*&---------------------------------------------------------------------*
*&      Form  select_mkpf_ref
*&---------------------------------------------------------------------*
FORM SELECT_MKPF_REF.
  DATA: BEGIN OF LT_ITAB OCCURS 0,
          LFBNR  TYPE LFBNR,
          LFGJA  TYPE LFGJA,
        END OF LT_ITAB.
  DATA: BEGIN OF LT_ITAB2 OCCURS 0,
          BELNR  TYPE LFBNR,
          GJAHR  TYPE LFGJA,
        END OF LT_ITAB2.

  LOOP AT ITAB.
    LT_ITAB-LFBNR = ITAB-LFBNR.
    LT_ITAB-LFGJA = ITAB-LFGJA.
    APPEND LT_ITAB.
    LT_ITAB2-GJAHR = ITAB-GJAHR.
    LT_ITAB2-BELNR = ITAB-BELNR.
    APPEND LT_ITAB2.
  ENDLOOP.
  SORT LT_ITAB  BY LFGJA LFBNR.
  SORT LT_ITAB2 BY GJAHR BELNR.
  DELETE ADJACENT DUPLICATES FROM LT_ITAB.
  DELETE ADJACENT DUPLICATES FROM LT_ITAB2.

  SELECT MBLNR MJAHR XBLNR BUDAT
    INTO TABLE IT_MKPF
    FROM MKPF
    FOR ALL ENTRIES IN LT_ITAB
    WHERE MBLNR = LT_ITAB-LFBNR
      AND MJAHR = LT_ITAB-LFGJA.

  SORT IT_MKPF BY MBLNR MJAHR.


  SELECT BELNR GJAHR LIFNR EMPFB
    INTO TABLE IT_RBKP
    FROM RBKP
    FOR ALL ENTRIES IN LT_ITAB2
    WHERE BELNR = LT_ITAB2-BELNR
      AND GJAHR = LT_ITAB2-GJAHR.
  SORT IT_RBKP BY BELNR GJAHR.

ENDFORM.                    " select_mkpf_ref
*&---------------------------------------------------------------------*
*&      Form  prepare_alv
*&---------------------------------------------------------------------*
FORM PREPARE_ALV.
  IF P_SHORT = 'X'.
    PERFORM FIELD_SETTING TABLES GT_FIELDCAT USING :
   'BEWTP'     'Category'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'YYYYMM'    'Ref.YYMM'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MATNR'     'Material'       '18'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MAKTX'     'Mat.Desc'       '25'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'REFDT'     'Ref.Date'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BUDAT'     'PstDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ASN'       'ASN#'           '30'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'GJAHR'     'InvYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR'     'InvDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELN'     'PO/SA'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELP'     'Itm'            '06'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZMENGE'    'Quantity'       '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'ZDMBTR'    'Invoice Amt'    '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IVPRC'     'IV Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'Price unit'     '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'DUEDT'     'DueDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CLRDT'     'ClrDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFBNR'     'Ref.Doc#'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.

  ELSE.
    PERFORM FIELD_SETTING TABLES GT_FIELDCAT USING :
   'MATNR'     'Material'       '18'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELN'     'PO/SA'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELP'     'Itm'            '06'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BEWTP'     'Category'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'GJAHR'     'InvYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR'     'InvDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BUDAT'     'PstDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CPUDT'     'CPUDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'SHKZG'     'D/C'            '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*'MENGE'     'Qty'            '14'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*'DMBTR'     'Amt'            '16'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFGJA'     'RefYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFBNR'     'RefDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BSART'     'PO Type'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BSTYP'     'PO Cat'         '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EKGRP'     'PO grp'         '03'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LIFNR'     'Vendor'         '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'PrcUnit'        '03'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*INFNR'     'Inf No'         '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'REFDT'     'Ref.Date'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'YYYYMM'    'Ref.YYMM'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZVBELN'    'Inbound'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ASN'       'ASN#'           '30'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZMENGE'    'Quantity'       '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'ZDMBTR'    'Invoice Amt'    '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IVPRC'     'IV Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'Price unit'     '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'UEBTK'     'Unlimited GR'   '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ELIKZ'     'Delivery compl' '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EREKZ'     'Final IV'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LOEKZ'     'Del'            '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MTART'     'MatType'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'PROFL'     'Profile'        '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MAKTX'     'Mat.Desc'       '25'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.


    IF P_DIFF = 'X'.
      PERFORM FIELD_SETTING TABLES GT_FIELDCAT USING :
   'SAPRC'     'SA Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'SAUNT'     'SA UNIT'        '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'SAVAL'     'SA VALUE'       '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IFPRC'     'INFO Price$'    '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IFUNT'     'INFO UNIT'      '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IFVAL'     'INFO VALUE'     '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'DIFFA'     'IV-SA'          '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'DIFFB'     'SA-INFO'        '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'SANO'      'NO SA Price'    '01'  ' '  'R'  ' '  ' '  ' '  ' '  ' '.
    ENDIF.
    IF P_DUE = 'X'.
      PERFORM FIELD_SETTING TABLES GT_FIELDCAT USING :
   'BLART'     'Doc.Typ'        '02'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZBELNR'    'Acct.Doc'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'DUEDT'     'DueDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CLRDT'     'ClrDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'AUGBL'     'ClrDoc'         '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.
    ENDIF.
  ENDIF.

ENDFORM.                    " prepare_alv
*&---------------------------------------------------------------------*
*&      Form  show_alv
*&---------------------------------------------------------------------*
FORM SHOW_ALV.

  G_REPID = SY-REPID.
  L_VARIANT-VARIANT = P_VARI.
  L_VARIANT-REPORT  = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = G_REPID
            IT_FIELDCAT        = GT_FIELDCAT
            IS_VARIANT         = L_VARIANT
            I_SAVE             = 'A'
       TABLES
            T_OUTTAB           = ALV_TAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.

ENDFORM.                    " show_alv
*&---------------------------------------------------------------------*
*&      Form  SELECT_BKPF_REF
*&---------------------------------------------------------------------*
FORM SELECT_BKPF_REF.

  CHECK P_DUE = 'X'.

  DATA: BEGIN OF LT_ITAB OCCURS 0,
          AWKEY  TYPE AWKEY,
        END OF LT_ITAB.

  LOOP AT ITAB.
    CONCATENATE ITAB-BELNR ITAB-GJAHR INTO LT_ITAB-AWKEY.
    APPEND LT_ITAB.
  ENDLOOP.
  SORT LT_ITAB  BY AWKEY.
  DELETE ADJACENT DUPLICATES FROM LT_ITAB.

  SELECT AWKEY BELNR BLART
    INTO TABLE IT_BKPF
    FROM BKPF
    FOR ALL ENTRIES IN LT_ITAB
    WHERE AWKEY = LT_ITAB-AWKEY
      AND BUKRS = P_BUKRS.

  SORT IT_BKPF BY AWKEY.

ENDFORM.                    " SELECT_BKPF_REF
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM FIELD_SETTING TABLES         P_FIELDCAT_T LIKE GT_FIELDCAT
                   USING          P_FIELDNAME       " FIELD name
                                  P_TITLE           " field titlw
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_ROUND           "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  P_DOSUM           " make sum
                                  .

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME  = P_FIELDNAME.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  LS_FIELDCAT-SELTEXT_L  = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN  = P_OUTPUTLEN.
  LS_FIELDCAT-KEY        = P_KEY.
  LS_FIELDCAT-JUST       = P_JUST.
  LS_FIELDCAT-EDIT       = ''.   "p_edit.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-DECIMALS_OUT   = P_ROUND.
*  ls_fieldcat-cfieldname = p_cfield.
  LS_FIELDCAT-CURRENCY   = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
  LS_FIELDCAT-DO_SUM     = P_DOSUM.

  APPEND LS_FIELDCAT TO P_FIELDCAT_T.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM POP_UP USING    P_TEXT P_TEXT2 P_CANC
            CHANGING P_ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TEXTLINE1      = P_TEXT
            TEXTLINE2      = P_TEXT2
            TITEL          = 'Check!'
            CANCEL_DISPLAY = P_CANC
       IMPORTING
            ANSWER         = P_ANSWER.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  select_vendors
*&---------------------------------------------------------------------*
FORM SELECT_VENDORS.
  SELECT SINGLE * FROM T001 WHERE BUKRS = P_BUKRS.

  SELECT LIFNR LAND1 INTO TABLE GT_LFA1
     FROM LFA1
     WHERE LIFNR IN S_LIFNR.

  R_LFA1_FR-SIGN   = 'I'.
  R_LFA1_FR-OPTION = 'EQ'.
  R_LFA1_US-SIGN   = 'I'.
  R_LFA1_US-OPTION = 'EQ'.

* NAFTA countries...
  LOOP AT GT_LFA1.
    CASE GT_LFA1-LAND1.
      WHEN T001-LAND1.
        R_LFA1_US-LOW = GT_LFA1-LIFNR.
        APPEND R_LFA1_US.
      WHEN 'CA' OR 'MX'.
        R_LFA1_US-LOW = GT_LFA1-LIFNR.
        APPEND R_LFA1_US.
      WHEN OTHERS.
        R_LFA1_FR-LOW = GT_LFA1-LIFNR.
        APPEND R_LFA1_FR.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " select_vendors
