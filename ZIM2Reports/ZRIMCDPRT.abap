*&---------------------------------------------------------------------*
*& Report  ZRIMCDPRT                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : Summarized FI Document Daily List                     *
*&      작성자 : Nashinho INFOLINK Ltd.                                *
*&      작성일 : 2003.12.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Summarized FI Document Daily List
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT ZRIMCDPRT         MESSAGE-ID ZIM
                         NO STANDARD PAGE HEADING
                         LINE-SIZE   130   LINE-COUNT   90.

TYPE-POOLS: SLIS.
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
CLASS CL_GUI_RESOURCES DEFINITION LOAD.

*>> Global Data Declaration.
TABLES: T001,      " Company code.
        T003T,     " Document Type Texts.
        T005,      " Countris.
        T007S,     " Tax Code Names.
        BKPF,      " Accounting Document Header.
        VBKPF,     " Document Header for Document Parking.
        ZTBKPF,    " Import System Accnt.doc.Header.
        BSEG,      " Accounting Document Segment.
        VBSEGS,    " Document Segment for G/L Accounts Document Parking.
        VBSEGK,    " Document Segment for Vendor Document Parking.
        VBSEGD,    " Document Segment for Customer Document Parking.
        VBSEGA,    " Document Segment for Assets Document Parking.
        VBSET,     " Document Segment for Taxes Document Parking.
        USR21,     " Assign User Name Address Key.
        ADCP,      " Person/ Address Assignment.
        ANLA,      " Asset Master Record Segment.
        KNA1,      " General Data in Customer Master.
        LFA1,      " Vendor Master (General Section).
        TGSBT,     " Business Area Names.
        SKAT,      " G/L Account Master Record.
        AUFK,      " Order Master Data.
        CSKS,      " Cost Center Master Data.
        CSKT,      " Cost Center Texts.
        FMFCTRT.   " Funds Center Texts.

DATA: BEGIN OF IT_BK OCCURS 100,
      BLART    LIKE  BKPF-BLART,             " Doc. Type.
      BUDAT    LIKE  BKPF-BUDAT,             " Posting Date.
      BLDAT    LIKE  BKPF-BLDAT,             " doc. date.
      CPUDT    LIKE  BKPF-CPUDT,             " Enrry data.
      USNAM    LIKE  BKPF-USNAM,             " User Name.
      PPNAM    LIKE  BKPF-PPNAM,             " Parked by.
      WAERS    LIKE  BKPF-WAERS,             " Currency key.
      TCODE    LIKE  BKPF-TCODE,             " Transaction Code.
      XBLNR    LIKE  BKPF-XBLNR,             " Reference.
      BSTAT    LIKE  BKPF-BSTAT,             " Doc. Status.
      DEPART   LIKE  ADCP-DEPARTMENT,        " Dept Name.
      XPRFG    LIKE  VBKPF-XPRFG,            " Doc. Complete.
      STBLG    LIKE  BKPF-STBLG,             " Reverse doc no.
      STJAH    LIKE  BKPF-STJAH,             " Reverse year.
      STGRD    LIKE  BKPF-STGRD,             " Reverse reason.
      BUKRS    LIKE  BKPF-BUKRS,             " Company Code.
      GJAHR    LIKE  BKPF-GJAHR,             " Fiscal Year.
      BELNR    LIKE  BKPF-BELNR,             " Doc. No.
END OF IT_BK.

DATA : BEGIN OF IT_T074U OCCURS 0,
          UMSKZ  LIKE T074U-UMSKZ, "Special G/L Indicator
       END OF IT_T074U.

DATA : BEGIN OF IT_LFC3 OCCURS 0.
        INCLUDE STRUCTURE LFC3.
DATA : END OF IT_LFC3.

DATA: BEGIN OF IT_BKPF OCCURS 0.
        INCLUDE STRUCTURE IT_BK.
DATA:   CHKBOX TYPE C,
        TABCOLOR     TYPE SLIS_T_SPECIALCOL_ALV,
      END OF IT_BKPF.

* BSEG : G/L Data segment
DATA: IT_BSEG TYPE TABLE OF BSEG WITH HEADER LINE.

* BSEG: additional info
DATA: BEGIN OF IT_L,
        TXT20(20),      " account short text
        ACCT_TYPE(48),  " account detail info.
        TAXCD(7),       "
        RATE(4),
        ORDNO(30),
        ASSGN1(17),
        ASSGN2(12),
        TEXT(48),
        DATE(12),
        DR  LIKE BSEG-DMBTR,
        CR  LIKE BSEG-DMBTR,
        DRT  LIKE BSEG-DMBTR, "tax
        CRT  LIKE BSEG-DMBTR, "tax
        NAME1 LIKE LFA1-NAME1,
        CODE(20),
    END OF IT_L.
* summary amount of fi doc.
DATA: BEGIN OF IT_S,
        SDR  LIKE BSEG-DMBTR,
        SCR  LIKE BSEG-DMBTR,
        SDRT LIKE BSEG-DMBTR,
        SCRT LIKE BSEG-DMBTR,
      END OF IT_S.

DATA: WA_WIDTH TYPE I VALUE 130, "Paper width
      WA_UL  LIKE SY-ULINE VALUE '-',
      WA_VL  LIKE SY-VLINE VALUE '|',
      WA_SP  LIKE SPACE    VALUE ' ',
      WA_TOTAL_PAGE_NUMBER TYPE I,
      WA_FINAL(16).

* for company code name...
DATA: WA_L_NAME1(40), WA_L_NAME2(40), WA_L_COMPANY_NAME(80).

DATA : WA_T_CNT TYPE I,
       WA_CNT   TYPE I,
       WA_FINAL_TXT(20),
       WA_DP   LIKE LFC3-SALDV,
       WA_NAME1 LIKE LFA1-NAME1.
*&--------------------------------------------------------------------&*
*& SELECTION-SCREEN                                                   &*
*&--------------------------------------------------------------------&*
SELECT-OPTIONS:
  S_BUKRS  FOR   BKPF-BUKRS DEFAULT 'H201' MEMORY ID BUK,
  S_BELNR  FOR   BKPF-BELNR,
  S_GJAHR  FOR   BKPF-GJAHR MEMORY ID GJR.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-010.
SELECT-OPTIONS:
  S_BLART  FOR   BKPF-BLART MEMORY ID BAR,
  S_BUDAT  FOR   BKPF-BUDAT.
*  S_CPUDT  FOR   BKPF-CPUDT.
SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-020.
PARAMETER   P_OWN TYPE C AS CHECKBOX.               " Own Document Only.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-030.
PARAMETER : P_PARKED AS CHECKBOX,                   " In Parked.
            P_POSTED AS CHECKBOX.                   " In Posted.
SELECTION-SCREEN END OF BLOCK B2.

*-------------------------------------------------------*
* Include for ALV
*-------------------------------------------------------*
INCLUDE: ZRIMCDPRT_ALV,
         ZRIMCDPRT_WRITE.

*-------------------------------------------------------*
AT SELECTION-SCREEN.
*-------------------------------------------------------*
  IF S_BUKRS-LOW IS INITIAL.
    MESSAGE E000(ZMFI) WITH 'Company code is not defined'.
  ENDIF.

  IF S_GJAHR-LOW IS INITIAL.
    MESSAGE E000(ZMFI) WITH 'Fiscal year is not defined'.
  ENDIF.
  IF S_BUDAT-LOW IS INITIAL.
    MESSAGE E000(ZMFI) WITH 'You have to input Posting date'.
  ENDIF.
  IF P_PARKED IS INITIAL AND P_POSTED IS INITIAL.
    MESSAGE E000(ZMFI) WITH 'You have to check in parked or in posted'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
*& Initialization.
*&---------------------------------------------------------------------*
INITIALIZATION.

  WA_VL = SY-VLINE.
  WA_UL = SY-ULINE.
  P_PARKED = 'X'.
  P_POSTED = 'X'.

  S_GJAHR-LOW = SY-DATUM(4).
  APPEND S_GJAHR.

  S_BUDAT-LOW  = SY-DATUM.
  S_BUDAT-HIGH = SY-DATUM.
  APPEND S_BUDAT.

  WA_REPID = SY-REPID.
* ==> Change Variant saving type
  WA_VAR_SAVE = 'A'.
* ==> Change first mode   GRID or LIST
  WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : GT_FIELDCAT.
  CLEAR   : GS_LAYOUT.

*&---------------------------------------------------------------------*
*& Start of Selection.
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
*& End of Selection.
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM READ_BKPF.
  IF IT_BKPF[] IS INITIAL.
    MESSAGE S000(ZMFI) WITH 'No data found'.
    EXIT.
  ENDIF.
  PERFORM MAKE_ALV_BKPF.
  PERFORM WRITE_DOCUMENTS.

*&---------------------------------------------------------------------*
*&      Form  READ BKPF
*&---------------------------------------------------------------------*
FORM READ_BKPF.
  RANGES : R_BSTAT FOR BKPF-BSTAT,
           R_USNAM FOR BKPF-USNAM,
           R_PPNAM FOR BKPF-PPNAM.

  REFRESH IT_BKPF. CLEAR IT_BKPF.
  REFRESH IT_BSEG. CLEAR IT_BSEG.

* check and read parked data
  IF P_PARKED = 'X'.
    R_BSTAT-SIGN   = 'I'.
    R_BSTAT-OPTION = 'EQ'.
    R_BSTAT-LOW    = 'V'.
    APPEND R_BSTAT.
  ENDIF.
* check and read posted data
  IF P_POSTED = 'X'.
    R_BSTAT-SIGN   = 'I'.
    R_BSTAT-OPTION = 'EQ'.
    R_BSTAT-LOW    = ' '.
    APPEND R_BSTAT.
    R_BSTAT-LOW    = 'A'.
    APPEND R_BSTAT.
    R_BSTAT-LOW    = 'B'.
    APPEND R_BSTAT.
    R_BSTAT-LOW    = 'D'.
    APPEND R_BSTAT.
    R_BSTAT-LOW    = 'S'.
    APPEND R_BSTAT.
  ENDIF.

  IF P_OWN EQ 'X'.
    R_USNAM-SIGN = 'I'.
    R_USNAM-OPTION = 'EQ'.
    R_USNAM-LOW = SY-UNAME.
    APPEND R_USNAM.
    R_PPNAM-SIGN = 'I'.
    R_PPNAM-OPTION = 'EQ'.
    R_PPNAM-LOW = SY-UNAME.
    APPEND R_PPNAM.
  ENDIF.
  SELECT S~BUKRS S~BELNR S~GJAHR S~BSTAT
         S~BUDAT S~USNAM S~PPNAM S~TCODE S~XBLNR
         S~BLDAT S~CPUDT S~BLART S~BKTXT S~WAERS
         S~STBLG S~STJAH S~STGRD
    INTO CORRESPONDING FIELDS OF TABLE IT_BK
    FROM BKPF AS S INNER JOIN ZTBKPF AS I
      ON S~BUKRS = I~BUKRS AND
         S~BELNR = I~ZFACDO AND
         S~GJAHR = I~GJAHR
   WHERE S~BUKRS IN S_BUKRS
     AND S~GJAHR IN S_GJAHR
     AND I~BELNR IN S_BELNR
     AND S~BLART IN S_BLART
     AND S~BUDAT IN S_BUDAT
*    AND BSTAT IN R_BSTAT
     AND ( S~USNAM IN R_USNAM OR S~PPNAM IN R_PPNAM ).
  LOOP AT IT_BK.
    MOVE-CORRESPONDING IT_BK TO IT_BKPF.
    APPEND IT_BKPF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_BSEG
*&---------------------------------------------------------------------*
FORM READ_BSEG.

  DATA: WA_L_COUNT TYPE I VALUE 0.

  CASE  IT_BKPF-BSTAT.
    WHEN 'V'.
      REFRESH IT_BSEG. CLEAR IT_BSEG.
      PERFORM  VBSEGS_READ_PROCESS.  "G/L Account Document Parking
      PERFORM  VBSEGK_READ_PROCESS.  "Vendor Document Parking
      PERFORM  VBSEGD_READ_PROCESS.  "Customer Document Parking
      PERFORM  VBSEGA_READ_PROCESS.  "Asset Document Parking
    WHEN OTHERS.
      PERFORM  BSEG_READ_PROCESS.
  ENDCASE.
  SORT IT_BSEG BY BUZEI.

  LOOP AT IT_BSEG.
    IF IT_BSEG-HKONT IS INITIAL.
      IT_BSEG-HKONT = IT_BSEG-SAKNR.
    ENDIF.
**--> Noted items : change hkont
    IF IT_BKPF-BSTAT = 'S'.
      SELECT SINGLE SKONT INTO IT_BSEG-HKONT FROM T074
        WHERE KTOPL = 'HNA1' AND
              KOART = IT_BSEG-KOART AND
              UMSKZ = IT_BSEG-ZUMSK AND
              HKONT = IT_BSEG-SAKNR.

    ENDIF.

    MODIFY IT_BSEG TRANSPORTING HKONT.
    ADD 1 TO WA_L_COUNT.
  ENDLOOP.

  WA_TOTAL_PAGE_NUMBER = WA_L_COUNT  / 16 + 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BSEG_READ_PROCESS
*&---------------------------------------------------------------------*
FORM BSEG_READ_PROCESS.
  REFRESH IT_BSEG. CLEAR IT_BSEG.

  SELECT *
    FROM BSEG
    INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
   WHERE BUKRS = IT_BKPF-BUKRS
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBSEGS_READ_PROCESS
*&---------------------------------------------------------------------*
FORM VBSEGS_READ_PROCESS.
  SELECT *
    FROM VBSEGS
    INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
   WHERE AUSBK = IT_BKPF-BUKRS
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR.
  LOOP AT IT_BSEG WHERE KOART EQ ' '.
    IT_BSEG-KOART = 'S'.
    MODIFY IT_BSEG TRANSPORTING KOART.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBSEGK_READ_PROCESS
*&---------------------------------------------------------------------*
FORM VBSEGK_READ_PROCESS.
  SELECT *
    FROM VBSEGK
    APPENDING CORRESPONDING FIELDS OF TABLE IT_BSEG
   WHERE AUSBK = IT_BKPF-BUKRS
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR.
  LOOP AT IT_BSEG WHERE KOART EQ ' '.
    IT_BSEG-KOART = 'K'.
    MODIFY IT_BSEG TRANSPORTING KOART.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBSEGD_READ_PROCESS
*&---------------------------------------------------------------------*
FORM VBSEGD_READ_PROCESS.
  SELECT *
    FROM VBSEGD
    APPENDING CORRESPONDING FIELDS OF TABLE IT_BSEG
   WHERE AUSBK = IT_BKPF-BUKRS
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR.
  LOOP AT IT_BSEG WHERE KOART EQ ' '.
    IT_BSEG-KOART = 'D'.
    MODIFY IT_BSEG TRANSPORTING KOART.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VBSEGA_READ_PROCESS
*&---------------------------------------------------------------------*
FORM VBSEGA_READ_PROCESS.
  SELECT *
    FROM VBSEGA
    APPENDING CORRESPONDING FIELDS OF TABLE IT_BSEG
   WHERE AUSBK = IT_BKPF-BUKRS
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR.
  LOOP AT IT_BSEG WHERE KOART EQ ' '.
    IT_BSEG-KOART = 'A'.
    MODIFY IT_BSEG TRANSPORTING KOART.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_POSITION_ID
*&---------------------------------------------------------------------*
FORM GET_POSITION_ID CHANGING P_CODE P_NAME.
  DATA: L_OBJNR LIKE IMZO-OBJNR,
        L_POSNR LIKE IMZO-POSNR,
        L_POSID LIKE IMPR-POSID.

  CONCATENATE 'OR'  IT_BSEG-AUFNR INTO L_OBJNR.
  SELECT SINGLE POSNR
           INTO L_POSNR
           FROM IMZO
          WHERE OBJNR EQ L_OBJNR.

  IF SY-SUBRC NE 0. EXIT. ENDIF.

  SELECT SINGLE A~POSID B~POST1
           INTO (P_CODE, P_NAME)
           FROM IMPR AS A INNER JOIN IMPU AS B
             ON B~POSNR EQ A~POSNR
          WHERE A~POSNR EQ L_POSNR
            AND B~SPRAS EQ SY-LANGU.

ENDFORM.                    " GET_POSITION_ID
