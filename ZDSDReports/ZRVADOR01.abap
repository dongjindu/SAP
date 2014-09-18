*----------------------------------------------------------------------*
*              Print of an order confirmation by SAPscript

* Date           Developer      Request         Description
* 09/13/06       Manju          UD1K922112      Bug Fix
*----------------------------------------------------------------------*
REPORT ZRVADOR01 LINE-COUNT 100 MESSAGE-ID VN.

TABLES: KOMK,                          "Communicationarea for conditions
        KOMP,                          "Communicationarea for conditions
        KOMVD,                         "Communicationarea for conditions
        VBCO3,                         "Communicationarea for view
        VBDKA,                         "Headerview
        VBDPA,                         "Itemview
        VBDPAU,                        "Subitemnumbers
        CONF_OUT,                      "Configuration data
        SADR,                          "Addresses
        TVAG,                          "Reason for rejection
        VEDKA,                         "Servicecontract head data
        VEDPA,                         "Servicecontract position data
        VEDKN,                         "Servicecontract head notice data
        VEDPN,                         "Servicecontract pos. notice data
        RISERLS,                       "Serialnumbers
        KOMSER,                        "Serialnumbers for print
        TVBUR,                         "Sales office
        TVKO,                          "Sales organisation
        ADRS,                          "Communicationarea for Address
        FPLTDR,                        "billing schedules
        WTAD_ADDIS_IN_SO_PRINT,        "additional
        WTAD_BUYING_PRINT_EXTRA_TEXT.  "texts belonging to additional
INCLUDE ZRVADTABL.
*INCLUDE RVADTABL.
INCLUDE ZRVDIREKT.
*INCLUDE RVDIREKT.
INCLUDE ZVEDADATA.
*INCLUDE VEDADATA.

* data for access to central address maintenance
INCLUDE ZSDZAVDAT.
*INCLUDE SDZAVDAT.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TYPE-POOLS: ADDI.

DATA PRICE_PRINT_MODE(1) TYPE C.       "Print-mode
DATA: RETCODE   LIKE SY-SUBRC.         "Returncode
DATA: REPEAT(1) TYPE C.
DATA: XSCREEN(1) TYPE C.               "Output on printer or screen
DATA: BEGIN OF STEU,                   "Controldata for output
        VDKEX(1) TYPE C,
        VDPEX(1) TYPE C,
        KBKEX(1) TYPE C,
        KBPEX(1) TYPE C,
      END OF STEU.
data : tot_qty like VBDPA-KWMENG,
       tot_val like VBDPA-NETWR,
       tot_CPR like VBDPA-NETWR,
       l_ZP01  like VBDPA-NETPR,
       l_ZV00  like VBDPA-NETPR,
       l_desc like VBDPA-ARKTX,
       l_page type i,
       l_page2 type i,
       l_cnt type i,
       f_page type c.


DATA: BEGIN OF TVBDPA OCCURS 0.        "Internal table for items
        INCLUDE STRUCTURE VBDPA.
DATA: END OF TVBDPA.

DATA: BEGIN OF TKOMV OCCURS 50.
        INCLUDE STRUCTURE KOMV.
DATA: END OF TKOMV.

DATA: BEGIN OF TKOMVD OCCURS 50.
        INCLUDE STRUCTURE KOMVD.
DATA: END OF TKOMVD.

DATA: BEGIN OF TVBDPAU OCCURS 5.
        INCLUDE STRUCTURE VBDPAU.
DATA: END   OF TVBDPAU.

DATA: BEGIN OF TKOMCON OCCURS 50.
        INCLUDE STRUCTURE CONF_OUT.
DATA: END   OF TKOMCON.

DATA: BEGIN OF TKOMSERVH OCCURS 1.
        INCLUDE STRUCTURE VEDKA.
DATA: END   OF TKOMSERVH.

DATA: BEGIN OF TKOMSERVP OCCURS 5.
        INCLUDE STRUCTURE VEDPA.
DATA: END   OF TKOMSERVP.

DATA: BEGIN OF TKOMSERVHN OCCURS 5.
        INCLUDE STRUCTURE VEDKN.
DATA: END   OF TKOMSERVHN.

DATA: BEGIN OF TKOMSERVPN OCCURS 5.
        INCLUDE STRUCTURE VEDPN.
DATA: END   OF TKOMSERVPN.

DATA: BEGIN OF TKOMSER OCCURS 5.
        INCLUDE STRUCTURE RISERLS.
DATA: END   OF TKOMSER.

DATA: BEGIN OF TKOMSER_PRINT OCCURS 5.
        INCLUDE STRUCTURE KOMSER.
DATA: END   OF TKOMSER_PRINT.

DATA: BEGIN OF TFPLTDR OCCURS 5.
        INCLUDE STRUCTURE FPLTDR.
DATA: END   OF TFPLTDR.

DATA: TADDI_PRINT TYPE ADDI_SO_PRINT_ITAB WITH HEADER LINE.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DATA: PR_KAPPL(01)   TYPE C VALUE 'V'. "Application for pricing

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

FORM ENTRY USING RETURN_CODE US_SCREEN.

  CLEAR RETCODE.
  XSCREEN = US_SCREEN.
  PERFORM PROCESSING.
  IF RETCODE NE 0.
    RETURN_CODE = 1.
  ELSE.
    RETURN_CODE = 0.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROCESSING.

  PERFORM GET_DATA.
  CHECK RETCODE = 0.
  PERFORM FORM_OPEN USING XSCREEN VBDKA-LAND1.
  CHECK RETCODE = 0.
  PERFORM FORM_TITLE_PRINT.
  CHECK RETCODE = 0.
  PERFORM VALIDITY_PRINT.
  CHECK RETCODE = 0.
  PERFORM HEADER_DATA_PRINT.
  CHECK RETCODE = 0.
  PERFORM HEADER_SERV_PRINT.
  CHECK RETCODE = 0.
  PERFORM HEADER_NOTICE_PRINT.
  CHECK RETCODE = 0.
  PERFORM HEADER_INTER_PRINT.
  CHECK RETCODE = 0.
  PERFORM HEADER_TEXT_PRINT.
  CHECK RETCODE = 0.
  PERFORM ITEM_PRINT.
  CHECK RETCODE = 0.
  PERFORM END_PRINT.
  CHECK RETCODE = 0.
  PERFORM FORM_CLOSE.
  CHECK RETCODE = 0.

ENDFORM.

***********************************************************************
*       S U B R O U T I N E S                                         *
***********************************************************************

*---------------------------------------------------------------------*
*       FORM ALTERNATIVE_ITEM                                         *
*---------------------------------------------------------------------*
*       A text is printed, if the item is an alternative item.        *
*---------------------------------------------------------------------*

FORM ALTERNATIVE_ITEM.

  CHECK VBDPA-GRPOS CN '0'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ALTERNATIVE_ITEM'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_REPEAT                                             *
*---------------------------------------------------------------------*
*       A text is printed, if it is a repeat print for the document.  *
*---------------------------------------------------------------------*

FORM CHECK_REPEAT.

  CLEAR REPEAT.
  SELECT * INTO *NAST FROM NAST WHERE KAPPL = NAST-KAPPL
                                AND   OBJKY = NAST-OBJKY
                                AND   KSCHL = NAST-KSCHL
                                AND   SPRAS = NAST-SPRAS
                                AND   PARNR = NAST-PARNR
                                AND   PARVW = NAST-PARVW
                                AND   NACHA BETWEEN '1' AND '4'.
    CHECK *NAST-VSTAT = '1'.
    REPEAT = 'X'.
    EXIT.
  ENDSELECT.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DELIVERY_DATE                                            *
*---------------------------------------------------------------------*
*       If the delivery date in the item is different to the header   *
*       date and there are no scheduled quantities, the delivery date *
*       is printed in the item block.                                 *
*---------------------------------------------------------------------*

FORM DELIVERY_DATE.

  IF VBDKA-LFDAT =  SPACE AND
     VBDPA-LFDAT NE SPACE AND
     VBDPA-ETENR_DA = SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_DELIVERY_DATE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DIFFERENT_CONSIGNEE                                      *
*---------------------------------------------------------------------*
*       If the consignee in the item is different to the header con-  *
*       signee, it is printed by this routine.                        *
*---------------------------------------------------------------------*

FORM DIFFERENT_CONSIGNEE.

  CHECK VBDKA-NAME1_WE NE VBDPA-NAME1_WE
    OR  VBDKA-NAME2_WE NE VBDPA-NAME2_WE
    OR  VBDKA-NAME3_WE NE VBDPA-NAME3_WE
    OR  VBDKA-NAME4_WE NE VBDPA-NAME4_WE.
  CHECK VBDPA-NAME1_WE NE SPACE
    OR  VBDPA-NAME2_WE NE SPACE
    OR  VBDPA-NAME3_WE NE SPACE
    OR  VBDPA-NAME4_WE NE SPACE.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_CONSIGNEE'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DIFFERENT_REFERENCE_NO                                   *
*---------------------------------------------------------------------*
*       If the reference number in the item is different to the header*
*       reference number, it is printed by this routine.              *
*---------------------------------------------------------------------*

FORM DIFFERENT_REFERENCE_NO.

  CHECK VBDPA-VBELN_VANG NE VBDKA-VBELN_VANG
    OR  VBDPA-VBTYP_VANG NE VBDKA-VBTYP_VANG.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_REFERENCE_NO'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DIFFERENT_TERMS                                          *
*---------------------------------------------------------------------*
*       If the terms in the item are different to the header terms,   *
*       they are printed by this routine.                             *
*---------------------------------------------------------------------*
FORM DIFFERENT_TERMS.

  DATA: US_VPOSN   LIKE VEDPA-VPOSN.
  DATA: US_TEXT(1) TYPE C.             "Flag for Noticetext was printed

  IF VBDPA-ZTERM NE VBDKA-ZTERM AND
     VBDPA-ZTERM NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_TERMS_OF_PAYMENT'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.
  IF VBDPA-INCO1 NE SPACE.
    IF VBDPA-INCO1 NE VBDKA-INCO1 OR
       VBDPA-INCO2 NE VBDKA-INCO2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TERMS_OF_DELIVERY'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ENDIF.
  ENDIF.

* Print different validity-data for the position
  READ TABLE TKOMSERVP WITH KEY VBDPA-POSNR.
  IF SY-SUBRC EQ 0.
    VEDPA = TKOMSERVP.
    IF VEDPA-VBEGDAT NE SPACE       AND
       VEDPA-VENDDAT NE SPACE       AND
       NOT VEDPA-VBEGDAT IS INITIAL AND
       NOT VEDPA-VENDDAT IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TERMS_OF_SERV1'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ELSEIF VEDPA-VBEGDAT NE SPACE AND
           NOT VEDPA-VBEGDAT IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TERMS_OF_SERV2'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TERMS_OF_SERV3'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ENDIF.
  ENDIF.

* Notice-rules for the positions.
  MOVE VBDPA-POSNR TO US_VPOSN.
  CLEAR US_TEXT.
  LOOP AT TKOMSERVPN WHERE VPOSN = US_VPOSN.
    VEDPN = TKOMSERVPN.
    IF US_TEXT IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TERMS_OF_NOTTXT'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
      US_TEXT = CHARX.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_TERMS_OF_NOTICE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDLOOP.
  IF NOT US_TEXT IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'EMPTY_LINE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM END_PRINT                                                *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*

FORM END_PRINT.

  PERFORM GET_HEADER_PRICES.

  CALL FUNCTION 'CONTROL_FORM'
       EXPORTING
            COMMAND = 'PROTECT'.

*  PERFORM HEADER_PRICE_PRINT.   "UD1K922112

  IF NOT PRICE_PRINT_MODE EQ CHARA.
* Pricing data init
    CALL FUNCTION 'RV_PRICE_PRINT_GET_BUFFER'
         EXPORTING
              I_INIT   = CHARX
         TABLES
              T_TKOMV  = TKOMV
              T_TKOMVD = TKOMVD.

  ENDIF.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'END_VALUES'.

*  CALL FUNCTION 'WRITE_FORM'
*       EXPORTING
*            ELEMENT = 'END_VALUES'
*            WINDOW  =  'FT_BANK'.

*  CaLL FUNCTION 'WRITE_FORM'
*       EXPORTING
*            ELEMENT = 'END_VALUES'
*            WINDOW  =  'DESCFT'.

   CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'END_VALUES'
            WINDOW  =  'FOOTER'.

   CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'END_VALUES'
            WINDOW  =  'LAST'.


  CALL FUNCTION 'CONTROL_FORM'
       EXPORTING
            COMMAND = 'ENDPROTECT'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'SUPPLEMENT_TEXT'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CALL FUNCTION 'END_FORM'.

            .
CALL FUNCTION 'START_FORM'
 EXPORTING
*   ARCHIVE_INDEX          =
   FORM                   = 'Z_DOWNPAYMENT'
*   LANGUAGE               = ' '
   STARTPAGE              = 'TERMS'
*   PROGRAM                = ' '
*   MAIL_APPL_OBJECT       =
* IMPORTING
*   LANGUAGE               =
 EXCEPTIONS
   FORM                   = 1
   FORMAT                 = 2
   UNENDED                = 3
   UNOPENED               = 4
   UNUSED                 = 5
   SPOOL_ERROR            = 6
   OTHERS                 = 7.
* Page counter for Trems & condition Page
       if   l_cnt >= 1 and f_page eq ''.
         l_page = l_page + 1.

       else.
         l_page = l_page.
       endif.
        l_page = l_page + 1.
        l_page2  = l_page + 1.

   CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            WINDOW  =  'PAGE_CNT'.

   CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'END_VALUES'
            WINDOW  =  'MAIN'.

  CALL FUNCTION 'END_FORM'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       End of printing the form                                      *
*---------------------------------------------------------------------*

FORM FORM_CLOSE.

  DATA DA_CLEAR_VBELN(1) TYPE C.

* bei Druckansicht im Anlegen gibt es noch keine Belegnummer - f¸r die
* Anzeige tempor‰re Belegnummer ¸bergeben und danach zur¸cknehmen, damit
* Folgeverarbeitung noch funktioniert
  IF VBDKA-VBELN IS INITIAL.
    DA_CLEAR_VBELN = CHARX.
    VBDKA-VBELN = '$000000001'.
  ENDIF.

  CALL FUNCTION 'CLOSE_FORM'
       EXCEPTIONS
            OTHERS = 1.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
    RETCODE = 1.
  ENDIF.
  SET COUNTRY SPACE.

  IF DA_CLEAR_VBELN EQ CHARX.
    CLEAR VBDKA-VBELN.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FORM_OPEN                                                *
*---------------------------------------------------------------------*
*       Start of printing the form                                    *
*---------------------------------------------------------------------*
*  -->  US_SCREEN  Output on screen                                   *
*                  ' ' = printer                                      *
*                  'X' = screen                                       *
*  -->  US_COUNTRY County for telecommunication and SET COUNTRY       *
*---------------------------------------------------------------------*

FORM FORM_OPEN USING US_SCREEN US_COUNTRY.

* Send confirmation to user who send the document.
  IF  nast-nacha EQ '2'.
    nast-usnam = vbdka-ernam.
*  get fax country key
    IF NAST-TELTX is initial AND  NAST-MANUE NE 'X'.
       perform get_fax_land using nast-tland.
    ENDIF.
  ENDIF.

INCLUDE ZRVADOPFO.
*  INCLUDE RVADOPFO.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FORM_TITLE_PRINT                                         *
*---------------------------------------------------------------------*
*       Printing of the form title depending of the field VBTYP       *
*---------------------------------------------------------------------*

FORM FORM_TITLE_PRINT.

  CASE VBDKA-VBTYP.
    WHEN 'A'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_A'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'B'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_B'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'C'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_C'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'E'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_E'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'F'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_F'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'G'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_F'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'H'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_H'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'K'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_K'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN 'L'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_L'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    WHEN OTHERS.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TITLE_OTHERS'
                WINDOW  = 'TITLE'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
  ENDCASE.
  IF REPEAT NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'REPEAT'
              WINDOW  = 'REPEAT'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*

FORM GET_DATA.

  DATA: US_VEDA_VBELN     LIKE VEDA-VBELN.
  DATA: US_VEDA_POSNR_LOW LIKE VEDA-VPOSN.

  DATA: DA_MESS LIKE VBFS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'RV_PRICE_PRINT_GET_MODE'
       IMPORTING
            E_PRINT_MODE = PRICE_PRINT_MODE.

  IF PRICE_PRINT_MODE EQ CHARA.
    CALL FUNCTION 'RV_PRICE_PRINT_REFRESH'
         TABLES
              TKOMV = TKOMV.
  ENDIF.

  CLEAR KOMK.
  CLEAR KOMP.

  VBCO3-MANDT = SY-MANDT.
  VBCO3-SPRAS = NAST-SPRAS.
  VBCO3-VBELN = NAST-OBJKY.
  VBCO3-KUNDE = NAST-PARNR.
  VBCO3-PARVW = NAST-PARVW.

  CALL FUNCTION 'RV_DOCUMENT_PRINT_VIEW'
       EXPORTING
            COMWA                       = VBCO3
       IMPORTING
            KOPF                        = VBDKA
       TABLES
            POS                         = TVBDPA
            MESS                        = DA_MESS
       EXCEPTIONS
            FEHLER_BEI_DATENBESCHAFFUNG = 1.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
    RETCODE = 1.
    EXIT.
  ELSE.
    LOOP AT DA_MESS.
      SY-MSGID = DA_MESS-MSGID.
      SY-MSGNO = DA_MESS-MSGNO.
      SY-MSGTY = DA_MESS-MSGTY.
      SY-MSGV1 = DA_MESS-MSGV1.
      SY-MSGV2 = DA_MESS-MSGV2.
      SY-MSGV3 = DA_MESS-MSGV3.
      SY-MSGV4 = DA_MESS-MSGV4.
      PERFORM PROTOCOL_UPDATE.
    ENDLOOP.
  ENDIF.

* fill address key --> necessary for emails
  ADDR_KEY-ADDRNUMBER = VBDKA-ADRNR.
  ADDR_KEY-PERSNUMBER = VBDKA-ADRNP.
  ADDR_KEY-ADDR_TYPE  = VBDKA-ADDRESS_TYPE.

* Fetch servicecontract-data and notice-data for head and position.
  US_VEDA_VBELN     = VBDKA-VBELN.
  US_VEDA_POSNR_LOW = POSNR_LOW.
  CALL FUNCTION 'SD_VEDA_GET_PRINT_DATA'
       EXPORTING
            I_DOCUMENT_NUMBER = US_VEDA_VBELN
            I_LANGUAGE        = SY-LANGU
            I_POSNR_LOW       = US_VEDA_POSNR_LOW
       TABLES
            PRINT_DATA_POS    = TKOMSERVP
            PRINT_DATA_HEAD   = TKOMSERVH
            PRINT_NOTICE_POS  = TKOMSERVPN
            PRINT_NOTICE_HEAD = TKOMSERVHN.

  PERFORM GET_CONTROLL_DATA.

  PERFORM SENDER.
  PERFORM CHECK_REPEAT.
  PERFORM TVBDPAU_CREATE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_ITEM_BILLING_SCHEDULES                               *
*---------------------------------------------------------------------*
*       In this routine the billing schedules are fetched from the    *
*       database.                                                     *
*---------------------------------------------------------------------*

FORM GET_ITEM_BILLING_SCHEDULES.

  REFRESH TFPLTDR.
  CHECK NOT VBDPA-FPLNR IS INITIAL.

  CALL FUNCTION 'BILLING_SCHED_PRINTVIEW_READ'
       EXPORTING
            I_FPLNR    = VBDPA-FPLNR
            I_LANGUAGE = NAST-SPRAS
            I_VBELN    = VBDKA-VBELN
       TABLES
            ZFPLTDR    = TFPLTDR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ITEM_BILLING_SCHEDULES_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the billing shedules of a salesdocument    *
*       position.                                                      *
*----------------------------------------------------------------------*
FORM  ITEM_BILLING_SCHEDULES_PRINT.

  DATA: FIRST_LINE(1) TYPE C.

  FIRST_LINE = CHARX.
  LOOP AT TFPLTDR.
    FPLTDR = TFPLTDR.
*   Output of the following printlines
    IF NOT FPLTDR-PERIO IS INITIAL.
*     periodische Fakturen
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_BILLING_SCHEDULE_PERIODIC'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
*     bei periodischen nur eine Zeile
      EXIT.
    ELSEIF FPLTDR-FAREG CA '14'.
*     prozentuale Teilfakturierung
      IF NOT FIRST_LINE IS INITIAL.
        CLEAR FIRST_LINE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_BILLING_SCHEDULE_PERCENT_HEADER'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_BILLING_SCHEDULE_PERCENT'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ELSEIF FPLTDR-FAREG CA '235'.
*     wertm‰ﬂige  Teilfakturierung
      IF NOT FIRST_LINE IS INITIAL.
        CLEAR FIRST_LINE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_BILLING_SCHEDULE_VALUE_HEADER'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_BILLING_SCHEDULE_VALUE'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ELSEIF FPLTDR-FAREG CA '3'.
*     Schluﬂrechnung
    ENDIF.
  ENDLOOP.
ENDFORM.
*eject

*&---------------------------------------------------------------------*
*&      FORM  GET_ITEM_ADDIS
*&---------------------------------------------------------------------*
*       Additionals data are fetched from database
*----------------------------------------------------------------------*
FORM GET_ITEM_ADDIS.

  CLEAR: TADDI_PRINT.

  CALL FUNCTION 'WTAD_ADDIS_IN_SO_PRINT'
       EXPORTING
            FI_VBELN              = VBDKA-VBELN
            FI_POSNR              = VBDPA-POSNR
*           FI_LANGUAGE           = SY-LANGU
       TABLES
            FET_ADDIS_IN_SO_PRINT = TADDI_PRINT
       EXCEPTIONS
            ADDIS_NOT_ACTIVE      = 1
            NO_ADDIS_FOR_SO_ITEM  = 2
            OTHERS                = 3.

ENDFORM.                               " GET_ITEM_ADDIS

*---------------------------------------------------------------------*
*       FORM GET_ITEM_CHARACTERISTICS                                 *
*---------------------------------------------------------------------*
*       In this routine the configuration data item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM GET_ITEM_CHARACTERISTICS.

  DATA DA_T_CABN LIKE CABN OCCURS 10 WITH HEADER LINE.
  DATA: BEGIN OF DA_KEY,
          MANDT LIKE CABN-MANDT,
          ATINN LIKE CABN-ATINN,
        END   OF DA_KEY.

  REFRESH TKOMCON.
  CHECK NOT VBDPA-CUOBJ IS INITIAL AND
            VBDPA-ATTYP NE VAR_TYP.

  CALL FUNCTION 'VC_I_GET_CONFIGURATION'
       EXPORTING
            INSTANCE      = VBDPA-CUOBJ
            LANGUAGE      = NAST-SPRAS
            PRINT_SALES   = CHARX
       TABLES
            CONFIGURATION = TKOMCON
       EXCEPTIONS
            OTHERS        = 4.

  RANGES : DA_IN_CABN FOR DA_T_CABN-ATINN.
* Beschreibung der Merkmale wegen Objektmerkmalen auf sdcom-vkond holen
  CLEAR DA_IN_CABN. REFRESH DA_IN_CABN.
  LOOP AT TKOMCON.
    DA_IN_CABN-OPTION = 'EQ'.
    DA_IN_CABN-SIGN   = 'I'.
    DA_IN_CABN-LOW    = TKOMCON-ATINN.
    APPEND DA_IN_CABN.
  ENDLOOP.

  CLEAR DA_T_CABN. REFRESH DA_T_CABN.
  CALL FUNCTION 'CLSE_SELECT_CABN'
*    EXPORTING
*         KEY_DATE                     = SY-DATUM
*         BYPASSING_BUFFER             = ' '
*         WITH_PREPARED_PATTERN        = ' '
*         I_AENNR                      = ' '
*    IMPORTING
*         AMBIGUOUS_OBJ_CHARACTERISTIC =
     TABLES
          IN_CABN                      = DA_IN_CABN
          T_CABN                       = DA_T_CABN
     EXCEPTIONS
          NO_ENTRY_FOUND               = 1
          OTHERS                       = 2.

* Preisfindungsmerkmale / Merkmale auf VCSD_UPDATE herausnehmen
  SORT DA_T_CABN.
  LOOP AT TKOMCON.
    DA_KEY-MANDT = SY-MANDT.
    DA_KEY-ATINN = TKOMCON-ATINN.
    READ TABLE DA_T_CABN WITH KEY DA_KEY BINARY SEARCH.
    IF SY-SUBRC <> 0 OR
       ( ( DA_T_CABN-ATTAB = 'SDCOM' AND
          DA_T_CABN-ATFEL = 'VKOND'       ) OR
        ( DA_T_CABN-ATTAB = 'VCSD_UPDATE' ) ) .
      DELETE TKOMCON.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_ITEM_PRICES                                          *
*---------------------------------------------------------------------*
*       In this routine the price data for the item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM GET_ITEM_PRICES.

  CLEAR: KOMP,
         TKOMV.

  IF KOMK-KNUMV NE VBDKA-KNUMV OR
     KOMK-KNUMV IS INITIAL.
    CLEAR KOMK.
    KOMK-MANDT = SY-MANDT.
    KOMK-KALSM = VBDKA-KALSM.
    KOMK-KAPPL = PR_KAPPL.
    KOMK-WAERK = VBDKA-WAERK.
    KOMK-KNUMV = VBDKA-KNUMV.
    KOMK-KNUMA = VBDKA-KNUMA.
    KOMK-VBTYP = VBDKA-VBTYP.
    KOMK-LAND1 = VBDKA-LAND1.
    KOMK-VKORG = VBDKA-VKORG.
    KOMK-VTWEG = VBDKA-VTWEG.
    KOMK-SPART = VBDKA-SPART.
    KOMK-BUKRS = VBDKA-BUKRS_VF.
    KOMK-HWAER = VBDKA-WAERS.
    KOMK-PRSDT = VBDKA-ERDAT.
    KOMK-KURST = VBDKA-KURST.
    KOMK-KURRF = VBDKA-KURRF.
    KOMK-KURRF_DAT = VBDKA-KURRF_DAT.
  ENDIF.
  KOMP-KPOSN = VBDPA-POSNR.
  KOMP-KURSK = VBDPA-KURSK.
  KOMP-KURSK_DAT = VBDPA-KURSK_DAT.
   if vbdka-vbtyp ca 'HKNOT6'.
      if vbdpa-shkzg ca ' A'.
           komp-shkzg = 'X'.
      endif.
    else.
       if vbdpa-shkzg ca 'BX'.
            komp-shkzg = 'X'.
       endif.
    endif.

  IF PRICE_PRINT_MODE EQ CHARA.
    CALL FUNCTION 'RV_PRICE_PRINT_ITEM'
         EXPORTING
              COMM_HEAD_I = KOMK
              COMM_ITEM_I = KOMP
              LANGUAGE    = NAST-SPRAS
         IMPORTING
              COMM_HEAD_E = KOMK
              COMM_ITEM_E = KOMP
         TABLES
              TKOMV       = TKOMV
              TKOMVD      = TKOMVD.
  ELSE.
    CALL FUNCTION 'RV_PRICE_PRINT_ITEM_BUFFER'
         EXPORTING
              COMM_HEAD_I = KOMK
              COMM_ITEM_I = KOMP
              LANGUAGE    = NAST-SPRAS
         IMPORTING
              COMM_HEAD_E = KOMK
              COMM_ITEM_E = KOMP
         TABLES
              TKOMV       = TKOMV
              TKOMVD      = TKOMVD.
  ENDIF.
* READ ZP01 Price - Color Price

  read table tkomv with key KPOSN = VBDPA-posnr        " UD1K922114
                            KSCHL = 'ZP01'.
   if sy-subrc eq 0 .
     l_ZP01 = tkomv-KWERT.
     tot_CPR = tot_CPR +  l_ZP01 .
   else.
     clear l_ZP01.
   endif.
* Read Vehicle Price
   read table tkomvd with key KSCHL = 'ZV00'.
    if sy-subrc eq 0.
      l_ZV00  =  tkomvd-KBETR.
    else.
      clear l_ZV00.
    endif.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_HEADER_PRICES                                        *
*---------------------------------------------------------------------*
*       In this routine the price data for the header is fetched from *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM GET_HEADER_PRICES.

  LOOP AT TVBDPA.

    CALL FUNCTION 'SD_TAX_CODE_MAINTAIN'
         EXPORTING
              KEY_KNUMV           = VBDKA-KNUMV
              KEY_KPOSN           = TVBDPA-POSNR
              I_APPLICATION       = ' '
              I_PRICING_PROCEDURE = VBDKA-KALSM
         TABLES
              XKOMV               = TKOMV.


  ENDLOOP.

  IF PRICE_PRINT_MODE EQ CHARA.
    CALL FUNCTION 'RV_PRICE_PRINT_HEAD'
         EXPORTING
              COMM_HEAD_I = KOMK
              LANGUAGE    = NAST-SPRAS
         IMPORTING
              COMM_HEAD_E = KOMK
         TABLES
              TKOMV       = TKOMV
              TKOMVD      = TKOMVD.
  ELSE.
    CALL FUNCTION 'RV_PRICE_PRINT_HEAD_BUFFER'
         EXPORTING
              COMM_HEAD_I = KOMK
              LANGUAGE    = NAST-SPRAS
         IMPORTING
              COMM_HEAD_E = KOMK
         TABLES
              TKOMV       = TKOMV
              TKOMVD      = TKOMVD.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_PRINT
*&---------------------------------------------------------------------*
*       Printing of header data like terms, weights ....               *
*----------------------------------------------------------------------*

FORM HEADER_DATA_PRINT.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'HEADER_DATA'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.                               " HEADER_DATA_PRINT

*---------------------------------------------------------------------*
*       FORM HEADER_PRICE_PRINT                                       *
*---------------------------------------------------------------------*
*       Printout of the header prices                                 *
*---------------------------------------------------------------------*

FORM HEADER_PRICE_PRINT.

  LOOP AT TKOMVD.

    AT FIRST.
      IF KOMK-SUPOS NE 0.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_SUM'.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'UNDER_LINE'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ENDAT.

    KOMVD = TKOMVD.
    IF KOMVD-KOAID = 'D'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TAX_LINE'.
    ELSE.
      IF NOT KOMVD-KNTYP EQ 'f'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'SUM_LINE'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE TKOMVD LINES SY-TFILL.
  IF SY-TFILL = 0.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER_LINE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM HEADER_TEXT_PRINT                                        *
*---------------------------------------------------------------------*
*       Printout of the headertexts                                   *
*---------------------------------------------------------------------*

FORM HEADER_TEXT_PRINT.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'HEADER_TEXT'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ITEM_BILLING_CORRECTION_HEADER                          *
*---------------------------------------------------------------------*
*       In the case of a billing correction, the header of the item   *
*       debit memo / credit memo position, is printed by this routine *
*---------------------------------------------------------------------*

FORM ITEM_BILLING_CORRECTION_HEADER USING US_GANF US_LANF.


  CHECK VBDKA-VBKLT EQ VBKLT_RECH_KORR.

  IF VBDKA-VBTYP = VBTYP_GANF.
*   Gutschriftsanforderung
    IF VBDPA-SHKZG = CHARX.
      IF US_GANF IS INITIAL.
        MOVE CHARX TO US_GANF.
        MOVE SPACE TO US_LANF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CORRECTION_TEXT_K'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ELSE.
      IF US_LANF IS INITIAL.
        MOVE CHARX TO US_LANF.
        MOVE SPACE TO US_GANF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CORRECTION_TEXT_L'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF VBDKA-VBTYP = VBTYP_LANF.
*   Lastschriftssanforderung
    IF VBDPA-SHKZG = SPACE.
      IF US_LANF IS INITIAL.
        MOVE CHARX TO US_LANF.
        MOVE SPACE TO US_GANF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CORRECTION_TEXT_L'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ELSE.
      IF US_GANF IS INITIAL.
        MOVE CHARX TO US_GANF.
        MOVE SPACE TO US_LANF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CORRECTION_TEXT_K'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITEM_ADDIS_PRINT
*&---------------------------------------------------------------------*
*       Printout of item additionals
*----------------------------------------------------------------------*
FORM ITEM_ADDIS_PRINT.

  LOOP AT TADDI_PRINT.
    MOVE-CORRESPONDING TADDI_PRINT TO WTAD_ADDIS_IN_SO_PRINT.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_ADDI_SO_INFO'
         EXCEPTIONS
              OTHERS  = 1.
    LOOP AT TADDI_PRINT-ADDI_SO_EXTRA_TEXT_INFO
            INTO WTAD_BUYING_PRINT_EXTRA_TEXT.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_ADDI_EXTRA_TEXT'
           EXCEPTIONS
                OTHERS  = 1.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                               " ITEM_ADDIS_PRINT
*---------------------------------------------------------------------*
*       FORM ITEM_CHARACERISTICS_PRINT                                *
*---------------------------------------------------------------------*
*       Printout of the item characteristics -> configuration         *
*---------------------------------------------------------------------*

FORM ITEM_CHARACTERISTICS_PRINT.

  LOOP AT TKOMCON.
    CONF_OUT = TKOMCON.
    IF SY-TABIX = 1.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_CONFIGURATION_HEADER'
           EXCEPTIONS
                OTHERS  = 1.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_CONFIGURATION'
           EXCEPTIONS
                OTHERS  = 1.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ITEM_DELIVERY_CONFIRMATION                               *
*---------------------------------------------------------------------*
*       If the delivery date is not confirmed, a text is printed      *
*---------------------------------------------------------------------*

FORM ITEM_DELIVERY_CONFIRMATION.

  CHECK VBDKA-VBTYP NE VBTYP_GANF AND VBDKA-VBTYP NE VBTYP_LANF.
  CHECK VBDPA-LFDAT = SPACE.
  CHECK VBDPA-KWMENG NE 0.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_DELIVERY_CONFIRMATION'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM ITEM_AGREED_DELIVERY_TIME                                *
*---------------------------------------------------------------------*
*       If an agreed delivery time and the corresponding text is      *
*       available on item level, the text is printed                  *
*---------------------------------------------------------------------*

FORM ITEM_AGREED_DELIVERY_TIME.

  CHECK VBDKA-VBTYP EQ 'B' OR VBDKA-VBTYP EQ 'G'.
  CHECK VBDPA-DELCO NE SPACE AND VBDPA-DELCO_BEZ NE SPACE.

  CALL FUNCTION 'WRITE_FORM'
     EXPORTING
          ELEMENT = 'ITEM_AGREED_DELIVERY_TIME'
     EXCEPTIONS
          ELEMENT = 1
          WINDOW  = 2.

  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ITEM_PRICE_PRINT                                         *
*---------------------------------------------------------------------*
*       Printout of the item prices                                   *
*---------------------------------------------------------------------*

FORM ITEM_PRICE_PRINT.

  LOOP AT TKOMVD.
    KOMVD = TKOMVD.
    IF SY-TABIX = 1 AND
     ( KOMVD-KOAID = CHARB OR
       KOMVD-KSCHL = SPACE ).
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_PRICE_QUANTITY'
          EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

    ELSE.
      IF KOMVD-KNTYP NE 'f'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_LINE_PRICE_TEXT'
                  EXCEPTIONS
                      ELEMENT = 1
                      WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

      ELSE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_LINE_REBATE_IN_KIND'
                  EXCEPTIONS
                            ELEMENT = 1
                            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ITEM_PRINT                                               *
*---------------------------------------------------------------------*
*       Printout of the items                                         *
*---------------------------------------------------------------------*

FORM ITEM_PRINT.

  DATA: DA_SUBRC LIKE SY-SUBRC,
        DA_DRAGR LIKE TVAG-DRAGR.

  DATA: DA_GANF(1) TYPE C,      "Print flag for billing correction
        DA_LANF(1) TYPE C.      "Print flag for billing correction

  CALL FUNCTION 'WRITE_FORM'           "First header
       EXPORTING  ELEMENT = 'ITEM_HEADER'
       EXCEPTIONS OTHERS  = 1.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.
  CALL FUNCTION 'WRITE_FORM'           "Activate header
       EXPORTING  ELEMENT = 'ITEM_HEADER'
                  TYPE    = 'TOP'
       EXCEPTIONS OTHERS  = 1.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.
clear : tot_qty, tot_val,tot_CPR.
l_page = 1.
 f_page = 'X'.
  LOOP AT TVBDPA.
    VBDPA = TVBDPA.
    tot_qty   = tot_qty + VBDPA-KWMENG.
    tot_val   = tot_val + VBDPA-NETWR.
    clear: l_desc,l_ZP01,l_ZV00.
*    concatenate vbdpa-ARKTX+0(3) vbdpa-ARKTX+12(8)  into   l_desc.
     move vbdpa-ARKTX to   l_desc.

    add 1 to l_cnt.
      if l_cnt = 13 and f_page = 'X'.
        clear f_page.
        clear l_cnt.
      endif.
    IF VBDPA-DRAGR EQ SPACE.           "Print rejected item?
      IF VBDPA-POSNR_NEU NE SPACE.     "Item
        PERFORM ITEM_BILLING_CORRECTION_HEADER USING DA_GANF DA_LANF.
        PERFORM GET_ITEM_SERIALS.
        PERFORM GET_ITEM_CHARACTERISTICS.
        PERFORM GET_ITEM_BILLING_SCHEDULES.
        PERFORM GET_ITEM_PRICES.
        PERFORM GET_ITEM_ADDIS.
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  COMMAND = 'ENDPROTECT'.
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  COMMAND = 'PROTECT'.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_LINE'.
        if l_cnt = 20 and f_page eq ''.
              CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  COMMAND = 'NEW-PAGE'    .
           clear l_cnt.
           l_page = l_page + 1.
        endif.
        PERFORM ITEM_REJECTED.
        PERFORM ITEM_PRICE_PRINT.
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  COMMAND = 'ENDPROTECT'.
        PERFORM ITEM_TEXT_PRINT.
        PERFORM ITEM_SERIALS_PRINT.
        PERFORM ITEM_CHARACTERISTICS_PRINT.
        PERFORM ITEM_ADDIS_PRINT.
        PERFORM ITEM_REFERENCE_BILLING.
        PERFORM ALTERNATIVE_ITEM.
        PERFORM DELIVERY_DATE.
        PERFORM ITEM_DELIVERY_CONFIRMATION.
        PERFORM ITEM_AGREED_DELIVERY_TIME.
        PERFORM ITEM_BILLING_SCHEDULES_PRINT.
        PERFORM DIFFERENT_REFERENCE_NO.
        PERFORM DIFFERENT_TERMS.
        PERFORM DIFFERENT_CONSIGNEE.
        PERFORM SCHEDULE_HEADER.
        PERFORM MAIN_ITEM.
      ELSE.
        PERFORM SCHEDULE_PRINT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'WRITE_FORM'           "Deactivate Header
       EXPORTING  ELEMENT  = 'ITEM_HEADER'
                  FUNCTION = 'DELETE'
                  TYPE     = 'TOP'
       EXCEPTIONS OTHERS   = 1.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM ITEM_REFERENCE_BILLING                                  *
*---------------------------------------------------------------------*
*       If the reference number of the billing is printed by this     *
*       routine. In case (debit memo / credit memo)                   *
*---------------------------------------------------------------------*

FORM ITEM_REFERENCE_BILLING.

  CHECK VBDKA-VBKLT EQ VBKLT_RECH_KORR.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_REFERENCE_BILLING'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM ITEM_REJECTED                                            *
*---------------------------------------------------------------------*
*       A text is printed, if the item is rejected                    *
*---------------------------------------------------------------------*

FORM ITEM_REJECTED.

  CHECK NOT VBDPA-ABGRU IS INITIAL.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_REJECTED'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM MAIN_ITEM                                                *
*---------------------------------------------------------------------*
*       A text is printed, if the item is a main item                 *
*---------------------------------------------------------------------*

FORM MAIN_ITEM.

  LOOP AT TVBDPAU INTO VBDPAU
                  WHERE POSNR EQ VBDPA-POSNR.
  IF VBDPAU-UPOSB IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ONE_SUBITEM'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'SEVERAL_SUBITEMS'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ITEM_TEXT_PRINT                                          *
*---------------------------------------------------------------------*
*       Printout of the item texts                                    *
*---------------------------------------------------------------------*

FORM ITEM_TEXT_PRINT.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_TEXT'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*

FORM PROTOCOL_UPDATE.

  CHECK XSCREEN = SPACE.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
       EXPORTING
            MSG_ARBGB = SYST-MSGID
            MSG_NR    = SYST-MSGNO
            MSG_TY    = SYST-MSGTY
            MSG_V1    = SYST-MSGV1
            MSG_V2    = SYST-MSGV2
            MSG_V3    = SYST-MSGV3
            MSG_V4    = SYST-MSGV4
       EXCEPTIONS
            OTHERS    = 1.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SCHEDULE_HEADER                                          *
*---------------------------------------------------------------------*
*       If there are schedules in the item, then here is printed the  *
*       header for the schedules.                                     *
*---------------------------------------------------------------------*

FORM SCHEDULE_HEADER.

  CHECK VBDPA-ETENR_DA NE SPACE.
  CALL FUNCTION 'CONTROL_FORM'
       EXPORTING
            COMMAND = 'PROTECT'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_SCHEDULE_HEADER'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SCHEDULE_PRINT                                           *
*---------------------------------------------------------------------*
*       This routine prints the schedules for an item.                *
*---------------------------------------------------------------------*

FORM SCHEDULE_PRINT.

  CHECK VBDPA-LFREL EQ 'X'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_SCHEDULE_PRINT'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SENDER                                                   *
*---------------------------------------------------------------------*
*       This routine determines the address of the sender (Table VKO) *
*---------------------------------------------------------------------*

FORM SENDER.

  SELECT SINGLE * FROM TVKO  WHERE VKORG = VBDKA-VKORG.
  IF SY-SUBRC NE 0.
    SYST-MSGID = 'VN'.
    SYST-MSGNO = '203'.
    SYST-MSGTY = 'E'.
    SYST-MSGV1 = 'TVKO'.
    SYST-MSGV2 = SYST-SUBRC.
    PERFORM PROTOCOL_UPDATE.
    EXIT.
  ENDIF.

  CLEAR GV_FB_ADDR_GET_SELECTION.
  GV_FB_ADDR_GET_SELECTION-ADDRNUMBER = TVKO-ADRNR.             "SADR40A
  CALL FUNCTION 'ADDR_GET'
       EXPORTING
            ADDRESS_SELECTION = GV_FB_ADDR_GET_SELECTION
            ADDRESS_GROUP     = 'CA01'
       IMPORTING
            SADR              = SADR
       EXCEPTIONS
            OTHERS            = 01.
  IF SY-SUBRC NE 0.
    CLEAR SADR.
  ENDIF.                               "SADR40A
  VBDKA-SLAND = SADR-LAND1.
  IF SY-SUBRC NE 0.
    SYST-MSGID = 'VN'.
    SYST-MSGNO = '203'.
    SYST-MSGTY = 'E'.
    SYST-MSGV1 = 'SADR'.
    SYST-MSGV2 = SYST-SUBRC.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.
*  SELECT SINGLE * FROM TVBUR  WHERE VKBUR = VBDKA-VKBUR.
*  IF SY-SUBRC NE 0.
*    SYST-MSGID = 'VN'.
*    SYST-MSGNO = '203'.
*    SYST-MSGTY = 'E'.
*    SYST-MSGV1 = 'TVBUR'.
*    SYST-MSGV2 = SYST-SUBRC.
*    PERFORM PROTOCOL_UPDATE.
*  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM TVBDPAU_CREATE                                           *
*---------------------------------------------------------------------*
*       This routine is creating a table which includes the subitem-  *
*       numbers                                                       *
*---------------------------------------------------------------------*

FORM TVBDPAU_CREATE.

  CLEAR TVBDPAU.
  REFRESH TVBDPAU.
  LOOP AT TVBDPA.
    IF TVBDPA-UEPOS IS INITIAL OR
       TVBDPA-UEPOS NE TVBDPAU-POSNR.
* Append work area to internal table TVBDPAU
      IF TVBDPAU-UPOSV > 0.
        APPEND TVBDPAU.
        CLEAR TVBDPAU.
      ENDIF.
* Start filling new work area
      TVBDPAU-POSNR = TVBDPA-POSNR.

      IF NOT TVBDPA-UEPOS IS INITIAL AND
         TVBDPA-UEPOS NE TVBDPAU-POSNR.
        TVBDPAU-POSNR = TVBDPA-UEPOS.
        TVBDPAU-UEPVW = TVBDPA-UEPVW.
        TVBDPAU-UPOSV = TVBDPA-POSNR.
      ENDIF.

    ELSE.
      IF TVBDPAU-UPOSV IS INITIAL OR
         TVBDPAU-UPOSV > TVBDPA-POSNR.
        TVBDPAU-UPOSV = TVBDPA-POSNR.
      ENDIF.
      IF TVBDPAU-UPOSB < TVBDPA-POSNR AND
         TVBDPAU-UPOSV < TVBDPA-POSNR.
        TVBDPAU-UPOSB = TVBDPA-POSNR.
      ENDIF.
      TVBDPAU-UEPVW = TVBDPA-UEPVW.    "UPOS-Verwendung
    ENDIF.
  ENDLOOP.
  IF TVBDPAU-UPOSV > 0.
    APPEND TVBDPAU.
  ENDIF.
  SORT TVBDPAU.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM VALIDITY_PRINT                                           *
*---------------------------------------------------------------------*
*       This routine is printing the period of validity for offers    *
*       and contracts                                                 *
*---------------------------------------------------------------------*

FORM VALIDITY_PRINT.

  CHECK STEU-VDKEX EQ SPACE.
  CASE VBDKA-VBTYP.
    WHEN 'B'.
      IF VBDKA-ANGDT CN '0' OR
         VBDKA-BNDDT CN '0'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'VALIDITY_OFFER'
                  WINDOW  = 'VALIDITY'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    WHEN 'E'.
      IF VBDKA-GUEBG CN '0' OR
         VBDKA-GUEEN CN '0'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'VALIDITY_CONTRACT'
                  WINDOW  = 'VALIDITY'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    WHEN 'F'.
      IF VBDKA-GUEBG CN '0' OR
         VBDKA-GUEEN CN '0'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'VALIDITY_CONTRACT'
                  WINDOW  = 'VALIDITY'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
    WHEN 'G'.
      IF VBDKA-GUEBG CN '0' OR
         VBDKA-GUEEN CN '0'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'VALIDITY_CONTRACT'
                  WINDOW  = 'VALIDITY'
             EXCEPTIONS
                  ELEMENT = 1
                  WINDOW  = 2.
        IF SY-SUBRC NE 0.
          PERFORM PROTOCOL_UPDATE.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HEADER_NOTICE_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the notice-rules of the contract-header.   *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HEADER_NOTICE_PRINT.

 DATA: US_TEXT(1) TYPE C.             "Kz. falls Text f¸r K¸ndigungsbed.

* K¸ndigungsbedingungen auf Kopfebene.
  CLEAR US_TEXT.
  LOOP AT TKOMSERVHN.
    VEDKN = TKOMSERVHN.
    IF US_TEXT IS INITIAL.
*     For the first time a headertext is printed.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'HEADER_TERMS_OF_NOTTXT'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
      US_TEXT = CHARX.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TERMS_OF_NOTICE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDLOOP.
* If notice-rules exists a empty line is printed.
  IF NOT US_TEXT IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'EMPTY_LINE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.                               " HEADER_NOTICE_PRINT
*eject

*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_SERIALS
*&---------------------------------------------------------------------*
*       This routine give back the serialnumbers of salesdocument      *
*       position. The numbers are processed as print-lines in the      *
*       table KOMSER_PRINT.                                            *
*----------------------------------------------------------------------*
*  -->  US_VBELN  Salesdocument
*  -->  US_POSNR  Position of the salesdocument
*----------------------------------------------------------------------*
FORM GET_ITEM_SERIALS.

  DATA: KEY_DATA LIKE RSEROB,
        SERNOS LIKE RSEROB OCCURS 0 WITH HEADER LINE.

  KEY_DATA-TASER = 'SER02'.
  KEY_DATA-SDAUFNR = VBDKA-VBELN.
  KEY_DATA-POSNR = VBDPA-POSNR.
  IF KEY_DATA-SDAUFNR IS INITIAL AND NOT
     KEY_DATA-POSNR IS INITIAL.
* beim Anlegen ist Belegnummer leer - deshalb Dummy-Belegnummer
    KEY_DATA-SDAUFNR = CHAR$.
  ENDIF.

* Read the Serialnumbers of a Position.
  REFRESH: TKOMSER,
           TKOMSER_PRINT.
  CALL FUNCTION 'GET_SERNOS_OF_DOCUMENT'
       EXPORTING
            KEY_DATA            = KEY_DATA
       TABLES
            SERNOS              = SERNOS
       EXCEPTIONS
            KEY_PARAMETER_ERROR = 1
            NO_SUPPORTED_ACCESS = 2
            NO_DATA_FOUND       = 3
            OTHERS              = 4.
  IF SY-SUBRC NE 0 AND
     SY-SUBRC NE 3.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  CHECK SY-SUBRC EQ 0.
* Serialnummern ¸bergeben
  TKOMSER-VBELN = SERNOS-SDAUFNR.
  TKOMSER-POSNR = SERNOS-POSNR.
  LOOP AT SERNOS.
    TKOMSER-SERNR = SERNOS-SERNR.
    APPEND TKOMSER.
  ENDLOOP.

* Process the stringtable for Printing.
  CALL FUNCTION 'PROCESS_SERIALS_FOR_PRINT'
       EXPORTING
            I_BOUNDARY_LEFT             = '(_'
            I_BOUNDARY_RIGHT            = '_)'
            I_SEP_CHAR_STRINGS          = ',_'
            I_SEP_CHAR_INTERVAL         = '_-_'
            I_USE_INTERVAL              = 'X'
            I_BOUNDARY_METHOD           = 'C'
            I_LINE_LENGTH               = 50
            I_NO_ZERO                   = 'X'
            I_ALPHABET                  = SY-ABCDE
            I_DIGITS                    = '0123456789'
            I_SPECIAL_CHARS             = '-'
            I_WITH_SECOND_DIGIT         = ' '
       TABLES
            SERIALS                     = TKOMSER
            SERIALS_PRINT               = TKOMSER_PRINT
       EXCEPTIONS
            BOUNDARY_MISSING            = 01
            INTERVAL_SEPARATION_MISSING = 02
            LENGTH_TO_SMALL             = 03
            INTERNAL_ERROR              = 04
            WRONG_METHOD                = 05
            WRONG_SERIAL                = 06
            TWO_EQUAL_SERIALS           = 07
            SERIAL_WITH_WRONG_CHAR      = 08
            SERIAL_SEPARATION_MISSING   = 09.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.


ENDFORM.                               " GET_ITEM_SERIALS
*eject


*&---------------------------------------------------------------------*
*&      Form  ITEM_SERIALS_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the serialnumbers of a salesdocument       *
*       position.                                                      *
*----------------------------------------------------------------------*
FORM ITEM_SERIALS_PRINT.

  DATA: FIRST_LINE(1) TYPE C.

  FIRST_LINE = CHARX.
  LOOP AT TKOMSER_PRINT.
    KOMSER = TKOMSER_PRINT.
    IF NOT FIRST_LINE IS INITIAL.
*     Output of the Headerline
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_SERIAL_HEADER'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
      CLEAR FIRST_LINE.
    ELSE.
*     Output of the following printlines
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_SERIAL'
           EXCEPTIONS
                ELEMENT = 1
                WINDOW  = 2.
      IF SY-SUBRC NE 0.
        PERFORM PROTOCOL_UPDATE.
      ENDIF.
    ENDIF.
  ENDLOOP.
* If serialnumbers exists a empty line is printed.
  IF FIRST_LINE IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'EMPTY_LINE'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.                               " ITEM_SERIALS_PRINT
*eject


*&---------------------------------------------------------------------*
*&      Form  HEADER_INTER_PRINT
*&---------------------------------------------------------------------*
*       Prints the message that if other condition for the positions   *
*       exists they are printed there.                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HEADER_INTER_PRINT.

  CHECK NOT STEU-VDKEX IS INITIAL.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'HEADER_TERMS_OF_TXTEND'
       EXCEPTIONS
            ELEMENT = 1
            WINDOW  = 2.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

ENDFORM.                               " HEADER_INTER_PRINT

*&---------------------------------------------------------------------*
*&      Form  GET_CONTROLL_DATA
*&---------------------------------------------------------------------*
*       Checks if servicedata for the header exists.                   *
*       Checks if servicedata for the position exists.                 *
*       Checks if noticedata for the header exists.                    *
*       Checks if noticedata for the position exists.                  *
*----------------------------------------------------------------------*
FORM GET_CONTROLL_DATA.

  DATA: LINES TYPE I.

* Exists servicedata for the header?
  DESCRIBE TABLE TKOMSERVH LINES LINES.
  IF LINES GT 0.
    STEU-VDKEX = 'X'.
  ENDIF.

* Exists servicedata for the position?
  DESCRIBE TABLE TKOMSERVP LINES LINES.
  IF LINES GT 0.
    STEU-VDPEX = 'X'.
  ENDIF.

* Exists noticedata for the header?
  DESCRIBE TABLE TKOMSERVHN LINES LINES.
  IF LINES GT 0.
    STEU-KBKEX = 'X'.
  ENDIF.

* Exists noticedata for the position?
  DESCRIBE TABLE TKOMSERVPN LINES LINES.
  IF LINES GT 0.
    STEU-KBPEX = 'X'.
  ENDIF.

ENDFORM.                               " GET_CONTROLL_DATA
*eject


*&---------------------------------------------------------------------*
*&      Form  HEADER_SERV_PRINT
*&---------------------------------------------------------------------*
*       Output of the validity of a service-contract.                  *
*----------------------------------------------------------------------*
FORM HEADER_SERV_PRINT.

  CHECK NOT STEU-VDKEX IS INITIAL.
  READ TABLE TKOMSERVH INDEX 1.
  MOVE TKOMSERVH TO VEDKA.

* Output of the validity.
  IF NOT VEDKA-VENDDAT IS INITIAL OR
     VEDKA-VENDDAT EQ SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TERMS_OF_SERV1'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ELSEIF VEDKA-VBEGDAT NE SPACE AND
         NOT VEDKA-VBEGDAT IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TERMS_OF_SERV2'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TERMS_OF_SERV3'
         EXCEPTIONS
              ELEMENT = 1
              WINDOW  = 2.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE.
    ENDIF.
  ENDIF.

ENDFORM.                               " HEADER_SERV_PRINT

*&---------------------------------------------------------------------*
*&      Form  get_fax_land
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST_TLAND  text
*----------------------------------------------------------------------*
form get_fax_land using   p_nast_land like nast-tland.

    DATA  L_land    like nast-tland .
    clear L_land.


    IF NOT addr_key-addrnumber IS INITIAL.
      CALL FUNCTION 'WFMC_FAXNUMBER_FOR_ADDRESS'
           EXPORTING
                adrnr          = addr_key-addrnumber
           IMPORTING
                tland          = L_land
           EXCEPTIONS
                addr_not_exist = 1
                OTHERS         = 2.
      IF sy-subrc = 0 AND NOT L_land IS INITIAL.
        p_nast_land = L_land.
      ENDIF.

    ENDIF.
endform.                    " get_fax_land
