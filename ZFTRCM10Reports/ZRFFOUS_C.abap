************************************************************************
*                                                                      *
*  Check print program RFFOUS_C for HMMA                               *
*                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*& Program Name :ZRFFOUS_C                                             *
*& Description  : International Payment Medium - Check                 *
*                 (with check management)                              *
*& Module : FI                                                         *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*& Date        Author           Description/Reason for Change          *
*& ----------  --------------   ------------------------------------   *
*&             Cyril Alex       Enhancement/modification               *
*& --------------------------------------------------------------------*
*& Objects modified : Reports RFFOUS_C and RFFORI01(incld.)            *
*&                  : Form YPCC_CHECK_NUM copied to                    *
*&                         ZS02FIS_AP_CHECK                            *
*  07/18/2005  Chris Li         Summarize the document type into two
*                               records: credit and debit for lit print
*    objects modified:  RFFOUS_C AND RFFORI01
*                       Sapcrapt form ZFI_APCHECK_Y05
*&---------------------------------------------------------------------*
* New functionalities:
* ---------------------------------------------------------------------
*  The form origionaly used to print a CHECK and a list of invoices for
* wich the check was cut. Now the form prints two such lists in the
* same page enabling Hyundai to keep one copy for themselves.
*
*  If the list extends to more than 11 lines the entire list is printed
* on the next page (only once).
*----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Program includes:                                                    *
*                                                                      *
* RFFORI0M  Definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORI01  check                                                      *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* report header                                                        *
*----------------------------------------------------------------------*
REPORT rffous_c
  LINE-SIZE 132
  MESSAGE-ID f0
  NO STANDARD PAGE HEADING.



*----------------------------------------------------------------------*
*  segments and tables for prenumbered checks                          *
*----------------------------------------------------------------------*
TABLES:
  reguh,
  regup.
* iNTERNAL TABLE FOR LINE ITEMS
DATA : wa_amt LIKE t042d-betre.
*data : wa_amt1(10) type p.
DATA: BEGIN OF t_lin OCCURS 0,
*---start wskim 03/05/2005 add 2
       budat  LIKE regup-budat,
       blart  LIKE regup-blart,
*---end
        bldat LIKE regup-bldat,
        xblnr LIKE regup-xblnr,
        sgtxt LIKE regup-sgtxt,
        wrbtr LIKE regud-wrbtr,
        wabzg LIKE regud-wabzg,
        wnett LIKE regud-wnett,
*------2004/01/29 add 2 field----*
        vblnr LIKE regup-vblnr,
        belnr LIKE regup-belnr,
      END OF t_lin.
DATA: BEGIN OF t_lin1 OCCURS 0,
      chect LIKE regud-chect,
      zaldt LIKE reguh-zaldt,
      lifnr LIKE reguh-lifnr,
      swnet LIKE regud-swnet,
      znme1 LIKE reguh-znme1.
        INCLUDE STRUCTURE t_lin.
DATA: END OF t_lin1.

* insert 2004/01/29
DATA : it_h LIKE reguh OCCURS 0 WITH HEADER LINE,
       it_p LIKE regup OCCURS 0 WITH HEADER LINE.
DATA : tmp LIKE bkpf-belnr.

DATA: w_11flg.

*----------------------------------------------------------------------*
*  macro definitions                                                   *
*----------------------------------------------------------------------*
INCLUDE zrffori0m.
*INCLUDE RFFORI0M.

INITIALIZATION.

  GET PARAMETER ID 'BLN' FIELD tmp.
*----------------------------------------------------------------------*
*  parameters and select-options                                       *
*----------------------------------------------------------------------*
*  PARAMETERS:  P_AMT(10) TYPE P.

  block 1.
  SELECT-OPTIONS:
    sel_zawe FOR  reguh-rzawe,              "payment method
    sel_uzaw FOR  reguh-uzawe,              "payment method supplement
    sel_gsbr FOR  reguh-srtgb,              "business area
    sel_hbki FOR  reguh-hbkid NO-EXTENSION NO INTERVALS, "house bank id
    sel_hkti FOR  reguh-hktid NO-EXTENSION NO INTERVALS. "account id
  SELECTION-SCREEN:
    BEGIN OF LINE,
    COMMENT 01(30) text-106 FOR FIELD par_stap,
    POSITION POS_LOW.
  PARAMETERS:
    par_stap LIKE rfpdo-fordstap.           "check lot number
  SELECTION-SCREEN:
    COMMENT 40(30) textinfo FOR FIELD par_stap,
    END OF LINE.
  PARAMETERS:
    par_rchk LIKE rfpdo-fordrchk.           "Restart from
  SELECT-OPTIONS:
    sel_waer FOR  reguh-waers,              "currency
    sel_vbln FOR  reguh-vblnr.              "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  block 2.
  auswahl: zdru z, avis a, begl b.
  spool_authority.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  block 3.
  PARAMETERS:
    par_zfor LIKE rfpdo1-fordzfor,          "different form
    par_fill LIKE rfpdo2-fordfill,          "filler for spell_amount
    par_anzp LIKE rfpdo-fordanzp,           "number of test prints
    par_maxp LIKE rfpdo-fordmaxp,           "no of items in summary list
    par_belp LIKE rfpdo-fordbelp,           "payment doc. validation
    par_espr LIKE rfpdo-fordespr,           "texts in reciepient's lang.
    par_isoc LIKE rfpdo-fordisoc,           "currency in ISO code
    par_nosu LIKE rfpdo2-fordnosu,          "no summary page
    par_novo LIKE rfpdo2-fordnovo.          "no voiding of checks
  SELECTION-SCREEN END OF BLOCK 3.

  SELECTION-SCREEN:
    BEGIN OF BLOCK 4 WITH FRAME TITLE text-100,
    BEGIN OF LINE.
  PARAMETERS:
    par_neud AS CHECKBOX.
  SELECTION-SCREEN:
    COMMENT 03(70) text-101 FOR FIELD par_neud,
    END OF LINE,
    BEGIN OF LINE,
    COMMENT 01(31) textchkf FOR FIELD par_chkf,
    POSITION POS_LOW.
  PARAMETERS:
    par_chkf LIKE payr-checf.
  SELECTION-SCREEN:
    COMMENT 52(05) textchkt FOR FIELD par_chkt,
    POSITION POS_HIGH.
  PARAMETERS:
    par_chkt LIKE payr-chect.
  SELECTION-SCREEN:
    END OF LINE,
    BEGIN OF LINE,
    COMMENT 01(30) text-107 FOR FIELD par_void,
    POSITION POS_LOW.
  PARAMETERS:
    par_void LIKE payr-voidr.
  SELECTION-SCREEN:
    COMMENT 38(30) textvoid FOR FIELD par_void,
    END OF LINE,
    END OF BLOCK 4.

  PARAMETERS:
    par_xdta LIKE rfpdo-fordxdta  NO-DISPLAY,
    par_priw LIKE rfpdo-fordpriw  NO-DISPLAY,
    par_sofw LIKE rfpdo1-fordsofw NO-DISPLAY,
    par_dtyp LIKE rfpdo-forddtyp  NO-DISPLAY,
    par_unix LIKE rfpdo2-fordnamd NO-DISPLAY,
    par_nenq(1)  TYPE c           NO-DISPLAY,
    par_vari(12) TYPE c           NO-DISPLAY,
    par_sofo(1)  TYPE c           NO-DISPLAY.


*----------------------------------------------------------------------*
*  Default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM init.
  PERFORM text(sapdbpyf) USING 102 textzdru.
  PERFORM text(rfchkl00) USING: textchkf 200, textchkt 201.
  sel_zawe-low    = 'C'.
  sel_zawe-option = 'EQ'.
  sel_zawe-sign   = 'I'.
  APPEND sel_zawe.

  par_belp = space.
  par_zdru = 'X'.
  par_xdta = space.
  par_dtyp = space.
  par_avis = space.
  par_begl = 'X'.
  par_fill = space.
  par_anzp = 2.
  par_espr = space.
  par_isoc = space.
  par_maxp = 9999.



*----------------------------------------------------------------------*
*  tables / fields / field-groups / at selection-screen                *
*----------------------------------------------------------------------*
  INCLUDE zrffori00.
*INCLUDE RFFORI00.

* AT SELECTION-SCREEN.

  PERFORM scheckdaten_eingabe USING par_rchk
                                    par_stap
                                    textinfo.

  textvoid = space.
  IF par_neud EQ 'X'.                    "Neu drucken / reprint
    IF par_rchk NE space.
      SET CURSOR FIELD 'PAR_RCHK'.
      MESSAGE e561(fs).                  "kein Neu drucken bei Restart
    ENDIF.                               "no reprint in restart mode
    IF zw_xvorl NE space.
      SET CURSOR FIELD 'ZW_XVORL'.
      MESSAGE e561(fs).                  "kein Neu drucken bei Vorschlag
    ENDIF.                               "no reprint if proposal run
    IF par_chkf EQ space AND par_chkt NE space.
      par_chkf = par_chkt.
    ENDIF.
    IF par_chkt EQ space.
      par_chkt = par_chkf.
    ENDIF.
    IF par_chkt LT par_chkf.
      SET CURSOR FIELD 'PAR_CHKF'.
      MESSAGE e650(db).
    ENDIF.
    IF par_chkf NE space OR par_void NE 0.
      IF par_chkf EQ space.
        SET CURSOR FIELD 'PAR_CHKF'.
        MESSAGE e055(00).
      ENDIF.
      SELECT * FROM payr UP TO 1 ROWS    "im angegebenen Intervall m?sen
        WHERE zbukr EQ zw_zbukr-low      "Schecks vorhanden sein
      AND hbkid EQ sel_hbki-low        "check interval is not allowed to
          AND hktid EQ sel_hkti-low      "be empty
          AND checf LE par_chkt
          AND chect GE par_chkf
          AND ichec EQ space
          AND voidr EQ 0
          AND xbanc EQ space.
      ENDSELECT.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'PAR_CHKF'.
        MESSAGE e509(fs).
      ENDIF.
      SELECT SINGLE * FROM tvoid WHERE voidr EQ par_void.
      IF sy-subrc NE 0 OR tvoid-xsyse NE space.
        SET CURSOR FIELD 'PAR_VOID'.
        MESSAGE e539(fs).
      ELSE.
        SELECT SINGLE * FROM tvoit
          WHERE langu EQ sy-langu AND voidr EQ par_void.
        textvoid = tvoit-voidt.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR:
      par_chkf,
      par_chkt,
      par_void.
  ENDIF.



AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 EQ 1.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'ZW_ZBUKR-HIGH' OR
       screen-name EQ '%_ZW_ZBUKR_%_APP_%-VALU_PUSH'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_stap.
  CALL FUNCTION 'F4_CHECK_LOT'
       EXPORTING
            i_xdynp      = 'X'
            i_dynp_progn = 'RFFOUS_C'
            i_dynp_dynnr = '1000'
            i_dynp_zbukr = 'ZW_ZBUKR-LOW'
            i_dynp_hbkid = 'SEL_HBKI-LOW'
            i_dynp_hktid = 'SEL_HKTI-LOW'
       IMPORTING
            e_stapl      = par_stap
       EXCEPTIONS
            OTHERS       = 0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_zfor.
  PERFORM f4_formular USING par_zfor.

AT SELECTION-SCREEN ON par_zfor.
  IF par_zfor NE space.
    SET CURSOR FIELD 'PAR_ZFOR'.
    CALL FUNCTION 'FORM_CHECK'
         EXPORTING
              i_pzfor = par_zfor.
  ENDIF.



*----------------------------------------------------------------------*
*  batch heading (for the payment summary list)                        *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  IF flg_begleitl EQ 1.
    PERFORM kopf_zeilen.                                    "RFFORI07
  ENDIF.



*----------------------------------------------------------------------*
*  preparations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*---jh add
  CLEAR : wa_amt.
  SELECT SINGLE betre INTO  wa_amt
  FROM t042d
  WHERE bukrs = 'H201'
  AND   hbkid = sel_hbki-low
  AND   hktid = sel_hkti-low.

  hlp_auth  = par_auth.                "spool authority
  hlp_temse  = '----------'.           "Keine TemSe-Verwendung
  hlp_filler = par_fill.
  PERFORM vorbereitung.
  PERFORM scheckdaten_pruefen USING par_rchk
                                    par_stap.

  IF zw_xvorl EQ space AND par_zdru NE space AND par_neud NE space.
    IF par_chkf NE space.
      flg_neud = 1.                    "neu drucken durchs Druckprogramm
      REFRESH tab_check.               "print program reprints checks
      tab_check-option = 'EQ'.
      tab_check-sign   = 'I'.
      tab_check-high   = space.
      SELECT * FROM payr
        WHERE zbukr EQ zw_zbukr-low
          AND hbkid EQ sel_hbki-low
          AND hktid EQ sel_hkti-low
          AND checf LE par_chkt
          AND chect GE par_chkf
          AND ichec EQ space
          AND voidr EQ 0
          AND xbanc EQ space.
        tab_check-low = payr-checf.
        APPEND tab_check.
      ENDSELECT.
      INSERT *payr INTO daten.
    ELSE.
      REFRESH tab_check.
      flg_neud = 2.
    ENDIF.
    SELECT SINGLE * FROM tvoid WHERE voidr EQ par_void.
  ENDIF.

  IF par_zdru EQ 'X'.
    IF sy-calld EQ space.              "fremder Enqueue nur wenn
      par_nenq = space.                "Programm gerufen wurde
    ENDIF.                             "foreign enqueue only if called
    IF par_nenq EQ space.
      PERFORM schecknummern_sperren.                        "RFFORI01
    ENDIF.
  ENDIF.



*----------------------------------------------------------------------*
*  check and extract data                                              *
*----------------------------------------------------------------------*

GET reguh.

  CHECK sel_zawe.
  CHECK sel_uzaw.
  CHECK sel_gsbr.
  CHECK sel_hbki.
  CHECK sel_hkti.
  CHECK sel_waer.
  CHECK sel_vbln.

  PERFORM pruefung.
  PERFORM scheckinfo_pruefen.                               "RFFORI01
  PERFORM extract_vorbereitung.
  MOVE : reguh TO it_h . APPEND it_h. CLEAR it_h.

GET regup.

*  CHECK REGUP-VBLNR = REGUP-BELNR.
  PERFORM extract.
  IF reguh-zbukr NE regup-bukrs.
    tab_uebergreifend-zbukr = reguh-zbukr.
    tab_uebergreifend-vblnr = reguh-vblnr.
    COLLECT tab_uebergreifend.
  ENDIF.

*----------------------------------------------------------------------*
*  print checks, remittance advices and lists                          *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF flg_selektiert NE 0.

    IF par_zdru EQ 'X'.
      hlp_zforn = par_zfor.
      hlp_checf_restart = par_rchk.
      IF par_novo NE space.
        flg_schecknum = 2.
      ENDIF.
      PERFORM scheck.                                       "RFFORI01
      IF par_nenq EQ space.
        PERFORM schecknummern_entsperren.                   "RFFORI01
      ENDIF.
    ENDIF.

    IF par_avis EQ 'X'.
      flg_schecknum = 1.
      PERFORM avis.                                         "RFFORI06
    ENDIF.

    IF par_begl EQ 'X' AND par_maxp GT 0.
      flg_bankinfo = 1.
      PERFORM begleitliste.                                 "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM fehlermeldungen.

  PERFORM information.

* CREATE A NEW SPOOL FOR PRINTING THE ADVICES IF THE
* NUMBER OF LINES  WAS MORE THAN 11 IN ANY OF THE CHECKS.
*  TEH DATA IS AVAILABLE IN TABLE T_LIN1.

  IF w_11flg = 'X'.
    PERFORM prn_list.
  ENDIF.


*----------------------------------------------------------------------*
*  subroutines for check print and prenumbered checks                  *
*----------------------------------------------------------------------*
  INCLUDE zrffori01.
*  INCLUDE RFFORI01.



*----------------------------------------------------------------------*
*  subroutines for remittance advices                                  *
*----------------------------------------------------------------------*
  INCLUDE zrffori06.
*  INCLUDE RFFORI06.



*----------------------------------------------------------------------*
*  subroutines for the payment summary list                            *
*----------------------------------------------------------------------*
  INCLUDE zrffori07.
*  INCLUDE RFFORI07.



*----------------------------------------------------------------------*
*  international subroutines                                           *
*----------------------------------------------------------------------*
  INCLUDE zrffori99.
*  INCLUDE RFFORI99.
*&---------------------------------------------------------------------*
*&      Form  PRN_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prn_list.
  DATA: w_lcnt LIKE sy-tabix.
* Print page LIST
  CLEAR itcpo-tdimmed.
  MOVE 'Advice' TO itcpo-tdcovtitle.
  MOVE 'X' TO itcpo-tdnewid.
*Issue # 20041111-010 Requested by SY LEE
*Changed by wskim, on 20041117
*supplements function
*-----Start
  SORT t_lin1 ASCENDING BY chect xblnr bldat .
*-----End
*move 'Advice' to ITCPO-TDDATASET.
*move 'Advice' to itcpo-tdtitle.
  CALL FUNCTION 'OPEN_FORM'
   EXPORTING
     device                            = 'PRINTER'
     dialog                            = space
     form                              = 'ZS02FIS_AP_LIST'
*   FORM                              = 'ZFI_AP_LIST'
*  FORM                              = 'ZFI_APCHECK'
     language                          = t001-spras
     options                           = itcpo
   EXCEPTIONS
     OTHERS                            = 1
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*-----start wskim 02/09/2005 include minus amount
  SORT t_lin1 BY chect.
*-----end
  LOOP AT t_lin1.
*=======jhs modify 2004/01/28
*-----start wskim 02/09/2005 include minus amount
*    CHECK  t_lin1-wrbtr > 0.
*-----end
    w_lcnt = sy-tabix.
    MOVE-CORRESPONDING t_lin1 TO regup.
    MOVE-CORRESPONDING t_lin1 TO regud.
    MOVE-CORRESPONDING t_lin1 TO reguh.

    MOVE-CORRESPONDING t_lin1 TO t_lin.
*====2004/01/29 modify    except request document
*====2004/02/07
    IF t_lin-vblnr <> t_lin-belnr.

      CALL FUNCTION 'WRITE_FORM'  " List of invoices
             EXPORTING
                  element  = '007'
                  function = 'APPEND'
             EXCEPTIONS
                  window   = 1
                  element  = 2.
    ENDIF.
*===============================================*
    AT NEW chect.

      CALL FUNCTION 'WRITE_FORM'  " List heading
            EXPORTING
                 element  = '102'
                 window   = 'LHEAD'
*                        function = 'APPEND'
            EXCEPTIONS
                 window   = 1
                 element  = 2.

      CALL FUNCTION 'WRITE_FORM'   " Hyundai motor etc.
            EXPORTING
                 element  = '101'
                 window   = 'HYUN'
*                        function = 'APPEND'
            EXCEPTIONS
                 window   = 1
                 element  = 2.
      CALL FUNCTION 'WRITE_FORM'   " Column headings
              EXPORTING
                   element  = '201'
                   window   = 'DETAIL_A'
*                          function = 'APPEND'
              EXCEPTIONS
                   window   = 1
                   element  = 2.

* LOOP AT T_LIN1 where CHEcT = t_lin1-CHECT.
*     ENDLOOP.
*
    ENDAT.

    AT END OF chect.
      SUM.

      MOVE t_lin1-wrbtr TO regud-swnet.

      CALL FUNCTION 'WRITE_FORM'  " Bottom line with total
             EXPORTING
                  element  = 'BOT'
                  function = 'APPEND'
             EXCEPTIONS
                    window   = 1
                  element  = 2.

      w_lcnt = w_lcnt + 1.
      READ TABLE t_lin1 INDEX w_lcnt.

      IF sy-subrc = 0.
        CALL FUNCTION 'CONTROL_FORM'  " to skip to page LIST
                    EXPORTING
                      command         = 'NEW-PAGE LIST'
                   EXCEPTIONS
                     unopened        = 1
                     unstarted       = 2
                     OTHERS          = 3.
      ENDIF.
    ENDAT.




  ENDLOOP.



  CALL FUNCTION 'CLOSE_FORM'.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " PRN_LIST
