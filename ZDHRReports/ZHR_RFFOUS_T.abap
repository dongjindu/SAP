* Program Name      : ZHR_RFFOUS_T_EE
* Author            : YONGPING LI
* Creation Date     : 2004.11.02.
* Specifications By : LATINA CARHEE
* Development Request No :UD1K912809
* Addl Documentation:
* Description       : COPIED FROM RFFOUS_T AND MODIFIED TO SORT BY COST
*                     CENTER. IT CAN ONLY BE USED FOR EMPLOYEE PAYMENT
*                     PRINT.
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/02/2006 Manju        UD1K919600   Work Schedule changes
* 03/08/2006 Manju        UD1K919674   Work schedule text changes
* 03/14/2006 Manju        UD1K919725   Provision to sort payslip based
**                                     on Cost center/ Time admin
************************************************************************
* Note :- Since this program uses Logical Database ,Field groups and the
*         requirement was to sort based on  time admin which is in *
*         different table--->
*         So made use of REGUH-ZTELX  field to populate Time admin at
*         the Run time. So that using existing code it can be easy
*         sorted with minimal code change.  This was done  to  avoid
*         re-writing of whole code and to avoid adding field
*         in Reguh / REGUP table.
*
***********************************************************************
*  Überweisungs-Druckprogramm RFFOUS_T (USA) ACH format                *
*  Print program for bank transfer RFFOUS_T (USA) ACH format           *
*                                                                      *
************************************************************************
*
*----------------------------------------------------------------------*
* Das Programm includiert:                                             *
*                                                                      *
* RFFORI0M  Makrodefinition für den Selektionsbildaufbau               *
* RFFORI00  Deklarationsteil der Zahlungsträger-Druckprogramme         *
* RFFORIUS  Deklararionsteil fuer RFFORIU4                             *
* RFFORIU4  Datenträgeraustausch USA (ACH format)                      *
* RFFORI06  Avis                                                       *
* RFFORI07  Zahlungsbegleitliste                                       *
* RFFORI99  Allgemeine Unterroutinen der Zahlungsträger-Druckprogramme *
*----------------------------------------------------------------------*
* The program includes:                                                *
*                                                                      *
* RFFORI0M  definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORIUS  data definitions for RFFORIU4                              *
* RFFORIU4  domestic transfer (DME) USA (ACH format)                   *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Report Header                                                        *
*----------------------------------------------------------------------*
REPORT zhr_rffous_t
  LINE-SIZE 132
  MESSAGE-ID f0
  NO STANDARD PAGE HEADING.



*----------------------------------------------------------------------*
*  Segments                                                            *
*----------------------------------------------------------------------*
TABLES:
  reguh,
  regup,
  rfsdo,
  pa0007.

TYPES: BEGIN OF hrpayus_remitkey,
  comp_code TYPE bukrs,
  bus_area TYPE gsber,
  ref_doc_no TYPE xblnr,
  item_text TYPE sgtxt,
  filler(6) TYPE c,
END OF hrpayus_remitkey.
DATA items TYPE hrpayus_remitkey OCCURS 0 WITH HEADER LINE.
DATA: l_err_garn LIKE bapiret2,
      l_dest LIKE tbdestination-rfcdest,
      l_function TYPE rs38l_fnam.

DATA: L_Y010.
Data: kostl_des(20).
data: kOStl_d(30).
data: l_schkz like pa0007-schkz,                            "UD1K919600
      l_rtext like t508s-rtext.                             "UD1K919600
data: l_kostl  like csks-kostl.
*----------------------------------------------------------------------*
*  Macro definitions                                                   *
*----------------------------------------------------------------------*
INCLUDE rffori0m.

INITIALIZATION.

*----------------------------------------------------------------------*
*  Parameters / Select-Options                                         *
*----------------------------------------------------------------------*
  block 1.
  SELECT-OPTIONS:
    sel_zawe FOR  reguh-rzawe,         "Zahlwege / payment methods
    sel_secc FOR  rfsdo-fordsecc,      "Standard Entry Class Code
    sel_uzaw FOR  reguh-uzawe,         "Zahlwegzusatz
    sel_hbki FOR  reguh-hbkid,         "house bank short key
    sel_hkti FOR  reguh-hktid,         "account data short key
    sel_waer FOR  reguh-waers,         "currency
    sel_vbln FOR  reguh-vblnr.         "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  block 2.
  auswahl: xdta w, avis a, begl b.
  spool_authority.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  block 3.
  PARAMETERS:
    par_unix LIKE rfpdo2-fordnamd,     "Dateiname für DTA und TemSe
    par_dtyp LIKE rfpdo-forddtyp,      "Ausgabeformat und -medium
    par_f_id LIKE dtausfh-fh7,         "ID File modifier
    par_c_id LIKE dtausbh-bh7,         "default: company entry descript.
    par_anzp LIKE rfpdo-fordanzp,      "number of test prints
    par_maxp LIKE rfpdo-fordmaxp,      "number of items in summary list
    par_maxa LIKE rfpdo2-fordmaxa,     "number of addenda records
    par_addn LIKE rfpdo2-fordaddn,     "addenda records
    par_belp LIKE rfpdo-fordbelp,      "payment doc. validation
    par_espr LIKE rfpdo-fordespr,      "texts in reciepient's lang.
    par_isoc LIKE rfpdo-fordisoc.      "currency in ISO code
  SELECTION-SCREEN END OF BLOCK 3.

  PARAMETERS:
    par_anzb LIKE rfpdo2-fordanzb NO-DISPLAY,
    par_zdru LIKE rfpdo-fordzdru  NO-DISPLAY,
    par_priz LIKE rfpdo-fordpriz  NO-DISPLAY,
    par_sofz LIKE rfpdo1-fordsofz NO-DISPLAY,
    par_vari(12) TYPE c           NO-DISPLAY,
    par_sofo(1)  TYPE c           NO-DISPLAY.

* Begin of changes  - UD1K919725
  SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE text-001.
  Parameters : R1 radiobutton group p1 default 'X' ,
               R2 radiobutton  group p1.
  SELECTION-SCREEN END OF BLOCK 4.
* End  of changes - UD1K919725

*----------------------------------------------------------------------*
*  Vorbelegung der Parameter und Select-Options                        *
*  default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM init.
  sel_zawe-low     = 'P'.
  sel_zawe-option  = 'EQ'.
  sel_zawe-sign    = 'I'.
  APPEND sel_zawe.

  par_belp = space.
  par_zdru = space.
  par_xdta = 'X'.
  par_dtyp = '0'.
  par_avis = 'X'.
  par_begl = 'X'.
  par_anzp = 2.
  par_anzb = 2.
  par_espr = space.
  par_isoc = space.
  par_maxp = 9999.
  par_maxa = 9999.
  par_f_id = 'A'.
  par_addn = space.



*----------------------------------------------------------------------*
*  tables / fields / field-groups / AT SELECTION SCREEN                *
*----------------------------------------------------------------------*
  INCLUDE rffori00.


*- Prüfungen bei DTA ---------------------------------------------------
*- special checks for US DME -------------------------------------------
  IF par_xdta EQ 'X'.                  "Datenträgeraustausch / DME
    IF par_dtyp EQ space.
      par_dtyp = '0'.                  "TemSe
    ENDIF.
    IF par_dtyp NA '01'.
      SET CURSOR FIELD 'PAR_DTYP'.
      MESSAGE e068.                   "this message and the docu must be
    ENDIF.                             "changed. Allowed: 0 = TemSe,
  ENDIF.                              "                  1 = File system



*----------------------------------------------------------------------*
*  Kopfzeilen (nur bei der Zahlungsbegleitliste)                       *
*  batch heading (for the payment summary list)                        *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  IF flg_begleitl EQ 1.
    PERFORM kopf_zeilen.                                    "RFFORI07
  ENDIF.



*----------------------------------------------------------------------*
*  Felder vorbelegen                                                   *
*  preparations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  hlp_auth  = par_auth.                "spool authority

  hlp_temse = '0---------'.

  PERFORM vorbereitung.





*----------------------------------------------------------------------*
*  Unterprogramm Datendefinitionen fuer RFFORIU4                       *
*  data definitions for RFFORIU4                                       *
*----------------------------------------------------------------------*
  INCLUDE rfforius.



*----------------------------------------------------------------------*
*  Daten prüfen und extrahieren                                        *
*  check and extract data                                              *
*----------------------------------------------------------------------*
GET reguh.

* Begin - UD1K919725
  if R1 eq 'X'.
  select single sachz into reguh-ZTELX from Pa0001
   where pernr = reguh-pernr
    and  endda >= sy-datum
    and  begda <= sy-datum.
  endif.
* End  - UD1K919725

  CHECK sel_zawe.
  CHECK reguh-bkref(3) IN sel_secc.
  CHECK sel_uzaw.
  CHECK sel_hbki.
  CHECK sel_hkti.
  CHECK sel_waer.
  CHECK sel_vbln.

*  select single SCHKZ into l_schkz              "UD1K919600/UD1K919674
*    from pa0007 where pernr = reguh-pernr.      "UD1K919600/UD1K919674
*  if sy-subrc eq 0.
*    reguh-idoc_num =  l_schkz.
*  endif.

  IF reguh-bkref EQ ' ' OR
   ( reguh-bkref(3) NE 'PPD' AND
     reguh-bkref(3) NE 'CCD' AND
     reguh-bkref(3) NE 'CTX' ).
    CASE zw_laufi+5(1).
      WHEN 'P'.
        reguh-bkref(3) = 'PPD'.
      WHEN OTHERS.
        reguh-bkref(3) = 'CCD'.
    ENDCASE.
  ENDIF.
  PERFORM pruefung.
  PERFORM pruefung_betrag USING 10 reguh-rwbtr.
  PERFORM extract_vorbereitung.


GET regup.



* CHECK THE VENDOR GROUP                      "UD1K912809
*  PERFORM CHECK_GROUP CHANGING L_Y010.       "UD1K912809
*  IF L_Y010 NE 'X'.                          "UD1K912809
*    REJECT.                                  "UD1K912809
*  ENDIF.                                     "UD1K912809
* READ THE COST CENTER                        "UD1K912809
  PERFORM READ_KOSTL.                                       "UD1K912809

  IF reguh-paygr IS INITIAL.
    reguh-paygr = par_c_id.
  ENDIF.
  hlp_sort+0(16) = reguh-paygr.
  hlp_sort+16(8) = reguh-ausfd.
  PERFORM extract.


  IF regup-xblnr(5) = 'HRGRN'.
    items-comp_code = regup-bukrs.
    items-bus_area = regup-gsber.
    items-ref_doc_no = regup-xblnr.
    items-item_text = regup-sgtxt.
    IF items-item_text CN '* '.
      WHILE items-item_text(1) CA '* '.
        SHIFT items-item_text.
      ENDWHILE.
    ENDIF.
    APPEND items.
  ENDIF.

*----------------------------------------------------------------------*
*  Bearbeitung der extrahierten Daten                                  *
*  print forms, DME, remittance advices and lists                      *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  l_function = 'HRPAYUS_REMIT_INFO_CREATE_LIST'.
  READ TABLE tab_rfc INDEX 1.
  IF sy-subrc NE 0.
    CALL FUNCTION 'FUNCTION_EXISTS'
         EXPORTING
              funcname           = l_function
         EXCEPTIONS
              function_not_exist = 1
              OTHERS             = 2.
    IF sy-subrc = 0.
      CALL FUNCTION l_function
           TABLES
                items  = items
           EXCEPTIONS
                OTHERS = 1.
    ENDIF.
  ELSE.
    l_dest = tab_rfc-dest.
    LOOP AT tab_rfc.
      IF tab_rfc-dest <> l_dest.
        CLEAR l_dest.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF NOT l_dest IS INITIAL.
      CALL FUNCTION l_function
            DESTINATION
                tab_rfc-dest
            TABLES
                items  = items
                EXCEPTIONS
                OTHERS = 1.
    ENDIF.
  ENDIF.
  IF flg_selektiert NE 0.

    IF par_xdta EQ 'X'.
      PERFORM dme_domestic.                                 "RFFORIU4
    ENDIF.

    IF par_avis EQ 'X'.
      PERFORM avis.                                         "RFFORI06
    ENDIF.

    IF par_begl EQ 'X' AND par_maxp GT 0.
      flg_bankinfo = 2.
      PERFORM begleitliste.                                 "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM error_messages_rffous_t.

  PERFORM fehlermeldungen.

  PERFORM information.



*----------------------------------------------------------------------*
*  Unterprogramm Datenträgeraustausch Inland                           *
*  subroutine for DME                                                  *
*----------------------------------------------------------------------*
  INCLUDE rfforiu4.



*----------------------------------------------------------------------*
*  Unterprogramm Avis ohne Allongeteil                                 *
*  subroutine for remittance advices                                   *
*----------------------------------------------------------------------*
  INCLUDE ZHR_rffori06.



*----------------------------------------------------------------------*
*  Unterprogramm Begleitliste                                          *
*  subroutine for the summary list                                     *
*----------------------------------------------------------------------*
  INCLUDE rffori07.



*----------------------------------------------------------------------*
*  Allgemeine Unterprogramme                                           *
*  international subroutines                                           *
*----------------------------------------------------------------------*
  INCLUDE Z_rffori99_COPY.                                  "UD1K912809



***#################################################################****
*&---------------------------------------------------------------------*
*&      Form  CHECK_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_Y010  text
*----------------------------------------------------------------------*
FORM CHECK_GROUP CHANGING P_Y010.                           "UD1K912809
  DATA: L_KTOKK LIKE LFA1-KTOKK.
  DATA: L_LIFNR LIKE LFA1-LIFNR.

  L_LIFNR = REGUP-LIFNR.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_LIFNR
       IMPORTING
            OUTPUT = L_LIFNR.

  SELECT SINGLE KTOKK INTO L_KTOKK FROM LFA1
    WHERE LIFNR = L_LIFNR.
  IF SY-SUBRC = 0 AND L_KTOKK = 'Y010'.
    P_Y010 = 'X'.
  ELSE.
    CLEAR P_Y010.
  ENDIF.
ENDFORM.                    " CHECK_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_KOSTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_KOSTL.                                            "UD1K912809
  DATA: BEGIN OF LT_KOSTL OCCURS 0,
          KOSTL  LIKE CSKS-KOSTL,
        END OF LT_KOSTL.
  DATA: L_LINES TYPE I.
  DATA: L_LIFNR LIKE LFA1-LIFNR.

  L_LIFNR = REGUP-LIFNR.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_LIFNR
       IMPORTING
            OUTPUT = L_LIFNR.

  SELECT PA0001~KOSTL INTO TABLE LT_KOSTL
   FROM PA0001 INNER JOIN CSKS ON
        PA0001~KOSTL = CSKS~KOSTL
   WHERE PA0001~PERNR = L_LIFNR      AND
         PA0001~ENDDA GE REGUP-LAUFD AND
         PA0001~BEGDA LE REGUP-LAUFD.

  DESCRIBE TABLE LT_KOSTL LINES L_LINES.
  READ TABLE LT_KOSTL INDEX L_LINES.
  REGUP-KOSTL = LT_KOSTL-KOSTL.

ENDFORM.                    " READ_KOSTL
