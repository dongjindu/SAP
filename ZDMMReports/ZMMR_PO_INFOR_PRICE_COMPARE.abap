************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZMMR_PO_INFOR_PRICE_COMPARE
*& Type   : Report                                                     *
*& Author : Manju                                                      *
*& Title  : Program to compare PO & Info record price                  *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 68ED695857                                   *
* Issue log :             20060814-001                                 *
*                                                                      *
*   Requested by:        Mr Oh                                         *
*
*   Assigned to:   Richard Davis                                       *
*   Original Request #:                                                *
*   ABAP Analyst:  Manjunath Venkatesh                                 *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*     Program to compare current Purchase order price to Info record
*     price                                                            *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*  For PO price join EKKO ,EKPO and EKKI and KONV.
*  For Info record price Join A018 & KONP Tables
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -   ZMMR209                       *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 09/06/06    Manju        UD1K922030   Initial Coding
* 09/20/06    Manju        UD1K922199   Consider Delivery completed PO's
* 09/20/06    Manju        UD1K922201   Check for missing Conditions
* 09/20/06    Manju        UD1K922205   Do not send email when no data
*                                       is found.
* 09/21/06    Manju        UD1K922222   Program bug fix
************************************************************************
REPORT ZMMR_PO_INFOR_PRICE_COMPARE LINE-SIZE 132 LINE-COUNT 65
                          NO STANDARD PAGE HEADING message-id zmmm.

INCLUDE : zrmmpmxxr_incl.
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
*TABLES : EKKO,
*         EKPO,
*         A018,
*         KONV,
*         KONP,
*         EKKI.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
* Internal Table for PO
Types : begin of it_po1,
         ebeln like ekko-ebeln,
         ebelp like ekpo-ebelp,
         matnr like ekpo-matnr,
         werks like ekpo-werks,
         lgort like ekpo-lgort,
         lifnr like ekko-lifnr,
         bstyp like ekko-bstyp,
         ekorg like ekko-ekorg,
         ekgrp like ekko-ekgrp,
         kdatb like ekko-kdatb,
         kdate like ekko-kdate,
         KNUMV like ekko-KNUMV,
         aedat like ekko-aedat,
        end of it_po1.

data : it_po type standard table of it_po1 with header line.

* Internal Table for PO Condition Number
data : begin of it_hcond occurs 0,
        knumv like ekko-knumv,
        KPOSN like konv-kposn,
       end of it_hcond.

* Internal table  for PO Conditions
data : begin of  it_konv  occurs 0,
        KNUMV like konv-knumv,
        KPOSN like konv-kposn,
        KSCHL like konv-KSCHL,
        kdatu like konv-kdatu,
        KBETR like konv-KBETR,
        KPEIN like konv-KPEIN,
       end of it_konv.

DATA : BEGIN OF wa_info_rec ,
         KOPOS like konp-kopos,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         konwa LIKE konp-konwa,
         lifnr LIKE konp-lifnr,
         kzust LIKE konh-kzust,
         datab LIKE konh-datab,
         datbi LIKE konh-datbi,
       END OF wa_info_rec.

* Internal table for Info Record
data : it_info_rec like standard table of wa_info_rec with header line,
       it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
       WITH HEADER LINE,
        it_ebody TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
       WITH HEADER LINE.

* Internal table for PO.
data : begin of it_Po_no occurs 0,
       EBELN like EKKO-EBELN,
       end   of it_po_no.

data : begin of it_exnum occurs 0,
       exnum like eikp-exnum,
       EXPOs  like EIPO-EXPOS,
       STAWN like KONH-VAKEY,
       REFNR like eikp-refnr,
       end of it_exnum.

data : begin of it_konh occurs 0,
       VAKEY like konh-VAKEY,
       KBETR like konp-KBETR,
       KPEIN like konp-KPEIN,
       datab like konh-DATAB,
       datbi like konh-DATBI,
       end of it_konh.

* Internal Table for ZOA1 Condition Type
data : begin of it_ZOA1 occurs 0,
       EBELN like ekko-ebeln,
       ebelp like ekpo-ebelp,
       VAKEY like konh-VAKEY,
       KBETR like konp-KBETR,
       KPEIN like konp-KPEIN,
       datab like konh-DATAB,
       datbi like konh-DATBI,
      end of it_ZOA1.

* Output Internal Table
Types: begin of it_output1,
         ebeln like ekko-ebeln,
         ebelp like ekpo-ebelp,
         matnr like ekpo-matnr,
         werks like ekpo-werks,
         lgort like ekpo-lgort,
         lifnr like ekko-lifnr,
         KSCHL like konv-kschl,
         aedat like ekko-aedat,
*         kdatb like ekko-kdatb,
*         kdate like ekko-kdate,
         PKBETR like konv-kbetr,
         PKPEIN like konv-KPEIN,
         DATBI like A018-DATBI,
         DATAB like A018-DATAB,
         IKBETR like konv-kbetr,
         IKPEIN like konv-KPEIN,
         linecolor(4),     " ALV Color
        end of it_output1.

data : it_output type standard table of it_output1 with header line.



* Internal Table for Email file attachment.
data : begin of it_body occurs 0,
         ebeln(12) ,
         ebelp(6)  ,
         matnr(18) ,
         werks(6)  ,
         lgort(6)  ,
         lifnr(10) ,
         KSCHL(6)  ,
         aedat(10),
         PKBETR(15) ,
         PKPEIN(4) ,
         DATBI(10) ,
         DATAB(10) ,
         IKBETR(15),
         IKPEIN(4) ,
        end of it_body.

*-------------------------------------------------------------*
* Macro's
*--------------------------------------------------------------*
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.


*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-011.
Parameters :     p_EKORG like EKKO-EKORG DEFAULT 'PU01'.
select-options : s_BSART for EKKO-BSART,
                 s_lifnr for EKKO-lifnr,
                 S_EBELN for EKKo-EBELN,
                 S_AEDAT for EKKO-AEDAT,
                 s_MATNR for EKPO-MATNR.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-012.
parameters :  p_P1 radiobutton group g1,
              p_p2 radiobutton group g1.
SELECTION-SCREEN END OF BLOCK BL2.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-013.
Select-options : s_kSchl for  konp-KSCHL no intervals DEFAULT 'PB00'.
SELECTION-SCREEN END OF BLOCK BL3.

SELECTION-SCREEN BEGIN OF BLOCK BL4 WITH FRAME TITLE TEXT-014.
parameters :  p_r1 radiobutton group g2,
              p_r2 radiobutton group g2,
              p_miss as checkbox,
              p_c5 as checkbox,
              p_dist(25) DEFAULT 'POIFVAL' .
SELECTION-SCREEN END OF BLOCK BL4.

*-------------------------------------------------------------*
* END of Selection Screen
*--------------------------------------------------------------*
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

TOP-OF-PAGE.
  PERFORM top_of_page.

AT SELECTION-SCREEN.
  if s_kSchl[] is initial and SSCRFIELDS-UCOMM eq 'ONLI'.
    s_kSchl-low = 'PB00'.
    s_kschl-option = 'EQ'.
    s_kschl-sign = 'I'.
    Append s_kschl.
  endif.
*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
Start-of-selection.

* GET PO & Condition Data
  perform get_po_data.

* Process output Data
  perform process_data.

  if it_output[] is initial.        "UD1K922222
      MESSAGE S999 WITH text-m01.
      exit.
  endif.


* Send EMAIL
  if p_c5 eq 'X'.
    check not it_output[] is initial.  "UD1K922205
    perform send_email.
  endif.

* Perform Display output in form of ALV
  perform display_ALV_OUTPUT.

*-------------------------------------------------------------*
* End-of-selection
*--------------------------------------------------------------*
end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  get_po_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_data.
* Select Data from EKKO & EKPO for the given selection inputs

  SELECT   a~ebeln
           b~ebelp
           b~matnr
           b~werks
           b~lgort
           a~lifnr
           a~bstyp
           a~ekorg
           a~ekgrp
           a~kdatb
           a~kdate
           a~KNUMV
           a~aedat
                 INTO CORRESPONDING FIELDS OF TABLE it_PO
                 FROM ekko AS a INNER JOIN ekpo AS b
                     on a~ebeln EQ b~ebeln
                 WHERE  a~lifnr IN s_lifnr  and
                        a~ekorg eq p_EKORG  and
                        a~ebeln in s_ebeln  and
                        a~aedat in S_AEDAT  and
                        a~bsart in s_bsart  and
                        b~matnr IN s_matnr  and
                        a~loekz EQ space    and
                        b~loekz EQ space  .
*                       b~elikz EQ space.  "UD1K922199

  IF it_PO[] IS INITIAL.
    MESSAGE S999 WITH text-m01.
    stop.
  endif.
* Collect Distinct PO Condition Records
  loop at it_po.
    it_hcond-knumv = it_po-knumv.
    it_hcond-KPOSN = it_po-ebelp.
    collect it_hcond.
  endloop.

* Select PO condition records
  if not it_hcond[] is initial.
    select  KNUMV
            KPOSN
            KSCHL
            kdatu
            KBETR
            KPEIN
            from konv into table it_konv
                for all entries in it_hcond
           where  knumv = it_hcond-knumv  and
                  kposn = it_hcond-kposn.
  endif.

* FOR ZOA1 Condition Value is not Stored in Info Record Table.
* So apply below Logic for checking Price Discrepancies
  loop at s_kschl.
    check s_kschl-low eq 'ZOA1'.
    it_po_no[] = it_po[].
    delete adjacent duplicates from it_po_no.
    exit.
  endloop.

  if not it_po_no[] is initial.
    select  eikp~exnum EIPO~EXPOS EIPO~STAWN eikp~refnr
            into table it_exnum  from EIKP
            inner join eipo on eikp~exnum eq eipo~exnum
            for all entries in it_po_no
            where  eikp~REFNR eq it_po_no-ebeln.
  endif.

  if not it_exnum[] is initial.
    select a~VAKEY b~KBETR b~KPEIN
           a~DATAB a~DATBI into table it_konh
              from konh as a inner join konp as b on
                   a~KNUMH eq b~KNUMH and
                   a~kappl eq b~kappl and
                   a~kschl eq b~kschl
              for all entries in it_exnum
              where a~KAPPL eq 'M'  and
                    a~KSCHL eq 'ZOA1' and
                    a~VAKEY eq it_exnum-stawn. " and
*                    a~DATAB <= sy-datum and
*                    a~DATBI >= sy-datum.

* Collect ALL ZOA1 condition Values in One table
    loop at it_exnum.
      read table it_konh with key vakey = it_exnum-stawn.
      if sy-subrc eq 0.
        it_ZOA1-EBELN = it_exnum-refnr.
        it_ZOA1-ebelp = it_exnum-expos.
        it_ZOA1-VAKEY = it_exnum-stawn.
        it_ZOA1-KBETR = it_konh-kbetr.
        it_ZOA1-KPEIN = it_konh-kpein.
        it_ZOA1-datab = it_konh-datab.
        it_ZOA1-datbi = it_konh-datbi.
        append it_ZOA1 . clear it_ZOA1.
      endif.
    endloop.
  endif.

ENDFORM.                    " get_po_data
*&---------------------------------------------------------------------*
*&      Form  get_info_record_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_record_data.
  DATA : l_knumh LIKE konh-knumh,
         l_datab LIKE a018-datab,
         l_datbi LIKE a018-datbi.

  refresh it_info_rec.
  clear :l_knumh, l_datab, l_datbi,
         it_info_rec.

  if P_P1  eq 'X' .
    SELECT SINGLE knumh datab datbi
          INTO (l_knumh, l_datab, l_datbi)
     FROM a018
          WHERE  kappl EQ 'M' and
                 kschl EQ 'PB00' and
                 lifnr EQ it_po-lifnr and
                 matnr EQ it_po-matnr and
                 datab <= sy-datum and
                 datbi >= sy-datum.
  else.
    SELECT SINGLE knumh datab datbi
          INTO (l_knumh, l_datab, l_datbi)
     FROM a018
          WHERE  kappl EQ 'M' and
                 kschl EQ 'PB00' and
                 lifnr EQ it_po-lifnr and
                 matnr EQ it_po-matnr and
                 datab <= it_po-aedat and
                 datbi >= it_po-aedat.
  endif.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_info_rec
           FROM konp
          WHERE knumh EQ l_knumh
            AND loevm_ko EQ space
            AND kbetr > 0.

  clear wa_info_rec.
  wa_info_rec-datab =  l_datab.
  wa_info_rec-datbi =  l_datbi.
  modify it_info_rec from wa_info_rec transporting datab datbi
           where datab is initial..
ENDFORM.                    " get_info_record_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  data flag .

  sort it_po by ebeln ebelp.

  loop at it_PO.

* Read Info Record conditions
    perform get_info_record_data.

* Compare condition record prices for PB00
*    perform compare_price using 'PB00'.

* Compare Price Based on Condition Options choosen
    loop at s_kschl.
      perform compare_price using s_kschl-low.
    endloop.
  endloop.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  compare_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_price using p_kschl.
* Compare PB00 Price by Default
  clear : it_konv, it_info_rec, it_ZOA1.
* PO Condition Price
  read table it_KONV with key KNUMV = it_po-knumv
                              KPOSN = it_po-ebelp
                              KSCHL =  p_kschl.
  if  sy-subrc eq 0.
   if p_kschl eq 'ZOA1'.
     read table it_ZOA1 with key EBELN =  it_po-ebeln
                                ebelp =  it_po-ebelp.
      it_info_rec-kbetr = it_ZOA1-kbetr.
      if it_info_rec-kbetr > 0.
      it_info_rec-kbetr = it_info_rec-kbetr / 10.
    endif.
    if it_konv-kbetr > 0.
      it_konv-kbetr  = it_konv-kbetr / 10.
    endif.
    it_info_rec-kpein = it_ZOA1-kpein.
    it_info_rec-DATBI = it_ZOA1-DATBI.
    it_info_rec-DATAB = it_ZOA1-DATAB.
  else.
* Info record price
    read table it_info_rec with key  KSCHL = p_kschl.
  endif.
*  check sy-subrc eq 0.

  if  p_r1  eq 'X'.   "if discrepancies Option is choosen
    IF it_konv-kbetr <> it_info_rec-kbetr or
       it_konv-kpein <> it_info_rec-kpein .
      move-corresponding it_po to it_output.
*      it_output-kdatb  =  it_po-kdatb.
*      it_output-kdate  =  it_po-kdate.
      it_output-aedat  = it_po-aedat.
      it_output-PKBETR =  it_konv-kbetr.
      it_output-PKPEIN =  it_konv-KPEIN.
      it_output-DATBI  =  it_info_rec-DATBI.
      it_output-DATAB  =  it_info_rec-DATAB.
      it_output-IKBETR =  it_info_rec-kbetr.
      it_output-IKPEIN =  it_info_rec-KPEIN.
      it_output-KSCHL  =  p_kschl.
      Append it_output. clear it_output.
    endif.
  else.               "Show All records
    move-corresponding it_po to it_output.
*    it_output-kdatb  =  it_po-kdatb.
*    it_output-kdate  =  it_po-kdate.
    it_output-aedat  = it_po-aedat.
    it_output-PKBETR =  it_konv-kbetr.
    it_output-PKPEIN =  it_konv-KPEIN.
    it_output-DATBI  =  it_info_rec-DATBI.
    it_output-DATAB  =  it_info_rec-DATAB.
    it_output-IKBETR =  it_info_rec-kbetr.
    it_output-IKPEIN =  it_info_rec-KPEIN.
    it_output-KSCHL  =  p_kschl.
    Append it_output. clear it_output.
  endif.

 else.
* If condition is missing in PO then flag as error
    check  p_miss eq 'X'.
    move-corresponding it_po to it_output.
    MOVE : c_red             TO it_output-linecolor.
    move p_kschl to it_output-kschl.
    append it_output. clear it_output.
 endif.

ENDFORM.                    " compare_price
*&---------------------------------------------------------------------*
*&      Form  display_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_ALV_OUTPUT.

  CLEAR: w_fieldcat, w_fieldcat[], w_sortcat, w_sortcat[],
         w_layout .


  CLEAR : w_line, w_top_of_page, w_top_of_page[].
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.


* Build Field Catalog
  Perform build_fieldcatalog.
* Build Sort Table
  perform sort_field.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

    MOVE : 'LINECOLOR'  TO w_layout-info_fieldname,
            'X'         TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_output[]
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.


ENDFORM.                    " display_ALV_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.
*--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
  w_col_pos 'EBELN' 10 'Po. Number'      'CHAR' '' ''      '',
  w_col_pos 'EBELP' 05 'Item Number'    'NUMC' '' ''      '',
  w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
  w_col_pos 'WERKS' 04 'Plant'          'CHAR' ''  ''      '',
  w_col_pos 'LIFNR' 04 'Vendor'         'CHAR' ''  ''      '',
  w_col_pos 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',
  w_col_pos 'AEDAT' 10 'PO  Date'     'DATS' ''  ''      '',
*    w_col_pos 'KDATB' 10 'PO  From'     'DATS' ''  ''      '',
*    w_col_pos 'KDATE' 10 'PO To'       'DATS' ''  ''      '',
  w_col_pos 'PKBETR' 12 'Amount'      'CURR' ''  ''      '',
  w_col_pos 'PKPEIN' 4  'Un/p'        'DEC' ''  ''      '',

  w_col_pos 'DATAB' 10 'Info From'      'DATS' ''  ''      '',
  w_col_pos 'DATBI' 10 'Info To'        'DATS' ''  ''      '',
  w_col_pos 'IKBETR' 12 'Amount'         'CURR' ''  ''      '',
  w_col_pos 'IKPEIN' 4  'Un/p'           'DEC' ''  ''      ''.


ENDFORM.                    " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: l_subject(40) TYPE c VALUE 'PO / Info Record Price Compare'.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          gd_cnt TYPE i,
          tab_lines type i,
          gd_sent_all(1) TYPE c,
          gd_doc_data LIKE sodocchgi1,
          gd_error TYPE sy-subrc.

* Compose Email Body
  PERFORM Compose_EMAIL_body.

  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  describe table it_mail lines tab_lines.
  read  table it_mail index tab_lines.

 it_packing_list-doc_size = ( tab_lines - 1 ) * 255 + strlen( it_mail ).
  it_packing_list-transf_bin = 'X'.
  it_packing_list-head_start = 1.
  it_packing_list-head_num   = 0.
  it_packing_list-body_start = 1.
  it_packing_list-body_num   = tab_lines.
  it_packing_list-doc_type   = 'RAW'.
  it_packing_list-obj_name   = 'ATTACHMENT'.
  it_packing_list-obj_descr  = 'PO_INFO_REC_PRICE'.
  APPEND it_packing_list.

***  Append 'Please check attached file for PO/Info. Record price
***discrepancies.' to  it_ebody.
  Append text-m02  to  it_ebody.

  Append '' to  it_ebody.
  Append 'Thanks' to  it_ebody.
  Append 'SAP MM TEAM' to it_ebody.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
  it_receivers-receiver = p_dist.
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = 'X'.
  it_receivers-notif_ndel = 'X'.
  APPEND it_receivers.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1' starting new task 'T1'
       EXPORTING
            document_data              = gd_doc_data
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
       TABLES
            packing_list               = it_packing_list
            contents_txt               = it_ebody
            contents_bin               = it_MAIL
            receivers                  = it_receivers
       EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.
 IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " send_email
*&---------------------------------------------------------------------*
*&      Form  Compose_EMAIL_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Compose_EMAIL_body.

  DATA: l_message TYPE so_text255,
          l_kpein(4),
          l_kbetr(12).

  CLEAR: it_mail,it_mail[].
* Heading
  it_body-ebeln = 'Po.No.'.
  it_body-ebelp = 'Line'.
  it_body-matnr = 'Material No.'.
  it_body-werks = 'Plant'.
  it_body-lgort = 'Sloc'.
  it_body-lifnr = 'Vendor'.
  it_body-KSCHL = 'COND'.
  it_body-aedat = 'PODate'.
  it_body-PKBETR = '      PO PRICE'.
  it_body-PKPEIN = 'P/U'.
  it_body-DATBI  = 'Info. Fr'.
  it_body-DATAB = 'Info. To'.
  it_body-IKBETR = '     Info Price'.
  it_body-IKPEIN = ' P/U'.
  Append it_body to it_mail.
  clear: it_body.
* Data
  LOOP AT it_output.
    l_kbetr = it_output-pkbetr.
    l_kpein = it_output-pkpein.
    move it_output-ebeln to  it_body-ebeln .
    move it_output-ebelp to  it_body-ebelp .
    move it_output-matnr to  it_body-matnr.
    move it_output-werks to  it_body-werks .
    move it_output-lgort to  it_body-lgort .
    move it_output-lifnr to  it_body-lifnr.
    move it_output-kschl to  it_body-KSCHL.
    move it_output-aedat to  it_body-aedat .
    move it_output-pkbetr to it_body-PKBETR .
    move it_output-pkpein to it_body-PKPEIN.
    move it_output-datbi  to it_body-DATBI .
    move it_output-datab  to it_body-DATAB .
    move it_output-IKBETR to it_body-IKBETR.
    move it_output-IKPEIN to it_body-IKPEIN.
    APPEND it_body  TO it_mail.
    CLEAR: it_mail, it_body.
  ENDLOOP.

ENDFORM.                    " Compose_EMAIL_body
*&---------------------------------------------------------------------*
*&      Form  sort_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_field.
*  append_sortcat : '1' 'EBELN' 'IT_OUTPUT' 'X' '',
*                   '2' 'EBELP' 'IT_OUTPUT' 'X' '',
*                   '3' 'MATNR' 'IT_OUTPUT' 'X' '',
*                   '4' 'WERKS' 'IT_OUTPUT' 'X' '',
*                   '5' 'LGORT' 'IT_OUTPUT' 'X' ''.

ENDFORM.                    " sort_field
