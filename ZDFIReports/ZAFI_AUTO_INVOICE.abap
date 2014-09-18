***************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA co.                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAFI_AUTO_INVOICE                                          *
*& Type   : Report                                                     *
*& Author : Manjunath Venkatesh                                        *
*& Title  : Auto Invoice   Report                                      *
*&---------------------------------------------------------------------*
* Request Identification:                                              *
*   System Id:    5CD963571A                                           *
*   Requested by:        ANDY CHOI                                     *
*   Assigned to:         Manjunath Venkatesh                           *
*   Original Request #:                                                *
*   ABAP Analyst:        Manjunath Venkatesh                           *
*                                                                      *
*   Business Users:                                                    *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *

* Sum of F records - Sum of M records per PO / PO line / Vendor
* combinations
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
*     < No direct updates to SAP tables are allowed.  List custom >    *
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
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 12/12/05    Manju        UD1K918657   Initial Coding
* 04/04/2006  Manju        UD1K919976   Program changes
************************************************************************
REPORT zafi_auto_invoice LINE-SIZE 255 LINE-COUNT 65
                              NO STANDARD PAGE HEADING MESSAGE-ID db .


*-------------------------------------------------------------*
* TOP Include
*-------------------------------------------------------------*
INCLUDE zfiautotop.

*******************************************************
*initilaisation
*******************************************************


*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS :     p_bukrs LIKE bkpf-bukrs OBLIGATORY DEFAULT 'H201',
                 p_kschl LIKE ekbz-kschl OBLIGATORY DEFAULT 'ZTIR'.
SELECT-OPTIONS : s_lifnr FOR ekko-lifnr.
*parameter :      p_budat like s001-spmon.
SELECT-OPTIONS : s_ebeln FOR ekbz-ebeln.
SELECTION-SCREEN END OF BLOCK block1.


SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS :     p_erdat LIKE ekbz-budat OBLIGATORY DEFAULT sy-datum,
                 p_price RADIOBUTTON GROUP r1,
                 p_no    RADIOBUTTON  GROUP r1.
*                p_chk as checkbox .
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-004.
PARAMETERS :     p_back AS  CHECKBOX MODIF ID abc,
                 p_bpost RADIOBUTTON GROUP r2,
                 p_bpark RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK block3.

AT SELECTION-SCREEN  .
  PERFORM set_dates.

INITIALIZATION.
*  move sy-datum to P_budat.
*----------------------------------------------------------*
* Start of  Selection
*----------------------------------------------------------*
START-OF-SELECTION.

*---------------------------------------------------------*
* Data Selection
*---------------------------------------------------------*

  PERFORM select_data.


*-----------------------------------------------------------*
* End of  Selection
*-----------------------------------------------------------*
END-OF-SELECTION.

  IF p_back EQ ''.
    CALL SCREEN 100.
  ENDIF.

  IF p_back EQ 'X' AND p_bpost  EQ 'X'.
    APPEND LINES OF it_tab TO out_tab.
    PERFORM post_invoice_bg.
  ELSEIF p_back EQ 'X' AND p_bpark EQ 'X'.
    APPEND LINES OF it_tab TO out_tab.
    PERFORM park_invoice_bg.
  ENDIF.

*Perform post_invoice.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'D100' .
  SET TITLEBAR 'D120'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'ABCK' OR 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'PARK'.
      CLEAR: sy-ucomm, p_park.
      p_park = 'X'.
      PERFORM pre_validations.
    WHEN  'POST'.
      CLEAR: sy-ucomm,p_post.
      p_post = 'X'.
      PERFORM pre_validations.
*    WHEN 'REFRESH'.
*      Perform select_data.
*      perform display_grid.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
  CREATE OBJECT grid_container
           EXPORTING container_name = 'MY_CONT'
           EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
                   i_appl_events = 'X'.


ENDFORM.                    " create_container
*&---------------------------------------------------------------------*
*&      Form  set_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog.
  FIELD-SYMBOLS:
      <ls_fieldcat>    TYPE lvc_t_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*     I_PROGRAM_NAME               = sy-repid
      i_internal_tabname           = 'GT_LIST'
      i_structure_name             = 'ZFI_AUTO_INVOICE'
      i_bypassing_buffer           = 'X'
    CHANGING
      ct_fieldcat                  = it_fieldcat
    EXCEPTIONS
      inconsistent_interface       = 1
      program_error                = 2
      OTHERS                       = 3.


ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  create_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_grid.
  CALL METHOD alv_grid->set_table_for_first_display
       EXPORTING i_structure_name = 'ZFI_AUTO_INVOICE'
                 is_layout        = wa_is_layout
                 i_save           = wa_save
                 is_variant       = wa_variant
                 i_default        = space
       CHANGING  it_fieldcatalog  = it_fieldcat[]
                 it_outtab        = it_tab[].

ENDFORM.                    " create_grid
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  Pre_validations
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_validations.
  PERFORM get_selected_rows.
*  Perform per_validations.
  PERFORM call_invoiceparking_bapi.
ENDFORM.                    " Pre_validations
*&---------------------------------------------------------------------*
*&      Module  create_ALV_Object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  IF grid_container IS INITIAL.
    PERFORM create_container.
    PERFORM set_attributes.
*    perform build_field_catalog.
    PERFORM create_grid.
  ENDIF.
ENDMODULE.                 " create_ALV_Object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_rows.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  REFRESH out_tab. CLEAR out_tab.
  LOOP AT lt_rows WHERE index NE 0.
    READ TABLE it_tab INDEX lt_rows-index.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_tab TO out_tab.
      MOVE: icon_light_out TO out_tab-icon.
      APPEND out_tab.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  per_validations
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM per_validations.
* Calculate accumulated Qty of GR and then subtract Invoice Qty for
* selected PO
  LOOP AT it_tab.
    wa_qty = wa_qty + it_tab-menge.
    AT END OF ebeln.
      flag = 'X'.
    ENDAT.
    IF flag = 'X'.
      MOVE-CORRESPONDING it_tab TO out_tab.
      SELECT SUM( menge ) INTO wa_qty1 FROM
          ekbz WHERE ebeln = it_tab-ebeln
                  AND bewtp = 'M' .
      IF sy-subrc EQ 0 AND wa_qty1 > 0  .
        out_tab-menge = wa_qty - wa_qty1.
      ELSE.
        out_tab-menge = wa_qty.
      ENDIF.
* Check Validaty date of ZTIR condition type and if multiple condition
* records exists then apply that conidtion rate
*     SELECT single *  FROM a018
*   WHERE kappl =  'M'
*     AND kschl =  'PB00'
*     AND matnr = it_tab-matnr
*     AND lifnr =  it_tab-lifnr
*     AND ekorg =  'PU01'
*     AND esokz =  '0'
*     AND datbi >= s_budat-low
*     AND datab <= s_budat-high.
*      if sy-subrc eq 0.
*      endif.
      APPEND out_tab.
      CLEAR: flag,wa_qty.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " per_validations
*&---------------------------------------------------------------------*
*&      Form  call_invoiceparking_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_invoiceparking_bapi.

  IF p_post  EQ 'X'.
    PERFORM post_invoice.
  ELSE.
    PERFORM park_invoice.
  ENDIF.


  CALL METHOD alv_grid->set_table_for_first_display
         EXPORTING i_structure_name = 'ZFI_AUTO_INVOICE'
                   is_layout        = wa_is_layout
                   i_save           = wa_save
                   is_variant       = wa_variant
                   i_default        = space
         CHANGING  it_fieldcatalog  = it_fieldcat[]
                   it_outtab        = it_tab[].

ENDFORM.                    " call_invoiceparking_bapi
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bapi_structure.

  ADD 1 TO w_line.

  ind = 'X'.

* Fill Header Data
  IF  flag1 = 'X'.
    MOVE : ind              TO  headerdata-invoice_ind,
           'RE'             TO  headerdata-doc_type,
           p_bukrs          TO  headerdata-comp_code,
           out_tab-dmbtr    TO  headerdata-gross_amount,
           out_tab-waers    TO  headerdata-currency,
           sy-datum         TO  headerdata-doc_date,
           p_erdat          TO  headerdata-pstng_date,
           sy-uname         TO  headerdata-person_ext,
           ''               TO  headerdata-calc_tax_ind.
    IF out_tab-lifnr NE ''    .                             "UD1K920317
      MOVE out_tab-lifnr   TO headerdata-diff_inv.          "UD1K920317
    ENDIF.                                                  "UD1K920317

    CLEAR flag.
  ENDIF.

* FIll Item data
  SELECT SINGLE * FROM ekpo
                    WHERE  ebeln = out_tab-ebeln
                       AND ebelp = out_tab-ebelp.
  IF sy-subrc EQ 0 AND ekpo-webre EQ 'X'.
    SELECT SINGLE * FROM ekbe
            WHERE ebeln = out_tab-ebeln
             AND  ebelp = out_tab-ebelp.
    IF sy-subrc EQ 0.
      MOVE :  ekbe-belnr  TO    itemdata-ref_doc,
              ekbe-gjahr  TO    itemdata-ref_doc_year,
              ekbe-buzei  TO    itemdata-ref_doc_it.
    ENDIF.
  ENDIF.
  MOVE :     w_line                  TO  itemdata-invoice_doc_item,
             out_tab-ebeln           TO  itemdata-po_number,
             out_tab-ebelp           TO  itemdata-po_item,
                         ''          TO  itemdata-item_text,
             'U0'                    TO  itemdata-tax_code,
             out_tab-dmbtr           TO  itemdata-item_amount,
             out_tab-menge           TO  itemdata-quantity,
             out_tab-meins           TO  itemdata-po_unit,
             p_kschl                 TO  itemdata-cond_type.
  IF out_tab-lifnr NE '' .
    MOVE out_tab-lifnr   TO itemdata-freight_ven. "
  ENDIF.
  APPEND   itemdata.



ENDFORM.                    " fill_bapi_structure
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  FIELD-SYMBOLS <fs>.
  DATA : l_dmbtr  LIKE ekbz-dmbtr.

* Select Data
  SELECT   ekbz~ebeln
           ekbz~ebelp
           ekbz~menge
           ekbz~dmbtr
           ekbz~waers
           ekbz~shkzg
           ekko~lifnr
           ekbz~lifnr
           ekpo~matnr
           ekpo~meins
           ekbe~lfbnr
           INTO TABLE it_po
           FROM ekbz
           INNER JOIN ekko
                   ON ekbz~ebeln = ekko~ebeln
                    INNER JOIN ekpo
                    ON ekbz~ebeln = ekpo~ebeln AND
                       ekbz~ebelp = ekpo~ebelp
* by ig.moon 1/11/08 {
           INNER JOIN ekbe
                   ON  ekbe~ebeln = ekbz~ebeln
                   AND ekbe~ebeln = ekbz~ebeln
                   AND ekbe~ebelp = ekbz~ebelp
                   AND ekbe~gjahr = ekbz~gjahr
                   AND ekbe~belnr = ekbz~belnr
                   AND ekbe~buzei = ekbz~buzei
* }
             WHERE
                    ekbz~lifnr IN s_lifnr AND
                    ekbz~ebeln IN s_ebeln AND
                    ekbz~kschl EQ p_kschl AND
                    ekbz~bewtp EQ 'F'     AND
                    ekko~bukrs EQ p_bukrs.

  SELECT   ekbz~ebeln
           ekbz~ebelp
           ekbz~menge
           ekbz~dmbtr
           ekbz~waers
           ekbz~shkzg
           ekbz~bewtp
           ekko~lifnr
           ekbz~lifnr
           ekpo~matnr
           ekpo~meins
           ekbz~belnr
           ekbe~lfbnr
           INTO TABLE it_po1
           FROM ekbz
           INNER JOIN ekko
                   ON ekbz~ebeln = ekko~ebeln
                    INNER JOIN ekpo
                    ON ekbz~ebeln = ekpo~ebeln AND
                       ekbz~ebelp = ekpo~ebelp
* by ig.moon 1/11/08 {
           INNER JOIN ekbe
                   ON  ekbe~ebeln = ekbz~ebeln
                   AND ekbe~ebeln = ekbz~ebeln
                   AND ekbe~ebelp = ekbz~ebelp
                   AND ekbe~gjahr = ekbz~gjahr
                   AND ekbe~belnr = ekbz~belnr
                   AND ekbe~buzei = ekbz~buzei
* }
             WHERE
                    ekbz~lifnr IN s_lifnr AND
                    ekbz~ebeln IN s_ebeln AND
                    ekbz~kschl EQ p_kschl AND
                    ekbz~bewtp IN ('M','T')  AND
                    ekko~bukrs EQ p_bukrs.

  LOOP AT it_po.
    MOVE-CORRESPONDING it_po TO it_tabpo.
    COLLECT it_tabpo.
  ENDLOOP.

  LOOP AT it_po1 WHERE bewtp = 'M'.
    MOVE-CORRESPONDING it_po1 TO it_tabpo1.
    COLLECT it_tabpo1.
  ENDLOOP.

  LOOP AT it_tabpo.
    l_dmbtr =  it_tabpo-dmbtr / it_tabpo-menge .
    l_cnt = sy-tabix.
    LOOP AT it_tabpo1 WHERE ebeln = it_tabpo-ebeln
                     AND ebelp = it_tabpo-ebelp
                     AND lifnr = it_tabpo-lifnr.
      IF it_tabpo1-shkzg = 'S'.
        it_tabpo-menge = it_tabpo-menge - it_tabpo1-menge.
      ELSE.
        it_tabpo-menge = it_tabpo-menge + it_tabpo1-menge.
      ENDIF.
    ENDLOOP.
    it_tabpo-dmbtr =  l_dmbtr * it_tabpo-menge .

    MODIFY it_tabpo TRANSPORTING menge dmbtr.
  ENDLOOP.

  DELETE it_po1 WHERE bewtp = 'M'.


* Check Validatity Dates of Condition records based on GR
* Posting Date and Split Invoice.

  DELETE  it_tabpo WHERE menge <=  0.

  LOOP AT it_tabpo .
    l_index = sy-tabix.
    READ TABLE it_po  WITH KEY ebeln = it_tabpo-ebeln
                               ebelp = it_tabpo-ebelp.
    CHECK sy-subrc EQ 0.
    MOVE-CORRESPONDING it_tabpo TO it_tab.
    MOVE-CORRESPONDING it_po TO it_tab.

* When new pricing is choosen - Pull and Apply rate from
* Schedule line Info record then check material info rec Condition table
    IF p_price EQ 'X'.

      SELECT SINGLE *  FROM a016
      WHERE kappl =  'M'
      AND kschl =  'PB00'
      AND evrtn =  it_tabpo-ebeln
      AND evrtp =  it_tabpo-ebelp
      AND datab <= sy-datum   "Start Date
      AND datbi >= sy-datum.  "End Date
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM konp
         WHERE knumh = a016-knumh AND
               kschl EQ p_kschl AND
               loevm_ko EQ ''.
        IF sy-subrc EQ 0 AND konp-kbetr > 0 .
          it_tabpo-lifnr = konp-lifnr.
          it_tabpo-dmbtr = it_tabpo-menge * konp-kbetr.
        ELSEIF sy-subrc NE 0 OR konp-kbetr <= 0.
          SELECT SINGLE *  FROM a018
           WHERE kappl =  'M'
              AND kschl =  'PB00'
              AND matnr =  it_po-matnr
              AND lifnr =  it_po-lifnr_ekko
              AND ekorg =  'PU01'
              AND esokz =  '0'
              AND datab <= sy-datum " Start Date
              AND datbi >= sy-datum.  " End Date
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM konp
              WHERE knumh = a018-knumh AND
                   kschl EQ p_kschl AND
                   loevm_ko EQ ''.
            IF sy-subrc EQ 0  AND konp-kbetr > 0 AND
                   konp-lifnr NE '' .
              it_tabpo-lifnr = konp-lifnr.
              it_tabpo-dmbtr = it_tabpo-menge * konp-kbetr.
            ELSEIF sy-subrc EQ 0 AND   konp-kbetr > 0.
              it_tabpo-dmbtr = it_tabpo-menge * konp-kbetr.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.  " Not Record in A016
        IF it_tabpo-lifnr NE it_po-lifnr_ekko.
          it_po-lifnr = it_po-lifnr_ekko.
        ELSE.
          it_po-lifnr  = it_po-lifnr.
        ENDIF.
        SELECT SINGLE *  FROM a018
           WHERE kappl =  'M'
              AND kschl =  'PB00'
              AND matnr =  it_po-matnr
              AND lifnr =  it_po-lifnr
              AND ekorg =  'PU01'
              AND esokz =  '0'
              AND datab <= sy-datum " Start Date
              AND datbi >= sy-datum.  " End Date
        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM konp
                 WHERE knumh = a018-knumh AND
                      kschl EQ p_kschl AND
                      loevm_ko EQ ''.
          IF sy-subrc EQ 0  AND konp-kbetr > 0 AND
               konp-lifnr NE '' .
            it_tabpo-lifnr = konp-lifnr.
            it_tabpo-dmbtr = it_tabpo-menge * konp-kbetr.
          ELSEIF sy-subrc EQ 0 AND  konp-kbetr > 0.
            it_tabpo-dmbtr = it_tabpo-menge * konp-kbetr.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.  "P_price eq 'X'
    IF  it_tabpo-lifnr EQ ''.
      it_tabpo-lifnr  = it_po-lifnr_ekko.
    ENDIF.
    MOVE icon_light_out TO it_tab-icon.
    MODIFY  it_tabpo TRANSPORTING dmbtr lifnr.
  ENDLOOP.


  SORT  it_tabpo BY ebeln ebelp .


** Accumalate Qty for PO's
  LOOP AT it_tabpo.
    MOVE icon_light_out TO it_tab-icon.
    MOVE-CORRESPONDING it_tabpo TO it_tab.
    COLLECT it_tab.
  ENDLOOP.

* check Whether parking Documents exists.
  LOOP AT it_tab.
    READ TABLE it_po1 WITH KEY ebeln = it_tab-ebeln
                            ebelp = it_tab-ebelp
                            bewtp = 'T'.
    IF sy-subrc EQ 0.
      it_tab-belum = it_po1-belnr.
      it_tab-msg   = 'Parking Document exists'.
      it_tab-icon  = icon_yellow_light.
      MODIFY it_tab TRANSPORTING belum msg icon.
    ENDIF.
  ENDLOOP.

* by ig.moon 1/11/08 {
  DATA $it_tab LIKE it_tab OCCURS 0 WITH HEADER LINE.

  LOOP AT it_tab.
    $it_tab = it_tab.
    IF $it_tab-shkzg EQ 'S'.
      $it_tab-menge = -1 * $it_tab-menge.
      $it_tab-dmbtr = -1 * $it_tab-dmbtr.
    ENDIF.
    CLEAR $it_tab-shkzg.
    COLLECT $it_tab.
  ENDLOOP.
* }
  delete $it_tab where menge eq 0.
  CLEAR : it_tab[], it_tab.
  it_tab[] = $it_tab[].

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  set_dates
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_dates.
*LAST_DAY_OF_MONTHS
*  concatenate p_budat '01' into f_date.
*  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*       EXPORTING
*            DAY_IN            = f_date
*       IMPORTING
*            LAST_DAY_OF_MONTH = t_date
*       EXCEPTIONS
*            DAY_IN_NO_DATE    = 1
*            OTHERS            = 2.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  concatenate f_date+4(2)  f_date+6(2)  f_date+0(4) into f_date.
*  concatenate t_date+4(2)  t_date+6(2)  t_date+0(4) into t_date.
*

ENDFORM.                    " set_dates
*&---------------------------------------------------------------------*
*&      Form  post_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_invoice.

  CLEAR flag1.

  LOOP AT out_tab.
    MOVE out_tab-ebelp TO po_line-line_no.
    APPEND po_line.

    AT END OF ebeln.
      flag1 = 'X'.
    ENDAT.

    PERFORM fill_bapi_structure.
    IF   flag1 = 'X'.
*      Perform post_BDC_invoice.
      PERFORM post_bapi_invoice.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " post_invoice
*&---------------------------------------------------------------------*
*&      Form  park_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM park_invoice.
  LOOP AT out_tab.

    MOVE out_tab-ebelp TO po_line-line_no.
    APPEND po_line.

    AT END OF ebeln.
      flag1 = 'X'.
    ENDAT.

    PERFORM fill_bapi_structure.
    IF flag1 = 'X'.
*      perform Park_BDC_invoice.
      PERFORM bapi_park_invoice.
    ENDIF.
*    refresh xreturn. clear xreturn.
  ENDLOOP.
ENDFORM.                    " park_invoice
*&---------------------------------------------------------------------*
*&      Form  fill_PARK_bapi_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_park_bapi_structure.

  IF flag1 = 'X'.
    IF out_tab-shkzg = 'H'.
      ind = 'X'.
    ELSE.
      ind = ''.
    ENDIF.
* Fill Header Data
    MOVE : ind              TO  headerdata-invoice_ind,
           'RE'             TO  headerdata-doc_type,
           p_bukrs          TO  headerdata-comp_code,
           out_tab-dmbtr    TO  headerdata-gross_amount,
           'USD'            TO  headerdata-currency,
           sy-datum         TO  headerdata-doc_date,
           p_erdat          TO  headerdata-pstng_date,
          sy-uname          TO  headerdata-person_ext,
          out_tab-dmbtr     TO  headerdata-del_costs,
          space             TO  headerdata-del_costs_taxc,
          space             TO  headerdata-del_costs_taxj,
           sy-uname         TO  headerdata-person_ext.

    CLEAR flag.
  ENDIF.

* FIll Item data
*  if out_tab-MENGE > 0.
*    wa_dmtr  =   out_tab-dmbtr .
*  endif.
  SELECT SINGLE * FROM ekpo
                      WHERE  ebeln = out_tab-ebeln
                         AND ebelp = out_tab-ebelp.
  IF sy-subrc EQ 0 AND ekpo-webre EQ 'X'.
    SELECT SINGLE * FROM ekbe
            WHERE ebeln = out_tab-ebeln
             AND  ebelp = out_tab-ebelp.
    IF sy-subrc EQ 0.
      MOVE :  ekbe-belnr  TO    itemdata-ref_doc,
              ekbe-gjahr  TO    itemdata-ref_doc_year,
              ekbe-buzei  TO    itemdata-ref_doc_it.
    ENDIF.
  ENDIF.

  MOVE :     w_line                  TO  itemdata-invoice_doc_item,
             out_tab-ebeln           TO  itemdata-po_number,
             out_tab-ebelp           TO  itemdata-po_item,
            'BAPI INV'               TO  itemdata-item_text,
             'U0'                    TO  itemdata-tax_code,
             out_tab-dmbtr           TO  itemdata-item_amount,
             out_tab-menge           TO  itemdata-quantity,
             out_tab-meins           TO  itemdata-po_unit.
  APPEND   itemdata.

* For Delivery Cost Populate Extra record with Cond type
*  add 1 to w_line.
*  move :      w_line                 TO  ITEMDATA-INVOICE_DOC_ITEM,
*             'ZTIR'                  TO  ITEMDATA-COND_TYPE.
*  append itemdata.


* Fill Tax Structure
  MOVE 'U0' TO taxdata-tax_code.
  APPEND taxdata.
  CLEAR w_line.

ENDFORM.                    " fill_PARK_bapi_structure
*&---------------------------------------------------------------------*
*&      Form  post_BDC_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_bdc_invoice.
  DATA : l_date(10) TYPE c,
         l_cnt(5) TYPE n,
         l_data(256) TYPE c,
         l_data1(256) TYPE c.


  ADD 1 TO l_cnt.
  WRITE sy-datum  TO l_date .
  PERFORM dynpro USING:
        'X' 'SAPLMR1M'    '6000',
        ' ' 'BDC_CURSOR'  'INVFO-BLDAT',
        ' ' 'INVFO-BLDAT'  l_date,
        ' ' 'INVFO-BUDAT'  l_date,
        ' '  'RM08M-REFERENZBELEGTYP' '1',
        ' '   'RM08M-VORGANG' '1',
        ' '  'BDC_CURSOR' 'RM08M-EBELN',
        ' '  'RM08M-EBELN' out_tab-ebeln,
         ' ' 'BDC_CURSOR' 'RM08M-XWARE_BNK',
          ' ' 'RM08M-XWARE_BNK' '2',
        ' '  'BDC_OKCODE'  '=MSEL'.

  PERFORM dynpro USING:
        'X' 'SAPLMR1M'    '6220',
        ' ' 'BDC_CURSOR'  'RM08M-EBELN(01)'.
  LOOP AT po_line.
    IF flag1 = 'X'.
      PERFORM dynpro USING:
           ' ' 'RM08M-EBELP(01)' po_line-line_no.
      ADD 1 TO l_cnt.
      CLEAR flag1.
    ELSE.
      WRITE 'RM08M-EBELN' TO l_data.
      CONCATENATE 'RM08M-EBELN(' l_cnt ')' INTO l_data.
      WRITE 'RM08M-EBELP' TO l_data1.
      CONCATENATE 'RM08M-EBELP(' l_cnt ')' INTO l_data1.

      PERFORM dynpro USING:
      ' ' 'BDC_CURSOR'  l_data,
       ' '  l_data  out_tab-ebeln,
       ' '  l_data1 po_line-line_no.
      CLEAR: l_data, l_data1.

      IF l_cnt >= 8.
        PERFORM dynpro USING:
*        ' '  'BDC_OKCODE' '=PRUEFEN'.
        ' '  'BDC_OKCODE' '=MSEL'.
*        l_cnt = 1.
      ENDIF.
      ADD 1 TO l_cnt.
    ENDIF.
  ENDLOOP.

  PERFORM dynpro USING:
*  ' ' 'BDC_CURSOR' 'RM08M-XWARE_BNK',
*  ' ' 'RM08M-XWARE_BNK' '2',
  ' ' 'BDC_OKCODE'  '=CREA_ITEML'.

  PERFORM dynpro USING:
         'X' 'SAPLMR1M'    '6000',
         ' ' 'BDC_CURSOR'  'DRSEG-RBLGP(01)',
         ' ' 'BDC_OKCODE'  '=BU',
         ' ' 'BDC_OKCODE'  '/00',
         ' ' 'BDC_OKCODE' '/11'.


  CALL TRANSACTION 'MIRO' USING bdc_tab
                      OPTIONS FROM wa_option_ds
                      MESSAGES INTO messtab.

  IF   sy-subrc EQ 0.

    LOOP AT po_line.
      it_tab-icon = icon_green_light.
      it_tab-belum = sy-msgv1 .
      it_tab-msg = ' Invoice document created for delivery cost'.
      MODIFY it_tab TRANSPORTING icon msg belum
                                       WHERE   ebeln = out_tab-ebeln
                                        AND     ebelp = po_line-line_no.
    ENDLOOP.
  ELSE.
    it_tab-icon = icon_red_light.
    it_tab-belum = sy-msgv1 .

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = xreturn-id
              msgnr               = xreturn-number
              msgv1               = xreturn-message_v1
              msgv2               = xreturn-message_v2
              msgv3               = xreturn-message_v3
              msgv4               = xreturn-message_v4
         IMPORTING
              message_text_output = xreturn-message.


    LOOP AT po_line.
      it_tab-icon = icon_red_light.
      it_tab-msg =   xreturn-message.
      MODIFY it_tab TRANSPORTING icon msg WHERE   ebeln = out_tab-ebeln
                                        AND     ebelp = po_line-line_no.
    ENDLOOP.


  ENDIF.

  REFRESH : bdc_tab, po_line.
  CLEAR : l_cnt , flag1,po_line.

ENDFORM.                    " post_BDC_invoice
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1495   text
*      -->P_1496   text
*      -->P_1497   text
*----------------------------------------------------------------------*
FORM dynpro USING   dynbegin   p_name   value .

  IF dynbegin = 'X'.
    CLEAR bdc_tab.
    MOVE: p_name     TO bdc_tab-program,
          value    TO bdc_tab-dynpro,
          dynbegin TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE: p_name    TO bdc_tab-fnam,
          value   TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.



ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  set_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mode.
  CLEAR : wa_option_ds.
  wa_option_ds-dismode = c_mode.
  wa_option_ds-defsize = 'X'.
  wa_option_ds-updmode = 'S'.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  Park_BDC_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM park_bdc_invoice.
  DATA : l_date(10) TYPE c,
           l_cnt TYPE n,
           l_data(256) TYPE c,
           l_data1(256) TYPE c.


  ADD 1 TO l_cnt.
  WRITE sy-datum  TO l_date .

  PERFORM dynpro USING:
          'X' 'SAPLMR1M'    '6000',
          ' ' 'BDC_CURSOR'  'INVFO-BLDAT',
          ' ' 'INVFO-BLDAT'  l_date,
          ' ' 'INVFO-BUDAT'  l_date,
          ' '  'RM08M-REFERENZBELEGTYP' '1',
          ' '   'RM08M-VORGANG' '1',
          ' '  'BDC_CURSOR' 'RM08M-EBELN',
          ' '  'RM08M-EBELN' out_tab-ebeln,
          ' '  'BDC_OKCODE'  '=MSEL'.

  PERFORM dynpro USING:
        'X' 'SAPLMR1M'    '6220',
        ' ' 'BDC_CURSOR'  'RM08M-EBELN(01)'.
  LOOP AT po_line.
    IF flag1 = 'X'.
      PERFORM dynpro USING:
           ' ' 'RM08M-EBELP(01)' po_line-line_no.
      ADD 1 TO l_cnt.
      CLEAR flag1.
    ELSE.
      WRITE 'RM08M-EBELN' TO l_data.
      CONCATENATE 'RM08M-EBELN(' l_cnt ')' INTO l_data.
      WRITE 'RM08M-EBELP' TO l_data1.
      CONCATENATE 'RM08M-EBELP(' l_cnt ')' INTO l_data1.
      PERFORM dynpro USING:
                         ' ' 'BDC_CURSOR'  l_data,
                         ' '  l_data  out_tab-ebeln,
                         ' '  l_data1 po_line-line_no.
      CLEAR: l_data, l_data1.
      ADD 1 TO l_cnt.
    ENDIF.

  ENDLOOP.

  PERFORM dynpro USING:
   ' ' 'BDC_CURSOR' 'RM08M-XWARE_BNK',
   ' ' 'RM08M-XWARE_BNK' '2',
   ' ' 'BDC_OKCODE'  '=CREA_ITEML'.

  PERFORM dynpro USING:
         'X' 'SAPLMR1M'    '6000',
         ' ' 'BDC_CURSOR'  'DRSEG-RBLGP(01)',
         ' ' 'BDC_OKCODE'  '=BU',
         ' ' 'BDC_OKCODE'  '/00',
         ' ' 'BDC_OKCODE' '/11'.


  CALL TRANSACTION 'MIR7' USING bdc_tab
                      OPTIONS FROM wa_option_ds
                      MESSAGES INTO messtab.

  IF   sy-subrc EQ 0.

    LOOP AT po_line.
      it_tab-icon = icon_green_light.
      it_tab-belum = sy-msgv1 .
      it_tab-msg = ' Invoice Parked  for delivery cost'.
      MODIFY it_tab TRANSPORTING icon msg belum
                                       WHERE   ebeln = out_tab-ebeln
                                        AND     ebelp = po_line-line_no.
    ENDLOOP.
  ELSE.
    it_tab-icon = icon_red_light.
    it_tab-belum = sy-msgv1 .

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = xreturn-id
              msgnr               = xreturn-number
              msgv1               = xreturn-message_v1
              msgv2               = xreturn-message_v2
              msgv3               = xreturn-message_v3
              msgv4               = xreturn-message_v4
         IMPORTING
              message_text_output = xreturn-message.


    LOOP AT po_line.
      it_tab-icon = icon_red_light.
      it_tab-msg =   xreturn-message.
      MODIFY it_tab TRANSPORTING icon msg WHERE   ebeln = out_tab-ebeln
                                        AND     ebelp = po_line-line_no.
    ENDLOOP.


  ENDIF.

  REFRESH : bdc_tab, po_line.
  CLEAR : l_cnt , flag1,po_line.



ENDFORM.                    " Park_BDC_invoice
*&---------------------------------------------------------------------*
*&      Form  post_BAPi_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_bapi_invoice.

* Call Invoice Posting BAPI
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
       EXPORTING
            headerdata       = headerdata
       IMPORTING
            invoicedocnumber = invoicedocnumber
            fiscalyear       = fiscalyear
       TABLES
            itemdata         = itemdata
            taxdata          = taxdata
            return           = xreturn.


  IF NOT invoicedocnumber IS INITIAL.
    LOOP AT po_line.
      it_tab-icon = icon_green_light.
      it_tab-belum = invoicedocnumber .
      it_tab-msg = 'Invoice Document Created'.
      MODIFY it_tab TRANSPORTING icon belum msg
       WHERE   ebeln = out_tab-ebeln AND ebelp = po_line-line_no.
    ENDLOOP.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    LOOP AT xreturn WHERE type = 'E'.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = xreturn-id
                msgnr               = xreturn-number
                msgv1               = xreturn-message_v1
                msgv2               = xreturn-message_v2
                msgv3               = xreturn-message_v3
                msgv4               = xreturn-message_v4
           IMPORTING
                message_text_output = xreturn-message.
      EXIT.
    ENDLOOP.
    LOOP AT po_line.
      it_tab-msg =   xreturn-message.
      it_tab-icon = icon_red_light.
      MODIFY it_tab TRANSPORTING icon msg WHERE ebeln = out_tab-ebeln
                                        AND   ebelp = po_line-line_no.
    ENDLOOP.
    CALL FUNCTION  'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
  REFRESH : itemdata,po_line. CLEAR :headerdata,itemdata,po_line,w_line.
  REFRESH xreturn. CLEAR xreturn.
ENDFORM.                    " post_BAPi_invoice
*&---------------------------------------------------------------------*
*&      Form  BAPI_PARK_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_park_invoice.
* Call Invoice Posting BAPI
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
       EXPORTING
            headerdata       = headerdata
       IMPORTING
            invoicedocnumber = invoicedocnumber
            fiscalyear       = fiscalyear
       TABLES
            itemdata         = itemdata
            taxdata          = taxdata
            return           = xreturn.


  IF NOT invoicedocnumber IS INITIAL.
    LOOP AT po_line.
      it_tab-icon = icon_green_light.
      it_tab-belum = invoicedocnumber .
      it_tab-msg = 'Invoice Parked'.
      MODIFY it_tab TRANSPORTING icon belum msg
       WHERE   ebeln = out_tab-ebeln AND ebelp = po_line-line_no.
    ENDLOOP.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    LOOP AT xreturn WHERE type = 'E'.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = xreturn-id
                msgnr               = xreturn-number
                msgv1               = xreturn-message_v1
                msgv2               = xreturn-message_v2
                msgv3               = xreturn-message_v3
                msgv4               = xreturn-message_v4
           IMPORTING
                message_text_output = xreturn-message.
      EXIT.
    ENDLOOP.
    LOOP AT po_line.
      it_tab-msg =   xreturn-message.
      it_tab-icon = icon_red_light.
      MODIFY it_tab TRANSPORTING icon msg WHERE ebeln = out_tab-ebeln
                                          AND   ebelp = po_line-line_no.
    ENDLOOP.
    CALL FUNCTION  'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
  REFRESH : itemdata,po_line. CLEAR :headerdata,itemdata,po_line,w_line.
  REFRESH xreturn. CLEAR xreturn.


ENDFORM.                    " BAPI_PARK_INVOICE
*&---------------------------------------------------------------------*
*&      Form  display_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid.
*SOFT_REFRESH_TABLE_DISPLAY
*  IF grid_container IS INITIAL.
*    perform create_container.
*    PERFORM set_attributes.
*    perform build_field_catalog.
*CALL METHOD alv_grid->REFRESH_TABLE_DISPLAY
*       EXPORTING i_structure_name = 'ZFI_AUTO_INVOICE'
*                 is_layout        = wa_is_layout
*                 i_save           = wa_save
*                 is_variant       = wa_variant
*                 i_default        = space
*       CHANGING  it_fieldcatalog  = it_fieldcat[]
*                 it_outtab        = it_tab[].



*  endif.
ENDFORM.                    " display_grid
*&---------------------------------------------------------------------*
*&      Form  post_invoice_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_invoice_bg.

  LOOP AT out_tab.
    AT END OF ebeln.
      flag1 = 'X'.
    ENDAT.

    PERFORM fill_bapi_structure.
    IF   flag1 = 'X'.
      PERFORM post_bapi_invoice_bg.
    ENDIF.
  ENDLOOP.

  PERFORM write_log.

ENDFORM.                    " post_invoice_BG
*&---------------------------------------------------------------------*
*&      Form  park_invoice_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM park_invoice_bg.

  LOOP AT out_tab.
    AT END OF ebeln.
      flag1 = 'X'.
    ENDAT.

    PERFORM fill_bapi_structure.
    IF   flag1 = 'X'.
      PERFORM park_bapi_invoice_bg.
    ENDIF.
  ENDLOOP.

  PERFORM write_log.
ENDFORM.                    " park_invoice_BG
*&---------------------------------------------------------------------*
*&      Form  post_BAPi_invoice_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_bapi_invoice_bg.
* Call Invoice Posting BAPI
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
       EXPORTING
            headerdata       = headerdata
       IMPORTING
            invoicedocnumber = invoicedocnumber
            fiscalyear       = fiscalyear
       TABLES
            itemdata         = itemdata
            taxdata          = taxdata
            return           = xreturn.


  IF NOT invoicedocnumber IS INITIAL.

    out_tab-icon = icon_green_light.
    out_tab-belum = invoicedocnumber .
    out_tab-msg = 'Invoice Document Created'.
    MODIFY out_tab TRANSPORTING icon belum msg
     WHERE   ebeln = out_tab-ebeln .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    LOOP AT xreturn WHERE type = 'E'.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = xreturn-id
                msgnr               = xreturn-number
                msgv1               = xreturn-message_v1
                msgv2               = xreturn-message_v2
                msgv3               = xreturn-message_v3
                msgv4               = xreturn-message_v4
           IMPORTING
                message_text_output = xreturn-message.
      CONCATENATE out_tab-msg  xreturn-message INTO out_tab-msg.
    ENDLOOP.
    out_tab-icon = icon_red_light.
    MODIFY out_tab TRANSPORTING icon msg WHERE ebeln = out_tab-ebeln.
    CALL FUNCTION  'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
  REFRESH : itemdata,po_line. CLEAR :headerdata,itemdata,po_line,w_line.
  REFRESH xreturn. CLEAR xreturn.

ENDFORM.                    " post_BAPi_invoice_BG
*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_log.
  FORMAT COLOR 1.
  WRITE :1(100) ' Summary Log' CENTERED.
  WRITE :/1 'Vendor',
          12 'Material Number',
          32 'PO. NO.',
          44  'Item No.',
          54  '   Qty',
          66  '   Amt',
          78  'ID',
          90  'Invoice No.',
          107 'Message'.
  WRITE :/ ''.
  FORMAT COLOR OFF.
  LOOP AT out_tab.
    WRITE : 1(10)  out_tab-lifnr,
            12(18) out_tab-matnr,
            32(10) out_tab-ebeln,
            44(8)  out_tab-ebelp,
            54(10) out_tab-menge,
            66(10) out_tab-dmbtr,
            78(10) out_tab-icon,
            90(15) out_tab-belum,
            107(100) out_tab-msg.
    WRITE :/ ''.
  ENDLOOP.

ENDFORM.                    " write_log
*&---------------------------------------------------------------------*
*&      Form  park_BAPi_invoice_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM park_bapi_invoice_bg.
* Call Invoice Posting BAPI
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
       EXPORTING
            headerdata       = headerdata
       IMPORTING
            invoicedocnumber = invoicedocnumber
            fiscalyear       = fiscalyear
       TABLES
            itemdata         = itemdata
            taxdata          = taxdata
            return           = xreturn.


  IF NOT invoicedocnumber IS INITIAL.

    out_tab-icon = icon_green_light.
    out_tab-belum = invoicedocnumber .
    out_tab-msg = 'Invoice Parked'.
    MODIFY out_tab TRANSPORTING icon belum msg
     WHERE   ebeln = out_tab-ebeln .
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ELSE.
    LOOP AT xreturn WHERE type = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = xreturn-id
                msgnr               = xreturn-number
                msgv1               = xreturn-message_v1
                msgv2               = xreturn-message_v2
                msgv3               = xreturn-message_v3
                msgv4               = xreturn-message_v4
           IMPORTING
                message_text_output = xreturn-message.
      CONCATENATE out_tab-msg  xreturn-message INTO out_tab-msg.

    ENDLOOP.

    it_tab-icon = icon_red_light.
    MODIFY out_tab TRANSPORTING icon msg WHERE ebeln = out_tab-ebeln.
    CALL FUNCTION  'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
  REFRESH : itemdata,po_line. CLEAR :headerdata,itemdata,po_line,w_line.
  REFRESH xreturn. CLEAR xreturn.


ENDFORM.                    " park_BAPi_invoice_BG
