************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZACOR26
*& Type   : Report                                                     *
*& Author : Manju                                                      *
*& Title  : Program to post Inventory Reconciliation account
*           for Valuation class change for Moving AVG. change for
*           materials
*&---------------------------------------------------------------------*
* Help Desk Request No  : 67KF453644                                   *
* System Id:                                                           *
*                                                                      *
*   Requested by:        Andy Choi                                     *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:  Manjunath Venkatesh                                 *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
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
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 07/25/06    Manju        UD1K921485   Initial Coding
************************************************************************
REPORT zacor26 LINE-SIZE 132 LINE-COUNT 65 NO STANDARD PAGE HEADING
MESSAGE-ID db.


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : mbewh.

INCLUDE <icon>.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
DATA : BEGIN OF it_tab OCCURS 0.
        INCLUDE STRUCTURE mbewh.
DATA:   bklas2 LIKE mbewh-bklas, "OLD VC
        diff   LIKE mbewh-salk3,
        icon   LIKE zsalv_inv_diff-icon,
        msg    LIKE zsalv_inv_diff-msg,
        END OF it_tab.

*DATA : itab_post LIKE TABLE OF it_tab WITH HEADER LINE.

DATA : BEGIN OF itab_post OCCURS 0,
        keys(10) TYPE c,
        lfgja  LIKE mbewh-lfgja,
        lfmon  LIKE mbewh-lfmon,
        bklas  LIKE mbewh-bklas,
        bklas2 LIKE mbewh-bklas, "OLD VC
        bwkey  LIKE mbewh-bwkey,
        matnr  LIKE mbewh-matnr,
        diff   LIKE mbewh-salk3,
        icon   LIKE zsalv_inv_diff-icon,
       END OF itab_post.

DATA : BEGIN OF out_tab OCCURS 0,
        bwkey LIKE mbewh-bwkey,
        matnr LIKE mbewh-matnr,
        lfgja LIKE mbewh-lfgja,
        lfmon LIKE mbewh-lfmon,
        bklas LIKE mbewh-bklas,
        bwtar LIKE mbewh-bwtar,
        lbkum LIKE mbewh-lbkum,
        salk3 LIKE mbewh-salk3,
        vprsv LIKE mbewh-vprsv,
        verpr LIKE mbewh-verpr,
        stprs LIKE mbewh-stprs,
        peinh LIKE mbewh-peinh,
        salkv LIKE mbewh-salkv,
        vksal LIKE mbewh-vksal,
       END OF out_tab.

DATA wa_mbewh LIKE LINE OF out_tab.

DATA : BEGIN OF it_t030 OCCURS 0,
        bklas LIKE t030-bklas,
        konts LIKE t030-konts,
       END OF it_t030.

DATA :   c1 TYPE cursor,
         l_bklas LIKE mbewh-bklas,
         l_rcnt TYPE i,
         l_cnt TYPE i,
         l_act1(10) TYPE c,
         l_act2(10) TYPE c.

* ALV GRID
DATA: alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

* Global variables for attributes or etc of ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

* BAPI Variables :-
DATA:
      obj_type LIKE bapiache02-obj_type,
      obj_key LIKE bapiache02-obj_key,
      obj_sys LIKE bapiache02-obj_sys,

      documentheader  LIKE bapiache08,
      accountgl       LIKE bapiacgl08  OCCURS 0 WITH HEADER LINE,
      currencyamount  LIKE bapiaccr08  OCCURS 0 WITH HEADER LINE,
      return          LIKE bapiret2    OCCURS 0 WITH HEADER LINE,
      extension1      LIKE bapiextc    OCCURS 0 WITH HEADER LINE,

      t_edidd         LIKE edidd       OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  LIKE bapiret2    OCCURS 0 WITH HEADER LINE .

DATA : wa_variant TYPE disvariant. "for parameter IS_VARIANT
DATA:  wa_save    TYPE c   VALUE 'A'.   "for Parameter I_SAVE



*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mbewh-matnr,
                 s_bklas FOR mbewh-bklas.
SELECTION-SCREEN END   OF BLOCK b1.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*

START-OF-SELECTION.
* Select Data
  PERFORM select_data.

* Display Material with Valuation class change
  PERFORM display_alv_grid.

*-------------------------------------------------------------*
* End-of-selection
*--------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  DATA: flag TYPE c,
        first TYPE c.

* Select Standard Accounts Table
  SELECT bklas konts INTO TABLE it_t030
    FROM t030  WHERE ktosl EQ 'BSX'.

* Select All materials
  SELECT *  FROM mbewh INTO CORRESPONDING FIELDS OF TABLE out_tab
  WHERE matnr IN s_matnr
    AND bklas IN s_bklas
    AND vprsv EQ 'V'
    AND lbkum <> 0.

  SORT out_tab BY bwkey matnr lfgja lfmon.

*  loop at it_tab.
*    move-corresponding it_tab to out_tab.
*    Append  out_tab.
*  endloop.

* For Each Material check whether Valulation class has changed from
* Previous Period. If so, Collect all those records
  first = 'X'.
  LOOP AT out_tab.
    l_rcnt = sy-tabix.
    AT NEW matnr.
      flag = 'X'.
    ENDAT.
    IF flag = 'X' AND first  IS INITIAL.
      IF l_bklas NE ''.
        IF out_tab-bklas NE l_bklas.
          l_rcnt = l_rcnt - 1.
          MOVE-CORRESPONDING out_tab TO it_tab.

          READ TABLE out_tab INTO wa_mbewh INDEX l_rcnt.
          IF sy-subrc EQ 0.
            it_tab-bklas2 = l_bklas.
            it_tab-diff   = wa_mbewh-salkv -
                            ( wa_mbewh-lbkum * wa_mbewh-stprs ).
            MOVE icon_light_out TO it_tab-icon.
            APPEND it_tab.

*            MOVE-CORRESPONDING wa_mbewh TO it_tab.
*            APPEND it_tab.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    l_bklas = out_tab-bklas.
    CLEAR first.
    AT END OF  matnr.
      CLEAR: flag, l_bklas.
    ENDAT.
  ENDLOOP.
* Calculation Difference for Posting..
*( Total value - Qty * Std Price )
*  LOOP AT it_tab.
*    it_tab-diff = it_tab-salk3 -  ( it_tab-lbkum * it_tab-stprs ) .
*    MOVE icon_light_out TO it_tab-icon.
*    MODIFY it_tab.
*  ENDLOOP.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  display_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_grid.
  CALL SCREEN '100'.
ENDFORM.                    " display_ALV_GRID
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MYPFS'.
  SET TITLEBAR 'TITBAR'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_ALV_OBJECT  OUTPUT
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
ENDMODULE.                 " create_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
  DATA : w_repid LIKE sy-repid.
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
*&      Form  create_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_grid.

  CALL METHOD alv_grid->set_table_for_first_display
        EXPORTING
                  i_structure_name = 'ZSALV_INV_DIFF'
                  is_layout        = wa_is_layout
                  i_save           = wa_save
                  is_variant       = wa_variant
                  i_default        = space
        CHANGING  it_fieldcatalog  = it_fieldcat[]
                  it_outtab        = it_tab[].

ENDFORM.                    " create_grid
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'POST'.
      PERFORM select_rows.

      PERFORM post_adjustments.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  post_adjustments
*&---------------------------------------------------------------------*
FORM post_adjustments.
  DATA: l_cnt LIKE sy-tabix.
*  describe tables itab_post lines l_cnt.

  SORT itab_post BY keys.
* CALL BAPI to POST Inventory Adjustments
  LOOP AT itab_post.
    ADD 1 TO l_cnt.

    AT NEW keys.
      PERFORM fill_bapi_header.
    ENDAT.

* CALL BAPI ITEM
    PERFORM fill_bapi_item.
* Call BAPI
    IF l_cnt >= 500.
      PERFORM call_bapi_fm.
      CLEAR l_cnt.
    ENDIF.

    AT END OF keys.
      IF l_cnt < 500.
        PERFORM call_bapi_fm.
        CLEAR l_cnt.
      ENDIF.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " post_adjustments
*&---------------------------------------------------------------------*
*&      Form  select_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_rows.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        w_repid LIKE sy-repid.
  REFRESH : itab_post.

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
  IF sy-subrc EQ 0.
    REFRESH out_tab. CLEAR out_tab.
    LOOP AT lt_rows WHERE index NE 0.
      READ TABLE it_tab INDEX lt_rows-index.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_tab TO itab_post.
        MOVE: icon_light_out TO itab_post-icon.
        CONCATENATE itab_post-lfgja itab_post-lfmon itab_post-bklas
                INTO itab_post-keys.
        APPEND itab_post. "out_tab.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT it_tab.
      MOVE-CORRESPONDING it_tab TO itab_post.
      CONCATENATE itab_post-lfgja itab_post-lfmon itab_post-bklas
             INTO itab_post-keys.
      APPEND itab_post. "out_tab.
    ENDLOOP.
  ENDIF.

  CLEAR itab_post.
ENDFORM.                    " select_rows
*&---------------------------------------------------------------------*
*&      Form  call_BAPI_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_fm.
  refresh return.

  CALL FUNCTION 'BAPI_ACC_GL_POSTING_POST'
       EXPORTING
            documentheader = documentheader
       IMPORTING
            obj_type       = obj_type
            obj_key        = obj_key
            obj_sys        = obj_sys
       TABLES
            accountgl      = accountgl
            currencyamount = currencyamount
            return         = return.

  READ TABLE return WITH KEY type = 'E'.
  IF sy-subrc <> 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    LOOP AT return where type = 'E'.
      WRITE:/ return-type, return-id, return-number, return-message.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " call_BAPI_FM
*&---------------------------------------------------------------------*
*&      Form  FIll_BAPI_HEADER
*&---------------------------------------------------------------------*
FORM fill_bapi_header.
  DATA: lv_datum LIKE sy-datum,
        lv_bdatj LIKE t009b-bdatj,
        lv_poper LIKE t009b-poper.

  lv_bdatj = itab_post-keys(4).
  lv_poper = itab_post-keys+4(2).
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = lv_bdatj
            i_periv = 'K4'
            i_poper = lv_poper
       IMPORTING
            e_date  = lv_datum.

  documentheader-username       = sy-uname.
  documentheader-comp_code      = 'H201'.
  documentheader-doc_type       = 'SA'.
  documentheader-fisc_year      = lv_datum(4).
  documentheader-fis_period     = lv_datum+4(2).
  documentheader-doc_date       = lv_datum.
  documentheader-pstng_date     = lv_datum.
  documentheader-trans_date     = lv_datum.

  documentheader-ref_doc_no = 'VAL-VAR'.
  documentheader-header_txt = 'Valuation Class Change Adj'.
  .

*lineitem currency
  currencyamount-currency    = 'USD'.

ENDFORM.                    " FIll_BAPI_HEADER
*&---------------------------------------------------------------------*
*&      Form  fill_BAPI_ITEm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bapi_item.
  concatenate 'VC CHG ADJ: ' itab_post-bklas2 '->' itab_post-bklas
         into accountgl-item_text.
  accountgl-alloc_nmbr = itab_post-matnr.
* Pick GL Account based on Material Valutation
  CLEAR :l_act1,l_act2.
  READ TABLE it_t030 WITH KEY bklas = itab_post-bklas.
  IF sy-subrc EQ 0.
    l_act1 = it_t030-konts.
*    l_act1 = '0000999111'.
  ENDIF.

  READ TABLE it_t030 WITH KEY bklas = itab_post-bklas2.
  IF sy-subrc EQ 0.
    l_act2 = it_t030-konts.
*    l_act2 = '0000999112'.
  ENDIF.

* 1st lineitem
  l_cnt = l_cnt + 1.
  MOVE: l_cnt        TO accountgl-itemno_acc,
        l_cnt        TO currencyamount-itemno_acc,
        l_act1       TO accountgl-gl_account,
        itab_post-diff  TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

* 2nd lineitem for credit
  itab_post-diff = - itab_post-diff.
  l_cnt = l_cnt + 1.
  MOVE: l_cnt         TO accountgl-itemno_acc,
        l_cnt         TO currencyamount-itemno_acc,
        l_act2        TO accountgl-gl_account,
        itab_post-diff  TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

ENDFORM.                    " fill_BAPI_ITEm
