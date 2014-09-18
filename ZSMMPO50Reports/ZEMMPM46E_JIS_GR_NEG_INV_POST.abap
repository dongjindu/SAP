************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZEMMPM46E_JIS_GR_NEG_INV_POST                              *
*& Type   : Report/Interface (Copy of ZEMMPM46E_JIS_GR program )       *
*& Author : Manju                                                      *
*& Title  : JIS GR Program to clear negative in previous  Month        *
*&---------------------------------------------------------------------*
* Help Desk Request No: 629H23527A                                     *
* System Id           : 20060209-001                                   *
*                                                                      *
*   Requested by:   Richard Davis                                      *
*   Assigned to:    Manjunath Venkatesh                                *
*   Original Request #:                                                *
*   ABAP Analyst:   Manjunath Venkatesh                                *
*                                                                      *
* Business Users:   Andy Choi                                          *
*                                                                      *
* Business Requirement Description:                                    *
* Negative inventory keeps occuring in the previous period for JIS     *
* Parts. This is mainly do to a bad business partice of canceling      *
* production in the current period and posting to previous period.     *
* Currently CO & PC correct this manually. So this new program will    *
* create GR for negative stock materials.                              *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >
* If Records exits in T001B Table for current YEAR & Period then
* program execution stops else program creates GR's.
* Select Data from EKKO , EKPO and MARDH which has Negative stock for  *
* the previous Period. Create GR's based on Vendors.
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
* 02/16/06    Manju        UD1K919466   Initial Coding
* 04/17/06    Manju        UD1K920139   Read MARD for Negative inventory
*                                       records apart from MARDH
************************************************************************
REPORT zemmpm46e_jis_gr_neg_inv_post NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID zmmm.
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : t001b,
         marv.

*-------------------------------------------------------------*
* Include
*-------------------------------------------------------------*
INCLUDE : zrmmpmxxr_incl.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
*--- Internal Tables

CONSTANTS: l_lgort_499 LIKE ekpo-lgort VALUE 'P499'.
DATA : BEGIN OF wa_itab ,
         lifnr LIKE ekko-lifnr,     " vendor
         name1 LIKE lfa1-name1,     " vendor desc.
         ebeln LIKE ekko-ebeln,     " scheduling agreement number
         matnr LIKE ekpo-matnr,     " material number
         txz01 LIKE ekpo-txz01,     " material desc.
         labst LIKE mard-labst,     " quantity
         meins LIKE ekpo-meins,     " unit of measure
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         ebelp LIKE ekpo-ebelp,
         mblnr LIKE mkpf-mblnr,     " GR document number
         mjahr LIKE mkpf-mjahr,
         messa(80),
         linecolor(4),     " ALV Color
       END OF wa_itab.
DATA: BEGIN OF wa_bapi_suc,
         lifnr LIKE ekko-lifnr,     " vendor
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         mblnr LIKE mkpf-mblnr,     " GR document number
         mjahr LIKE mkpf-mjahr,
         sumsg(80) TYPE c,
      END OF wa_bapi_suc.
DATA: BEGIN OF wa_bapi_err,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        ermsg LIKE bapiret2-message,
       END OF wa_bapi_err.
DATA: wa_print TYPE slis_print_alv.
DATA : it_itab LIKE TABLE OF wa_itab,
       it_bapi_suc LIKE TABLE OF wa_bapi_suc,
       it_bapi_err LIKE TABLE OF wa_bapi_err.
FIELD-SYMBOLS: <fs_itab> LIKE LINE OF it_itab.

**--- BAPI
DATA : w_goodsmvt_header  LIKE bapi2017_gm_head_01,
       w_goodsmvt_code    LIKE bapi2017_gm_code,
       w_goodsmvt_headret LIKE bapi2017_gm_head_ret,
       w_materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
       w_matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
       it_goodsmvt_item
               LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE,
       it_goodsmvt_serialnumber
               LIKE TABLE OF bapi2017_gm_serialnumber WITH HEADER LINE,
       it_return LIKE TABLE OF bapiret2.

DATA : w_uzeit TYPE t.
*&-----Parallel process
DATA: w_taskname(4) TYPE n VALUE '0001',
      w_snd_jobs    TYPE i VALUE 1,
      w_rcv_jobs    TYPE i VALUE 1,
      w_excep_flag  TYPE c.
*&---end

DATA: w_classname LIKE rzllitab-classname.

DATA : l_gjahr LIKE t009b-bdatj,
       l_perio LIKE t009b-poper,
       lv_datum LIKE sy-datum.


*-------------------------------------------------------------*
* Constants
*--------------------------------------------------------------*
CONSTANTS : c_bstyp LIKE ekko-bstyp VALUE 'L',
            c_bsart LIKE ekko-bsart VALUE 'JIS',
            c_mtart LIKE ekpo-mtart VALUE 'ROH',
            c_lgort LIKE ekpo-lgort VALUE 'P500',
            c_bwart LIKE mseg-bwart VALUE '101',
            c_bukrs LIKE t001b-bukrs VALUE 'H201',
            c_gm_code LIKE w_goodsmvt_code-gm_code VALUE '01',
            c_kzbew LIKE bapi2017_gm_item_create-mvt_ind VALUE 'B',
            c_uzeit_000000 TYPE t VALUE '000000',
            c_uzeit_055959 TYPE t VALUE '055959'.


*-------------------------------------------------------------*
* Macros
*--------------------------------------------------------------*
**--- Macro
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

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.
*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln,
                 s_matnr FOR ekpo-matnr.
*SELECTION-SCREEN ULINE.
*PARAMETERS : p_budat LIKE mkpf-budat.
SELECTION-SCREEN END OF BLOCK block1.

PARAMETERS: p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_JIS' ,
            p_test AS CHECKBOX DEFAULT 'X',
            p_noent(3) TYPE n DEFAULT 30.

*-------------------------------------------------------------*
* AT Selection-Screen.
*--------------------------------------------------------------*


AT SELECTION-SCREEN.
  SELECT SINGLE classname INTO w_classname
                          FROM rzllitab
                          WHERE classname = p_srvgrp.
  IF sy-subrc NE 0.
    MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '009' WITH text-003.
    LEAVE PROGRAM.
  ENDIF.

*-------------------------------------------------------------*
* INITIALIZATION.
*--------------------------------------------------------------*
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
*-------------------------------------------------------------*
* TOP-OF-PAGE
*--------------------------------------------------------------*

TOP-OF-PAGE.
  PERFORM top_of_page.
*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
START-OF-SELECTION.

* Check Whether Posting Allowed
  PERFORM validate_posting.

* Get last day of Previous Period
  PERFORM get_previous_period.

* Select DATA from EKKO, EKPO , MARDH and MARD
  PERFORM get_data.


*-------------------------------------------------------------*
* END-of- selection
*--------------------------------------------------------------*

END-OF-SELECTION.

  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
* POST  GR for above selected Records
    PERFORM posting_gr.
    FREE: it_bapi_err, it_bapi_suc,it_goodsmvt_item,
          it_goodsmvt_serialnumber, w_goodsmvt_header,
           w_goodsmvt_code, w_goodsmvt_headret.

    PERFORM comment_build.     " USING w_top_of_page[].
* Display Log as ALV LIST
    PERFORM make_alv_grid.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_labst LIKE mard-labst,
        l_index LIKE sy-tabix.
  DATA : lt_itab_temp LIKE TABLE OF wa_itab.
*---
*  CLEAR : it_temp, it_temp[], it_itab, it_itab[].
  CLEAR : it_itab, it_itab[].

  SELECT a~lifnr name1 a~ebeln b~matnr b~txz01
                 labst b~meins b~werks b~lgort b~ebelp
       INTO TABLE it_itab
       FROM ekko AS a
       INNER JOIN ekpo AS b
       ON a~mandt EQ b~mandt
       AND a~ebeln EQ b~ebeln
       INNER JOIN mardh AS c
       ON b~mandt EQ c~mandt
       AND b~matnr EQ c~matnr
       AND b~werks EQ c~werks
       INNER JOIN lfa1 AS d
       ON a~mandt EQ d~mandt
       AND a~lifnr EQ d~lifnr
       WHERE a~bstyp EQ c_bstyp   " document category : L
       AND a~bsart EQ c_bsart     " document type : JIS
       AND b~mtart EQ c_mtart     " material type : ROH
       AND b~lgort EQ c_lgort     " storage location : P500
       AND a~loekz EQ space       " Del. Ind.
       AND b~loekz EQ space       " Del. Ind.
       AND c~lfgja EQ l_gjahr     " Current Fiscal YEAR
       AND c~lfmon EQ l_perio     " Previous Period
       AND c~labst < 0            " Negative Stock
       AND c~matnr IN s_matnr     " material number
       AND c~lgort EQ c_lgort     " storage location
       AND a~ebeln IN s_ebeln     " scheduling agreement
       AND b~elikz EQ space.      " delivery compl. ind.

** Added by Furong on 06/24/10
** Changed on 02/03/11
*  LOOP AT IT_ITAB INTO WA_ITAB.
*    SELECT SINGLE LABST INTO L_LABST
*      FROM MARDH
*      WHERE MATNR = WA_ITAB-MATNR
*        AND WERKS = 'P001'
*        AND LGORT = L_LGORT_499
*        AND LFGJA = L_GJAHR
*        AND LFMON = L_PERIO.
*    IF SY-SUBRC = 0.
*      WA_ITAB-LABST = WA_ITAB-LABST + L_LABST.
*      MODIFY IT_ITAB FROM WA_ITAB.
*      CLEAR: WA_ITAB.
*    ENDIF.
*  ENDLOOP.
  LOOP AT it_itab INTO wa_itab.
    SELECT SINGLE labst INTO l_labst
      FROM mardh
      WHERE matnr = wa_itab-matnr
        AND werks = 'P001'
        AND lgort = l_lgort_499
        AND lfgja = l_gjahr
        AND lfmon = l_perio.
    IF sy-subrc = 0.
      wa_itab-labst = wa_itab-labst + l_labst.
    ENDIF.
    SELECT SINGLE labst INTO l_labst
       FROM mard
       WHERE matnr = wa_itab-matnr
         AND werks = 'P001'
         AND lgort = l_lgort_499
         AND lfgja = l_gjahr
         AND lfmon = l_perio.
    IF sy-subrc = 0.
      wa_itab-labst = wa_itab-labst + l_labst.
    ENDIF.
    MODIFY it_itab FROM wa_itab.
    CLEAR: wa_itab.
  ENDLOOP.

** End of change on 02/03/11
** End of addition

* Sometimes   Negative inventory records during Month end will not
*follow down to MARDH ..So Also READ MARD table for Negative Inventory

* Begin of changes - UD1K920139

** Changed by Furong on 06/24/10
*  SELECT A~LIFNR NAME1 A~EBELN B~MATNR B~TXZ01
*                   LABST B~MEINS B~WERKS B~LGORT B~EBELP
*         APPENDING  TABLE IT_ITAB
*         FROM EKKO AS A
*         INNER JOIN EKPO AS B
*         ON A~MANDT EQ B~MANDT
*         AND A~EBELN EQ B~EBELN
*         INNER JOIN MARD AS C
*         ON B~MANDT EQ C~MANDT
*         AND B~MATNR EQ C~MATNR
*         AND B~WERKS EQ C~WERKS
*         INNER JOIN LFA1 AS D
*         ON A~MANDT EQ D~MANDT
*         AND A~LIFNR EQ D~LIFNR
*         WHERE A~BSTYP EQ C_BSTYP   " document category : L
*         AND A~BSART EQ C_BSART     " document type : JIS
*         AND B~MTART EQ C_MTART     " material type : ROH
*         AND B~LGORT EQ C_LGORT     " storage location : P500
*         AND A~LOEKZ EQ SPACE       " Del. Ind.
*         AND B~LOEKZ EQ SPACE       " Del. Ind.
*         AND C~LFGJA EQ L_GJAHR     " Current Fiscal YEAR
*         AND C~LFMON EQ L_PERIO     " Previous Period
*         AND C~LABST < 0            " Negative Stock
*         AND C~MATNR IN S_MATNR     " material number
*         AND C~LGORT EQ C_LGORT     " storage location
*         AND A~EBELN IN S_EBELN     " scheduling agreement
*         AND B~ELIKZ EQ SPACE.      " delivery compl. ind.

  SELECT a~lifnr name1 a~ebeln b~matnr b~txz01
                    labst b~meins b~werks b~lgort b~ebelp
          APPENDING  TABLE lt_itab_temp
          FROM ekko AS a
          INNER JOIN ekpo AS b
          ON a~mandt EQ b~mandt
          AND a~ebeln EQ b~ebeln
          INNER JOIN mard AS c
          ON b~mandt EQ c~mandt
          AND b~matnr EQ c~matnr
          AND b~werks EQ c~werks
          INNER JOIN lfa1 AS d
          ON a~mandt EQ d~mandt
          AND a~lifnr EQ d~lifnr
          WHERE a~bstyp EQ c_bstyp   " document category : L
          AND a~bsart EQ c_bsart     " document type : JIS
          AND b~mtart EQ c_mtart     " material type : ROH
          AND b~lgort EQ c_lgort     " storage location : P500
          AND a~loekz EQ space       " Del. Ind.
          AND b~loekz EQ space       " Del. Ind.
          AND c~lfgja EQ l_gjahr     " Current Fiscal YEAR
          AND c~lfmon EQ l_perio     " Previous Period
          AND c~labst < 0            " Negative Stock
          AND c~matnr IN s_matnr     " material number
          AND c~lgort EQ c_lgort     " storage location
          AND a~ebeln IN s_ebeln     " scheduling agreement
          AND b~elikz EQ space.      " delivery compl. ind.

** Changed on 02/03/11
*  LOOP AT LT_ITAB_TEMP INTO WA_ITAB.
*    SELECT SINGLE LABST INTO L_LABST
*      FROM MARD
*      WHERE MATNR = WA_ITAB-MATNR
*        AND WERKS = 'P001'
*        AND LGORT = L_LGORT_499
*        AND LFGJA = L_GJAHR
*        AND LFMON = L_PERIO.
*    IF SY-SUBRC = 0.
*      WA_ITAB-LABST = WA_ITAB-LABST + L_LABST.
*      MODIFY LT_ITAB_TEMP FROM WA_ITAB.
*      CLEAR: WA_ITAB.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lt_itab_temp INTO wa_itab.
    SELECT SINGLE labst INTO l_labst
      FROM mard
      WHERE matnr = wa_itab-matnr
        AND werks = 'P001'
        AND lgort = l_lgort_499
        AND lfgja = l_gjahr
        AND lfmon = l_perio.
    IF sy-subrc = 0.
      wa_itab-labst = wa_itab-labst + l_labst.
    ENDIF.
    SELECT SINGLE labst INTO l_labst
      FROM mardh
      WHERE matnr = wa_itab-matnr
        AND werks = 'P001'
        AND lgort = l_lgort_499
        AND lfgja = l_gjahr
        AND lfmon = l_perio.
    IF sy-subrc = 0.
      wa_itab-labst = wa_itab-labst + l_labst.
    ENDIF.
    MODIFY lt_itab_temp FROM wa_itab.
    CLEAR: wa_itab.
  ENDLOOP.

** End of change on 02/03/11

  APPEND LINES OF lt_itab_temp TO it_itab.

  LOOP AT it_itab INTO wa_itab.
    l_index = sy-tabix.
    IF wa_itab-labst >= 0.
      DELETE it_itab INDEX l_index.
    ENDIF.
    CLEAR: wa_itab.
  ENDLOOP.

** End of addition on 06/24/10


* End of changes - UD1K920139

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.
*  wa_print-print = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
*            is_print           = wa_print
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  posting_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_gr.
*---
  DATA: BEGIN OF wa_vendor,
          lifnr LIKE lfa1-lifnr,
          count TYPE i,
        END OF wa_vendor.
  DATA: it_vendor LIKE TABLE OF wa_vendor.

  DATA: w_idx TYPE i VALUE 1,
        w_cnt TYPE i,
        w_grp_cnt TYPE i.

  CLEAR: w_uzeit.

  MOVE: sy-uzeit TO w_uzeit.
*&---calculate no.of records for each vendor.
  LOOP AT it_itab ASSIGNING <fs_itab>.
    wa_vendor-lifnr = <fs_itab>-lifnr.
    wa_vendor-count = 1.
    COLLECT wa_vendor INTO it_vendor.
  ENDLOOP.

  SORT: it_vendor BY lifnr,
        it_itab BY lifnr ebeln.
  LOOP AT it_vendor INTO wa_vendor.
    LOOP AT it_itab ASSIGNING <fs_itab> FROM w_idx.
      IF wa_vendor-lifnr NE <fs_itab>-lifnr.
        w_idx = sy-tabix.
        CLEAR : w_cnt, w_grp_cnt,
                w_goodsmvt_header, w_goodsmvt_code,w_goodsmvt_headret,
                w_materialdocument, w_matdocumentyear,
                it_goodsmvt_item, it_goodsmvt_item[],
                it_goodsmvt_serialnumber, it_goodsmvt_serialnumber[].
        EXIT.
      ELSE.
        w_cnt = w_cnt + 1.
        w_grp_cnt = w_grp_cnt + 1.
* Fill BAPI ITEM Structure
        PERFORM clear_data_move.
        IF w_cnt EQ p_noent OR w_grp_cnt EQ wa_vendor-count.
          PERFORM call_bapi.
          CLEAR : w_cnt,w_goodsmvt_header,w_goodsmvt_code,
                w_goodsmvt_headret,w_materialdocument,w_matdocumentyear,
                  it_goodsmvt_item, it_goodsmvt_item[],
                  it_goodsmvt_serialnumber, it_goodsmvt_serialnumber[].
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
  PERFORM modify_itab.
ENDFORM.                    " posting_gr

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
    w_col_pos 'NAME1' 20 'Vendor Name'    'CHAR' 'X' ''      '',
    w_col_pos 'EBELN' 10 'SA Number'      'CHAR' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'TXZ01' 20 'Material Name'  'CHAR' ''  ''      '',
    w_col_pos 'LABST' 12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'MBLNR' 10 'Document No'    'CHAR' ''  ''      '',
    w_col_pos 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'LIFNR' 'IT_ITAB' 'X' '',
                   '2' 'EBELN' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  clear_data_move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data_move.

*---* Fill BAPI ITEM Structure
  MOVE : <fs_itab>-matnr     TO it_goodsmvt_item-material,
         <fs_itab>-werks     TO it_goodsmvt_item-plant,
         <fs_itab>-lgort     TO it_goodsmvt_item-stge_loc,
         c_bwart             TO it_goodsmvt_item-move_type,
         <fs_itab>-lifnr     TO it_goodsmvt_item-vendor,
*         it_itab-labst     TO it_goodsmvt_item-entry_qnt,
         <fs_itab>-meins     TO it_goodsmvt_item-entry_uom,
         <fs_itab>-meins     TO it_goodsmvt_item-entry_uom_iso,
         <fs_itab>-ebeln     TO it_goodsmvt_item-po_number,
         <fs_itab>-ebelp     TO it_goodsmvt_item-po_item,
         c_kzbew           TO it_goodsmvt_item-mvt_ind.
  it_goodsmvt_item-entry_qnt = <fs_itab>-labst * -1.

  APPEND it_goodsmvt_item.
ENDFORM.                    " clear_data_move

*&---------------------------------------------------------------------*
*&      Form  call_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi.
*---
  DATA : l_budat TYPE d.
* FILL HEADER BAPI Structure
*  IF p_budat IS INITIAL.
*    IF w_uzeit GE c_uzeit_000000 AND
*       w_uzeit LE c_uzeit_055959.
*      l_budat = sy-datum - 1.
*    ENDIF.
* Last Day of Previous Month
  l_budat = lv_datum.
  MOVE : l_budat   TO w_goodsmvt_header-pstng_date.   " posting date
*  ELSEIF NOT p_budat IS INITIAL.
*    MOVE : p_budat   TO w_goodsmvt_header-pstng_date.   " posting date
*  ENDIF.

  l_budat = lv_datum.
  CONCATENATE  'JIS-GR' w_goodsmvt_header-pstng_date
          INTO w_goodsmvt_header-ref_doc_no.

  MOVE : sy-datum  TO w_goodsmvt_header-doc_date,
         c_gm_code TO w_goodsmvt_code-gm_code.

*  CALL Parallel Processing Function Module to create GR's
  CALL FUNCTION 'Z_FMM_JISGR_CREATE'
     STARTING NEW TASK w_taskname
     DESTINATION IN GROUP p_srvgrp
     PERFORMING gr_info ON END OF TASK
   EXPORTING
     wa_gdsmvt_hdr  = w_goodsmvt_header
     wa_gdsmvt_code = w_goodsmvt_code
     w_tstrun       = p_test
   TABLES
     it_gdsmvt_itm = it_goodsmvt_item
     it_gdsmvt_sno = it_goodsmvt_serialnumber
     it_ret        = it_return
   EXCEPTIONS
     communication_failure = 1
     system_failure        = 2
     RESOURCE_FAILURE      = 3.

  CASE sy-subrc.
    WHEN 0.
      w_taskname = w_taskname + 1.
      w_snd_jobs = w_snd_jobs + 1.
    WHEN 1 OR 2.
      w_excep_flag = 'X'.
    WHEN 3.
      IF w_excep_flag = space.
        w_excep_flag = 'X'.
        WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.01' SECONDS.
      ELSE.
        WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.1' SECONDS.
      ENDIF.
      IF sy-subrc EQ 0.
        CLEAR w_excep_flag.
      ELSE.
        EXIT.
      ENDIF.
  ENDCASE.

ENDFORM.                    " call_bapi

*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab.

  CLEAR: wa_itab.
* Gather All Sucessfully / Failed  GR'S.
  LOOP AT it_bapi_suc INTO wa_bapi_suc.
    wa_itab-mblnr     = wa_bapi_suc-mblnr.
    wa_itab-mjahr     = wa_bapi_suc-mjahr.
    wa_itab-messa     = wa_bapi_suc-sumsg.
    wa_itab-linecolor = c_green.
    MODIFY it_itab FROM wa_itab
                   TRANSPORTING linecolor messa mblnr mjahr
                   WHERE lifnr = wa_bapi_suc-lifnr
                   AND   ebeln = wa_bapi_suc-ebeln
                   AND   ebelp = wa_bapi_suc-ebelp.
    CLEAR: wa_itab.
  ENDLOOP.
  LOOP AT it_bapi_err INTO wa_bapi_err.
    wa_itab-messa     = wa_bapi_err-ermsg.
    wa_itab-linecolor = c_red.
    MODIFY it_itab FROM wa_itab TRANSPORTING messa linecolor
                                WHERE ebeln = wa_bapi_err-ebeln
                                AND   ebelp = wa_bapi_err-ebelp.
    CLEAR: wa_itab.
  ENDLOOP.

ENDFORM.                    " modify_itab
*&---------------------------------------------------------------------*
*&      Form  gr_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gr_info USING w_taskname.

  DATA: wa_item_info LIKE bapi2017_gm_item_create.

  DATA: it_item_info LIKE TABLE OF wa_item_info.

  DATA: w_lifnr LIKE ekko-lifnr.

  FIELD-SYMBOLS: <fs_ret> LIKE LINE OF it_return.

  RECEIVE RESULTS FROM FUNCTION 'Z_FMM_JISGR_CREATE'
           IMPORTING
             wa_gdsmvt_hdrrtn = w_goodsmvt_headret
             w_matdoc         = w_materialdocument
             w_mdyear         = w_matdocumentyear
          TABLES
            it_gdsmvt_itm = it_goodsmvt_item
            it_gdsmvt_sno = it_goodsmvt_serialnumber
            it_ret        = it_return
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2.

  IF sy-subrc NE 0.
    w_excep_flag = 'X'.
    EXIT.
  ENDIF.

  w_rcv_jobs = w_rcv_jobs + 1.

  READ TABLE it_return ASSIGNING <fs_ret> WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    LOOP AT it_return ASSIGNING <fs_ret>.
      READ TABLE it_goodsmvt_item INDEX <fs_ret>-row.
      IF sy-subrc NE 0.
      ELSE.
        wa_bapi_err-ebeln = it_goodsmvt_item-po_number.
        wa_bapi_err-ebelp = it_goodsmvt_item-po_item.
        wa_bapi_err-ermsg = <fs_ret>-message.
        APPEND wa_bapi_err TO it_bapi_err.
      ENDIF.
      CLEAR: it_goodsmvt_item, wa_bapi_err.
    ENDLOOP.
  ELSE.
    it_item_info[] = it_goodsmvt_item[].
    LOOP AT it_item_info INTO wa_item_info.
      wa_bapi_suc-lifnr = wa_item_info-vendor.
      wa_bapi_suc-ebeln = wa_item_info-po_number.
      wa_bapi_suc-ebelp = wa_item_info-po_item.
      wa_bapi_suc-mblnr = w_materialdocument.
      wa_bapi_suc-mjahr = w_matdocumentyear.
      wa_bapi_suc-sumsg = 'GR Document created'.
      APPEND wa_bapi_suc TO it_bapi_suc.
      CLEAR: wa_bapi_suc.
    ENDLOOP.
  ENDIF.
  REFRESH: it_return, it_item_info.
ENDFORM.                    " gr_info
*&---------------------------------------------------------------------*
*&      Form  get_previous_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_previous_period.
* Get Last Day of previous Period
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr        = l_gjahr
            i_periv        = 'K4'
            i_poper        = l_perio
       IMPORTING
            e_date         = lv_datum
       EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.

ENDFORM.                    " get_previous_period
*&---------------------------------------------------------------------*
*&      Form  Validate_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_posting.
* Read Current Fiscal Year and Period .
* IF Fiscal Year & Current Period of MARV matches with T001B ..Exit
* Program.. Don't Proceed.
  SELECT SINGLE * FROM marv WHERE bukrs EQ c_bukrs.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM t001b
                    WHERE rrcty EQ '0'
                      AND bukrs EQ c_bukrs
                      AND mkoar EQ 'M'
                      AND frye1 EQ marv-lfgja
                      AND frpe1 EQ marv-lfmon.
    IF sy-subrc EQ 0.
      MESSAGE s999 WITH text-m04.
      EXIT.
      STOP.
    ELSE.
* Get Previous Period
      l_gjahr = marv-vmgja.
      l_perio = marv-vmmon.
    ENDIF.
  ENDIF.
ENDFORM.                    " Validate_posting
