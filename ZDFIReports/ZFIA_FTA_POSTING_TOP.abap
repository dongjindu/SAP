*----------------------------------------------------------------------*
*   INCLUDE ZACOU123_TOP                                               *
*----------------------------------------------------------------------*
TABLES: ekbz, ekpo, ekbe, sscrfields, skat, lfa1, ekko, t001,
        *ztcou124, ztcou124,ztmm_duty_it, ztmm_duty_hd, *rbkp.
TYPE-POOLS: truxs.
*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*\
DATA it_ztmm_duty_hd     LIKE ztmm_duty_hd OCCURS 0 WITH HEADER LINE.
DATA it_ztmm_duty_it     LIKE ztmm_duty_it OCCURS 0 WITH HEADER LINE.
* Screen display
DATA: BEGIN OF it_data   OCCURS  0,
          icon(10)       TYPE  c,
          entno          LIKE  ztmm_duty_hd-entno,         "Entry No.
          seq            LIKE  ztmm_duty_it-seq,           "Seq
          ebeln          LIKE  ztmm_duty_it-ebeln,         "Document Number
          duty_amt       LIKE  ztmm_duty_it-duty_amt,      "amt
          land1          LIKE  lfa1-land1,                 "KR
* screen parameter
          acct11         LIKE  skat-saknr,                 "Accrual Acct
          text11(30)     TYPE  c,                          "text
          acct12         LIKE  skat-saknr,                 "Duty MPF
          amt012         LIKE  ztmm_duty_it-duty_amt,      "MPF Amt
*          text12(30)     TYPE  c,                          "text
          belnr          LIKE  ztmm_duty_hd-belnr,         "Parking Document Number
      END OF it_data.
* Screen sum
DATA: BEGIN OF it_summ_data OCCURS 0,
          entno          LIKE  ztmm_duty_hd-entno,         "Entry No.
          duty_amt       LIKE  ztmm_duty_it-duty_amt,      "amt
          land1          LIKE  lfa1-land1,                 "KR
          acct11         LIKE  skat-saknr,                 "Accrual Acct
          text11(30)     TYPE  c,                          "text
          acct12         LIKE  skat-saknr,                 "Duty MPF
          amt012         LIKE  ztmm_duty_it-duty_amt,      "MPF Amt
          no(05)         TYPE  c,
      END OF it_summ_data.
DATA it_summ_data_temp   LIKE  it_summ_data OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_balance_data OCCURS 0,
          entno          LIKE  ztmm_duty_hd-entno,         "Entry No.
          duty_amt       LIKE  ztmm_duty_it-duty_amt,      "amt
      END OF it_balance_data.
DATA : headerdata        LIKE  bapi_incinv_create_header,
       i_invoice         LIKE  rbkp-xrech,
       i_creditmemo      LIKE  rbkp-xrech,
*       invoicedocnumber  LIKE  bapi_incinv_fld-inv_doc_no,
       fiscalyear        LIKE  bapi_incinv_fld-fisc_year.
DATA : i_ztcou124        LIKE  ztcou124 OCCURS 0.
DATA :  p_data_cnt       TYPE  i,
        p_cnt(05)        TYPE  c,
        w_check(05)      TYPE  c,
        w_bdc_tot         TYPE  i,
        w_total_amt      LIKE  ztmm_duty_it-duty_amt,      "amt
        w_amt            LIKE  ztmm_duty_it-duty_amt,      "amt
        w_waers          LIKE  t001-waers,
        w_ventxt(50)     TYPE  c,
        w_tabix          LIKE  sy-tabix,
        w_docdt(10)      TYPE  c,
        w_posdt(10)      TYPE  c,
        w_loop_cnt       TYPE  n,
        w_repid          LIKE  sy-repid.

DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.
DATA : w_mode LIKE ctu_params-dismode VALUE 'E'. "'E'. "A-display 'N' *

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no,
      handle_hotspot_click
                     FOR EVENT hotspot_click OF cl_gui_alv_grid
                     IMPORTING e_row_id
                               e_column_id.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

* Setting for hotspot click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click
                 USING e_row_id e_column_id.
  ENDMETHOD.                    " handle_hotspot_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* Others
*----------------------------------------------------------------------*
DATA:
      gv_index      TYPE i,
      g_error(1).

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
    exporting
      titel = &1
      txt1  = &2
      txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-a10.
*a1 Co. code,   Doc date
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-a11 FOR FIELD p_bukrs.
PARAMETERS :  p_bukrs    TYPE   bukrs OBLIGATORY MEMORY ID buk.
SELECTION-SCREEN COMMENT 35(12) text-a12 FOR FIELD p_bukrs.
PARAMETERS :  p_docdt    TYPE   sy-datum MODIF ID lod DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF LINE.
*a2 Vendor,   Reference
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)  text-a21 FOR FIELD p_ven1.
PARAMETERS :  p_ven1            LIKE ekko-lifnr OBLIGATORY.
SELECTION-SCREEN COMMENT 35(12) text-a22 FOR FIELD p_refer.
PARAMETERS :  p_refer(15)       TYPE   c OBLIGATORY.
SELECTION-SCREEN END OF LINE.
*b3 Vendor Text	
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b31 FOR FIELD p_ventxt.
PARAMETERS :  p_ventxt(40)       TYPE   c OBLIGATORY.
SELECTION-SCREEN END OF LINE.
*a3 Exp. Acct, Posting date
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-a31 FOR FIELD p_expact.
PARAMETERS :  p_expact           TYPE   skat-saknr OBLIGATORY.
SELECTION-SCREEN COMMENT 35(12)  text-a32 FOR FIELD p_refer.
PARAMETERS :  p_posdt            TYPE sy-datum MODIF ID lod DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF LINE.

*b1 Doc header text
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b11 FOR FIELD p_hdtxt.
PARAMETERS :  p_hdtxt(40)        TYPE c OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b10.
SELECTION-SCREEN SKIP 1.
*b41 Accrual Acct, Assignment
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b41 FOR FIELD p_acct11.
PARAMETERS :  p_acct11    TYPE   skat-saknr         OBLIGATORY.
SELECTION-SCREEN COMMENT 35(12)  text-b42 FOR FIELD p_entn11.
PARAMETERS :  p_entn11    TYPE   ztmm_duty_hd-entno OBLIGATORY.
SELECTION-SCREEN COMMENT 79(05)  text-b43 FOR FIELD p_text11.
PARAMETERS :  p_text11    TYPE   ztmm_duty_hd-fentp OBLIGATORY DEFAULT '06'.
SELECTION-SCREEN END OF LINE.
*b42 Duty MPF , MPF Amt, Text
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b44 FOR FIELD p_acct12.
PARAMETERS :  p_acct12    TYPE   skat-saknr OBLIGATORY.
SELECTION-SCREEN COMMENT 35(12)  text-b45 FOR FIELD p_amt012.
PARAMETERS :  p_amt012    TYPE   ztmm_duty_hd-duty_amt OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
*b51 Accrual Acct, Assignment
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-c41 FOR FIELD p_acct21.
PARAMETERS :  p_acct21    TYPE   skat-saknr.
SELECTION-SCREEN COMMENT 35(12)  text-b42 FOR FIELD p_entn21.
PARAMETERS :  p_entn21    TYPE   ztmm_duty_hd-entno.
SELECTION-SCREEN COMMENT 79(05)  text-b43 FOR FIELD p_text21.
PARAMETERS :  p_text21    TYPE   ztmm_duty_hd-fentp.
SELECTION-SCREEN END OF LINE.
*b52 Duty MPF , MPF Amt, Text
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b44 FOR FIELD p_acct22.
PARAMETERS :  p_acct22    TYPE   skat-saknr.
SELECTION-SCREEN COMMENT 35(12)  text-b45 FOR FIELD p_amt022.
PARAMETERS :  p_amt022    TYPE   ztmm_duty_hd-duty_amt.
*SELECTION-SCREEN COMMENT 79(05)  text-b46 FOR FIELD p_text22.
*PARAMETERS :  p_text22    TYPE   ztmm_duty_hd-fentp.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
*b61 Accrual Acct, Assignment
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-d41 FOR FIELD p_acct31.
PARAMETERS :  p_acct31    TYPE   skat-saknr.
SELECTION-SCREEN COMMENT 35(12)  text-b42 FOR FIELD p_entn31.
PARAMETERS :  p_entn31    TYPE   ztmm_duty_hd-entno.
SELECTION-SCREEN COMMENT 79(05)  text-b43 FOR FIELD p_text31.
PARAMETERS :  p_text31    TYPE   ztmm_duty_hd-fentp.
SELECTION-SCREEN END OF LINE.
*b62 Duty MPF , MPF Amt, Text
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15)   text-b44 FOR FIELD p_acct32.
PARAMETERS :  p_acct32    TYPE   skat-saknr.
SELECTION-SCREEN COMMENT 35(12)  text-b45 FOR FIELD p_amt032.
PARAMETERS :  p_amt032    TYPE   ztmm_duty_hd-duty_amt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
*
SELECTION-SCREEN SKIP 1.
* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-033.
PARAMETER p_vari TYPE slis_vari.
PARAMETERS :  p_bdcmod    TYPE ctu_mode DEFAULT 'N'..
SELECTION-SCREEN END OF BLOCK b4.
