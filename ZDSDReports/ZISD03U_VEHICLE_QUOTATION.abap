************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZISD03U_VEHICLE_QUOTATION                                  *
*& Type   : Report/Interface                                           *
*& Author : Manju                                                      *
*& Title  : Program to create Quotation for Vehicles                   *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 58GE3A1432                                   *
* System Id:                                                           *
*                                                                      *
*   Requested by:    Lance  Younce                                     *
*   Assigned to:     Manjunath Venkatesh                               *
*   Original Request #:                                                *
*   ABAP Analyst:    Manjunath Venkatesh                               *
*   Business Users:                                                    *

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
* 07/19/06    Manju        UD1K921401   Initial Coding
************************************************************************
REPORT ZISD03U_VEHICLE_QUOTATION LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : ZTPP_WOSUM,
         ZTPP_QUOTATION.

TYPE-POOLS: slis.
INCLUDE: <icon>.
*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
data : W_CNT type i,
       sales_quotation like BAPIVBELN-VBELN,
       mode like BAPI_STAND-NO_COMMIT.

* Internal Tables
DATA : BEGIN OF IT_WOSUM OCCURS 0.
        INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM.

* Internal table
data : begin of wa_wosum occurs 0,
        key(4),
        WO_SER like ZTPP_WOSUM-WO_SER,
        NATION like ZTPP_WOSUM-nation,
        DEALER like ZTPP_WOSUM-dealer,
        EXTC   like ZTPP_WOSUM-EXTC,
        INTC   like ZTPP_WOSUM-INTC,
        INITQTY like ZTPP_WOSUM-INITQTY,
        WOCREDATE like ZTPP_WOSUM-WOCREDATE,
        FSC       like ZTPP_WOSUM-FSC,
   end of wa_wosum.

data : begin of it_Quot occurs 0.
        INCLUDE STRUCTURE ZTPP_QUOTATION.
data:  end of it_quot.

* Internal Table for Ouotation header
data : begin of order_header_in occurs 0.
        include STRUCTURE BAPISDHEAD.
data:   end of order_header_in.

* Internal Table for Ouotation Items
data : begin of ORDER_ITEMS_IN occurs 0.
        include STRUCTURE BAPIITEMIN.
data:   end of ORDER_ITEMS_IN.

* Internal Table for Ouotation Partners
data : begin of ORDER_PARTNERS occurs 0.
        include STRUCTURE BAPIPARTNR.
data:   end of ORDER_PARTNERS.

* Internal Table for Returns
data : begin of BAPIRETURN occurs 0.
        include STRUCTURE BAPIRETURN1.
data:   end of BAPIRETURN.

data : begin of ORDER_CFGS_INST occurs 0.
        include STRUCTURE BAPICUINS.
data : end of ORDER_CFGS_INST.

data : begin of ORDER_CFGS_PART_OF occurs 0.
        include STRUCTURE BAPICUPRT.
data : end of ORDER_CFGS_PART_OF.

data : begin of ORDER_CFGS_VALUE occurs 0.
        include STRUCTURE BAPICUVAL.
data : end of ORDER_CFGS_VALUE.

data : begin of ORDER_ITEMS_OUT occurs 0.
        include STRUCTURE BAPIITEMEX.
data : end of ORDER_ITEMS_OUT.

data : begin of ORDER_CFGS_REF occurs 0.
        include STRUCTURE BAPICUCFG.
data : end of ORDER_CFGS_REF.


* Table for Log
data : begin of it_succ occurs 0,
       WO_SER LIKE ZTPP_WOSUM-WO_SER,
       NATION LIKE ZTPP_WOSUM-NATION,
       DEALER LIKE ZTPP_WOSUM-DEALER,
       EXTC   LIKE ZTPP_WOSUM-EXTC,
       INTC   LIKE ZTPP_WOSUM-INTC,
       INITQTY LIKE ZTPP_WOSUM-INITQTY,
       FSC    LIKE ZTPP_WOSUM-FSC,
       SALES  LIKE ZTPP_WOSUM-SALES,
       mess(120) type c,
       ICON  like ZSCO_COGS_NEW-ICON,
       WOCREDATE like ZTPP_WOSUM-WOCREDATE,
       flag(1),
       key(4),
      end of it_succ.

data : l_nation like ZTPP_WOSUM-nation,
       l_dealer like ZTPP_WOSUM-dealer,
       l_key(4) type c.

* ALV Internal Table
data : g_repid     LIKE sy-repid,
      x_layout TYPE disvariant,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      l_date type sy-datum,
      l_variant    TYPE disvariant,  "Display Variant
      l_layout     TYPE slis_layout_alv.  "List layout specifications


*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATE FOR SY-DATUM NO-EXTENSION OBLIGATORY
                default sy-datum.
SELECT-OPTIONS: S_WO_SER FOR ZTPP_WOSUM-WO_SER,
                S_NATION FOR ZTPP_WOSUM-NATION default 'B35',
                S_DEALER FOR ZTPP_WOSUM-DEALER.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_post as checkbox   DEFAULT 'X'.
*             p_simu radiobutton group R1.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_AUART like VBAK-AUART DEFAULT 'ZQT',
             p_VKORG like vbak-VKORG DEFAULT 'D100',
             p_VTWEG like vbak-VTWEG default '10',
             p_SPART like vbak-SPART default  '10'.
SELECTION-SCREEN END OF BLOCK B2.


*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
start-of-selection.

* Select Data
  Perform select_data.

* Create Quotation
  perform create_quotation.

* Write Log
  perform display_log.


end-of-selection.
*---------------------------------------------------------------------*
* End-of-selection
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  data : l_index type sy-tabix.

* Select Work order data
  SELECT *
         INTO TABLE IT_WOSUM
         FROM ZTPP_WOSUM
        WHERE WOCREDATE IN S_DATE
          AND WO_SER IN S_WO_SER
          AND NATION IN S_NATION
          AND DEALER IN S_DEALER .



* Select Quotation Data
  if not it_wosum[] is initial.
    select * into table it_quot from ZTPP_quotation
             for all entries in it_wosum
             where WOCREDATE eq  it_wosum-wocredate
                AND WO_SER   eq  it_wosum-wo_ser
                AND NATION   eq  it_wosum-NATION
                AND DEALER   eq  it_wosum-DEALER
                and EXTC     eq  it_wosum-extc
                and intc     eq  it_wosum-intc.
  endif.

* Check Whether Already Quotation is created for above selected WO Order
  loop at it_wosum.
    l_index = sy-tabix.
    read table it_quot with key wocredate = it_wosum-wocredate
                                WO_SER    = it_wosum-wo_ser
                                nation    = it_wosum-nation
                                dealer    = it_wosum-dealer
                                extc      = it_wosum-extc
                                intc      = it_wosum-intc .
    if sy-subrc eq 0.
      delete it_wosum index l_index.
    endif.
  endloop.

  DESCRIBE TABLE IT_WOSUM LINES W_CNT.
  IF W_CNT = 0.
    SKIP 5.
    WRITE:/ 'No Data found in table for given Input'.
    STOP.
  ENDIF.

* Move to Different Table
  loop at it_wosum.
    move-corresponding it_wosum to wa_wosum.
    wa_wosum-key = it_wosum-WO_SER+1(4).
    Append wa_wosum.
    clear wa_wosum.
  endloop.

ENDFORM.                    " select_data
*
*&---------------------------------------------------------------------*
*&      Form  create_quotation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_quotation.


  check P_post eq 'X'.
  loop at wa_wosum.

* Fill BAPI Structure
    l_nation = wa_wosum-NATION.
    l_dealer = wa_wosum-dealer.
    l_key   =  wa_wosum-key.
    at new key.
      perform fill_bapiHeader.
    endat.

    perform fill_bapistructure.
    move-corresponding wa_wosum to it_succ.
    append it_succ.
    clear it_succ.

    At end of key.
* Call BAPI to create Sales Quotation
      perform call_BAPI.
    endat.

  endloop.
ENDFORM.                    " create_quotation
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.

  check p_post eq 'X' .
* Insert Records into Quotation table
  perform update_quotation_table.

  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
    'ICON'    'Status'       '6'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'WO_SER'  'Work Order'   '9'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'NATION'  'Nation'       '9'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'DEALER'  'Dealer'       '9'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'EXTC'    'EXTC'         '6'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'INTC'    'INTC'         '6'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'INITQTY' 'Qty'          '5'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'FSC'      'FSC#'        '18'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'SALES'   'Quotation'   '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
    'MESS'    'Message'     '80'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.

  g_repid = sy-repid.
  l_variant-REPORT  = sy-repid.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            is_layout          = l_layout
            it_fieldcat        = gt_fieldcat
            is_variant         = l_variant
            i_save             = 'A'
       TABLES
            t_outtab           = it_succ
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.


ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  update_quotation_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_quotation_table.
* Capture All Work Orders for Which Quotations are created.
  .
  loop at it_succ where flag  eq 'X'.
    move-corresponding it_succ to ZTPP_QUOTATION.
    ZTPP_QUOTATION-QVBELN = it_succ-SALES.
    ZTPP_QUOTATION-ERDAT  = sy-datum.
    ZTPP_QUOTATION-ERZET  = sy-uzeit.
    ZTPP_QUOTATION-ERNAM  = sy-uname.
    modify ZTPP_QUOTATION .
  endloop.
  if sy-subrc eq 0.
    commit work.
  endif.
ENDFORM.                    " update_quotation_table
*&---------------------------------------------------------------------*
*&      Form  fill_bapistructure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bapistructure.


* Fill Ouotation items
  ORDER_ITEMS_IN-MATERIAL     =  wa_wosum-FSC.
  ORDER_ITEMS_IN-REQ_QTY      =  wa_wosum-INITQTY * 1000.
  concatenate wa_wosum-WO_SER wa_wosum-EXTC wa_wosum-INTC into
                              ORDER_ITEMS_IN-CUST_MAT.
  append ORDER_ITEMS_IN.


ENDFORM.                    " fill_bapistructure
*&---------------------------------------------------------------------*
*&      Form  call_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_BAPI.

* Call BAPI to create Quotation at end of WO_SER+0(4) ( YEAR + Month )

  CALL FUNCTION 'BAPI_QUOTATION_CREATEFROMDATA'
       EXPORTING
            ORDER_HEADER_IN     = order_header_in
            WITHOUT_COMMIT      = mode
            CONVERT_PARVW_AUART = 'X'
       IMPORTING
            SALESDOCUMENT       = SALES_QUOTation
            RETURN              = BAPIRETURN
       TABLES
            ORDER_ITEMS_IN      = ORDER_ITEMS_IN
            ORDER_PARTNERS      = ORDER_PARTNERS
            ORDER_ITEMS_OUT     = ORDER_ITEMS_OUT
            ORDER_CFGS_REF      = ORDER_CFGS_REF
            ORDER_CFGS_INST     = ORDER_CFGS_INST
            ORDER_CFGS_PART_OF  = ORDER_CFGS_PART_OF
            ORDER_CFGS_VALUE    = ORDER_CFGS_VALUE.

  if  not SALES_QUOTation is initial.
* If Successful.
    it_succ-SALES =  SALES_QUOTation.
    it_succ-icon  = icon_green_light.
    it_succ-mess  = 'Quotation Created Succesfully'.
    it_succ-flag  = 'X'.
    modify it_succ transporting sales icon mess flag
     where key = wa_wosum-key.
  else.
* In case of Error
    it_succ-icon  = icon_red_light.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = BAPIRETURN-ID
              MSGNR               = BAPIRETURN-NUMBER
              MSGV1               = BAPIRETURN-MESSAGE_V1
              MSGV2               = BAPIRETURN-MESSAGE_V2
              MSGV3               = BAPIRETURN-MESSAGE_V3
              MSGV4               = BAPIRETURN-MESSAGE_V4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = it_succ-mess.
    modify it_succ transporting icon mess where key = wa_wosum-key.
  endif.
* Clear variables
  refresh : ORDER_ITEMS_IN,ORDER_PARTNERS,order_header_in.
  clear   : ORDER_ITEMS_IN,ORDER_PARTNERS,SALES_QUOTation,BAPIRETURN,
            ORDER_HEADER_IN,SALES_QUOTation,it_succ.
ENDFORM.                    " call_BAPI
*&---------------------------------------------------------------------*
*&      Form  fill_bapiHeader
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bapiHeader.

* Fill Quotation Header
  order_header_in-DOC_TYPE     =  P_AUART.          "Document Type
  order_header_in-SALES_ORG    =  p_vkorg.          " Sales Org
  order_header_in-DISTR_CHAN   =  p_VTWEG.          " Distr Chan
  order_header_in-DIVISION     =  p_SPART.          " Division
  order_header_in-PURCH_NO     =  l_key.            " PO No
  order_header_in-QT_VALID_T   = sy-datum + 30.     "Valid to date

* Fill Partners
  ORDER_PARTNERS-PARTN_ROLE = 'SH'.
  concatenate l_nation l_dealer
               into  ORDER_PARTNERS-PARTN_NUMB.
  append ORDER_PARTNERS.
  clear: l_nation,l_dealer,l_key.
ENDFORM.                    " fill_bapiHeader
