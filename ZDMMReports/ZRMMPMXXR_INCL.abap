***INCLUDE ZRMMPMXXR_INCL .

type-pools : slis,
             cxtab.

**--- Tables, Views & Structures
tables : mara,     " General Material Data
         makt,     " Material Descriptions
         marc,     " Plant Data for Material
         mard,     " Storage Location Data for Material
         ekko,     " Purchasing Document Header
         ekpo,     " Purchasing Document Item
         eket,     " Scheduling Agreement Schedule Lines
         mkpf,     " Header: Material Document
         mseg,     " Document Segment: Material
         lfa1,     " Vendor Master (General Section)
         lfm1,     " Vendor master record purchasing organization data
         t001w,    " Plants/Branches
         t024d,    " MRP controllers
         rsdxx,    " DD system table: Dynpro fields for DD transactions
         eord,     " Purchasing Source List
         t158,     " Transaction Control: Inventory Management
         rm07m,    " I/O Fields: Module Pool SAPMM07M
         t001l,    " Storage Locations
         msegk,    " Account Assignment Fields for Material Segment
         dm07m,    " Dialog Control Fields for Module Pool SAPMM07M
         t156,     " Movement Type
         t156n,    " Next Movement Type
         t156t,    " Movement Type Text
         marm,     " Units of Measure for Material
         t320,   " Assignment IM Storage Location to WM Warehouse Number
         t156b,    " Movement Types: Screen Selection
         t158b,    " Check Table: Movement Type for Transaction Code
         t333,     " WM Movement Types
         ltbk,     " Transfer requirement header
         ltbp,     " Transfer requirement item
         t301,     " WM Storage Types
         lagp,     " Storage bins
         cobl,     " Coding Block
         t001,     " Company Codes
         ska1,     " G/L Account Master (Chart of Accounts)
         csks,     " Cost Center Master Data
         tka02,    " Controlling area assignment
         s031,     " Statistics: Movements for current stocks
         likp,     " SD Document: Delivery Header Data
         lips,     " SD document: Delivery: Item data
         ztmm_mast,     " Supply to Line Master Table
         ztmm_dock,     " DMS Dock Management System Table
         tvsa,     " SD Documents: Processing Groups
         ztmm_kd_asn_main, " ASN I/F Temp Table (KDWeb -> SAP MM) : Real
         t163g,     " Confirmation Control
         mlgt,      " Material Data for Each Storage Type
         am07m,     " Work Fields for Lists (Selection and Output)
         t157d,     " Reason for Movement
         ztmm_nstl,     " Non Supply to Line
         plaf,      " Planned order
         resb,      " Reservation/dependent requirements
         ztca_if_log,     " Interface Log Table
*         ytpp_dvrt1,     " DVRT - Vehicle Master
         ztpp_dvrt1,     " DVRT - Vehicle Master
         eikp,      " Foreign Trade: Export/Import Header Data
         ztmm_kd_po,     " PO I/F Temp Table (SAP MM -> KDWeb)
         lubu,      " Posting change document
         stpo,      " BOM item
         ztbl,      " [IMPORT] Bill of Lading Header
         ztblit,    " [Import] Bill of Lading Item
         mdbs,      " Material View of Order Item/Schedule Line
         vbfa,      " Sales Document Flow
         ltap,      " Transfer order item
         ltak,      " WM transfer order header
         ztmm_dvrt, " [MM] DVRT - Vehicle Master
         pkhd,      " Control Cycle
         aufk,      " Order master data
         t163d,     " Assignment Internal/External Confirmation Category
         ekes,      " Vendor Confirmations
         lqua,      " Quants
         zvmm_delv, " LIKP + LIPS
         ztmm_cond, " Amortization Quantity Maintenance Table
         t024e,     " Purchasing Organizations
         v_eina,    " Aggregated object for reading EINA + EINE
         ztmm_stl_log,     " Supply to Line Log Table
         cabn,      " Characteristic
         a018,      " Material Info Record
         konp,      " Conditions (Item)
         affw,      " Goods movements with errors from confirmations
         ppc1_all,  " View of all Backflushing Tables
         ztmm_bom,  " [MM] BOM - FSC vs End Item
         ztmm_ppc1_all,     " View of all Backflushing Tables
         sscrfields,     " Fields on selection screens
         crhd,     " Work Center Header
         crca,     " Work Center Capacity Allocation
         kako,     " Capacity Header Segment
         kazy,     " Interval of Available Capacity
         kapa,     " Shift Parameters for Available Capacity
         tc37a,    " Shift definition
         ztmm_stl_time,
         ztmm_stl_exec,
         ztmm_nstl_log,
         ztmm_nstl_time,
         rkpf,
         a016,
         konh,
         ekbe,
         bkpf,
         bseg.


*--- ALV
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line type slis_listheader,
       w_layout   type slis_layout_alv.

data : w_fieldcat02 type slis_t_fieldcat_alv with header line,
       w_eventcat02 type slis_t_event with header line,
       w_selfield02 type slis_selfield,
       w_sortcat02  type slis_t_sortinfo_alv with header line,
       w_col_pos02  type i,
       w_program02  like sy-repid,
       w_top_of_page02 type slis_t_listheader,
       w_line02 type slis_listheader,
       w_layout02   type slis_layout_alv.

data : w_fieldcat03 type slis_t_fieldcat_alv with header line,
       w_eventcat03 type slis_t_event with header line,
       w_selfield03 type slis_selfield,
       w_sortcat03  type slis_t_sortinfo_alv with header line,
       w_col_pos03  type i,
       w_program03  like sy-repid,
       w_top_of_page03 type slis_t_listheader,
       w_line03 type slis_listheader,
       w_layout03   type slis_layout_alv.

**--- Constants
constants : c_formname_top_of_page   type slis_formname
                                        value 'TOP_OF_PAGE',
            c_formname_top_of_page02 type slis_formname
                                        value 'TOP_OF_PAGE02',
            c_formname_top_of_page03 type slis_formname
                                        value 'TOP_OF_PAGE03',
            c_yell(4)  value 'C310',
            c_green(4) value 'C510',
            c_red(4)   value 'C610'.

**--- Variables
data: w_time01l type t value '063501', w_time01h type t value '073000',
      w_time02l type t value '073001', w_time02h type t value '083000',
      w_time03l type t value '083001', w_time03h type t value '093000',
      w_time04l type t value '093001', w_time04h type t value '103000',
      w_time05l type t value '111501', w_time05h type t value '121500',
      w_time06l type t value '121501', w_time06h type t value '131500',
      w_time07l type t value '131501', w_time07h type t value '141500',
      w_time08l type t value '141501', w_time08h type t value '151500',
      w_time09l type t value '151501', w_time09h type t value '161500',
      w_time10l type t value '161501', w_time10h type t value '171500',
      w_time11l type t value '171501', w_time11h type t value '181500',
      w_time12l type t value '181501', w_time12h type t value '191500',
      w_time13l type t value '191501', w_time13h type t value '201500',
      w_time14l type t value '201501', w_time14h type t value '211500',
      w_time15l type t value '220001', w_time15h type t value '230000',
      w_time16l type t value '230001', w_time16h type t value '240000',
      w_time17l type t value '000001', w_time17h type t value '010000',
      w_time18l type t value '010001', w_time18h type t value '020000',
      w_time19l type t value '020001', w_time19h type t value '030000',
      w_time20l type t value '030001', w_time20h type t value '040000'.


**--- Macro
define assign_proper_field.
  if &1 between w_time01l and w_time01h.
    move : it_order-bdmng to it_itab-time01.
  elseif &1 between w_time02l and w_time02h.
    move : it_order-bdmng to it_itab-time02.
  elseif &1 between w_time03l and w_time03h.
    move : it_order-bdmng to it_itab-time03.
  elseif &1 between w_time04l and w_time04h.
    move : it_order-bdmng to it_itab-time04.
  elseif &1 between w_time05l and w_time05h.
    move : it_order-bdmng to it_itab-time05.
  elseif &1 between w_time06l and w_time06h.
    move : it_order-bdmng to it_itab-time06.
  elseif &1 between w_time07l and w_time07h.
    move : it_order-bdmng to it_itab-time07.
  elseif &1 between w_time08l and w_time08h.
    move : it_order-bdmng to it_itab-time08.
  elseif &1 between w_time09l and w_time09h.
    move : it_order-bdmng to it_itab-time09.
  elseif &1 between w_time10l and w_time10h.
    move : it_order-bdmng to it_itab-time10.
  elseif &1 between w_time11l and w_time11h.
    move : it_order-bdmng to it_itab-time11.
  elseif &1 between w_time12l and w_time12h.
    move : it_order-bdmng to it_itab-time12.
  elseif &1 between w_time13l and w_time13h.
    move : it_order-bdmng to it_itab-time13.
  elseif &1 between w_time14l and w_time14h.
    move : it_order-bdmng to it_itab-time14.
  elseif &1 between w_time15l and w_time15h.
    move : it_order-bdmng to it_itab-time15.
  elseif &1 between w_time16l and w_time16h.
    move : it_order-bdmng to it_itab-time16.
  elseif &1 between w_time17l and w_time17h.
    move : it_order-bdmng to it_itab-time17.
  elseif &1 between w_time18l and w_time18h.
    move : it_order-bdmng to it_itab-time18.
  elseif &1 between w_time19l and w_time19h.
    move : it_order-bdmng to it_itab-time19.
  elseif &1 between w_time20l and w_time20h.
    move : it_order-bdmng to it_itab-time20.
  endif.
end-of-definition.




*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_EVENTCAT[]  text
*----------------------------------------------------------------------*
form event_build using    p_w_eventcat type slis_t_event.
**---
  data : l_event type slis_alv_event.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = p_w_eventcat.

  read table p_w_eventcat with key name = slis_ev_top_of_page
                          into l_event.

  if sy-subrc eq 0.
    move c_formname_top_of_page to l_event-form.
    append l_event to p_w_eventcat.
  endif.
endform.                    " event_build

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_of_page.
**---
  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
            it_list_commentary = w_top_of_page.
endform.                    " top_of_page

*&---------------------------------------------------------------------*
*&      Form  get_material_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_MATNR  text
*----------------------------------------------------------------------*
form get_material_desc using    p_it_temp_matnr.
**---
  clear : makt.

  select single maktx into makt-maktx
                      from makt
                     where matnr eq p_it_temp_matnr
                       and spras eq sy-langu.
endform.                    " get_material_desc

*&---------------------------------------------------------------------*
*&      Form  get_vendor_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_LIFNR  text
*----------------------------------------------------------------------*
form get_vendor_desc using    p_it_temp_lifnr.
**---
  clear : lfa1.

  select single name1 into lfa1-name1
                      from lfa1
                     where lifnr eq p_it_temp_lifnr.
endform.                    " get_vendor_desc

*&---------------------------------------------------------------------*
*&      Form  get_plant_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
form get_plant_desc using    p_p_werks.
**---
  clear : t001w.

  select single name1 into t001w-name1
                      from t001w
                     where werks eq p_p_werks.
endform.                    " get_plant_desc

*&---------------------------------------------------------------------*
*&      Form  table_control_page_scrol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAVE_OKCODE  text
*      -->P_TC_9000_TOP_LINE  text
*      -->P_W_TOT_LINES  text
*      -->P_W_LOOPC  text
*----------------------------------------------------------------------*
form table_control_page_scrol using    p_okcode
                                       p_top_line
                                       p_tot_lines
                                       p_loopc.
**---
  call function 'SCROLLING_IN_TABLE'
       exporting
            entry_act             = p_top_line     " Top Line
            entry_from            = 1
            entry_to              = p_tot_lines        " Lines
*           LAST_PAGE_FULL        = 'X'
            loops                 = p_loopc           " SY-LOOPC
            ok_code               = p_okcode     "* OK_Code
*           OVERLAPPING           = ' '
*           PAGE_ACT              = 0
*           PAGE_GO               = 0
       importing
*           ENTRIES_SUM           =
            entry_new             = p_top_line
*           PAGES_SUM             =
*           PAGE_NEW              =
       exceptions
            no_entry_or_page_act  = 1
            no_entry_to           = 2
            no_ok_code_or_page_go = 3
            others                = 4.
endform.                    " table_control_page_scrol

*&---------------------------------------------------------------------*
*&      Form  get_manager_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*      -->P_ZTMM_MAST_DISPO  text
*----------------------------------------------------------------------*
form get_manager_desc using    p_werks
                               p_dispo.
**---
  clear : t024d.

  select single dsnam into t024d-dsnam
                      from t024d
                     where werks eq p_werks
                       and dispo eq p_dispo.
endform.                    " get_manager_desc

*&---------------------------------------------------------------------*
*&      Form  conversion_exit_alpha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COBL_KOSTL  text
*----------------------------------------------------------------------*
form conversion_exit_alpha using    p_alpha.
*---
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
       exporting
            input  = p_alpha
       importing
            output = p_alpha.
endform.                    " conversion_exit_alpha

*&---------------------------------------------------------------------*
*&      Form  get_pur_org_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTMM_COND_EKORG  text
*----------------------------------------------------------------------*
form get_pur_org_desc using    p_ztmm_cond_ekorg.
*---
  clear : t024e.

  select single ekotx into t024e-ekotx
                      from t024e
                     where ekorg eq p_ztmm_cond_ekorg.
endform.                    " get_pur_org_desc
