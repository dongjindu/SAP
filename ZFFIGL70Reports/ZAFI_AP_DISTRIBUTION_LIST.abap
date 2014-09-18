************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAFI_AP_DISTRIBUTION_LIST                                  *
*& Type   : Report                                                     *
*& Author : Manju                                                      *
*& Title  : AP Distribution List Report                                *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 667E23918                                    *
*   Requested by:        Andy                                          *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:  Manjunath Venkatesh                                 *
*                                                                      *
* Business Users:  AP Tax team                                         *
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
* 06/09/06    Manju        UD1K921035   Initial Coding
* 02/19/06    IG.MOON                   Enhance main logic

************************************************************************
 report zafi_ap_distribution_list line-size 132 line-count 65
                       no standard page heading message-id db .
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
 tables : bseg,
          bsak,
          aufk, sscrfields.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
 data: wa_custom_control type scrfname value 'ALV_CONTAINER',
       alv_grid          type ref to cl_gui_alv_grid,
       grid_container    type ref to cl_gui_custom_container,
       w_repid type sy-repid.

* Global variables for attributes or etc of ALV GRID
 data : wa_is_layout type lvc_s_layo,
        it_fieldcat     type lvc_t_fcat with header line,
        it_fieldcat_fi  type lvc_t_fcat with header line,
        it_fieldcat_co  type lvc_t_fcat with header line,
        it_fieldname    type slis_t_fieldcat_alv,
        it_sort         type lvc_t_sort with header line,
        it_fieldcat_det type lvc_t_fcat with header line. "/Detail

*       w_fieldname  LIKE LINE OF it_fieldcat.

 data : wa_variant type disvariant.
 data: wa_save    type c   value 'A'.
*----------------------------------------------------------------------*
* Internal Table
*----------------------------------------------------------------------

 data : begin of it_saknr occurs 0,
            saknr type saknr ,
        end of it_saknr .

 ranges r_saknr for skb1-saknr occurs 0.


* Internal table for BSAK
 data : begin of it_bsak occurs 0,
         bukrs like bsak-bukrs,
         gjahr like bsak-gjahr,
         belnr like bsak-belnr,
         buzei like bsak-buzei,
         bldat like bsak-bldat,
         blart like bsak-blart,
         augbl like bsak-augbl,
         umskz like bsak-umskz,
         shkzg like bsak-shkzg,
         budat like bsak-budat,
         lifnr like bsak-lifnr,
         augdt like bsak-augdt,
         sgtxt like bsak-sgtxt,
         wt_withcd like  with_item-wt_withcd,
         umsks like bsak-umsks,
         xcpdk like lfa1-xcpdk,
         bschl like bsak-bschl,
         qbshb like bsak-qbshb, "withholding
        end of it_bsak.

* Internal Table for BSEG
 data :begin of it_bseg occurs 0,
         bukrs like bseg-bukrs,
         gjahr like bseg-gjahr,
         belnr like bseg-belnr,
         hkont like bseg-hkont,
         lifnr like bseg-lifnr,
         augdt like bseg-augdt,
         augbl like bseg-augbl,
         zuonr like bseg-zuonr,
         buzei like bseg-buzei,
         shkzg like bseg-shkzg,
         dmbtr like bseg-dmbtr,
*         sgtxt like bseg-sgtxt,
         kostl like bseg-kostl,
         prctr like bseg-prctr,
         aufnr like bseg-aufnr,
         fistl like bseg-fistl,
*         qsskz like bseg-qsskz,
         koart like bseg-koart,
         bschl like bseg-bschl,
         umsks like bseg-umsks,
         rebzt like bseg-rebzt,
         vorgn like bseg-vorgn,
         xzahl like bseg-xzahl,
        end of it_bseg.

* Internal table for BKPF
 data : begin of it_bkpf occurs 0,
         belnr like bkpf-belnr,
         gjahr like bkpf-gjahr,
         stblg like bkpf-stblg,
         stjah like bkpf-stjah,
         bldat like bkpf-bldat,
         end of it_bkpf.

 data : begin of it_bkpf2 occurs 0,
         belnr like bkpf-belnr,
         gjahr like bkpf-gjahr,
         stblg like bkpf-stblg,
         stjah like bkpf-stjah,
         bldat like bkpf-bldat,
         end of it_bkpf2.

* Internal table for Data Download
 data : begin of it_data occurs 0,
            hkont(10),
            budat(8),
            augdt(8),
            belnr(10),
            dmbtr(15),
            sgtxt(50),
            bukrs(4),
            kostl(10),
            lifnr(65),
            augbl(10),
            chect(13),
            rwbtr(13),
            pridt(8),
        end of it_data.

 data :  l_lifnr like bseg-lifnr,
         flag type c,
         l_augdt like bseg-augdt,
         l_augbl like bseg-augbl,
         l_bldat like bkpf-bldat.

* Internal table for ALV GRID
 data : out_tab type table of  zfi_ap_list with header line .
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------


*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
 selection-screen begin of block bl1 with frame title text-bl1 .
 parameters : p_bukrs like bsak-bukrs default 'H201',
              p_gjahr like bsec-gjahr obligatory memory id gjr.
 select-options : s_monat for bsak-monat." default sy-datum+4(2).
 select-options : s_lifnr for bseg-lifnr,
                  s_belnr for bseg-belnr,
                  s_budat for bseg-bzdat,
                  s_augdt for bseg-augdt,
*                  s_HKONT for bseg-HKONT .
                  s_blart for bsak-blart.
 selection-screen end of block bl1.

 selection-screen begin of block bl2 with frame title text-bl2 .

 parameters : p_chk as checkbox user-command ucom.

 selection-screen skip 1.

 selection-screen begin of block bl11 with frame title text-bl2.

 parameters par_r1 radiobutton group 1 modif id fil.

 parameters : p_file like rlgrap-filename  default
  'C:\Documents and Settings\doc.txt' modif id fil.

 selection-screen skip 1.

 parameters par_r2 radiobutton group 1 modif id fil.
 parameters:  par_file(50) modif id fil.

 parameters:  p_fchk as checkbox.

 selection-screen end of block bl11.
 selection-screen end of block bl2.

* Layout
 selection-screen begin of block b4 with frame title text-010.
 parameter p_vari type slis_vari.
 selection-screen end of block b4.

*----------------------------------------------------------------------*
* At selection screen
*-----------------------------------------------------------------------
 at selection-screen on value-request for p_vari.
   perform alv_variant_f4 changing p_vari.

 at selection-screen on value-request for p_file.

* Allow F4 on filename
   call function 'WS_FILENAME_GET'
     exporting
       mask             = ',*.*,*.*.'
       mode             = 'O'
       title            = text-t01
     importing
       filename         = p_file
     exceptions
       inv_winsys       = 1
       no_batch         = 2
       selection_cancel = 3
       selection_error  = 4
       others           = 5.

 initialization.
   if p_vari is initial.
     perform get_default_variant_f14 using p_vari.
   endif.

   if s_blart[] is initial.
     s_blart = 'INE'.
     s_blart-low = 'SA'.
     append s_blart.
   endif.

   perform make_file_name using par_file.

 at selection-screen .
   case sscrfields-ucomm.
     when 'UCOM'.
       perform modify_screen.
   endcase.

 at selection-screen output.
   perform modify_screen.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
 start-of-selection.

   if p_fchk eq 'X'.
     perform chk_unix_file.
     exit.
   endif.

* Select Data
   perform select_data.

* Populate data
   perform process_data.

   perform refine_data.

* Download data into dat file
   if   p_chk eq 'X'.

     if par_r1 eq 'X'.
       perform download_data.
     else.
       perform download_data_to_unix.
     endif.
   else.

* Display data in ALV Grid
     perform display_alv_grid.

   endif.

 end-of-selection.
*----------------------------------------------------------------------*
* END-of - Selection
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form select_data.
   data l_index type i.

* Select Data from BSAK
   select   a~bukrs
            a~gjahr
            a~belnr
            a~buzei
            a~bldat
            a~blart
            a~augbl
            a~umskz
            a~shkzg
            a~budat
            a~lifnr
            a~augdt
            a~sgtxt
            b~wt_withcd
            a~umsks
            c~xcpdk
            a~bschl
            a~qbshb
            into table it_bsak
            from bsak as a left outer join with_item as b
            on   b~bukrs eq a~bukrs
            and  b~belnr eq a~belnr
            and  b~gjahr eq a~gjahr
            and  b~buzei eq a~buzei
            inner join lfa1 as c
            on c~lifnr eq a~lifnr
            where a~bukrs eq p_bukrs  and
                  a~gjahr eq p_gjahr  and
                  a~lifnr in s_lifnr and
                  a~monat in s_monat  and
                  a~budat in s_budat  and
                  a~augdt in s_augdt  and
                  a~belnr in s_belnr and
                  a~xstov eq '' and
                  a~umskz ne 'F' and
                  a~blart in s_blart." AND
*                  a~bschl NOT IN ('05','15','25','35').
*
   select   a~bukrs
            a~gjahr
            a~belnr
            a~buzei
            a~bldat
            a~blart
            a~augbl
            a~umskz
            a~shkzg
            a~budat
            a~lifnr
            a~augdt
            a~sgtxt
            b~wt_withcd
            a~umsks
            c~xcpdk
            a~bschl
            a~qbshb
            appending table it_bsak
            from bsik as a left outer join with_item as b
            on   b~bukrs eq a~bukrs
            and  b~belnr eq a~belnr
            and  b~gjahr eq a~gjahr
            and  b~buzei eq a~buzei
            inner join lfa1 as c
            on c~lifnr eq a~lifnr
            where a~bukrs eq p_bukrs  and
                  a~gjahr eq p_gjahr  and
                  a~lifnr in s_lifnr  and
                  a~monat in s_monat  and
*                  a~blart not in ('AB','ZR') and
                  a~budat in s_budat  and
                  a~augdt in s_augdt  and
                  a~belnr in s_belnr  and
                  a~xstov eq '' and
                  a~umskz ne 'F' and
                  a~blart in s_blart. " AND
*                  a~bschl NOT IN ('05','15','25','35').

   loop at it_bsak where bschl = '05'
                      or bschl = '15'
                      or bschl = '25'
                      or bschl = '35'.
     if it_bsak-qbshb = 0.
       delete it_bsak index sy-tabix.
     endif.
   endloop.

* Consider KZ Documents with Special G/L Indicator
*   delete it_bsak where blart eq 'KZ' and umskz is initial.
*
** Payment Documents
**   delete it_bsak where BLART  eq 'ZP' and shkzg eq 'S'.
*   delete it_bsak where BLART  eq 'KA' and shkzg eq 'S'.

* GL Account is entered ...
*   if not s_hkont[] is initial.
*    delete it_bsak where BLART  eq 'ZP' .
*   endif.

   clear : it_saknr[],  it_saknr, r_saknr[], r_saknr.

   select saknr into table it_saknr
   from skb1
   where fdlev like 'B%'.

   loop at it_saknr.
     r_saknr = 'IEQ'.
     r_saknr-low = it_saknr-saknr.
     append r_saknr.
   endloop.

* Select data from BSEG
   if not it_bsak[] is initial.

     select   bukrs
              gjahr
              belnr
              hkont
              lifnr
              augdt
              augbl
              zuonr
              buzei
              shkzg
              dmbtr
              kostl
              prctr
              aufnr
              fistl
              koart
              bschl
              umsks
              rebzt
              vorgn xzahl
              from bseg
              into table it_bseg
              for all entries in it_bsak
              where bukrs eq p_bukrs
                and belnr eq it_bsak-belnr
                and gjahr eq p_gjahr
                and not hkont in r_saknr
                and bschl not in
                ('05','15','25','35','07','17','27','37').

*                   hkont in s_hkont .
   endif.

* Select reverse documents from BKPF
   if not it_bsak[] is initial.
     select belnr gjahr stblg stjah bldat from bkpf into table it_bkpf
             for all entries in it_bsak
          where bukrs = it_bsak-bukrs  and
                belnr = it_bsak-belnr and
                gjahr = it_bsak-gjahr and
                stblg ne space.

     clear : it_bkpf2, it_bkpf2[].

     read table it_bkpf index 1.
     if sy-subrc eq 0.
       select belnr gjahr stblg stjah bldat from bkpf
       into table it_bkpf2
               for all entries in it_bkpf
            where bukrs = p_bukrs and
                  belnr = it_bkpf-stblg and
                  gjahr = it_bkpf-stjah.

       append lines of it_bkpf2 to it_bkpf.
     endif.

   endif.
   sort it_bkpf by belnr gjahr.

* Ignore  Reveresal Documents
   loop at it_bseg.
     l_index = sy-tabix.
     read table it_bkpf with key belnr = it_bseg-belnr
                                 gjahr = it_bseg-gjahr
                                 binary search.
     if sy-subrc eq 0.
       delete  it_bseg index l_index.
     endif.
   endloop.

* For  down payments - Display Advance Payment and Total payemnt Amount
* And ignore all other records related to Advance payment.
   sort it_bsak by belnr augbl.

*   loop at it_bsak where blart eq 'ZP' .
**                        and shkzg eq 'S'.
*     if it_bsak-belnr ne it_bsak-augbl .
*       delete it_bseg where  belnr eq it_bsak-belnr and
*                             augbl is initial.
*     endif.
*     delete it_bseg where augbl eq it_bsak-augbl
*            and  belnr eq it_bsak-augbl.
*
*     delete it_bseg where  belnr eq it_bsak-augbl.
*     delete it_bseg where  belnr eq it_bsak-augbl and
*                           augbl is initial.
*
*   endloop.
*
*   delete it_bsak where BLART  eq 'ZP' and shkzg eq 'S'
*          and umskz eq '' .

   loop at  it_bsak where umsks eq 'A'. "blart eq 'ZP' .


*     DELETE it_bseg WHERE belnr EQ it_bsak-belnr
*                      AND umsks NE 'A'.

*     SELECT SINGLE * FROM bsak WHERE bukrs EQ p_bukrs
*                          AND lifnr EQ it_bsak-lifnr
*                          AND umsks EQ it_bsak-umsks
*                          AND umskz EQ 'F'
*                          AND augdt EQ it_bsak-budat
*                          AND augbl EQ it_bsak-belnr.
*     IF sy-subrc EQ 0.
*     ELSE.
*       DELETE it_bseg WHERE belnr EQ it_bsak-belnr
*                        AND buzei EQ it_bsak-buzei.
*     ENDIF.


   endloop.

   if it_bseg[] is initial.
     write text-001 .
     stop.
     exit.
   endif.
 endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  display_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form display_alv_grid.

   call screen 100.

 endform.                    " display_alv_grid
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 module status_0100 output.

   set pf-status 'M10'.
   set titlebar 'M20'.

 endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 module user_command_0100 input.

   case sy-ucomm.
     when 'BACK' or 'EXIT' or 'CANC'.
       clear: sy-ucomm.
       leave to screen 0.
   endcase.
 endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  Create_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 module create_alvobject output.
   if grid_container is initial.
     perform create_logical_container.
     perform set_attributes.
     perform create_grid.
   endif.

 endmodule.                 " Create_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_logical_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form create_logical_container.
   create object grid_container
     exporting
       container_name              = 'MY_CONT'
     exceptions
       cntl_error                  = 1
       cntl_system_error           = 2
       create_error                = 3
       lifetime_error              = 4
       lifetime_dynpro_dynpro_link = 5.

   if sy-subrc ne 0.
     w_repid = sy-repid.
     call function 'POPUP_TO_INFORM'
       exporting
         titel = w_repid
         txt2  = sy-subrc
         txt1  = 'The control can not be created'.
   endif.

   create object alv_grid
     exporting
       i_parent      = grid_container
       i_appl_events = 'X'.
 endform.                    " create_logical_container
*&---------------------------------------------------------------------*
*&      Form  set_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form set_attributes.
   data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

   clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
   wa_is_layout-edit       = ''.       "/Edit Mode Enable
   wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
   wa_is_layout-language   = sy-langu. "/Language Key
   wa_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
*  wa_is_layout-no_merging = 'X'.      "/Disable cell merging

*//-- Set Variant Structure
   wa_variant-report       = sy-repid.
   wa_variant-username     = sy-uname.

* Field Catalog
   call function 'LVC_FIELDCATALOG_MERGE'
     exporting
       i_structure_name = 'ZFI_AP_LIST'
     changing
       ct_fieldcat      = it_fieldcat[].


 endform.                    " set_attributes
*&---------------------------------------------------------------------*
*&      Form  create_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form create_grid.

*   Set variant
   wa_variant-report = sy-repid.
   wa_variant-variant = p_vari.


   wa_is_layout-detailtitl  = 'Update One Time Vendor Data'.
   call method alv_grid->set_table_for_first_display
     exporting
       is_layout       = wa_is_layout
       i_save          = wa_save
       is_variant      = wa_variant
       i_default       = 'X'
     changing
       it_fieldcatalog = it_fieldcat[]
       it_outtab       = out_tab[].

 endform.                    " create_grid
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form download_data.
   data: begin of it_colnames occurs 10,
         name(20),                       "Column names for download
        end of it_colnames.

* 09/06/2013 - T00306 Start
   data: begin of lt_user occurs 0,
           bname type xubname,
           name_first type ad_namefir,
           name_last  type ad_namelas,
         end of lt_user,

         begin of lt_lfa1 occurs 0,
           lifnr type lifnr,
           name1 type name1_gp,
         end of lt_lfa1.

   data: l_bname type xubname.

   if not out_tab[] is initial.
     loop at out_tab.
       call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
         exporting
           input  = out_tab-lifnr
         importing
           output = lt_user-bname.
       append lt_user.
     endloop.

     select lifnr name1
       into table lt_lfa1
       from lfa1
        for all entries in out_tab
      where lifnr = out_tab-lifnr.
     sort lt_lfa1 by lifnr.

     select t1~bname
            t2~name_first t2~name_last
       into table lt_user
       from usr21 as t1 inner join adrp as t2
                  on t1~persnumber = t2~persnumber
        for all entries in lt_user
      where bname = lt_user-bname.
     sort lt_user by bname.
   endif.
* 09/06/2013 - T00306 End

* Column Names :-
*  COLUMN HEADER
*   it_colnames-name = 'G/L Acct'. APPEND it_colnames.
*   it_colnames-name = 'Post DT'. APPEND it_colnames.
*   it_colnames-name = 'Clr DT'. APPEND it_colnames.
*   it_colnames-name = 'Document'.    APPEND it_colnames.
*   it_colnames-name = 'Amount'.  APPEND it_colnames.
*   it_colnames-name = 'SGTXT'.  APPEND it_colnames.
*   it_colnames-name = 'BUKRS'.  APPEND it_colnames.
*   it_colnames-name = 'Cst.CC'.   APPEND it_colnames.
*   it_colnames-name = 'Vendor'.   APPEND it_colnames.
*   it_colnames-name = 'Clr.Doc'.   APPEND it_colnames.
*   it_colnames-name = 'Chk.No'.   APPEND it_colnames.
*   it_colnames-name = 'Paid.Amt'.   APPEND it_colnames.
*   it_colnames-name = 'Print Date'.APPEND it_colnames.
*   it_colnames-name = 'Year'.   APPEND it_colnames.
*   it_colnames-name = 'D/C'.   APPEND it_colnames.

* Transfer data
   clear : it_data[], it_data.
   loop at out_tab.
     move-corresponding out_tab to it_data.
     if out_tab-lifnr = out_tab-belnr.
       concatenate out_tab-lifnr out_tab-gjahr into it_data-lifnr.
     endif.
* 09/06/2013 - T00306 Start
     read table lt_lfa1 with key lifnr = out_tab-lifnr binary search.
     if sy-subrc eq 0.
       concatenate lt_lfa1-lifnr lt_lfa1-name1 into it_data-lifnr
                                               separated by '-'.
     else.
       call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
         exporting
           input  = out_tab-lifnr
         importing
           output = l_bname.
       read table lt_user with key bname = l_bname binary search.
       if sy-subrc eq 0.
         concatenate out_tab-lifnr lt_user-name_first lt_user-name_last
                into it_data-lifnr
           separated by '-'.
       endif.
     endif.
* 09/06/2013 - T00306 End
     append it_data.
   endloop.

* DOWNLOAD Data
   call function 'WS_DOWNLOAD'
     exporting
       filename                = p_file
       filetype                = 'DAT'
*      col_select              = 'X'
     tables
       data_tab                = it_data
       fieldnames              = it_colnames
     exceptions
       file_open_error         = 1
       file_write_error        = 2
       invalid_filesize        = 3
       invalid_table_width     = 4
       invalid_type            = 5
       no_batch                = 6
       unknown_error           = 7
*      GUI_REFUSE_FILETRANSFER = 8
       others                  = 9.
   if sy-subrc <> 0.
     write :/ 'File Download Not Success'.
   else.
     write :/  'File Download Success '.
   endif.


 endform.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form process_data.
* Collect data in output internal table
   sort it_bseg by belnr koart.
   sort it_bsak by belnr. " Augbl.

   loop at it_bseg.

     if it_bseg-koart eq 'K' or it_bseg-koart eq 'D'.
       "downpay/dp clear or residual pay
       if it_bseg-vorgn = 'AZBU' or it_bseg-rebzt = 'V'.
       else.
         continue.
       endif.
     endif.

     clear it_bsak.
     move-corresponding it_bseg to out_tab.
     read table it_bsak with key belnr = it_bseg-belnr
*                                 Augbl = it_bseg-augbl
                                 binary search.
     if sy-subrc eq 0.

**Ignore  self.. except Down Payment
*       IF it_bseg-umsks NE 'A' AND
*          it_bseg-belnr EQ it_bsak-augbl .
**            and it_bseg-buzei eq it_bsak-buzei ).
*         CONTINUE.
*       ENDIF.


       move-corresponding it_bsak to out_tab.

       if it_bsak-xcpdk eq 'X'.
         out_tab-lifnr = it_bseg-belnr.
       endif.

       if out_tab-augdt(4) ne p_gjahr.
         clear :  out_tab-augdt,out_tab-augbl.
       endif.
       if it_bsak-umsks eq 'A'.
         out_tab-augdt = out_tab-budat.
         out_tab-augbl = out_tab-belnr.
       endif.
     endif.

*Ignore  some of the Down Payment records
*     if out_tab-belnr eq out_tab-augbl.
*       continue.
*     endif.

*     clear : l_lifnr, l_augdt,l_augbl.

***     if  it_bseg-koart eq 'K' .
****         it_bseg-BSCHL eq '31'.
***       l_lifnr = it_bseg-lifnr.
***       l_augdt = it_bseg-augdt.
***       l_augbl = it_bseg-augbl.
***       l_bldat = it_bsak-budat.
***       clear flag.
***     else.
***       out_tab-lifnr = l_lifnr.
***       out_tab-augdt = l_augdt.
***       out_tab-augbl = l_augbl.
***       out_tab-budat = l_bldat.
***     endif.

     if it_bseg-kostl is initial.
       select single * from aufk
               where aufnr eq it_bseg-aufnr.
       if sy-subrc eq 0.
*         out_tab-KOSTV = aufk-KOSTV.
*         out_tab-akstl = aufk-akstl.
         out_tab-kostl = aufk-akstl.
       endif.
     endif.

     if ( it_bseg-koart eq 'K' or it_bseg-koart eq 'D' )
        and it_bseg-umsks eq space.
       if it_bseg-shkzg eq 'H'.
         out_tab-shkzg = 'D'.
       else.
         out_tab-dmbtr = it_bseg-dmbtr * -1.
         out_tab-shkzg = 'C'.
       endif.
     else.
       if it_bseg-shkzg eq 'H'."fixme and it_bsak-UMSKZ eq ''.
         out_tab-dmbtr = it_bseg-dmbtr * -1.
         out_tab-shkzg = 'C'.
       else.
         out_tab-shkzg = 'D'.
       endif.
     endif.

**     if  it_bseg-koart eq 'S' or
**       ( it_bseg-koart eq 'K' and it_bseg-bschl ne '31' ).
**
**       if it_bseg-shkzg eq 'H'."fixme and it_bsak-UMSKZ eq ''.
**         out_tab-dmbtr = it_bseg-dmbtr * -1.
**         out_tab-SHKZG = 'C'.
**       else.
**         out_tab-SHKZG = 'D'.
**       endif.
**** For Downpayment Reverse Sign; fix
***       if   it_bseg-shkzg eq 'S'."fix and it_bsak-UMSKZ ne ''
***             "and it_bsak-BLART eq 'ZP'.
***         out_tab-dmbtr = it_bseg-dmbtr * -1.
***         out_tab-SHKZG = 'C'.
***       endif.

     if out_tab-wt_withcd eq 'XX'.
       clear out_tab-wt_withcd.
     endif.

     append out_tab.

     if it_bseg-koart eq 'K' or it_bseg-koart eq 'D'.
       if it_bseg-xzahl = 'X'.
         out_tab-augdt = out_tab-budat.
         out_tab-augbl = out_tab-belnr.
         if out_tab-shkzg = 'C'.
           out_tab-shkzg = 'D'.
         else.
           out_tab-shkzg = 'C'.
         endif.
         out_tab-dmbtr = -1 * out_tab-dmbtr.
         append out_tab.
       endif.
     endif.

     clear out_tab.

   endloop.

   if out_tab[] is initial.
     write text-001 .
     stop.
     exit.
   endif.

 endform.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  refine_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form refine_data.
   data $out_tab like out_tab occurs 0 with header line.

   loop at out_tab.
     $out_tab = out_tab.
     clear :
          $out_tab-belnr,
          $out_tab-bukrs,
          $out_tab-gjahr,
          $out_tab-hkont,
          $out_tab-lifnr,
          $out_tab-budat,
          $out_tab-augdt,
          $out_tab-shkzg,
          $out_tab-sgtxt,
          $out_tab-kostl,
          $out_tab-wt_withcd,
          $out_tab-bldat,
          $out_tab-bschl.
     collect $out_tab.
   endloop.

   loop at $out_tab.
     if $out_tab-dmbtr is initial.
       delete out_tab where augbl eq $out_tab-augbl.
     endif.
   endloop.
*
   data $ix type i.
   loop at out_tab.
     $ix = sy-tabix.
     select single chect rwbtr into (out_tab-chect,out_tab-rwbtr)
     from payr where vblnr eq out_tab-augbl
                 and gjahr eq out_tab-augdt(4).
     if sy-subrc eq 0.
       out_tab-pridt = out_tab-augdt.
       modify out_tab index $ix transporting chect rwbtr pridt.
     endif.

   endloop.

 endform.                    " refine_data
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
 form alv_variant_f4 changing p_vari.
   data: rs_variant like disvariant,
         lv_nof4 type c.

   clear lv_nof4.
   loop at screen.
     if screen-name = 'PA_VARI'.
       if screen-input = 0.
         lv_nof4 = 'X'.
       endif.
     endif.
   endloop.

   clear rs_variant.
   rs_variant-report   = sy-repid.
   rs_variant-username = sy-uname.

   call function 'REUSE_ALV_VARIANT_F4'
     exporting
       is_variant = rs_variant
       i_save     = 'A'
     importing
       es_variant = rs_variant
     exceptions
       others     = 1.

   if sy-subrc = 0 and lv_nof4 = space.
     p_vari = rs_variant-variant.
   endif.

 endform.                    " ALV_VARIANT_F4
*---------------------------------------------------------------------*
*       FORM get_default_variant_f14                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VARIANT                                                     *
*---------------------------------------------------------------------*
 form get_default_variant_f14 using p_variant.

   data: gx_variant         like disvariant.

   gx_variant-report   = sy-repid.
   gx_variant-username = sy-uname.

   if not p_variant is initial.
     gx_variant-variant = p_variant.
   endif.

   call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
     exporting
       i_save        = 'U'
     changing
       cs_variant    = gx_variant
     exceptions
       wrong_input   = 1
       not_found     = 2
       program_error = 3
       others        = 4.

   case sy-subrc.
     when 0.
       p_variant = gx_variant-variant.
     when 2.
       clear p_variant.
   endcase.

 endform.                    "get_default_variant_f14
*&---------------------------------------------------------------------*
*&      Form  download_data_to_unix
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form download_data_to_unix.

   data $par_file(80).
   constants: con_tab type x value '09'.
   data: str type string.


* 09/06/2013 - T00306 Start
   data: begin of lt_user occurs 0,
           bname type xubname,
           name_first type ad_namefir,
           name_last  type ad_namelas,
         end of lt_user,

         begin of lt_lfa1 occurs 0,
           lifnr type lifnr,
           name1 type name1_gp,
         end of lt_lfa1.

   data: l_bname type xubname.

   if not out_tab[] is initial.
     loop at out_tab.
       call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
         exporting
           input  = out_tab-lifnr
         importing
           output = lt_user-bname.
       append lt_user.
     endloop.

     select lifnr name1
       into table lt_lfa1
       from lfa1
        for all entries in out_tab
      where lifnr = out_tab-lifnr.
     sort lt_lfa1 by lifnr.

     select t1~bname
            t2~name_first t2~name_last
       into table lt_user
       from usr21 as t1 inner join adrp as t2
                  on t1~persnumber = t2~persnumber
        for all entries in lt_user
      where bname = lt_user-bname.
     sort lt_user by bname.
   endif.
* 09/06/2013 - T00306 End

* Transfer data
   clear : it_data[], it_data.
   loop at out_tab.
     move-corresponding out_tab to it_data.
     if out_tab-lifnr = out_tab-belnr.
       concatenate out_tab-lifnr out_tab-gjahr into it_data-lifnr.
     endif.
* 09/06/2013 - T00306 Start
     read table lt_lfa1 with key lifnr = out_tab-lifnr binary search.
     if sy-subrc eq 0.
       concatenate lt_lfa1-lifnr lt_lfa1-name1 into it_data-lifnr
                                               separated by '-'.
     else.
       call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
         exporting
           input  = out_tab-lifnr
         importing
           output = l_bname.
       read table lt_user with key bname = l_bname binary search.
       if sy-subrc eq 0.
         concatenate out_tab-lifnr lt_user-name_first lt_user-name_last
                into it_data-lifnr
           separated by '-'.
       endif.
     endif.
* 09/06/2013 - T00306 End
     append it_data.
   endloop.

   if par_file eq space.
     perform make_file_name using par_file.
   endif.

*   CONCATENATE '/usr/sap/EDI_SAP/BOM/' par_file
*INTO $par_file.

   concatenate '/usr/sap/UP2/FI_AP_DISTRIBUTION_LIST/'
   par_file into $par_file.

   open dataset $par_file for output in text mode.

   if sy-subrc ne 0.
     write:/ 'Error : ' color col_negative, $par_file.
     exit.
   endif.

   loop at it_data.
     concatenate
             it_data-hkont
             it_data-budat
             it_data-augdt
             it_data-belnr
             it_data-dmbtr
             it_data-sgtxt
             it_data-bukrs
             it_data-kostl
             it_data-lifnr
             it_data-augbl
             it_data-chect
             it_data-rwbtr
        it_data-pridt
                 into str
                   separated by '|'.
     transfer str to $par_file.
   endloop.
   close dataset $par_file.

   if sy-subrc <> 0.
     message s000 with 'ERROR OPENING/DOWNLOADING TO UNIX FILE.'.
   else.
     message s000 with 'File was created successfully!:' $par_file.
   endif.


 endform.                    " download_data_to_unix
*&---------------------------------------------------------------------*
*&      Form  make_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form make_file_name using p_file.

   concatenate 'IRS' sy-datum into p_file.
   concatenate p_file '.txt' into p_file.
   condense p_file no-gaps.

 endform.                    " make_file_name
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form modify_screen.
   loop at screen.
     if screen-group1 = 'FIL'.
       if p_chk eq 'X'.
         screen-input = 1.
       else.
         screen-input = 0.
       endif.
     endif.
     modify screen.
   endloop.

 endform.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  chk_unix_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 form chk_unix_file.

   data $par_file(80).
   constants: con_tab type x value '09'.
   data: str type string.

   if par_file eq space.
     exit.
   endif.

   concatenate '/usr/sap/UP2/FI_AP_DISTRIBUTION_LIST/' par_file
into $par_file.

*   CONCATENATE '/usr/sap/EDI_SAP/BOM/' par_file
*INTO $par_file.

   open dataset $par_file for input.
   if sy-subrc = 0.
*     DELETE DATASET $par_file.
     write / 'File found'.
   else.
     write / 'File not found'.
   endif.

 endform.                    " chk_unix_file
