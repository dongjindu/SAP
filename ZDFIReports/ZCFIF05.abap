*&-------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 13/02/2004
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K907453
*& Addl documentation     :1
*& Description  : FM Budget Download / Release Month/Quarter/Half/Year
*& FM actual total : FMIT
*&   RVERS, RYEAR, FIKRS, RFISTL, RFONDS, RFIPEX, TSL01
*
*& Modification Log
*& Date       Developer   Index    Request ID      Description
* 09/28/2011    KDM       KDM01
*&--------------------------------------------------------------------


report zcfif05 line-size 190
               line-count 58
               no standard page heading
               message-id zmfi.

include <icon>.
include zcfi_excel_ole.

*copy program : WISP_LOESCHE_LAYOUT

tables: fmci, fmcit,    "commitment
        fmfctr, fmfctrt,  "fund center
*        fmhictr,          "fund center hier
        fmhisv,          "fund center hier
        bppe.
tables: fmep,fmsu.
tables: fkrs, fpos, tbpfm.
tables: sscrfields.

data: ibppe like bppe occurs 0 with header line.

data: begin of itab occurs 0,
         fictr   like fmfctr-fictr,
*         FIPOS   like fmci-FIPOS,
         fipos   like fmit-rfipex,
         bezei like fmcit-bezei,
         geber   like bppe-geber,
         wtp01   like bppe-wtp01,
         wtp02   like bppe-wtp02,
         wtp03   like bppe-wtp03,
         wtp04   like bppe-wtp04,
         wtp05   like bppe-wtp05,
         wtp06   like bppe-wtp06,
         wtp07   like bppe-wtp07,
         wtp08   like bppe-wtp08,
         wtp09   like bppe-wtp09,
         wtp10   like bppe-wtp10,
         wtp11   like bppe-wtp11,
         wtp12   like bppe-wtp12,
         profil  like tbpfm-profil,
         posit   like fmci-posit,
      end of itab.

data : begin of ifmfctr occurs 0,
         ctr_objnr  like fmfctr-ctr_objnr,
         fictr      like fmfctr-fictr,
         parent_obj like fmhictr-parent_obj,
       end of ifmfctr.

data : begin of ifmci occurs 0,
         fipos    like fmci-fipos,
         bezei  like fmcit-bezei,
         posit    like fmep-posit,
       end of ifmci.

* Sam file Layout
data : begin of rec occurs 10,
             geber like fmps-geber,   "fund
             fistl like bpfmps-fistl, "fund center
             perio like bpdy-perio,   "period
             fipos like bpfmps-fipos, "commitment
             wert(15) type c,         "amt
        end of rec.


data: begin of iftab occurs 0,
         fictr     like fmfctr-fictr,
         fipos     like fmci-fipos,
         bezei   like fmcit-bezei,
         geber     like bppe-geber,
         wtp01(15) type c,
         wtp02(15) type c,
         wtp03(15) type c,
         wtp04(15) type c,
         wtp05(15) type c,
         wtp06(15) type c,
         wtp07(15) type c,
         wtp08(15) type c,
         wtp09(15) type c,
         wtp10(15) type c,
         wtp11(15) type c,
         wtp12(15) type c,
      end of iftab.

* Active availability control on commitment budget
data: begin of fmctl occurs 0,
         fictr     like fmfctr-fictr,
         fipos     like fmci-fipos,
         geber     like bppe-geber,
         profil    like tbpfm-profil,
      end of fmctl.
*---2004/02/20 Actual
data: it_fmit like fmit occurs 0 with header line.
data: it_message type table of zdsfi_fm_err_01.

*******************************************************
* for excel upload - start
tables: alsmex_tabline.

data: begin of iexcel occurs 0.
        include structure alsmex_tabline.
data: end of iexcel.

* No of columns
data: begin of data_tab,
       value_0001(50),
       value_0002(50),
       value_0003(50),
       value_0004(50),
       value_0005(50),
       value_0006(50),
       value_0007(50),
       value_0008(50),
       value_0009(50),
       value_0010(50),
       value_0011(50),
       value_0012(50),
       value_0013(50),
       value_0014(50),
       value_0015(50),
       value_0016(50),
       value_0017(50),
       value_0018(50),
       value_0019(50),
       value_0020(50),
       value_0021(50),
       value_0022(50),
       value_0023(50),
       value_0024(50),
       value_0025(50),
       value_0026(50),
       value_0027(50),
       value_0028(50),
       value_0029(50),
       value_0030(50),
       value_0031(50),
       value_0032(50),
       value_0033(50),
       value_0034(50),
       value_0035(50),
       value_0036(50),
       value_0037(50),
       value_0038(50),
       value_0039(50),
       value_0040(50),
       value_0041(50),
       value_0042(50),
       value_0043(50),
       value_0044(50),
       value_0045(50),
       value_0046(50),
       value_0047(50),
       value_0048(50),
       value_0049(50),
       value_0050(50),
       value_0051(50),
       value_0052(50),
       value_0053(50),
       value_0054(50),
       value_0055(50),
       value_0056(50),
       value_0057(50),
       value_0058(50),
       value_0059(50),
       value_0060(50),
       value_0061(50),
       value_0062(50),
       value_0063(50),
       value_0064(50),
       value_0065(50),
       value_0066(50),
       value_0067(50),
       value_0068(50),
       value_0069(50),
       value_0070(50),
       value_0071(50),
       value_0072(50),
       value_0073(50),
       value_0074(50),
       value_0075(50),
       value_0076(50),
       value_0077(50),
       value_0078(50),
       value_0079(50),
       value_0080(50),
       value_0081(50),
       value_0082(50),
       value_0083(50),
       value_0084(50),
       value_0085(50),
       value_0086(50),
       value_0087(50),
       value_0088(50),
       value_0089(50),
       value_0090(50),
       value_0091(50),
       value_0092(50),
       value_0093(50),
       value_0094(50),
       value_0095(50),
       value_0096(50),
       value_0097(50),
       value_0098(50),
       value_0099(50),
       value_0100(50).
data: end of data_tab.

data: tind(4) type n.
data: zwfeld(19).
field-symbols: <fs1>.
* for excel upload - end
*******************************************************

* for combobox
type-pools: vrm.
data: it_val type vrm_values,
      w_line like line of it_val.

*--work
data : wa_t_cnt type i,
       wa_carry   like bppe-wtp01.


parameters: p_act(1) type c as listbox visible length 25 obligatory.

selection-screen begin of block sb with frame. " TITLE text-s10.
parameters :
        p_fik  like fmps-fikrs memory id fik obligatory
                                      default 'H201',
        p_gjr  like bpdy-jahr  memory id gjr obligatory
                                      default sy-datum+0(4).
selection-screen end of block sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release


selection-screen begin of block sl with frame.
select-options: p_fictr for fmfctr-fictr,
                p_fipos for fmci-fipos,
                p_geber for bppe-geber,
                p_prof  for tbpfm-profil. " default 'B' option NE.

parameters: p_ver  like bppe-versn default '000'.
* ' ' - available.    'X' - released

selection-screen begin of line.
selection-screen comment (15) c_scale.
selection-screen position 33.
parameters: p_r like rfpdo1-ts70skal  default '0'.
selection-screen comment 35(1) c_slash.
selection-screen position 37.
parameters: p_d like rfpdo1-ts70skal  default '0'.
selection-screen end of line.
selection-screen end of block sl.

parameters : p_file like rlgrap-filename default
   'c:\temp\budget.xls',
             noheader as checkbox default 'X',
             p_rev    as checkbox.


data: g_per(2)  type n.
data: g_bldat like bpdy-bldat,
      g_subrc like sy-subrc.

* for BDC
***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input
*  for programs doing a data transfer by CALL TRANSACTION USING
selection-screen begin of line.
parameters session radiobutton group ctu.  "create session
selection-screen comment 3(20) text-s07 for field session.
selection-screen position 45.
parameters ctu     radiobutton group  ctu.  "call transaction
selection-screen comment 48(20) text-s08 for field ctu.
selection-screen end of line.

parameters group(12) no-display.    "group name of session
* run-mode
* A: show all dynpros
* E: show dynpro on error only
* N: do not display dynpro
parameters ctumode like ctu_params-dismode default 'N'. " no-display.

* user for session in batch
parameters user(12) default sy-uname no-display.
parameters cupdate like ctu_params-updmode default 'L' no-display.
"S: synchronously
"A: asynchronously
"L: local
parameters holddate like sy-datum no-display.
* 'X' = keep   session if finished
parameters keep(1) type c default ' ' no-display.
"' ' = delete session if finished
parameters nodata default '/' lower case no-display.          "nodata
* 'X' = no transaction logging
parameters smalllog(1) type c default ' ' no-display.

* comments/Notes
selection-screen begin of block blk2 with frame title text-003.
selection-screen begin of line.
selection-screen comment  1(60) cmt1.
selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt2.
*selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt3.
*selection-screen end of line.
selection-screen end of block blk2.

selection-screen function key 1.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
data:   bdcdata like bdcdata    occurs 0 with header line.
*       messages of call transaction
data:   messtab like bdcmsgcoll occurs 0 with header line.
*       error session opened (' ' or 'X')
data:   e_group_opened.
*       message texts
tables: t100.

* for function
data: g_func(1) type c.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
initialization.
  ctu = 'X'.
*  session = ' '.
  group = 'FM-Release'.

* for combo box
  w_line-key  = 'U'.
  w_line-text = 'Upload Budget'.
  append w_line to it_val.
  w_line-key  = 'R'.
  w_line-text = 'Release Budget'.
  append w_line to it_val.
  w_line-key  = 'B'.
  w_line-text = 'Display Released Budget'.
  append w_line to it_val.
  p_act = 'U'.

* initial screen text
  c_scale = 'Scale/Decimal'.
  c_slash = '/'.

  cmt1 = 'Please make sure date format mm/dd/yyyy in your profile'.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
at selection-screen output.
  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_ACT'
      values = it_val.

** : Excel File layout
  sscrfields-functxt_01 = 'File Layout'.
* WRITE icon_plant AS ICON TO text_001.

at selection-screen.
* group and user must be filled for create session
  if session = 'X' and
     group = space or user = space.
    message e613(ms).
  endif.

* Function Key
  case  sy-ucomm.
    when  'FC01'.    "DISPLAY
      perform  get_layout.
  endcase.

at selection-screen on value-request for p_file.
*->Popup for looking Excel file in Local server
  perform get_filename_dual changing p_file.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
start-of-selection.

  perform get_master_info.
* upload
  if p_act = 'U'.
    perform upload_pc_file.
    loop at iftab.
* filter selection option
      check iftab-fictr in p_fictr
        and iftab-fipos in p_fipos
        and iftab-geber in p_geber.

      move-corresponding iftab to itab. append itab.
    endloop.
* read database
  else.
    perform get_bbpe.
    perform delete_zero.
  endif.

  perform get_budget_period.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
end-of-selection.

  perform display_data.


************************************************************************
top-of-page.
  perform top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
top-of-page during line-selection.
  perform top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
at line-selection.
  perform pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
at pf13.
  perform data_download.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
at pf16.
  perform release using 'X'.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
at pf17.
  perform release using ' '.

*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
form get_master_info.
* commitment Item
  select * into corresponding fields of table ifmci
     from fmci
     where fikrs =  p_fik
       and fipos in p_fipos.

* commitment item text
  loop at ifmci.
    select single bezei into ifmci-bezei
       from fmcit
       where spras = sy-langu
         and fikrs = p_fik
         and fipex = ifmci-fipos.
    modify ifmci.
  endloop.

* Fund Center
  select * into corresponding fields of table ifmfctr
     from fmfctr
     where fikrs =  p_fik
       and fictr in p_fictr.

** : Fund Center Hiearachy change(09/28/2011, KDM01)
** : Fund Center Logic change(Hiearchy Table fmhictr --> FMHISV)
** : Display all fund centers that belong to HMMA)
** Fund Center Hiearchy (select end node only)
*  LOOP AT ifmfctr.
*    SELECT SINGLE * FROM fmhictr
*       WHERE ctr_objnr = ifmfctr-ctr_objnr.
*    IF fmhictr-parent_obj = space.
*      DELETE ifmfctr.
*    ENDIF.
*  ENDLOOP.

  loop at ifmfctr.
    select single * from fmhisv
       where fikrs = p_fik
         and hivarnt = '0000'
         and hiroot_st = 'HMMA'
         and fistl = ifmfctr-fictr.
    if fmhisv-child_st <> space.
      delete ifmfctr.
    endif.
  endloop.

endform.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
form data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  call function 'DOWNLOAD'
    exporting
      filename = p_file
      filetype = 'WK1'
    tables
      data_tab = itab.

  write:/ p_file, ' is created...'.
endform.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
form get_bbpe.
  refresh itab.

  select * from bppe
    into table ibppe
    for all entries in ifmfctr
    where gjahr =  p_gjr
      and versn =  p_ver
      and objnr =  ifmfctr-ctr_objnr
      and geber in p_geber.

  loop at ibppe.
* if action is 'B', then just select released budget only
    if p_act = 'B'.
      check ibppe-wrttp = '46'.
    endif.

* filter for commitment item
    read table ifmci  with key posit = ibppe-posit.
    check sy-subrc = 0.
*   check itab-fipos in p_fipos.
    itab-fipos   = ifmci-fipos.
    itab-bezei = ifmci-bezei.

    read table ifmfctr with key ctr_objnr = ibppe-objnr.
    itab-fictr = ifmfctr-fictr.
*   check itab-fictr in p_fictr.


    move-corresponding ibppe to itab.

*   available budget = orgin - released
    if p_act = 'R' and ibppe-wrttp = '46'.
      itab-wtp01 = -1 * itab-wtp01.
      itab-wtp02 = -1 * itab-wtp02.
      itab-wtp03 = -1 * itab-wtp03.
      itab-wtp04 = -1 * itab-wtp04.
      itab-wtp05 = -1 * itab-wtp05.
      itab-wtp06 = -1 * itab-wtp06.
      itab-wtp07 = -1 * itab-wtp07.
      itab-wtp08 = -1 * itab-wtp08.
      itab-wtp09 = -1 * itab-wtp09.
      itab-wtp10 = -1 * itab-wtp10.
      itab-wtp11 = -1 * itab-wtp11.
      itab-wtp12 = -1 * itab-wtp12.
    endif.

    collect itab.

  endloop.
endform.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
form display_data.
*===carryfoward 2004/02/20
  describe table itab lines wa_t_cnt.
  if wa_t_cnt > 0.
    select * into corresponding fields of table it_fmit
    from fmit
    for all entries in itab
    where  rbukrs = 'H201'
    and    rfistl = itab-fictr
    and    rfonds = itab-geber
    and    rfipex = itab-fipos
    and    rwrttp in ('50', '51')
    and    rbtart = '0350'.
  endif.
  sort itab by fictr fipos.
  loop at itab.
*----read carryfoward
    read table it_fmit with key rfistl = itab-fictr
                                rfonds = itab-geber
                                rfipex = itab-fipos.
    if sy-subrc = 0.
      wa_carry = it_fmit-hslvt.
    else.
      wa_carry = 0.
    endif.
*----
    format color col_key.
    write:/ itab-fictr(7),
            itab-fipos(7) no-gap,
            itab-bezei  no-gap,
            itab-geber  ,
            itab-profil(1).
    format color col_normal.
    write:
      48(10) wa_carry    round p_r decimals p_d no-gap,
      58(10) itab-wtp01  round p_r decimals p_d no-gap,
      68(10) itab-wtp02  round p_r decimals p_d no-gap,
      78(10) itab-wtp03  round p_r decimals p_d no-gap,
      88(10) itab-wtp04  round p_r decimals p_d no-gap,
      98(10) itab-wtp05  round p_r decimals p_d no-gap,
     108(10) itab-wtp06  round p_r decimals p_d no-gap,
     118(10) itab-wtp07  round p_r decimals p_d no-gap,
     128(10) itab-wtp08  round p_r decimals p_d no-gap,
     138(10) itab-wtp09  round p_r decimals p_d no-gap,
     148(10) itab-wtp10  round p_r decimals p_d no-gap,
     158(10) itab-wtp11  round p_r decimals p_d no-gap,
     168(10) itab-wtp12  round p_r decimals p_d no-gap.
  endloop.
  uline.
endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_REC
*&---------------------------------------------------------------------*
form check_rec.
  clear g_subrc.

  select single * from fmfctr
                  where fictr eq rec-fistl
                    and fikrs eq p_fik.
  if sy-subrc ne 0.
    write:/ 'No fund center : ', rec-fistl.
    g_subrc = 1.
  else.
    select single * from fmci
                    where fipos eq rec-fipos
                      and fikrs eq p_fik.
    if sy-subrc ne 0.
      write:/ 'No commitment item: ', rec-fipos.
      g_subrc = 2.
    else.
      if rec-wert eq 0.
        write:/ 'Amt is zero'.
        g_subrc = 3.
      endif.
    endif.
  endif.

endform.                               " CHECK_REC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
form bdc_fld using fnam fval.
  clear bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  append bdcdata.
endform.
*&---------------------------------------------------------------------*
*&      Form  release_budget
*&---------------------------------------------------------------------*
form release_budget.
  data: l_mmyyyy like rvdat-extdatum,
        l_date   like sy-datum.
  data: l_year(4),
        l_perio(3).

*  if p_act = 'R'.
*    l_mmyyyy(2) = G_PER.
*    l_mmyyyy+2(4) = P_GJR.
*    CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_INPUT'
*         EXPORTING
*              DIALOG_DATE_IS_IN_THE_PAST = ' '
*              EXTERNAL_DATE              = l_mmyyyy
*              EXTERNAL_PERIOD            = 'M'
*         IMPORTING
*              INTERNAL_DATE              = l_date.
*  endif.
*temp... use current date

** : Block : FMMA don't input posting date(Change : 2011.09.28, KDM01)
*  l_date = sy-datum.
*
*  g_bldat+4(4) = l_date(4).    "yyyy
*  g_bldat(2)   = l_date+4(2).  "mm
*  g_bldat+2(2) = l_date+6(2).  "dd


** : Sort for Posting (Change : 2011.09.28, KDM02)
** : geber fistl --> perio fistl
*  SORT rec BY geber fistl.
  sort rec by fistl perio.

  data:l_tabix(02) type n,
       l_fldname1(20),
       l_fldname2(20),
       l_fldname3(20).


  perform open_group.
  loop at rec.
*   AT NEW fistl.
*    CLEAR: l_tabix.
*    PERFORM bdc_dynpro      USING 'SAPMKBUA'         '0111'.
*    PERFORM bdc_field       USING 'BPDY-BLDAT'       g_bldat.
*    PERFORM bdc_field       USING 'FMPS-FIKRS'       p_fik.
*
*    IF p_ver <> '000'.
*      PERFORM bdc_field       USING 'BPDY-VERSN'       p_ver.
*    ELSE.
*      PERFORM bdc_field       USING 'BPDY-VERSN'       space.
*    ENDIF.
*
*    PERFORM bdc_field       USING 'FMPS-GEBER'       rec-geber.
*    PERFORM bdc_fld         USING 'BPDY-JAHR'        p_gjr.
*    PERFORM bdc_fld         USING 'BPDY-PERIO'       rec-perio.
*    PERFORM bdc_field       USING 'BPDY-TI_GES'      'X'. "overall
*    PERFORM bdc_field       USING 'BPDY-SGTXT'       rec-fistl.
*    PERFORM bdc_field       USING 'BDC_OKCODE'       '/00'.
**   ENDAT.
**   ADD 1 TO L_TABIX.
*
*    l_tabix = 1.
*    CLEAR:l_fldname1,l_fldname2,l_fldname3.
*    CONCATENATE 'BPFMPS-FISTL' '(' l_tabix ')' INTO l_fldname1.
*    CONCATENATE 'BPFMPS-FIPOS' '(' l_tabix ')' INTO l_fldname2.
*    CONCATENATE 'BPAK-WERT'    '(' l_tabix ')' INTO l_fldname3.
*
*    PERFORM bdc_dynpro      USING 'SAPMKBUA'         '0320'.
*    PERFORM bdc_field       USING l_fldname1         rec-fistl.
*    PERFORM bdc_field       USING l_fldname2         rec-fipos.
*    PERFORM bdc_fld         USING l_fldname3         rec-wert.
*
*    PERFORM bdc_field       USING 'BDC_OKCODE'       '/00'.
**======2004/02/17
*    PERFORM bdc_fld         USING 'MARK(01)'         'X'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'       '=DETA'.
*
*    PERFORM bdc_dynpro      USING 'SAPMKBUA'         '0330'.
*    PERFORM bdc_fld         USING 'BPAK-SGTEXT'      '101Initial plan'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'       '=ENTE'.
*
** It is strange, standard program just ignore multi line..
** Why? ... So block at end of...
** and standard program just create one-by-one document
**  AT END OF fistl.
*    PERFORM bdc_dynpro      USING 'SAPMKBUA'         '0320'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'       '=BUCH'.
*
*    IF p_act = 'U'.  " upload file & post original budget
*      PERFORM bdc_transaction USING 'FR33'.
*    ELSEIF p_act = 'R'.           " release budget
*      PERFORM bdc_transaction USING 'FR35'.                 "'FM9J'.
*    ENDIF.
**  ENDAT.

    l_year = p_gjr.
    l_perio = rec-perio.

    at new perio.
      clear: l_tabix.
      perform bdc_dynpro      using 'SAPLKBPB' '0200'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'BPDY-PERIO'.
      perform bdc_field       using 'FMDY-FIKRS'
                                    p_fik.

      if p_ver <> '000'.
        perform bdc_field       using 'BPDY-VERSN'       p_ver.
      else.
        perform bdc_field       using 'BPDY-VERSN'       space.
      endif.

*      perform bdc_field       using 'BPDY-BLDAT'
*                                    g_bldat.
      perform bdc_field       using 'BPDY-GJAHR'
                                    l_year.
      perform bdc_field       using 'BPDY-PERIO'
                                    l_perio.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
    endat.

    add 1 to l_tabix.

*perform bdc_field       using 'BPDY-WGES'
*                              record-WGES_006.
    perform bdc_dynpro      using 'SAPLKBPB' '0400'.
*                                  '/00'.

    clear:l_fldname1,l_fldname2,l_fldname3.
    concatenate 'FMDY-FICTR'  '(' l_tabix ')' into l_fldname1.
    concatenate 'FMDY-FIPEX'  '(' l_tabix ')' into l_fldname2.
    concatenate 'FMBPDY-VAL0' '(' l_tabix ')' into l_fldname3.

    perform bdc_field       using 'BDC_CURSOR'
                                  'FMBPDY-VAL0(01)'.

    perform bdc_field       using l_fldname1 rec-fistl.
    perform bdc_field       using l_fldname2 rec-fipos.
    perform bdc_field       using l_fldname3 rec-wert.

    if l_tabix >= 11.
      perform bdc_field       using 'BDC_OKCODE' '=P+'.
      l_tabix = 0.
    endif.

    at end of perio.
      perform bdc_dynpro      using 'SAPLKBPB' '0400'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=POST'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'FMBPDY-VAL0(01)'.

      if p_act = 'U'.  " upload file & post original budget
        perform bdc_transaction_n using 'FR50'.
      elseif p_act = 'R'.           " release budget
        perform bdc_transaction_n using 'FR51'.                 "'FM9J'.
      endif.

      clear : bdcdata, messtab.
      refresh : bdcdata, messtab.
      l_tabix = 0.

    endat.
  endloop.

  perform close_group.
endform.                    " release_budget
*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
form fill_budget.
  refresh rec.
  loop at itab.

    rec-fistl = itab-fictr. "fundcenter
    rec-fipos = itab-fipos. "commitment item
    rec-geber = itab-geber. "fund

* Action : Upload
    if p_act = 'U'.  " upload file & post original budget
      rec-wert = itab-wtp01.     perform append_rec using 1.
      rec-wert = itab-wtp02.     perform append_rec using 2.
      rec-wert = itab-wtp03.     perform append_rec using 3.
      rec-wert = itab-wtp04.     perform append_rec using 4.
      rec-wert = itab-wtp05.     perform append_rec using 5.
      rec-wert = itab-wtp06.     perform append_rec using 6.
      rec-wert = itab-wtp07.     perform append_rec using 7.
      rec-wert = itab-wtp08.     perform append_rec using 8.
      rec-wert = itab-wtp09.     perform append_rec using 9.
      rec-wert = itab-wtp10.     perform append_rec using 10.
      rec-wert = itab-wtp11.     perform append_rec using 11.
      rec-wert = itab-wtp12.     perform append_rec using 12.
    else.
* Action: Release
* check release period
      case itab-profil.
        when 'M'.   "Month
          case g_per.
            when 1.   rec-wert = itab-wtp01.
            when 2.   rec-wert = itab-wtp02.
            when 3.   rec-wert = itab-wtp03.
            when 4.   rec-wert = itab-wtp04.
            when 5.   rec-wert = itab-wtp05.
            when 6.   rec-wert = itab-wtp06.
            when 7.   rec-wert = itab-wtp07.
            when 8.   rec-wert = itab-wtp08.
            when 9.   rec-wert = itab-wtp09.
            when 10.  rec-wert = itab-wtp10.
            when 11.  rec-wert = itab-wtp11.
            when 12.  rec-wert = itab-wtp12.
          endcase.
          perform append_rec using g_per.

        when 'Q'.   "Quaterly
          check g_per = 1 or g_per = 4 or g_per = 7 or g_per = 10.
          if g_per = 1.
            rec-wert = itab-wtp01.     perform append_rec using 1.
            rec-wert = itab-wtp02.     perform append_rec using 2.
            rec-wert = itab-wtp03.     perform append_rec using 3.
          elseif g_per = 4.
            rec-wert = itab-wtp04.     perform append_rec using 4.
            rec-wert = itab-wtp05.     perform append_rec using 5.
            rec-wert = itab-wtp06.     perform append_rec using 6.
          elseif g_per = 7.
            rec-wert = itab-wtp07.     perform append_rec using 7.
            rec-wert = itab-wtp08.     perform append_rec using 8.
            rec-wert = itab-wtp09.     perform append_rec using 9.
          else.
            rec-wert = itab-wtp10.     perform append_rec using 10.
            rec-wert = itab-wtp11.     perform append_rec using 11.
            rec-wert = itab-wtp12.     perform append_rec using 12.
          endif.
        when 'H'.   "Half
          check g_per = 1 or g_per = 7.
          if g_per = 1.
            rec-wert = itab-wtp01.     perform append_rec using 1.
            rec-wert = itab-wtp02.     perform append_rec using 2.
            rec-wert = itab-wtp03.     perform append_rec using 3.
            rec-wert = itab-wtp04.     perform append_rec using 4.
            rec-wert = itab-wtp05.     perform append_rec using 5.
            rec-wert = itab-wtp06.     perform append_rec using 6.
          else.
            rec-wert = itab-wtp07.     perform append_rec using 7.
            rec-wert = itab-wtp08.     perform append_rec using 8.
            rec-wert = itab-wtp09.     perform append_rec using 9.
            rec-wert = itab-wtp10.     perform append_rec using 10.
            rec-wert = itab-wtp11.     perform append_rec using 11.
            rec-wert = itab-wtp12.     perform append_rec using 12.
          endif.
        when 'Y'.   "Year
          check g_per = 1.
          rec-wert = itab-wtp01.     perform append_rec using 1.
          rec-wert = itab-wtp02.     perform append_rec using 2.
          rec-wert = itab-wtp03.     perform append_rec using 3.
          rec-wert = itab-wtp04.     perform append_rec using 4.
          rec-wert = itab-wtp05.     perform append_rec using 5.
          rec-wert = itab-wtp06.     perform append_rec using 6.
          rec-wert = itab-wtp07.     perform append_rec using 7.
          rec-wert = itab-wtp08.     perform append_rec using 8.
          rec-wert = itab-wtp09.     perform append_rec using 9.
          rec-wert = itab-wtp10.     perform append_rec using 10.
          rec-wert = itab-wtp11.     perform append_rec using 11.
          rec-wert = itab-wtp12.     perform append_rec using 12.

        when others.
          continue.
      endcase.
    endif.
  endloop.
endform.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  append_rec
*&---------------------------------------------------------------------*
form append_rec using l_per.
  if p_rev = 'X'.
    rec-wert = -1 * rec-wert.
  endif.

  if rec-wert <> 0.
    rec-perio = l_per.
    append rec.
  endif.
endform.                    " append_rec
*&---------------------------------------------------------------------*
*&      Form  delete_zero
*&---------------------------------------------------------------------*
form delete_zero.
  loop at itab.
    if  itab-wtp01 = 0
    and itab-wtp02 = 0
    and itab-wtp03 = 0
    and itab-wtp04 = 0
    and itab-wtp05 = 0
    and itab-wtp06 = 0
    and itab-wtp07 = 0
    and itab-wtp08 = 0
    and itab-wtp09 = 0
    and itab-wtp10 = 0
    and itab-wtp11 = 0
    and itab-wtp12 = 0.
      delete itab.
    endif.
  endloop.
endform.                    " delete_zero
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
form upload_pc_file.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 100
      i_end_row               = 30000
    tables
      intern                  = iexcel
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    write: / 'EXCEL UPLOAD FAILED ', sy-subrc.
  else.
    sort iexcel by row col.
    loop at iexcel.
      if noheader = 'X' and iexcel-row = 1.
        continue.
      endif.
      tind = iexcel-col.
      concatenate 'DATA_TAB-VALUE_' tind into zwfeld.
      assign (zwfeld) to <fs1>.
      <fs1> = iexcel-value.
      at end of row.
*       APPEND data_tab.

        iftab-fictr     = data_tab-value_0001.
        iftab-fipos     = data_tab-value_0002.
        iftab-bezei   = data_tab-value_0003.
        iftab-geber     = data_tab-value_0004.
        iftab-wtp01     = data_tab-value_0005.
        iftab-wtp02     = data_tab-value_0006.
        iftab-wtp03     = data_tab-value_0007.
        iftab-wtp04     = data_tab-value_0008.
        iftab-wtp05     = data_tab-value_0009.
        iftab-wtp06     = data_tab-value_0010.
        iftab-wtp07     = data_tab-value_0011.
        iftab-wtp08     = data_tab-value_0012.
        iftab-wtp09     = data_tab-value_0013.
        iftab-wtp10     = data_tab-value_0014.
        iftab-wtp11     = data_tab-value_0015.
        iftab-wtp12     = data_tab-value_0016.
        append iftab.
        clear data_tab.
      endat.
    endloop.
  endif.


*  CALL FUNCTION 'WS_UPLOAD'
*       EXPORTING
*            FILENAME            = p_file
*            FILETYPE            = 'DAT'
*       TABLES
*            DATA_TAB            = iftab
*       EXCEPTIONS
*            CONVERSION_ERROR    = 1
*            INVALID_TABLE_WIDTH = 2
*            INVALID_TYPE        = 3
*            NO_BATCH            = 4
*            UNKNOWN_ERROR       = 5
*            FILE_OPEN_ERROR     = 6
*            FILE_READ_ERROR     = 7
*            OTHERS              = 8.

endform.                    " UPLOAD_PC_FILE
*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
form open_dataset using p_dataset.
  open dataset p_dataset in text mode.
  if sy-subrc <> 0.
    write: / text-e00, sy-subrc.
    stop.
  endif.
endform.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
form close_dataset using p_dataset.
  close dataset p_dataset.
endform.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
form open_group.
  if session = 'X'.
    skip.
    write: /(20) 'Create group'(i01), group.
    skip.
*   open batchinput group
    call function 'BDC_OPEN_GROUP'
      exporting
        client   = sy-mandt
        group    = group
        user     = user
        keep     = keep
        holddate = holddate.
    write: /(30) 'BDC_OPEN_GROUP'(i02),
            (12) 'returncode:'(i05),
                 sy-subrc.
  endif.
endform.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
form close_group.
  if session = 'X'.
*   close batchinput group
    call function 'BDC_CLOSE_GROUP'.
    write: /(30) 'Close session',
            (12) 'Return code =',
                 sy-subrc.
  else.
    if e_group_opened = 'X'.
      call function 'BDC_CLOSE_GROUP'.
      write: /.
      write: /(30) 'Error session created'.
    endif.
  endif.
endform.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
form bdc_transaction using tcode.
  data: l_mstring(480).
  data: l_subrc like sy-subrc.
* batch input session
  if session = 'X'.
    call function 'BDC_INSERT'
      exporting
        tcode     = tcode
      tables
        dynprotab = bdcdata.
    if smalllog <> 'X'.
      write: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    endif.
* call transaction using
  else.
    refresh messtab.
    call transaction tcode using bdcdata
                     mode   ctumode
                     update cupdate
                     messages into messtab.
    l_subrc = sy-subrc.

    if smalllog <> 'X'.
*---2004/04/21 jhs modify
*      format color COL_KEY.
*      WRITE: / 'Return code =',
*               L_SUBRC,
*               'RECORD:',
*               SY-INDEX.
*      format color COL_NORMAL.
*      LOOP AT MESSTAB.
*        SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
*                                  AND   ARBGB = MESSTAB-MSGID
*                                  AND   MSGNR = MESSTAB-MSGNR.
*        IF SY-SUBRC = 0.
*          L_MSTRING = T100-TEXT.
*          IF L_MSTRING CS '&1'.
*            REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
*            REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
*            REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
*            REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
*          ELSE.
*            REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
*          ENDIF.
*          CONDENSE L_MSTRING.
*          WRITE: / MESSTAB-MSGTYP, L_MSTRING(150).
*        ELSE.
*          WRITE: / MESSTAB.
*        ENDIF.
*      ENDLOOP.
*      SKIP.
    else.
      format color col_key.
      write: / 'Return code =',
               l_subrc,
               'RECORD:',
               sy-index.
      format color col_normal.
      loop at messtab.
        replace '&' with messtab-msgv1 into l_mstring.
        replace '&' with messtab-msgv2 into l_mstring.
        replace '&' with messtab-msgv3 into l_mstring.
        replace '&' with messtab-msgv4 into l_mstring.
        condense l_mstring.
        write: / messtab-msgtyp, l_mstring(150).
      endloop.

    endif.
*********************************************************
** Erzeugen fehlermappe ************************************************
    if l_subrc <> 0 and group <> space.
      if e_group_opened = ' '.
        call function 'BDC_OPEN_GROUP'
          exporting
            client   = sy-mandt
            group    = group
            user     = user
            keep     = keep
            holddate = holddate.
        e_group_opened = 'X'.
      endif.
      call function 'BDC_INSERT'
        exporting
          tcode     = tcode
        tables
          dynprotab = bdcdata.
    endif.
  endif.
  refresh bdcdata.
endform.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  if fval <> nodata.
    clear bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    append bdcdata.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
form get_budget_period.


  select * into corresponding fields of table fmctl
    from tbpfm
    where fikrs = p_fik
      and gjahr = p_gjr.


  loop at itab.
    read table ifmci  with key fipos = itab-fipos.
    itab-posit   = ifmci-posit.
    itab-bezei = ifmci-bezei.

    perform determine_profile_fs using    p_fik
                                          itab-fictr
                                          itab-posit
                                          itab-geber
                                          p_gjr
                                 changing itab-profil.
    if itab-profil in p_prof.
      modify itab.
    else.
      delete itab.
    endif.
  endloop.

endform.                    " get_budget_period
*---------------------------------------------------------------------*
*       FORM DETERMINE_PROFILE_FS                                     *
*---------------------------------------------------------------------*
form determine_profile_fs using    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          changing l_bprof.
  data: l_objnr like fmfctr-ctr_objnr.
  data: l_farea  like bpja-farea.
  clear: l_farea.


  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  call function 'KBPA_FIFM_GET_PROFIL'
    exporting
      i_objnr         = l_objnr
      i_posit         = l_posit
      i_geber         = l_geber
      i_gjahr         = l_gjahr
      i_farea         = l_farea
    importing
      e_profil        = l_bprof
    exceptions
      no_profil_found = 01.

  if not sy-subrc is initial.
*   Profile from FundMgt Area
    call function 'FM5B_GET_PROFILE'
      exporting
        i_fikrs           = l_fikrs
        i_fincode         = l_geber
      importing
        e_profil          = l_bprof
      exceptions
        fm_area_not_found = 1
        others            = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
form pick.
  check g_func = 'X'.

  if sy-cucol < 15.     " download
    perform data_download.
  elseif sy-cucol < 30. " test
    perform release using 'X'.
  elseif sy-cucol < 45. " run
    perform release using ' '.
  else.
    read line 1 field value p_r.
    read line 1 field value p_d.
    sy-lsind = sy-lsind - 1.
    perform display_data.

  endif.

  if it_message[] is not initial.


    call function 'ZGFI_DISP_ALV_POPUP'
      exporting
        i_grid_title     = 'FM Budgeting Result'
        i_structure_name = 'ZDSFI_FM_ERR_01'
      tables
        t_alvtab         = it_message.

    refresh it_message.
    clear it_message.

  endif.
* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
endform.                    " pick
*&---------------------------------------------------------------------*
*&      Form  release
*&---------------------------------------------------------------------*
form release using    value(l_test).
* Upload
  if p_act = 'U'.
    perform fill_budget.
    if l_test = 'X'.
      write:/ 'Release Simulation'.
      loop at rec.
        write:/ 'Log...', rec-fistl, rec-fipos, rec-geber, rec-wert.
      endloop.
    else.
      perform release_budget.
    endif.

  elseif p_act = 'R'.
* Release
    read line 1 field value g_per.
    if g_per between 1 and 12.
      perform fill_budget.

      if l_test = 'X'.
        write:/ 'Release Simulation'.
        loop at rec.
          write:/ 'Log...', rec-fistl, rec-fipos, rec-geber, rec-wert.
        endloop.
      else.
        perform release_budget.
*    sy-lsind = sy-lsind - 1.
      endif.
*   perform release_budget.
    else.
      write:/ 'Input period correctly!!!'.
    endif.
  endif.
  sy-lsind = 1.
endform.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
form top_of_page.
  write:/ 'Period:', g_per input on.
  write: ' (Round:', p_r no-gap input on,
         ', Decimal:', p_d no-gap input on, ')'.
  case p_act.
    when 'U'.
      write: ' Uploading...'     color col_group.
      if p_rev = 'X'.
        write: '(REVERSE)'     color col_group.
      endif.
    when 'R'. write: ' Releasing...'     color col_group.
    when 'B'. write: ' Released Budget'  color col_group.
  endcase.

  uline.
  g_func = 'X'.
  write: icon_execute_object as icon hotspot, 'Download(S_F1) ',
         icon_execute_object as icon hotspot, 'Test(S_F4)     ',
         icon_execute_object as icon hotspot, 'Run(S_F5)      ',
         icon_refresh        as icon hotspot, 'Refresh        '.

  hide: g_func.
  clear: g_func.
  uline.

  format color col_heading.
  write:/ 'FundCtr Commitment Item            Fund       P'.
  write:  '     Carry   Jan       Feb       Mar       Apr  ' no-gap.
  write:  '     May       Jun       Juy       Aug  ' no-gap.
  write:  '     Sep       Oct       Nov       Dec  '.
  uline.
  format color col_normal.

endform.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_DUAL
*&---------------------------------------------------------------------*
*       Look for the filepath of Uploading Source file
*----------------------------------------------------------------------*
form get_filename_dual  changing fp_file.

  data: lt_files type filetable,
        l_file   type file_table,
        l_title  type string,
        l_subrc  type sysubrc.

  data  l_init_dir type string.

  l_title = 'Choose Upload file'.

  call method cl_gui_frontend_services=>directory_get_current
    changing
      current_directory = l_init_dir.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = l_title
      initial_directory       = l_init_dir
*      default_extension       = 'EXCEL'
*      file_filter             = cl_gui_frontend_services=>filetype_excel
       file_filter            = '*.XLS,*.TXT'
    changing
      file_table              = lt_files
      rc                      = l_subrc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      others                  = 4.
  check sy-subrc = 0.
  loop at lt_files into l_file.
    fp_file = l_file.
    exit.
  endloop.

endform.                    " GET_FILENAME_DUAL
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION_N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1731   text
*----------------------------------------------------------------------*
form bdc_transaction_n using tcode.

  data: l_mstring(480).
  data: l_subrc like sy-subrc.
  data: ls_message type zdsfi_fm_err_01.

* batch input session
  if session = 'X'.
    call function 'BDC_INSERT'
      exporting
        tcode     = tcode
      tables
        dynprotab = bdcdata.
    if smalllog <> 'X'.
      write: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    endif.
* call transaction using
  else.
    refresh messtab.
    call transaction tcode using bdcdata
                     mode   ctumode
                     update cupdate
                     messages into messtab.
    l_subrc = sy-subrc.

    read table messtab with key msgtyp = 'S'.
    if sy-subrc = 0.
      ls_message-icon  = '@5B@'.
      ls_message-perio = rec-perio.
      ls_message-fistl = rec-fistl.
      ls_message-docnr = messtab-msgv1.
      ls_message-errtx = 'Successfully Posted'.
      append ls_message to it_message.
    else.
      loop at messtab.
         call function 'MESSAGE_TEXT_BUILD'
          exporting
            msgid               = messtab-msgid
            msgnr               = messtab-msgnr
            msgv1               = messtab-msgv1
            msgv2               = messtab-msgv2
            msgv3               = messtab-msgv3
            msgv4               = messtab-msgv4
          importing
            message_text_output = l_mstring.

        ls_message-icon  = '@5C@'.
        ls_message-perio = rec-perio.
        ls_message-fistl = rec-fistl.
        ls_message-errtx = l_mstring.
        append ls_message to it_message.
      endloop.
    endif.
*********************************************************
** Erzeugen fehlermappe ************************************************
    if l_subrc <> 0 and group <> space.
      if e_group_opened = ' '.
        call function 'BDC_OPEN_GROUP'
          exporting
            client   = sy-mandt
            group    = group
            user     = user
            keep     = keep
            holddate = holddate.
        e_group_opened = 'X'.
      endif.
      call function 'BDC_INSERT'
        exporting
          tcode     = tcode
        tables
          dynprotab = bdcdata.
    endif.
  endif.
  refresh bdcdata.

endform.                    " BDC_TRANSACTION_N
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_layout .
***->>>>Assign Input structure for display
  perform field_assign_for_ole_int tables gt_ole.
**->>>>Show Object table's structure
  perform show_excel_with_structure.

endform.                    " GET_LAYOUT


form field_assign_for_ole_int  tables   ft_tab   type t_oletab.

  data ls_tab type t_ole.

  define input_field.
    ls_tab-fieldname = &2.
    ls_tab-ddtext = &1.
    append ls_tab to ft_tab.
  end-of-definition.

  input_field 'FICTR'   'Fund Center'.
  input_field 'FIPOS'   'Commitment Item'.
  input_field 'bezei' 'Description'.
  input_field 'GEBER'   'Fund'.
  input_field 'WTP01'   'Jan'.
  input_field 'WTP02'   'Feb'.
  input_field 'WTP03'   'Mar'.
  input_field 'WTP04'   'Apr'.
  input_field 'WTP05'   'May'.
  input_field 'WTP06'   'June'.
  input_field 'WTP07'   'July'.
  input_field 'WTP08'   'Aug'.
  input_field 'WTP09'   'Sep'.
  input_field 'WTP10'   'Oct'.
  input_field 'WTP11'   'Nov'.
  input_field 'WTP12'   'Dec'.

endform.
