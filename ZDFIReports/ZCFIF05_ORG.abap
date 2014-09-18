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


REPORT zcfif05 LINE-SIZE 190
               LINE-COUNT 58
               NO STANDARD PAGE HEADING
               MESSAGE-ID ZMFI.

INCLUDE <icon>.
INCLUDE ZCFI_EXCEL_OLE.

*copy program : WISP_LOESCHE_LAYOUT

TABLES: fmfpo, fmfpot,    "commitment
        fmfctr, fmfctrt,  "fund center
*        fmhictr,          "fund center hier
        FMHISV,          "fund center hier
        bppe.
TABLES: fmep,fmsu.
TABLES: fkrs, fpos, tbpfm.
TABLES: sscrfields.

DATA: ibppe LIKE bppe OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab OCCURS 0,
         fictr   LIKE fmfctr-fictr,
*         FIPOS   like fmfpo-FIPOS,
         fipos   LIKE fmit-rfipex,
         bezeich LIKE fmfpot-bezeich,
         geber   LIKE bppe-geber,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
         posit   LIKE fmfpo-posit,
      END OF itab.

DATA : BEGIN OF ifmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF ifmfctr.

DATA : BEGIN OF ifmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
         bezeich  LIKE fmfpot-bezeich,
         posit    LIKE fmep-posit,
       END OF ifmfpo.

* Sam file Layout
DATA : BEGIN OF rec OCCURS 10,
             geber LIKE fmps-geber,   "fund
             fistl LIKE bpfmps-fistl, "fund center
             perio LIKE bpdy-perio,   "period
             fipos LIKE bpfmps-fipos, "commitment
             wert(15) TYPE c,         "amt
        END OF rec.


DATA: BEGIN OF iftab OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmfpo-fipos,
         bezeich   LIKE fmfpot-bezeich,
         geber     LIKE bppe-geber,
         wtp01(15) TYPE c,
         wtp02(15) TYPE c,
         wtp03(15) TYPE c,
         wtp04(15) TYPE c,
         wtp05(15) TYPE c,
         wtp06(15) TYPE c,
         wtp07(15) TYPE c,
         wtp08(15) TYPE c,
         wtp09(15) TYPE c,
         wtp10(15) TYPE c,
         wtp11(15) TYPE c,
         wtp12(15) TYPE c,
      END OF iftab.

* Active availability control on commitment budget
DATA: BEGIN OF fmctl OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmfpo-fipos,
         geber     LIKE bppe-geber,
         profil    LIKE tbpfm-profil,
      END OF fmctl.
*---2004/02/20 Actual
DATA: it_fmit LIKE fmit OCCURS 0 WITH HEADER LINE.
DATA: it_message TYPE TABLE OF ZDSFI_FM_ERR_01.

*******************************************************
* for excel upload - start
TABLES: alsmex_tabline.

DATA: BEGIN OF iexcel OCCURS 0.
        INCLUDE STRUCTURE alsmex_tabline.
DATA: END OF iexcel.

* No of columns
DATA: BEGIN OF data_tab,
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
DATA: END OF data_tab.

DATA: tind(4) TYPE n.
DATA: zwfeld(19).
FIELD-SYMBOLS: <fs1>.
* for excel upload - end
*******************************************************

* for combobox
TYPE-POOLS: vrm.
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

*--work
DATA : wa_t_cnt TYPE i,
       wa_carry   LIKE bppe-wtp01.


PARAMETERS: p_act(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME. " TITLE text-s10.
PARAMETERS :
        p_fik  LIKE fmps-fikrs MEMORY ID fik OBLIGATORY
                                      DEFAULT 'H201',
        p_gjr  LIKE bpdy-jahr  MEMORY ID gjr OBLIGATORY
                                      DEFAULT sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release


SELECTION-SCREEN BEGIN OF BLOCK sl WITH FRAME.
SELECT-OPTIONS: p_fictr FOR fmfctr-fictr,
                p_fipos FOR fmfpo-fipos,
                p_geber FOR bppe-geber,
                p_prof  FOR tbpfm-profil. " default 'B' option NE.

PARAMETERS: p_ver  LIKE bppe-versn DEFAULT '000'.
* ' ' - available.    'X' - released

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_scale.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_r LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN COMMENT 35(1) c_slash.
SELECTION-SCREEN POSITION 37.
PARAMETERS: p_d LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sl.

PARAMETERS : p_file LIKE rlgrap-filename DEFAULT
   'c:\temp\budget.xls',
             noheader AS CHECKBOX DEFAULT 'X',
             p_rev    AS CHECKBOX.


DATA: g_per(2)  TYPE n.
DATA: g_bldat LIKE bpdy-bldat,
      g_subrc LIKE sy-subrc.

* for BDC
***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input
*  for programs doing a data transfer by CALL TRANSACTION USING
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS session RADIOBUTTON GROUP ctu.  "create session
SELECTION-SCREEN COMMENT 3(20) text-s07 FOR FIELD session.
SELECTION-SCREEN POSITION 45.
PARAMETERS ctu     RADIOBUTTON GROUP  ctu.  "call transaction
SELECTION-SCREEN COMMENT 48(20) text-s08 FOR FIELD ctu.
SELECTION-SCREEN END OF LINE.

PARAMETERS group(12) NO-DISPLAY.    "group name of session
* run-mode
* A: show all dynpros
* E: show dynpro on error only
* N: do not display dynpro
PARAMETERS ctumode LIKE ctu_params-dismode DEFAULT 'N'. " no-display.

* user for session in batch
PARAMETERS user(12) DEFAULT sy-uname NO-DISPLAY.
PARAMETERS cupdate LIKE ctu_params-updmode DEFAULT 'L' NO-DISPLAY.
"S: synchronously
"A: asynchronously
"L: local
PARAMETERS holddate LIKE sy-datum NO-DISPLAY.
* 'X' = keep   session if finished
PARAMETERS keep(1) TYPE c DEFAULT ' ' NO-DISPLAY.
"' ' = delete session if finished
PARAMETERS nodata DEFAULT '/' LOWER CASE NO-DISPLAY.          "nodata
* 'X' = no transaction logging
PARAMETERS smalllog(1) TYPE c DEFAULT ' ' NO-DISPLAY.

* comments/Notes
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(60) cmt1.
SELECTION-SCREEN END OF LINE.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt2.
*selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt3.
*selection-screen end of line.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN FUNCTION KEY 1.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.

* for function
DATA: g_func(1) TYPE c.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
INITIALIZATION.
  ctu = 'X'.
*  session = ' '.
  group = 'FM-Release'.

* for combo box
  w_line-key  = 'U'.
  w_line-text = 'Upload Budget'.
  APPEND w_line TO it_val.
  w_line-key  = 'R'.
  w_line-text = 'Release Budget'.
  APPEND w_line TO it_val.
  w_line-key  = 'B'.
  w_line-text = 'Display Released Budget'.
  APPEND w_line TO it_val.
  p_act = 'U'.

* initial screen text
  c_scale = 'Scale/Decimal'.
  c_slash = '/'.

  cmt1 = 'Please make sure date format mm/dd/yyyy in your profile'.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ACT'
      values = it_val.

** : Excel File layout
  sscrfields-functxt_01 = 'File Layout'.
* WRITE icon_plant AS ICON TO text_001.

AT SELECTION-SCREEN.
* group and user must be filled for create session
  IF session = 'X' AND
     group = space OR user = space.
    MESSAGE e613(ms).
  ENDIF.

* Function Key
  CASE  SY-UCOMM.
    WHEN  'FC01'.    "DISPLAY
      PERFORM  GET_LAYOUT.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*->Popup for looking Excel file in Local server
  PERFORM get_filename_dual CHANGING p_file.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM get_master_info.
* upload
  IF p_act = 'U'.
    PERFORM upload_pc_file.
    LOOP AT iftab.
* filter selection option
      CHECK iftab-fictr IN p_fictr
        AND iftab-fipos IN p_fipos
        AND iftab-geber IN p_geber.

      MOVE-CORRESPONDING iftab TO itab. APPEND itab.
    ENDLOOP.
* read database
  ELSE.
    PERFORM get_bbpe.
    PERFORM delete_zero.
  ENDIF.

  PERFORM get_budget_period.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
END-OF-SELECTION.

  PERFORM display_data.


************************************************************************
TOP-OF-PAGE.
  PERFORM top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
AT LINE-SELECTION.
  PERFORM pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
  PERFORM data_download.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
AT PF16.
  PERFORM release USING 'X'.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
AT PF17.
  PERFORM release USING ' '.

*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
FORM get_master_info.
* commitment Item
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmfpo
     FROM fmfpo
     WHERE fikrs =  p_fik
       AND fipos IN p_fipos.

* commitment item text
  LOOP AT ifmfpo.
    SELECT SINGLE bezeich INTO ifmfpo-bezeich
       FROM fmfpot
       WHERE spras = sy-langu
         AND fikrs = p_fik
         AND fipos = ifmfpo-fipos.
    MODIFY ifmfpo.
  ENDLOOP.

* Fund Center
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmfctr
     FROM fmfctr
     WHERE fikrs =  p_fik
       AND fictr IN p_fictr.

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

  LOOP AT ifmfctr.
    SELECT SINGLE * FROM FMHISV
       WHERE FIKRS = p_fik
         and HIVARNT = '0000'
         and HIROOT_ST = 'HMMA'
         and FISTL = ifmfctr-fictr.
    IF fmhisv-child_st <> space.
      DELETE ifmfctr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      filename = p_file
      filetype = 'WK1'
    TABLES
      data_tab = itab.

  WRITE:/ p_file, ' is created...'.
ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
FORM get_bbpe.
  REFRESH itab.

  SELECT * FROM bppe
    INTO TABLE ibppe
    FOR ALL ENTRIES IN ifmfctr
    WHERE gjahr =  p_gjr
      AND versn =  p_ver
      AND objnr =  ifmfctr-ctr_objnr
      AND geber IN p_geber.

  LOOP AT ibppe.
* if action is 'B', then just select released budget only
    IF p_act = 'B'.
      CHECK ibppe-wrttp = '46'.
    ENDIF.

* filter for commitment item
    READ TABLE ifmfpo  WITH KEY posit = ibppe-posit.
    CHECK sy-subrc = 0.
*   check itab-fipos in p_fipos.
    itab-fipos   = ifmfpo-fipos.
    itab-bezeich = ifmfpo-bezeich.

    READ TABLE ifmfctr WITH KEY ctr_objnr = ibppe-objnr.
    itab-fictr = ifmfctr-fictr.
*   check itab-fictr in p_fictr.


    MOVE-CORRESPONDING ibppe TO itab.

*   available budget = orgin - released
    IF p_act = 'R' AND ibppe-wrttp = '46'.
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
    ENDIF.

    COLLECT itab.

  ENDLOOP.
ENDFORM.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.
*===carryfoward 2004/02/20
  DESCRIBE TABLE itab LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmit
    FROM fmit
    FOR ALL ENTRIES IN itab
    WHERE  rbukrs = 'H201'
    AND    rfistl = itab-fictr
    AND    rfonds = itab-geber
    AND    rfipex = itab-fipos
    AND    rwrttp IN ('50', '51')
    AND    rbtart = '0350'.
  ENDIF.
  sort itab by fictr fipos.
  LOOP AT itab.
*----read carryfoward
    READ TABLE it_fmit WITH KEY rfistl = itab-fictr
                                rfonds = itab-geber
                                rfipex = itab-fipos.
    IF sy-subrc = 0.
      wa_carry = it_fmit-hslvt.
    ELSE.
      wa_carry = 0.
    ENDIF.
*----
    FORMAT COLOR COL_KEY.
    WRITE:/ itab-fictr(7),
            itab-fipos(7) NO-GAP,
            itab-bezeich  NO-GAP,
            itab-geber  ,
            itab-profil(1).
    FORMAT COLOR COL_NORMAL.
    WRITE:
      48(10) wa_carry    ROUND p_r DECIMALS p_d NO-GAP,
      58(10) itab-wtp01  ROUND p_r DECIMALS p_d NO-GAP,
      68(10) itab-wtp02  ROUND p_r DECIMALS p_d NO-GAP,
      78(10) itab-wtp03  ROUND p_r DECIMALS p_d NO-GAP,
      88(10) itab-wtp04  ROUND p_r DECIMALS p_d NO-GAP,
      98(10) itab-wtp05  ROUND p_r DECIMALS p_d NO-GAP,
     108(10) itab-wtp06  ROUND p_r DECIMALS p_d NO-GAP,
     118(10) itab-wtp07  ROUND p_r DECIMALS p_d NO-GAP,
     128(10) itab-wtp08  ROUND p_r DECIMALS p_d NO-GAP,
     138(10) itab-wtp09  ROUND p_r DECIMALS p_d NO-GAP,
     148(10) itab-wtp10  ROUND p_r DECIMALS p_d NO-GAP,
     158(10) itab-wtp11  ROUND p_r DECIMALS p_d NO-GAP,
     168(10) itab-wtp12  ROUND p_r DECIMALS p_d NO-GAP.
  ENDLOOP.
  ULINE.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_REC
*&---------------------------------------------------------------------*
FORM check_rec.
  CLEAR g_subrc.

  SELECT SINGLE * FROM fmfctr
                  WHERE fictr EQ rec-fistl
                    AND fikrs EQ p_fik.
  IF sy-subrc NE 0.
    WRITE:/ 'No fund center : ', rec-fistl.
    g_subrc = 1.
  ELSE.
    SELECT SINGLE * FROM fmfpo
                    WHERE fipos EQ rec-fipos
                      AND fikrs EQ p_fik.
    IF sy-subrc NE 0.
      WRITE:/ 'No commitment item: ', rec-fipos.
      g_subrc = 2.
    ELSE.
      IF rec-wert EQ 0.
        WRITE:/ 'Amt is zero'.
        g_subrc = 3.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_REC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM bdc_fld USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  release_budget
*&---------------------------------------------------------------------*
FORM release_budget.
  DATA: l_mmyyyy LIKE rvdat-extdatum,
        l_date   LIKE sy-datum.
  DATA: L_YEAR(4),
        L_PERIO(3).

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
  SORT rec BY fistl perio.

  DATA:l_tabix(02) TYPE n,
       l_fldname1(20),
       l_fldname2(20),
       l_fldname3(20).


  PERFORM open_group.
  LOOP AT rec.
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

    L_YEAR = p_gjr.
    L_PERIO = rec-PERIO.

    AT NEW perio.
      CLEAR: l_tabix.
      perform bdc_dynpro      using 'SAPLKBPB' '0200'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'BPDY-PERIO'.
      perform bdc_field       using 'FMDY-FIKRS'
                                    p_fik.

      IF p_ver <> '000'.
        PERFORM bdc_field       USING 'BPDY-VERSN'       p_ver.
      ELSE.
        PERFORM bdc_field       USING 'BPDY-VERSN'       space.
      ENDIF.

*      perform bdc_field       using 'BPDY-BLDAT'
*                                    g_bldat.
      perform bdc_field       using 'BPDY-GJAHR'
                                    L_YEAR.
      perform bdc_field       using 'BPDY-PERIO'
                                    L_PERIO.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
    ENDAT.

    ADD 1 TO L_TABIX.

*perform bdc_field       using 'BPDY-WGES'
*                              record-WGES_006.
    perform bdc_dynpro      using 'SAPLKBPB' '0400'.
*                                  '/00'.

    CLEAR:l_fldname1,l_fldname2,l_fldname3.
    CONCATENATE 'FMDY-FICTR'  '(' l_tabix ')' INTO l_fldname1.
    CONCATENATE 'FMDY-FIPEX'  '(' l_tabix ')' INTO l_fldname2.
    CONCATENATE 'FMBPDY-VAL0' '(' l_tabix ')' INTO l_fldname3.

    perform bdc_field       using 'BDC_CURSOR'
                                  'FMBPDY-VAL0(01)'.

    perform bdc_field       using l_fldname1 rec-fistl.
    perform bdc_field       using l_fldname2 rec-fipos.
    perform bdc_field       using l_fldname3 rec-wert.

    IF l_tabix >= 11.
      perform bdc_field       using 'BDC_OKCODE' '=P+'.
      l_tabix = 0.
    ENDIF.

    AT END OF perio.
      perform bdc_dynpro      using 'SAPLKBPB' '0400'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=POST'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'FMBPDY-VAL0(01)'.

      IF p_act = 'U'.  " upload file & post original budget
        PERFORM bdc_transaction_n USING 'FR50'.
      ELSEIF p_act = 'R'.           " release budget
        PERFORM bdc_transaction_n USING 'FR51'.                 "'FM9J'.
      ENDIF.

      CLEAR : bdcdata, messtab.
      REFRESH : bdcdata, messtab.
      l_tabix = 0.

    ENDAT.
  ENDLOOP.

  PERFORM close_group.
ENDFORM.                    " release_budget
*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
FORM fill_budget.
  REFRESH rec.
  LOOP AT itab.

    rec-fistl = itab-fictr. "fundcenter
    rec-fipos = itab-fipos. "commitment item
    rec-geber = itab-geber. "fund

* Action : Upload
    IF p_act = 'U'.  " upload file & post original budget
      rec-wert = itab-wtp01.     PERFORM append_rec USING 1.
      rec-wert = itab-wtp02.     PERFORM append_rec USING 2.
      rec-wert = itab-wtp03.     PERFORM append_rec USING 3.
      rec-wert = itab-wtp04.     PERFORM append_rec USING 4.
      rec-wert = itab-wtp05.     PERFORM append_rec USING 5.
      rec-wert = itab-wtp06.     PERFORM append_rec USING 6.
      rec-wert = itab-wtp07.     PERFORM append_rec USING 7.
      rec-wert = itab-wtp08.     PERFORM append_rec USING 8.
      rec-wert = itab-wtp09.     PERFORM append_rec USING 9.
      rec-wert = itab-wtp10.     PERFORM append_rec USING 10.
      rec-wert = itab-wtp11.     PERFORM append_rec USING 11.
      rec-wert = itab-wtp12.     PERFORM append_rec USING 12.
    ELSE.
* Action: Release
* check release period
      CASE itab-profil.
        WHEN 'M'.   "Month
          CASE g_per.
            WHEN 1.   rec-wert = itab-wtp01.
            WHEN 2.   rec-wert = itab-wtp02.
            WHEN 3.   rec-wert = itab-wtp03.
            WHEN 4.   rec-wert = itab-wtp04.
            WHEN 5.   rec-wert = itab-wtp05.
            WHEN 6.   rec-wert = itab-wtp06.
            WHEN 7.   rec-wert = itab-wtp07.
            WHEN 8.   rec-wert = itab-wtp08.
            WHEN 9.   rec-wert = itab-wtp09.
            WHEN 10.  rec-wert = itab-wtp10.
            WHEN 11.  rec-wert = itab-wtp11.
            WHEN 12.  rec-wert = itab-wtp12.
          ENDCASE.
          PERFORM append_rec USING g_per.

        WHEN 'Q'.   "Quaterly
          CHECK g_per = 1 OR g_per = 4 OR g_per = 7 OR g_per = 10.
          IF g_per = 1.
            rec-wert = itab-wtp01.     PERFORM append_rec USING 1.
            rec-wert = itab-wtp02.     PERFORM append_rec USING 2.
            rec-wert = itab-wtp03.     PERFORM append_rec USING 3.
          ELSEIF g_per = 4.
            rec-wert = itab-wtp04.     PERFORM append_rec USING 4.
            rec-wert = itab-wtp05.     PERFORM append_rec USING 5.
            rec-wert = itab-wtp06.     PERFORM append_rec USING 6.
          ELSEIF g_per = 7.
            rec-wert = itab-wtp07.     PERFORM append_rec USING 7.
            rec-wert = itab-wtp08.     PERFORM append_rec USING 8.
            rec-wert = itab-wtp09.     PERFORM append_rec USING 9.
          ELSE.
            rec-wert = itab-wtp10.     PERFORM append_rec USING 10.
            rec-wert = itab-wtp11.     PERFORM append_rec USING 11.
            rec-wert = itab-wtp12.     PERFORM append_rec USING 12.
          ENDIF.
        WHEN 'H'.   "Half
          CHECK g_per = 1 OR g_per = 7.
          IF g_per = 1.
            rec-wert = itab-wtp01.     PERFORM append_rec USING 1.
            rec-wert = itab-wtp02.     PERFORM append_rec USING 2.
            rec-wert = itab-wtp03.     PERFORM append_rec USING 3.
            rec-wert = itab-wtp04.     PERFORM append_rec USING 4.
            rec-wert = itab-wtp05.     PERFORM append_rec USING 5.
            rec-wert = itab-wtp06.     PERFORM append_rec USING 6.
          ELSE.
            rec-wert = itab-wtp07.     PERFORM append_rec USING 7.
            rec-wert = itab-wtp08.     PERFORM append_rec USING 8.
            rec-wert = itab-wtp09.     PERFORM append_rec USING 9.
            rec-wert = itab-wtp10.     PERFORM append_rec USING 10.
            rec-wert = itab-wtp11.     PERFORM append_rec USING 11.
            rec-wert = itab-wtp12.     PERFORM append_rec USING 12.
          ENDIF.
        WHEN 'Y'.   "Year
          CHECK g_per = 1.
          rec-wert = itab-wtp01.     PERFORM append_rec USING 1.
          rec-wert = itab-wtp02.     PERFORM append_rec USING 2.
          rec-wert = itab-wtp03.     PERFORM append_rec USING 3.
          rec-wert = itab-wtp04.     PERFORM append_rec USING 4.
          rec-wert = itab-wtp05.     PERFORM append_rec USING 5.
          rec-wert = itab-wtp06.     PERFORM append_rec USING 6.
          rec-wert = itab-wtp07.     PERFORM append_rec USING 7.
          rec-wert = itab-wtp08.     PERFORM append_rec USING 8.
          rec-wert = itab-wtp09.     PERFORM append_rec USING 9.
          rec-wert = itab-wtp10.     PERFORM append_rec USING 10.
          rec-wert = itab-wtp11.     PERFORM append_rec USING 11.
          rec-wert = itab-wtp12.     PERFORM append_rec USING 12.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  append_rec
*&---------------------------------------------------------------------*
FORM append_rec USING l_per.
  IF p_rev = 'X'.
    rec-wert = -1 * rec-wert.
  ENDIF.

  IF rec-wert <> 0.
    rec-perio = l_per.
    APPEND rec.
  ENDIF.
ENDFORM.                    " append_rec
*&---------------------------------------------------------------------*
*&      Form  delete_zero
*&---------------------------------------------------------------------*
FORM delete_zero.
  LOOP AT itab.
    IF  itab-wtp01 = 0
    AND itab-wtp02 = 0
    AND itab-wtp03 = 0
    AND itab-wtp04 = 0
    AND itab-wtp05 = 0
    AND itab-wtp06 = 0
    AND itab-wtp07 = 0
    AND itab-wtp08 = 0
    AND itab-wtp09 = 0
    AND itab-wtp10 = 0
    AND itab-wtp11 = 0
    AND itab-wtp12 = 0.
      DELETE itab.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_zero
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM upload_pc_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 100
      i_end_row               = 30000
    TABLES
      intern                  = iexcel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    WRITE: / 'EXCEL UPLOAD FAILED ', sy-subrc.
  ELSE.
    SORT iexcel BY row col.
    LOOP AT iexcel.
      IF noheader = 'X' AND iexcel-row = 1.
        CONTINUE.
      ENDIF.
      tind = iexcel-col.
      CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
      ASSIGN (zwfeld) TO <fs1>.
      <fs1> = iexcel-value.
      AT END OF row.
*       APPEND data_tab.

        iftab-fictr     = data_tab-value_0001.
        iftab-fipos     = data_tab-value_0002.
        iftab-bezeich   = data_tab-value_0003.
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
        APPEND iftab.
        CLEAR data_tab.
      ENDAT.
    ENDLOOP.
  ENDIF.


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

ENDFORM.                    " UPLOAD_PC_FILE
*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
FORM open_dataset USING p_dataset.
  OPEN DATASET p_dataset IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE: / text-e00, sy-subrc.
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
FORM close_dataset USING p_dataset.
  CLOSE DATASET p_dataset.
ENDFORM.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM open_group.
  IF session = 'X'.
    SKIP.
    WRITE: /(20) 'Create group'(i01), group.
    SKIP.
*   open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client   = sy-mandt
        group    = group
        user     = user
        keep     = keep
        holddate = holddate.
    WRITE: /(30) 'BDC_OPEN_GROUP'(i02),
            (12) 'returncode:'(i05),
                 sy-subrc.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
  IF session = 'X'.
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    WRITE: /(30) 'Close session',
            (12) 'Return code =',
                 sy-subrc.
  ELSE.
    IF e_group_opened = 'X'.
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      WRITE: /.
      WRITE: /(30) 'Error session created'.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
* batch input session
  IF session = 'X'.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode     = tcode
      TABLES
        dynprotab = bdcdata.
    IF smalllog <> 'X'.
      WRITE: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH messtab.
    CALL TRANSACTION tcode USING bdcdata
                     MODE   ctumode
                     UPDATE cupdate
                     MESSAGES INTO messtab.
    l_subrc = sy-subrc.

    IF smalllog <> 'X'.
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
    ELSE.
      FORMAT COLOR COL_KEY.
      WRITE: / 'Return code =',
               l_subrc,
               'RECORD:',
               sy-index.
      FORMAT COLOR COL_NORMAL.
      LOOP AT messtab.
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
        CONDENSE l_mstring.
        WRITE: / messtab-msgtyp, l_mstring(150).
      ENDLOOP.

    ENDIF.
*********************************************************
** Erzeugen fehlermappe ************************************************
    IF l_subrc <> 0 AND group <> space.
      IF e_group_opened = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
          EXPORTING
            client   = sy-mandt
            group    = group
            user     = user
            keep     = keep
            holddate = holddate.
        e_group_opened = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode     = tcode
        TABLES
          dynprotab = bdcdata.
    ENDIF.
  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> nodata.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE fmctl
    FROM tbpfm
    WHERE fikrs = p_fik
      AND gjahr = p_gjr.


  LOOP AT itab.
    READ TABLE ifmfpo  WITH KEY fipos = itab-fipos.
    itab-posit   = ifmfpo-posit.
    itab-bezeich = ifmfpo-bezeich.

    PERFORM determine_profile_fs USING    p_fik
                                          itab-fictr
                                          itab-posit
                                          itab-geber
                                          p_gjr
                                 CHANGING itab-profil.
    IF itab-profil IN p_prof.
      MODIFY itab.
    ELSE.
      DELETE itab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_budget_period
*---------------------------------------------------------------------*
*       FORM DETERMINE_PROFILE_FS                                     *
*---------------------------------------------------------------------*
FORM determine_profile_fs USING    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          CHANGING l_bprof.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.
  data: l_farea  like BPJA-FAREA.
  clear: l_farea.


  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
    EXPORTING
      i_objnr         = l_objnr
      i_posit         = l_posit
      i_geber         = l_geber
      i_gjahr         = l_gjahr
      i_farea         = l_farea
    IMPORTING
      e_profil        = l_bprof
    EXCEPTIONS
      no_profil_found = 01.

  IF NOT sy-subrc IS INITIAL.
*   Profile from FundMgt Area
    CALL FUNCTION 'FM5B_GET_PROFILE'
      EXPORTING
        i_fikrs           = l_fikrs
        i_fincode         = l_geber
      IMPORTING
        e_profil          = l_bprof
      EXCEPTIONS
        fm_area_not_found = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
FORM pick.
  CHECK g_func = 'X'.

  IF sy-cucol < 15.     " download
    PERFORM data_download.
  ELSEIF sy-cucol < 30. " test
    PERFORM release USING 'X'.
  ELSEIF sy-cucol < 45. " run
    PERFORM release USING ' '.
  ELSE.
    READ LINE 1 FIELD VALUE p_r.
    READ LINE 1 FIELD VALUE p_d.
    sy-lsind = sy-lsind - 1.
    PERFORM display_data.

  ENDIF.

  IF it_message[] IS NOT INITIAL.


    CALL FUNCTION 'ZGFI_DISP_ALV_POPUP'
      EXPORTING
        i_grid_title     = 'FM Budgeting Result'
        i_structure_name = 'ZDSFI_FM_ERR_01'
      TABLES
        t_alvtab         = it_message.

    REFRESH it_message.
    CLEAR it_message.

  ENDIF.
* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
ENDFORM.                    " pick
*&---------------------------------------------------------------------*
*&      Form  release
*&---------------------------------------------------------------------*
FORM release USING    value(l_test).
* Upload
  IF p_act = 'U'.
    PERFORM fill_budget.
    IF l_test = 'X'.
      WRITE:/ 'Release Simulation'.
      LOOP AT rec.
        WRITE:/ 'Log...', rec-fistl, rec-fipos, rec-geber, rec-wert.
      ENDLOOP.
    ELSE.
      PERFORM release_budget.
    ENDIF.

  ELSEIF p_act = 'R'.
* Release
    READ LINE 1 FIELD VALUE g_per.
    IF g_per BETWEEN 1 AND 12.
      PERFORM fill_budget.

      IF l_test = 'X'.
        WRITE:/ 'Release Simulation'.
        LOOP AT rec.
          WRITE:/ 'Log...', rec-fistl, rec-fipos, rec-geber, rec-wert.
        ENDLOOP.
      ELSE.
        PERFORM release_budget.
*    sy-lsind = sy-lsind - 1.
      ENDIF.
*   perform release_budget.
    ELSE.
      WRITE:/ 'Input period correctly!!!'.
    ENDIF.
  ENDIF.
  sy-lsind = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.
  WRITE:/ 'Period:', g_per INPUT ON.
  WRITE: ' (Round:', p_r NO-GAP INPUT ON,
         ', Decimal:', p_d NO-GAP INPUT ON, ')'.
  CASE p_act.
    WHEN 'U'.
      WRITE: ' Uploading...'     COLOR COL_GROUP.
      IF p_rev = 'X'.
        WRITE: '(REVERSE)'     COLOR COL_GROUP.
      ENDIF.
    WHEN 'R'. WRITE: ' Releasing...'     COLOR COL_GROUP.
    WHEN 'B'. WRITE: ' Released Budget'  COLOR COL_GROUP.
  ENDCASE.

  ULINE.
  g_func = 'X'.
  WRITE: icon_execute_object AS ICON HOTSPOT, 'Download(S_F1) ',
         icon_execute_object AS ICON HOTSPOT, 'Test(S_F4)     ',
         icon_execute_object AS ICON HOTSPOT, 'Run(S_F5)      ',
         icon_refresh        AS ICON HOTSPOT, 'Refresh        '.

  HIDE: g_func.
  CLEAR: g_func.
  ULINE.

  FORMAT COLOR COL_HEADING.
  WRITE:/ 'FundCtr Commitment Item            Fund       P'.
  WRITE:  '     Carry   Jan       Feb       Mar       Apr  ' NO-GAP.
  WRITE:  '     May       Jun       Juy       Aug  ' NO-GAP.
  WRITE:  '     Sep       Oct       Nov       Dec  '.
  ULINE.
  FORMAT COLOR COL_NORMAL.

ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_DUAL
*&---------------------------------------------------------------------*
*       Look for the filepath of Uploading Source file
*----------------------------------------------------------------------*
FORM get_filename_dual  CHANGING fp_file.

  DATA: lt_files TYPE filetable,
        l_file   TYPE file_table,
        l_title  TYPE string,
        l_subrc  TYPE sysubrc.

  DATA  l_init_dir TYPE string.

  l_title = 'Choose Upload file'.

  CALL METHOD cl_gui_frontend_services=>directory_get_current
    CHANGING
      current_directory = l_init_dir.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = l_title
      initial_directory       = l_init_dir
*      default_extension       = 'EXCEL'
*      file_filter             = cl_gui_frontend_services=>filetype_excel
       file_filter            = '*.XLS,*.TXT'
    CHANGING
      file_table              = lt_files
      rc                      = l_subrc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.
  CHECK sy-subrc = 0.
  LOOP AT lt_files INTO l_file.
    fp_file = l_file.
    EXIT.
  ENDLOOP.

ENDFORM.                    " GET_FILENAME_DUAL
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION_N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1731   text
*----------------------------------------------------------------------*
FORM bdc_transaction_n USING tcode.

  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: ls_message TYPE ZDSFI_FM_ERR_01.

* batch input session
  IF session = 'X'.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode     = tcode
      TABLES
        dynprotab = bdcdata.
    IF smalllog <> 'X'.
      WRITE: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH messtab.
    CALL TRANSACTION tcode USING bdcdata
                     MODE   ctumode
                     UPDATE cupdate
                     MESSAGES INTO messtab.
    l_subrc = sy-subrc.

    READ TABLE messtab WITH KEY msgtyp = 'S'.
    IF sy-subrc = 0.
      ls_message-icon  = '@5B@'.
      ls_message-perio = rec-perio.
      ls_message-fistl = rec-fistl.
      ls_message-docnr = messtab-MSGV1.
      ls_message-errtx = 'Successfully Posted'.
      APPEND ls_message TO it_message.
    ELSE.
      LOOP AT messtab.
         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = messtab-MSGID
            MSGNR               = messtab-MSGNR
            MSGV1               = messtab-MSGV1
            MSGV2               = messtab-MSGV2
            MSGV3               = messtab-MSGV3
            MSGV4               = messtab-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = l_mstring.

        ls_message-icon  = '@5C@'.
        ls_message-perio = rec-perio.
        ls_message-fistl = rec-fistl.
        ls_message-errtx = l_mstring.
        APPEND ls_message TO it_message.
      ENDLOOP.
    ENDIF.
*********************************************************
** Erzeugen fehlermappe ************************************************
    IF l_subrc <> 0 AND group <> space.
      IF e_group_opened = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
          EXPORTING
            client   = sy-mandt
            group    = group
            user     = user
            keep     = keep
            holddate = holddate.
        e_group_opened = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode     = tcode
        TABLES
          dynprotab = bdcdata.
    ENDIF.
  ENDIF.
  REFRESH bdcdata.

ENDFORM.                    " BDC_TRANSACTION_N
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LAYOUT .
***->>>>Assign Input structure for display
  PERFORM field_assign_for_ole_int TABLES gt_ole.
**->>>>Show Object table's structure
  PERFORM show_excel_with_structure.

ENDFORM.                    " GET_LAYOUT


FORM FIELD_ASSIGN_FOR_OLE_INT  TABLES   ft_TAB   TYPE T_OLETAB.

  DATA ls_TAB TYPE T_OLE.

  DEFINE input_field.
    ls_tab-fieldname = &2.
    ls_tab-ddtext = &1.
    APPEND ls_TAB TO ft_tab.
  END-OF-DEFINITION.

  input_field 'FICTR'   'Fund Center'.
  input_field 'FIPOS'   'Commitment Item'.
  input_field 'BEZEICH' 'Description'.
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

ENDFORM.
