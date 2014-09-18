*
* CO Inventory Check Report
* by Andy Choi.
*  - 2005.01.20
*
REPORT zmmflow
               MESSAGE-ID zmfi
               LINE-SIZE 200
               LINE-COUNT 65
               NO STANDARD PAGE HEADING.

INCLUDE <icon>.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES: t001, skb1, t001w, t001k, t169f, t158, mbew,
        bkpf, bsim, t030, rseg,
        bseg.

DATA: BEGIN OF it_bkpf OCCURS 0,
        bukrs  LIKE bkpf-bukrs,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        blart  LIKE bkpf-blart,
        cpudt  LIKE bkpf-cpudt,
        cputm  LIKE bkpf-cputm,
        budat  LIKE bkpf-budat,
        aedat  LIKE bkpf-aedat,
        waers  LIKE bkpf-waers,
        bstat  LIKE bkpf-bstat,
        stblg  LIKE bkpf-stblg,
        stjah  LIKE bkpf-stjah,
        awtyp  LIKE bkpf-awtyp,
        awkey  LIKE bkpf-awkey,
        tcode  LIKE bkpf-tcode, "Loan..reverse:FNM3
     END OF it_bkpf.

DATA: BEGIN OF it_bsim OCCURS 0,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
      END OF it_bsim.

DATA: BEGIN OF it_rseg OCCURS 0,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
      END OF it_rseg.

DATA: BEGIN OF it_bseg OCCURS 0,
*        buzei  LIKE bseg-buzei,

        shkzg  LIKE bseg-shkzg,
        xnegp  LIKE bseg-xnegp, "negative posting
        hkont  LIKE bseg-hkont,
        wrbtr  LIKE bseg-wrbtr,
        dmbtr  LIKE bseg-dmbtr,
        menge  LIKE bseg-menge,

        zuonr  LIKE bseg-zuonr,
        matnr  LIKE bseg-matnr,
        werks  LIKE bseg-werks,
        sgtxt  LIKE bseg-sgtxt,
      END   OF it_bseg.

DATA: BEGIN OF i_line OCCURS 0,
        hkont  LIKE bseg-hkont,
        blart  LIKE bkpf-blart,
        matnr  LIKE bseg-matnr,
        bklas  LIKE mbew-bklas,
        maktx  LIKE makt-maktx,

        mvnt(2) type c,   "movement type

        shkzg  LIKE bseg-shkzg,
        xnegp  LIKE bseg-xnegp,
        wrbtr  LIKE bseg-wrbtr,
        dmbtr  LIKE bseg-dmbtr,
        menge  LIKE bseg-menge,

        tcode  LIKE bkpf-tcode,
        cpudt  LIKE bkpf-cpudt,
        budat  LIKE bkpf-budat,
*       BUKRS  LIKE BSEG-BUKRS,
        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
*       buzei  LIKE bseg-buzei,

        zuonr  LIKE bseg-zuonr,
        sgtxt  LIKE bseg-sgtxt,
        waers  LIKE bkpf-waers,
      END OF i_line.

*master data
*DATA: BEGIN OF i_mara OCCURS 0,
*        matnr   LIKE mara-matnr,       "Meterial no
*        maktx   LIKE makt-maktx,       "Meterial Desc.
*
*      END OF i_mara.

DATA: BEGIN OF i_mlcd OCCURS 0,
        bklas   LIKE t030-bklas,       "?????
        bwkey   LIKE ckmlhd-bwkey,     "Plant
        matnr   LIKE mara-matnr,       "Meterial no
        kalnr   LIKE ckmlhd-kalnr,     "Cost Estimate No
        mlast   LIKE ckmlhd-mlast,     "tran/single/multi
        maktx   LIKE makt-maktx,       "Meterial Desc.
        peinh   LIKE mbew-peinh,       "price unit 1/10
      END OF i_mlcd.

RANGES: r_mara  FOR mara-matnr,
        r_blart FOR bkpf-blart,
        r_bwkey FOR bsim-bwkey,
        r_bsx   for t030-konts,
        r_gbb   for t030-konts,
        r_wrx   for t030-konts,
        r_fr1   for t030-konts.


*..For Display
CONSTANTS: v VALUE '|',
           l_yea TYPE i VALUE 04,
           l_ste TYPE i VALUE 08,
           l_bel TYPE i VALUE 10,
           l_tra TYPE i VALUE 13,
           l_amt TYPE i VALUE 16,
           l_txt TYPE i VALUE 16.

DATA: BEGIN OF pftab  OCCURS 0,
      fcode LIKE  rsmpe-func,
      END   OF pftab.

* USING ALV REPORTING..
TYPE-POOLS : slis.

INCLUDE rvreuse_global_data.
INCLUDE rvreuse_local_data.
INCLUDE rvreuse_forms.

DATA : gs_layout    TYPE slis_layout_alv,
       gt_fieldcat  TYPE slis_t_fieldcat_alv,
       gt_field     TYPE slis_t_fieldcat_alv,
       g_fieldcat_s TYPE slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    TYPE slis_t_event,
       it_sort      TYPE slis_t_sortinfo_alv,
       g_save(1)    TYPE c,
       g_exit(1)    TYPE c,
       gx_variant   LIKE disvariant,
       g_variant    LIKE disvariant,
       g_repid      LIKE sy-repid,
       g_cnt(2)     TYPE n,
       ls_sort      TYPE slis_sortinfo_alv,
       gt_sort      TYPE slis_t_sortinfo_alv.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.


DATA: g_exit_caused_by_caller,
      gt_list_top_of_page TYPE slis_t_listheader,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
      gs_exit_caused_by_user TYPE slis_exit_by_user,
      g_tabname TYPE slis_tabname VALUE 'ITAB',
      g_boxname TYPE slis_fieldname VALUE 'BOX'.

DATA:

      gt_sp_group TYPE slis_t_sp_group_alv,

      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.

DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c,
      wa_mode.

************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs LIKE t001-bukrs MEMORY ID buk OBLIGATORY.
SELECT-OPTIONS:
                s_matnr FOR bseg-matnr.

PARAMETER: p_bsx  AS CHECKBOX DEFAULT 'X',
           p_wrx  AS CHECKBOX DEFAULT ' ',
           p_fr1  AS CHECKBOX DEFAULT ' ',
           p_gbb  AS CHECKBOX DEFAULT ' ',
           p_all  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl1.
PARAMETER: p_BSIM AS CHECKBOX DEFAULT 'X',  "MM doc.only
           p_mlex as checkbox default 'X',  "ML exclude
           p_summ AS CHECKBOX DEFAULT 'X'.  "Summary


SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_gjahr for bkpf-gjahr,
                s_MONAT for bkpf-MONAT,
                s_budat FOR bkpf-budat,
                s_cpudt FOR bkpf-cpudt,
                r_hkont FOR skb1-saknr,
                s_bklas FOR mbew-bklas,   "valuation class
                s_bwkey FOR t001w-werks,  "Plant
                s_tcode FOR bkpf-tcode.

SELECT-OPTIONS: s_blart  FOR bkpf-blart.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b3.

************************************************************************
* MACRO
************************************************************************
DEFINE append_fld.
  &1 = &1 + 1.
  clear ls_fieldcat.
  ls_fieldcat-key           = ' '.
  ls_fieldcat-col_pos       =  &1.
  ls_fieldcat-fieldname     =  &2.
  ls_fieldcat-ref_fieldname =  &3.
  ls_fieldcat-ref_tabname   =  &4.
  append ls_fieldcat to  rt_fieldcat.
END-OF-DEFINITION.

DEFINE append_key.
  &1 = &1 + 1.
  clear ls_fieldcat.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-col_pos       =  &1.
  ls_fieldcat-fieldname     =  &2.
  ls_fieldcat-ref_fieldname =  &3.
  ls_fieldcat-ref_tabname   =  &4.
  append ls_fieldcat to  rt_fieldcat.
END-OF-DEFINITION.

*----------------------------------
* Initialization fieldcatalog
INITIALIZATION.
  g_repid = sy-repid.
*  PERFORM fieldcat_init USING gt_fieldcat[].

  wa_repid = sy-repid.
* ==> Change Variant saving type
*                         U-???, X-??(??), A-??, ' '-????
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
* wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.



************************************************************************
*     START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CLEAR: pftab[], pftab.

  PERFORM read_master_data.

  PERFORM get_bkpf.

  LOOP AT it_bkpf.

    if p_mlex = 'X'
    and ( it_bkpf-blart = 'ML' or it_bkpf-blart = 'PR' ).
*    tcode = 'CKML' or it_bkpf-awtyp = 'PRCHG' ).
      continue.
    endif.

    perform get_bseg.

    CHECK sy-subrc = 0.
    LOOP AT it_bseg.
      clear i_line.

      case it_bkpf-blart.
        when 'WE' or 'WF' or 'WG'.
          i_line-mvnt = 'GR'.
        when 'WA' or 'WB'.
          i_line-mvnt = 'GI'.
        when 'WC'.
          if it_bkpf-tcode = 'MIGO'.
            i_line-mvnt = 'GR'.
          else.
            i_line-mvnt = 'GI'.
          endif.
        when 'WI'.
          i_line-mvnt = 'AJ'.
        when 'WL'.
          i_line-mvnt = 'SL'.
        when others.
      endcase.

      if p_bsx = 'X'.
        check it_bseg-hkont in r_bsx.
      endif.
      if p_wrx = 'X'.
        check it_bseg-hkont in r_wrx.
      endif.
      if p_fr1 = 'X'.
        check it_bseg-hkont in r_fr1.
      endif.
      if p_gbb = 'X'.
        check it_bseg-hkont in r_gbb.
      endif.

*      CHECK it_bseg-matnr IN s_matnr.
*      CHECK it_bseg-werks IN s_bwkey.

      MOVE-CORRESPONDING it_bkpf TO i_line.
      MOVE-CORRESPONDING it_bseg TO i_line.


* ML transaction
      if i_line-tcode = 'CKML'.
        clear i_line-menge.
      endif.

      IF it_bseg-shkzg = 'H'.
        i_line-wrbtr = -1 * i_line-wrbtr.
        i_line-dmbtr = -1 * i_line-dmbtr.
        i_line-menge = -1 * i_line-menge.
*        if it_bseg-XNEGP = space.
*          i_line-shkzg = 'C'.
*        else.
*          i_line-shkzg = 'D'.   "negative
*        endif.
*      else.
*        if it_bseg-XNEGP = space.
*          i_line-shkzg = 'D'.
*        else.
*          i_line-shkzg = 'C'.   "negative
*        endif.
      ENDIF.

* reverse, original doc
*     if it_bkpf-stblg <> space and it_bseg-XNEGP = space.
*       i_line-XNEGP = 'O'.
*     endif.

      READ TABLE i_mlcd WITH KEY matnr = it_bseg-matnr.
      CHECK sy-subrc = 0.

      i_line-maktx = i_mlcd-maktx.
      i_line-bklas = i_mlcd-bklas.

      IF p_summ = 'X'.
        CLEAR: " i_line-matnr, i_line-maktx,
               i_line-belnr, i_line-sgtxt,
               i_line-budat, i_line-cpudt,
               i_line-xnegp, i_line-shkzg, i_line-zuonr.
        COLLECT i_line.
      ELSE.
        APPEND i_line.
      ENDIF.
      CLEAR  i_line.
    ENDLOOP.

  ENDLOOP.

  SORT i_line BY hkont blart bklas matnr.
************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.

* ALV HEADER & FIELD SETTING
  PERFORM : alvprn_basic01,
            alvprn_field01 USING 'I_LINE'.

*PERFORM   sort_build.

** ==> 2. set variant default
*  PERFORM set_variant CHANGING wa_var.
** ==> 3. set layout for alv style
*  PERFORM set_layout CHANGING gs_layout.
** ==> 4. set events for alv
*  PERFORM set_events CHANGING gt_events.
* ==> 7. call function display alv.
* PERFORM display_alv.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'PF_STATUS_SET'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            it_sort                  = gt_sort[]
            it_events                = gt_events[]
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = i_line.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.
  DATA l_field(20).

  CASE p_ucomm.
    WHEN '&IC1'.
      GET CURSOR FIELD l_field.
      CHECK l_field EQ 'I_LINE-BELNR'.
      READ TABLE i_line INDEX p_selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID:'BLN' FIELD i_line-belnr,
                         'BUK' FIELD p_bukrs,
                         'GJR' FIELD i_line-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.
************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'MENU'.
ENDFORM.


*----------------------------------------------------------------------
*    Forms
*----------------------------------------------------------------------
*
** Initialization fieldcatalog
*
*&---------------------------------------------------------------------*
*&      Form  read_master_data
*&---------------------------------------------------------------------*
FORM read_master_data.
  SELECT SINGLE * INTO t001 FROM t001
                WHERE bukrs EQ p_bukrs.

  SELECT SINGLE * INTO t001 FROM t001
                WHERE bukrs EQ p_bukrs.

  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.
  SELECT * FROM t001k
    WHERE bukrs = p_bukrs.
    r_bwkey-low = t001k-bwkey.
    APPEND r_bwkey.
  ENDSELECT.


  SELECT DISTINCT mbew~bklas mbew~matnr mbew~bwkey mbew~peinh
                  ckmlhd~kalnr ckmlhd~mlast
           INTO CORRESPONDING FIELDS OF TABLE i_mlcd
           FROM ( mbew INNER JOIN ckmlhd
                       ON  mbew~matnr = ckmlhd~matnr
                       AND mbew~bwkey = ckmlhd~bwkey )
           WHERE mbew~matnr IN s_matnr
             AND mbew~bklas IN s_bklas
           ORDER BY mbew~bwkey mbew~matnr.

  LOOP AT i_mlcd.
    SELECT SINGLE maktx FROM makt
        INTO i_mlcd-maktx
        WHERE matnr = i_mlcd-matnr
          AND spras = sy-langu.
    MODIFY i_mlcd.
  ENDLOOP.

  R_bsx-SIGN = 'I'.  R_bsx-OPTION = 'EQ'.
  R_wrx-SIGN = 'I'.  R_wrx-OPTION = 'EQ'.
  R_fr1-SIGN = 'I'.  R_fr1-OPTION = 'EQ'.
  R_gbb-SIGN = 'I'.  R_gbb-OPTION = 'EQ'.

  select * from t030
    where ktopl = t001-ktopl.

    case t030-ktosl.
      when 'BSX'.
        R_bsx-low = t030-konts. append r_bsx.
      when 'FR1' or 'FR2' or 'FR3' or 'ZR3' or 'ZR4'.
        R_fr1-low = t030-konts. append r_fr1.
      when 'GBB'.
        R_gbb-low = t030-konts. append r_gbb.
      when 'WRX'.
        R_wrx-low = t030-konts. append r_wrx.
      when others.
    endcase.
  endselect.

* select I/V, GR document types
  if p_bsim = 'X'.

    r_blart-sign   = 'I'.
    r_blart-option = 'EQ'.
    SELECT * FROM t158.

      if p_mlex = 'X' and ( t158-blart = 'ML' or t158-blart = 'PR' ).
        continue.
      endif.

      READ TABLE r_blart WITH KEY low = t158-blart.
      IF sy-subrc <> 0.
        r_blart-low = t158-blart.  APPEND r_blart.
      ENDIF.

      READ TABLE r_blart WITH KEY low = t158-blaum.
      IF sy-subrc <> 0.
        r_blart-low = t158-blaum.  APPEND r_blart.
      ENDIF.
    ENDSELECT.

* ML, IV, PR
    SELECT * FROM t169f.
      if p_mlex = 'X' and ( t158-blart = 'ML' or t158-blart = 'PR' ).
        continue.
      endif.

      READ TABLE r_blart WITH KEY low = t169f-blart.
      IF sy-subrc <> 0.
        r_blart-low = t169f-blart. APPEND r_blart.
      ENDIF.
    ENDSELECT.


  endif.

** for select...
*  REFRESH R_mara.
*  R_mara-SIGN = 'I'.
*  R_mara-OPTION = 'EQ'.
*  LOOP AT I_MLCD.
*    R_mara-LOW = i_mlcd-matnr.  APPEND R_mara.
*  ENDLOOP.

ENDFORM.                    " read_master_data
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
FORM alvprn_basic01.
  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.
  gs_layout-f2code           = '&IC1'.
*jhs
  gs_layout-numc_sum               = 'X'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  DELETE gt_events WHERE name NE 'END_OF_PAGE'
                     AND name NE 'TOP_OF_PAGE'
                     AND name NE 'TOP_OF_LIST'
                     AND name NE 'END_OF_LIST'.
  LOOP AT gt_events ASSIGNING <ls_event>.
    CONCATENATE 'ALV_EVENT_'
                <ls_event>-name
                INTO <ls_event>-form.
  ENDLOOP.
ENDFORM.                    " alvprn_basic01
*&---------------------------------------------------------------------*
*&      Form  alvprn_field01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0641   text
*----------------------------------------------------------------------*
FORM alvprn_field01 USING  p_intab.

  DATA: l_cu LIKE fdes-dispw.

  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  l_cu = t001-waers.

  PERFORM field_setting TABLES gt_fieldcat USING :
   'HKONT'  'Account'    '07' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'MATNR'  'Material'   '18' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
   'MAKTX'  'Mat.Desc'   '25' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
*  'DMBTR'  'Amount'     '15' ' ' 'R'  ' '  L_CU ' ',
   'MVNT'   'MV'         '02' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'BLART'  'DT'         '02' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'TCODE'  'TCDE'       '04' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
   'BKLAS'  'VCls'       '04' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
   'MENGE'  'Qty'        '14' ' ' 'R'  ' ' '0' '  ' ' ' 'X',
   'DMBTR'  'Amount'     '18' ' ' 'R'  ' ' ' ' ' '  ' ' 'X',
   'GJAHR'  'Year'       '04' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'BUDAT'  'PstDte'     '10' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'BELNR'  'Document'   '10' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'SHKZG'  'S/H'        '02' ' ' 'C'  ' ' ' ' '  ' ' ' ' ',
   'XNEGP'  'X'          '01' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
   'SGTXT'  'line text'  '15' ' ' 'L'  ' ' ' ' '  ' ' ' ' ',
   'CPUDT'  'CPUDt'      '10' ' ' 'R'  ' ' ' ' '  ' ' ' ' ',
   'WAERS'  'Curr'       '04' ' ' 'L'  ' ' ' ' '  ' ' ' ' '.


ENDFORM.                    " alvprn_field01
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-round      = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

*  DATA: l_col(40).
*
*  IF p_gub = 'S'.
*    CLEAR g_fieldcat_s.
*    READ TABLE gt_field INTO g_fieldcat_s
*                        WITH KEY fieldname  = p_fname.
*    EXIT.
*  ENDIF.
*
*  FIELD-SYMBOLS <fs>.
*  CONCATENATE 'G_FIELDCAT_S-' p_fname  INTO l_col.
*  ASSIGN      (l_col)         TO       <fs>.
*  MOVE         p_con          TO       <fs>.
*
*
** DATA  APPEND
*  CHECK  p_gub = 'E'.
*
*  g_cnt = g_cnt + 1.
*  g_fieldcat_s-col_pos = g_cnt.
*
**  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
**                         = g_fieldcat_s-seltext_m.
*  APPEND g_fieldcat_s TO p_fieldcat_t.
ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
FORM sort_build.
  CLEAR: gt_sort[], gt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'BKLAS'.  "valuation class
  ls_sort-spos = 1.    "---> KEY
  ls_sort-up = 'X'.
  APPEND ls_sort TO gt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'MATNR'.
  ls_sort-spos = 2.
  ls_sort-up = 'X'.
  ls_sort-subtot = 'X'.
* ls_sort-group = '*'.
  APPEND ls_sort TO gt_sort.
ENDFORM.                    " sort_build
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space. "'X'.
  "?????
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '. "LIGHT'.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.


ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  set_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VAR  text
*----------------------------------------------------------------------*
FORM set_variant CHANGING cs_vari TYPE disvariant.

  CHECK p_layout NE space.

  cs_vari-report      = sy-repid.
  cs_vari-handle      = space.
  cs_vari-log_group   = space.
  cs_vari-username    = space.
  cs_vari-variant     = p_layout.
  cs_vari-text        = space.
  cs_vari-dependvars  = space.

ENDFORM.                    " set_variant
*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = ct_events
       EXCEPTIONS
            list_type_wrong = 1
            OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " set_events
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.
  CALL FUNCTION wa_alv_function_name
    EXPORTING
         i_callback_program      = wa_repid
         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout               = gs_layout
         it_fieldcat             = gt_fieldcat[]
         it_special_groups       = gt_sp_group[]
         it_sort                 = gt_sorts[]
*         IT_FILTER               =
         i_default               = wa_default
         i_save                  = wa_var_save
         is_variant              = wa_var
         it_events               = gt_events[]
         is_print                = gs_prnt
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         t_outtab                =   i_line.
ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  get_bkpf
*&---------------------------------------------------------------------*
FORM get_bkpf.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_bkpf
           FROM bkpf
           WHERE bukrs EQ p_bukrs
           AND   GJAHR in s_gjahr
           AND   MONAT in s_MONAT
           AND   cpudt IN s_cpudt
           AND   budat IN s_budat
           AND   blart IN r_blart
           AND   blart IN s_blart
           AND   tcode IN s_tcode.
*           AND   bstat NE 'V'. -> In ( range )

ENDFORM.                    " get_bkpf
*&---------------------------------------------------------------------*
*&      Form  get_bseg
*&---------------------------------------------------------------------*
FORM get_bseg.
  refresh: it_rseg, it_bsim.

  if it_bkpf-awtyp = 'RMRP'. "Invoice
    SELECT * from rseg
               WHERE gjahr EQ it_bkpf-awkey+10(4)
                 AND belnr EQ it_bkpf-awkey(10)
                 AND matnr IN s_matnr
                 AND werks IN s_bwkey.
      move-corresponding rseg to it_rseg..
      collect it_rseg.
    endselect.
    loop at it_rseg.
      SELECT hkont shkzg wrbtr dmbtr zuonr
             matnr werks sgtxt menge xnegp
                     INTO CORRESPONDING FIELDS OF TABLE it_bseg
                     FROM bseg
                     WHERE bukrs EQ it_bkpf-bukrs
                       AND gjahr EQ it_bkpf-gjahr
                       AND belnr EQ it_bkpf-belnr
                       AND hkont IN r_hkont
                       AND matnr IN s_matnr
                       AND werks IN s_bwkey.

    endloop.

  else.
    if p_bsim = 'X'.
      select * from   bsim
           WHERE gjahr EQ it_bkpf-gjahr
             AND belnr EQ it_bkpf-belnr
             AND matnr IN s_matnr
             AND bwkey IN s_bwkey.
        move-corresponding bsim to it_bsim.
        collect it_bsim.
      endselect.

      loop at it_bsim.
        SELECT hkont shkzg wrbtr dmbtr zuonr
               matnr werks sgtxt menge xnegp
                       INTO CORRESPONDING FIELDS OF TABLE it_bseg
                       FROM bseg
                       WHERE bukrs EQ it_bkpf-bukrs
                         AND gjahr EQ it_bkpf-gjahr
                         AND belnr EQ it_bkpf-belnr
                         AND hkont IN r_hkont
                         AND matnr IN s_matnr
                         AND werks IN s_bwkey.

      endloop.

    else.
      SELECT hkont shkzg wrbtr dmbtr zuonr
             matnr werks sgtxt menge xnegp
                 INTO CORRESPONDING FIELDS OF TABLE it_bseg
                 FROM bseg
                 WHERE bukrs EQ it_bkpf-bukrs
                   AND gjahr EQ it_bkpf-gjahr
                   AND belnr EQ it_bkpf-belnr
                   AND hkont IN r_hkont
                   AND matnr IN s_matnr
                   AND werks IN s_bwkey.
    endif.
  endif.
ENDFORM.                    " get_bseg
