REPORT ZHR_PAYROLL_DUMP_REPORT line-size 255 .
**********************************************************************
*Date        Developer    Request        Description
*05/25/2007  Manju        UD1K940671     Initial coding
***********************************************************************

TABLES : P0001,
         PA0001,
         PA0000,
         P0000,
         PA0041,
         PA0169,
         PC261,
         T511P.

data : c_red(4)   value 'C610'.
type-pools: slis.
DATA: IN_RGDIR LIKE PC261 OCCURS 0 WITH HEADER LINE,
      WA_RT LIKE PC207 OCCURS 0 WITH HEADER LINE,
      SEQNR LIKE PC261-SEQNR,
      ws_pernr type P0000-PERNR,
      RESULT TYPE PAY99_RESULT.

data: w_permo  like t549a-permo,   " Period parameters
      w_abkrt  like t549t-atext,   " Payroll Area Text
        g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user.

data : begin of it_output occurs 0,
        pernr like pa0001-pernr,
        FPPER like PC261-FPPER,
        FPBEG like pc261-FPBEG,
        FPEND like pc261-FPEND,
        SRTZA  like pc261-SRTZA,
        402  like PC207-betrg,
        403  like PC207-betrg,
        404  like PC207-betrg,
        405  like PA0169-EEPCT,
        406  like PC207-betrg,
        407  like PC207-betrg,
        430  like PC207-betrg,
        441  like PC207-betrg,
        448  like PC207-betrg,
        9A01  like PC207-betrg,
        9A10  like PC207-betrg,
        9F01  like PC207-betrg,
        9F10  like PC207-betrg,
        8337  like PC207-betrg,
        8387 like PC207-betrg,
        linecolor(4),
       end of it_output.

data : begin of it_calc occurs 0,
        pernr like pa0001-pernr,
        FPPER like PC261-FPPER,
        FPBEG like pc261-FPBEG,
        FPEND like pc261-FPEND,
        SRTZA  like pc261-SRTZA,
        402  like PC207-betrg,
        403  like PC207-betrg,
        404  like PC207-betrg,
        405  like PA0169-EEPCT,
        406  like PC207-betrg,
        407  like PC207-betrg,
        430  like PC207-betrg,
        441  like PC207-betrg,
        448  like PC207-betrg,
        9A01  like PC207-betrg,
        9A10  like PC207-betrg,
        9F01  like PC207-betrg,
        9F10  like PC207-betrg,
        8337  like PC207-betrg,
        8387 like PC207-betrg,
        linecolor(4),
       end of it_calc.

data : begin of it_diff occurs 0,
        pernr like pa0001-pernr,
        FPPER like PC261-FPPER,
        FPBEG like pc261-FPBEG,
        FPEND like pc261-FPEND,
        SRTZA  like pc261-SRTZA,
        402  like PC207-betrg,
        403  like PC207-betrg,
        404  like PC207-betrg,
        405  like PA0169-EEPCT,
        406  like PC207-betrg,
        407  like PC207-betrg,
        430  like PC207-betrg,
        441  like PC207-betrg,
        448  like PC207-betrg,
        9A01  like PC207-betrg,
        9A10  like PC207-betrg,
        9F01  like PC207-betrg,
        9F10  like PC207-betrg,
        8337  like PC207-betrg,
        8387 like PC207-betrg,
        linecolor(4),
       end of it_diff.

data : ws_rgdir like line of in_rgdir,
       L_relid like  PCL2-RELID,
       l_pernr like PC200-PERNR,
       ll_pernr like P0000-PERNR.
.

data : begin of it_tab occurs 0,
        pernr like P0000-pernr,
        BEGDA like p0000-BEGDA,     " UD1K940413
       end of it_tab.


data: gt_fieldcat  type slis_t_fieldcat_alv with header line,
      gt_fieldcat1 type slis_t_fieldcat_alv with header line,
      gt_fieldcat2 type slis_t_fieldcat_alv with header line,
      gt_fc       type slis_t_fieldcat_alv,
      gt_fc1       type slis_t_fieldcat_alv,
      gt_fc2       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv with header line,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event with header line,
      gt_header   type slis_t_listheader,
      g_repid     like sy-repid,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.


* Select_options
selection-screen begin of block b1 with frame title text-001.
select-options : s_pernr for P0000-PERNR,
                 s_WERKS for Pa0001-WERKS,
                 s_BTRTL for Pa0001-BTRTL,
                 s_PERSG for Pa0001-PERSG,
                 s_persk for PA0001-PERSK,
                 s_STAT2 for PA0000-STAT2.
selection-screen end of block b1  .
selection-screen begin of block b4 with frame title text-004.
Parameters :   p_ABKRS like  Pa0001-ABKRS obligatory default '11'.
select-options : s_BEGDA   for P0001-BEGDA,
                 s_FPPER for    PC261-FPPER.
selection-screen end of block b4.

parameters: p_abrpr  like t549q-pabrp modif id p1 no-display,
            p_abrjr  like t549q-pabrj modif id p1 no-display.
select-options : s_ABKRS for  Pa0001-ABKRS default '11' no-display.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(40) TEXT-009
                 FOR FIELD PARM.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS PARM LIKE SAPLANE-PLANETYPE no-display.
SELECTION-SCREEN END OF LINE.


initialization.
* Get Current Payroll Period
  perform get_payroll_period using p_ABKRS
                          changing w_permo s_BEGDA-low s_BEGDA-high
                                   w_abkrt p_abrpr p_abrjr.
  concatenate  p_abrjr p_abrpr into s_FPPER-low.
  append s_fpper.
  append s_BEGDA.


start-of-selection.

* Select All Employee's who are active.
  Perform select_valid_employees.


* Select Payroll Cluster Data
  perform read_payroll_data.

* Calculate diff B/w A & P records
  perform calc_diff.

end-of-selection.

* Display Output
  perform display_output.


*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_payroll_data.
  data : flag(1) type c,
         prev_fpper type  PC261-FPPER,
         lv_molga type molga,
         lw_month  LIKE  t5a4a-dlydy.



  loop at it_tab.
    ll_pernr = it_tab-pernr .



* Read Payroll Control Records
    clear lv_molga.
    CALL FUNCTION 'CU_READ_RGDIR'
         EXPORTING
              persnr          = ll_pernr
         IMPORTING
              molga           = lv_molga
         TABLES
              in_rgdir        = in_rgdir
         EXCEPTIONS
              no_record_found = 1
              OTHERS          = 2.

    ws_pernr = ll_pernr.


* Delete payroll control records where payroll period is 000000
    DELETE in_rgdir where FPPER eq '000000'.

** Delete voided payroll data.
    DELETE in_rgdir WHERE voidr NE space.

* Check for Retro Period


    clear  result.

*** Store only active payroll results.
*    DELETE in_rgdir WHERE srtza NE 'A'. "Active
*** Delete payroll control records based on selection input
    if not s_BEGDA[] is initial.
      DELETE in_rgdir WHERE not paydt in s_BEGDA. "System Year
    endif.
*** Delete payroll control records based on selection input
    if  not  s_FPPER[]  is initial.
      DELETE in_rgdir WHERE not FPPER in s_FPPER. "Payroll Period
    endif.

* Cluster id for US
* Personnel Country Grouping
    clear L_relid.
    SELECT SINGLE relid INTO L_relid
                  FROM t500l
                  WHERE molga = lv_molga.
    if   L_relid is initial.
      l_relid = 'RU'.
    endif.



    LOOP AT in_rgdir INTO ws_rgdir.
      seqnr = ws_rgdir-seqnr.
      l_pernr = ws_pernr.
      it_output-pernr = it_tab-pernr.
      it_output-SRTZA = ws_rgdir-srtza.
      it_output-FPPER  = ws_rgdir-fpper.
      it_output-FPBEG =  ws_rgdir-fpbeg.
      it_output-FPEND =  ws_rgdir-fpend.
* Read Payroll cluster Data for each payroll control record
      CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
           EXPORTING
                clusterid                    = l_relid
                employeenumber               = L_pernr
                sequencenumber               = seqnr
                read_only_international      = 'X'
           CHANGING
                payroll_result               = result
           EXCEPTIONS
                illegal_isocode_or_clusterid = 1
                error_generating_import      = 2
                import_mismatch_error        = 3
                subpool_dir_full             = 4
                no_read_authority            = 5
                no_record_found              = 6
                versions_do_not_match        = 7
                error_reading_archive        = 8
                error_reading_relid          = 9
                OTHERS                       = 10.

      LOOP AT result-inter-rt INTO wa_rt.

        case wa_rt-lgart.

          when '/402'.
            it_output-402 = wa_rt-BETRG.
          when '/403'.
            it_output-403 = wa_rt-BETRG.
          when '/404' .
            it_output-404 = wa_rt-BETRG.
          when '/405' .
            it_output-405 = wa_rt-BETRG.
          when '/406'.
            it_output-406 = wa_rt-BETRG.
          when '/407'.
            it_output-407 = wa_rt-BETRG.
          when '/430'.
            it_output-430 = wa_rt-BETRG.
          when '/441'.
            it_output-441 = wa_rt-BETRG.
          when '/448'.
            it_output-448 = wa_rt-BETRG.
          when '9A01'.
            it_output-9A01 = wa_rt-BETRG.
          when '9A10'.
            it_output-9A10 = wa_rt-BETRG.
          when '9F01'.
            it_output-9F01 = wa_rt-BETRG.
          when '9F10'.
            it_output-9F10 = wa_rt-BETRG.
          when '8337'.
            it_output-8337 = it_output-8337 + wa_rt-BETRG.
            if wa_rt-APZNR eq '00' .

            elseif wa_rt-APZNR eq '01'.

            endif.
          when '8387'.
            it_output-8387 = it_output-8387 +  wa_rt-BETRG .
        endcase.

        at last.

        endat.


      ENDLOOP.
      Append it_output. clear it_output.


    endloop.

  endloop.
  sort it_output by pernr  FPPER.

ENDFORM.                    " read_payroll_data
*&---------------------------------------------------------------------*
*&      Form  select_valid_employees
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_valid_employees.

* 401K Plan enrolled Option is choosen

  select  a~pernr
          b~begda                                           "UD1K940413
           into table it_tab from Pa0001 as a inner join
                                         PA0000 as b on
                                       a~pernr = b~pernr
           where  a~pernr in s_pernr and
                  a~BEGDA <= sy-datum and
                  a~ENDDA >= sy-datum and
                  a~ABKRS eq  P_ABKRS and
                  a~WERKS in  s_werks and
                  a~BTRTL in  s_BTRTL  and
                  a~PERSG in  s_PERSG  and
                  a~persk in  s_persk and
                  b~BEGDA <= sy-datum and
                  b~ENDDA >= sy-datum and
                  b~STAT2 in s_STAT2.


ENDFORM.                    " select_valid_employees
*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output.

  g_repid = sy-repid.


  perform build_fieldcat  using  'IT_OUTPUT'.
  perform build_layout  using 'X' 'X' space.
  perform build_comment     using  gt_header[].
  perform display_grid.



ENDFORM.                    " display_output
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*----------------------------------------------------------------------*
FORM build_fieldcat  using p_intab type slis_tabname.

  data: gs_fieldcat like line of gt_fieldcat.
  clear   : gt_fieldcat, gt_fc.
  refresh : gt_fieldcat, gt_fc.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat[] = gt_fc[].





  loop at gt_fieldcat.
    case gt_fieldcat-fieldname.
      when 'PERNR'.
        gt_fieldcat-seltext_l = 'Team Member'.
        gt_fieldcat-seltext_M = 'Team Member'.
        gt_fieldcat-seltext_S = 'Team Member'.
        gt_fieldcat-key = 'X' .
        gt_fieldcat-KEY_SEL = 'X'.

      when 'FPPER'.
        gt_fieldcat-seltext_l = 'Payroll Period'.
        gt_fieldcat-seltext_M = 'Payroll Period'.
        gt_fieldcat-seltext_S = 'Payroll Period'.

      when 'FPBEG'.
        gt_fieldcat-seltext_l = 'Begin Date'.
        gt_fieldcat-seltext_M = 'Begin Date'.
        gt_fieldcat-seltext_S = 'Begin Date'.
        gt_fieldcat-REPTEXT_DDIC = 'Begin Date'.

      when 'FPEND'.
        gt_fieldcat-seltext_l = 'END Date'.
        gt_fieldcat-seltext_M = 'END Date'.
        gt_fieldcat-seltext_S = 'END Date'.
        gt_fieldcat-REPTEXT_DDIC = 'END Date'.


      when 'SRTZA'.
        gt_fieldcat-seltext_l = 'Status of Record'.
        gt_fieldcat-seltext_M = 'Status of Record'.
        gt_fieldcat-seltext_S = 'Status of Record'.

      when '402'.
        gt_fieldcat-seltext_l = '/402-TX Earned Income'.
        gt_fieldcat-seltext_M = '/402-TX Earned Income'.
        gt_fieldcat-seltext_S = '/402-TX Earned Income'.
        gt_fieldcat-REPTEXT_DDIC = '/402-TX Earned Income'.

      when '403' .
        gt_fieldcat-seltext_l = '/403-TX EE Social Security Tax'.
        gt_fieldcat-seltext_M = '/403-TX EE SS Tax'.
        gt_fieldcat-seltext_S = '/403-TX EE SS Tax'.
        gt_fieldcat-REPTEXT_DDIC = '/403-TX EE SS Tax'.

      when '404'.
        gt_fieldcat-seltext_l = '/404-TX ER Social Security Tax'.
        gt_fieldcat-seltext_M = '/404-TX ER SS Tax'.
        gt_fieldcat-seltext_S = '/404-TX ER SS Tax'.
        gt_fieldcat-REPTEXT_DDIC = '/404-TX ER SS Tax'.

      when '405'.
        gt_fieldcat-seltext_l = '/405-TX EE Medicare Tax'.
        gt_fieldcat-seltext_M = '/405-TX EE Medicare Tax'.
        gt_fieldcat-seltext_S = '/405-TX EE Medicare Tax'.
        gt_fieldcat-REPTEXT_DDIC = '/405-TX EE Medicare TAX'.

      when '406'.
        gt_fieldcat-seltext_l = '/406-TX ER Medicare Tax'.
        gt_fieldcat-seltext_M = '/406-TX ER Medicare Tax'.
        gt_fieldcat-seltext_S = '/406-TX ER Medicare Tax'.
        gt_fieldcat-REPTEXT_DDIC = '/406-TX ER Medicare Tax'.

      when '407'.
        gt_fieldcat-seltext_l = '/407-TX St. Unemployment Ins'.
        gt_fieldcat-seltext_M = '/407-TX St. Unemployment Ins'.
        gt_fieldcat-seltext_S = '/407-TX St. Unemployment Ins'.
        gt_fieldcat-REPTEXT_DDIC = '/407-TX St. Unemployment Ins'.


      when '430'.
        gt_fieldcat-seltext_l = '/430-TX ER Worker Compesatn Tx'.
        gt_fieldcat-seltext_M = '/430-TX ER Worker Compesatn Tx'.
        gt_fieldcat-seltext_S = '/430-TX ER Worker Compesatn Tx'.
        gt_fieldcat-REPTEXT_DDIC = '/430-TX ER Worker Compesatn Tx'.

      when '441'.
        gt_fieldcat-seltext_l = '/441-TX EE Disability Tax'.
        gt_fieldcat-seltext_M = '/441-TX EE Disability Tax'.
        gt_fieldcat-seltext_S = '/441-TX EE Disability Tax'.
        gt_fieldcat-REPTEXT_DDIC = '/441-TX EE Disability Tax'.

      when '448'.
        gt_fieldcat-seltext_l = '/448-TX EE Voluntary Disabilit'.
        gt_fieldcat-seltext_M = '/448-TX EE Voluntary Disabilit'.
        gt_fieldcat-seltext_S = '/448-TX EE Voluntary Disabilit'.
        gt_fieldcat-REPTEXT_DDIC = '/448-TX EE Voluntary Disabilit'.


      when '9A01'.
        gt_fieldcat-seltext_l = '9A01-TX Witholding-Alabama Sym'.
        gt_fieldcat-seltext_M = '9A01-TX Witholding-Alabama Sym'.
        gt_fieldcat-seltext_S = '9A01-TX Witholding-Alabama Sym'.
        gt_fieldcat-REPTEXT_DDIC = '9A01-TX Witholding-Alabama Sym.'.

      when '9A10'.
        gt_fieldcat-seltext_l = '9A10-TX ER Unemployment Tax-AL'.
        gt_fieldcat-seltext_M = '9A10-TX ER Unemployment Tax-AL'.
        gt_fieldcat-seltext_S = '9A10-TX ER Unemployment Tax-AL'.
        gt_fieldcat-REPTEXT_DDIC = '9A10-TX ER Unemployment Tax-AL'.

      when '9F01'.
        gt_fieldcat-seltext_l = '9F01-TX Witholding-Fed Sym'.
        gt_fieldcat-seltext_M = '9F01-TX Witholding-Fed Sym'.
        gt_fieldcat-seltext_S = '9F01-TX Witholding-Fed Sym'.
        gt_fieldcat-REPTEXT_DDIC = '9F01-TX Witholding-Fed Sym'.

      when '9F10'.
        gt_fieldcat-seltext_l = '9F10-TX ERUnemployment Tax-Fed'.
        gt_fieldcat-seltext_M = '9F10-TX ERUnemployment Tax-Fed'.
        gt_fieldcat-seltext_S = '9F10-TX ERUnemployment Tax-Fed'.
        gt_fieldcat-REPTEXT_DDIC = '9F10-TX ERUnemployment Tax-Fed'.

      when '8337'.
        gt_fieldcat-seltext_l = '8337-401K EE PreTax'.
        gt_fieldcat-seltext_M = '8337-401K EE PreTax'.
        gt_fieldcat-seltext_S = '8337-401K EE PreTax.'.
        gt_fieldcat-REPTEXT_DDIC = '8337-401K EE PreTax'.

      when '8387'.
        gt_fieldcat-seltext_l = '8387-401K ER Contrib'.
        gt_fieldcat-seltext_M = '8387-401K ER Contrib'.
        gt_fieldcat-seltext_S = '8387-401K ER Contrib'.
        gt_fieldcat-REPTEXT_DDIC = '8387-401K ER Contrib'.

    endcase.
    modify gt_fieldcat transporting seltext_l seltext_M seltext_S
reptext_ddic key KEY_SEL do_sum .

  endloop.


ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0478   text
*      -->P_0479   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout USING  p_cb p_color p_sum  .
  clear gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  if sy-batch eq 'X'.
    gs_layout-colwidth_optimize = 'X'.
  else.
    gs_layout-colwidth_optimize = ' '.
  endif.
*  gs_layout-default_item      = 'X'.
  gs_layout-list_append  = 'X'.


ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header type slis_t_listheader.
  data: ls_line  type slis_listheader,
         ls_color type slis_specialcol_alv,
         l_date(50).
  data: l_text(70) type c.
  data: i_lines(5).
  data: i_count(5).

  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = '401K Payroll Report'.
  append ls_line to p_gt_header.

ENDFORM.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  display_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid.
*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

* if Summary Option is choosen.
*  if p_r6 eq 'X' .

* Sort fields.
  clear gt_sort[].
  gt_sort-fieldname = 'PERNR'.
  gt_sort-UP = 'X'.
  append gt_sort.


* Layout
  MOVE : 'LINECOLOR' TO gs_layout-info_fieldname.
*         'X'         TO gs_layout-colwidth_optimize,
*         'MARK'      TO gs_layout-box_fieldname.


  call function 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer      = 'X'
            i_callback_program      = g_repid
            i_callback_user_command = 'USER_COMMAND'
            it_fieldcat             = gt_fieldcat[]
            i_save                  = 'A'
            it_events               = gt_events[]
            is_print                = gs_print
            it_sort                 = gt_sort[]
            is_layout               = gs_layout
       IMPORTING
            e_exit_caused_by_caller = g_exit_caused_by_caller
            es_exit_caused_by_user  = gs_exit_caused_by_user
       TABLES
            t_outtab                = it_output.


  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
*  endif.
ENDFORM.                    " display_grid
*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ABKRS  text
*      <--P_W_PERMO  text
*      <--P_P_BEGCA  text
*      <--P_P_ENDCA  text
*      <--P_W_ABKRT  text
*      <--P_P_ABRPC  text
*      <--P_P_ABRJC  text
*----------------------------------------------------------------------*
FORM get_payroll_period using v_abkrs
                     changing v_permo v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.

  CALL FUNCTION 'PA03_PERIODDATES_GET'
       EXPORTING
            F_ABKRS               = v_abkrs
       IMPORTING
            F_PERMO               = v_permo
            F_CURRENT_BEGDA       = v_begda
            F_CURRENT_ENDDA       = v_endda
            F_ABKRS_TEXT          = v_abkrt
       CHANGING
            F_CURRENT_PERIOD      = v_pabrp
            F_CURRENT_YEAR        = v_pabrj
       EXCEPTIONS
            PCR_DOES_NOT_EXIST    = 1
            ABKRS_DOES_NOT_EXIST  = 2
            PERIOD_DOES_NOT_EXIST = 3
            OTHERS                = 4.
  if sy-subrc <> 0.
*      message e003 with v_pabrp v_pabrj.
  endif.


ENDFORM.                    " get_payroll_period


*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE401K                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form TOP_OF_PAGE401K.
  data: ld_text type slis_text40.
  ld_text = 'Employees without 401K Eligibility Date'..
  write: / ld_text.
endform.

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE401K_SUM                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form TOP_OF_PAGE401K_SUM.
  data: ld_text type slis_text40.
  ld_text = '401K Difference Fiscal Year wise'..
  write: / ld_text.
endform.


*&---------------------------------------------------------------------*
*&      Form  build_fieldcat1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2184   text
*----------------------------------------------------------------------*
FORM build_fieldcat1 USING p_intab type slis_tabname.


  clear   : gt_fieldcat1, gt_fc1.
  refresh : gt_fieldcat1, gt_fc1.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc1
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat1[] = gt_fc1[].


ENDFORM.                    " build_fieldcat1


*---------------------------------------------------------------------*
*       FORM build_fieldcat2                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_INTAB                                                       *
*---------------------------------------------------------------------*
FORM build_fieldcat2 USING p_intab type slis_tabname.


  clear   : gt_fieldcat2, gt_fc2.
  refresh : gt_fieldcat2, gt_fc2.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc2
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat2[] = gt_fc2[].


ENDFORM.                    " build_fieldcat1
*&---------------------------------------------------------------------*
*&      Form  calc_diff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_diff.
  data : l_cnt type i.
  it_calc[] = it_output[].

  delete it_calc where srtza eq 'O' .

  loop at it_calc .
*check   it_calc-srtza eq 'A' or
*        it_calc-srtza eq 'P' .

    l_cnt = l_cnt + 1.

    if  it_calc-srtza eq 'A'.
      move-corresponding it_calc to it_diff.
      move c_red to it_diff-linecolor.
      it_diff-srtza = 'Z'.
      collect it_diff.
    else.
      move-corresponding it_calc to it_diff.
      it_diff-402  =  it_diff-402 * -1.
      it_diff-403  =  it_diff-403 * -1.
      it_diff-404  =  it_diff-404 * -1.
      it_diff-405  =  it_diff-405 * -1.
      it_diff-406  = it_diff-406 * -1.
      it_diff-407  = it_diff-407 * -1.
      it_diff-430  = it_diff-430 * -1.
      it_diff-441  =  it_diff-441 * -1.
      it_diff-448  =  it_diff-448 * -1.
      it_diff-9A01 =  it_diff-9A01 * -1.
      it_diff-9A10 =  it_diff-9A10 * -1.
      it_diff-9F01 = it_diff-9F01 * -1.
      it_diff-9F10 = it_diff-9F10 * -1.
      it_diff-8337 = it_diff-8337 * -1.
      it_diff-8387 = it_diff-8387 * -1.
      move c_red to it_diff-linecolor.

      it_diff-srtza = 'Z'.
      collect it_diff.
    endif.

    at end of  FPPER.
      if l_cnt <> 2.
        delete  it_diff where pernr = it_calc-pernr and
                              FPPER = it_calc-fpper.
        clear    l_cnt.
      else.
        clear    l_cnt.
      endif.
    endat.

  endloop.
  clear it_diff.
  loop at it_diff .
    append it_diff to it_output.
  endloop.
  sort it_output by pernr fpper srtza.

ENDFORM.                    " calc_diff
