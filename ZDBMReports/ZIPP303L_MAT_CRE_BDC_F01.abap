*----------------------------------------------------------------------*
*   INCLUDE ZIPP303L_MAT_CRE_BDC_F01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
form read_process.
  select *
       from ztbm_abxmmcdt
       into table it_ammc
       where zedat eq p_zedat
       and   zbtim eq p_zbtim.
  if sy-subrc ne 0.
    write: / text-001 color 6 on.
  endif.
endform.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
form data_process.
  perform check_material_same_data.
  perform collect_lt_matnr_read_mara.
endform.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
form write_process.
  format color col_negative intensified off.
  write: / text-002, wa_line_idx.
  write: / text-003, wa_erro_idx.
  format color off.
  if wa_erro_idx ge 1.
    format color col_heading intensified off.
    write: / text-004.
    format color off.
    write: /(10)  text-005,
            (20)  text-006,
            (20)  text-007,
            (40)  text-008,
            (10)  text-009,
            (03)  text-010,
            (06)  text-011,
            (12)  text-012,
            (18)  text-013,
            (10)  text-014,
            (07)  text-015,
            (07)  text-016,
            (07)  text-017,
            (07)  text-018,
            (10)  text-019,
            (10)  text-020,
            (18)  text-021,
            (10)  text-022,
            (10)  text-023,
            (11)  text-024,
                  text-025.

    format color off.
    loop at it_ammc where zresult eq 'E'.
      write: /(10) it_ammc-plnt,
              (20) it_ammc-mtyp,
              (20) it_ammc-mtno,
              (40) it_ammc-zdesc,
              (10) it_ammc-indu,
              (03) it_ammc-unit,
              (06) it_ammc-mgrp,
              (12) it_ammc-gica,
              (18) it_ammc-sour,
              (10) it_ammc-mtcn,
              (07) it_ammc-mrpy,
              (07) it_ammc-mrpc,
              (07) it_ammc-lots,
              (07) it_ammc-prty,
              (10) it_ammc-smky,
              (10) it_ammc-avck,
              (18) it_ammc-cnmt,
              (10) it_ammc-slmd,
              (10) it_ammc-inco,
              (11) it_ammc-vesn,
                   it_ammc-zmsg.
    endloop.
  endif.
endform.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
form collect_lt_matnr_read_mara.
  data l_tabix type sy-tabix.
  data lt_ammc type ztbm_abxmmcdt occurs 0 with header line.
  data: begin of lt_matnr occurs 0,
          matnr type marc-matnr,
          werks type marc-werks,
        end   of lt_matnr.
  data: begin of lt_marc occurs 0,
          matnr type marc-matnr,
          werks type marc-werks,
          mtart type mara-mtart,
        end   of lt_marc.
* DATA TOTAL LINES
  describe table it_ammc lines wa_line_idx.

  loop at it_ammc.
    lt_matnr-matnr = it_ammc-mtno.
    lt_matnr-werks = it_ammc-plnt.
    collect lt_matnr.
    clear: lt_matnr, it_ammc.
  endloop.

  if not lt_matnr[] is initial.
    select a~matnr
           a~werks
           b~mtart
         from marc as a inner join mara as b
                        on a~matnr eq b~matnr
         into table lt_marc
         for all entries in lt_matnr
         where a~matnr eq lt_matnr-matnr
         and   a~werks eq lt_matnr-werks.
    if sy-subrc eq 0.
*     SORTING
      sort lt_marc by matnr werks mtart.
*     ERROR CHECK
      loop at it_ammc.
        l_tabix = sy-tabix.
        read table lt_marc with key matnr = it_ammc-mtno
                                    werks = it_ammc-plnt
*                                    MTART = IT_AMMC-MTYP
                           binary search
                           transporting mtart.
        if sy-subrc eq 0.
*          P_CHECK = 'X'.
          lt_ammc = it_ammc.
          lt_ammc-zresult = 'L'.
*         MATERIAL TYPE CHECKING
          if lt_marc-mtart eq it_ammc-mtyp.
            lt_ammc-zmsg = 'MATERIAL does exist'.
          else.
            lt_ammc-zmsg =
                      'MATERIAL does exist OR MATERIAL TYPE unequal '.
          endif.
          perform zsbm_if_time_change using     'E'
                                                it_ammc-zedat
                                                it_ammc-zetim
                                       changing lt_ammc-zbdat
                                                lt_ammc-zbtim
                                                lt_ammc-zbnam
                                                lt_ammc-zmode.
          append lt_ammc.
          delete it_ammc index l_tabix.
        endif.
        clear: it_ammc, lt_marc, lt_ammc.
      endloop.
    endif.
  endif.
  describe table lt_ammc lines wa_erro_idx.
  format color col_negative intensified off.
  write: / text-026, wa_line_idx.
  write: / text-027, wa_erro_idx.
  format color off.
  perform write_error tables   lt_ammc
                      using    'L'.
  perform update_ztbm_abxmmcdt tables   lt_ammc.
endform.                    " COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
*&      Form  ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
form zsbm_if_time_change using     p_chk
                                   p_zedat
                                   p_zetim
                          changing p_zbdat
                                   p_zbtim
                                   p_zbnam
                                   p_zmode.
* BDC EXECUTE DATE
  p_zbdat = sy-datum.
  p_zbnam = sy-uname.
  p_zmode = 'C'.
* If error becomes, do not input time.
  if p_chk eq 'S'.
    p_zbtim = sy-uzeit.
  endif.
endform.                    " ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
*&      Form  ZTBM_ABXMMCDT_MARA_UPDATE
*&---------------------------------------------------------------------*
form ztbm_abxmmcdt_mara_update.
* ZTBM_ABXMMCDT UPDATE
  update ztbm_abxmmcdt from table it_ammc.
  if sy-subrc eq 0.
    commit work.
  else.
    rollback work.
  endif.


endform.                    " ZTBM_ABXMMCDT_MARA_UPDATE
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
form dynpro using dynbegin name value.
  if dynbegin = 'X'.
    clear it_bdc.
    move: name to it_bdc-program,
          value to it_bdc-dynpro,
          dynbegin to it_bdc-dynbegin.
    append it_bdc.
  else.
    clear it_bdc.
    move: name to it_bdc-fnam,
          value to it_bdc-fval.
    append it_bdc.
  endif.
endform.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
form initialization.
  p_zedat = sy-datum.
  clear: p_zbtim.
* BDC MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
endform.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
form write_error tables    pt_ammc structure it_ammc
                 using     zresult.
  if not pt_ammc[] is initial.
    format color col_heading intensified off.
    write: /(10)  text-005,
            (20)  text-006,
            (20)  text-007,
            (40)  text-008,
            (10)  text-009,
            (03)  text-010,
            (06)  text-011,
            (12)  text-012,
            (18)  text-013,
            (10)  text-014,
            (07)  text-015,
            (07)  text-016,
            (07)  text-017,
            (07)  text-018,
            (10)  text-019,
            (10)  text-020,
            (18)  text-021,
            (10)  text-022,
            (10)  text-023,
            (11)  text-024,
            (10)  text-028,
                  text-025.
    format color off.
    loop at pt_ammc where zresult eq zresult.
      write: /(10) pt_ammc-plnt,
              (20) pt_ammc-mtyp,
              (20) pt_ammc-mtno,
              (40) pt_ammc-zdesc,
              (10) pt_ammc-indu,
              (03) pt_ammc-unit,
              (06) pt_ammc-mgrp,
              (12) pt_ammc-gica,
              (18) pt_ammc-sour,
              (10) pt_ammc-mtcn,
              (07) pt_ammc-mrpy,
              (07) pt_ammc-mrpc,
              (07) pt_ammc-lots,
              (07) pt_ammc-prty,
              (10) pt_ammc-smky,
              (10) pt_ammc-avck,
              (18) pt_ammc-cnmt,
              (10) pt_ammc-slmd,
              (10) pt_ammc-inco,
              (11) pt_ammc-vesn,
              (10) pt_ammc-zresult,
                   pt_ammc-zmsg.

    endloop.
  endif.
endform.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXMMCDT
*&---------------------------------------------------------------------*
form update_ztbm_abxmmcdt tables   pt_ammc structure it_ammc.
* ERROR DATA UPDATE
  update ztbm_abxmmcdt from table pt_ammc.
  if sy-subrc eq 0.
    commit work.
  else.
    rollback work.
  endif.
endform.                    " UPDATE_ZTBM_ABXMMCDT
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
form bdc_process.
  data: lt_ammc like it_ammc occurs 0 with header line.
* DATA TOTAL LINES
  describe table it_ammc lines wa_line_idx.
  format color col_negative intensified off.
  write: / text-026, wa_line_idx.
  format color off.

  perform bdc_execution.

  refresh: it_bdc, it_mess.
* CONFIGURABLE 'X'
  perform mm02_bdc_exection.

  clear: wa_line_idx, wa_erro_idx.
* ERROR LINES
  loop at it_ammc.
    case it_ammc-zresult.
      when 'E'.
*        WA_ERRO_IDX = WA_ERRO_IDX + 1.
        lt_ammc = it_ammc.
        append lt_ammc.
      when others.
        wa_line_idx = wa_line_idx + 1.
    endcase.
    clear: it_ammc, lt_ammc.
  endloop.
  describe table lt_ammc lines wa_erro_idx.
  format color col_negative intensified off.
  write: / text-002, wa_line_idx.
  write: / text-003, wa_erro_idx.
  format color off.
  perform write_error tables   lt_ammc
                      using    'E'.
* ZTBM_ABXMMCDT UPDATE
  perform update_ztbm_abxmmcdt tables   it_ammc.

endform.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
form check_material_same_data.
  data: lt_ammc like it_ammc occurs 0 with header line,
        mt_ammc like it_ammc occurs 0 with header line,
        l_tabix type sy-tabix,
        l_count type i.
* DATA TOTAL LINES
  describe table it_ammc lines wa_line_idx.
  lt_ammc[] = it_ammc[].
  clear: lt_ammc, it_ammc.

  loop at it_ammc.
    l_tabix = sy-tabix.
    loop at lt_ammc where mtno eq it_ammc-mtno.
      l_count = l_count + 1.
    endloop.
    if l_count gt 2.
*      P_CHECK = 'X'.
      mt_ammc = it_ammc.
      mt_ammc-zresult = 'L'.
      mt_ammc-zmsg = 'MATERIAL does exist that PLANT is unequal'.
      perform zsbm_if_time_change using     'E'
                                             mt_ammc-zedat
                                             mt_ammc-zetim
                                    changing mt_ammc-zbdat
                                             mt_ammc-zbtim
                                             mt_ammc-zbnam
                                             mt_ammc-zmode.
      append mt_ammc.
      delete it_ammc index l_tabix.
    endif.
    clear: l_count, it_ammc, lt_ammc, mt_ammc.
  endloop.
  if not mt_ammc[] is initial.
    describe table mt_ammc lines wa_erro_idx.
    format color col_negative intensified off.
    write: / text-026, wa_line_idx.
   write: / 'MATERIAL does exist that PLANT is unequal : ', wa_erro_idx.
    format color off.
    perform write_error tables   mt_ammc
                        using    'L'.
    perform update_ztbm_abxmmcdt tables   mt_ammc.
  endif.
endform.                    " CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
form fert_selection_view.
  data: l_vkorg      type rmmg1-vkorg,  "Sales organization
        l_prodh      type t179t-prodh,  "Product hierarchy
        l_mtpos      type mvke-mtpos,   "Item category group from M/M
        l_natn_c     type ztbm_abxtrtdt-natn_c,
        l_delr_c     type ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 type ztbm_abxtrtdt-blnk_cloc4.

  clear l_vkorg.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS
** Changed and Added by Tonkey on 05/20/2004.
  move: it_ammc-mtno+01(03) to l_natn_c,
        it_ammc-mtno+04(02) to l_delr_c.
*
  select single blnk_cloc4
    into l_blnk_cloc4
    from ztbm_abxtrtdt
    where natn_c = l_natn_c and  "Nation
          delr_c = l_delr_c   .  "Dealer
*
  case l_blnk_cloc4.
    when 'D'.
      l_vkorg = 'D100'.
      l_mtpos = '0002'.
    when 'E'.
      l_vkorg = 'E100'.
      l_mtpos = 'Z002'.
  endcase.
*  case it_ammc-mtno+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.
*    when 'B06'.
*      l_vkorg = 'E100'.
*  endcase.
**

  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'RMMG1-VKORG' l_vkorg,
     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  perform read_t179t using    it_ammc-mtyp
                              it_ammc-mtno+6(2)
                     changing l_prodh.
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      it_ammc-zdesc,
     ' ' 'MARA-MEINS'      it_ammc-unit,
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_AMMC-SOUR,
     ' ' 'MARA-PROFL' it_ammc-sour,
     ' ' 'BDC_OKCODE'  '=SP04'.
* Classification
* Sales: Sales Org. Data 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',
*     ' ' 'MG03STEUER-TAXKM(01)' '0',
*     ' ' 'MG03STEUER-TAXKM(02)' '0',
*     ' ' 'MG03STEUER-TAXKM(03)' '0',
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',
     ' ' 'MG03STEUER-TAXKM(02)' '0',
     ' ' 'MG03STEUER-TAXKM(03)' '0',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,
     ' ' 'MVKE-PRODH'      l_prodh,
     ' ' 'MVKE-MVGR3'      it_ammc-mtno+12(01),
     ' ' 'MVKE-MVGR4'      it_ammc-mtno+10(01),
     ' ' 'MVKE-MVGR5'      it_ammc-mtno+09(01),
** Changed By Tonkey on 05/20/2004.
     ' ' 'MVKE-MTPOS'      l_mtpos,
*     ' ' 'MVKE-MTPOS'      '0002',
**
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'MARA-TRAGR'  '0001',
     ' ' 'MARC-LADGR'  '0001',
     ' ' 'BDC_OKCODE'  '=SP12'.
* Foreign Trade: Export Data
* Sales Text
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
* 2003.10.27
*     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,
     ' ' 'MARC-DISPO'  'V01',
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'MARC-STRGR'  '56',
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARC-ALTSL'  IT_AMMC-SLMD,
     ' ' 'MARC-ALTSL'  '2',
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'MARC-SAUFT'  'X',
*     ' ' 'MARC-SFEPR'  '0002',
     ' ' 'MARC-SFEPR'  'VEHI',
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting
* Work Scheduling
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  '001',
     ' ' 'MARC-UNETO'  '10.0',
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Warehouse Management 1
* Warehouse Management 2
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7920',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.

endform.                    " FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
form halb_selection_view.
  data: l_prodh like t179t-prodh.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
*     ' ' 'RMMG1-VKORG' 'D100',
*     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  perform read_t179t using    it_ammc-mtyp
                              it_ammc-mtno+6(2)
                     changing l_prodh.
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'MAKT-MAKTX' it_ammc-zdesc,
     ' ' 'MARA-MEINS' it_ammc-unit,
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,
     ' ' 'BDC_OKCODE'  '=SP02'.
* Basic Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_AMMC-SOUR,
     ' ' 'MARA-PROFL' it_ammc-sour,
     ' ' 'BDC_OKCODE'  '=SP12'.
* 11/25/2003 CHANGE
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_AMMC-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_AMMC-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'MARC-RGEKZ'  '1',
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  '001',
     ' ' 'MARC-UNETO'  '10.0',
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
endform.                    " HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
form halb_phan_selection_view.
  data: l_prodh like t179t-prodh.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'BDC_OKCODE'  '=ENTR'.

* Basic Data 1
  perform read_t179t using    it_ammc-mtyp
                              it_ammc-mtno+6(2)
                     changing l_prodh.
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      it_ammc-zdesc,
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'MARA-MEINS'      it_ammc-unit,
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_AMMC-NORMT,
     ' ' 'MARA-PROFL' it_ammc-sour,
     ' ' 'BDC_OKCODE'  '=SP12'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_AMMC-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_AMMC-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'MARC-RGEKZ'  '1',
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
endform.                    " HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
form roh_selection_view.
  data: l_matkl type mara-matkl.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX' it_ammc-zdesc,
     ' ' 'MARA-MEINS' it_ammc-unit.
*       2004/02/26 SCREEN NUMBER CHANGE
*     ' ' 'MARA-MATKL' 'A01',  "CLIENT '130'
*     ' ' 'MARA-MATKL' 'P01',  "CLIENT '140'
  clear l_matkl.
  perform read_t023t changing l_matkl.
  perform dynpro using:
     ' ' 'MARA-MATKL' l_matkl,  "CLIENT '140'
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,
     ' ' 'BDC_OKCODE'  '=SP02'.
* Basic Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_AMMC-NORMT,
     ' ' 'MARA-PROFL' it_ammc-sour,
     ' ' 'BDC_OKCODE'  '=SP09'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_AMMC-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_AMMC-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* Purchasing View
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-BESKZ'  IT_AMMC-BESKZ,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'MARC-RGEKZ'  '1',
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
     'X' 'SAPLMGMM'    '5000',
*     'X' 'SAPLMGMM'    '4000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '3000',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
*     ' ' 'MBEW-BKLAS'  '3000',
     ' ' 'BDC_OKCODE'  '=BU'.
endform.                    " ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
form rkc_msg_string changing p_msg.
  call function 'RKC_MSG_STRING'
       exporting
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       importing
            msg_lin = p_msg
       exceptions
            others  = 1.
endform.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  BDC_EXECUTION
*&---------------------------------------------------------------------*
form bdc_execution.
  data: l_tabix type sy-tabix,
        l_mtart type mara-mtart,
        l_messg like cfgnl-msglin.
  loop at it_ammc.
    l_tabix = sy-tabix.
    select single mtart
            from mara
            into l_mtart
            where matnr eq it_ammc-mtno.
    if sy-subrc eq 0.
      select single *
                  from marc
                  where matnr eq it_ammc-mtno
                  and   werks eq it_ammc-plnt.
      if sy-subrc eq 0.
*       Material already maintained for this transaction/event
        perform error_log_modify using l_tabix.
      else.
*   Create material that there does not exist to 'MARC' being to 'MARA'.
        if l_mtart eq it_ammc-mtyp.
          perform marc_does_not_exist_material.
          perform call_transaction_mm01 using l_tabix.
        else.
          perform error_log_mat_type using l_tabix.
        endif.
      endif.
    else.
      perform mara_does_not_exist_material.
      perform call_transaction_mm01 using l_tabix.
    endif.
    refresh: it_bdc, it_mess.
    clear: it_ammc.
  endloop.
endform.                    " BDC_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
form mm02_bdc_exection.
  loop at it_ammc where zresult ne 'E'
                  and   mtcn    eq 'X'.
    perform dynpro using:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR'  it_ammc-mtno,
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)'  'X',
       ' ' 'BDC_OKCODE'  '=ENTR',
*       2003/12/21 SCREEN NUMBER CHANGE
*       'X' 'SAPLMGMM'    '4004',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  it_ammc-mtcn,
       ' ' 'BDC_OKCODE'  '=BU'.
    call transaction 'MM02'   using it_bdc
                     options  from wa_opt
                     messages into it_mess.
    refresh: it_bdc, it_mess.
  endloop.
endform.                    " MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
*&      Form  READ_T179T
*&---------------------------------------------------------------------*
form read_t179t using    p_mtart
                         p_carty
                changing p_prodh.
  data: l_prodh type t179t-prodh,
        l_vtext type t179t-vtext.
  case p_mtart.
    when 'FERT'.
      concatenate p_mtart '%' into l_vtext.
      select single prodh
                  from t179t
                  into l_prodh
                  where vtext like l_vtext.
      clear l_vtext.

      concatenate l_prodh '%' into l_prodh.
      concatenate p_carty '%' into l_vtext.
      select single prodh
           from t179t
           into p_prodh
           where prodh like l_prodh
           and   vtext like l_vtext.
    when 'HALB'.
      concatenate p_mtart '%' into l_vtext.
      select single prodh
                  from t179t
                  into p_prodh
                  where vtext like l_vtext.
      clear l_vtext.
  endcase.
endform.                    " READ_T179T
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
form error_log_modify using    p_tabix.
  it_ammc-zresult = 'E'.
  it_ammc-zbdat = sy-datum.
  it_ammc-zbnam = sy-uname.
  it_ammc-zmode = 'C'.
  it_ammc-zmsg =
  'Material already maintained for this transaction/event'.
* MODIFY IT_AMMC
  modify it_ammc index p_tabix transporting zresult
                                            zbdat
                                            zbnam
                                            zmode
                                            zmsg.
endform.                    " ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
form marc_does_not_exist_material.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' it_ammc-mtno,    "
     ' ' 'RMMG1-MBRSH' it_ammc-indu,    "
     ' ' 'RMMG1-MTART' it_ammc-mtyp,    "
     ' ' 'BDC_OKCODE'  '/00'.
* MATERIAL TYPE
  case it_ammc-mtyp.
    when 'FERT'.
*     Industry Standard Description
      perform fert_selection_view1.
    when 'HALB'.
      case it_ammc-sour.
*       MIP
        when 'M'.
*         SELECTION VIEW( HALB(MIP) : BDC TAB )
          perform halb_selection_view1.
*       PHANTOM
        when others.
*         SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          perform halb_phan_selection_view1.
      endcase.

    when 'ROH'.
*     SELECTION VIEW( ROH : BDC TAB )
      perform roh_selection_view1.
  endcase.
endform.                    " MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
form fert_selection_view1.
  data: l_vkorg      type rmmg1-vkorg,
        l_mtpos      TYPE mvke-mtpos,   "Item category group from M/M
        l_natn_c     type ztbm_abxtrtdt-natn_c,
        l_delr_c     type ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 type ztbm_abxtrtdt-blnk_cloc4.

  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS
** Changed and Added by Tonkey on 05/20/2004.
  move: it_ammc-mtno+01(03) to l_natn_c,
        it_ammc-mtno+04(02) to l_delr_c.
*
  select single blnk_cloc4
    into l_blnk_cloc4
    from ztbm_abxtrtdt
    where natn_c = l_natn_c and  "Nation
          delr_c = l_delr_c   .  "Dealer
*
  CASE l_blnk_cloc4.
    WHEN 'D'.
      l_vkorg = 'D100'.
      l_mtpos = '0002'.
    WHEN 'E'.
      l_vkorg = 'E100'.
      l_mtpos = 'Z002'.
  ENDCASE.
*  case it_ammc-mtno+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.
*    when 'B06'.
*      l_vkorg = 'E100'.
*  endcase.

**

  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'RMMG1-VKORG' l_vkorg,
     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Sales: Sales Org. Data 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',
     ' ' 'MG03STEUER-TAXKM(02)' '0',
     ' ' 'MG03STEUER-TAXKM(03)' '0',
     ' ' 'BDC_OKCODE'  '/00',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARA-MTPOS_MARA' IT_EXCL-MTPOS,

** Changed By Tonkey on 05/20/2004.
     ' ' 'MVKE-MTPOS'      l_mtpos,
*     ' ' 'MVKE-MTPOS'      '0002',
**

     ' ' 'BDC_OKCODE'  '=SP06'.
* Sales: General/Plant Data
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
*     ' ' 'MARA-TRAGR'  '0001',
     ' ' 'MARC-LADGR'  '0001',
     ' ' 'BDC_OKCODE'  '=SP12'.
* Foreign Trade: Export Data
* Sales Text
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_ammc-zdesc,
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
* 2003.10.27
*     ' ' 'MARC-DISPO'  IT_AMMC-MRPC,
     ' ' 'MARC-DISPO'  'V01',
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'MARC-SAUFT'  'X',
     ' ' 'MARC-SFEPR'  '0002',
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting
* Work Scheduling
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  '001',
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Warehouse Management 1
* Warehouse Management 2
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7920',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.

endform.                    " FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
form halb_selection_view1.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
*     ' ' 'RMMG1-VKORG' 'D100',
*     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_ammc-zdesc,
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  '001',
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
endform.                    " HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
form halb_phan_selection_view1.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
*     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'BDC_OKCODE'  '=ENTR'.

* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_ammc-zdesc,
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
endform.                    " HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
form roh_selection_view1.
  perform dynpro using:
     'X' 'SAPLMGMM'    '0070',
*     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  perform dynpro using:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Purchasing View
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_ammc-zdesc,
     ' ' 'MARC-DISMM'  it_ammc-mrpy,
     ' ' 'MARC-DISPO'  it_ammc-mrpc,
     ' ' 'MARC-DISLS'  it_ammc-lots,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY,
     ' ' 'MARC-FHORI'  it_ammc-smky,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,
     ' ' 'MARC-SBDKZ'  it_ammc-inco,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '3000',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  perform dynpro using:
*       2003/12/21 SCREEN NUMBER CHANGE
*    'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-STPRS'  '1',
*     ' ' 'MBEW-BKLAS'  '3000',
     ' ' 'BDC_OKCODE'  '=BU'.

endform.                    " ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
form call_transaction_mm01 using    p_tabix.
  data l_messg like cfgnl-msglin.

  call transaction 'MM01'  using it_bdc
                  options from wa_opt
                  messages into it_mess.
  it_ammc-zresult = sy-msgty.

  perform rkc_msg_string changing l_messg.

  it_ammc-zmsg = l_messg.
  perform zsbm_if_time_change using     it_ammc-zresult
                                        it_ammc-zedat
                                        it_ammc-zetim
                               changing it_ammc-zbdat
                                        it_ammc-zbtim
                                        it_ammc-zbnam
                                        it_ammc-zmode.
*   MODIFY IT_AMMC
  modify it_ammc index p_tabix transporting zbdat
                                            zbtim
                                            zbnam
                                            zmode
                                            zresult
                                            zmsg.
  refresh: it_bdc, it_mess.
  clear: it_ammc.
endform.                    " CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
form error_log_mat_type using    p_tabix.
  it_ammc-zresult = 'E'.
  it_ammc-zbdat = sy-datum.
  it_ammc-zbnam = sy-uname.
  it_ammc-zmode = 'C'.
  it_ammc-zmsg = 'MATERIAL TYPE is Mismatch'.
* MODIFY IT_AMMC
  modify it_ammc index p_tabix transporting zresult
                                            zbdat
                                            zbnam
                                            zmode
                                            zmsg.
endform.                    " ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
*&      Form  MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
form mara_does_not_exist_material.
  perform dynpro using:
      'X' 'SAPLMGMM'    '0060',
      ' ' 'RMMG1-MATNR' it_ammc-mtno,   "
      ' ' 'RMMG1-MBRSH' it_ammc-indu,   "
      ' ' 'RMMG1-MTART' it_ammc-mtyp,   "
      ' ' 'BDC_OKCODE'  '/00'.
*   MATERIAL TYPE
  case it_ammc-mtyp.
    when 'FERT'.
*       Industry Standard Description
      perform fert_selection_view.
    when 'HALB'.
      case it_ammc-sour.
*           MIP
        when 'M'.
          perform halb_selection_view.
*           PHANTOM
        when others.
*           SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          perform halb_phan_selection_view.
      endcase.

    when 'ROH'.
      perform roh_selection_view.
  endcase.
endform.                    " MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_T023T
*&---------------------------------------------------------------------*
form read_t023t changing p_matkl.
  select single matkl
              from t023t
              into p_matkl.
endform.                    " READ_T023T
