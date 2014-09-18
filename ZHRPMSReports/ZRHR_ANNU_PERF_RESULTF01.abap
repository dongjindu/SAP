*&---------------------------------------------------------------------*
*&  Include           ZRHR_ANNU_PERF_RESULTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_droplist_year .

  data: lt_values     type vrm_values,
        ls_value      like line of lt_values,
        l_fieldname   type vrm_id,
        l_year        type zdhr_year.

  l_fieldname = 'P_YEAR'.
  l_year = sy-datum(4).

  clear lt_values.
  do 10 times.
    ls_value-key = l_year.
    ls_value-text = l_year.
    append ls_value to lt_values.clear ls_value.
    l_year = sy-datum(4) - sy-index.
  enddo.

  call function 'VRM_SET_VALUES'
    exporting
      id     = l_fieldname
      values = lt_values.

endform.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  set_droplist_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form set_droplist_status.

*    08/14/2013 - T00306  Start
  data: lt_values     type vrm_values,
        ls_value      like line of lt_values,
        l_fieldname   type vrm_id,
        l_staus       type hap_ap_status.

  data: lt_dd07t type table of dd07t with header line.

  l_fieldname = 'P_STATS'.

  select *
     into table lt_dd07t
     from dd07t
    where domname = 'HAP_AP_STATUS'
      and ddlanguage = sy-langu
      and as4local = 'A'
      and domvalue_l in ('3','5').

  clear lt_values.
  loop at lt_dd07t.
    ls_value-key = lt_dd07t-domvalue_l.
    ls_value-text = lt_dd07t-ddtext.
    append ls_value to lt_values.
    clear ls_value.
  endloop.

  sort lt_values by key ascending.

  call function 'VRM_SET_VALUES'
    exporting
      id     = l_fieldname
      values = lt_values.
*    08/14/2013 - T00306  End

endform.                    "set_droplist_status
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .

  data: lt_appraisees         type hap_t_hrsobid,
        lt_documents          type hap_t_documents,
        lt_ktext              type table of cskt with header line,
        lt_p0041              type table of pa0041 with header line.

  data: ls_sel_dates          type hap_s_sel_dates,
        ls_sel_status         type hap_s_sel_status,
        ls_sel_with_or_witout type hap_s_sel_with_or_without,
        ls_appraisees         type hrsobid,
        ls_return             type bapiret2,
        ls_documents          type hap_s_documents.

  data: begin of lt_p0000 occurs 0,
          pernr               type p0000-pernr,
          stat2               type p0000-stat2,
        end of lt_p0000.

  data: begin of lt_p0001 occurs 0,
          pernr               type pa0001-pernr,
          kostl               type pa0001-kostl,
          plans               type pa0001-plans,
          stell               type pa0001-stell,
        end of lt_p0001.

  data: begin of lt_p0002 occurs 0,
          pernr               type pa0002-pernr,
          vorna               type pa0002-vorna,
          nachn               type pa0002-nachn,
        end of lt_p0002.

  data: begin of lt_class occurs 0,
          clfid               type zthr_clfaj-clfid,
          jobid               type zthr_clfaj-jobid,
          clftx               type zthr_class-clftx,
          zefrom              type zthr_class-zefrom,
          zeto                type zthr_class-zeto,
        end of lt_class.

  data: begin of lt_hirda occurs 0,
          pernr               type pa0000-pernr,
          begda               type pa0000-begda,
        end of lt_hirda.

  data: l_endda               type endda,
        l_index               type n length 2,
        l_fieldname           type string,
        l_rfc_destination     type rfcdest.

  data: rt_kostl              type range of kostl with header line.

  field-symbols: <fs_dar>     type any,
                 <fs_dat>     type any.

  clear: gt_result.

  if p_year is initial.
    message s026 display like 'E'.
    exit.
  else.
    concatenate p_year '1231' into l_endda.
  endif.

  if p_stats is initial.
    message s011 with 'Please select the status' display like 'E'.
    exit.
  endif.

*  09/05/2013 - T00306 Start
  data: l_myey type zdhr_myey.

  case p_stats.
    when '3'.
      l_myey = 'MY'.
    when others.
      l_myey = 'EY'.
  endcase.

  select * from hrp9873
    into table gt_hrp9873
    where plvar = '01'
      and otype = 'VC'
      and istat in ('3', '4')
      and begda <= sy-datum
      and endda >= sy-datum
      and role_id in ('Z0','Z7','Z9')
      and myey = l_myey.
  sort gt_hrp9873 by role_id.
*  09/05/2013 - T00306 End

************************************************
*   get data
************************************************
  " get classification text
  clear lt_class[].
  select a~clfid
         a~jobid
         b~clftx
** On 11/14/13 for YEAR check
         b~zefrom
         b~zeto
** End on 11/14/13
    from zthr_clfaj as a
      inner join zthr_class as b
      on a~clfid = b~clfid
    into table lt_class.

** On 11/14/13 for YEAR check
    loop at lt_class.
   if p_year >= lt_class-zefrom+0(4)
      and p_year <= lt_class-zeto+0(4).
   else.
     delete lt_class.
   endif.
  endloop.
** End
  sort lt_class by jobid.

  " check active
  clear: lt_p0000[].
  select pernr stat2 from pa0000
    into table lt_p0000
    where pernr in s_pernr
      and endda >= l_endda
      and begda <= l_endda
      and stat2 = '3'.

  check lines( lt_p0000 ) > 0.
  sort lt_p0000 by pernr.
  delete adjacent duplicates from lt_p0000 comparing pernr.

  " check Employee Group, Employee Subgroup
  clear lt_p0001[].
  select pernr
         kostl
         plans
         stell
    from pa0001
    into table lt_p0001
    for all entries in lt_p0000
    where pernr = lt_p0000-pernr
      and endda >= l_endda
      and begda <= l_endda
      and persg = '1'
      and persk in ('U2','U3')
      and kostl in s_kostl.

  check lines( lt_p0001 ) > 0.
  sort lt_p0001 by pernr.
  delete adjacent duplicates from lt_p0001 comparing pernr.

  " get first name, last name
  clear lt_p0002[].
  select pernr vorna nachn from pa0002
    into table lt_p0002
    for all entries in lt_p0001
    where pernr = lt_p0001-pernr
      and endda >= l_endda
      and begda <= l_endda.
  sort lt_p0002 by pernr.

  clear rt_kostl[].
  loop at lt_p0001.
    rt_kostl-sign = 'I'.
    rt_kostl-option = 'EQ'.
    rt_kostl-low = lt_p0001-kostl.
    append rt_kostl.clear rt_kostl.
  endloop.
  sort rt_kostl by low.
  delete adjacent duplicates from rt_kostl comparing low.

  " get cost center text
  clear lt_ktext[].
  select * from cskt
    into table lt_ktext
    where spras = sy-langu
      and kokrs = 'H201'
      and kostl in rt_kostl
      and datbi = '99991231'.
  sort lt_ktext by kostl.

  " get date specifications
  clear lt_p0041[].
  select * from pa0041
    into table lt_p0041
    for all entries in lt_p0001
    where pernr = lt_p0001-pernr
      and endda >= l_endda
      and begda <= l_endda.

  " set hire date
  clear lt_hirda[].
  loop at lt_p0041.
    lt_hirda-pernr = lt_p0041-pernr.
    do 12 times.
      l_index = l_index + 1.
      concatenate 'LT_P0041-DAR' l_index into l_fieldname.
      assign (l_fieldname) to <fs_dar>.
      if <fs_dar> eq 'Z1'.
        concatenate 'LT_P0041-DAT' l_index into l_fieldname.
        assign (l_fieldname) to <fs_dat>.
        lt_hirda-begda = <fs_dat>.
        unassign: <fs_dar>, <fs_dat>.
        exit.
      endif.

      unassign <fs_dar>.
    enddo.

    append lt_hirda.
    clear: lt_p0041, lt_hirda, l_index.
  endloop.

  " set appraisal dates
  clear ls_sel_dates.
  ls_sel_dates-validity_to_date = l_endda.

  " set appraisal status
  clear ls_sel_status.
  case p_stats.
    when '3'.
      ls_sel_status-ap_status_3 = 'X'.
    when others.
      ls_sel_status-ap_status_5 = 'X'.
  endcase.

  " set appraisal display
  clear ls_sel_with_or_witout.
  ls_sel_with_or_witout-sel_display_existing = 'X'.
*  ls_sel_with_or_witout-sel_display_without = 'X'.

  " get destination
  call function 'HRHAP_GET_RFC_DESTINATION'
*   EXPORTING
*     SYSTEM                = 'WDU' " YHM left empty to force space
    importing
      rfc_destination       = l_rfc_destination.

  loop at lt_p0001.
    " set appraisal appraisee
    ls_appraisees-plvar = '01'.
    ls_appraisees-otype = 'P'.
    ls_appraisees-sobid = lt_p0001-pernr.
    append ls_appraisees to lt_appraisees.

    " get appraisal document
    call function 'HRHAP_RFC_DOCUMENT_GET_LIST'
      destination l_rfc_destination
      exporting
        plan_version          = '01'
        s_sel_date            = ls_sel_dates
        s_sel_status          = ls_sel_status
        s_sel_with_or_without = ls_sel_with_or_witout
      importing
        s_return              = ls_return
      tables
        t_appraisees          = lt_appraisees
        t_documents           = lt_documents.

    if ls_return-type eq 'E'.
      continue.
    endif.

    " read the most recent appraisal document
    sort lt_documents by ap_end_date descending.
    read table lt_documents into ls_documents
                            index 1.
    if sy-subrc ne 0.
      continue.
    endif.

    " set cost center id
    gs_result-kostl = lt_p0001-kostl.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gs_result-kostl
      importing
        output = gs_result-kostl.
    " read cost center text
    read table lt_ktext with key kostl = lt_p0001-kostl
                        binary search.
    if sy-subrc = 0.
      " set cost center text
      gs_result-ktext = lt_ktext-ktext.
    endif.

    " set appraisee id
    gs_result-appee = ls_documents-appraisee_id.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gs_result-appee
      importing
        output = gs_result-appee.
    " read first name, last name
    read table lt_p0002 with key pernr = lt_p0001-pernr
                        binary search.
    if sy-subrc = 0.
      " set appraisee first name, last name
      gs_result-vorna = lt_p0002-vorna.
      gs_result-nachn = lt_p0002-nachn.
    endif.

    " read classification
    read table lt_class with key jobid = lt_p0001-stell
                        binary search.
    if sy-subrc = 0.
      " set classification
      gs_result-clfid = lt_class-clfid.
      gs_result-clftx = lt_class-clftx.
    endif.

    " get grade
    select single grade from hrp9870
      into gs_result-grade
      where plvar = '01'
        and otype = 'S'
        and objid = lt_p0001-plans
        and istat = '1'
        and begda <= ls_documents-ap_end_date
        and endda >= ls_documents-ap_end_date.

    " set supervisor id, name
    gs_result-apper = ls_documents-appraiser_id.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gs_result-apper
      importing
        output = gs_result-apper.
    gs_result-appernm = ls_documents-appraiser_name.

    " read hiring date
    read table lt_hirda with key pernr = lt_p0001-pernr
                        binary search.
    if sy-subrc = 0.
      " set hiring date
      gs_result-hirda = lt_hirda-begda.
    endif.

    " set appraisal period
    gs_result-zyear = ls_documents-ap_end_date(4).
    gs_result-strda = ls_documents-ap_start_date.
    gs_result-endda = ls_documents-ap_end_date.

    " get appraisal detail info
    perform get_detail using ls_documents.

    append gs_result to gt_result.

    clear: lt_p0001, lt_appraisees, ls_appraisees, ls_return,
           lt_documents, ls_documents, gs_result.
  endloop.


  if gt_result[] is initial.
    message s027 display like 'E'.
    exit.
  endif.

endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DOCUMENTS  text
*      <--P_GS_RESULT  text
*----------------------------------------------------------------------*
form get_detail  using    ps_documents type hap_s_documents.

  data: lt_body_elements  type hap_t_body_elements,
        lt_body_cells     type hap_t_body_cells,
        lt_hrp1001        type table of hrp1001 with header line,
        lt_hrp9871        type table of hrp9871 with header line.
  data: ls_appraisal_id   type hap_s_appraisal_id,
        ls_body_elements  type hap_s_body_elements,
        ls_body_cells     type hap_s_body_cells,
        ls_return         type bal_s_msg.
  data: rt_objid          type range of hrobjid with header line.

  clear ls_appraisal_id.
  ls_appraisal_id-appraisal_id = ps_documents-appraisal_id.
  ls_appraisal_id-part_ap_id = ps_documents-part_ap_id.

  " get appraisal detail
  call function 'HRHAP_DOCUMENT_GET_DETAIL'
    exporting
      plan_version    = ps_documents-plan_version
      s_appraisal_id  = ls_appraisal_id
    importing
      t_body_elements = lt_body_elements
      t_body_cells    = lt_body_cells
      s_return        = ls_return.

  if ls_return-msgty eq 'E'.
    exit.
  endif.

  call function 'ZFHR_CHANGE_RATING'
    exporting
      i_status        = p_stats
    tables
      t_body_elements = lt_body_elements
      t_body_cells    = lt_body_cells.


  sort lt_body_elements by row_iid.
  sort lt_body_cells by row_iid column_iid.

  " read VA element
  read table lt_body_elements into ls_body_elements
                              index 1.
  if sy-subrc ne 0.
    exit.
  endif.

  " get VB elements
  clear lt_hrp1001[].
  select * from hrp1001
    into table lt_hrp1001
    where otype = ls_body_elements-element_type
      and objid = ls_body_elements-element_id
      and plvar = '01'
      and rsign = 'B'
      and relat = '605'
      and istat in ('3','4')
      and begda <= sy-datum
      and endda >= sy-datum.

  check lines( lt_hrp1001 ) > 0.
  sort lt_hrp1001 by sobid.
  delete adjacent duplicates from lt_hrp1001 comparing sobid.

  clear rt_objid[].
  loop at lt_hrp1001.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_hrp1001-sobid.
    append rt_objid.clear rt_objid.
  endloop.

  " get Rating Summary, Overall Ratings VB elements
  clear lt_hrp9871[].
  select * from hrp9871
    into table lt_hrp9871
    where plvar = '01'
      and otype = 'VB'
      and objid in rt_objid
      and istat in ('3','4')
      and begda <= sy-datum
      and endda >= sy-datum
      and igid in (5,6).

  data: lv_first type c.

*  08/27/2013 - T00306 Start
  data: l_column_iid type hap_column_iid.
  case p_stats.
    when '3'.
      l_column_iid = c_column_iid_zp15.
    when others.
      l_column_iid = c_column_iid_fapp.
  endcase.
*  08/27/2013 - T00306 End


  loop at lt_hrp9871.
    " read vb element
    read table lt_body_elements into ls_body_elements
                                with key element_id = lt_hrp9871-objid.
    if sy-subrc = 0.
      if lt_hrp9871-igid = 5.     " Rating Summary
        " read child

        lv_first = 'X'.
        read table lt_body_elements into ls_body_elements
                                    with key row_iid = ls_body_elements-child
                                    binary search.
        while sy-subrc = 0.

          if lv_first is initial.
            read table lt_body_elements into ls_body_elements
                                          with key row_iid = ls_body_elements-brother
                                          binary search.

            if sy-subrc ne 0.
              exit.
            endif.
          endif.

          if lv_first eq 'X'.
            clear lv_first.
          endif.

          " read annual rating cell
          read table lt_body_cells into ls_body_cells
                                   with key row_iid = ls_body_elements-row_iid
                                            column_iid = l_column_iid
                                   binary search.
          if sy-subrc = 0.
*** 07/22/2013 - T00306 Start
            find 'Performance' in ls_body_elements-name.
            if sy-subrc eq 0.
              " set performance
              gs_result-pfmev = ls_body_cells-value_text.
              continue.
            endif.
            find 'Core' in ls_body_elements-name.
            if sy-subrc eq 0.
              " set core competency
              gs_result-corev = ls_body_cells-value_text.
              continue.
            endif.
            find 'Leadership' in ls_body_elements-name.
            if sy-subrc eq 0.
              " set leadership competency
              gs_result-ldsev = ls_body_cells-value_text.
              continue.
            endif.
            find 'Job' in ls_body_elements-name.
            if sy-subrc eq 0.
              " set job competency
              gs_result-jobev = ls_body_cells-value_text.
              continue.
            endif.
*** 07/22/2013 - T00306 End
          endif.

        endwhile.

      elseif lt_hrp9871-igid = 6. " Overall Rating

        case p_stats.
          when '3'.
            read table lt_body_cells into ls_body_cells
                                     with key row_iid = ls_body_elements-row_iid
                                              column_iid = c_column_iid_zp15
                                     binary search.
            if sy-subrc = 0.
              " set annual evaluation
              gs_result-annev = ls_body_cells-value_text.
              " set annual rating
              gs_result-annrt = ls_body_cells-value_num.
** On 11/20/13 by Furong
              SHIFT gs_result-annrt right DELETING TRAILING ` `.
              SHIFT gs_result-annrt by 1 places right.
** End
            endif.
          when others.
            " read annual rating cell
            read table lt_body_cells into ls_body_cells
                                     with key row_iid = ls_body_elements-row_iid
                                              column_iid = c_column_iid_fapp
                                     binary search.
            if sy-subrc = 0.
              " set annual evaluation
              gs_result-annev = ls_body_cells-value_text.
            endif.
            "read Raing cell
            read table lt_body_cells into ls_body_cells
                                     with key row_iid = ls_body_elements-row_iid
                                              column_iid = c_column_iid_zp16
                                     binary search.
            if sy-subrc = 0.
              " set annual rating
              gs_result-annrt = ls_body_cells-value_num.
** On 11/20/13 by Furong
              SHIFT gs_result-annrt right DELETING TRAILING ` `.
              SHIFT gs_result-annrt by 1 places right.
** End
            endif.
        endcase.
      endif.
    endif.
  endloop.

endform.                    " GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_alv_100 .

  data: ls_variant type disvariant.

  if gr_cont is initial.
    create object gr_cont
      exporting
        container_name = 'CONTAINER'.

    create object gr_grid
      exporting
        i_parent = gr_cont.

    perform set_layout.
    perform set_fcat.

    ls_variant-report   = sy-repid.
    ls_variant-username = sy-uname.

    call method gr_grid->set_table_for_first_display
      exporting
        is_layout                     = gs_layo
        is_variant                    = ls_variant
        i_save                        = 'A'
      changing
        it_outtab                     = gt_result
        it_fieldcatalog               = gt_fcat[]
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

  else.
    call method gr_grid->refresh_table_display
      exceptions
        finished = 1
        others   = 2.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

  endif.

endform.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_layout .

  gs_layo-zebra = 'X'.
  gs_layo-sel_mode = 'D'.

endform.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fcat .

  clear: gt_fcat[].
  " Cost Center ID
  gt_fcat-fieldname = 'KOSTL'.
  gt_fcat-coltext = text-t02.
  gt_fcat-col_pos = 1.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  append gt_fcat.clear: gt_fcat.

  " Cost Center Name
  gt_fcat-fieldname = 'KTEXT'.
  gt_fcat-coltext = text-t03.
  gt_fcat-col_pos = 2.
  gt_fcat-outputlen = 20.
  append gt_fcat.clear: gt_fcat.

  " TM ID
  gt_fcat-fieldname = 'APPEE'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 3.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  append gt_fcat.clear: gt_fcat.

  " First Name
  gt_fcat-fieldname = 'VORNA'.
  gt_fcat-coltext = text-t05.
  gt_fcat-col_pos = 4.
  gt_fcat-outputlen = 20.
  append gt_fcat.clear: gt_fcat.

  " Last Name
  gt_fcat-fieldname = 'NACHN'.
  gt_fcat-coltext = text-t06.
  gt_fcat-col_pos = 5.
  gt_fcat-outputlen = 20.
  append gt_fcat.clear: gt_fcat.

  " Classification
  gt_fcat-fieldname = 'CLFTX'.
  gt_fcat-coltext = text-t07.
  gt_fcat-col_pos = 6.
  gt_fcat-outputlen = 25.
  append gt_fcat.clear: gt_fcat.

  " Grade
  gt_fcat-fieldname = 'GRADE'.
  gt_fcat-coltext = text-t08.
  gt_fcat-col_pos = 7.
  gt_fcat-outputlen = 3.
  append gt_fcat.clear: gt_fcat.

  " Supervisor ID
  gt_fcat-fieldname = 'APPER'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 8.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  append gt_fcat.clear: gt_fcat.

  " Supervisor Name
  gt_fcat-fieldname = 'APPERNM'.
  gt_fcat-coltext = text-t10.
  gt_fcat-col_pos = 9.
  gt_fcat-outputlen = 30.
  append gt_fcat.clear: gt_fcat.

  " Hiring Date
  gt_fcat-fieldname = 'HIRDA'.
  gt_fcat-coltext = text-t11.
  gt_fcat-col_pos = 10.
  gt_fcat-outputlen = 10.
  append gt_fcat.clear: gt_fcat.

  " Appraisal Year
  gt_fcat-fieldname = 'ZYEAR'.
  gt_fcat-coltext = text-t12.
  gt_fcat-col_pos = 11.
  gt_fcat-outputlen = 4.
  append gt_fcat.clear: gt_fcat.

  " Duration(From)
  gt_fcat-fieldname = 'STRDA'.
  gt_fcat-coltext = text-t13.
  gt_fcat-col_pos = 12.
  gt_fcat-outputlen = 10.
  append gt_fcat.clear: gt_fcat.

  " Duration(To)
  gt_fcat-fieldname = 'ENDDA'.
  gt_fcat-coltext = text-t14.
  gt_fcat-col_pos = 13.
  gt_fcat-outputlen = 10.
  append gt_fcat.clear: gt_fcat.

  " Annual Evaluation
  gt_fcat-fieldname = 'ANNEV'.
  gt_fcat-coltext = text-t15.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

  " Rating
  gt_fcat-fieldname = 'ANNRT'.
  gt_fcat-coltext = text-t16.
  gt_fcat-col_pos = 15.
  gt_fcat-outputlen = 4.
  gt_fcat-just = 'R'.
  append gt_fcat.clear: gt_fcat.

  " Performance
  gt_fcat-fieldname = 'PFMEV'.
  gt_fcat-coltext = text-t17.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

  " Core Competency
  gt_fcat-fieldname = 'COREV'.
  gt_fcat-coltext = text-t18.
  gt_fcat-col_pos = 17.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

  " Leadership Competency
  gt_fcat-fieldname = 'LDSEV'.
  gt_fcat-coltext = text-t19.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

  " Job Competency
  gt_fcat-fieldname = 'JOBEV'.
  gt_fcat-coltext = text-t20.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

endform.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_init_value .

  if p_year is initial.
    p_year = sy-datum(4).
  endif.

endform.                    " SET_INIT_VALUE
