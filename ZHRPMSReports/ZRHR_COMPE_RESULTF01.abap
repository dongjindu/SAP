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

  data: l_year        type zdhr_year.

  g_fieldname = 'P_YEAR'.
  l_year = sy-datum(4).

  clear gt_values.
  do 10 times.
    gs_value-key = l_year.
    gs_value-text = l_year.
    append gs_value to gt_values.clear gs_value.
    l_year = sy-datum(4) - sy-index.
  enddo.

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

endform.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_RATING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_droplist_rating .

  data: l_gsval   type gsval.

  g_fieldname = 'P_RATING'.

  " get scale id
  clear l_gsval.
  select single gsval from t77s0
    into l_gsval
    where grpid = 'ZPMS'
      and semid = 'RATNG'.

  " get rating
  clear gt_values.
  select rating as key
         pstext as text
    from t77tp
    into table gt_values
    where scale_id = l_gsval.
  sort gt_values by key descending.

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

endform.                    " SET_DROPLIST_RATING
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_COMPG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_droplist_compg .

  data: lt_t77s0    type table of t77s0 with header line,
        lt_hrp1001  type table of hrp1001 with header line.

  data: rt_objid    type range of hrobjid with header line.

  g_fieldname = 'P_COMPG'.

  " get Core, Job, Leadership Root Competency
  clear lt_t77s0[].
  select * from t77s0
    into table lt_t77s0
   where grpid = 'ZPMS'
     and semid in ('RTCOR','RTJOB').

  clear rt_objid[].
  loop at lt_t77s0.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_t77s0-gsval.
    append rt_objid.clear rt_objid.
  endloop.

  " get Competency Group
  clear gt_values.
  select stext as key
*  SELECT objid AS key
         stext as text
    from hrp1000
    into table gt_values
   where plvar = '01'
     and otype = 'QK'
     and objid in rt_objid
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and langu = sy-langu.


*** #Leadership Competency for G5# and #Leadership Competency for GL, G3, G4#
  clear lt_t77s0[].
  select * from t77s0
    into table lt_t77s0
   where grpid = 'ZPMS'
     and semid = 'RTLDS'.

  clear rt_objid[].
  loop at lt_t77s0.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_t77s0-gsval.
    append rt_objid.clear rt_objid.
  endloop.


  if sy-subrc eq 0.

    clear lt_hrp1001[].
    select * from hrp1001
      into table lt_hrp1001
     where otype = 'QK'
       and objid in rt_objid
       and plvar = '01'
       and rsign = 'B'
       and relat = '030'
       and istat = '1'
       and begda <= sy-datum
       and endda >= sy-datum.

    clear rt_objid[].
    loop at lt_hrp1001.
      rt_objid-sign = 'I'.
      rt_objid-option = 'EQ'.
      rt_objid-low = lt_hrp1001-sobid.
      append rt_objid.clear rt_objid.
    endloop.

    select stext as key
           stext as text
      from hrp1000
      appending corresponding fields of table gt_values
     where plvar = '01'
       and otype = 'QK'
       and objid in rt_objid
       and istat = '1'
       and begda <= sy-datum
       and endda >= sy-datum
       and langu = sy-langu.
  endif.
*************

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

endform.                    " SET_DROPLIST_COMPG
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_COMPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_droplist_compt .

  data: lt_t77s0    type table of t77s0 with header line,
        lt_hrp1001  type table of hrp1001 with header line.

  data: rt_objid    type range of hrobjid with header line.
  data: l_objid     type objid.

  g_fieldname = 'P_COMPT'.
  clear gt_values.

  clear p_compt.
  clear p_beatt.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_COMPT'
      values = gt_values.
  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_BEATT'
      values = gt_values.

  if p_compg is initial.
    exit.
  endif.

  select single objid
    into l_objid
    from hrp1000
   where plvar = '01'
     and otype = 'QK'
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and langu = sy-langu
     and stext = p_compg.

  " get child
  clear lt_hrp1001[].
  select * from hrp1001
    into table lt_hrp1001
   where otype = 'QK'
     and objid = l_objid
     and plvar = '01'
     and rsign = 'B'
     and relat = '030'
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum.

  clear rt_objid[].
  loop at lt_hrp1001.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_hrp1001-sobid.
    append rt_objid.clear rt_objid.
  endloop.

  select stext as key
*  SELECT objid AS key
         stext as text
    from hrp1000
    into table gt_values
   where plvar = '01'
     and otype = 'Q'
     and objid in rt_objid
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and langu = sy-langu.

  sort gt_values by text.
  delete adjacent duplicates from gt_values comparing text.
  sort gt_values by key.

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

endform.                    " SET_DROPLIST_COMPT
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_BEATT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_droplist_beatt.

  data: lt_t77s0    type table of t77s0 with header line,
        lt_hrp1001  type table of hrp1001 with header line.

  data: rt_objid    type range of hrobjid with header line.
  data: l_objid     type objid.

  g_fieldname = 'P_BEATT'.

  clear gt_values.
  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_BEATT'
      values = gt_values.

  if p_compt is initial.
    exit.
  endif.


  select single objid
    into l_objid
    from hrp1000
   where plvar = '01'
     and otype = 'QK'
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and langu = sy-langu
     and stext = p_compg.

  " get child
  clear lt_hrp1001[].
  select * from hrp1001
    into table lt_hrp1001
   where otype = 'QK'
     and objid = l_objid
     and plvar = '01'
     and rsign = 'B'
     and relat = '030'
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum.

  clear rt_objid[].
  loop at lt_hrp1001.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_hrp1001-sobid.
    append rt_objid.clear rt_objid.
  endloop.

  select single objid
    into l_objid
    from hrp1000
   where plvar = '01'
     and otype = 'Q'
     and objid in rt_objid
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and stext = p_compt.

  " get otype 10 elements
  clear lt_hrp1001[].
  select * from hrp1001
    into table lt_hrp1001
   where otype = 'Q'
     and objid = l_objid
     and plvar = '01'
     and rsign = 'B'
     and relat = '030'
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum.

  clear rt_objid[].
  loop at lt_hrp1001.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = lt_hrp1001-sobid.
    append rt_objid.clear rt_objid.
  endloop.

  " get Behavioral Attributes
  clear gt_values.
  select stext as key
         stext as text
    from hrp1000
    into table gt_values
   where plvar = '01'
     and otype = '10'
     and objid in rt_objid
     and istat = '1'
     and begda <= sy-datum
     and endda >= sy-datum
     and langu = sy-langu.

  sort gt_values by text.
  delete adjacent duplicates from gt_values comparing text.
  sort gt_values by key.

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

endform.                    " SET_DROPLIST_BEATTFORM set_droplist_compg .
*&---------------------------------------------------------------------*
*&      Form  set_droplist_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form set_droplist_status.

  g_fieldname = 'P_STAUS'.

  clear gt_values[].

  select domvalue_l as key ddtext as text
      into table gt_values
      from dd07t
     up to 5 rows
     where domname = 'HAP_AP_STATUS'
       and ddlanguage = sy-langu
       and as4local = 'A'
   order by domvalue_l.

  call function 'VRM_SET_VALUES'
    exporting
      id     = g_fieldname
      values = gt_values.

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

************************************************
*   get data
************************************************
  " get classification text
  clear lt_class[].
  select a~clfid
         a~jobid
         b~clftx
         b~zefrom
         b~zeto
    from zthr_clfaj as a
      inner join zthr_class as b
      on a~clfid = b~clfid
    into table lt_class.
** On 12/12/13 for YEAR check
  LOOP AT lt_class.
    IF p_year >= lt_class-zefrom+0(4)
       AND p_year <= lt_class-zeto+0(4).
    ELSE.
      DELETE lt_class.
    ENDIF.
  ENDLOOP.
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

*  CHECK lines( lt_p0000 ) > 0.
  if lt_p0000[] is initial.
    message s027 display like 'E'.
    exit.
  endif.
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

*  CHECK lines( lt_p0001 ) > 0.
  if lt_p0001[] is initial.
    message s027 display like 'E'.
    exit.
  endif.
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
*   07/22/2013 - T00306 Start
*  clear ls_sel_status.
*  ls_sel_status-ap_status_5 = 'X'.
  clear ls_sel_status.
  case p_staus.
    when '1'.
      ls_sel_status-ap_status_1 = 'X'.
    when '2'.
      ls_sel_status-ap_status_2 = 'X'.
    when '3'.
      ls_sel_status-ap_status_3 = 'X'.
    when '4'.
      ls_sel_status-ap_status_4 = 'X'.
    when '5'.
      ls_sel_status-ap_status_5 = 'X'.
    when '6'.
      ls_sel_status-ap_status_6 = 'X'.
    when '7'.
      ls_sel_status-ap_status_7 = 'X'.
    when '8'.
      ls_sel_status-ap_status_8 = 'X'.
    when '9'.
      ls_sel_status-ap_status_9 = 'X'.
  endcase.
*   07/22/2013 - T00306 End


  " set appraisal display
  clear ls_sel_with_or_witout.
  ls_sel_with_or_witout-sel_display_existing = 'X'.

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

*   07/22/2013 - T00306 Start
    gs_result-statx = ls_documents-ap_status_name.
*   07/22/2013 - T00306 End

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

    clear: lt_p0001, lt_appraisees, ls_appraisees, ls_return,
           lt_documents, ls_documents, gs_result.
  endloop.

************************************************
*   filtering
************************************************
  " Competency Group
  if p_compg is not initial.
    delete gt_result where compg <> p_compg.
*    DELETE gt_result WHERE cgeid <> p_compg.
  endif.
  " Competency
  if p_compt is not initial.
    delete gt_result where compt <> p_compt.
*    DELETE gt_result WHERE cteid <> p_compt.
  endif.
  " Behavioral Attribute
  if p_beatt is not initial.
    delete gt_result where beatt <> p_beatt.
*    DELETE gt_result WHERE baeid <> p_beatt.
  endif.
  " Rating
  if p_rating is not initial.
    delete gt_result where comps <> p_rating.
  endif.

  if gt_result[] is initial.
    message s027 display like 'E'.
    exit.
  endif.

  sort gt_result by appee compg compt.
  data: l_tabix type stabix.
  loop at gt_result into gs_result.
    l_tabix = sy-tabix.
    at new compt.
      continue.
    endat.
    clear gs_result-compr.
    modify gt_result from gs_result index l_tabix.
  endloop.

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

  data: lt_body_elements        type hap_t_body_elements,
        lt_body_cells           type hap_t_body_cells,
        lt_hrp1001              type table of hrp1001 with header line,
        lt_hrp9871              type table of hrp9871 with header line.

  data: ls_appraisal_id         type hap_s_appraisal_id,
        ls_body_elements        type hap_s_body_elements,
        ls_body_elements_child  type hap_s_body_elements,
        ls_body_cells           type hap_s_body_cells,
        ls_return               type bal_s_msg.

  data: rt_objid                type range of hrobjid with header line.

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

  " get Core, Leadership, Job Competency VB elements
  clear lt_hrp9871[].
  select * from hrp9871
    into table lt_hrp9871
    where plvar = '01'
      and otype = 'VB'
      and objid in rt_objid
      and istat in ('3','4')
      and begda <= sy-datum
      and endda >= sy-datum
      and igid in (2,3,4).
  sort lt_hrp9871 by igid.

  loop at lt_hrp9871.
    " read vb element
    read table lt_body_elements into ls_body_elements
                                with key element_id = lt_hrp9871-objid.
    if sy-subrc = 0.
      " set Competency Group Row ID
      gs_result-cgrid = ls_body_elements-row_iid.
      " set Competency Group Element ID
      gs_result-cgeid = ls_body_elements-element_id.
*      gs_result-cgeid = ls_body_elements-foreign_id.
      " set Competency Group
      gs_result-compg = ls_body_elements-name.
      " read child(VB element)
      read table lt_body_elements into ls_body_elements
                                  with key row_iid = ls_body_elements-child
                                  binary search.
      if sy-subrc = 0.
        while sy-subrc = 0.
          "set Competency Row ID
          gs_result-ctrid = ls_body_elements-row_iid.
          " set Competency Element ID
          gs_result-cteid = ls_body_elements-element_id.
*          gs_result-cteid = ls_body_elements-foreign_id.
          " set Competency
          gs_result-compt = ls_body_elements-name.
          " read cell Self Appraisal(YE)
* 07/25/2013 - T00306 Start
          if p_staus eq 3.
            read table lt_body_cells into ls_body_cells
                                     with key row_iid = ls_body_elements-row_iid
                                              column_iid = 5
                                     binary search.
          else.
            read table lt_body_cells into ls_body_cells
                                     with key row_iid = ls_body_elements-row_iid
                                              column_iid = c_column_iid_zp14
                                     binary search.
          endif.
* 07/25/2013 - T00306 End
          if sy-subrc = 0.
            " set Competency Rating Score
            gs_result-comps = ls_body_cells-value_txt.
            " set Competency Rating
            gs_result-compr = ls_body_cells-value_text.
          endif.

          " read child child(VC Element)
          read table lt_body_elements into ls_body_elements_child
                                      with key row_iid = ls_body_elements-child
                                      binary search.
          if sy-subrc = 0.
            while sy-subrc = 0.
              " set BA Row ID
              gs_result-barid = ls_body_elements_child-row_iid.
              " set BA Element ID
              gs_result-baeid = ls_body_elements_child-element_id.
*              gs_result-baeid = ls_body_elements_child-foreign_id.
              " set BA
              gs_result-beatt = ls_body_elements_child-name.
              " read cell Self Appraisal(YE)

* 07/25/2013 - T00306 Start
              if p_staus eq 3.
                read table lt_body_cells into ls_body_cells
                                         with key row_iid = ls_body_elements_child-row_iid
                                              column_iid = 5
                                         binary search.
              else.
                read table lt_body_cells into ls_body_cells
                                         with key row_iid = ls_body_elements_child-row_iid
                                              column_iid = c_column_iid_zp14
                                         binary search.
              endif.
* 07/25/2013 - T00306 End
              if sy-subrc = 0.
                gs_result-beatr = ls_body_cells-value_text.
              endif.

              append gs_result to gt_result.

              " read child child brother
              read table lt_body_elements into ls_body_elements_child
                                          with key row_iid = ls_body_elements_child-brother
                                          binary search.
            endwhile.
*** 07/24/2013 - T00306 Start
          else.
            append gs_result to gt_result.
          endif.
*** 07/24/2013 - T00306 End
          " read child brother
          read table lt_body_elements into ls_body_elements
                                      with key row_iid = ls_body_elements-brother
                                      binary search.
        endwhile.
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

  data: lt_exclud type ui_functions.
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
    perform set_sort.

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
        it_sort                       = gt_sort[]
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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

  " Status
  gt_fcat-fieldname = 'STATX'.
  gt_fcat-coltext = text-t27.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 10.
  append gt_fcat.clear: gt_fcat.

  " Competency Group Row ID
  gt_fcat-fieldname = 'CGRID'.
  gt_fcat-coltext = text-t20.
  gt_fcat-col_pos = 15.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " Competency Group
  gt_fcat-fieldname = 'COMPG'.
  gt_fcat-coltext = text-t15.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 19.
  append gt_fcat.clear: gt_fcat.

  " Competency Row ID
  gt_fcat-fieldname = 'CTRID'.
  gt_fcat-coltext = text-t21.
  gt_fcat-col_pos = 17.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " Competency
  gt_fcat-fieldname = 'COMPT'.
  gt_fcat-coltext = text-t16.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 21.
  append gt_fcat.clear: gt_fcat.

  " Competency Rating
  gt_fcat-fieldname = 'COMPR'.
  gt_fcat-coltext = text-t17.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 17.
  append gt_fcat.clear: gt_fcat.

  " BA Row ID
  gt_fcat-fieldname = 'BARID'.
  gt_fcat-coltext = text-t22.
  gt_fcat-col_pos = 20.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " BA
  gt_fcat-fieldname = 'BEATT'.
  gt_fcat-coltext = text-t18.
  gt_fcat-col_pos = 21.
  gt_fcat-outputlen = 22.
  append gt_fcat.clear: gt_fcat.

  " BA Rating
  gt_fcat-fieldname = 'BEATR'.
  gt_fcat-coltext = text-t19.
  gt_fcat-col_pos = 22.
  gt_fcat-outputlen = 12.
  append gt_fcat.clear: gt_fcat.

  " Competency Group Element ID
  gt_fcat-fieldname = 'CGEID'.
  gt_fcat-coltext = text-t23.
  gt_fcat-col_pos = 23.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " Competency element id
  gt_fcat-fieldname = 'CTEID'.
  gt_fcat-coltext = text-t24.
  gt_fcat-col_pos = 24.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " Behavioral attribute element id
  gt_fcat-fieldname = 'BAEID'.
  gt_fcat-coltext = text-t25.
  gt_fcat-col_pos = 25.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

  " Competency Rating Score
  gt_fcat-fieldname = 'COMPS'.
  gt_fcat-coltext = text-t26.
  gt_fcat-col_pos = 26.
  gt_fcat-no_out = 'X'.
  append gt_fcat.clear: gt_fcat.

endform.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_sort .

  clear gt_sort[].
  gt_sort-spos      = '1'.
  gt_sort-fieldname = 'KOSTL'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '2'.
  gt_sort-fieldname = 'KTEXT'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '3'.
  gt_sort-fieldname = 'APPEE'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '4'.
  gt_sort-fieldname = 'VORNA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '5'.
  gt_sort-fieldname = 'NACHN'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '6'.
  gt_sort-fieldname = 'CLFTX'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '7'.
  gt_sort-fieldname = 'GRADE'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '8'.
  gt_sort-fieldname = 'APPER'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '9'.
  gt_sort-fieldname = 'APPERNM'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '10'.
  gt_sort-fieldname = 'HIRDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '11'.
  gt_sort-fieldname = 'ZYEAR'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '12'.
  gt_sort-fieldname = 'STRDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '13'.
  gt_sort-fieldname = 'ENDDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '14'.
  gt_sort-fieldname = 'STATX'.
  gt_sort-up        = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '15'.
  gt_sort-fieldname = 'CGRID'.
  gt_sort-up        = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '16'.
  gt_sort-fieldname = 'COMPG'.
  gt_sort-up        = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '17'.
  gt_sort-fieldname = 'CTRID'.
  gt_sort-up        = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '18'.
  gt_sort-fieldname = 'COMPT'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '19'.
  gt_sort-fieldname = 'COMPR'.
  gt_sort-down      = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  clear gt_sort-level.
  gt_sort-spos      = '20'.
  gt_sort-fieldname = 'BARID'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

  gt_sort-spos      = '21'.
  gt_sort-fieldname = 'BEATT'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  append gt_sort.clear  gt_sort.

endform.                    " SET_SORT
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
