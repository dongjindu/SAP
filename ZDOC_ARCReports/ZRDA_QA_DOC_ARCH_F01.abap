*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCH_F01
*&---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       FORM GET_alcvin_DATA                                             *
*---------------------------------------------------------------------*
*       Make input data value                                                      *
*---------------------------------------------------------------------*
FORM get_alcvin_data.

  CLEAR: is_ztda_posts.
  CLEAR: g_post_id, is_key.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZDOC_ARC01'
    IMPORTING
      number                  = g_post_id
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* Creat ZTDA_POSTS table's data
  is_ztda_posts-post_id      = g_post_id.
  is_ztda_posts-post_author  = sy-uname.
  is_ztda_posts-post_date    = g_date.
  is_ztda_posts-post_date_tm = g_date_tm.
  is_ztda_posts-post_title   = is_buyback-acl_no.
  is_ztda_posts-post_title_c = is_buyback-acl_no.
  is_ztda_posts-post_desc    = is_buyback-title.
  is_ztda_posts-post_desc_c  = is_buyback-title.


* Contract
  IF g_proc = 'V'.
    is_ztda_posts-post_title   = g_lifnr.
    is_ztda_posts-post_title_c = g_lifnr.
    is_ztda_posts-post_desc    = g_title.
    is_ztda_posts-post_desc_c  = g_title.
    is_ztda_posts-post_type  = 'V'.    "Contract
  ENDIF.

  TRANSLATE is_ztda_posts-post_title_c TO UPPER CASE.
  CONDENSE  is_ztda_posts-post_title_c NO-GAPS.
  TRANSLATE is_ztda_posts-post_desc_c  TO UPPER CASE.
  CONDENSE  is_ztda_posts-post_desc_c  NO-GAPS.
  is_ztda_posts-post_status  = 'A'.
  is_ztda_posts-post_flag    = g_af.

* 20140531 change start
  IF g_proc = 'V'.
    is_ztda_posts-post_flag = gv_sp. "'4'.   "20140602 CHANGE
***    CASE 'X'.
***      WHEN cb1.  is_ztda_posts-post_flag = '1'.
***      WHEN cb2.  is_ztda_posts-post_flag = '2'.
***      WHEN cb3.  is_ztda_posts-post_flag = '3'.
***      WHEN cb4.  is_ztda_posts-post_flag = '4'.
***    ENDCASE.
  ELSE.
    IF g_af = '1' AND g_av = '1'.
      is_ztda_posts-post_flag    = '3'.
    ELSEIF g_af = ' ' AND g_av = '1'.
      is_ztda_posts-post_flag    = '2'.
    ELSEIF g_af = '1' AND g_av = ' '.
      is_ztda_posts-post_flag    = '1'.
    ELSE.
      is_ztda_posts-post_flag    = ' '.
    ENDIF.
  ENDIF.   "20140531 change end
  CLEAR: g_af, g_av.

* Creat ZTDA_POSTMETA table's data
  CLEAR: it_ztda_postmeta, it_ztda_postmeta[].

* Create ztda_postmeta table's data
* Creat meta_id
  g_postid_o       = g_post_id.
  g_title_o        = is_ztda_posts-post_desc.
  PERFORM meta_id_creat USING g_meta_id.

  it_ztda_postmeta-post_id       = g_post_id.
  it_ztda_postmeta-meta_id       = g_meta_id + 1.

  CLEAR: it_ztda_postmeta-meta_key.   "20140602 Add
  IF is_buyback-aclonly IS INITIAL.
* 20140516 Add s   20140602 REMARKING
*****    it_ztda_postmeta-meta_key      = gc_only.
*****    APPEND it_ztda_postmeta.
* 20140516 Add e
    it_ztda_postmeta-meta_key      = gc_vinno.
    it_ztda_postmeta-meta_value    = is_buyback-vin_no.
    it_ztda_postmeta-meta_value_c  = is_buyback-vin_no.
  ELSE.
    it_ztda_postmeta-meta_key      = gc_only.
  ENDIF.


* Contract
  IF g_proc = 'V'.
    it_ztda_postmeta-meta_value    = g_name1.
    it_ztda_postmeta-meta_value_c  = g_name1.
*   20140508 s Change Request Kang
**    it_ztda_postmeta-meta_desc     = g_title.
**    it_ztda_postmeta-meta_desc_c   = g_title.
*   20140508 e Change Request Kang
    CASE 'X'.
      WHEN cb1.
        it_ztda_postmeta-meta_key     = gc_agree.
      WHEN cb2.
        it_ztda_postmeta-meta_key     = gc_meeting.
      WHEN cb3.
        it_ztda_postmeta-meta_key     = gc_report.
      WHEN cb4.
        it_ztda_postmeta-meta_key     = gc_object.
    ENDCASE.
  ENDIF.
  IF it_ztda_postmeta-meta_value IS NOT INITIAL.
    TRANSLATE it_ztda_postmeta-meta_value_c  TO UPPER CASE.
    CONDENSE  it_ztda_postmeta-meta_value_c  NO-GAPS.
**    TRANSLATE it_ztda_postmeta-meta_desc_c   TO UPPER CASE.
**    CONDENSE  it_ztda_postmeta-meta_desc_c   NO-GAPS.
  ENDIF.
  APPEND it_ztda_postmeta.

ENDFORM.                    "GET_alcvin_DATA



*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.

**select max( meta_id ) into g_meta_id
**  from  ZTDA_POSTMETA
**  ORDER BY meta_id DESCENDING.
**
ENDFORM.                    " check_input_value



*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
*  LOOP AT SCREEN.
*    IF SCREEN-NAME = 'P_EXCEL'.
*      SCREEN-INPUT = 0.
*      SCREEN-INVISIBLE = 1.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " check_screen



*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.

  CLEAR: is_buyback.

ENDFORM.                    " INIT_DATA



*&---------------------------------------------------------------------*
*&      Form  ONLY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM only_check.

ENDFORM.                 " ONLY_CHECK



*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_BUYBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_buyback .

  DATA  : lv_pid           TYPE zepostid,
          lv_mid           TYPE zemetaid,
          lv_tid           TYPE zetaxonomyid,
          lv_cnt           TYPE i.

  PERFORM get_alcvin_data.

  IF is_ztda_posts IS NOT INITIAL.
    CLEAR ztda_posts.
    MOVE-CORRESPONDING is_ztda_posts TO ztda_posts.
    MODIFY ztda_posts.
    COMMIT WORK AND WAIT.
  ENDIF.

  lv_pid = ztda_posts-post_id.
  IF lv_pid IS INITIAL.
    lv_pid = g_postid_o.
  ENDIF.
  SELECT SINGLE taxonomy_id INTO lv_tid
    FROM ztda_term_rel
   WHERE post_id = lv_pid.

  PERFORM meta_id_creat USING lv_mid.
  lv_mid = lv_mid + 1.
  lv_tid = g_tid.
  PERFORM save_standard_data_process USING lv_pid lv_mid lv_tid.
  PERFORM ztda_term_rel_process      USING lv_pid lv_mid lv_tid.
  IF g_link IS NOT INITIAL.
    lv_tid = '00002'.
    PERFORM ztda_term_rel_process    USING lv_pid lv_mid lv_tid.
  ENDIF.

  IF it_ztda_postmeta[] IS NOT INITIAL.
    LOOP AT it_ztda_postmeta.
      lv_cnt = sy-tabix.
      CLEAR: g_meta_id, lv_pid, lv_mid, lv_tid.
      PERFORM meta_id_creat USING g_meta_id.
      it_ztda_postmeta-meta_id  = g_meta_id + 1.

      lv_pid = it_ztda_postmeta-post_id.
      MODIFY ztda_postmeta FROM it_ztda_postmeta.
      COMMIT WORK AND WAIT.
      lv_mid = it_ztda_postmeta-meta_id.
      lv_tid = g_tid.

    ENDLOOP.

  ENDIF.

  LEAVE PROGRAM.

ENDFORM.                    " SAVE_DATA_BUYBACK



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK
*&---------------------------------------------------------------------*
*       text : Input data check
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_data_check .

  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_vin_no   TYPE ze_vin,
          lv_category TYPE char10,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80).

* ACL Number check
  IF is_buyback-acl_no IS INITIAL.
    MESSAGE s999 WITH text-m01.     "Fill in all required entry fields
    g_flg = 'X'.
    g_fldname = 'IS_BUYBACK-ACL_NO'.
    EXIT.
  ENDIF.

  PERFORM acl_check USING lv_flag is_buyback-acl_no.
  IF lv_flag IS NOT INITIAL.
    MESSAGE s999 WITH text-m02      "ACL Number does not exist !
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'IS_BUYBACK-ACL_NO'.
    EXIT.
  ENDIF.


*   Post title check
  CLEAR g_title.
  g_title = is_buyback-title.
***  IF is_buyback-title IS INITIAL.
***    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
***    g_flg = 'T'.
***    g_fldname = 'IS_BUYBACK-TITLE'.
***    EXIT.
***  ENDIF.

  CLEAR: g_postid_o, g_title_o.
  PERFORM aclno_search USING g_postid_o g_title_o.

  g_af = '1'.
  PERFORM qa_acl_no_check USING g_af  is_buyback-acl_no.




  CHECK is_buyback-aclonly IS INITIAL.

* VIN Number check
  IF is_buyback-vin_no IS INITIAL.
    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
    g_flg = 'Y'.
    g_fldname = 'IS_BUYBACK-VIN_NO'.
    EXIT.
  ENDIF.

  PERFORM vin_check USING lv_flag is_buyback-vin_no.
  IF lv_flag IS NOT INITIAL.
    MESSAGE s999 WITH text-m03    "VIN Number does not exist !
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'IS_BUYBACK-VIN_NO'.
    EXIT.
  ENDIF.

* 20140512 Add
  g_av = '1'.
  PERFORM qa_vin_no_check USING g_av  is_buyback-vin_no.

***** 20140512 moved up
******   Post title check
*****  CLEAR g_title.
*****  g_title = is_buyback-title.
********  IF is_buyback-title IS INITIAL.
********    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
********    g_flg = 'T'.
********    g_fldname = 'IS_BUYBACK-TITLE'.
********    EXIT.
********  ENDIF.
*****
*****  CLEAR: g_postid_o, g_title_o.
*****  PERFORM aclno_search USING g_postid_o g_title_o.
*****
*****  g_af = 'A'.
*****  PERFORM qa_acl_no_check USING g_af  is_buyback-acl_no.

ENDFORM.                    " INPUT_DATA_CHECK



*&---------------------------------------------------------------------*
*&      Form  META_ID_CREAT
*&---------------------------------------------------------------------*
*       text: Creat meta_id
*----------------------------------------------------------------------*
*      -->P_G_META_ID  text
*----------------------------------------------------------------------*
FORM meta_id_creat  USING    p_meta_id TYPE zemetaid.

  CLEAR p_meta_id.
  CALL FUNCTION 'ENQUEUE_EZ_LOCK_METAID'
    EXPORTING
      mandt = sy-mandt.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*
  SELECT MAX( meta_id ) INTO p_meta_id
  FROM  ztda_postmeta
  ORDER BY meta_id DESCENDING.

  CALL FUNCTION 'DEQUEUE_EZ_LOCK_METAID'
    EXPORTING
      mandt = sy-mandt.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF p_meta_id = '999'.
    CLEAR p_meta_id.
  ENDIF.

ENDFORM.                    " META_ID_CREAT



*&---------------------------------------------------------------------*
*&      Form  ACL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_IS_BUYBACK_ACL_NO  text
*----------------------------------------------------------------------*
FORM acl_check  USING    p_flag
                         p_acl_no.
  CLEAR: ztsd_acm_h, p_flag.
  TRANSLATE p_acl_no TO UPPER CASE.
  SELECT SINGLE zacln INTO ztsd_acm_h-zacln
    FROM ztsd_acm_h
   WHERE zacln = p_acl_no.

  IF sy-subrc NE 0.
    p_flag = 'N'.
  ENDIF.

ENDFORM.                    " ACL_CHECK



*&---------------------------------------------------------------------*
*&      Form  VIN_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_IS_BUYBACK_VIN_NO  text
*----------------------------------------------------------------------*
FORM vin_check  USING    p_flag
                         p_vin_no.
  CLEAR: ztpp_vm, p_flag.
  TRANSLATE p_vin_no TO UPPER CASE.
  SELECT SINGLE vin INTO ztpp_vm-vin
    FROM ztpp_vm
   WHERE vin = p_vin_no.

  IF sy-subrc NE 0.
    p_flag = 'N'.
  ENDIF.

ENDFORM.                    " VIN_CHECK



*&---------------------------------------------------------------------*
*&      Form  ACLNO_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_POSTID_O  text
*      -->P_G_TITLE_O  text
*----------------------------------------------------------------------*
FORM aclno_search  USING    p_postid_o
                            p_title_o.
  DATA  : lv_aclno TYPE zdl_post_title.   "
  lv_aclno = is_buyback-acl_no.
  TRANSLATE lv_aclno TO UPPER CASE.
  SELECT SINGLE a~post_id a~post_desc INTO (p_postid_o, p_title_o)
    FROM ztda_posts AS a
   WHERE a~post_title_c  = lv_aclno.
***    FROM ztda_posts AS a INNER JOIN ztda_term_rel AS b
***                         ON    a~post_id = b~post_id
***   WHERE a~post_title  = lv_aclno
***     AND b~taxonomy_id = g_tid.

ENDFORM.                    " ACLNO_SEARCH



*&---------------------------------------------------------------------*
*&      Form  SAVE_STANDARD_DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_standard_data_process  USING p_pid p_mid p_tid.

  CLEAR : g_object_id.

  GET PARAMETER ID 'OAL' FIELD last_document.
  g_object_id = p_pid.
  CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
    EXPORTING
      archiv_id             = last_document-archiv_id
      arc_doc_id            = last_document-arc_doc_id
      ar_object             = 'ZDA_QA01'
      object_id             = g_object_id
      sap_object            = 'ZDA_QA01'
      doc_type              = last_document-doc_type
    EXCEPTIONS
      error_connectiontable = 1.

ENDFORM.                    " SAVE_STANDARD_DATA_PROCESS



*&---------------------------------------------------------------------*
*&      Form  INPUT_LEGAL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_legal_check .

  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_vin_no   TYPE ze_vin,
          lv_category TYPE char10,
          lv_lname    TYPE zedalname,
          lv_fname    TYPE zedafname,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80).

* ACL Number check
  IF is_legal-acl_no IS INITIAL.
    MESSAGE s999 WITH text-m01.     "Fill in all required entry fields
    g_flg = 'X'.
    g_fldname = 'IS_BUYBACK-ACL_NO'.
    EXIT.
  ENDIF.
  lv_flag = 'A'.
  PERFORM acl_check USING lv_flag is_legal-acl_no.
  IF lv_flag IS NOT INITIAL.
    MESSAGE s999 WITH text-m02      "ACL Number does not exist !
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'IS_BUYBACK-ACL_NO'.
    EXIT.
  ENDIF.

*** Moved 20140512 s
  CLEAR: g_postid_o, g_title_o.
  PERFORM aclno_search    USING g_postid_o g_title_o.

  g_af = '1'.
  PERFORM qa_acl_no_check USING g_af  is_buyback-acl_no.
*** 20140512 moved e

  CHECK is_buyback-aclonly IS INITIAL.

* Customer name check
  IF is_legal-vin_no IS INITIAL.
    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
    g_flg = 'Y'.
    g_fldname = 'IS_BUYBACK-VIN_NO'.
    EXIT.
    lv_flag = 'V'.
    PERFORM vin_check USING lv_flag is_legal-vin_no.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m03    "VIN Number does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-VIN_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* 20140512 Add
  g_av = '1'.
  PERFORM qa_vin_no_check USING g_av  is_buyback-vin_no.

* 20140516 Request Harry s : Require marking
*  IF g_lname IS INITIAL.
*    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
*    g_flg = 'L'.
*    g_fldname = 'G_LNAME'.
*    EXIT.
*  ENDIF.
*  IF g_fname IS INITIAL.
*    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
*    g_flg = 'F'.
*    g_fldname = 'G_FNAME'.
*    EXIT.
*  ENDIF.
* 20140516 Request Harry e


***  20140512 moved up
****   Post title check
******  IF g_title IS INITIAL.
******    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
******    g_flg = 'T'.
******    g_fldname = 'G_TITLE'.
******    EXIT.
******  ENDIF.
***
***  CLEAR: g_postid_o, g_title_o.
***  PERFORM aclno_search    USING g_postid_o g_title_o.
******  g_af = '2'.
***  PERFORM qa_acl_no_check USING g_af  is_buyback-acl_no.

ENDFORM.                    " INPUT_LEGAL_CHECK



*&---------------------------------------------------------------------*
*&      Form  ACLNO_SEARCH_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_POSTID_O  text
*      -->P_G_TITLE_O  text
*----------------------------------------------------------------------*
FORM aclno_search_l  USING    p_postid_o
                              p_title_o.
  DATA  : lv_aclno TYPE zdl_post_title.   "
  lv_aclno = is_buyback-acl_no.
  SELECT SINGLE a~post_id a~post_desc INTO (p_postid_o, p_title_o)
    FROM ztda_posts AS a
   WHERE a~post_title = lv_aclno.


ENDFORM.                    " ACLNO_SEARCH_L



*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_LEGAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_legal .

  DATA  : lv_pid           TYPE zepostid,
          lv_mid           TYPE zemetaid,
          lv_tid           TYPE zetaxonomyid,
          lv_flag          TYPE c.

  PERFORM get_alcvin_data_legal.

  CLEAR ztda_posts.
  MOVE-CORRESPONDING is_ztda_posts TO ztda_posts.
  MODIFY ztda_posts.
  COMMIT WORK AND WAIT.

  lv_pid = ztda_posts-post_id.
  IF lv_pid IS INITIAL.
    lv_pid = g_postid_o.
  ENDIF.
  SELECT SINGLE taxonomy_id INTO lv_tid
    FROM ztda_term_rel
   WHERE post_id = lv_pid.

  PERFORM meta_id_creat USING lv_mid.
  lv_mid = lv_mid + 1.
  lv_tid = g_tid.
  PERFORM save_standard_data_process USING lv_pid lv_mid lv_tid.

  PERFORM ztda_term_rel_process      USING lv_pid lv_mid lv_tid.
  IF g_link IS NOT INITIAL.
    lv_tid = '00001'.
    PERFORM ztda_term_rel_process    USING lv_pid lv_mid lv_tid.
  ENDIF.

  IF is_ztda_meta_name IS NOT INITIAL.
    CLEAR ztda_meta_name.
    MOVE-CORRESPONDING is_ztda_meta_name TO ztda_meta_name.
    MODIFY ztda_meta_name.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF it_ztda_postmeta[] IS NOT INITIAL.
    LOOP AT it_ztda_postmeta.
      CLEAR: g_meta_id, lv_pid, lv_mid, lv_tid.
      PERFORM meta_id_creat USING g_meta_id.
      it_ztda_postmeta-meta_id  = g_meta_id + 1.

      lv_pid = it_ztda_postmeta-post_id.
      MODIFY ztda_postmeta FROM it_ztda_postmeta.
      COMMIT WORK AND WAIT.
      lv_mid = it_ztda_postmeta-meta_id.
      lv_tid = g_tid.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " SAVE_DATA_LEGAL



*&---------------------------------------------------------------------*
*&      Form  GET_ALCVIN_DATA_LEGAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alcvin_data_legal .

  CLEAR: is_ztda_posts.
  CLEAR: g_post_id, is_key.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZDOC_ARC01'
    IMPORTING
      number                  = g_post_id
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* Creat ZTDA_POSTS table's data
  is_ztda_posts-post_id      = g_post_id.
  is_ztda_posts-post_author  = sy-uname.
  is_ztda_posts-post_date    = g_date.
  is_ztda_posts-post_date_tm = g_date_tm.
  is_ztda_posts-post_title   = is_buyback-acl_no.
  is_ztda_posts-post_title_c = is_buyback-acl_no.
  TRANSLATE is_ztda_posts-post_title_c TO UPPER CASE.
  CONDENSE is_ztda_posts-post_title_c NO-GAPS.
  is_ztda_posts-post_desc    = g_title.
  is_ztda_posts-post_desc_c  = g_title.
  TRANSLATE is_ztda_posts-post_desc_c TO UPPER CASE.
  CONDENSE  is_ztda_posts-post_desc_c NO-GAPS.
  is_ztda_posts-post_status  = 'A'.
  is_ztda_posts-post_flag    = g_af.
* 20140512 Add s
  IF g_af = '1' AND g_av = '1'.
    is_ztda_posts-post_flag    = '3'.
  ELSEIF g_af = ' ' AND g_av = '1'.
    is_ztda_posts-post_flag    = '2'.
  ELSEIF g_af = '1' AND g_av = ' '.
    is_ztda_posts-post_flag    = '1'.
  ELSE.
    is_ztda_posts-post_flag    = ' '.
  ENDIF.
* 20140512 Add e
  CLEAR: g_af, g_av.

* Creat ZTDA_POSTMETA table's data
  CLEAR: it_ztda_postmeta, it_ztda_postmeta[].

* Create ztda_postmeta table's data
* Creat meta_id
  g_postid_o       = g_post_id.
  g_title_o        = is_ztda_posts-post_desc.
  PERFORM meta_id_creat USING g_meta_id.

  it_ztda_postmeta-post_id       = g_post_id.
  it_ztda_postmeta-meta_id       = g_meta_id + 1.

  CLEAR: is_ztda_meta_name.
  IF is_buyback-aclonly IS INITIAL.
* 20140516 Add s   20140602 REMARKING
***    it_ztda_postmeta-meta_key      = gc_only.
***    APPEND it_ztda_postmeta.
* 20140516 Add e
    it_ztda_postmeta-meta_key      = gc_vinno.
    it_ztda_postmeta-meta_value    = is_buyback-vin_no.
    it_ztda_postmeta-meta_value_c  = is_buyback-vin_no.
    TRANSLATE it_ztda_postmeta-meta_value_c TO UPPER CASE.
    CONDENSE  it_ztda_postmeta-meta_value_c NO-GAPS.

* Create ztda_meta_name table's data
    PERFORM meta_id_creat USING g_meta_id.
    is_ztda_meta_name-post_id       = g_post_id.
    is_ztda_meta_name-last_name     = g_lname.
    is_ztda_meta_name-first_name    = g_fname.
    is_ztda_meta_name-last_name_c   = g_lname.
    is_ztda_meta_name-first_name_c  = g_fname.
    is_ztda_meta_name-post_status   = 'A'.

    TRANSLATE is_ztda_meta_name-last_name_c  TO UPPER CASE.
    CONDENSE  is_ztda_meta_name-last_name_c  NO-GAPS.
    TRANSLATE is_ztda_meta_name-first_name_c TO UPPER CASE.
    CONDENSE  is_ztda_meta_name-first_name_c NO-GAPS.
    is_ztda_meta_name-meta_flag     = '5'.      "20140508 Add
* 20140512 Add s
    PERFORM name_check USING is_ztda_meta_name-meta_flag.
* 20140512 Add e
  ELSE.
    it_ztda_postmeta-meta_key      = gc_only.
  ENDIF.

  APPEND it_ztda_postmeta.



ENDFORM.                    " GET_ALCVIN_DATA_LEGAL



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_CLAIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_data_check_claim .

  DATA  : lv_date    TYPE sydatum,               "Return value: N-Nothing,
          lv_msg(80).

* yearmonth check
  CONCATENATE ztda_posts-post_month '01' INTO lv_date.
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lv_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE s999 WITH text-m05 DISPLAY LIKE 'E'.
    g_flg = 'e'.
    g_fldname = 'ZTDA_POSTS-POST_MONTH'.
    EXIT.
  ENDIF.

*   Post title check
***  IF g_title IS INITIAL.
***    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
***    g_flg = 'T'.
***    g_fldname = 'G_TITLE'.
***    EXIT.
***  ENDIF.

  CASE g_proc.
    WHEN 'C'.
      g_af = '3'.
    WHEN 'R'.
      g_af = '4'.
  ENDCASE.
  PERFORM qa_datemonth_check USING g_af.

ENDFORM.                    " INPUT_DATA_CHECK_CLAIM



*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_CLAIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_claim .

  DATA  : lv_pid           TYPE zepostid,
          lv_mid           TYPE zemetaid,
          lv_tid           TYPE zetaxonomyid.

  PERFORM get_alcvin_data_claim.

  CLEAR ztda_posts.
  MOVE-CORRESPONDING is_ztda_posts TO ztda_posts.
  MODIFY ztda_posts.
  COMMIT WORK AND WAIT.

  lv_pid = ztda_posts-post_id.
  IF lv_pid IS INITIAL.
    lv_pid = g_postid_o.
  ENDIF.

  lv_mid = 1.
  CASE g_proc.
    WHEN 'C'.
      lv_tid = '00003'.
    WHEN 'R'.
      lv_tid = '00004'.
  ENDCASE.
  PERFORM save_standard_data_process USING lv_pid lv_mid lv_tid.
  PERFORM ztda_term_rel_process      USING lv_pid lv_mid lv_tid.

  IF is_ztda_meta_name IS NOT INITIAL.
    CLEAR ztda_meta_name.
    MOVE-CORRESPONDING is_ztda_meta_name TO ztda_meta_name.
    MODIFY ztda_meta_name.
    COMMIT WORK AND WAIT.
  ENDIF.

  LOOP AT it_ztda_postmeta.
    CLEAR: g_meta_id, lv_pid, lv_mid, lv_tid.
    PERFORM meta_id_creat USING g_meta_id.
    it_ztda_postmeta-meta_id  = g_meta_id + 1.

    lv_pid = it_ztda_postmeta-post_id.
    MODIFY ztda_postmeta FROM it_ztda_postmeta.
    COMMIT WORK AND WAIT.
    lv_mid = it_ztda_postmeta-meta_id.
    lv_tid = g_tid.

  ENDLOOP.

  LEAVE PROGRAM.

ENDFORM.                    " SAVE_DATA_CLAIM



*&---------------------------------------------------------------------*
*&      Form  GET_ALCVIN_DATA_CLAIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alcvin_data_claim .

  CLEAR: g_post_id, is_key.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZDOC_ARC01'
    IMPORTING
      number                  = g_post_id
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* Creat ZTDA_POSTS table's data
  is_ztda_posts-post_id      = g_post_id.
  is_ztda_posts-post_author  = sy-uname.
  is_ztda_posts-post_date    = g_date.
  is_ztda_posts-post_date_tm = g_date_tm.
  is_ztda_posts-post_status  = 'A'.
  is_ztda_posts-post_month   = ztda_posts-post_month.
  is_ztda_posts-post_title   = ztda_posts-post_month.
  is_ztda_posts-post_title_c = ztda_posts-post_month.
  is_ztda_posts-post_desc    = g_title.
  is_ztda_posts-post_desc_c  = g_title.
  TRANSLATE is_ztda_posts-post_desc_c TO UPPER CASE.
  CONDENSE  is_ztda_posts-post_desc_c NO-GAPS.
***  is_ztda_posts-post_flag    = g_af.   20140508
  is_ztda_posts-post_flag    = g_af.                        "20140531
* Creat ZTDA_POSTMETA table's data
  CLEAR: it_ztda_postmeta, it_ztda_postmeta[].

* Creat meta_id
  CLEAR: it_ztda_postmeta.
  PERFORM meta_id_creat USING g_meta_id.

  it_ztda_postmeta-post_id         = g_post_id.
  it_ztda_postmeta-meta_id         = g_meta_id + 1.
  it_ztda_postmeta-meta_key        = gc_date.
  it_ztda_postmeta-meta_value      = ztda_posts-post_month.
  it_ztda_postmeta-meta_value_c    = ztda_posts-post_month.
  TRANSLATE it_ztda_postmeta-meta_value_c TO UPPER CASE.
  CONDENSE  it_ztda_postmeta-meta_value_c NO-GAPS.

  APPEND it_ztda_postmeta.

ENDFORM.                    " GET_ALCVIN_DATA_CLAIM



*&---------------------------------------------------------------------*
*&      Form  ZTDA_TERM_REL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PID  text
*      -->P_LV_MID  text
*      -->P_LV_TID  text
*----------------------------------------------------------------------*
FORM ztda_term_rel_process  USING    p_pid
                                     p_mid
                                     p_tid.

  CLEAR ztda_term_rel.
  ztda_term_rel-post_id     = p_pid.
  ztda_term_rel-taxonomy_id = p_tid.
  MODIFY ztda_term_rel.

ENDFORM.                    " ZTDA_TERM_REL_PROCESS



*&---------------------------------------------------------------------*
*&      Form  QA_ACL_NO_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_category           text
*      -->P_IS_BUYBACK_ACL_NO  text
*----------------------------------------------------------------------*
FORM qa_acl_no_check  USING    p_af p_aclno.

  DATA: lv_aclno      TYPE zdl_post_title,  "ZDL_POST_TITLE
        lv_flag       TYPE c,
        lv_termid     TYPE zetermid,
        lv_postid     TYPE zepostid.
  DATA  : BEGIN OF lt_postid OCCURS 0,
          post_id       TYPE zepostid,
          END   OF lt_postid.

  lv_termid = g_tid.
  lv_aclno  = p_aclno.
  TRANSLATE lv_aclno TO UPPER CASE.
  SELECT SINGLE * FROM zvda_arch01
   WHERE post_status = 'A'
     AND post_title_c = lv_aclno
     AND term_id      = lv_termid.
  IF sy-subrc <> 0.
  ELSE.
    g_af = ' '.
  ENDIF.

ENDFORM.                    " QA_ACL_NO_CHECK



*&---------------------------------------------------------------------*
*&      Form  DUPLICATE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLAG  text
*----------------------------------------------------------------------*
FORM duplicate_check  USING    p_flag.

  DATA : lt_post     TYPE zsda_arch01 OCCURS 0 WITH HEADER LINE
       , lt_out12    TYPE zsda_arch01 OCCURS 0 WITH HEADER LINE
       , ls_post     TYPE zsda_arch01
       , lt_date     TYPE zsda_arch02 OCCURS 0 WITH HEADER LINE
       , lt_out34    TYPE zsda_arch02 OCCURS 0 WITH HEADER LINE
       , ls_date     TYPE zsda_arch02

       , ls_private  TYPE slis_data_caller_exit
       , ls_selfield TYPE slis_selfield
       , exc_exctab  TYPE slis_t_extab
       , lv_status   TYPE zdl_post_status
       , l_exit(1)   TYPE c
       .
*-------------------------------
  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_month    TYPE spmon,
          lv_vin_no   TYPE ze_vin,
          lv_post_id  TYPE zepostid,
          lv_category TYPE char10,
          lv_gubun    TYPE zetaxonomyid,
          lv_value    TYPE zemetaval,
          lv_title    TYPE zdl_post_title,
          lv_meta_key TYPE zemetakey,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80),
          lv_question TYPE sy-title,
          answer.
***  DATA : is_fieldcat  TYPE slis_t_fieldcat_alv.
  DATA : is_fieldcat  TYPE LINE OF slis_t_fieldcat_alv,
         lt_fieldcat  TYPE LINE OF slis_t_fieldcat_alv OCCURS 0,
         ls_fieldcat  LIKE LINE OF lt_fieldcat.
  CONSTANTS:
        answer_cancel(1)     VALUE 'A',
        answer_add(1)        VALUE '1',
        answer_ch(1)         VALUE '2'.


  CONCATENATE is_buyback-acl_no is_buyback-vin_no INTO lv_value.
  CONDENSE lv_value NO-GAPS.

  lv_status = 'A'.
  CASE p_flag.
*-- BUYBACK * LEGAL
    WHEN '1' OR '2'.
      lv_title = is_buyback-acl_no.
      lv_value = is_buyback-vin_no.
      TRANSLATE lv_title TO UPPER CASE.
      TRANSLATE lv_value TO UPPER CASE.
      SELECT a~post_title   a~post_desc    a~post_author
             a~post_date    a~post_date_tm a~post_id
             a~meta_value
        INTO CORRESPONDING FIELDS OF TABLE lt_post
        FROM zvda_arch01 AS a
       WHERE taxonomy_id  = g_tid
         AND post_title_c = lv_title
         AND meta_value_c = lv_value                        "20140508
         AND post_status  = lv_status
         AND ( meta_key = 'ACL_NO' OR meta_key = 'VIN_NO' ).   "A
      IF sy-subrc = 0.
        CLEAR: lt_out12[].
        SORT lt_post BY post_id post_date post_date_tm.
        LOOP AT lt_post INTO ls_post.
          CLEAR: lt_out12.
          MOVE-CORRESPONDING ls_post TO lt_out12.
          lt_out12-acl_no = ls_post-post_title.
          lt_out12-vin_no = ls_post-meta_value.
***          IF is_buyback-vin_no IS NOT INITIAL.
***            IF lt_out12-vin_no <> is_buyback-vin_no.
***              CONTINUE.
***            ENDIF.
***          ENDIF.
          APPEND lt_out12.
        ENDLOOP.
      ENDIF.

      IF lt_out12[] IS INITIAL.
        p_flag = 'X'.
        EXIT.
      ENDIF.

*-- Contract
    WHEN '5'.

      lv_title = g_lifnr.
      TRANSLATE lv_title TO UPPER CASE.
      CASE 'X'.
        WHEN cb1. lv_meta_key = gc_agree.
        WHEN cb2. lv_meta_key = gc_meeting.
        WHEN cb3. lv_meta_key = gc_report.
        WHEN cb4. lv_meta_key = gc_object.
      ENDCASE.
      SELECT a~post_title   a~post_desc    a~post_author
             a~post_date    a~post_date_tm a~post_id
             a~meta_value   a~meta_key
        INTO CORRESPONDING FIELDS OF TABLE lt_post
        FROM zvda_arch01 AS a
       WHERE meta_key     = lv_meta_key
         AND taxonomy_id  = g_tid
         AND post_title_c = lv_title
         AND post_status  = lv_status.
      IF sy-subrc = 0.
        CLEAR: lt_out12[].
        SORT lt_post BY post_id post_date post_date_tm.
        LOOP AT lt_post INTO ls_post.
          CLEAR: lt_out12.
          MOVE-CORRESPONDING ls_post TO lt_out12.
          lt_out12-vin_no     = ls_post-post_title.
          lt_out12-post_title = ls_post-post_desc.
          APPEND lt_out12.
        ENDLOOP.
      ENDIF.

*- 20140603 Add s
      clear: gv_sp.
      SELECT SINGLE *
        FROM zvda_arch01
       WHERE post_title_c = lv_title
         AND post_status  = lv_status.
      IF sy-subrc <> 0.
        gv_sp = '4'.
      ENDIF.
*- 20140603 Add e

      IF lt_out12[] IS INITIAL.
        p_flag = 'X'.
        EXIT.
      ENDIF.

*-- CLAIM
    WHEN '3' OR '4'.
      lv_month = ztda_posts-post_month.
      SELECT a~post_desc    a~post_author
             a~post_date    a~post_date_tm a~post_month a~post_id
        INTO CORRESPONDING FIELDS OF TABLE lt_date
        FROM zvda_arch01 AS a
       WHERE taxonomy_id = g_tid
         AND post_month  = lv_month
         AND term_id     = g_tid
         AND post_status = lv_status.   "A
      IF sy-subrc = 0.
        CLEAR: lt_out34[].
        SORT lt_date BY post_id post_date post_date_tm.
        LOOP AT lt_date INTO ls_date.
          CLEAR: lt_out34.
          MOVE-CORRESPONDING ls_date TO lt_out34.
          lt_out34-post_title  = ls_date-post_desc.
          APPEND lt_out34.
        ENDLOOP.
      ENDIF.
      IF lt_out34[] IS INITIAL.
        p_flag = 'X'.
        EXIT.
      ENDIF.
****-- ReCLAIM
***    WHEN '4'.
***      lv_month = ztda_posts-post_month.
***      SELECT a~post_desc    a~post_author
***             a~post_date    a~post_date_tm a~post_month a~post_id
***        INTO CORRESPONDING FIELDS OF TABLE lt_date
***        FROM zvda_arch01 AS a
***       WHERE post_month  = lv_month
***         AND post_status = lv_status.   "A
***      IF sy-subrc = 0.
***        CLEAR: lt_out34[].
***        SORT lt_date BY post_id post_date post_date_tm.
***        LOOP AT lt_date INTO ls_date.
***          CLEAR: lt_out34.
***          MOVE-CORRESPONDING ls_date TO lt_out34.
***          lt_out34-post_title  = ls_date-post_desc.
***          APPEND lt_out34.
***        ENDLOOP.
***      ENDIF.
***      IF lt_out34[] IS INITIAL.
***        p_flag = 'X'.
***        EXIT.
***      ENDIF.
  ENDCASE.
*
  CLEAR: lv_title, lv_question.
  CASE p_flag.
    WHEN '1' OR '2'.
      CONCATENATE 'Current VIN ' is_buyback-acl_no is_buyback-vin_no
      'number is already existed.'
                                  INTO lv_title SEPARATED BY space.
    WHEN '3'.
      CONCATENATE 'CLAIM Year-Month ' ztda_posts-post_month
      'is already existed.'
                                  INTO lv_title SEPARATED BY space.
    WHEN '4'.
      CONCATENATE 'reCLAIM Year-Month ' ztda_posts-post_month
      'number is already existed.'
                                  INTO lv_title SEPARATED BY space.

    WHEN '5'.
      CONCATENATE 'Contract Vendor :  ' g_lifnr
      'code is already existed.'
                                  INTO lv_title SEPARATED BY space.
  ENDCASE.

  lv_question = 'Do you want to Add or replace documentment ?'.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar                    = lv_title
*        DIAGNOSE_OBJECT             = ' '
      text_question               = lv_question
      text_button_1               = 'Add'(001)
*        ICON_BUTTON_1               = ' '
      text_button_2               = 'Replace'(002)
*        ICON_BUTTON_2               = ' '
      default_button              = '1'
      display_cancel_button       = 'X'
*        USERDEFINED_F1_HELP         = ' '
      start_column                = 25
      start_row                   = 6
*        POPUP_TYPE                  =
*        IV_QUICKINFO_BUTTON_1       = ' '
*        IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer                      = answer
*      TABLES
*        PARAMETER                   =
    EXCEPTIONS
      text_not_found              = 1
      OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  CASE answer.
    WHEN answer_add.     lv_flag = 'A'.   "Add
    WHEN answer_cancel.  lv_flag = 'E'.   "Cancel
    WHEN answer_ch.      lv_flag = 'C'.   "Replace
  ENDCASE.

  CASE answer.
    WHEN answer_add OR answer_cancel.
      p_flag = lv_flag.
      EXIT.
  ENDCASE.

  CLEAR: gt_fieldcat, gs_fieldcat, g_pos.
  CASE p_flag.
    WHEN '1' OR '2'.
      PERFORM make_list_field_cat USING :
         'POST_TITLE'   'Title' 'CHAR' '' '' '' '40' 'X' '' '',
         'ACL_NO'       'ACL No' 'CHAR' '' '' '' '09' '' 'X' '',
         'ACL_NO'       'ACL No' 'CHAR' '' '' '' '09' '' 'X' '',
         'VIN_NO'       'VIN No' 'CHAR' '' '' '' '18' '' 'X' '',
         'POST_AUTHOR'  'Author' 'CHAR' '' '' '' '12' '' '' '',
         'POST_DATE'    'Post Date' 'DATS' '' '' '' '10' '' '' '',
         'POST_DATE_TM' 'Post Time' 'TIMS' '' '' '' '08' '' '' '',
         'POST_DESC'    'Desc' 'CHAR' '' '' '' '40' '' '' '',
         'POST_ID'      'Post ID' 'CHAR' '' '' '' '12' '' '' '',
         'META_VALUE'   'Value' 'CHAR' '' '' '' '40' '' 'X' ''.


***      CLEAR:  is_fieldcat, g_pos.
***      g_pos = g_pos + 1.
***      is_fieldcat-fieldname   = 'ACL_NO'.
***      is_fieldcat-seltext_m   = 'ACL No'.
***      is_fieldcat-key         = 'X'.
***      is_fieldcat-outputlen   = 9.
***      is_fieldcat-col_pos     = g_pos.
***      APPEND is_fieldcat TO gt_fieldcat.
***
***      CLEAR:  is_fieldcat.
***      g_pos = g_pos + 1.
***      is_fieldcat-fieldname   = 'VIN_NO'.
***      is_fieldcat-seltext_m   = 'VIN No'.
***      is_fieldcat-key         = 'X'.
***      is_fieldcat-outputlen   = 18.
***      is_fieldcat-col_pos     = g_pos.
***      APPEND is_fieldcat TO gt_fieldcat.
***
***      CLEAR:  is_fieldcat.
***      g_pos = g_pos + 1.
***      is_fieldcat-fieldname   = 'POST_AUTHOR'.
***      is_fieldcat-seltext_m   = 'Author'.
***      is_fieldcat-key         = ' '.
***      is_fieldcat-outputlen   = 12.
***      is_fieldcat-col_pos     = g_pos.
***      APPEND is_fieldcat TO gt_fieldcat.

      LOOP AT gt_fieldcat INTO ls_fieldcat TO 10.
        MODIFY gt_fieldcat FROM ls_fieldcat.
      ENDLOOP.

      CLEAR: lv_title.
      lv_title = 'Double click line or Excute Click after select line'.
      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title                 = lv_title
          i_selection             = 'X'
*         I_ZEBRA                 = ' '
*         I_SCREEN_START_COLUMN   = 0
*         I_SCREEN_START_LINE     = 0
*         i_screen_end_column     = 60
*         i_screen_end_line       = 13
*         I_CHECKBOX_FIELDNAME    =
*         I_LINEMARK_FIELDNAME    = 'MARK'
          i_scroll_to_sel_line    = 'X'
          i_tabname               = '1'
*         i_structure_name        = 'ZSDA_ARCH01'
          it_fieldcat             = gt_fieldcat
          it_excluding            = exc_exctab
*         I_CALLBACK_PROGRAM      =
*         I_CALLBACK_USER_COMMAND =
          is_private              = ls_private
        IMPORTING
          es_selfield             = ls_selfield
          e_exit                  = l_exit
        TABLES
          t_outtab                = lt_out12
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE i000(0k) WITH sy-subrc.
      ENDIF.

      IF ls_selfield-tabindex = 0.
        p_flag = 'E'.
        EXIT.
      ENDIF.
      READ TABLE lt_out12 INDEX ls_selfield-tabindex INTO ls_post.
      IF sy-subrc = 0.
        SET PARAMETER ID 'PID' FIELD ls_post-post_id.
      ENDIF.

    WHEN '5'.
      CLEAR: g_pos.
      PERFORM make_list_field_cat USING :
            'META_KEY'     'MetaKey' 'CHAR' '' '' '' '10' 'X' '' '',
            'ACL_NO'       'ACL No' 'CHAR' '' '' '' '09' '' 'X' '',
            'VIN_NO'       'VIN No' 'CHAR' '' '' '' '18' '' 'X' '',
            'POST_AUTHOR'  'Author' 'CHAR' '' '' '' '12' '' '' '',
            'POST_DATE'    'Post Date' 'DATS' '' '' '' '10' '' '' '',
            'POST_DATE_TM' 'Post Time' 'TIMS' '' '' '' '08' '' '' '',
            'POST_TITLE'   'Title' 'CHAR' '' '' '' '40' '' '' '',
***            'POST_DESC'    'Desc.' 'CHAR' '' '' '' '40' '' '' '',
            'POST_ID'      'Post ID' 'CHAR' '' '' '' '12' '' ' ' '',
            'META_VALUE'   'Value' 'CHAR' '' '' '' '40' '' 'X' ''.
*      CLEAR  is_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'ACL_NO'.
*      gs_fieldcat-seltext_m   = 'ACL No'.
*      gs_fieldcat-no_out      = 'X'.
*      gs_fieldcat-outputlen   = 10.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'META_KEY'.
*      gs_fieldcat-seltext_m   = 'MetaKey'.
*      gs_fieldcat-key         = 'X'.
*      gs_fieldcat-outputlen   = 10.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'VIN_NO'.
*      gs_fieldcat-seltext_m   = 'Vendor'.
*      gs_fieldcat-key         = 'X'.
*      gs_fieldcat-outputlen   = 10.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'META_VALUE'.
*      gs_fieldcat-seltext_m   = 'Vendor Name'.
*      gs_fieldcat-outputlen   = 35.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_AUTHOR'.
*      gs_fieldcat-seltext_m   = 'Author'.
*      gs_fieldcat-outputlen   = 12.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_DATE'.
*      gs_fieldcat-seltext_m   = 'POST DATE'.
*      gs_fieldcat-outputlen   = 10.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_DATE_TM'.
*      gs_fieldcat-seltext_m   = 'POST TIME'.
*      gs_fieldcat-outputlen   = 8.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_TITLE'.
*      gs_fieldcat-seltext_m   = 'Title'.
*      gs_fieldcat-outputlen   = 60.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_DESC'.
*      gs_fieldcat-seltext_m   = 'POST DESC'.
*      gs_fieldcat-outputlen   = 60.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.
*
*      CLEAR  gs_fieldcat.
*      g_pos = g_pos + 1.
*      gs_fieldcat-fieldname   = 'POST_ID'.
*      gs_fieldcat-seltext_m   = 'POST ID'.
*      gs_fieldcat-no_out      = 'X'.
*      gs_fieldcat-outputlen   = 12.
*      gs_fieldcat-col_pos     = g_pos.
*      APPEND gs_fieldcat TO gt_fieldcat.

***      LOOP AT gt_fieldcat INTO gs_fieldcat TO 8.
***        APPEND gs_fieldcat TO lt_fieldcat.
***        CLEAR: gs_fieldcat.
***      ENDLOOP.
      LOOP AT gt_fieldcat INTO ls_fieldcat TO 9.
        MODIFY gt_fieldcat FROM ls_fieldcat.
      ENDLOOP.

      CLEAR: lv_title.
      lv_title = 'Double click line or Excute Click after select line'.
      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title                 = lv_title
          i_selection             = 'X'
*         I_ZEBRA                 = ' '
*         I_SCREEN_START_COLUMN   = 0
*         I_SCREEN_START_LINE     = 0
*         i_screen_end_column     = 60
*         i_screen_end_line       = 13
*         I_CHECKBOX_FIELDNAME    =
*         I_LINEMARK_FIELDNAME    = 'MARK'
          i_scroll_to_sel_line    = 'X'
          i_tabname               = '1'
*         i_structure_name        = 'ZSDA_ARCH01'
          it_fieldcat             = gt_fieldcat
          it_excluding            = exc_exctab
*         I_CALLBACK_PROGRAM      =
*         I_CALLBACK_USER_COMMAND =
          is_private              = ls_private
        IMPORTING
          es_selfield             = ls_selfield
          e_exit                  = l_exit
        TABLES
          t_outtab                = lt_out12
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE i000(0k) WITH sy-subrc.
      ENDIF.

      IF ls_selfield-tabindex = 0.
        p_flag = 'E'.
        EXIT.
      ENDIF.
      READ TABLE lt_out12 INDEX ls_selfield-tabindex INTO ls_post.
      IF sy-subrc = 0.
        SET PARAMETER ID 'PID' FIELD ls_post-post_id.
      ENDIF.

    WHEN '3' OR '4'.
      PERFORM make_list_field_cat USING :
         'POST_MONTH'   'Post Month' 'CHAR' '' '' '' '08' 'X' '' '',
         'POST_AUTHOR'  'Author' 'CHAR' '' '' '' '12' '' '' '',
         'POST_DATE'    'Post Date' 'DATS' '' '' '' '10' '' '' '',
         'POST_DATE_TM' 'Post Time' 'TIMS' '' '' '' '08' '' '' '',
         'POST_TITLE'   'Title' 'CHAR' '' '' '' '20' 'X' '' '',
***         'POST_DESC'    'Desc.' 'CHAR' '' '' '' '40' '' '' '',
         'POST_ID'      'Post ID' 'CHAR' '' '' '' '12' '' '' ''.

      LOOP AT gt_fieldcat INTO ls_fieldcat TO 6.
        MODIFY gt_fieldcat FROM ls_fieldcat.
      ENDLOOP.

      CLEAR: lv_title.
      lv_title = 'Double click line or Excute Click after select line'.
      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title                 = lv_title
          i_selection             = 'X'
*         I_ZEBRA                 = ' '
*         I_SCREEN_START_COLUMN   = 0
*         I_SCREEN_START_LINE     = 0
*         i_screen_end_column     = 60
*         i_screen_end_line       = 13
*         I_CHECKBOX_FIELDNAME    =
*         I_LINEMARK_FIELDNAME    = 'MARK'
          i_scroll_to_sel_line    = 'X'
          i_tabname               = '1'
*         i_structure_name        = 'ZSDA_ARCH02'
          it_fieldcat             = gt_fieldcat
          it_excluding            = exc_exctab
*         I_CALLBACK_PROGRAM      =
*         I_CALLBACK_USER_COMMAND =
          is_private              = ls_private
        IMPORTING
          es_selfield             = ls_selfield
          e_exit                  = l_exit
        TABLES
          t_outtab                = lt_out34
        EXCEPTIONS
          program_error           = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE i000(0k) WITH sy-subrc.
      ENDIF.

      IF ls_selfield-tabindex = 0.
        p_flag = 'E'.
        EXIT.
      ENDIF.
      READ TABLE lt_out34 INDEX ls_selfield-tabindex INTO ls_date.
      IF sy-subrc = 0.
        SET PARAMETER ID 'PID' FIELD ls_date-post_id.
      ENDIF.

  ENDCASE.
  p_flag = lv_flag.

ENDFORM.                    " DUPLICATE_CHECK



*&---------------------------------------------------------------------*
*&      Form  MODIFY_POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_post_data .

  DATA : ls_st       TYPE zsda_arch01
       , lv_postid   TYPE zepostid
       , lv_flag       TYPE c
       .

  GET PARAMETER ID 'PID' FIELD lv_postid.
  TRY.
      UPDATE ztda_posts
         SET post_status = 'D'
       WHERE post_id = lv_postid.
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Error in update!` TYPE 'I'.
      lv_flag = 'E'.
  ENDTRY.
  IF g_proc = 'L'.
    TRY.
        UPDATE ztda_meta_name
           SET post_status = 'D'
         WHERE post_id = lv_postid.
      CATCH cx_sy_dynamic_osql_error.
        MESSAGE `Error in update!` TYPE 'I'.
        lv_flag = 'E'.
    ENDTRY.
  ENDIF.

ENDFORM.                    " MODIFY_POST_DATA





*&---------------------------------------------------------------------*
*&      Form  MAKE_LIST_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1981   text
*      -->P_1982   text
*      -->P_1983   text
*      -->P_1984   text
*      -->P_1985   text
*      -->P_1986   text
*      -->P_1987   text
*      -->P_1988   text
*      -->P_1989   text
*      -->P_1990   text
*----------------------------------------------------------------------*
FORM make_list_field_cat5 USING    p_fieldname
                                   p_rddic
                                   p_datatype
                                   p_tabname
                                   p_do_sum
                                   p_cfieldname
                                   p_outputlen
                                   p_key
                                   p_no_out
                                   p_quantity.
  DATA: ls_fieldcat TYPE slis_t_fieldcat_alv.

***  CASE p_fieldname.
***    WHEN 'META_KEY' OR 'POST_AUTHOR' OR 'POST_DATE' OR 'POST_DATE' OR
***         'POST_DATE_TM' OR 'POST_TITLE' OR 'POST_DESC' OR 'POST_ID' OR
***         'META_VALUE'.
  gs_fieldcat-fieldname     = p_fieldname.
  gs_fieldcat-reptext_ddic  = p_rddic.
  gs_fieldcat-datatype      = p_datatype.
  gs_fieldcat-tabname       = p_tabname.
  gs_fieldcat-do_sum        = p_do_sum.
  gs_fieldcat-outputlen     = p_outputlen.
  gs_fieldcat-cfieldname    = p_cfieldname.
  gs_fieldcat-key           = p_key.
  gs_fieldcat-no_out        = p_no_out.
  gs_fieldcat-quantity      = p_quantity.

  IF NOT gs_fieldcat IS INITIAL.
    g_pos = g_pos + 1.
    gs_fieldcat-col_pos       = g_pos.
    APPEND gs_fieldcat TO  gt_fieldcat.
    CLEAR  gs_fieldcat.
  ENDIF.
***  ENDCASE.

ENDFORM.                    " MAKE_LIST_FIELD_CAT



*&---------------------------------------------------------------------*
*&      Form  make_list_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   text
*      -->P_RDDIC       text
*      -->P_DATATYPE    text
*      -->P_TABNAME     text
*      -->P_DO_SUM      text
*      -->P_CFIELDNAME  text
*      -->P_OUTPUTLEN   text
*      -->P_KEY         text
*      -->P_NO_OUT      text
*      -->P_QUANTITY    text
*----------------------------------------------------------------------*
FORM make_list_field_cat  USING    p_fieldname
                                   p_rddic
                                   p_datatype
                                   p_tabname
                                   p_do_sum
                                   p_cfieldname
                                   p_outputlen
                                   p_key
                                   p_no_out
                                   p_quantity.
  DATA: ls_fieldcat TYPE slis_t_fieldcat_alv.

  gs_fieldcat-fieldname     = p_fieldname.
  gs_fieldcat-reptext_ddic  = p_rddic.
  gs_fieldcat-datatype      = p_datatype.
  gs_fieldcat-tabname       = p_tabname.
  gs_fieldcat-do_sum        = p_do_sum.
  gs_fieldcat-outputlen     = p_outputlen.
  gs_fieldcat-cfieldname    = p_cfieldname.
  gs_fieldcat-key           = p_key.
  gs_fieldcat-no_out        = p_no_out.
  gs_fieldcat-quantity      = p_quantity.

  IF NOT gs_fieldcat IS INITIAL.
    g_pos = g_pos + 1.
    gs_fieldcat-col_pos       = g_pos.
    APPEND gs_fieldcat TO  gt_fieldcat.
    CLEAR  gs_fieldcat.
  ENDIF.

ENDFORM.                    " MAKE_LIST_FIELD_CAT



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_data_check_vendor .

  DATA  : lv_vendor   TYPE lifnr,
          lv_flag,                 "Return value: N-Nothing,
          lv_msg(80).

* Vendor code check
  IF lfa1-lifnr IS INITIAL.
    MESSAGE s999 WITH text-m01.     "Fill in all required entry fields
    g_flg = 'X'.
    g_fldname = 'LFA1-LIFNR'.
    EXIT.
  ENDIF.

  PERFORM vendor_check USING lv_flag lfa1-lifnr.
  IF lv_flag IS NOT INITIAL.
    MESSAGE s999 WITH text-m04      "Vendor code does not exist !
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'LFA1-LIFNR'.
    EXIT.
  ENDIF.

*   Post title check
  IF g_title IS INITIAL.
    MESSAGE s999 WITH text-m01 DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'G_TITLE'.
    EXIT.
  ENDIF.

  g_af = '4'.
  PERFORM qa_vendor_check USING g_af  g_lifnr.

ENDFORM.                    " INPUT_DATA_CHECK_VENDOR



*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_vendor .


  DATA  : lv_pid           TYPE zepostid,
          lv_mid           TYPE zemetaid,
          lv_tid           TYPE zetaxonomyid,
          lv_cnt           TYPE i.

  PERFORM get_alcvin_data.

  IF is_ztda_posts IS NOT INITIAL.
    CLEAR ztda_posts.
    MOVE-CORRESPONDING is_ztda_posts TO ztda_posts.
    MODIFY ztda_posts.
    COMMIT WORK AND WAIT.
  ENDIF.

  lv_pid = ztda_posts-post_id.
  IF lv_pid IS INITIAL.
    lv_pid = g_postid_o.
  ENDIF.
  SELECT SINGLE taxonomy_id INTO lv_tid
    FROM ztda_term_rel
   WHERE post_id = lv_pid.

  PERFORM meta_id_creat USING lv_mid.
  lv_mid = lv_mid + 1.
  lv_tid = g_tid.
  PERFORM save_standard_data_process USING lv_pid lv_mid lv_tid.
  PERFORM ztda_term_rel_process      USING lv_pid lv_mid lv_tid.
  IF g_link IS NOT INITIAL.
    lv_tid = '00002'.
    PERFORM ztda_term_rel_process    USING lv_pid lv_mid lv_tid.
  ENDIF.

  IF it_ztda_postmeta[] IS NOT INITIAL.
    LOOP AT it_ztda_postmeta.
      lv_cnt = sy-tabix.
      CLEAR: g_meta_id, lv_pid, lv_mid, lv_tid.
      PERFORM meta_id_creat USING g_meta_id.
      it_ztda_postmeta-meta_id  = g_meta_id + 1.

      lv_pid = it_ztda_postmeta-post_id.
      MODIFY ztda_postmeta FROM it_ztda_postmeta.
      COMMIT WORK AND WAIT.
      lv_mid = it_ztda_postmeta-meta_id.
      lv_tid = g_tid.

    ENDLOOP.

  ENDIF.

  LEAVE PROGRAM.

ENDFORM.                    " SAVE_DATA_VENDOR



*&---------------------------------------------------------------------*
*&      Form  DUPLICATE_CHECK_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG1  text
*----------------------------------------------------------------------*
FORM duplicate_check_vendor  USING    p_g_flg1.

ENDFORM.                    " DUPLICATE_CHECK_VENDOR



*&---------------------------------------------------------------------*
*&      Form  VENDOR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_LFA1_LIFNR  text
*----------------------------------------------------------------------*
FORM vendor_check  USING    p_flag
                            p_lifnr.

  CLEAR: lfa1.
  SELECT SINGLE * FROM lfa1
   WHERE lifnr = g_lifnr.
  IF sy-subrc = 0.
    lfa1-name1 = lfa1-name1.
    EXIT.
  ELSE.
    p_flag = 'E'.
  ENDIF.

ENDFORM.                    " VENDOR_CHECK



*&---------------------------------------------------------------------*
*&      Form  CHECKBOX_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLAG1  text
*----------------------------------------------------------------------*
FORM checkbox_check  USING    p_flag.

  DATA: lv_cnt TYPE i.
  IF cb1 NE space.
    ADD 1 TO lv_cnt.     p_flag = '1'.
  ENDIF.

  IF cb2 NE space.
    ADD 1 TO lv_cnt.     p_flag = '2'.
  ENDIF.

  IF cb3 NE space.
    ADD 1 TO lv_cnt.     p_flag = '3'.
  ENDIF.

  IF cb4 NE space.
    ADD 1 TO lv_cnt.     p_flag = '4'.
  ENDIF.

  IF lv_cnt <> 1.
    p_flag = 'E'.
  ENDIF.

ENDFORM.                    " CHECKBOX_CHECK



*&---------------------------------------------------------------------*
*&      Form  QA_VENDOR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_AF  text
*      -->P_G_LIFNR  text
*----------------------------------------------------------------------*
FORM qa_vendor_check  USING    p_af
                               p_lifnr.

  DATA : lv_title      TYPE zdl_post_title,  "ZDL_POST_TITLE
         lv_flag       TYPE c,
         lv_termid     TYPE zetermid,
         lv_postid     TYPE zepostid.
  DATA : BEGIN OF lt_postid OCCURS 0,
         post_id       TYPE zepostid,
         END   OF lt_postid.

  lv_termid = g_tid.
  lv_title  = p_lifnr.
  SELECT SINGLE * FROM zvda_arch01
   WHERE post_status = 'A'
     AND post_title  = lv_title
     AND post_flag   = p_af.
**     AND term_id     = lv_termid.
  IF sy-subrc <> 0.
    g_af = p_af.
  ELSE.
    EXIT.
  ENDIF.

ENDFORM.                    " QA_VENDOR_CHECK



*&---------------------------------------------------------------------*
*&      Form  QA_DATEMONTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_AF  text
*----------------------------------------------------------------------*
FORM qa_datemonth_check  USING    p_af.

  DATA: lv_title      TYPE zdl_post_title,  "ZDL_POST_TITLE
        lv_flag       TYPE c,
        lv_termid     TYPE zetermid,
        lv_postid     TYPE zepostid.
  DATA  : BEGIN OF lt_postid OCCURS 0,
          post_id       TYPE zepostid,
          END   OF lt_postid.

  lv_termid = g_tid.
  lv_title  = ztda_posts-post_month.
  SELECT SINGLE * FROM zvda_arch01
   WHERE post_status = 'A'
     AND post_title  = lv_title
     AND post_flag   = p_af
     AND term_id     = lv_termid.
  IF sy-subrc <> 0.
    g_af = p_af.
  ELSE.
    g_af = zvda_arch01-post_flag.
    EXIT.
  ENDIF.

ENDFORM.                    " QA_DATEMONTH_CHECK



*&---------------------------------------------------------------------*
*&      Form  NAME_CHECK
*&---------------------------------------------------------------------*
*       text : "20140512 Add
*----------------------------------------------------------------------*
*      -->P_IS_ZTDA_META_NAME_META_FLAG  text
*----------------------------------------------------------------------*
FORM name_check  USING    p_flag.

  DATA : lv_fname     TYPE zedalname,
         lv_lname     TYPE zedalname
       .
  lv_lname = is_ztda_meta_name-last_name_c.
  lv_fname = is_ztda_meta_name-first_name_c.
  SELECT SINGLE * FROM ztda_meta_name
   WHERE last_name_c  = lv_lname
     AND first_name_c = lv_fname.
  IF sy-subrc NE 0.
    p_flag     = '5'.
  ELSE.
    CLEAR p_flag.
  ENDIF.

ENDFORM.                    " NAME_CHECK



*&---------------------------------------------------------------------*
*&      Form  QA_VIN_NO_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_AF  text
*      -->P_IS_BUYBACK_VIN_NO  text
*----------------------------------------------------------------------*
FORM qa_vin_no_check  USING    p_av
                               p_vinno.

  DATA: lv_aclno      TYPE zdl_post_title,  "ZDL_POST_TITLE
        lv_vinno      TYPE zemetaval,
        lv_flag       TYPE c,
        lv_metakey    TYPE zemetakey,
        lv_termid     TYPE zetermid.
  DATA  : BEGIN OF lt_postid OCCURS 0,
          post_id       TYPE zepostid,
          END   OF lt_postid.

  lv_metakey = gc_vinno.
  lv_termid  = g_tid.
  lv_vinno   = p_vinno.
  TRANSLATE lv_vinno TO UPPER CASE.
  SELECT SINGLE * FROM zvda_arch01
   WHERE meta_key     = lv_metakey
     AND post_status  = 'A'
     AND meta_value_c = lv_vinno
     AND term_id      = lv_termid.
  IF sy-subrc <> 0.
  ELSE.
    g_av = ' '.
  ENDIF.

ENDFORM.                    " QA_VIN_NO_CHECK
