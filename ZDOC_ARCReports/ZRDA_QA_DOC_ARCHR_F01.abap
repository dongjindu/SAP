*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCHR_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .

  CLEAR: g_date, g_date_tm, g_first.
  g_date    = sy-datum.
  g_date_tm = sy-uzeit.

ENDFORM.                    " INIT_DATA



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLAG  text
*----------------------------------------------------------------------*
FORM input_data_check  USING    p_flag.

  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_vin_no   TYPE ze_vin,
          lv_category TYPE char10,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80).


* VIN Number check
  IF is_buyback-acl_no IS NOT INITIAL.
    PERFORM acl_check USING lv_flag is_buyback-acl_no.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m02      "ACL Number does not exist !
      DISPLAY LIKE 'E'.
      p_flag = g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-ACL_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* VIN Number check
  IF is_buyback-vin_no IS NOT INITIAL.
    PERFORM vin_check USING lv_flag is_buyback-vin_no.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m03      "VIN Number does not exist !
      DISPLAY LIKE 'E'.
      p_flag = g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-VIN_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* title check
  IF g_title IS NOT INITIAL.
    PERFORM title_check USING lv_flag g_title.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m06      "This title's record does not exist !
      DISPLAY LIKE 'E'.
      p_flag = g_flg = 'E'.
      g_fldname = 'G_TITLE'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_DATA_CHECK



*&---------------------------------------------------------------------*
*&      Form  ACL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_IS_BUYBACK_ACL_NO  text
*----------------------------------------------------------------------*
FORM acl_check  USING    p_flag
                         p_acl_no TYPE zacln.
  DATA : lv_meta_key     TYPE zemetakey,
         lv_title        TYPE zdl_post_title,
         lv_status       TYPE zdl_post_status.
  CLEAR: ztsd_acm_h, p_flag.
  TRANSLATE p_acl_no TO UPPER CASE.
  SELECT SINGLE zacln INTO ztsd_acm_h-zacln
    FROM ztsd_acm_h
   WHERE zacln = p_acl_no.

  IF sy-subrc NE 0.
    p_flag = 'N'.
    EXIT.
  ENDIF.

  lv_meta_key = gc_only.  "ACL_NO
  lv_status   = gc_a.     "A
  lv_title    = p_acl_no. "Input acl_no
  CLEAR: zvda_arch01, it_data[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM zvda_arch01
   WHERE meta_key    = lv_meta_key
     AND post_status = lv_status
     AND post_title  = lv_title.

  IF sy-subrc NE 0.
    p_flag = 'N'.
    EXIT.
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
  DATA : lv_meta_key     TYPE zemetakey,
         lv_vinno        TYPE zemetaval,
         lv_status       TYPE zdl_post_status.
  CLEAR: ztpp_vm, p_flag.
  TRANSLATE p_vin_no TO UPPER CASE.
  SELECT SINGLE vin INTO ztpp_vm-vin
    FROM ztpp_vm
   WHERE vin = p_vin_no.

  IF sy-subrc NE 0.
    p_flag = 'N'.
  ENDIF.

  lv_meta_key = gc_vinno.  "VIN_NO
  lv_status   = gc_a.     "A
  lv_vinno    = p_vin_no. "Input vin_no
  CLEAR zvda_arch01.
  SELECT SINGLE post_title INTO zvda_arch01-post_title
    FROM zvda_arch01
   WHERE meta_key    = lv_meta_key
     AND post_status = lv_status
     AND meta_value  = lv_vinno.

  IF sy-subrc NE 0.
    p_flag = 'N'.
    EXIT.
  ENDIF.

ENDFORM.                    " VIN_CHECK



*&---------------------------------------------------------------------*
*&      Form  TITLE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_G_TITLE  text
*----------------------------------------------------------------------*
FORM title_check  USING    p_flag
                           p_title.
  DATA : lv_meta_key     TYPE zemetakey,
         lv_title        TYPE zemetadesc,
         lv_status       TYPE zdl_post_status,
         BEGIN OF lt_title OCCURS 0,
           post_id       LIKE zvda_arch01-post_id,
           post_desc     LIKE zvda_arch01-post_desc,
         END   OF lt_title.
  CLEAR: zvda_arch01, p_flag.

  lv_status   = gc_a.      "A
  CONCATENATE '%' p_title '%' INTO lv_title.
  CONDENSE  lv_title NO-GAPS.
  TRANSLATE lv_title TO UPPER CASE.

  CLEAR zvda_arch01.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_title
    FROM zvda_arch01
   WHERE taxonomy_id = g_tid
     AND post_status = lv_status
     AND post_desc_c  LIKE lv_title.

  IF sy-subrc NE 0.
    p_flag = 'N'.
    EXIT.
  ENDIF.

ENDFORM.                    " TITLE_CHECK



*&---------------------------------------------------------------------*
*&      Form  BUYBACK_DATA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM buyback_data_get  USING    p_flag.

  DATA : lv_meta_key     TYPE zemetakey,
         lv_acl          TYPE zdl_post_title,
         lv_vin          TYPE zemetaval,
         lv_desc         TYPE zemetadesc,
         lv_status       TYPE zdl_post_status,
         cond_syntax     TYPE string,
         wtab(72)  OCCURS 100 WITH HEADER LINE,
         and(4).

  lv_status   = gc_a.      "A  CONCATENATE 'CARRID = ' '' CARR_ID '''' INTO WTAB.
  CONCATENATE 'POST_STATUS = ' 'LV_STATUS' INTO wtab SEPARATED BY space.
  APPEND wtab.
  and = 'AND'.
  CLEAR: it_data, it_data[].
  IF is_buyback-acl_no IS NOT INITIAL.
    lv_acl               =    is_buyback-acl_no.
    CONCATENATE and 'POST_TITLE = ' 'LV_ACL' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF is_buyback-vin_no IS NOT INITIAL.
    lv_vin               =    is_buyback-vin_no.
    CONCATENATE and 'META_VALUE = ' 'LV_VIN' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF g_title IS NOT INITIAL.
    CONCATENATE '%' g_title '%' INTO lv_desc.
    CONDENSE lv_desc NO-GAPS.
    TRANSLATE lv_desc TO UPPER CASE.
    CONCATENATE and 'POST_DESC_C LIKE ' 'LV_DESC' INTO wtab
                SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'TAXONOMY_ID = ' 'G_TID' INTO wtab SEPARATED BY space.
  APPEND wtab.
  wtab = '.'.     APPEND wtab.

  CLEAR: gt_data[], gt_data.
  TRY.
      SELECT * FROM zvda_arch01
        INTO CORRESPONDING FIELDS OF TABLE gt_data
       WHERE (wtab).
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Wrong WHERE condition!` TYPE 'I'.
  ENDTRY.

  IF gt_data[] IS INITIAL.
    p_flag = 'N'.
    MESSAGE s999 WITH text-m12      "ACL Number does not exist !
    DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_data[].
  LOOP AT gt_data INTO gs_data.
    CLEAR: it_data.
    MOVE-CORRESPONDING gs_data TO it_data.

    it_data-acl_no = gs_data-post_title.
    it_data-vin_no = gs_data-meta_value.
    CLEAR: it_data-post_title.
    it_data-post_title = gs_data-post_desc.
    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " BUYBACK_DATA_GET



*&---------------------------------------------------------------------*
*&      Form  ZTDA_POSTS_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ztda_posts_change .

  DATA : ls_data          LIKE LINE OF it_data,
         lv_aclno         TYPE zdl_post_title,
         lv_title         TYPE zdl_post_title,
         lv_desc          TYPE zemetadesc.
  DATA : post_desc        TYPE zemetadesc,  " post title
         meta_value       TYPE zemetaval,   " vin number
         post_desc_c      TYPE zemetadesc,  " post title
         meta_value_c     TYPE zemetaval,   " vin number
         last_name_c      TYPE zedalname,
         first_name_c     TYPE zedalname.
  CASE g_proc.
    WHEN 'B'. "Buyback change
      lv_aclno = is_data_o-acl_no.
      lv_title = is_data-acl_no.
      lv_desc  = is_data-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_title       = is_data-acl_no
                 post_desc        = is_data-post_title
                 post_title_c     = lv_title
                 post_desc_c      = lv_desc
           WHERE post_id    = is_data_o-post_id
             AND post_title = lv_aclno.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.
* ztda_postmeta update
      IF is_data_o-vin_no IS NOT INITIAL.
        meta_value   = is_data-vin_no.
        meta_value_c = is_data-vin_no.
        TRANSLATE meta_value_c    TO UPPER CASE.
        CONDENSE  meta_value_c    NO-GAPS.
        TRY.
            UPDATE ztda_postmeta
               SET meta_value     = meta_value
                   meta_value_c   = meta_value_c
             WHERE post_id        = is_data_o-post_id
               AND meta_key       = 'VIN_NO'.

          CATCH cx_sy_dynamic_osql_error.
            MESSAGE `Error in update !"POSTMETA"` TYPE 'I'.
        ENDTRY.
      ENDIF.

    WHEN 'L'. "Legal change
      lv_aclno = is_data_o-acl_no.
      lv_title = is_data-acl_no.
      lv_desc  = is_data-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_title       = is_data-acl_no
                 post_desc        = is_data-post_title
                 post_title_c     = lv_title
                 post_desc_c      = lv_desc
           WHERE post_id    = is_data_o-post_id
             AND post_title = lv_aclno.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.

* ztda_postmeta update
      IF is_data_o-vin_no IS NOT INITIAL.
        meta_value   = is_data-vin_no.
        meta_value_c = is_data-vin_no.
        TRANSLATE meta_value_c    TO UPPER CASE.
        CONDENSE  meta_value_c    NO-GAPS.
        TRY.
            UPDATE ztda_postmeta
               SET meta_value     = meta_value
                   meta_value_c   = meta_value_c
             WHERE post_id        = is_data_o-post_id
               AND meta_key       = 'VIN_NO'.

          CATCH cx_sy_dynamic_osql_error.
            MESSAGE `Error in update !"POSTMETA"` TYPE 'I'.
        ENDTRY.
      ENDIF.

* ztda_meta_name update
      IF is_data_o-last_name IS NOT INITIAL.
        last_name_c  = is_data-last_name.
        TRANSLATE last_name_c     TO UPPER CASE.
        CONDENSE  last_name_c     NO-GAPS.
        first_name_c = is_data-first_name.
        TRANSLATE first_name_c    TO UPPER CASE.
        CONDENSE  first_name_c    NO-GAPS.
        TRY.
            UPDATE ztda_meta_name
               SET last_name     = is_data-last_name
                   last_name_c   = last_name_c
                   first_name    = is_data-first_name
                   first_name_c  = first_name_c
             WHERE post_id       = is_data_o-post_id.

          CATCH cx_sy_dynamic_osql_error.
            MESSAGE `Error in update !"MATA_NAME"` TYPE 'I'.
        ENDTRY.
      ENDIF.

    WHEN 'C' OR 'R'. "Claim change & Reclaim change
      lv_title = is_data-post_month.
      lv_desc  = is_data-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_title       = is_data-post_month
                 post_desc        = is_data-post_title
                 post_title_c     = lv_title
                 post_desc_c      = lv_desc
           WHERE post_id    = is_data_o-post_id
             AND post_month = is_data_o-post_month.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.
* ztda_postmeta update
      IF is_data_o-post_month IS NOT INITIAL.
        meta_value   = is_data-post_month.
        meta_value_c = is_data-post_month.
        TRANSLATE meta_value_c    TO UPPER CASE.
        CONDENSE  meta_value_c    NO-GAPS.
        TRY.
            UPDATE ztda_postmeta
               SET meta_value     = meta_value
                   meta_value_c   = meta_value_c
             WHERE post_id        = is_data_o-post_id
               AND meta_key       = 'DATE'.

          CATCH cx_sy_dynamic_osql_error.
            MESSAGE `Error in update !"POSTMETA"` TYPE 'I'.
        ENDTRY.
      ENDIF.
    WHEN 'V'. "Vendor Files change
      lv_desc  = is_data-post_title.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.

      lv_title = is_data_o-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_desc        = is_data-post_title
                 post_desc_c      = lv_desc
           WHERE post_id      = is_data_o-post_id
             AND post_desc_c = lv_title.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.
  ENDCASE.
  COMMIT WORK.

ENDFORM.                    " ZTDA_POSTS_CHANGE



*&---------------------------------------------------------------------*
*&      Form  CONFIRM_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_ANSWER  text
*      -->P_TEXT_M07  text
*----------------------------------------------------------------------*
FORM confirm_job   USING   p_answer
                           p_text.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = gv_msg   "text-m08
      text_question         = p_text
      icon_button_1         = c_icon_okay
      icon_button_2         = c_icon_cancel
      default_button        = c_2
      display_cancel_button = ''
    IMPORTING
      answer                = p_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.                    " CONFIRM_JOB



*&---------------------------------------------------------------------*
*&      Form  ZTDA_POSTS_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ztda_posts_delete .

  DATA : ls_data          LIKE LINE OF it_data,
         lv_aclno         TYPE zdl_post_title,
         lv_title         TYPE zdl_post_title,
         lv_desc          TYPE zemetadesc.
  DATA : post_desc        TYPE zemetadesc,  " post title
         meta_value       TYPE zemetaval,   " vin number
         post_desc_c      TYPE zemetadesc,  " post title
         meta_value_c     TYPE zemetaval.   " vin number
  CASE g_proc.
    WHEN 'B' OR 'L'. "Buyback change
      lv_aclno = is_data-acl_no.
      lv_title = is_data-acl_no.
      lv_desc  = is_data-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_status      = 'D'
           WHERE post_id          = is_data-post_id
             AND post_title       = lv_aclno.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.

    WHEN 'C' OR 'R'. "Claim change & Reclaim change
      lv_desc  = is_data-post_title.
      TRANSLATE lv_title   TO UPPER CASE.
      CONDENSE  lv_title   NO-GAPS.
      TRANSLATE lv_desc    TO UPPER CASE.
      CONDENSE  lv_desc    NO-GAPS.
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_status      = 'D'
           WHERE post_id          = is_data-post_id.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.

    WHEN 'V'. "Vendor Files change
* ztda_posts Update
      TRY.
          UPDATE ztda_posts
             SET post_author      = sy-uname
                 post_modified    = sy-datum
                 post_modified_tm = sy-uzeit
                 post_status      = 'D'
           WHERE post_id          = is_data-post_id.

        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Error in update ! "POSTS"` TYPE 'I'.
      ENDTRY.
  ENDCASE.
  COMMIT WORK.

ENDFORM.                    " ZTDA_POSTS_DELETE



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_LEGAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM input_data_check_legal  USING    p_flg.

  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_vin_no   TYPE ze_vin,
          lv_category TYPE char10,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80).

* VIN Number check
  IF is_buyback-acl_no IS NOT INITIAL.
    PERFORM acl_check USING lv_flag is_buyback-acl_no.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m02      "ACL Number does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-ACL_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* VIN Number check
  IF is_buyback-vin_no IS NOT INITIAL.
    PERFORM vin_check USING lv_flag is_buyback-vin_no.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m03      "VIN Number does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-VIN_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* Last name check
  IF g_lname IS NOT INITIAL.
    lv_flag = 'L'.
    PERFORM name_check USING lv_flag g_lname.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s039 WITH 'Last name'       "Last name does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_LNAME'.
      EXIT.
    ENDIF.
  ENDIF.

* First name check
  IF g_fname IS NOT INITIAL.
    lv_flag = 'F'.
    PERFORM name_check USING lv_flag g_fname.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s039 WITH 'First name'       "First name does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_FNAME'.
      EXIT.
    ENDIF.
  ENDIF.

* title check
  IF g_title IS NOT INITIAL.
    PERFORM title_check USING lv_flag g_title.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m06      "This title record does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_TITLE'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_DATA_CHECK_LEGAL



*&---------------------------------------------------------------------*
*&      Form  NAME_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*      -->P_G_LNAME  text
*----------------------------------------------------------------------*
FORM name_check  USING   p_flag
                         p_name.

  DATA : lv_name         TYPE zedalname.
  CLEAR: ztda_meta_name.
  lv_name = p_name.
  TRANSLATE lv_name TO UPPER CASE.
  CONDENSE lv_name NO-GAPS.
  IF p_flag = 'L'.
    SELECT SINGLE last_name  INTO p_name
      FROM ztda_meta_name
     WHERE last_name_c = lv_name.
  ELSE.
    SELECT SINGLE first_name  INTO p_name
      FROM ztda_meta_name
     WHERE first_name_c = lv_name.
  ENDIF.
  IF sy-subrc NE 0.
    p_flag = 'N'.
  ELSE.
    CLEAR p_flag.
  ENDIF.

ENDFORM.                    " NAME_CHECK



*&---------------------------------------------------------------------*
*&      Form  LEGAL_DATA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM legal_data_get  USING    p_flg.

  DATA : lv_meta_key     TYPE zemetakey,
         lv_acl          TYPE zdl_post_title,
         lv_vin          TYPE zemetaval,
         lv_desc         TYPE zemetadesc,
         lv_status       TYPE zdl_post_status,
         lv_fname        TYPE zedafname,
         lv_lname        TYPE zedalname,
         cond_syntax     TYPE string,
         lv_flag,               "Return value: N-Nothing,
         wtab(72)  OCCURS 100 WITH HEADER LINE,
         and(4).

  lv_status   = gc_a.      "A  CONCATENATE 'CARRID = ' '' CARR_ID '''' INTO WTAB.
  CONCATENATE 'POST_STATUS = ' 'LV_STATUS' INTO wtab SEPARATED BY space.
  APPEND wtab.
  and = 'AND'.
  CLEAR: it_data, it_data[].
  IF is_buyback-acl_no IS NOT INITIAL.
    lv_acl               =    is_buyback-acl_no.
    CONCATENATE and 'POST_TITLE = ' 'LV_ACL' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF is_buyback-vin_no IS NOT INITIAL.
    lv_vin               =    is_buyback-vin_no.
    CONCATENATE and 'META_VALUE = ' 'LV_VIN' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF g_title IS NOT INITIAL.
    CONCATENATE '%' g_title '%' INTO lv_desc.
    CONDENSE lv_desc NO-GAPS.
    TRANSLATE lv_desc TO UPPER CASE.
    CONCATENATE and 'POST_DESC_C LIKE ' 'LV_DESC' INTO wtab
                SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'TAXONOMY_ID = ' 'G_TID' INTO wtab SEPARATED BY space.
  APPEND wtab.
  wtab = '.'.     APPEND wtab.

  CLEAR: gt_data[], gt_data.
  TRY.
      SELECT * FROM zvda_arch01
        INTO CORRESPONDING FIELDS OF TABLE gt_data
       WHERE (wtab).
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Wrong WHERE condition!` TYPE 'I'.
  ENDTRY.

  IF gt_data[] IS INITIAL.
    p_flg = 'N'.
    MESSAGE s999 WITH text-m12      "No data found !
    DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_data[], wtab[].
  IF g_fname IS NOT INITIAL.
    lv_fname = g_fname.
    CONDENSE lv_fname NO-GAPS.
    TRANSLATE lv_fname TO UPPER CASE.
    CONCATENATE 'FIRST_NAME_C = ' 'LV_FNAME' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  IF g_lname IS NOT INITIAL.
    lv_lname = g_lname.
    CONDENSE lv_lname NO-GAPS.
    TRANSLATE lv_lname TO UPPER CASE.
    IF g_fname IS NOT INITIAL.
      CONCATENATE and 'LAST_NAME_C = ' 'LV_LNAME' INTO wtab SEPARATED BY space.
    ELSE.
      CONCATENATE 'LAST_NAME_C = ' 'LV_LNAME' INTO wtab SEPARATED BY space.
    ENDIF.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'POST_ID = ' 'IT_DATA-POST_ID' INTO wtab
              SEPARATED BY space.
  APPEND wtab.
  IF wtab[] IS NOT INITIAL.
    wtab = '.'.     APPEND wtab.
  ENDIF.

  LOOP AT gt_data INTO gs_data.
    CLEAR: it_data.
    MOVE-CORRESPONDING gs_data TO it_data.

    it_data-acl_no = gs_data-post_title.
    it_data-vin_no = gs_data-meta_value.
    CLEAR: it_data-post_title.
    it_data-post_title = gs_data-post_desc.
    IF g_lname IS INITIAL AND g_fname IS INITIAL.
      PERFORM name_get_fl USING it_data-post_id.
      it_data-last_name  = ztda_meta_name-last_name.
      it_data-first_name = ztda_meta_name-first_name.
    ELSE.
      TRY.
          SELECT SINGLE * FROM ztda_meta_name
           WHERE (wtab).
        CATCH cx_sy_dynamic_osql_error.
          MESSAGE `Wrong WHERE condition!` TYPE 'I'.
      ENDTRY.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      it_data-last_name  = ztda_meta_name-last_name.
      it_data-first_name = ztda_meta_name-first_name.
    ENDIF.
    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " LEGAL_DATA_GET



*&---------------------------------------------------------------------*
*&      Form  NAME_GET_FL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_POST_ID  text

*----------------------------------------------------------------------*
FORM name_get_fl  USING    p_post_id.

  CLEAR: ztda_meta_name.
  SELECT SINGLE * FROM ztda_meta_name
   WHERE post_id = p_post_id.

ENDFORM.                    " NAME_GET_FL



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_CLAIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM input_data_check_claim  USING    p_flg.

  DATA  : lv_date     TYPE dats,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80),
          w_i         TYPE i.


* VIN Number check
  IF g_year IS INITIAL.
    MESSAGE s999 WITH 'Enter the year, Please !'
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'G_YEAR'.
    EXIT.
  ELSE.
    CONCATENATE g_year '0101' INTO lv_date.
    w_i = lv_date.
    IF w_i = 0.
      MESSAGE s999 WITH 'Invalid Year'
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_YEAR'.
      EXIT.
    ENDIF.
  ENDIF.

* title check
  IF g_title IS NOT INITIAL.
    PERFORM title_check USING lv_flag g_title.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m06      "This title record does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_TITLE'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_DATA_CHECK_CLAIM



*&---------------------------------------------------------------------*
*&      Form  CLAIM_DATA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM claim_data_get  USING    p_flg.

  DATA : lv_meta_key     TYPE zemetakey,
         lv_acl          TYPE zdl_post_title,
         lv_vin          TYPE zemetaval,
         lv_desc         TYPE zemetadesc,
         lv_status       TYPE zdl_post_status,
         lv_fname        TYPE zedafname,
         lv_lname        TYPE zedalname,
         lv_yearmon      TYPE spmon,
         cond_syntax     TYPE string,
         lv_flag,               "Return value: N-Nothing,
         wtab(72)  OCCURS 100 WITH HEADER LINE,
         and(4).

  lv_status   = gc_a.      "A  CONCATENATE 'CARRID = ' '' CARR_ID '''' INTO WTAB.
  CONCATENATE 'POST_STATUS = ' 'LV_STATUS' INTO wtab SEPARATED BY space.
  APPEND wtab.
  and = 'AND'.
  CLEAR: it_data, it_data[].
  IF g_year IS NOT INITIAL.
    CONCATENATE g_year '%' INTO lv_yearmon.
    CONCATENATE and 'POST_MONTH LIKE ' 'LV_YEARMON' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF g_title IS NOT INITIAL.
    CONCATENATE '%' g_title '%' INTO lv_desc.
    CONDENSE lv_desc NO-GAPS.
    TRANSLATE lv_desc TO UPPER CASE.
    CONCATENATE and 'POST_DESC_C LIKE ' 'LV_DESC' INTO wtab
                SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'TAXONOMY_ID = ' 'G_TID' INTO wtab SEPARATED BY space.
  APPEND wtab.
  wtab = '.'.     APPEND wtab.

  CLEAR: gt_data[], gt_data.
  TRY.
      SELECT * FROM zvda_arch01
        INTO CORRESPONDING FIELDS OF TABLE gt_data
       WHERE (wtab).
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Wrong WHERE condition!` TYPE 'I'.
  ENDTRY.

  IF gt_data[] IS INITIAL.
    p_flg = 'N'.
    MESSAGE s999 WITH text-m12      "No data found !
    DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_data[], wtab[].
  IF g_fname IS NOT INITIAL.
    lv_fname = g_fname.
    CONDENSE lv_fname NO-GAPS.
    TRANSLATE lv_fname TO UPPER CASE.
    CONCATENATE 'FIRST_NAME_C = ' 'LV_FNAME' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  IF g_lname IS NOT INITIAL.
    lv_lname = g_lname.
    CONDENSE lv_lname NO-GAPS.
    TRANSLATE lv_lname TO UPPER CASE.
    IF g_fname IS NOT INITIAL.
      CONCATENATE and 'LAST_NAME_C = ' 'LV_LNAME' INTO wtab SEPARATED BY space.
    ELSE.
      CONCATENATE 'LAST_NAME_C = ' 'LV_LNAME' INTO wtab SEPARATED BY space.
    ENDIF.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'POST_ID = ' 'IT_DATA-POST_ID' INTO wtab
              SEPARATED BY space.
  APPEND wtab.
  IF wtab[] IS NOT INITIAL.
    wtab = '.'.     APPEND wtab.
  ENDIF.

  LOOP AT gt_data INTO gs_data.
    CLEAR: it_data.
    MOVE-CORRESPONDING gs_data TO it_data.

    it_data-acl_no = gs_data-post_title.
    it_data-vin_no = gs_data-meta_value.
    CLEAR: it_data-post_title.
    it_data-post_title = gs_data-post_desc.
    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " CLAIM_DATA_GET



*&---------------------------------------------------------------------*
*&      Module  GET_SPMON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_spmon INPUT.

  DATA: lv_spmon TYPE spmon.

  MOVE: sy-datum(6) TO lv_spmon.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
    IMPORTING
      selected_month             = ztda_posts-post_month
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " GET_SPMON  INPUT



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_VONDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM input_data_check_vondor  USING    p_flg.

  DATA : lv_vendor        TYPE zdl_post_title
       , lv_flag               "Return value: N-Nothing,
       .
  IF lfa1-lifnr IS INITIAL.
    MESSAGE s999 WITH 'Enter the Vendor Code, Please !'
    DISPLAY LIKE 'E'.
    g_flg = 'E'.
    g_fldname = 'LFA1-LIFNR'.
    EXIT.
  ELSE.
    lv_vendor = lfa1-lifnr.
    TRANSLATE lv_vendor TO UPPER CASE.

    SELECT SINGLE * FROM ztda_posts
     WHERE post_status = 'A'
       AND post_title_c  = lv_vendor.
    IF sy-subrc NE 0.
      MESSAGE s999 WITH 'Unregistered Vendor Code.'
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'LFA1-LIFNR'.
      EXIT.
    ENDIF.
  ENDIF.

* title check
  IF g_title IS NOT INITIAL.
    PERFORM title_check USING lv_flag g_title.
    IF lv_flag IS NOT INITIAL.
      MESSAGE s999 WITH text-m06      "This title record does not exist !
      DISPLAY LIKE 'E'.
      g_flg = 'E'.
      g_fldname = 'G_TITLE'.
      EXIT.
    ENDIF.
  ENDIF.

  CASE 'X'.
    WHEN cb1. g_meta_key = gc_agree.
    WHEN cb2. g_meta_key = gc_meeting.
    WHEN cb3. g_meta_key = gc_report.
    WHEN cb4. g_meta_key = gc_object.
  ENDCASE.

ENDFORM.                    " INPUT_DATA_CHECK_VONDOR



*&---------------------------------------------------------------------*
*&      Form  VENFOR_DATA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_FLG  text
*----------------------------------------------------------------------*
FORM venfor_data_get  USING    p_flg.

  DATA : lv_meta_key     TYPE zemetakey,
         lv_title        TYPE zdl_post_title,
         lv_vin          TYPE zemetaval,
         lv_desc         TYPE zemetadesc,
         lv_status       TYPE zdl_post_status,
         lv_fname        TYPE zedafname,
         lv_lname        TYPE zedalname,
         lv_yearmon      TYPE spmon,
         cond_syntax     TYPE string,
         lv_flag,               "Return value: N-Nothing,
         wtab(72)  OCCURS 100 WITH HEADER LINE,
         and(4).

  lv_status   = gc_a.      "A  CONCATENATE 'CARRID = ' '' CARR_ID '''' INTO WTAB.
  CONCATENATE 'POST_STATUS = ' 'LV_STATUS' INTO wtab SEPARATED BY space.
  APPEND wtab.
  and = 'AND'.
  CONCATENATE and 'META_KEY = ' 'G_META_KEY' INTO wtab SEPARATED BY space.
  APPEND wtab.
  and = 'AND'.

  IF lfa1-lifnr IS NOT INITIAL.
    lv_title = lfa1-lifnr.
    TRANSLATE lv_title TO UPPER CASE.
    CONCATENATE and 'POST_TITLE_C = ' 'LV_TITLE' INTO wtab SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.

  IF g_title IS NOT INITIAL.
    CONCATENATE '%' g_title '%' INTO lv_desc.
    CONDENSE lv_desc NO-GAPS.
    TRANSLATE lv_desc TO UPPER CASE.
    CONCATENATE and 'POST_DESC_C LIKE ' 'LV_DESC' INTO wtab
                SEPARATED BY space.
    APPEND wtab.
    and = 'AND'.
  ENDIF.
  CONCATENATE and 'TAXONOMY_ID = ' 'G_TID' INTO wtab SEPARATED BY space.
  APPEND wtab.
  wtab = '.'.     APPEND wtab.

  CLEAR: gt_data[], gt_data.
  TRY.
      SELECT * FROM zvda_arch01
        INTO CORRESPONDING FIELDS OF TABLE gt_data
       WHERE (wtab).
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Wrong WHERE condition!` TYPE 'I'.
  ENDTRY.

  IF gt_data[] IS INITIAL.
    p_flg = 'N'.
    MESSAGE s999 WITH text-m12      "No data found !
    DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_data[], wtab[].
  LOOP AT gt_data INTO gs_data.
    CLEAR: it_data.
    MOVE-CORRESPONDING gs_data TO it_data.
    CLEAR: it_data-post_title.
    it_data-post_title = gs_data-post_desc.
    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " VENFOR_DATA_GET



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*----------------------------------------------------------------------*
FORM input_data_check_re  USING    p_flag.
  DATA  : lv_acl_no   TYPE zacln,  "9 digit char.
          lv_vin_no   TYPE ze_vin,
          lv_category TYPE char10,
          lv_flag,               "Return value: N-Nothing,
          lv_msg(80).


* VIN Number check
  IF p_flag = 'A'.
    CLEAR p_flag.
***    PERFORM acl_check USING lv_flag is_buyback-acl_no.
    CLEAR: ztsd_acm_h.
    TRANSLATE is_buyback-acl_no TO UPPER CASE.
    SELECT SINGLE zacln INTO ztsd_acm_h-zacln
      FROM ztsd_acm_h
     WHERE zacln = is_buyback-acl_no.

    IF sy-subrc NE 0.
      lv_flag = 'N'.
      MESSAGE s999 WITH text-m02      "ACL Number does not exist !
      DISPLAY LIKE 'E'.
      p_flag = g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-ACL_NO'.
      EXIT.
    ENDIF.
  ENDIF.

* VIN Number check
  IF p_flag = 'V'.
    CLEAR p_flag.
***    PERFORM vin_check USING lv_flag is_buyback-vin_no.
    CLEAR: ztpp_vm, p_flag.
    TRANSLATE is_buyback-vin_no TO UPPER CASE.
    SELECT SINGLE vin INTO ztpp_vm-vin
      FROM ztpp_vm
     WHERE vin = is_buyback-vin_no.

    IF sy-subrc NE 0.
      lv_flag = 'N'.
      MESSAGE s999 WITH text-m03      "VIN Number does not exist !
      DISPLAY LIKE 'E'.
      p_flag = g_flg = 'E'.
      g_fldname = 'IS_BUYBACK-VIN_NO'.
      EXIT.
    ENDIF.
  ENDIF.

**** title check
***  IF g_title IS NOT INITIAL.
***    PERFORM title_check USING lv_flag g_title.
***    IF lv_flag IS NOT INITIAL.
***      MESSAGE s999 WITH text-m06      "This title's record does not exist !
***      DISPLAY LIKE 'E'.
***      p_flag = g_flg = 'E'.
***      g_fldname = 'G_TITLE'.
***      EXIT.
***    ENDIF.
***  ENDIF.

ENDFORM.                    " INPUT_DATA_CHECK_RE
