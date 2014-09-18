*&---------------------------------------------------------------------*
*& Report  ZRHHAPUNRELEASE                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Unrelease template even if it's already used in documents
**
*&  !!!!! Use only in exceptional cases, when advised by SAP !!!!!
*
*   Copied from HRHAP_C_ELEMENT_SET_STATUS
*&---------------------------------------------------------------------*

REPORT  zrhhapunrelease MESSAGE-ID hrhap00_template.

INCLUDE: incl_hap_status, incl_hap_global, incl_hap_elements,
         incl_hap_messages.
* tables
DATA: lt_wplog         TYPE TABLE OF wplog INITIAL SIZE 0.
DATA: lt_structure     TYPE hap_t_object_tree.
DATA: lt_objects       TYPE hap_t_hrobject.
DATA: lt_elements_xl   TYPE hap_t_appraisal_element_xl.
DATA: lt_message_range TYPE bal_range.
DATA: lt_ppenq         TYPE TABLE OF ppenq INITIAL SIZE 0.
* workareas
DATA: lw_wplog       TYPE wplog.
DATA: lw_structure   TYPE hap_s_object_tree.
DATA: lw_return      TYPE bal_s_msg.
DATA: lw_objects     TYPE hrobject.
DATA: lw_elements_xl TYPE hap_s_appraisal_element_xl.
DATA: lw_ppenq       TYPE ppenq.
* structures
DATA: ls_element_id TYPE hap_s_appraisal_element_id.
DATA: ls_msg_fields TYPE hap_s_c_ret_struc.
DATA: ls_dd07v      TYPE dd07v.
* variables
DATA: l_tabix TYPE sytabix.
DATA: l_new_istat TYPE istat_d,
      l_new_histo TYPE histo.
DATA: l_fcode TYPE fcode.
DATA: l_error.
DATA: l_va_status_change TYPE char1.
DATA: l_domain_value   TYPE domvalue_l,
      l_status_name    TYPE msgv1.
DATA: l_simulated_bc   TYPE char1.
DATA: l_subrc          TYPE sysubrc.
DATA: s_element_id TYPE  hap_s_appraisal_element_id,
      vtask TYPE  vtask VALUE 'D',
      new_status TYPE  hap_element_status VALUE c_status_unreleased,
      t_return TYPE  hap_t_msg,
      lt_return TYPE  hap_t_msg,
      lt_templates TYPE hap_t_templates,
      lw_template TYPE objec,
      char255(255),
      l_locked TYPE boole_d.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(80) comm1.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(80) comm2.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN SKIP 1.

PARAMETERS: plvar TYPE plvar DEFAULT '01',
            template TYPE hap_template_id.


INITIALIZATION.
  comm1 = 'Cancel release of appraisal template'.
  comm2 = 'USE ONLY WHEN ADVISED BY SAP!'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR template.

  CALL FUNCTION 'HRHAP_POPUP_F4_TEMPLATE'
    EXPORTING
      authority_check = '02'
      plan_version    = plvar
    IMPORTING
      t_templates     = lt_templates.

  LOOP AT lt_templates INTO lw_template.
    template = lw_template-objid.
    EXIT.
  ENDLOOP.

START-OF-SELECTION.

  s_element_id-plvar = plvar.
  s_element_id-otype = 'VA'.
  s_element_id-objid = template.
  new_status = '1'.

* read message range to do this
  CALL FUNCTION 'HRHAP_C_MESSAGE_RANGE_FILL'
    EXPORTING
      msg_type_i      = 'X'
      msg_type_e      = 'X'
      msg_type_a      = 'X'
    IMPORTING
      t_message_range = lt_message_range.

* read all effected elements
* -> e.g. for VA the complete appraisal template
  CALL FUNCTION 'HRHAP_C_CATALOG_READ_BRANCH'
    EXPORTING
      plvar              = s_element_id-plvar
      otype              = s_element_id-otype
      objid              = s_element_id-objid
      read_complete_tree = 'X'     "read the complete structure
      read_kind          = 'C'      "for B605 / B606 and B607
    IMPORTING
      s_return           = lw_return
      t_object_tree      = lt_structure.

* fill objects table
  MOVE-CORRESPONDING s_element_id TO lw_structure.
  INSERT lw_structure INTO lt_structure INDEX 1.
  LOOP AT lt_structure INTO lw_structure.
    lw_elements_xl-plvar = s_element_id-plvar.
    IF  NOT lw_structure-foreign_type IS INITIAL
    AND NOT lw_structure-foreign_id   IS INITIAL.
      lw_elements_xl-otype         = lw_structure-foreign_type.
      lw_elements_xl-objid         = lw_structure-foreign_id.
    ELSE.
      lw_elements_xl-otype         = lw_structure-otype.
      lw_elements_xl-objid         = lw_structure-objid.
    ENDIF.      "standard or foreign element ?
    lw_elements_xl-standard_type = lw_structure-otype.
    lw_elements_xl-standard_id   = lw_structure-objid.
    APPEND lw_elements_xl TO lt_elements_xl.
    lw_objects-plvar = s_element_id-plvar.
    MOVE-CORRESPONDING lw_structure TO lw_objects.
    APPEND lw_objects TO lt_objects.
  ENDLOOP.  "at complete structure

* perform business check
* -> the business check will be performed
*    for the base and all dependent objects
  IF new_status = c_status_released
  OR new_status = c_status_in_history.
    l_simulated_bc = c_true.
  ELSE.
    l_simulated_bc = c_false.
  ENDIF.
* change of the VA status ?
  IF s_element_id-otype = c_otype_va.
    l_va_status_change = c_true.
  ENDIF.     "VA?
* business check
  CALL FUNCTION 'HRHAP_C_BC_ELEMENT_SET_STATUS'
    EXPORTING
      s_element_id     = s_element_id
      t_elements       = lt_elements_xl
      va_status_change = l_va_status_change
      va_objid         = template
      new_status       = new_status
      simulated_bc     = l_simulated_bc
    IMPORTING
      t_return         = lt_return
    EXCEPTIONS
      nothing_to_do    = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
*   status hasn't been changed or wrong status given
    l_error = c_true.
  ELSEIF sy-subrc = 0.
*   check whether the action can be performed
*   -> if for all objects no errors or aborts are occured
    LOOP AT lt_return INTO lw_return
                     WHERE msgty IN lt_message_range
                     AND ( msgid <> c_message_class_catalog
                          OR msgno <> '219' ).
*     action can't be performed
      l_error = c_true.
      APPEND lw_return TO t_return.
    ENDLOOP.           "at return messages
  ENDIF.

* can the status change be performed ?
  IF NOT l_error IS INITIAL.
*   fill message
*-- element contains errors. Status change not performed
    MOVE-CORRESPONDING s_element_id TO ls_msg_fields.
    CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
      EXPORTING
        msgno          = '220'
        fill_variables = 'X'
        s_msg_fields   = ls_msg_fields
      IMPORTING
        s_return       = lw_return.
    APPEND lw_return TO t_return.
  ENDIF.       "errors in object ?

  IF l_error IS INITIAL.
* perform business check for all related elements
    LOOP AT lt_objects INTO lw_objects.
      l_tabix = sy-tabix.
      MOVE-CORRESPONDING lw_objects TO ls_element_id.
      CLEAR lt_return.
      CALL FUNCTION 'HRHAP_C_BC_ELEMENT_SET_STATUS'
        EXPORTING
          s_element_id     = ls_element_id
          va_status_change = c_true
          va_objid         = template
          s_superior_id    = s_element_id
          new_status       = new_status
          simulated_bc     = ' '
        IMPORTING
          t_return         = lt_return
        EXCEPTIONS
          nothing_to_do    = 1
          OTHERS           = 2.
      l_subrc = sy-subrc.
      READ TABLE lt_return INTO lw_return INDEX 1.
      IF    l_subrc <> 0
      OR  ( sy-subrc = 0
      AND lw_return-msgid = c_message_class_catalog
      AND lw_return-msgno = '238' )."differ. status of dep.elemen
*     return code which tells us that the status can not be changed!
        DELETE lt_objects INDEX l_tabix.
        APPEND lw_return TO t_return.
      ELSEIF sy-subrc = 0
      AND lw_return-msgid = c_message_class_catalog
      AND lw_return-msgno = '219'.     "usage in appraisal document
*   ignore this error. (Difference compared to
*   HRHAP_C_ELEMENT_SET_STATUS!)
      ELSEIF sy-subrc = 0
      AND lw_return-msgty IN lt_message_range.
*     'error' return code which tells us that the status change
*     can not be performed at all
*     action can't be performed
        l_error = c_true.
        APPEND lw_return TO t_return.
        EXIT.
      ENDIF.        "can the element status being changed?
    ENDLOOP.    "at all elements
  ENDIF.

* can the status change be performed ?
  IF NOT l_error IS INITIAL.
*   fill message
*-- element contains errors. Status change not performed
    MOVE-CORRESPONDING s_element_id TO ls_msg_fields.
    CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
      EXPORTING
        msgno          = '220'
        fill_variables = 'X'
        s_msg_fields   = ls_msg_fields
      IMPORTING
        s_return       = lw_return.
    APPEND lw_return TO t_return.
  ENDIF.       "errors in object ?

  SORT lt_objects BY plvar otype objid.        "Note 1578673
  DELETE ADJACENT DUPLICATES FROM lt_objects.  "Note 1578673

  IF l_error IS INITIAL.
* lock objects !!!
* -> foreign objects hasn't to be locked (they will not be changed)
    LOOP AT lt_objects INTO lw_objects.
*   enqueue object
      CALL FUNCTION 'HR_ENQUEUE_OBJECT'
        EXPORTING
          plvar                  = lw_objects-plvar
          otype                  = lw_objects-otype
          objid                  = lw_objects-objid
          enqueue_once           = c_true
*     IMPORTING
*       LOCK_USER              =
        EXCEPTIONS
          enqueue_failed         = 1
          objid_is_initial       = 2
          illegal_otype          = 3
          internal_error         = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
*     fill return
        CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
          EXPORTING
            msgno    = 'LOC'
          IMPORTING
            s_return = lw_return.
        APPEND lw_return TO t_return.
        l_locked = c_true.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING lw_objects TO lw_ppenq.
      APPEND lw_ppenq TO lt_ppenq.
    ENDLOOP.
    IF l_locked = c_true.
*   objects was already enqueued by another user
*   -> dequeue already enqueued objects
      CALL FUNCTION 'HR_DEQUEUE_OBJECT_LIST'
        EXPORTING
          dequeue_once           = 'X'
*     IMPORTING
*       ERROR_TABIX            =
        TABLES
          dequeue_list           = lt_ppenq
        EXCEPTIONS
          illegal_otype          = 0
          objid_is_initial       = 0
          internal_error         = 0
          OTHERS                 = 0.
      l_error = c_true.
    ENDIF.
  ENDIF.

  IF l_error IS INITIAL.
* convert internal status -> ISTAT and HISTO
    CALL FUNCTION 'HRHAP_C_0CONVERT_STATUS_DB'
      EXPORTING
        status = new_status
      IMPORTING
        istat  = l_new_istat
        histo  = l_new_histo.

* READ ALL INFOTYPES of OBJECT
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        sort                 = ' '
      TABLES
        innnn                = lt_wplog
        objects              = lt_objects
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
*   technical error occured
      CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
        EXPORTING
          msgno    = '000'
        IMPORTING
          s_return = lw_return.
      APPEND lw_return TO t_return.
      l_error = c_true.
    ENDIF.
  ENDIF.

  IF l_error IS INITIAL.

* SORT LT_WPLOG
* REASON: time constraint 1 can cause problems
*         if the entries are inserted in the wrong order !!!
    SORT lt_wplog BY mandt
                     plvar
                     otype
                     objid
                     infty
                     subty
                     istat
                     priox
                     begda.

* loop at all infotypes
    LOOP AT lt_wplog INTO lw_wplog.
      l_tabix = sy-tabix.
      IF lw_wplog-histo = l_new_histo
      AND lw_wplog-istat = l_new_istat.
*--   Element already has the correct status
        DELETE lt_wplog INDEX l_tabix.
      ELSE.
        lw_wplog-histo = l_new_histo.
        MODIFY lt_wplog FROM lw_wplog INDEX l_tabix
                        TRANSPORTING histo.
      ENDIF.
    ENDLOOP.  "at all infotypes

    CASE l_new_istat.
      WHEN c_istat_3.
        l_fcode = 'BEAN'.
      WHEN c_istat_4.
        l_fcode = 'GENE'.
    ENDCASE.

* INSERT ALL INFTY's
* ...with state FCODE:
*    1 = 'AKTI'  - active     -> set the object to status 1
*    2 = 'PLVO'  - planned    -> copies the object to a planned one
*    3 = 'BEAN'  - submitted  -> set the object to status 3
*    4 = 'GENE'  - approved   -> set the object to status 4
*    5 = 'ABLN'  - rejected   -> set the object to status 5
*   ' '= 'CUT '  - historicized
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = l_fcode
        vtask               = vtask
        order_flg           = 'X'
        old_tabnr           = 'X'
      TABLES
        innnn               = lt_wplog
      EXCEPTIONS
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
*       no authority
          CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
            EXPORTING
              msgno    = '006'
            IMPORTING
              s_return = lw_return.
        WHEN OTHERS.
*       data not saved
          CALL FUNCTION 'HRHAP_C_MESSAGE_FILL'
            EXPORTING
              msgno    = '003'
            IMPORTING
              s_return = lw_return.
      ENDCASE.
      APPEND lw_return TO t_return.
    ELSE.
*   read status text
      l_domain_value = new_status.
      CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
        EXPORTING
          name          = 'HAP_ELEMENT_STATUS'
          value         = l_domain_value
          langu         = sy-langu
*         TEXTS_ONLY    = ' '
        IMPORTING
          dd07v_wa      = ls_dd07v
        EXCEPTIONS
          not_found     = 1
          illegal_input = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
        l_status_name = new_status.
      ELSE.
        l_status_name = ls_dd07v-ddtext.
      ENDIF.
*   status changed to '...'
      CLEAR lw_return.
      lw_return-msgty = c_message_type_s.
      lw_return-msgno = '222'.
      lw_return-msgid = c_message_class_catalog.
      lw_return-msgv1 = l_status_name.
      APPEND lw_return TO t_return.
*     message where-used
      IF 1 = 2.
        MESSAGE s222 WITH lw_return-msgv1.
      ENDIF.
    ENDIF.

* DEQUEUE the enqueued objects
    CALL FUNCTION 'HR_DEQUEUE_OBJECT_LIST'
      TABLES
        dequeue_list     = lt_ppenq
      EXCEPTIONS
        illegal_otype    = 0
        objid_is_initial = 0
        internal_error   = 0
        OTHERS           = 0.
  ENDIF.

  LOOP AT t_return INTO lw_return.
    MESSAGE ID lw_return-msgid TYPE lw_return-msgty
           NUMBER lw_return-msgno
           WITH lw_return-msgv1 lw_return-msgv2
           lw_return-msgv3 lw_return-msgv4
           INTO char255.

    WRITE: / lw_return-context-value.
    WRITE 20 char255.

  ENDLOOP.
