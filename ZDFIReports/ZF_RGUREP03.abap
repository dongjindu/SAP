REPORT zf_rgurep03 MESSAGE-ID gg.

*This program deletes actual line items and corrects the totals

*Types
TYPE-POOLS: gusl,                      "Selection processor
            kkblo.
*Tables
TABLES: glu1, t800a, t881.

*Select options
PARAMETERS:     p_rldnr  LIKE glu1-rldnr OBLIGATORY.
PARAMETERS:     p_bukrs  LIKE glu1-bukrs,
                p_rcomp  LIKE glu1-rcomp,
                p_ryear  LIKE glu1-ryear OBLIGATORY.
SELECT-OPTIONS sel_doc FOR glu1-docnr OBLIGATORY.
PARAMETERS: testlauf AS CHECKBOX DEFAULT 'X'.

*Internal table for select where clause
DATA: t_selection TYPE gusl_t_selection.
DATA: s_range TYPE gusl_s_range,
      t_range TYPE gusl_t_range,
      s_selection TYPE gusl_s_selection.


*Internal tables for selecting and posting
DATA: t_sel_glu1 TYPE gusl_t_glu1.
DATA: t_post_glu1 LIKE glu1 OCCURS 0 WITH HEADER LINE.
DATA: t_list_glu1 TYPE gusl_t_glu1.
DATA: t_glu1_add LIKE rgiad2 OCCURS 0 WITH HEADER LINE.
DATA: t_used LIKE rgiuse OCCURS 0 WITH HEADER LINE.
DATA:   h_funcname LIKE tfdir-funcname VALUE 'G_GLDB_POSTING_$',
        t_activity LIKE rgiact OCCURS 0 WITH HEADER LINE,
        h_count_items LIKE sy-tabix,
        h_count_glidxb LIKE sy-tabix,
        h_count_glidxa LIKE sy-tabix,
        h_org_name(6),
        answer.
*******************Start of processing********************************

AT SELECTION-SCREEN.
*Check ledger
  SELECT SINGLE * FROM t881 WHERE rldnr = p_rldnr.
  IF sy-subrc NE 0.
    SET CURSOR FIELD 'P_RLDNR'.
    MESSAGE e448 WITH p_rldnr.
  ELSE.
    SELECT SINGLE * FROM t800a WHERE tab = t881-tab.
    IF sy-subrc NE 0 OR t800a-ntable IS INITIAL.
      SET CURSOR FIELD 'P_RLDNR'.
      MESSAGE e449.
    ELSE.
    ENDIF.
  ENDIF.
  IF t800a-comptab IS INITIAL.
    IF t800a-objtable IS INITIAL.
      h_org_name = 'BUKRS'.
    ELSE.
      h_org_name = 'RBUKRS'.
    ENDIF.
  ELSE.
    h_org_name = 'RCOMP'.
  ENDIF.

**Check Company code
*  IF h_org_name = 'RCOMP'.
*    IF p_rcomp IS INITIAL
*    OR p_bukrs NE space.
*      SET CURSOR FIELD 'P_RCOMP'.
*      MESSAGE e450.
*    ENDIF.
*  ELSE.
*    IF p_bukrs IS INITIAL
*    OR p_rcomp NE space.
*      SET CURSOR FIELD 'P_BUKRS'.
*      MESSAGE e451.
*    ENDIF.
*  ENDIF.
*

********************Begin of processing*********************************
START-OF-SELECTION.

*Security question
*  CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*       EXPORTING
*            TEXTLINE1 = TEXT-001
*            TITEL     = TEXT-002
*       IMPORTING
*            ANSWER    = ANSWER.
*  IF ANSWER = 'N' OR ANSWER = 'A'.
*    STOP.
*  ENDIF.

*Delete the line items and correct totals
  PERFORM e01_delete_and_correct TABLES sel_doc
                                 USING p_rldnr
                                       p_bukrs
                                       p_rcomp
                                       p_ryear.
  IF testlauf EQ ' '.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  PERFORM e02_list_records USING p_rldnr.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM E01_DELETE_AND_CORRECT                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E01_RLDNR                                                     *
*  -->  E01_BUKRS                                                     *
*  -->  E01_RCOMP                                                     *
*  -->  E01_RYEAR                                                     *
*  -->  E01_SEL_DOC                                                   *
*---------------------------------------------------------------------*
FORM e01_delete_and_correct TABLES e01_sel_doc STRUCTURE sel_doc
                            USING e01_rldnr LIKE glu1-rldnr
                                  e01_bukrs LIKE glu1-rbukrs
                                  e01_rcomp LIKE glu1-rcomp
                                  e01_ryear LIKE glu1-ryear.

  DATA: h_records_selected LIKE sy-tabix,
        h_records_deleted  LIKE sy-tabix.
*Authority check for deleting records
  CALL FUNCTION 'G_ADMI_AUTHORITY_CHECK'
       EXPORTING
            activity = '04'.

*Reset global information
  CLEAR: t_selection, s_range, t_range, s_selection,
         t_sel_glu1, t_post_glu1, t_glu1_add, t_used.
  REFRESH: t_selection, t_range,
           t_sel_glu1, t_post_glu1, t_glu1_add, t_used.

*Get table information
  SELECT SINGLE * FROM t881 WHERE rldnr = e01_rldnr.
  CHECK sy-subrc = 0 AND t881-tab NE space.
  SELECT SINGLE * FROM t800a WHERE tab = t881-tab.
  CHECK sy-subrc = 0 AND t800a-ntable NE space.
  IF t800a-comptab IS INITIAL.
    IF t800a-objtable IS INITIAL.
      h_org_name = 'BUKRS'.
    ELSE.
      h_org_name = 'RBUKRS'.
    ENDIF.
  ELSE.
    h_org_name = 'RCOMP'.
  ENDIF.


*Fill select clause
*Ledger
  CLEAR: s_range, t_range, s_selection.
  s_range-sign = 'I'.
  s_range-option = 'EQ'.
  s_range-low = e01_rldnr.
  APPEND s_range TO t_range.
  s_selection-fieldname = 'RLDNR'.
  s_selection-t_range = t_range.
  APPEND s_selection TO t_selection.
*Comany code
  CLEAR: s_range, t_range, s_selection.
  IF h_org_name NE 'RCOMP'.
    s_range-sign = 'I'.
    s_range-option = 'EQ'.
    s_range-low = e01_bukrs.
    APPEND s_range TO t_range.
    s_selection-fieldname = h_org_name.
    s_selection-t_range = t_range.
    APPEND s_selection TO t_selection.
*Company
  ELSE.
    s_range-sign = 'I'.
    s_range-option = 'EQ'.
    s_range-low = e01_rcomp.
    APPEND s_range TO t_range.
    s_selection-fieldname = h_org_name.
    s_selection-t_range = t_range.
    APPEND s_selection TO t_selection.
  ENDIF.
*Fiscal year
  CLEAR: s_range, t_range, s_selection.
  s_range-sign = 'I'.
  s_range-option = 'EQ'.
  s_range-low = e01_ryear.
  APPEND s_range TO t_range.
  s_selection-fieldname = 'RYEAR'.
  s_selection-t_range = t_range.
  APPEND s_selection TO t_selection.
*Document numbers
  CLEAR: s_range, t_range, s_selection.
  LOOP AT e01_sel_doc.
    MOVE-CORRESPONDING e01_sel_doc TO s_range.
    APPEND s_range TO t_range.
  ENDLOOP.
  s_selection-fieldname = 'DOCNR'.
  s_selection-t_range = t_range.
  APPEND s_selection TO t_selection.

*Select records from database
  CALL FUNCTION 'G_TABLE_SELECT_WITH_CURSOR'
       EXPORTING
            i_tabname      = t800a-ntable
            i_selection    = t_selection
            i_zero_records = 'X'
       CHANGING
            c_t_glu1       = t_sel_glu1.

*Check if records have been found
  DESCRIBE TABLE t_sel_glu1 LINES h_records_selected.
  IF h_records_selected = 0.
    MESSAGE i453.
    EXIT.
  ENDIF.

*Store GLU1 records for list output
  APPEND LINES OF t_sel_glu1 TO t_list_glu1.

*Delete records from line item table
  PERFORM e01_init(sapfgdel) USING t800a-ntable.
  PERFORM e02_set_field(sapfgdel) USING t800a-ntable
                                        'RLDNR'
                                        e01_rldnr
                                        e01_rldnr.
  IF h_org_name NE 'RCOMP'.
    PERFORM e02_set_field(sapfgdel) USING t800a-ntable
                                          h_org_name
                                          e01_bukrs
                                          e01_bukrs.
  ELSE.
    PERFORM e02_set_field(sapfgdel) USING t800a-ntable
                                          h_org_name
                                          e01_rcomp
                                          e01_rcomp.
  ENDIF.
  PERFORM e02_set_field(sapfgdel) USING t800a-ntable
                                        'RYEAR'
                                        e01_ryear
                                        e01_ryear.
  LOOP AT e01_sel_doc.
    PERFORM e02_set_field_extended(sapfgdel)
                    USING 'DOCNR'
                          e01_sel_doc-low
                          e01_sel_doc-high
                          e01_sel_doc-option
                          e01_sel_doc-sign.
  ENDLOOP.

  PERFORM e08_delete(sapfgdel) USING t800a-ntable
                                     1000000
                                     sy-subrc
                                     h_records_deleted
                                     h_count_glidxb
                                     h_count_glidxa
                                     ' '.
  IF h_records_deleted NE h_records_selected.
    MESSAGE a454 WITH h_records_deleted"Internal error
                      h_records_selected.
  ENDIF.

*Bring records into posting form
  LOOP AT t_sel_glu1 INTO t_post_glu1.
    t_post_glu1-tsl = t_post_glu1-tsl * -1.
    t_post_glu1-hsl = t_post_glu1-hsl * -1.
    t_post_glu1-ksl = t_post_glu1-ksl * -1.
    t_post_glu1-osl = t_post_glu1-osl * -1.
    t_post_glu1-msl = t_post_glu1-msl * -1.
    t_post_glu1-asl = t_post_glu1-asl * -1.
    APPEND t_post_glu1.
    CALL FUNCTION 'G_MAX_PERIOD_AND_OFFSET_GET'
         EXPORTING
              period = t_post_glu1-poper
              table  = t881-tab
         IMPORTING
              offset = t_glu1_add-offset
              rpmax  = t_glu1_add-rpmax.
    t_glu1_add-post = 'X'.
    CLEAR t_glu1_add-glsip.
    APPEND t_glu1_add.
  ENDLOOP.

*Call function module to update T_POST_GLU1
  REPLACE '$' WITH t800a-progroup INTO h_funcname.
  t_used-tab = t800a-ntable.
  t_used-progroup = t800a-progroup.
  APPEND t_used.
  CALL FUNCTION h_funcname
       TABLES
            int_glu1     = t_post_glu1
            int_glu1_add = t_glu1_add
            int_used     = t_used
            int_activity = t_activity.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  E02_LIST_RECORDS
*&---------------------------------------------------------------------*
*       Output of line items that have been deleted
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM e02_list_records USING e02_rldnr.
  DATA: t_cathead TYPE  slis_t_fieldcat_alv,
        t_catitem TYPE  slis_t_fieldcat_alv.



  DESCRIBE TABLE t_list_glu1 LINES sy-tfill.
  CHECK sy-tfill NE 0.

  IF testlauf = ' '.
    MESSAGE s455 WITH sy-tfill t800a-ntable.
  ENDIF.

*Call function module to display records
  CALL FUNCTION 'G_GLU1_ITEMS_DISPLAY_HS'
       EXPORTING
            i_ledger    = e02_rldnr
            i_items     = t_list_glu1
            i_cwcode    = 'X'
            i_text      = text-003
       IMPORTING
            e_t_cathead = t_cathead
            e_t_catitem = t_catitem.
ENDFORM.                               " E02_LIST_RECORDS
