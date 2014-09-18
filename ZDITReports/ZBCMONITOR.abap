REPORT zbcmonitor MESSAGE-ID 03 LINE-SIZE 255 NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------
* Program ID        : ZBCMONITOR
* Title             : [IT] Check system for critical problems
* Created on        : 04/19/2010
* Created by        : I.G.MOON
* Specifications By : Andy Choi
*----------------------------------------------------------------------
*
*
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

TABLES : usr21,seqg3.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

************************************************************************
* internal table for list
************************************************************************
DATA: BEGIN OF enq OCCURS 0.
        INCLUDE STRUCTURE seqg3.
DATA: END OF enq.
************************************************************************
* internal structure for storing the select specification
************************************************************************
DATA: BEGIN OF sel.
        INCLUDE STRUCTURE seqg3.
DATA: END OF sel.

DATA: BEGIN OF i_lock_err OCCURS 0.
        INCLUDE STRUCTURE seqg3.
DATA: time_diff TYPE p.
DATA: END OF i_lock_err.

DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
        it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        gd_cnt TYPE i,
        gd_sent_all(1) TYPE c,
        gd_doc_data LIKE sodocchgi1,
        gd_error TYPE sy-subrc.

DATA:   it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

FIELD-SYMBOLS : <s> TYPE ANY.
DATA tmp_t(20).
DATA tmp_n(2) TYPE n.

*--- CPIC Connection Control -------------------------------------------
DATA: subrc TYPE i.
*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS gname FOR seqg3-gname NO INTERVALS.
SELECT-OPTIONS guname FOR seqg3-gname NO INTERVALS.

PARAMETERS : "gname TYPE eqegraname,
             gclient TYPE eqeclient DEFAULT sy-mandt,
             "guname TYPE eqeuname,
             p_sec TYPE i DEFAULT '3600'.

SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS:  c_emailu AS CHECKBOX.
PARAMETERS:  c_emailb AS CHECKBOX.
PARAMETERS:  c_emaila AS CHECKBOX.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:  email01(100) DEFAULT ''.
PARAMETERS:  email02(100) DEFAULT ''.
PARAMETERS:  email03(100) DEFAULT ''.
PARAMETERS:  email04(100) DEFAULT ''.
PARAMETERS:  email05(100) DEFAULT ''.
PARAMETERS:  email06(100) DEFAULT ''.
PARAMETERS:  email07(100) DEFAULT ''.
PARAMETERS:  email08(100) DEFAULT ''.
PARAMETERS:  email09(100) DEFAULT ''.
PARAMETERS:  email10(100) DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.

  PERFORM build_list.

END-OF-SELECTION.

  DATA : $p_sec(20), $alert_title(100).
  DATA p_h TYPE p DECIMALS 4.

  p_h = p_sec / 3600.
  WRITE p_h TO $p_sec DECIMALS 2.. " = p_sec .
  CONCATENATE '[Alert] HMMA - Long locked SAP table' ' '  INTO
$alert_title.

  __cls it_receivers.

  IF c_emailu EQ true.
    LOOP AT i_lock_err.
      PERFORM get_email_address USING i_lock_err-guname.
    ENDLOOP.
  ENDIF.

  IF c_emailb EQ true.

    it_receivers-receiver = 'hmmasapbasis@hmmausa.com'.
    it_receivers-rec_type = 'U'.
    it_receivers-com_type = 'INT'.
    it_receivers-notif_del = ' '.
    it_receivers-notif_ndel = ' '.
    APPEND it_receivers.

  ENDIF.

  IF c_emaila EQ true.

    DO 10 TIMES.
      tmp_n = sy-index.
      CONCATENATE 'EMAIL' tmp_n INTO tmp_t.
      ASSIGN : (tmp_t) TO <s>.

      IF NOT <s> IS INITIAL.
        it_receivers-receiver = <s>.
        it_receivers-rec_type = 'U'.
        it_receivers-com_type = 'INT'.
        it_receivers-notif_del = ' '.
        it_receivers-notif_ndel = ' '.
        APPEND it_receivers.
        CLEAR <s>.
      ENDIF.
    ENDDO.

  ENDIF.

  READ TABLE i_lock_err INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM populate_data_for_output.
    PERFORM send_out USING  $alert_title.
  ENDIF.

  READ TABLE it_mail INDEX 1.
  IF sy-subrc EQ 0.
    LOOP AT it_mail.
      WRITE :/ it_mail-line.
    ENDLOOP.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  build_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_list.

  DATA: lgname    LIKE  enq-gname.
  DATA: lgarg     LIKE  enq-garg.
  DATA: lgtarg    LIKE  enq-gtarg.
  DATA: lguname   LIKE  enq-guname.

  DATA: cur_date LIKE sy-datum,
        cur_time LIKE sy-uzeit.
  DATA e_time_diff LIKE  ltak-istwm.
  DATA p_time_diff TYPE p DECIMALS 4.

*** initializations ****************************************************
  REFRESH enq.

  lgname   = gname.
  lguname  = guname.
*  lgarg    = garg.
*  lgtarg   = garg.

** first try via RFC, retry up to 3 times ******************************
*  DO 3 TIMES.
*    CALL FUNCTION 'ENQUEUE_READ'
*      EXPORTING
*        guname  = lguname
*        gname   = lgname
*        garg    = lgarg
*      IMPORTING
*        subrc   = subrc
*      TABLES
*        enq     = enq
*      EXCEPTIONS
*        OTHERS  = 1.
*    IF sy-subrc = 0.
*      EXIT.
*    ENDIF.
*  ENDDO.

*  IF sy-subrc <> 0.
*
*  CALL FUNCTION 'ENQUE_READ'
*       EXPORTING
*            guname = '*'
**            gname  = lgname
**            garg   = lgarg
*       IMPORTING
*            subrc  = subrc
*       TABLES
*            enq    = enq.

  CALL FUNCTION 'ENQUEUE_READ'
       EXPORTING
            guname = '*'
       IMPORTING
            subrc  = subrc
       TABLES
            enq    = enq.

*  ENDIF.

  cur_date = sy-datum.
  cur_time = sy-uzeit.

  __cls i_lock_err.

  LOOP AT enq.

    IF enq-gname IN gname AND enq-guname IN guname.
      CALL FUNCTION 'L_TO_TIME_DIFF'
           EXPORTING
                i_start_date     = enq-gtdate
                i_start_time     = enq-gttime
                i_end_date       = cur_date
                i_end_time       = cur_time
                i_time_uom       = 'MIN'
           IMPORTING
                e_time_diff      = e_time_diff
           EXCEPTIONS
                input_data_empty = 1
                OTHERS           = 2.
      IF sy-subrc <> 0.
      ENDIF.

      IF NOT p_sec IS INITIAL.
      ELSE.
        p_sec = 0.
      ENDIF.

      p_time_diff = e_time_diff * 60.
      IF p_time_diff >= p_sec.
        MOVE-CORRESPONDING enq TO i_lock_err.
        i_lock_err-time_diff = p_time_diff.
        APPEND i_lock_err.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " build_list
*&---------------------------------------------------------------------*
*&      Form  get_email_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_email_address USING p_guname.

  DATA: g_person_number LIKE adrp-persnumber,
        g_address_number LIKE adrc-addrnumber,
        new_address_number LIKE adrc-addrnumber,
        new_address_handle LIKE addr1_sel-addrhandle,
        g_person_handle LIKE szad_field-handle,
        g_address_handle LIKE szad_field-handle,
        g_nation LIKE adrc-nation,
        g_date_from LIKE adrc-date_from.

  DATA: int_adr6 LIKE adsmtp OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE * FROM usr21 WHERE bname EQ p_guname.
  IF sy-subrc EQ 0.
    g_address_number = usr21-addrnumber.
    g_person_number = usr21-persnumber.

    CALL FUNCTION 'ADDR_PERS_COMP_COMM_GET'
         EXPORTING
              address_handle    = g_address_handle
              address_number    = g_address_number
*                 DATE_FROM         = '00010101'
*                 LANGUAGE          = SY-LANGU
              table_type        = 'ADSMTP'
              person_handle     = g_person_handle
              person_number     = g_person_number
*            IMPORTING
*                 RETURNCODE        =
         TABLES
              comm_table        = int_adr6
*                 ERROR_TABLE       =
       EXCEPTIONS
*                 PARAMETER_ERROR   = 1
*                 ADDRESS_NOT_EXIST = 2
*                 PERSON_NOT_EXIST  = 3
*                 INTERNAL_ERROR    = 4
            OTHERS            = 5
              .
    IF sy-subrc = 0.
      LOOP AT int_adr6 WHERE flgdefault NE space.
        it_receivers-receiver = int_adr6-smtp_addr.
        it_receivers-rec_type = 'U'.
        it_receivers-com_type = 'INT'.
        it_receivers-notif_del = ' '.
        it_receivers-notif_ndel = ' '.
        APPEND it_receivers.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_email_address
*&---------------------------------------------------------------------*
*&      Form  send_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_EMAIL  text
*----------------------------------------------------------------------*
FORM send_out USING  p_title.

  DATA: l_subject(50) TYPE c.
  DATA: w_tab_lines TYPE i.

  l_subject = p_title.

  gd_doc_data-doc_size = 1.
  DESCRIBE TABLE it_mail LINES w_tab_lines.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  __cls it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 1.
  it_packing_list-body_start = 1.
  it_packing_list-body_num = w_tab_lines.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  DELETE it_receivers WHERE receiver IS initial.
  READ TABLE it_receivers INDEX 1.

  CHECK sy-subrc EQ 0.

** Call the FM to post the message to SAPMAIL
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*       EXPORTING
*            document_data              = gd_doc_data
*            put_in_outbox              = 'X'
*       IMPORTING
*            sent_to_all                = gd_sent_all
*       TABLES
*            packing_list               = it_packing_list
*            contents_txt               = it_mail
*            receivers                  = it_receivers
*       EXCEPTIONS
*            too_many_receivers         = 1
*            document_not_sent          = 2
*            document_type_not_exist    = 3
*            operation_no_authorization = 4
*            parameter_error            = 5
*            x_error                    = 6
*            enqueue_error              = 7
*            OTHERS                     = 8.

* Call the FM to post the message to SAPMAIL

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
      put_in_outbox              = ' '
          COMMIT_WORK   = 'X'
*    IMPORTING
*      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.


ENDFORM.                    " send_out
*&---------------------------------------------------------------------*
*&      Form  populate_data_for_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_data_for_output.
  DATA: l_message TYPE so_text255,
        l_kbetr(13),
        l_string(15).
  DATA: v_lf TYPE x VALUE '0A'.
  DATA: l_h TYPE p DECIMALS 4.

  APPEND '(Warning!)' TO it_mail.
  APPEND '=================================='  TO
it_mail.

  APPEND
  'SAP Tables have been locked too long.' TO
it_mail.

  APPEND '=================================='  TO
it_mail.

  LOOP AT i_lock_err.
    l_h = i_lock_err-time_diff / 3600.
    WRITE l_h TO l_string DECIMALS 2.
    CONDENSE l_string NO-GAPS.
    CONCATENATE '* Table:' i_lock_err-gname
    ' has been locked by User:' i_lock_err-guname  ' for^'
    l_string ' Hours'
    INTO l_message.
    REPLACE '^' WITH ' ' INTO l_message.
    APPEND l_message TO it_mail.
  ENDLOOP.

  LOOP AT it_mail FROM 4.
    CONCATENATE it_mail-line v_lf INTO it_mail-line.
    MODIFY it_mail INDEX sy-tabix TRANSPORTING line.
  ENDLOOP.

ENDFORM.                    " populate_data_for_output
