*&---------------------------------------------------------------------*
*&  Include           ZEMM_CHANGE_PR_CLOSED_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_PR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pr_data .

  DATA : l_date_before TYPE sy-datum.

* "OLDer than Created date(Days before)
  l_date_before  = sy-datum - p_days.

  SELECT *
    FROM eban
    INTO CORRESPONDING FIELDS OF TABLE it_pr
   WHERE banfn IN s_banfn       "PR No.
     AND bsart IN s_bsart       "PR Doc.Type
     AND bstyp = 'B'            "Purch.Doc.Cat.('B' Purchase requisition)
     AND loekz = ''             "Deletion Indicator
     AND ( erdat <= l_date_before AND "PR created date
           erdat IN s_erdat          )
     AND knttp IN s_knttp       "Account Assignment Category
     AND ebakz = ''.            "PR Closed(Not yet closed)
*     AND ebeln = ''              "PO No.
*     AND ebelp = ''.             "PO item No.

* "filtering PO
  IF ( chkbx_1 = 'X' AND chkbx_2 = 'X' ) OR
     ( chkbx_1 = ''  AND chkbx_2 = ''  ).
*    "no restriction for PO
  ELSE.
    CASE 'X'.
      WHEN chkbx_1.             "Exist PO
        DELETE it_pr WHERE ebeln = ''.

      WHEN chkbx_2.             "Non-exist PO
        DELETE it_pr WHERE ebeln NE ''.
    ENDCASE.
  ENDIF.


ENDFORM.                    " GET_PR_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_default_values .

  IF p_days IS INITIAL.
    p_days = 365 * 2.     "2 years
  ENDIF.

ENDFORM.                    " SET_DEFAULT_VALUES

*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify .

  LOOP AT SCREEN.
    CASE 'X'.
      WHEN ra_1.
        IF screen-group1 = 'G2'.
          screen-invisible = 1.
          screen-input     = 0.
        ENDIF.

      WHEN ra_2.
        IF screen-group1 = 'G1'.
          screen-invisible = 1.
          screen-input     = 0.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY

*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*----------------------------------------------------------------------*
FORM selection_screen  USING    p_fname.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.*,*.*.'
      mode             = 'O'
    IMPORTING
      filename         = p_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                    " SELECTION_SCREEN


*&---------------------------------------------------------------------*
*&      Form  GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_upload_file .

  CHECK : NOT p_fname IS INITIAL.

  CLEAR   : it_excl.
  REFRESH : it_excl[].

  CALL FUNCTION 'Z_MM_EXCEL_UPLOAD'
    EXPORTING
      filename   = p_fname
      itab       = 'IT_EXCL'
      begin_line = 2
    TABLES
      outab      = it_excl.

ENDFORM.                    " GET_UPLOAD_FILE


*&---------------------------------------------------------------------*
*&      Form  GET_PR_DATA_OF_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pr_data_of_excel .

  IF NOT it_excl[] IS INITIAL.
    SELECT *
      FROM eban
      INTO CORRESPONDING FIELDS OF TABLE it_pr
       FOR ALL ENTRIES IN it_excl
     WHERE banfn = it_excl-banfn      "PR No.
       AND bnfpo = it_excl-bnfpo.     "PR item No.
  ENDIF.

ENDFORM.                    " GET_PR_DATA_OF_EXCEL

*&---------------------------------------------------------------------*
*&      Form  message_popup_screen
*&---------------------------------------------------------------------*
*       text : Popup message screen
*----------------------------------------------------------------------*
*      -->P_TEXT_200  text    : Title description
*      -->P_TEXT_201  text    : Body description
*      <--P_G_CONTINUE  text  : Continue flag
*----------------------------------------------------------------------*
FORM message_popup_screen    USING p_title
                                   p_body
                          CHANGING p_continue.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_title
*     DIAGNOSE_OBJECT       = ' '
      text_question         = p_body
      text_button_1         = 'YES'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'
      icon_button_2         = 'ICON_CANCEL'
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ' '
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = ' '
*     START_ROW             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = p_continue.

*  CASE p_continue.
*    WHEN '1'.
*
*    WHEN '2'.
*
*  ENDCASE.

ENDFORM.                    " MESSAGE_POPUP_SCREEN


*&---------------------------------------------------------------------*
*&      Form  CHANGE_PR_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_pr_status .

  DATA :
    lt_tmp_alv   LIKE TABLE OF gt_display       WITH HEADER LINE,

    lt_return    TYPE TABLE OF bapiret2         WITH HEADER LINE,
    lt_pr_itm    TYPE TABLE OF bapimereqitemimp WITH HEADER LINE,
    lt_pr_itmx   TYPE TABLE OF bapimereqitemx   WITH HEADER LINE,
    l_pr_number  TYPE          bapimereqheader-preq_no,
    l_message    TYPE          bapi_msg,
    l_icon       TYPE          char5.

  CLEAR   : lt_tmp_alv.
  REFRESH : lt_tmp_alv[].

* -----   ---              -----------
  CHECK : NOT gt_display[] IS INITIAL.
* -----   ---              -----------
  lt_tmp_alv[] = gt_display[].

* "for Binary search
  SORT lt_tmp_alv BY banfn      "PR No.
                     bnfpo.     "PR item No.

* PR number base !!!!!!!!!!!!!!!!
  CLEAR : it_pr_num.
  LOOP AT it_pr_num.

    CLEAR   : lt_pr_itm,   lt_pr_itmx,  l_pr_number.
    REFRESH : lt_pr_itm[], lt_pr_itmx[].

    CLEAR : lt_tmp_alv.
    READ TABLE lt_tmp_alv WITH KEY banfn = it_pr_num-banfn
                                   BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT lt_tmp_alv FROM sy-tabix.
        IF lt_tmp_alv-banfn <> it_pr_num-banfn. "PR No.
          EXIT.                       "exit this inner loop
        ENDIF.

        IF ( lt_tmp_alv-check = 'X'  AND
             lt_tmp_alv-icon  <> icon_green_light ).

          l_pr_number          = it_pr_num-banfn.   "PR No.

          lt_pr_itm-preq_item  = lt_tmp_alv-bnfpo.  "PR item No.
          lt_pr_itm-closed     = 'X'.
          lt_pr_itmx-preq_item = lt_tmp_alv-bnfpo.  "PR item No.
          lt_pr_itmx-closed    = 'X'.
          APPEND lt_pr_itm.
          APPEND lt_pr_itmx.

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ( NOT l_pr_number  IS INITIAL AND
         NOT lt_pr_itm[]  IS INITIAL AND
         NOT lt_pr_itmx[] IS INITIAL   ).

      CLEAR   : lt_return.
      REFRESH : lt_return[].
*     "BAPI PR change
      CALL FUNCTION 'BAPI_PR_CHANGE'
        EXPORTING
          number  = l_pr_number
        TABLES
          return  = lt_return
          pritem  = lt_pr_itm
          pritemx = lt_pr_itmx.

      CLEAR : l_message, l_icon.
      CLEAR : lt_return.
      READ TABLE lt_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        l_message = lt_return-message.
        l_icon    = icon_red_light.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        l_message = 'Success'.
        l_icon    = icon_green_light.
      ENDIF.

      CLEAR : lt_pr_itm.
      LOOP AT lt_pr_itm.

        CLEAR : gt_display.
        READ TABLE gt_display
              WITH KEY banfn = l_pr_number
                       bnfpo = lt_pr_itm-preq_item. "PR item No.
        IF sy-subrc = 0.
          gt_display-check      = ''.
          gt_display-icon       = l_icon.
          gt_display-result_msg = l_message.

          CASE l_icon.
            WHEN icon_green_light.
              gt_display-ebakz = 'X'. "PR Closed
            WHEN OTHERS.
              gt_display-ebakz = ''.  "Not changed (PR Closed status)
          ENDCASE.

          MODIFY gt_display TRANSPORTING
                                         check
                                         icon
                                         ebakz          "PR Closed
                                         result_msg
                                   WHERE banfn = l_pr_number
                                     AND bnfpo = lt_pr_itm-preq_item.
        ENDIF.

        CLEAR : lt_pr_itm.
      ENDLOOP.

    ENDIF.

    CLEAR : it_pr_num.
  ENDLOOP.


ENDFORM.                    " CHANGE_PR_STATUS


*&---------------------------------------------------------------------*
*&      Form  WHEN_BATCH_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_batch_job .

  IF ( NOT sy-batch     IS INITIAL AND
       NOT gt_display[] IS INITIAL    ).

    CLEAR : gt_display.
    gt_display-check = 'X'.
    MODIFY gt_display TRANSPORTING check
                             WHERE check = ''.
  ENDIF.

ENDFORM.                    " WHEN_BATCH_JOB

*&---------------------------------------------------------------------*
*&      Form  RESULT_BATCH_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM result_batch_job .

  DATA : l_success_count TYPE i,
         l_fail_count    TYPE i.

  IF NOT sy-batch IS INITIAL.

    CLEAR : gt_display.
    LOOP AT gt_display.

      IF ( gt_display-icon  = icon_green_light  AND
           gt_display-ebakz = 'X'                   ). "PR Closed
        ADD  1   TO l_success_count.
      ELSE.
        ADD  1   TO l_fail_count.
      ENDIF.

      CLEAR : gt_display.
    ENDLOOP.

*   "for display spool(T-code 'SP01')
    IF NOT gt_display[] IS INITIAL.
      WRITE :/ '[PR Closed] Success count:', l_success_count,
               '/Fail count:', l_success_count.
    ELSE.
*     "piled up in spool (cf. although does not existing data)
*      WRITE :/ 'Data does not exist for Batch Job process.'.
    ENDIF.
  ENDIF.

ENDFORM.                    " RESULT_BATCH_JOB

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_BAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar USING p_text.

  DATA: l_text(50).
  l_text = p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = c_percentage
      text       = l_text.  "c_text.

ENDFORM.                    " DISPLAY_PROGRESS_BAR
