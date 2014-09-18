************************************************************************
* Program Name      : ZIPP107I_PMT06GB_RFC
* Author            : Bobby
* Creation Date     : 2003.10.06
* Specifications By :
* Pattern           : 1.1
* Development Request No :  UD1K901977
* Addl Documentation:
* Description       : Call RFC for Outbound Interface
*
* Modification Logs
* Date       Developer    RequestNo    Description
*09/22/2008 Haseeb UD1K944561  Copied from old version for APS2.
************************************************************************
REPORT zipp107i_pmt06gb_rfc NO STANDARD PAGE HEADING
*                          LINE-SIZE 120
                          MESSAGE-ID zmpp.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztpp_pp_log_head,
         ztpp_pp_log_deta,
         ztpp_pmt06gb.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_pms06gb   LIKE TABLE OF zspp_pmt06gb WITH HEADER LINE,
       it_pmt06gb   LIKE TABLE OF ztpp_pmt06gb WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA :     wa_cnt          LIKE ztpp_pp_log_deta-sequence,
           wa_number       LIKE ztpp_pp_log_head-logkey,
           wa_error        TYPE c               ,
           wa_flg          TYPE char1  VALUE 'X'.

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_count(3)  VALUE 500     ,
           c_dest(10)  VALUE 'WMPP01'.   "Outbound Interface Destination

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-100 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CHECK p_run = 'X'  .
  PERFORM excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  DATA: l_msgtxt(100),
        l_size      TYPE num9,
        l_idx       TYPE num9,
        l_curr      TYPE num9,
        l_count(3)  TYPE n   .

  CLEAR : it_pmt06gb, it_pmt06gb[].
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_pmt06gb
         FROM ztpp_pmt06gb
         WHERE zresult EQ 'E' OR zresult EQ space.

  IF sy-subrc EQ 0.
    DESCRIBE TABLE it_pmt06gb LINES l_size.
*    l_idx = l_size DIV c_count .
    l_idx = l_idx + 1          .          " Total RFC Call Count
    LOOP AT it_pmt06gb.
*      l_count = l_count + 1.

      MOVE-CORRESPONDING it_pmt06gb TO it_pms06gb.
      APPEND it_pms06gb.
      IF l_count >= l_size.
        l_curr = l_curr + 1.
        CALL FUNCTION 'Z_FPP_SET_PMT06GB_2'
          DESTINATION  c_dest
              EXPORTING
            total_c              = l_idx
            curr_c               = l_curr
          TABLES
            i_ztpp_pmt06gb       = it_pms06gb
          EXCEPTIONS
            communication_failure = 1  MESSAGE l_msgtxt
            system_failure        = 2  MESSAGE l_msgtxt.

        IF sy-subrc <> 0.
          " Write Log....
          PERFORM create_log USING 'E' 1  text-001  l_msgtxt .
          PERFORM create_log USING 'R' 1  text-001  l_msgtxt .
          IF sy-batch = ' '.
            MESSAGE e001 WITH text-001 .
          ENDIF.
          wa_error = 'X' .          EXIT.
        ELSE.
          LOOP AT it_pms06gb WHERE zzret = 'E'.
            wa_error = 'X' .
            PERFORM create_log USING 'E' 1  text-001  l_msgtxt .
            PERFORM create_log USING 'R' 1  text-001  l_msgtxt .
          IF sy-batch = ' '.
            MESSAGE e001 WITH text-001 .
          ENDIF.
            EXIT.
          ENDLOOP.
          CLEAR: it_pms06gb, it_pms06gb[].
          IF wa_error = 'X'. EXIT. ENDIF.
        ENDIF.
        CLEAR: wa_flg, l_count.
      ENDIF.
    ENDLOOP.
    IF wa_error = space.
      l_curr = l_curr + 1.
      CALL FUNCTION 'Z_FPP_SET_PMT06GB_2'
        DESTINATION  c_dest
            EXPORTING
          total_c              = l_idx
          curr_c               = l_curr
        TABLES
          i_ztpp_pmt06gb       = it_pms06gb
        EXCEPTIONS
          communication_failure = 1  MESSAGE l_msgtxt
          system_failure        = 2  MESSAGE l_msgtxt.

      IF sy-subrc <> 0.
        " Write Log....
        PERFORM create_log USING 'E' 1  text-001  l_msgtxt .
        PERFORM create_log USING 'R' 1  text-001  l_msgtxt .
        IF sy-batch = ' '.
          MESSAGE e001 WITH text-001 .
        ENDIF.
        wa_error = 'X' .
      ELSE.
        LOOP AT it_pms06gb WHERE zzret = 'E'.
          wa_error = 'X' .
          PERFORM create_log USING 'E' 1  text-001  l_msgtxt .
          PERFORM create_log USING 'R' 1  text-001  l_msgtxt .
          IF sy-batch = ' '.
            MESSAGE e001 WITH text-001 .
          ENDIF.
          EXIT.
        ENDLOOP.
      ENDIF.
      CHECK wa_error = space.
      CONCATENATE 'Total Record is' l_size 'Success!!!' INTO l_msgtxt.
      IF sy-batch = ' ' .
        MESSAGE s001 WITH text-002 .
      ENDIF.
      PERFORM create_log USING 'S' 2  text-002  l_msgtxt .
    ENDIF.
  ELSE.
    CONCATENATE 'Total Record is' l_size 'Success!!!' INTO l_msgtxt.
    PERFORM create_log USING 'S' 2  text-002  l_msgtxt .
    IF sy-batch = ' '.
      MESSAGE s001 WITH text-002 .
    ENDIF.
  ENDIF.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0177   text
*      -->P_1      text
*      -->P_TEXT_001  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM create_log USING    pa_type  pa_step  pa_text  pa_key .
  wa_cnt = wa_cnt + 1 .
  IF wa_cnt = 1       .
    PERFORM get_logserial.               " Log Number Generation........
    ztpp_pp_log_head-logkey   = wa_number   .
    ztpp_pp_log_head-programm = sy-repid    .
    ztpp_pp_log_head-logtype  = pa_type     .
    ztpp_pp_log_head-jobtype  = sy-batch    .
    ztpp_pp_log_head-logstep  = pa_step     .
    ztpp_pp_log_head-msg      = pa_text     .
    ztpp_pp_log_head-ldate    = sy-datum    .
    ztpp_pp_log_head-ltime    = sy-uzeit    .
    ztpp_pp_log_head-luser    = sy-uname    .
    INSERT INTO ztpp_pp_log_head VALUES ztpp_pp_log_head .
  ENDIF.

  " Log Detail Creation
  ztpp_pp_log_deta-logkey   = wa_number    .
  ztpp_pp_log_deta-sequence = wa_cnt      .
  ztpp_pp_log_deta-logtype  = pa_type     .
  ztpp_pp_log_deta-jobtype  = sy-batch    .
  ztpp_pp_log_deta-logstep  = pa_step     .
  ztpp_pp_log_deta-keydata  = pa_key      .
  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  get_logserial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_logserial.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZLOG'
       IMPORTING
            number                  = wa_number
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL
