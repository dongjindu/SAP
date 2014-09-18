*----------------------------------------------------------------------*
***INCLUDE MZRQM25R_NOTI_SUMMARY_PF01 .
*----------------------------------------------------------------------*
  data: wa_bdcdata like bdcdata.
  data: it_bdcdata like table of wa_bdcdata.



*&---------------------------------------------------------------------*
*&      Form  load_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM load_text.

    field-symbols: <fs_otxt> like line of it_tlines.

    clear w_tdname.
    refresh: it_tlines,it_txtline.
    refresh: it_txtline1.
    w_tdname = qmel-qmnum.
*    perform read_text tables it_tlines using 'ZQM1' w_tdname 'ZQMEL'.
    perform read_text tables it_tlines using 'ZQM3' w_tdname 'ZQMEL'.

    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
*         append <fs_otxt>-tdline to it_txtline1.
      endloop.
      call method w_editor1->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
*call method w_editor1->set_text_as_stream
*             exporting text = it_txtline1.
*      refresh: it_tlines,it_txtline1.
    endif.
    if <fs_otxt> is assigned.
      unassign <fs_otxt>.
    endif.

** Changed by Furong on 07/16/09
*    perform read_text tables it_tlines using 'ZQM2' w_tdname 'ZQMEL'.
    perform read_text tables it_tlines using 'LTQM' w_tdname 'QMEL'.
** End of change
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor2->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
* Modification by 100565
*    perform read_text tables it_tlines using 'ZQM3' w_tdname 'ZQMEL'.
    perform read_text tables it_tlines using 'ZQM1' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor3->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
    perform read_text tables it_tlines using 'ZQM4' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor4->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
    perform read_text tables it_tlines using 'ZQM5' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor5->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
    perform read_text tables it_tlines using 'ZQM6' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor6->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
    perform read_text tables it_tlines using 'ZQM7' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      loop at it_tlines assigning <fs_otxt>.
        append <fs_otxt>-tdline to it_txtline.
      endloop.
      refresh it_tlines.
      call method w_editor7->set_text_as_stream
             exporting text = it_txtline.
      refresh: it_tlines,it_txtline.
    endif.
***read other fields
   select single * from ZTQM_NOTI_PORTAL where qmart = 'Q2' and qmnum =
                      qmel-qmnum.
* end modification

    call method cl_gui_cfw=>flush.
  ENDFORM.                    " load_text
*&---------------------------------------------------------------------*
*&      Form  save_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM save_text.
    data: wa_thead like thead.
    data : wa_check1,wa_check2,wa_check3,wa_check4,wa_check5,
           wa_check6,wa_check7.
    data: wa_no(4) type c,
        itftext type tline occurs 0 with header line.
    field-symbols: <fs_itxt> like line of it_txtline.

    clear: w_tdname,wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor1->get_text_as_stream
            importing text = it_txtline.

*      delete it_txtline where TDLINE eq ''.
*"UD1K940691
*
*      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
*             EXPORTING
*                  LANGUAGE    = sy-LANGU
*             TABLES
*                  TEXT_STREAM = it_txtline[]
*                  ITF_TEXT    = itftext.


    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
*    wa_thead-tdid     = 'ZQM1'.  "UD1K923261
    wa_thead-tdid     = 'ZQM3'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
*    loop at itftext assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check1 = 'X'.
    endif.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

************************************************
**Modification by 100565 to add more text box
    data: wa_qmnum_1 like qmel-qmnum.
    clear wa_qmnum_1.
    move qmel-qmnum to stxh-tdname.
    move 'ZQMEL' to stxh-tdobject.
*   move 'ZQM3' to stxh-tdid.  "UD1K923261
    move 'ZQM1' to stxh-tdid.                               "UD1K923261
    move 'EN' to stxh-tdspras.
    perform modify_stxh.

    clear: w_tdname,wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor3->get_text_as_stream
            importing text = it_txtline.

    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
*    wa_thead-tdid     = 'ZQM3'.
    wa_thead-tdid     = 'ZQM1'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check3 = 'X'.
    endif.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* text box 4
    move qmel-qmnum to stxh-tdname.
    move 'ZQMEL' to stxh-tdobject.
    move 'ZQM4' to stxh-tdid.
    move 'EN' to stxh-tdspras.
    perform modify_stxh.

    clear: w_tdname,wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor4->get_text_as_stream
            importing text = it_txtline.

    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
    wa_thead-tdid     = 'ZQM4'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check4 = 'X'.
    endif.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* Text box 5
    move 'ZQM5' to stxh-tdid.
    perform modify_stxh.

    clear: w_tdname,wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor5->get_text_as_stream
            importing text = it_txtline.

    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
    wa_thead-tdid     = 'ZQM5'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check5 = 'X'.
    endif.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* Text box 6
    move 'ZQM6' to stxh-tdid.
    perform modify_stxh.

    clear: w_tdname,wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor6->get_text_as_stream
            importing text = it_txtline.

    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
    wa_thead-tdid     = 'ZQM6'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check6 = 'X'.
    endif.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* Text box 7
    move 'ZQM7' to stxh-tdid.
    perform modify_stxh.

    clear:
    w_tdname,
    wa_txtline,wa_thead.
    refresh: it_txtline,it_tlines.
    call method w_editor7->get_text_as_stream
            importing text = it_txtline.

    w_tdname          = qmel-qmnum.
    wa_thead-tdobject = 'ZQMEL'.
    wa_thead-tdname   = w_tdname.
    wa_thead-tdid     = 'ZQM7'.
    wa_thead-tdspras  = sy-langu.
    loop at it_txtline assigning <fs_itxt>.
      wa_tline-tdline = <fs_itxt>-tdline.
      append wa_tline to it_tlines.
    endloop.
    clear wa_no.
    describe table it_tlines lines wa_no.
    if wa_no >= 1.
      wa_check7 = 'X'.
    endif.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*      CLIENT                = SY-MANDT
        HEADER                = wa_thead
      TABLES
        LINES                 = it_tlines
      EXCEPTIONS
        ID                    = 1
        LANGUAGE              = 2
        NAME                  = 3
        OBJECT                = 4
        OTHERS                = 5
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    perform copy_other_fields_to_table.



    Perform save_other_text_box
    using wa_check1 wa_check3 wa_check4 wa_check5 wa_check6 wa_check7.

**End Modification
***********************************************
    perform chg_plandt_comflg.
  ENDFORM.                    " save_text

*&---------------------------------------------------------------------*
*&      Form  chg_plandt_comflg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM chg_plandt_comflg.

    data: w_plandt(10) type c.
*HASEEB
*    DATA: MESSGTAB TYPE TABLE OF BDCMSGCOLL  with header line.
    DATA:   MESSGTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
    TABLES T100.
    DATA:   MESSTXT(255) TYPE C.
    DATA : L_LINE LIKE SY-TABIX.
    DATA: L_MSTRING(480),
           mode_type.
    mode_type = 'N'.
*HASEEB
    write qmel-plandat to w_plandt.
    if    w_plandt eq '00/00/0000' OR w_plandt eq '0000/00/00'
       OR w_plandt eq '00-00-0000' OR w_plandt eq '0000-00-00'
       OR w_plandt eq '00.00.0000' OR w_plandt eq '0000.00.00'.
      clear w_plandt .
    endif.

    perform fill_bdcdata using 'SAPLIQS0' '0200' 'X'.
    perform fill_bdcdata using 'RIWO00-QMNUM' qmel-qmnum space.
    perform fill_bdcdata using 'BDC_OKCODE' '/0' space.
    perform fill_bdcdata using 'SAPLIQS0' '7200' 'X'.
    perform fill_bdcdata using 'BDC_OKCODE' '10\TAB03' space.
    perform fill_bdcdata using 'SAPLIQS0' '7200' 'X'.
    perform fill_bdcdata using 'ZSQM_CI_QMEL-PLANDAT' w_plandt space.
    if qmel-completed = 'X'.
      perform fill_bdcdata using 'BDC_OKCODE' 'COMPLT' space.
      perform fill_bdcdata using 'SAPLIQS0' '7200' 'X'.
    endif.
    perform fill_bdcdata using 'BDC_OKCODE' '=BUCH' space.
*haseeb
    REFRESH : MESSGTAB.
    call transaction 'QM02' using it_bdcdata mode mode_type
             messages into MESSGTAB.

*  CALL TRANSACTION TCODE   USING       BDCDATA
*                           MODE        DISP_MODE
*                           UPDATE      UMODE
*                           MESSAGES    INTO   MESSTAB.
*
*    P_SUBRC = SY-SUBRC.
*
*    DESCRIBE TABLE MESSGTAB LINES L_LINE.
*    READ TABLE MESSGTAB  INDEX L_LINE.
*
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              MSGID               = MESSGTAB-MSGID
*              MSGNR               = MESSGTAB-MSGNR
*              MSGV1               = MESSGTAB-MSGV1
*              MSGV2               = MESSGTAB-MSGV2
*              MSGV3               = MESSGTAB-MSGV3
*              MSGV4               = MESSGTAB-MSGV4
*         IMPORTING
*              MESSAGE_TEXT_OUTPUT = MESSTXT.
*    MESSAGE e000(ZMQM) with MESSTXT.


    LOOP AT MESSGTAB.
      IF MESSGTAB-MSGTYP EQ 'E'
         OR MESSGTAB-MSGTYP EQ 'A'.

        SELECT SINGLE * FROM T100 WHERE SPRSL = MESSGTAB-MSGSPRA
                                  AND   ARBGB = MESSGTAB-MSGID
                                  AND   MSGNR = MESSGTAB-MSGNR.
        IF SY-SUBRC = 0.
          L_MSTRING = T100-TEXT.
          IF L_MSTRING CS '&1'.
            REPLACE '&1' WITH MESSGTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&2' WITH MESSGTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&3' WITH MESSGTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&4' WITH MESSGTAB-MSGV4 INTO L_MSTRING.
          ELSE.
            REPLACE '&' WITH MESSGTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&' WITH MESSGTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&' WITH MESSGTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&' WITH MESSGTAB-MSGV4 INTO L_MSTRING.
          ENDIF.
          CONDENSE L_MSTRING.
          MESSAGE e000(ZMQM) with MESSGTAB-MSGTYP MESSGTAB-MSGNR
                           L_MSTRING(250).
        ELSE.
          WRITE: / MESSGTAB.
        ENDIF.
      ENDIF.
    ENDLOOP.

*haseeb
    refresh messgtab.
    refresh it_bdcdata.
  ENDFORM.                    " chg_plandt_comflg
*&---------------------------------------------------------------------*
*&      Form  fill_bdcdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_field1   text
*      -->P_field2   text
*      -->P_field3   text
*----------------------------------------------------------------------*
  FORM fill_bdcdata USING p_field1
                          p_field2
                          p_field3.
    if p_field3 = 'X'.
      wa_bdcdata-program  = p_field1.
      wa_bdcdata-dynpro   = p_field2.
      wa_bdcdata-dynbegin = p_field3.
    else.
      wa_bdcdata-fnam = p_field1.
      wa_bdcdata-fval = p_field2.
    endif.
    append wa_bdcdata to it_bdcdata.
    clear wa_bdcdata.

  ENDFORM.                    " fill_bdcdata
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINES  text
*      -->P_0049   text
*      -->P_W_TDNAME  text
*      -->P_0051   text
*----------------------------------------------------------------------*
  FORM read_text tables p_it_tlines structure tline
                 using  p_tdid
                        p_tdname
                        p_tdobject.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*   CLIENT                        = SY-MANDT
        ID                            = p_tdid
        LANGUAGE                      = sy-langu
        NAME                          = p_tdname
        OBJECT                        = p_tdobject
* IMPORTING
*   HEADER                        =
      TABLES
        LINES                         = p_it_tlines
   EXCEPTIONS
     ID                            = 1
     LANGUAGE                      = 2
     NAME                          = 3
     NOT_FOUND                     = 4
     OBJECT                        = 5
     REFERENCE_CHECK               = 6
     WRONG_ACCESS_TO_ARCHIVE       = 7
     OTHERS                        = 8 .

  ENDFORM.                    " read_text
*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text  PRINT THE 9200 SCREEN.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  FORM PRINT_FORM.

*   field-symbols: <fs_txt> type line of it_txtline.

    data : itftext type tline occurs 0 with header line.


    clear w_tdname.
    refresh: it_tlines,it_txtline.
    refresh: it_txtline1.
    CLEAR : wa_txtline.


    CALL FUNCTION 'OPEN_FORM'
         EXPORTING
              DEVICE                      = 'PRINTER'
              DIALOG                      = 'X'
              FORM                        = 'ZQM_PORTAL_PRINT'
              LANGUAGE                    = SY-LANGU
         EXCEPTIONS
              CANCELED                    = 1
              DEVICE                      = 2
              FORM                        = 3
              OPTIONS                     = 4
              UNCLOSED                    = 5
              MAIL_OPTIONS                = 6
              ARCHIVE_ERROR               = 7
              INVALID_FAX_NUMBER          = 8
              MORE_PARAMS_NEEDED_IN_BATCH = 9
              SPOOL_ERROR                 = 10
              OTHERS                      = 11.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW = 'HEADER'.

    w_tdname = qmel-qmnum.
** Changed by Furong on 07/16/09
*    perform read_text tables it_tlines using 'ZQM2' w_tdname 'ZQMEL'.
    perform read_text tables it_tlines using 'LTQM' w_tdname 'QMEL'.
** End of change
    if sy-subrc <> 0.
    else.

      delete it_tlines where TDLINE eq ''.                  "UD1K940691
      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.

*      loop at itftext into wa_txtline.
      loop at it_tlines into wa_txtline.
*           CONCATENATE V_DISPLAY WA_TXTLINE-TDLINE INTO V_DISPLAY.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'NOTIFY'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.
      endloop.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'UNDER1'
                WINDOW  = 'MAIN'
           EXCEPTIONS
                ELEMENT = 1.


      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.

    endif.


    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'CONTAINMENT'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.


    perform read_text tables it_tlines using 'ZQM1' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.

      delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.


      loop at itftext .
       move itftext-TDLINE to wa_txtline-TDLINE.           " UD1K940691
*      loop at it_tlines into wa_txtline.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'RCPROCESS'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.

    endif.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER2'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    perform read_text tables it_tlines using 'ZQM3' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.

      loop at itftext. " into wa_txtline.
       move itftext-TDLINE to wa_txtline-TDLINE.          " UD1K940691
*      loop at it_tlines into wa_txtline.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CAPROCESS'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.

    endif.

    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER3'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    perform read_text tables it_tlines using 'ZQM5' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.

  delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.

      loop at itftext. " into wa_txtline.
       move itftext-TDLINE to wa_txtline-TDLINE.         " UD1K940691
*      loop at it_tlines into wa_txtline.      "UD1K940691

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'RCSYSTEM'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.
    endif.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER4'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    perform read_text tables it_tlines using 'ZQM4' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.

    delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.


      loop at itftext. " into wa_txtline.
      move itftext-TDLINE to wa_txtline-TDLINE.  " UD1K940691
*      loop at it_tlines into wa_txtline.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'CASYSTEM'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.
    endif.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER5'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    perform read_text tables it_tlines using 'ZQM6' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.
      delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.


      loop at itftext. " into wa_txtline.
      move itftext-TDLINE to wa_txtline-TDLINE.   " UD1K940691
*      loop at it_tlines into wa_txtline.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'STANDARD'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.

    endif.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER6'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    perform read_text tables it_tlines using 'ZQM7' w_tdname 'ZQMEL'.
    if sy-subrc <> 0.
    else.

      delete it_tlines where TDLINE eq ''.                  "UD1K940691

      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'            "UD1K940691
             EXPORTING
                  LANGUAGE    = sy-LANGU
             TABLES
                  TEXT_STREAM = it_tlines[]
                  ITF_TEXT    = itftext.


      loop at itftext. " into wa_txtline.
      move itftext-TDLINE to wa_txtline-TDLINE.   " UD1K940691
*      loop at it_tlines into wa_txtline.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'VALIDATION'
                  WINDOW  = 'MAIN'
             EXCEPTIONS
                  ELEMENT = 1.

      endloop.

      refresh: it_tlines,it_txtline,itftext.
      CLEAR : wa_txtline.

    endif.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'UNDER7'
              WINDOW  = 'MAIN'
         EXCEPTIONS
              ELEMENT = 1.

    CALL FUNCTION 'CLOSE_FORM'.


  ENDFORM.                "END OF PRINT_FORM.
