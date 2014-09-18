************************************************************************
* Program Name      : ZEMMPM29E_CD_CHANGE_SA
* Author            : Furong Wang
* Creation Date     : 05/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Price comparison (SA & Info Record)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 05/24/06        Furong Wang                     Condition ZPXX not
**                                                included
************************************************************************


REPORT zemmpm29e_cd_change_s NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.
**---
INCLUDE : zrmmpmxxr_incl.

**--- Internal Tables
DATA : BEGIN OF it_temp OCCURS 0,
         ebeln LIKE ekko-ebeln,
         ebelp LIKE ekpo-ebelp,
         lifnr LIKE ekko-lifnr,
         matnr LIKE ekpo-matnr,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         kschl LIKE konp-kschl,
         konwa LIKE konp-konwa,
         kzust LIKE konh-kzust,
         kbetr_sa LIKE konp-kbetr,
         kpein_sa LIKE konp-kpein,
         datab_sa LIKE a018-datab,
         datbi_sa LIKE a018-datbi,
         datab LIKE a018-datab,
         datbi LIKE a018-datbi,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         linecolor(4),     " ALV Color
       END OF it_temp.

DATA : it_itab LIKE it_temp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_sa_all OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         lifnr LIKE ekko-lifnr,
         matnr LIKE ekpo-matnr,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         etfz1 LIKE ekpo-etfz1,
         bstyp LIKE ekko-bstyp,
         bukrs LIKE ekko-bukrs,
         bsart LIKE ekko-bsart,
         ekorg LIKE ekko-ekorg,
         ekgrp LIKE ekko-ekgrp,
         kdatb LIKE ekko-kdatb,
         kdate LIKE ekko-kdate,
       END OF it_sa_all.

DATA : BEGIN OF it_info_item OCCURS 0,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         konwa LIKE konp-konwa,
         lifnr LIKE konp-lifnr,
         kzust LIKE konh-kzust,
         datab LIKE konh-datab,
         datbi LIKE konh-datbi,
       END OF it_info_item.

DATA : it_sacond_item LIKE it_info_item OCCURS 0 WITH HEADER LINE.

DATA: it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

DATA: it_t683s LIKE TABLE OF t683s WITH HEADER LINE.

** wokking variant
DATA : w_subrc LIKE sy-subrc.

DATA : w_knumh LIKE konh-knumh.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ekorg FOR ekko-ekorg DEFAULT 'PU01'.
SELECT-OPTIONS : s_lifnr FOR ekko-lifnr.
SELECT-OPTIONS : s_matnr FOR ekpo-matnr.
PARAMETERS : p_date LIKE ekko-aedat DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: p_pr RADIOBUTTON GROUP grp.
PARAMETERS: p_prdt RADIOBUTTON GROUP grp DEFAULT 'X'.
*SELECTION-SCREEN SKIP.
*PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
*SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.
PARAMETERS: p_send AS CHECKBOX DEFAULT 'X' USER-COMMAND ucom.
PARAMETERS: p_email(40) DEFAULT 'SAIFVAL' MODIF ID md3.
SELECTION-SCREEN END OF BLOCK block2.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
  PERFORM get_scheduling_agreement.

  IF it_sa_all[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM get_cond_price.
    PERFORM change_conditions.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  change_conditions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_conditions.
  CLEAR : it_itab, it_itab[].
  LOOP AT it_sa_all.
    PERFORM get_info_item_condition.
    PERFORM get_sa_item_condition.
    SORT it_info_item BY kschl DESCENDING.
    SORT it_sacond_item BY kschl DESCENDING.
    CHECK it_info_item[] NE it_sacond_item[].
    PERFORM fill_table.
  ENDLOOP.

  IF p_send = 'X'.
    if it_itab[] is initial.
    else.
    PERFORM send_email.
    endif.
  ENDIF.
ENDFORM.                    " change_conditions

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line, w_top_of_page, w_top_of_page[].
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

*  CLEAR : w_line.
*  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  CLEAR: w_fieldcat, w_fieldcat[], w_sortcat, w_sortcat[].
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  get_scheduling_agreement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_scheduling_agreement.
*---
  CLEAR: it_sa_all, it_sa_all[].
  SELECT b~ebeln
         ebelp
         matnr
         werks
         lgort
         lifnr
         etfz1
         a~bstyp
         a~bukrs
         bsart
         ekorg
         ekgrp
         kdatb
         kdate
               INTO CORRESPONDING FIELDS OF TABLE it_sa_all
               FROM ekko AS a INNER JOIN ekpo AS b
                 ON a~mandt EQ b~mandt
                AND a~ebeln EQ b~ebeln
               WHERE a~bstyp EQ 'L'
                AND a~lifnr IN s_lifnr
                AND b~matnr IN s_matnr
                AND a~loekz EQ space
                AND b~loekz EQ space
                AND elikz EQ space.
*                AND ( kdatb LE sy-datum
*                  AND kdate GE sy-datum.
ENDFORM.                    " get_scheduling_agreement

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'EBELN' 10 'SA Number'      'CHAR' '' ''      '',
*    w_col_pos 'EBELP' 05 'Item Number'    'NUMC' '' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    w_col_pos 'WERKS' 04 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LIFNR' 04 'Vendor'         'CHAR' ''  ''      '',
    w_col_pos 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',

    w_col_pos 'DATAB_SA' 10 'SA From'     'DATS' ''  ''      '',
    w_col_pos 'DATBI_SA' 10 'SA To'       'DATS' ''  ''      '',
    w_col_pos 'KBETR_SA' 12 'Amount'      'CURR' ''  ''      '',
    w_col_pos 'KPEIN_SA' 4  'Un/p'        'DEC' ''  ''      '',

    w_col_pos 'DATAB' 10 'Info From'      'DATS' ''  ''      '',
    w_col_pos 'DATBI' 10 'Info To'        'DATS' ''  ''      '',
    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      '',
    w_col_pos 'KPEIN' 4  'Un/p'           'DEC' ''  ''      ''.

ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
*  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
*                   '2' 'EBELP' 'IT_ITAB' 'X' '',
*                   '3' 'MATNR' 'IT_ITAB' 'X' '',
*                   '4' 'WERKS' 'IT_ITAB' 'X' '',
*                   '5' 'LGORT' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_table.
  DATA: l_diff(1).

  READ TABLE it_sacond_item WITH KEY kschl = 'PB00'.
  IF sy-subrc EQ 0.
    READ TABLE it_info_item WITH KEY kschl = 'PB00'.
    IF it_sacond_item-kbetr = it_info_item-kbetr AND
       it_sacond_item-kpein = it_info_item-kpein AND
       it_sacond_item-datbi = it_info_item-datbi AND
       it_sacond_item-datab = it_info_item-datab.
      DELETE it_sacond_item WHERE kschl = 'PB00'.
    ELSE.
      l_diff = 'X'.
      MOVE : it_sa_all-ebeln        TO it_itab-ebeln,
             it_sa_all-ebelp        TO it_itab-ebelp,
             it_sa_all-lifnr        TO it_itab-lifnr,
             it_sa_all-matnr        TO it_itab-matnr,
             it_sa_all-werks        TO it_itab-werks,
             it_sa_all-lgort        TO it_itab-lgort,
             'PB00'                 TO it_itab-kschl,
             it_sacond_item-datab TO it_itab-datab_sa,
             it_sacond_item-datbi TO it_itab-datbi_sa,
             it_sacond_item-kbetr TO it_itab-kbetr_sa,
             it_sacond_item-kpein TO it_itab-kpein_sa.
      DELETE it_sacond_item WHERE kschl = 'PB00'.

      MOVE:  it_info_item-datab TO it_itab-datab,
             it_info_item-datbi TO it_itab-datbi,
             it_info_item-kbetr TO it_itab-kbetr,
             it_info_item-kpein TO it_itab-kpein.
      DELETE it_info_item WHERE kschl = 'PB00'.

      APPEND it_itab.
      CLEAR : it_itab.
    ENDIF.
    LOOP AT it_sacond_item.
      READ TABLE it_info_item WITH KEY kschl = it_sacond_item-kschl.
      IF it_sacond_item-kbetr = it_info_item-kbetr AND
       it_sacond_item-kpein = it_info_item-kpein AND
       it_sacond_item-datbi = it_info_item-datbi AND
       it_sacond_item-datab = it_info_item-datab.
        DELETE it_info_item WHERE kschl = it_sacond_item-kschl.
      ELSE.
        IF l_diff IS INITIAL.
          l_diff = 'X'.
          MOVE : it_sa_all-ebeln        TO it_itab-ebeln,
            it_sa_all-ebelp        TO it_itab-ebelp,
            it_sa_all-lifnr        TO it_itab-lifnr,
            it_sa_all-matnr        TO it_itab-matnr,
            it_sa_all-werks        TO it_itab-werks,
            it_sa_all-lgort        TO it_itab-lgort.
        ENDIF.
        MOVE: it_sacond_item-kschl TO it_itab-kschl,
              it_sacond_item-datab TO it_itab-datab_sa,
            it_sacond_item-datbi TO it_itab-datbi_sa,
            it_sacond_item-kbetr TO it_itab-kbetr_sa,
            it_sacond_item-kpein TO it_itab-kpein_sa.

        MOVE: it_info_item-datab TO it_itab-datab,
              it_info_item-datbi TO it_itab-datbi,
              it_info_item-kbetr TO it_itab-kbetr,
              it_info_item-kpein TO it_itab-kpein.
        DELETE it_info_item WHERE kschl = it_sacond_item-kschl.
        APPEND it_itab.
        CLEAR : it_itab.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT it_info_item.
    IF l_diff IS INITIAL.
      l_diff = 'X'.
      MOVE : it_sa_all-ebeln        TO it_itab-ebeln,
        it_sa_all-ebelp        TO it_itab-ebelp,
        it_sa_all-matnr        TO it_itab-matnr,
        it_sa_all-werks        TO it_itab-werks,
        it_sa_all-lgort        TO it_itab-lgort.
    ENDIF.

    MOVE: it_info_item-kschl  TO it_itab-kschl,
          it_info_item-datab TO it_itab-datab,
          it_info_item-datbi TO it_itab-datbi,
          it_info_item-kbetr TO it_itab-kbetr,
          it_info_item-kpein TO it_itab-kpein.
    APPEND it_itab.
    CLEAR : it_itab.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_info_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  get_info_item_condition.

  DATA: l_datab LIKE a018-datab,
        l_datbi LIKE a018-datbi.

  CLEAR : it_info_item, it_info_item[].
  CLEAR : w_knumh.

  SELECT SINGLE knumh datab datbi INTO (w_knumh, l_datab, l_datbi)
                      FROM a018
                     WHERE kappl EQ 'M'
                       AND kschl EQ 'PB00'
                       AND lifnr EQ it_sa_all-lifnr
                       AND matnr EQ it_sa_all-matnr
                       AND datab <= p_date
                       AND datbi >= p_date.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_info_item
           FROM konp
          WHERE knumh EQ w_knumh
            AND loevm_ko EQ space
            AND kbetr > 0.

  LOOP AT it_info_item.
    READ TABLE it_t683s WITH KEY
                      kschl = it_info_item-kschl
                      kalsm = 'RM0002'.
    IF it_t683s-kstat = ' ' OR
        ( it_t683s-kstat = 'X' AND it_t683s-kvsl1 <> ' ' ).
      IF p_pr = 'X'.
      ELSE.
        it_info_item-datab = l_datab.
        it_info_item-datbi = l_datbi.
        MODIFY it_info_item.
      ENDIF.
      CONTINUE.
    ELSE.
      DELETE it_info_item.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  get_sa_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sa_item_condition.

  DATA: l_datab LIKE a018-datab,
        l_datbi LIKE a018-datbi.

  CLEAR : it_sacond_item, it_sacond_item[], w_knumh.

  SELECT SINGLE knumh datab datbi INTO (w_knumh, l_datab, l_datbi)
                      FROM a016
                     WHERE kappl EQ 'M'
                       AND kschl EQ 'PB00'
                       AND evrtn EQ it_sa_all-ebeln
                       AND evrtp EQ it_sa_all-ebelp
                       AND datab <= p_date
                       AND datbi >= p_date.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_sacond_item
           FROM konp
          WHERE knumh EQ w_knumh
            AND kbetr > 0.

  LOOP AT it_sacond_item.
    READ TABLE it_t683s WITH KEY
                kschl = it_sacond_item-kschl
                kalsm = 'RM0000'.
    IF it_t683s-kstat = ' ' OR
        ( it_t683s-kstat = 'X' AND it_t683s-kvsl1 <> ' ' ).
      IF p_pr = 'X'.
      ELSE.
        it_sacond_item-datab = l_datab.
        it_sacond_item-datbi = l_datbi.
        MODIFY it_sacond_item.
      ENDIF.
      CONTINUE.
    ELSE.
      DELETE it_sacond_item.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_sa_item_condition

*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.

  DATA: l_subject(40) TYPE c VALUE 'SA Info Record Price Compare'.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          gd_cnt TYPE i,
          gd_sent_all(1) TYPE c,
          gd_doc_data LIKE sodocchgi1,
          gd_error TYPE sy-subrc.

  PERFORM populate_data_for_output.

  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
  it_receivers-receiver = p_email.
*  it_receivers-rec_type = 'U'.  " internet email
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = 'X'.
  it_receivers-notif_ndel = 'X'.
  APPEND it_receivers.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
       EXPORTING
            document_data              = gd_doc_data
            put_in_outbox              = 'X'
       IMPORTING
            sent_to_all                = gd_sent_all
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

* Store function module return code
  gd_error = sy-subrc.

ENDFORM.                    " send_email

*---------------------------------------------------------------------*
*       FORM populate_data_for_output                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM populate_data_for_output.
  DATA: l_message TYPE so_text255,
        l_kpein(4),
        l_kbetr(12).

  CLEAR: it_mail,it_mail[].

  APPEND 'SA Number Material        Plant Vend Cond SA Fr     SA To
amount u/p  info fr  info to    amount u/p' TO it_mail.

  LOOP AT it_itab.
    l_kbetr = it_itab-kbetr_sa.
    l_kpein = it_itab-kpein_sa.
    CONCATENATE it_itab-ebeln it_itab-matnr it_itab-werks
                it_itab-lifnr INTO l_message SEPARATED BY space.
    CONCATENATE l_message it_itab-kschl it_itab-datab_sa
                it_itab-datbi_sa l_kbetr
                l_kpein INTO l_message SEPARATED BY space.
    l_kbetr = it_itab-kbetr.
    l_kpein = it_itab-kpein.
    CONCATENATE l_message it_itab-datab it_itab-datbi l_kbetr
                l_kpein INTO l_message SEPARATED BY space.

    APPEND l_message TO it_mail.
    CLEAR: it_mail, l_message.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_cond_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cond_price.
  CLEAR: it_t683s, it_t683s[].
  SELECT * INTO TABLE it_t683s FROM t683s
  WHERE kvewe = 'A'
    AND kappl = 'M'
    AND ( kalsm = 'RM0000' OR kalsm = 'RM0002' ).
ENDFORM.   " get_cond_price
