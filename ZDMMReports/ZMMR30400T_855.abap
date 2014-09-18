*&------------------------------------------------------------------
*& Program ID     : ZMMR30400T
*& Program Name   : Acknowledgement creation for ASN (DESADV)
*& Created by     : yang
*& Created on     : 05.21.2009
*& Development ID : MM-000
*& Reference Pgm. : ZMMR1650
*& Description    : Acknowledgement creation for ASN (DESADV)
*&                  IDoc document update and send IDoc docunent
*& IDOC Message : APERAK – Application error and acknowledgement message
*&--> Sending RFC Function 'Z_MM_IF_OB_03_001'[TGLO003],
*                          'Z_MM_IF_OB_03_002'[TGLO003]
*& EDI and WEB leads and ASN creates
*& and is a program to transmit about the message which occurs.
*& Modification Log
*&====================================================================
*& Date        Developer    Request ID    Description
*& 05.21.2009  Yang                           first dev.
*&--------------------------------------------------------------------

REPORT  zmmr30400t_855  NO STANDARD PAGE HEADING   MESSAGE-ID zmpp.

*===============================================================*
* Data definition                                               *
*===============================================================*
*-----------------------------------------*
* Include                                 *
*-----------------------------------------*
INCLUDE zmmitop01.                                  "Global TOP
INCLUDE <icon>.
*-----------------------------------------*
* Table definition                        *
*-----------------------------------------*
TABLES : edidc, lfa1, listedidc, teds2, vbuk,likp,
         stacust,
    "Customizing for IDoc status (status groups, archive, procg)
         stalight.
"Traffic Light Assignment to Status Groups for IDoc Display

*-----------------------------------------*
* data definition                         *
*-----------------------------------------*
* list
DATA : BEGIN OF it_list OCCURS 0.
        INCLUDE STRUCTURE listedidc.
        INCLUDE STRUCTURE e1adrm1.
DATA :  mark,
        partner_idlf   TYPE e1adrm1-partner_id,
        partner_idwe   TYPE e1adrm1-partner_id,
        name1_we       TYPE e1adrm1-name1,
        lifex   TYPE e1edl20-lifex,       "delivery note number
        rcode   TYPE char3,               "result code
        traid   TYPE char20,              "container number
        msg  TYPE idoc_msg,
        retry(1),                         "Retry
        bolnr   TYPE edilsegtyp,
       END OF it_list.

* IDoc document table
DATA : it_edidc TYPE TABLE OF edidc WITH HEADER LINE.

* Customizing for IDoc status (status groups, archive, procg)
DATA: BEGIN OF it_stacust OCCURS 0,
        status LIKE stacust-status,
        statva LIKE stacust-statva,
      END OF it_stacust.

* Traffic Light Assignment to Status Groups for IDoc Display
DATA: BEGIN OF it_stalight OCCURS 0,
        statva   LIKE stalight-statva,
        stalight LIKE stalight-stalight,
      END OF it_stalight.

* Short description of IDoc status values
DATA: BEGIN OF it_teds2 OCCURS 30.
        INCLUDE STRUCTURE teds2.
DATA: END OF it_teds2.

* export data
DATA : it_send_head TYPE TABLE OF zmms0025 WITH HEADER LINE,
       it_send_item TYPE TABLE OF zmms0026 WITH HEADER LINE,
       gt_send_item TYPE TABLE OF zmms0026 WITH HEADER LINE.

DATA : it_send_sef9 TYPE TABLE OF zmms0024 WITH HEADER LINE, "Glovis
       it_send_safe TYPE TABLE OF zmms0024 WITH HEADER LINE, "KMC
       it_send_sstx TYPE TABLE OF zmms0024 WITH HEADER LINE. "Glovis CN

*__IDOC LOCAL CREATE ADD 04/11/11 PAUL
DATA : l_rcvpor              LIKE edp13-rcvpor,
       idoc_control_complete LIKE edidc,
       idoc_hdr_seg          LIKE e1adhdr,
       t_idoc_data           LIKE edidd OCCURS 0 WITH HEADER LINE,
       idoc_seg              LIKE e1state,
       idoc_obj_seg          LIKE e1prtob,
       send_control          LIKE edidc OCCURS 0 WITH HEADER LINE,
       idoc_status           LIKE bdidocstat,
       error_occured,
       it_edidd              LIKE edidd OCCURS 0 WITH HEADER LINE,
       i_idoc_status         LIKE bdidocstat OCCURS 0 WITH HEADER LINE.

* IDoc record
TYPES : item_table_type LIKE STANDARD TABLE OF mtreeitm
                       WITH DEFAULT KEY.

DATA : node_table TYPE treev_ntab,
       item_table TYPE item_table_type.

* interface destination
DATA : g_dest(20), g_dest_ckd(20).
*
DATA:adm_txt LIKE zmms0027-messag2, "M_2
     t500(500),
     BEGIN OF alr_exist_msg OCCURS 1,
      docnum  LIKE edidc-docnum,
      messag2 LIKE zmms0027-messag2,
     END OF alr_exist_msg,
     u_traid LIKE likp-traid,
     u_find_traid TYPE char1,
     mess2 LIKE zmms0027-messag2,
     BEGIN OF it_msg OCCURS 1,
      docnum LIKE edidc-docnum,
      messag2 LIKE zmms0027-messag2,
     END OF it_msg,
     it_id TYPE zmms0027 OCCURS 1 WITH HEADER LINE.
*-----------------------------------------*
* Constants                               *
*-----------------------------------------*


*===============================================================*
* Selection screen                                              *
*===============================================================*
*--> block 1
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS :
     s_docnum   FOR edidc-docnum,
     s_lifnr    FOR lfa1-lifnr.
SELECTION-SCREEN END OF BLOCK block1.
PARAMETERS:p_64 AS CHECKBOX DEFAULT 'X'. "M_1
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-b03.

*selection-screen uline.
PARAMETERS:addmess AS CHECKBOX. "M_2
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:adm_vend FOR likp-lifnr. "M_2
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-b04."M_3
SELECT-OPTIONS:exi_vend FOR likp-lifnr. "M_3
SELECTION-SCREEN END OF BLOCK block3.

SELECTION-SCREEN END OF BLOCK block2.
*
PARAMETERS : p_dest(3) TYPE c.             "interface destination
*===============================================================*
* Events                                                        *
*===============================================================*
INITIALIZATION.
  PERFORM check_other_running.

AT SELECTION-SCREEN .

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  PERFORM run.

END-OF-SELECTION.
  PERFORM list.

TOP-OF-PAGE.

*===============================================================*
* Subroutine                                                    *
*===============================================================*
*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run .

  PERFORM read_idoc_table.
  PERFORM read_status_table.
  PERFORM arrange_list.


  CHECK sy-batch = 'X'.
  PERFORM batch_target_item.
  PERFORM alv_command_save.
ENDFORM.                    " run
*&---------------------------------------------------------------------*
*&      Form  read_idoc_table
*&---------------------------------------------------------------------*
*       read idoc table
*----------------------------------------------------------------------*
FORM read_idoc_table .
  REFRESH it_edidc.

  SELECT *
    FROM edidc
    INTO TABLE it_edidc
   WHERE docnum     IN s_docnum
*     AND status NOT IN ('50', '62', '64')
     AND status NOT IN ('50', '62', '68')
     AND direct     EQ '2'
*     AND STDMES     EQ 'DESADV'
** Requested by Prasad on 04/15/11
     AND mestyp    EQ 'DESADV'
** end of change
     AND refint     EQ space
     AND credat     GE '20110706'
     AND sndprn     IN s_lifnr.

ENDFORM.                    " read_idoc_table
*&---------------------------------------------------------------------*
*&      Form  arrange_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM arrange_list .
  DATA:c_traid LIKE likp-traid,c_lifex LIKE likp-lifex.
  DATA: act_lines TYPE i.
  DATA: max_lines TYPE i VALUE 10000.
  DESCRIBE TABLE it_edidc LINES act_lines.

  REFRESH  it_list.

  LOOP AT it_edidc.
    IF sy-tabix GT 10000.
      MESSAGE s999(b1) WITH max_lines ' of'
                      act_lines
                      ' Selected Idocs will be shown in the ALV-list'.
      EXIT.
    ENDIF.
    CLEAR it_list.
    MOVE-CORRESPONDING it_edidc TO it_list.

    READ TABLE it_stacust  WITH KEY status = it_edidc-status.
    READ TABLE it_stalight WITH KEY statva = it_stacust-statva.

    IF it_stalight-stalight = '1' OR it_stalight-stalight = '3'.
*--> idoc message text
      PERFORM read_idoc_message USING    it_list-docnum
                                         it_stalight-stalight
                                         it_list-status
                                         it_edidc-sndprn "M_2
                                CHANGING it_list-msg
                                         it_stalight-stalight
                                         it_list-retry
                                         it_list-cimtyp. "M_3
    ELSE. "M_3
*"M_3
      IF it_list-traid IS INITIAL AND it_list-lifex IS INITIAL."M_3
        CALL FUNCTION 'Z_MM_ANALYZE_INCOM_ASN'
          EXPORTING
            s_docnum         = it_list-docnum
            s_status         = it_list-status
            only_lifex_traid = 'X'
          IMPORTING
            id_traid         = it_list-traid
            id_lifex         = it_list-lifex
          TABLES
            it_id            = it_id.
      ENDIF.
      CONCATENATE it_list-traid '/' it_list-lifex
                          INTO it_list-cimtyp."M_3
*"M_3
    ENDIF.
    CASE it_stalight-stalight.
      WHEN '1'.
        MOVE icon_yellow_light TO it_list-statusicon.
        it_list-rcode = '000'.
      WHEN '2'.
        MOVE icon_green_light TO it_list-statusicon.
        it_list-rcode = '000'.
      WHEN '3'.
        MOVE icon_red_light TO it_list-statusicon.
        it_list-rcode = '100'.
      WHEN OTHERS.
        MOVE icon_yellow_light TO it_list-statusicon.
    ENDCASE.

    MOVE text-017        TO it_list-directtext.
    MOVE it_edidc-sndprt TO it_list-partnr(2). "partnerart
    MOVE '/'             TO it_list-partnr+2(1).
    MOVE it_edidc-sndpfc TO it_list-partnr+3(2). "partnerrolle
    MOVE '/'             TO it_list-partnr+5(1).
    MOVE it_edidc-sndprn TO it_list-partnr+6(10). "partnernummer
    MOVE it_edidc-rcvprt TO it_list-ident(2). "partnerart
    MOVE '/'             TO it_list-ident+2(1).
    MOVE it_edidc-rcvpfc TO it_list-ident+3(2). "partnerrolle
    MOVE '/'             TO it_list-ident+5(1).
    MOVE it_edidc-rcvprn TO it_list-ident+6(10). "partnernummer
    MOVE it_edidc-sndpor TO it_list-rcvpor.
    MOVE it_edidc-rcvpor TO it_list-sndpor.
*    ENDIF.

*--> read text
    PERFORM func_idoc_read_completely USING    it_list-docnum
                                      CHANGING it_list-partner_idlf
                                               it_list-partner_idwe
                                               it_list-name1_we
                                               it_list-lifex
                                               it_list-traid
                                               it_list-bolnr.

*    IF it_edidc-sndprn = 'SEF9' OR  it_edidc-sndprn = 'SAFE' .
    it_list-partner_idlf = it_edidc-sndprn.
*    ENDIF.
    APPEND it_list.
  ENDLOOP.


ENDFORM.                    " arrange_list
*&---------------------------------------------------------------------*
*&      Form  list
*&---------------------------------------------------------------------*
*       display list
*----------------------------------------------------------------------*
FORM list .
  DESCRIBE TABLE it_list .
  IF sy-tfill > 0.
    PERFORM alv.
  ELSE.
    MESSAGE s429(mo) .
  ENDIF.
ENDFORM.                    " list
*&---------------------------------------------------------------------*
*&      Form  alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv .
  PERFORM alv_content.
  PERFORM alv_func_grid.
ENDFORM.                    " alv
*&---------------------------------------------------------------------*
*&      Form  alv_content
*&---------------------------------------------------------------------*
*       make alv variants
*----------------------------------------------------------------------*
FORM alv_content .
*** variant
  g_repid = sy-repid.

*** layout.
  CLEAR is_layout.
  is_layout-zebra                 = 'X'.
  is_layout-box_fieldname         = 'MARK'.
*  is_layout-coltab_fieldname = 'COLOR'.
*  is_layout-no_subtotals     = 'X'.

*** grid_settings
*  i_grid_settings-edt_cll_cb = 'X'.

*** field catalog
  PERFORM alv_fieldcat.

ENDFORM.                    " alv_content
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fieldcat .
  DATA : l_fieldcat TYPE slis_fieldcat_alv.
  DATA : col_pos(2) TYPE n.

  REFRESH it_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'LISTEDIDC'
    CHANGING
      ct_fieldcat            = it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT it_fieldcat INTO l_fieldcat.
    CASE l_fieldcat-fieldname.
      WHEN 'DOCNUM'.
        l_fieldcat-key                 = 'X'.
*        l_fieldcat-hotspot             = 'X'.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'MESTYP'.
        l_fieldcat-outputlen        = 10.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'DIRECT' OR 'STATUS'.
        l_fieldcat-outputlen        = 6.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'ARCKEY'.
        l_fieldcat-outputlen           = 12. "M_3 orig was 16
        l_fieldcat-reptext_ddic        = 'ASN number'.
        l_fieldcat-seltext_l           = 'ASN number'.
        l_fieldcat-seltext_m           = 'ASN number'.
        l_fieldcat-seltext_s           = 'ASN no'.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'CIMTYP'.
        l_fieldcat-outputlen           = 16.
        l_fieldcat-reptext_ddic        = 'ASN-ID'.
        l_fieldcat-seltext_l           = 'TRAID/LIFEX'.
        l_fieldcat-seltext_m           = 'TRAID/LIFEX'.
        l_fieldcat-seltext_s           = 'ASN-ID'.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'REFMES'.
        l_fieldcat-outputlen           = 10.
        l_fieldcat-reptext_ddic        = 'Rev. date'.
        l_fieldcat-seltext_l           = 'Receiving date'.
        l_fieldcat-seltext_m           = 'Receive date'.
        l_fieldcat-seltext_s           = 'Rev. date'.
        MODIFY it_fieldcat FROM l_fieldcat.
      WHEN 'RCVPOR' OR 'MESCOD' OR 'MESFCT' OR 'TEST' OR 'SNDPOR' OR
           'CIMTYP' OR 'IDOCTP' OR 'DOCREL' OR 'STD' OR 'STATXT' OR
           'ARCKEY' OR 'STDVRS' OR 'STDMES' OR 'OUTMOD' OR
           'SERIAL' OR 'UPDDAT' OR 'UPDTIM' OR 'IDENT' OR
           'MAXSEGNUM' OR 'DIRECTTEXT' OR 'REFGRP'.
        DELETE TABLE it_fieldcat FROM l_fieldcat.
    ENDCASE.
    col_pos = l_fieldcat-col_pos.
  ENDLOOP.

  CLEAR l_fieldcat.
  col_pos = col_pos + 1.
  l_fieldcat-col_pos             = col_pos.
  l_fieldcat-fieldname           = 'RCODE'.
  l_fieldcat-reptext_ddic        = 'Result code'.
  l_fieldcat-outputlen           = 6.
  APPEND l_fieldcat TO it_fieldcat.

  CLEAR l_fieldcat.
  col_pos = col_pos + 1.
  l_fieldcat-col_pos             = col_pos.
  l_fieldcat-fieldname           = 'MSG'.
  l_fieldcat-reptext_ddic        = 'Message'.
  l_fieldcat-outputlen           = 40.
  APPEND l_fieldcat TO it_fieldcat.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_func_grid
*&---------------------------------------------------------------------*
*       CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*----------------------------------------------------------------------*
FORM alv_func_grid .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_background_id          = g_background_id
      i_callback_pf_status_set = g_status
      i_callback_user_command  = g_user_command
      i_grid_settings          = i_grid_settings
      is_layout                = is_layout
      it_fieldcat              = it_fieldcat
      it_sort	                 = it_sort
      i_save                   = 'X'
      it_events                = it_events
    TABLES
      t_outtab                 = it_list.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " alv_func_grid
*&---------------------------------------------------------------------*
*&      Form  alv_status
*&---------------------------------------------------------------------*
*      main  pf status
*----------------------------------------------------------------------*
FORM alv_status USING extab TYPE slis_t_extab .
  DATA : l_extab TYPE slis_extab.
  SET PF-STATUS 'GRID'. " EXCLUDING extab.
ENDFORM.                    " pf_status
*&---------------------------------------------------------------------*
*&      Form  alv_command
*&---------------------------------------------------------------------*
*      alv command
*----------------------------------------------------------------------*
FORM alv_command USING u_ucomm     LIKE sy-ucomm
                       rs_selfield TYPE slis_selfield.
  CASE u_ucomm.
    WHEN 'REFR'.


    WHEN '&IC1'.
      PERFORM alv_command_link USING rs_selfield-fieldname
                                     rs_selfield-value
                                     rs_selfield-tabindex.
    WHEN 'SAVE'.
      PERFORM alv_command_save.

    WHEN 'MSG'.
      PERFORM alv_command_show_message.

    WHEN OTHERS.

  ENDCASE.
  rs_selfield-refresh    = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.
ENDFORM.                    "user_command_alv
*&---------------------------------------------------------------------*
*&      Form  read_status_table
*&---------------------------------------------------------------------*
*       read status table
*----------------------------------------------------------------------*
FORM read_status_table .
  REFRESH : it_teds2, it_stacust, it_stalight.
* Statustext
  SELECT *
    FROM teds2
    INTO TABLE it_teds2
   WHERE langua EQ sy-langu.
* Statusgroups
  SELECT *
    FROM stacust
    INTO CORRESPONDING FIELDS OF TABLE it_stacust.

* Traffic Light Assignment
  SELECT *
    FROM stalight
    INTO CORRESPONDING FIELDS OF TABLE it_stalight.
ENDFORM.                    " read_status_table
*&---------------------------------------------------------------------*
*&      Form  alv_command_link
*&---------------------------------------------------------------------*
*       link detail screen
*----------------------------------------------------------------------*
FORM alv_command_link  USING    u_fieldname
                                u_value
                                u_tabindex.
  CASE u_fieldname.
    WHEN 'DOCNUM'.
      RANGES : range_credat FOR edidc-credat.
      SUBMIT rseidoc2 WITH docnum = u_value
                      WITH credat IN range_credat
                      AND  RETURN.
  ENDCASE.

ENDFORM.                    " alv_command_link
*&---------------------------------------------------------------------*
*&      Form  read_idoc_message
*&---------------------------------------------------------------------*
*       read idoc message
*   return code     message
*   1 (Warning)    2nd message of IDoc status record
*   2 (Success)    blank
*   3 (Error)      1st message of IDoc status record
*     in error case
*      send only error message

*->    Retry data
*   When, msgid = ME msgno = 185, 186 and status = 51
*   or status = 64
*   Execute a program in RBDINPUT by background
*   then... if Result is success
*   Status code will be changed 52.
*----------------------------------------------------------------------*
FORM read_idoc_message  USING    u_docnum
                                 u_recode
                                 u_status
                                 u_partner "M_2
                        CHANGING c_msg
                                 c_recode
                                 c_retry
                                 c_cimtyp."M_3

  DATA: r_likp LIKE likp,check_asn_exist,t30(30),tgr(30),"M_3
        c_traid LIKE likp-traid,c_lifex LIKE likp-lifex.
  DATA : l_msg     TYPE idoc_msg,
         t_msg     TYPE idoc_msg,
         l_strlen  TYPE i.         "text field length

  DATA : msgid  LIKE sy-msgid,
         msgno  LIKE sy-msgno,
         statyp LIKE sy-msgty.

  DATA : BEGIN OF t_idoc_status OCCURS 0.
          INCLUDE STRUCTURE edids.
  DATA : END OF t_idoc_status.

  DATA : BEGIN OF status_code,
          sap(3) TYPE c,
          msgid  LIKE sy-msgid,
          msgno  LIKE sy-msgno,
         END OF status_code.

  DATA : gd_edids      TYPE edids.
  DATA : l_vbeln TYPE vbuk-vbeln.

* Read data records.
  SELECT *
    FROM edids
    INTO TABLE t_idoc_status
   WHERE docnum  EQ u_docnum
     AND stamid  NE space
     AND stamno  NE space
     AND status  NOT IN ('62', '50').

* Get most recent status record
  SORT t_idoc_status DESCENDING BY countr.

*==> Delete all status records that have another creation time or
*    do not fit the select pattern.
  LOOP AT t_idoc_status.
    IF u_docnum <> gd_edids-docnum.
      MOVE t_idoc_status TO gd_edids.
    ENDIF.
    CHECK NOT ( t_idoc_status-credat = gd_edids-credat
        AND     t_idoc_status-cretim = gd_edids-cretim ) .
    DELETE t_idoc_status.
  ENDLOOP.


  LOOP AT t_idoc_status.
* Get msgid and msgno from STAMID, STAMNO. If empty, use STACOD.
    CLEAR : msgid, msgno, statyp, l_msg.
    msgid  = t_idoc_status-stamid.
    msgno  = t_idoc_status-stamno.
    statyp = t_idoc_status-statyp.
    IF statyp = space.
      statyp = 'E'.
    ENDIF.
*==> check a locked idoc document
    IF ( u_status = '51' AND msgid = 'ME' AND
       ( msgno = '185' OR msgno = '186' ) ) OR u_status = '64'.
      c_retry = 'X'.
    ENDIF.

    IF u_recode = '3'.   "error
      CHECK statyp = 'E' OR  statyp = 'A'.
    ENDIF.
    MESSAGE ID msgid TYPE statyp NUMBER msgno
       WITH t_idoc_status-stapa1
            t_idoc_status-stapa2
            t_idoc_status-stapa3
            t_idoc_status-stapa4
       INTO l_msg.



*--> in case of resultcode = 'W'
*    overall packing status of all items in inbound delivery ne 'C'
*    change resultcode to 'E'
*    IF u_recode = '1' AND sy-tabix = 1
*       AND t_idoc_status-stapa2 NE space.
    IF t_idoc_status-stamid = 'ME' AND t_idoc_status-stamno = '780'
       AND t_idoc_status-stapa2 NE space.
      CLEAR l_vbeln.
      l_vbeln = t_idoc_status-stapa2.

      SELECT SINGLE *
        FROM vbuk
       WHERE vbeln   = l_vbeln.
*      IF VBUK-PKSTK  NE 'C'.
      IF vbuk-pkstk  EQ 'A' OR
         vbuk-pkstk  EQ ' ' OR
         vbuk-pkstk  EQ 'D'.
        c_recode  = '3'.
      ENDIF.
    ENDIF.

* make message text
    CONDENSE l_msg.
    CONCATENATE c_msg l_msg INTO t_msg SEPARATED BY space.
    CLEAR c_msg.
    c_msg = t_msg.
    CLEAR : t_msg, l_strlen.
    l_strlen = strlen( c_msg ).
    CHECK l_strlen > 250.
    EXIT.
  ENDLOOP.

  IF addmess = 'X' AND u_partner IN adm_vend. "M_2
    IF u_partner IN exi_vend. "M_3
      check_asn_exist = 'X'.
    ELSE.
      CLEAR check_asn_exist.
    ENDIF.
    CALL FUNCTION 'Z_MM_ANALYZE_INCOM_ASN'     "M_2
       EXPORTING  s_docnum  = u_docnum
                  s_status  = u_status
                  p_traid   = u_traid
                  check_asn_exist = check_asn_exist "M_3
       IMPORTING  messag2    = mess2
                  find_traid = u_find_traid
                  r_likp     = r_likp               "M_3
                  id_traid   = c_traid
                  id_lifex   = c_lifex
       TABLES it_id = it_id.
    IF NOT r_likp IS INITIAL. "M_3
      CONCATENATE r_likp-traid '/' r_likp-lifex INTO t500.
      CONCATENATE r_likp-erdat '-' r_likp-ernam INTO t30.
      IF r_likp-wadat_ist IS INITIAL.
        tgr = 'No GR'.
      ELSE.
        CONCATENATE 'GR' r_likp-wadat_ist INTO tgr SEPARATED BY space.
      ENDIF.
*
      CONCATENATE 'ASN' t500 'Already in KMMG' t30 tgr
                  INTO alr_exist_msg-messag2 SEPARATED BY space.
      MOVE u_docnum TO alr_exist_msg-docnum.
      APPEND alr_exist_msg.
*
      CONCATENATE 'ASN' t500 'Already in KMMG' t30 tgr '|..|' c_msg
                   INTO t500 SEPARATED BY space.
      MOVE t500(255) TO c_msg.
    ENDIF.
*
    CONCATENATE c_traid '/' c_lifex INTO c_cimtyp."M_3
*
    IF NOT mess2 IS INITIAL.
      MOVE:u_docnum TO it_msg-docnum,
           mess2 TO it_msg-messag2.
      APPEND it_msg.
      CONCATENATE c_msg '|..|' mess2 INTO t500 SEPARATED BY space.
      MOVE t500(255) TO c_msg.
    ENDIF.
  ENDIF.

ENDFORM.                    " read_idoc_message
*&---------------------------------------------------------------------*
*&      Form  alv_command_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_command_save .
  CALL FUNCTION 'MESSAGES_INITIALIZE'.
*  PERFORM send_retry_idoc. "M_1 rem
  PERFORM send_retry_idoc_new.                              "M_1 ins
  PERFORM send_item_collect.


** Furong on 09/14/12
*  CLEAR : IT_SEND_ITEM[], IT_SEND_ITEM.
*  IT_SEND_ITEM[] = GT_SEND_ITEM[].
** End

  PERFORM send_item.
  PERFORM send_item_result.
ENDFORM.                    " alv_command_save
*&---------------------------------------------------------------------*
*&      Form  send_item_collect
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_item_collect .
  DATA : l_pos(3) TYPE n.

  REFRESH : it_send_head, it_send_item, it_send_sef9, it_send_safe,
it_send_sstx.
  CLEAR : gt_send_item[], gt_send_item.

  LOOP AT it_list WHERE mark = 'X' AND retry = space.
*--> lock item
*    CALL FUNCTION 'ENQUEUE_ES_EDIDOCE'
*      EXPORTING
*        mode_edidc     = 'E'
*        mandt          = sy-mandt
*        docnum         = it_list-docnum
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'MESSAGE_STORE'
*        EXPORTING
*          arbgb                  = sy-msgid
*          msgty                  = sy-msgty
*          msgv1                  = sy-msgv1
*          msgv2                  = sy-msgv2
*          msgv3                  = sy-msgv3
*          msgv4                  = sy-msgv4
*          txtnr                  = sy-msgno
*          zeile                  = it_list-docnum
*        EXCEPTIONS
*          message_type_not_valid = 1
*          not_active             = 2.
*    ENDIF.
*
*    CHECK sy-subrc = 0.
    CLEAR : it_send_head, it_send_sef9, it_send_safe, it_send_sstx.

    CASE it_list-partner_idlf.
      WHEN 'SEF9'.    "Glovis
        MOVE-CORRESPONDING it_list TO it_send_sef9.
        it_send_sef9-message = it_list-msg.
**C__ Paul change.
        IF it_send_sef9-rcode = '000'.
          it_send_sef9-refmes = it_list-msg+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND it_send_sef9.
**C__ Paul change.
*      WHEN 'SAFE'.    "KMC
      WHEN 'SBC3'.    "KMC
**E__ 06/16/11
        MOVE-CORRESPONDING it_list TO it_send_safe.
        it_send_safe-message = it_list-msg.
**C__ Paul change.
        IF it_send_safe-rcode = '000'.
          it_send_safe-refmes = it_list-msg+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND it_send_safe.
      WHEN 'SSTX'.    "Glovis China  03/20/2010 by Victor
        MOVE-CORRESPONDING it_list TO it_send_sstx.
        it_send_sstx-message = it_list-msg.
**C__ Paul change.
        IF it_send_sstx-rcode = '000'.
          it_send_sstx-refmes = it_list-msg+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND it_send_sstx.
      WHEN OTHERS.
        MOVE-CORRESPONDING it_list TO it_send_head.
** Furong on 09/21/12
        it_send_head-arckey = it_list-lifex.
        CONCATENATE it_list-upddat it_list-updtim
               INTO it_send_head-refmes.
** End on 09/21/12
        it_send_item-message = it_list-msg.
**C__ Paul change.
**        IF IT_SEND_ITEM-RCODE = '000'.
**          IT_SEND_ITEM-REFMES = IT_LIST-MSG+1(10).
**        ENDIF.
**E__ 06/30/11
        l_pos                = l_pos + 1.
        it_send_head-pos     = l_pos.

        COLLECT it_send_head.
        PERFORM get_item_message USING it_send_head-pos
                                       it_list-docnum
                                       it_list-lifex
                                       it_list-rcode.

** furong on 09/14/12
*        PERFORM CREATE_ALEAUD_IDOC_CREATE.
** End
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " send_item_collect
*&---------------------------------------------------------------------*
*&      Form  send_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_item .
  CLEAR : g_dest, g_dest_ckd.
*  CONCATENATE 'KMMG_EDI_CLNT' p_dest INTO g_dest.  "Target destination
*  CONCATENATE 'KMMG_EAI_CLNT' p_dest INTO g_dest_ckd. "Glovis/KMC Desc

*--> sending data
  IF NOT  it_send_head[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_001_DB'
      TABLES
        t_head = it_send_head
        t_item = it_send_item.
  ENDIF.

*--> only sending glovis and KMC data
  IF NOT  it_send_sef9[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
      TABLES
        t_idoc = it_send_sef9.
  ENDIF.
  IF NOT it_send_safe[] IS  INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
*      DESTINATION g_dest_ckd
      TABLES
        t_idoc = it_send_safe.
  ENDIF.

*-- glovis China 03/30/2010 by Victor
  IF  NOT it_send_sstx[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
      TABLES
        t_idoc = it_send_sstx.
  ENDIF.
ENDFORM.                    " send_item
*&---------------------------------------------------------------------*
*&      Form  send_item_result
*&---------------------------------------------------------------------*
*       check return message
*----------------------------------------------------------------------*
FORM send_item_result .
*--> error message
  LOOP AT it_send_head WHERE if_return = 'E'.
    LOOP AT it_send_item WHERE pos = it_send_head-pos.
      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = '25'
          msgty                  = 'E'
          msgv1                  = it_send_item-docnum
          msgv2                  = ''
          msgv3                  = ''
          msgv4                  = ''
          txtnr                  = '206'
          zeile                  = it_send_item-docnum
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2.
    ENDLOOP.
  ENDLOOP.
  IF sy-subrc NE 0 AND NOT it_send_head[] IS  INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = 'FMBAPI'
        msgty                  = 'S'
        msgv1                  = 'Local vendor ASN'
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '025'
        zeile                  = '001'
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDIF.

  LOOP AT it_send_sef9 WHERE if_return = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = '25'
        msgty                  = 'E'
        msgv1                  = it_send_sef9-docnum
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '206'
        zeile                  = it_send_sef9-docnum
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDLOOP.
  IF sy-subrc NE 0 AND NOT  it_send_sef9[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = 'FMBAPI'
        msgty                  = 'S'
        msgv1                  = 'CKD vendor ASN'
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '025'
        zeile                  = '001'
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDIF.

  LOOP AT it_send_safe WHERE if_return = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = '25'
        msgty                  = 'E'
        msgv1                  = it_send_safe-docnum
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '206'
        zeile                  = it_send_safe-docnum
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDLOOP.
  IF sy-subrc NE 0 AND NOT it_send_safe[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = 'FMBAPI'
        msgty                  = 'S'
        msgv1                  = 'CKD vendor ASN'
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '025'
        zeile                  = '001'
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDIF.

*-03/30/2010 by Victor  Glovis China
  LOOP AT it_send_sstx WHERE if_return = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = '25'
        msgty                  = 'E'
        msgv1                  = it_send_sstx-docnum
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '206'
        zeile                  = it_send_sstx-docnum
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDLOOP.
  IF sy-subrc NE 0 AND NOT it_send_sstx[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = 'FMBAPI'
        msgty                  = 'S'
        msgv1                  = 'CKD vendor ASN'
        msgv2                  = ''
        msgv3                  = ''
        msgv4                  = ''
        txtnr                  = '025'
        zeile                  = '001'
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDIF.

*--> Update success item to DB table
** Changed on 08/31/11 by Furong - update Idoc ack. for all ('E')
  LOOP AT it_send_head.   " WHERE IF_RETURN = 'S'.
    LOOP AT it_send_item WHERE pos = it_send_head-pos.
      UPDATE edidc SET refint   = 'Acknowledged'
                   WHERE docnum = it_send_item-docnum.
    ENDLOOP.
  ENDLOOP.
  LOOP AT it_send_sef9.   " WHERE IF_RETURN = 'S'.
    UPDATE edidc SET refint   = 'Acknowledged'
                 WHERE docnum = it_send_sef9-docnum.
  ENDLOOP.
  LOOP AT it_send_safe.  " WHERE IF_RETURN = 'S'.
    UPDATE edidc SET refint   = 'Acknowledged'
                 WHERE docnum = it_send_safe-docnum.
  ENDLOOP.
*-03/30/2010 by Victor
  LOOP AT it_send_sstx.  " WHERE IF_RETURN = 'S'.
    UPDATE edidc SET refint   = 'Acknowledged'
                 WHERE docnum = it_send_sstx-docnum.
  ENDLOOP.

  MESSAGE s526(/sapht/drm01) .    "success message
ENDFORM.                    " send_item_result
*&---------------------------------------------------------------------*
*&      Form  alv_command_show_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_command_show_message .
  CALL FUNCTION 'MESSAGES_SHOW'.
ENDFORM.                    " alv_command_show_message
*&---------------------------------------------------------------------*
*&      Form  batch_target_item
*&---------------------------------------------------------------------*
*       when program is run by background job
*        Choose all items automatically
*----------------------------------------------------------------------*
FORM batch_target_item .
  it_list-mark = 'X'.
  MODIFY it_list TRANSPORTING mark WHERE mark = space.
ENDFORM.                    " batch_target_item
*&---------------------------------------------------------------------*
*&      Form  func_IDOC_READ_COMPLETELY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM func_idoc_read_completely  USING    u_docnum
                                CHANGING c_idlf
                                         c_idwe
                                         c_name1_we
                                         c_lifex
                                         c_traid
                                         c_bolnr.

  DATA : int_edids LIKE TABLE OF edids WITH HEADER LINE,
         int_edidd LIKE TABLE OF edidd WITH HEADER LINE.
  DATA : l_e1adrm1 TYPE e1adrm1,
         l_e1edl20 TYPE e1edl20.

  CALL FUNCTION 'IDOC_READ_COMPLETELY'
    EXPORTING
      document_number         = u_docnum
    TABLES
      int_edids               = int_edids
      int_edidd               = int_edidd
    EXCEPTIONS
      document_not_exist      = 1
      document_number_invalid = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    LOOP AT int_edidd WHERE segnam = 'E1EDL20'
                         OR segnam = 'E1ADRM1'.
      CLEAR : l_e1edl20, l_e1adrm1.
      CASE int_edidd-segnam.
        WHEN  'E1EDL20'.
          MOVE int_edidd-sdata TO l_e1edl20.
          c_lifex = l_e1edl20-lifex.
          c_traid = l_e1edl20-traid.
          c_bolnr = l_e1edl20-bolnr.

        WHEN 'E1ADRM1'.
          MOVE int_edidd-sdata TO l_e1adrm1.
          IF l_e1adrm1-partner_q = 'LF'.
            c_idlf     = l_e1adrm1-partner_id.
          ELSEIF l_e1adrm1-partner_q = 'WE'.
            c_idwe     = l_e1adrm1-partner_id.
            c_name1_we = l_e1adrm1-name1.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " func_IDOC_READ_COMPLETELY
*&---------------------------------------------------------------------*
*&      Form  get_item_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_item_message  USING  u_pos  u_docnum  u_lifex  u_rcode  .
  DATA : l_msg_seq   TYPE edi_stamno.
  DATA : l_msg     TYPE idoc_msg,
         t_msg     TYPE idoc_msg,
         l_flg,
         l_strlen  TYPE i.         "text field length

  DATA : msgid  LIKE sy-msgid,
         msgno  LIKE sy-msgno,
         statyp LIKE sy-msgty.

  DATA : BEGIN OF t_idoc_status OCCURS 0.
          INCLUDE STRUCTURE edids.
  DATA : END OF t_idoc_status.

  DATA : BEGIN OF status_code,
          sap(3) TYPE c,
          msgid  LIKE sy-msgid,
          msgno  LIKE sy-msgno,
         END OF status_code.

  DATA : gd_edids      TYPE edids.
  DATA : l_vbeln TYPE vbuk-vbeln.

* Read data records.
  SELECT *
    FROM edids
    INTO TABLE t_idoc_status
   WHERE docnum  EQ u_docnum
     AND stamid  NE space
     AND stamno  NE space
     AND status  NOT IN ('62', '64', '50', '68').


* Get most recent status record
  SORT t_idoc_status DESCENDING BY countr.

*==> Delete all status records that have another creation time or
*    do not fit the select pattern.
  LOOP AT t_idoc_status.
    IF u_docnum <> gd_edids-docnum.
      MOVE t_idoc_status TO gd_edids.
    ENDIF.
    CHECK NOT ( t_idoc_status-credat = gd_edids-credat
        AND     t_idoc_status-cretim = gd_edids-cretim ) .
    DELETE t_idoc_status.
  ENDLOOP.

  SORT t_idoc_status.

*  LOOP AT T_IDOC_STATUS.
*S__PAUL
  LOOP AT t_idoc_status WHERE statyp EQ 'S'
                           OR statyp EQ 'E'
                           OR statyp EQ ''.
* Get msgid and msgno from STAMID, STAMNO. If empty, use STACOD.
    CLEAR : msgid, msgno, statyp, l_msg.
    msgid  = t_idoc_status-stamid.
    msgno  = t_idoc_status-stamno.
    statyp = t_idoc_status-statyp.
    IF statyp = space.
      statyp = 'E'.
    ENDIF.

    IF u_rcode = '100'.   "error
      CHECK statyp = 'E' OR  statyp = 'A'.
    ENDIF.
    MESSAGE ID msgid TYPE statyp NUMBER msgno
       WITH t_idoc_status-stapa1
            t_idoc_status-stapa2
            t_idoc_status-stapa3
            t_idoc_status-stapa4
       INTO l_msg.

    IF msgid = 'BORGR' AND
       msgno = '520'.
      l_flg = 'X'.
      l_msg = t_idoc_status-stapa1.
      l_vbeln = l_msg.
    ENDIF.
* make message text
    l_msg_seq  = l_msg_seq  + 1.
    CLEAR it_send_item.
    it_send_item-pos        = u_pos.
    it_send_item-docnum     = u_docnum.
    it_send_item-lifex      = u_lifex.
    it_send_item-rcode      = u_rcode.
    it_send_item-msg_seq    = l_msg_seq.
    it_send_item-message    = l_msg.
    APPEND it_send_item.

    AT END OF docnum.
      IF l_flg = 'X'.
        DELETE it_send_item WHERE docnum = u_docnum
                              AND ( message <> l_vbeln ).
      ENDIF.
      CLEAR : l_vbeln, l_flg.
    ENDAT.
  ENDLOOP.

  IF addmess = 'X'. "M_2
    LOOP AT it_send_item.
      READ TABLE it_msg WITH KEY docnum = u_docnum.
      IF sy-subrc = 0.
        CONCATENATE it_send_item-message '|..|' it_msg-messag2
                   INTO t500 SEPARATED BY space.
        MOVE t500(255) TO it_send_item-message.
        MODIFY it_send_item.
      ENDIF.
*
      READ TABLE alr_exist_msg WITH KEY docnum = u_docnum.
      IF sy-subrc = 0.
        CONCATENATE alr_exist_msg-messag2  '|..|'  it_send_item-message
                   INTO t500 SEPARATED BY space.
        MOVE t500(255) TO it_send_item-message.
        MODIFY it_send_item.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
ENDFORM.                    " get_item_message
*&---------------------------------------------------------------------*
*&      Form  send_retry_idoc
*&---------------------------------------------------------------------*
*->    Retry data
*   When, msgid = ME msgno = 185, 186 and status = 51
*   or status = 64
*   Execute a program in RBDMANI2 by background
*   then... if Result is success
*   Status code will be changed 52.
*----------------------------------------------------------------------*
FORM send_retry_idoc .
  RANGES : s_docnu FOR edidc-docnum.
  DATA : l_tabix TYPE sy-tabix.

  LOOP AT it_list WHERE retry = 'X'.
    l_tabix         = l_tabix + 1.
    s_docnu-low     = it_list-docnum.
    s_docnu-sign    = 'I'.
    s_docnu-option  = 'EQ'.
    APPEND s_docnu.
    IF l_tabix = 100.
      SUBMIT rbdmani2
        WITH so_docnu IN s_docnu
         AND RETURN.
      REFRESH s_docnu.
      CLEAR l_tabix.
    ELSE.
      AT LAST.
        SUBMIT rbdmani2
          WITH so_docnu IN s_docnu
           AND RETURN.
        REFRESH s_docnu.
        CLEAR l_tabix.
      ENDAT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " send_retry_idoc
*&---------------------------------------------------------------------*
*&      Form  send_retry_idoc_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_retry_idoc_new . "M_1
  DATA:n64 TYPE i,nn TYPE i.
  RANGES:s_docnu FOR edidc-docnum,
         s_docnu64 FOR edidc-docnum.
  DATA : l_tabix TYPE sy-tabix.

  LOOP AT it_list WHERE retry = 'X'.
    l_tabix         = l_tabix + 1.
    s_docnu-low     = it_list-docnum.
    s_docnu-sign    = 'I'.
    s_docnu-option  = 'EQ'.
    IF it_list-status = 64 AND p_64 = 'X'.
      MOVE s_docnu TO s_docnu64.
      APPEND s_docnu64.
      n64 = n64 + 1.
    ELSE.
      nn = nn + 1.
      APPEND s_docnu.
    ENDIF.
    IF l_tabix = 100.
      IF n64 > 0. "M_1
        SUBMIT rbdapp01  WITH docnum IN s_docnu64 AND RETURN. "M_1
      ENDIF.
      IF nn > 0.
        SUBMIT rbdmani2 WITH so_docnu IN s_docnu AND RETURN.
      ENDIF.
      REFRESH:s_docnu,s_docnu64.
      CLEAR:l_tabix,n64,nn.
    ELSE.
      AT LAST.
        IF n64 > 0. "M_1
          SUBMIT rbdapp01  WITH docnum IN s_docnu64 AND RETURN. "M_1
        ENDIF.
        IF nn > 0.
          SUBMIT rbdmani2 WITH so_docnu IN s_docnu AND RETURN.
        ENDIF.
        REFRESH:s_docnu,s_docnu64.
        CLEAR:l_tabix,n64,nn.
      ENDAT.
    ENDIF.
  ENDLOOP.
  IF n64 > 0. "M_1
    SUBMIT rbdapp01  WITH docnum IN s_docnu64 AND RETURN.
  ENDIF.
  IF nn > 0.
    SUBMIT rbdmani2 WITH so_docnu IN s_docnu AND RETURN.
  ENDIF.
ENDFORM.                    " send_retry_idoc_new

*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHER_RUNNING
*&---------------------------------------------------------------------*
*       Check Other Running
*----------------------------------------------------------------------*
FORM check_other_running .
  DATA: ls_program TYPE zmmt0000,
        l_count TYPE i.
  SELECT SINGLE *
    INTO ls_program
    FROM zmmt0000
   WHERE programm = sy-cprog.
  IF sy-subrc <> 0.
    ls_program-programm = sy-cprog.
    INSERT zmmt0000 FROM ls_program.
  ENDIF.
  CALL FUNCTION 'ENQUEUE_EZ_ZMMT0000'
    EXPORTING
      programm       = 'ZMMR30400T'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE PROGRAM.
  ENDIF.


*CONSTANTS:
*  btc_running       LIKE tbtco-status VALUE 'R',
*  btc_ready         LIKE tbtco-status VALUE 'Y',
*  btc_scheduled     LIKE tbtco-status VALUE 'P',
*  btc_released      LIKE tbtco-status VALUE 'S',
*  btc_aborted       LIKE tbtco-status VALUE 'A',
*  btc_finished      LIKE tbtco-status VALUE 'F',
*  btc_put_active    LIKE tbtco-status VALUE 'Z',
*  btc_unknown_state LIKE tbtco-status VALUE 'X'.


  SELECT COUNT(*)
    INTO l_count
    FROM  tbtcp AS a INNER JOIN tbtco AS b
                        ON a~jobname  EQ b~jobname
                       AND a~jobcount EQ b~jobcount
   WHERE a~progname = sy-cprog
     AND b~status NOT IN ('F', 'A', 'S', 'P').
  IF l_count > 1.
    MESSAGE s127(c$).
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " CHECK_OTHER_RUNNING
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALEAUD_IDOC_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_aleaud_idoc_create.
  DATA : lv_docnum LIKE it_send_item-docnum.
  CLEAR: lv_docnum.

  SELECT SINGLE rcvpor INTO l_rcvpor FROM edp13
     WHERE rcvprn = it_list-partnr+6(10)
       AND rcvprt = 'LI'
       AND rcvpfc = ''
       AND mestyp = 'ALEAUD'.

  IF sy-subrc NE 0 AND sy-batch EQ 'X'.
    CONCATENATE ' Maintain Partner profile for Message type ALEAUD for'
   it_list-partnr+6(10) INTO it_list-msg .
  ELSE.

* Control Record
    idoc_control_complete-doctyp = 'ALEAUD01'.
    idoc_control_complete-mestyp = 'ALEAUD'.
    idoc_control_complete-direct = '1'.
    idoc_control_complete-outmod = '2'.

* Sender of original DESADV willl be receiver
    idoc_control_complete-rcvprt = it_list-partnr(2).
    idoc_control_complete-rcvprn = it_list-partnr+6(10).
* Select Port
    SELECT SINGLE rcvpor INTO l_rcvpor FROM edp13
       WHERE rcvprn = idoc_control_complete-rcvprn
         AND rcvprt = idoc_control_complete-rcvprt
         AND mestyp = idoc_control_complete-mestyp.
    IF sy-subrc EQ 0.
      idoc_control_complete-rcvpor = l_rcvpor.
    ENDIF.

** Receiver of original DESADV ( SAP)  wil be the sender
    idoc_control_complete-sndpor = it_list-sndpor.
    idoc_control_complete-sndprt = it_list-ident(2).
    idoc_control_complete-sndprn = it_list-ident+6(10).

* Populate E1ADHDR segment
    idoc_hdr_seg-mestyp = 'ALEAUD'.
*S__MOD PAUL
*    IDOC_HDR_SEG-MESCOD = ''.
    idoc_hdr_seg-mescod = it_list-rcode.
    idoc_hdr_seg-mesfct = ''.

    IF it_list-rcode = '000'.
      idoc_hdr_seg-mestyp_lng = it_list-msg.
      CONDENSE idoc_hdr_seg-mestyp_lng.
    ELSE.
      idoc_hdr_seg-mestyp_lng = 'E'.
    ENDIF.

    t_idoc_data-segnam = 'E1ADHDR'.
    t_idoc_data-sdata = idoc_hdr_seg.
    APPEND t_idoc_data.

    LOOP AT it_send_item.
      idoc_seg-docnum = it_send_item-docnum.
      idoc_seg-statxt = it_send_item-message." Error Message
      idoc_seg-stapa4 = ''.
      IF it_send_item-rcode = '000'.
        idoc_seg-statyp = 'Success'.
      ELSE.
        idoc_seg-statyp = 'Error'.
      ENDIF.
      idoc_seg-stamid = it_send_item-pos  .
      idoc_seg-stamno = it_send_item-msg_seq.
* DEFAULT VALUE
      IF idoc_seg-stapa1_lng  IS INITIAL.
        idoc_seg-stapa1_lng = 'Hyundai Car Assembly, AL'.
      ENDIF.
*S__MOD PAUL
      idoc_seg-stapa2_lng = it_list-upddat. "IDOC creation Dt.
      idoc_seg-stapa3_lng = it_list-updtim. "IDOC creation time
***     IDOC_SEG-STAPA4_LNG = sy-langu.         "Language
      idoc_seg-stapa4_lng = it_send_item-lifex.  "ASN Number
      t_idoc_data-segnam = 'E1STATE'.
      t_idoc_data-sdata = idoc_seg.
      APPEND t_idoc_data.

* Populate  E1PRTOB segment
      idoc_obj_seg-docnum  = it_list-docnum."Original IDOC No of ASN
      lv_docnum            = it_list-docnum.
      idoc_obj_seg-logsys  = it_list-partner_idlf.   " Partner ID
      idoc_obj_seg-objtype = ''.
*      IDOC_OBJ_SEG-OBJKEY = IT_LIST-PARTNER_IDLF.   " Partner Name
      t_idoc_data-segnam = 'E1PRTOB'.
      t_idoc_data-sdata  = idoc_obj_seg.
      APPEND t_idoc_data.

    ENDLOOP.

    CALL FUNCTION 'IDOC_CREATE_ON_DATABASE'
      EXPORTING
        idoc_status             = idoc_status
        error_occured           = error_occured
      TABLES
        idoc_data               = t_idoc_data
      CHANGING
        idoc_control            = idoc_control_complete
      EXCEPTIONS
        idoc_input_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CLEAR : t_idoc_data, t_idoc_data[].
    ENDIF.

    APPEND idoc_control_complete TO send_control.

*A__Add by PAUL
    UPDATE edidc SET refint   = 'Acknowledged'
             WHERE docnum = lv_docnum.
*E__<

*  loop at it_tab where flag  =   'X'.
*    l_docnum = it_tab-docnum.
*    I_IDOC_STATUS-DOCNUM = IT_LIST-DOCNUM.
*    I_IDOC_STATUS-STATUS = '68'.
*    APPEND I_IDOC_STATUS.
*
*    CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
*         EXPORTING
*              IDOC_NUMBER               = IT_LIST-DOCNUM
*              NO_DEQUEUE_FLAG           = 'X'
*         TABLES
*              IDOC_STATUS               = I_IDOC_STATUS
*         EXCEPTIONS
*              IDOC_FOREIGN_LOCK         = 1
*              IDOC_NOT_FOUND            = 2
*              IDOC_STATUS_RECORDS_EMPTY = 3
*              IDOC_STATUS_INVALID       = 4
*              DB_ERROR                  = 5
*              OTHERS                    = 6.
*    CLEAR :  I_IDOC_STATUS[],I_IDOC_STATUS.
*
*  endloop.

    REFRESH it_edidd.
    CLEAR it_edidd.

    CALL FUNCTION 'EDI_OUTPUT_NEW'
      TABLES
        i_edidc = send_control
        i_edidd = it_edidd
      EXCEPTIONS
        OTHERS  = 0.
** Furong on 03/21/12
    CALL FUNCTION 'DB_COMMIT'.
    CALL FUNCTION 'DEQUEUE_ALL'.
** on 03/21/12
    COMMIT WORK.
    CLEAR: send_control[], send_control.

  ENDIF.

  it_send_head-824 = idoc_control_complete-docnum.

  CLEAR : idoc_control_complete.

  APPEND it_send_item TO gt_send_item.

  CLEAR : it_send_item, it_send_item[].

ENDFORM.                    " CREATE_ALEAUD_IDOC_CREATE
