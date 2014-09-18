*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM p2000_get_data.

  DATA : BEGIN OF lt_key OCCURS 0,
          status LIKE edidc-status,
          sndprn LIKE edidc-sndprn,
          sndprt LIKE edidc-sndprt,
          rcvprn LIKE edidc-rcvprn,
*         RCVPRT LIKE EDIDC-RCVPRT,
          mestyp LIKE edidc-mestyp,
          n_stt  TYPE p,
         END OF lt_key.
*          DESC_SNDPRN LIKE TEDTT-DESCRP,
  DATA : lt_data LIKE TABLE OF lt_key WITH HEADER LINE.
  DATA : BEGIN OF lt_status OCCURS 0,
           status LIKE edidc-status,
         END OF lt_status.

  DATA : lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         lt_edim LIKE TABLE OF edimsgt WITH HEADER LINE,
         lt_teds LIKE TABLE OF teds2 WITH HEADER LINE.

  DATA: percent(3) TYPE n.

  DO 100 TIMES.
    MOVE: sy-index TO percent.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              percentage = percent
              text       = text-001
         EXCEPTIONS
              OTHERS     = 1.

  ENDDO.

  SELECT sndprn rcvprn mestyp status sndprt
    INTO CORRESPONDING FIELDS OF TABLE lt_key
    FROM edidc
    WHERE "CREDAT IN S_DATUM
          upddat IN s_datum
      AND updtim IN s_uptim
      AND mestyp IN s_mestyp
      AND sndprn IN s_sndprn
      AND status IN s_status
      AND docnum IN s_docnm
      AND rcvprt = 'LS'
      AND direct IN s_dirct. "inbound/outbound



  SORT lt_key BY status.
  LOOP AT lt_key.
    lt_status-status = lt_key-status.
    AT END OF status.
      APPEND lt_status.
    ENDAT.
  ENDLOOP.

  SORT lt_key BY sndprn mestyp status.
  LOOP AT lt_key.
    lt_key-n_stt = 1.
    MOVE-CORRESPONDING lt_key TO lt_data.

    COLLECT lt_data.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE  lt_edim
    FROM edimsgt
    FOR ALL ENTRIES IN lt_data
    WHERE mestyp EQ lt_data-mestyp
      AND langua EQ sy-langu.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_teds
    FROM teds2
    FOR ALL ENTRIES IN lt_data
    WHERE status EQ lt_data-status
      AND langua EQ sy-langu.

  CLEAR : ls_fcat .
  ls_fcat-fieldname = 'SNDPRN'.
  ls_fcat-ref_table = 'BDSER'.
  ls_fcat-ref_field = 'SNDPRN'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '10'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'DESCRP'.
  ls_fcat-ref_table = 'TEDTT'.
  ls_fcat-ref_field = 'DESCRP'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '60'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'RCVPRN'.
  ls_fcat-ref_table = 'BDSER'.
  ls_fcat-ref_field = 'RCVPRN'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '10'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

*  LS_FCAT-FIELDNAME = 'DESRCV'.
*  LS_FCAT-REF_TABLE = 'TEDTT'.
*  LS_FCAT-REF_FIELD = 'DESRCV'.
*  LS_FCAT-INTTYPE  = 'C'.
*  LS_FCAT-INTLEN  = '60'.
*  LS_FCAT-KEY  = 'X'.
*  APPEND LS_FCAT TO LT_FCAT.

  ls_fcat-fieldname = 'MESTYP'.
  ls_fcat-ref_table = 'EDIDC'.
  ls_fcat-ref_field = 'MESTYP'.
  ls_fcat-datatype  = 'C'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'MESDESC'.
  ls_fcat-ref_table = 'EDIMSGT'.
  ls_fcat-ref_field = 'DESCRP'.
  ls_fcat-datatype  = 'C'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  CLEAR : ls_fcat.
  DATA : i_field(10), i_cnt TYPE i.
  LOOP AT lt_status .
    CONCATENATE 'STATUS' lt_status-status INTO i_field.

    ls_fcat-fieldname = i_field.
    ls_fcat-seltext = lt_status-status.
    ls_fcat-reptext = lt_status-status.
    ls_fcat-ref_table = 'MSEG'.
    ls_fcat-ref_field = 'MENGE'.
    ls_fcat-quantity = 'EA'.
    READ TABLE lt_teds WITH KEY status = lt_status-status.
    IF sy-subrc EQ 0 .
      ls_fcat-tooltip = lt_teds-descrp .
    ELSE.
      ls_fcat-tooltip = ''.
    ENDIF.
    APPEND ls_fcat TO lt_fcat.
    CLEAR : ls_fcat.

  ENDLOOP.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_fcat
    IMPORTING
      ep_table        = d_tab.

  ASSIGN d_tab->* TO <new_tab>.


  CREATE DATA new_line LIKE LINE OF <new_tab>.
  ASSIGN new_line->* TO <status>.

  FIELD-SYMBOLS : <fs_wa> TYPE ANY.
  DATA : f_txt(50), pnrdesc LIKE tedststruc-name1.

  SORT lt_data BY sndprn mestyp.
  LOOP AT lt_data .

*    CONCATENATE 'NEW_LINE->*-SNDPRN' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

* by ig.moon
    CONCATENATE 'SNDPRN' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    MOVE lt_data-sndprn TO <fs_wa>.
*    <FS_WA> =  LT_DATA-SNDPRN.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-DESCRP' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'DESCRP' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    CALL FUNCTION 'EDI_PARTNER_GET_DESCRIPTION'
         EXPORTING
              pi_partyp = lt_data-sndprt
              pi_parnum = lt_data-sndprn
         IMPORTING
              pe_descrp = pnrdesc
         EXCEPTIONS
              OTHERS    = 0.
    <fs_wa> = pnrdesc.

    UNASSIGN <fs_wa>.
    CONCATENATE 'RCVPRN' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.
    MOVE lt_data-rcvprn TO <fs_wa>.

*FIXME
*    UNASSIGN <FS_WA>.
*    CONCATENATE 'DESRCV' '' INTO F_TXT.
*    ASSIGN COMPONENT F_TXT OF STRUCTURE <STATUS> TO <FS_WA>.
*    CALL FUNCTION 'EDI_PARTNER_GET_DESCRIPTION'
*         EXPORTING
*              PI_PARTYP = 'LS'
*              PI_PARNUM = LT_DATA-RCVPRN
*         IMPORTING
*              PE_DESCRP = PNRDESC
*         EXCEPTIONS
*              OTHERS    = 0.
*    <FS_WA> = PNRDESC.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-MESTYP' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'MESTYP' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    <fs_wa> = lt_data-mestyp.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-MESDESC' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'MESDESC' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    READ TABLE lt_edim WITH KEY mestyp = lt_data-mestyp.
    IF sy-subrc = 0 .
      <fs_wa> = lt_edim-descrp.
    ENDIF.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-STATUS' LT_DATA-STATUS INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'STATUS' lt_data-status  INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.


    <fs_wa> = lt_data-n_stt .



    COLLECT <status> INTO <new_tab>.
    CLEAR <status>.
    UNASSIGN <fs_wa>.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P2100_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p2100_clear_data.

  CLEAR : d_tab, lt_fcat, ls_fcat.
  CLEAR : <new_tab>.
  UNASSIGN <new_tab>.




ENDFORM.                    " P2100_CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_PBDPROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3000_call_rbdproc.
  DATA: gt_seltab TYPE STANDARD TABLE OF rsparams
                  WITH HEADER LINE.
  DATA  seltab LIKE LINE OF gt_seltab.
  DATA: lv_row TYPE i, f_txt(100),
        lv_sndprn LIKE edidc-sndprn,
        lv_mestyp LIKE edidc-mestyp.

  FIELD-SYMBOLS : <fs_sndprn> TYPE ANY,
                  <fs_mestyp> TYPE ANY.

  RANGES : r_status FOR edidc-status.
  CLEAR : gt_seltab[], seltab, <status>.

  CALL METHOD g_grid->get_current_cell
    IMPORTING
      e_row = lv_row.
  READ TABLE <new_tab> INTO <status> INDEX lv_row.

  CONCATENATE 'SNDPRN' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_sndprn>.
  lv_sndprn = <fs_sndprn>.

  CONCATENATE 'MESTYP' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_mestyp>.
  lv_mestyp = <fs_mestyp>.

  r_status-sign = 'I'.
  r_status-option = 'EQ'.
  r_status-low = '30'. APPEND r_status.
  r_status-low = '02'. APPEND r_status.
  r_status-low = '04'. APPEND r_status.
  r_status-low = '05'. APPEND r_status.
  r_status-low = '25'. APPEND r_status.
  r_status-low = '29'. APPEND r_status.
  r_status-low = '26'. APPEND r_status.
  r_status-low = '32'. APPEND r_status.
  r_status-low = '51'. APPEND r_status.
  r_status-low = '56'. APPEND r_status.
  r_status-low = '61'. APPEND r_status.
  r_status-low = '63'. APPEND r_status.
  r_status-low = '65'. APPEND r_status.
  r_status-low = '60'. APPEND r_status.
  r_status-low = '64'. APPEND r_status.
  r_status-low = '66'. APPEND r_status.
  r_status-low = '69'. APPEND r_status.


  SELECT docnum INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM edidc
  WHERE "CREDAT IN S_DATUM
        upddat IN s_datum
    AND updtim IN s_uptim
    AND mestyp EQ lv_mestyp
    AND sndprn EQ lv_sndprn
    AND status IN r_status
    AND rcvprt EQ 'LS'
      AND direct IN s_dirct. "inbound/outbound


  LOOP AT gt_data.
    seltab-selname  = 'P_IDOC'.
    seltab-sign     = 'I'.
    seltab-option   = 'EQ'.
    seltab-low      =  gt_data-docnum.

    APPEND seltab TO gt_seltab.
  ENDLOOP.

  IF NOT gt_seltab[] IS INITIAL.
    SUBMIT rbdprocess WITH SELECTION-TABLE gt_seltab
                        AND RETURN.
  ELSE.
*    MESSAGE-ID a891.
  ENDIF.
*
ENDFORM.                    " P3000_CALL_PBDPROC
*&---------------------------------------------------------------------*
*&      Form  P3100_CALL_RBDMON00
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3100_call_rbdmon00.

  DATA: lv_row TYPE i, f_txt(100),
        lv_sndprn LIKE edidc-sndprn,
        lv_mestyp LIKE edidc-mestyp,
        cnt TYPE i, answer.
  CLEAR :  <status>.
  FIELD-SYMBOLS : <fs_sndprn> TYPE ANY,
              <fs_mestyp> TYPE ANY.

  CALL METHOD g_grid->get_current_cell
    IMPORTING
      e_row = lv_row.
  READ TABLE <new_tab> INTO <status> INDEX lv_row.

  CONCATENATE 'SNDPRN' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_sndprn>.
  lv_sndprn = <fs_sndprn>.

  CONCATENATE 'MESTYP' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_mestyp>.
  lv_mestyp = <fs_mestyp>.

  SELECT docnum INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM edidc
  WHERE "CREDAT IN S_DATUM
        upddat IN s_datum
      AND updtim IN s_uptim
      AND mestyp EQ lv_mestyp
      AND sndprn EQ lv_sndprn
      AND rcvprt EQ 'LS'
      AND direct IN s_dirct. "inbound/outbound

*    SELECT DOCNUM STATUS MESTYP CREDAT CRETIM SNDPRN RCVPRN
*                     IDOCTP CIMTYP MAXSEGNUM UPDDAT UPDTIM
*                     MESCOD MESFCT SNDPRT SNDPFC RCVPRT RCVPFC TEST
*              INTO TABLE GT_DATA FROM EDIDC
*              WHERE  CREDAT IN S_DATUM
*              AND MESTYP EQ LV_MESTYP
*              AND SNDPRN EQ LV_SNDPRN
*              AND  DIRECT = '2' .

  DESCRIBE TABLE gt_data LINES cnt .


  IF cnt > max_display.
*    ATT_TEXT = 'Wollen Sie wirklich & IDocs anzeigen?'(020).
*    NUM_TEXT = ISTAT-ICOUNT.
*    REPLACE '&' WITH NUM_TEXT INTO ATT_TEXT.
*    CONDENSE ATT_TEXT.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
              text_question = 'Wollen Sie wirklich & IDocs anzeigen?'
           default_button        = '2'
              display_cancel_button = ''
         IMPORTING
              answer                = answer.
    IF answer = '2' OR answer = 'A'.
      sy-subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.

  DATA : aud_line LIKE LINE OF aud_idoc_tab.
  LOOP AT gt_data.
    aud_line-docnum = gt_data-docnum.
    aud_line-status = gt_data-status.
    aud_line-mestyp = gt_data-mestyp.
    aud_line-credat = gt_data-credat.
    aud_line-cretim = gt_data-cretim.
    aud_line-partner = gt_data-sndprn.
    aud_line-idoctp = gt_data-idoctp.
    aud_line-maxsegnum = gt_data-maxsegnum.
    aud_line-idoctp = gt_data-idoctp.
    aud_line-cimtyp = gt_data-cimtyp.

    APPEND aud_line TO aud_idoc_tab.
  ENDLOOP.

ENDFORM.                    " P3100_CALL_RBDMON00

*&---------------------------------------------------------------------*
*&      Form  SHOW_IDOC_DATA
*&---------------------------------------------------------------------*
*  Achtung: Hier noch keine Mehrsystemfähigkeit!
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_idoc_data USING detail TYPE c.
  DATA ai TYPE bdmod1tp.
  DATA: e_row TYPE i, e_col TYPE i.
  DATA: es_row_id TYPE lvc_s_row, es_col_id TYPE lvc_s_col.
  DATA: logsys TYPE logsys, docnum TYPE edi_docnum.
  DATA: d_obj TYPE borident.

  CALL METHOD g_grid1->get_current_cell
    IMPORTING e_row = e_row
              e_col = e_col
              es_row_id = es_row_id
              es_col_id = es_col_id.

  CALL METHOD cl_gui_cfw=>flush.

  IF ( es_col_id-fieldname <> 'DOCNUM'
       AND es_col_id-fieldname <> 'STATXT'
       AND es_col_id-fieldname <> 'DOCNUM_PR' ) OR e_row = 0.
    MESSAGE w897(b1).
* Es ist kein IDoc markiert.
    EXIT.
  ENDIF.

  READ TABLE aud_idoc_tab INTO ai INDEX e_row.
  CHECK sy-subrc = 0.

  IF es_col_id-fieldname = 'DOCNUM'.
    docnum = ai-docnum.
  ELSEIF es_col_id-fieldname = 'DOCNUM_PR'.
    logsys = ai-partner.
    IF ai-docnum_pr IS INITIAL OR ai-docnum_pr = '?'.
      MESSAGE w897(b1).
* Es ist kein IDoc markiert.
      EXIT.
    ENDIF.
    docnum = ai-docnum_pr.
  ENDIF.


  CASE detail.
    WHEN 'E'.
      docnum = ai-docnum.
      PERFORM show_err_long_text USING docnum.
    WHEN 'R'.                          " Show object Relations
      d_obj-objkey = docnum.
      d_obj-objtype = 'IDOC'.
      d_obj-logsys = logsys.


      CALL FUNCTION 'SREL_DISPLAY_LIST_OF_NEIGHBORS'
           EXPORTING
                object         = d_obj
*         ROLETYPE       =
*         I_MESSAGE      = 'X'
           EXCEPTIONS
                no_logsys      = 1
                internal_error = 2
                OTHERS         = 3
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    WHEN 'I'.                          " Show IDoc
      PERFORM display_single_idoc USING docnum logsys.
  ENDCASE.

  " SHOW_LINKAGE
ENDFORM.                    " SHOW_IDOC_DATA

*---------------------------------------------------------------------*
*       FORM display_single_idoc                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  docnum                                                        *
*  -->  logsys                                                        *
*---------------------------------------------------------------------*
FORM display_single_idoc USING docnum TYPE edi_docnum
                               logsys TYPE logsys.

  DATA: rfc_destination TYPE rfcdes-rfcdest.
  DATA  e_msg(50) TYPE c.

*  IF LOGSYS IS INITIAL OR LOGSYS = BD_LS_MON=>OWN_LOGICAL_SYSTEM.
**    CALL FUNCTION 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
*    CALL FUNCTION 'BAPI_IDOCAPPL_DISPLAY'
*         EXPORTING
*              IDOCNUMBER            = DOCNUM
*         EXCEPTIONS
*              SYSTEM_FAILURE        = 01
*              COMMUNICATION_FAILURE = 02. "#EC *
*    EXIT.
*  ENDIF.

* RFC-Destination bestimmen
  CALL FUNCTION 'OBJ_METHOD_GET_RFC_DESTINATION'
       EXPORTING
            object_type                   = 'IDOCALEAUD'
            method                        = 'DISPLAY'
            logical_system                = logsys
       IMPORTING
            rfc_destination               = rfc_destination
       EXCEPTIONS
            no_rfc_destination_maintained = 1
            error_reading_method_props    = 2
            OTHERS                        = 3.

  IF sy-subrc <> 0.
*Für das logische System & konnte keine RFC-Destination ermittelt werden
    MESSAGE s555(b1) WITH logsys 'IDOCALEAUD' 'DISPLAY'.
    EXIT.
  ENDIF.

* Check wether system is type R/3
  SELECT COUNT( * ) FROM rfcdes
    WHERE rfcdest = rfc_destination
      AND rfctype = '3'.
  IF sy-subrc <> 0.
    MESSAGE s086(b1) WITH rfc_destination.
    EXIT.
  ENDIF.

* RFC-Berechtigung prüfen
  CALL FUNCTION 'AUTHORITY_CHECK_RFC'
       EXPORTING
            userid           = sy-uname
       EXCEPTIONS
            user_dont_exist  = 1
            rfc_no_authority = 2.
  IF sy-subrc <> 0.
* Keine Berechtigung zur Ausführung der Aktion
    MESSAGE s227(b1) WITH '' 'S_RFC' ''.
    EXIT.
* Fehlerhandling
  ENDIF.

* show remote idoc

  CALL FUNCTION 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
     DESTINATION rfc_destination
       EXPORTING
            docnum  = docnum
         EXCEPTIONS
              system_failure = 1 MESSAGE e_msg
              communication_failure = 2 MESSAGE e_msg
              OTHERS = 3.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1 OR 2.
      MESSAGE e778(b1)
        WITH 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
             rfc_destination '' e_msg.
    WHEN 3.
      MESSAGE e778(b1)
        WITH 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
             rfc_destination 'EXCEPTION RAISED'.            "#EC *
  ENDCASE.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM show_err_long_text                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DOCNUM                                                        *
*---------------------------------------------------------------------*
FORM show_err_long_text
  USING docnum TYPE edi_docnum.

  DATA info_langtext TYPE help_info.
  DATA: BEGIN OF int_dselc OCCURS 0.
          INCLUDE STRUCTURE dselc.
  DATA: END OF int_dselc.

  DATA: BEGIN OF int_dval OCCURS 0.
          INCLUDE STRUCTURE dval.
  DATA: END OF int_dval.

* Workaround, runs only local!
  DATA edids_tab TYPE TABLE OF edids.
  DATA edids TYPE edids.
  DATA: buffer LIKE help_info-message.

  SELECT * INTO TABLE edids_tab FROM edids
       WHERE docnum = docnum.
  CHECK sy-subrc = 0.

  SORT edids_tab BY countr DESCENDING.
  READ TABLE edids_tab INTO edids INDEX 1.

  info_langtext-call = 'D'.
  info_langtext-object = 'N'.
  info_langtext-spras = sy-langu.
  info_langtext-messageid = edids-stamid. "ii-stamid.
  info_langtext-messagenr = edids-stamno. "ii-stamno.
  info_langtext-msgv1     = edids-stapa1.                   "ii-stapa1.
  info_langtext-msgv2     = edids-stapa2.                   "ii-stapa2.
  info_langtext-msgv3     = edids-stapa3.                   "ii-stapa3.
  info_langtext-msgv4     = edids-stapa4.                   "ii-stapa4.
  info_langtext-program   = 'RBDMON00'.
  info_langtext-dynpro    = sy-dynnr.
  info_langtext-docuid    = 'NA'.
  info_langtext-cucol     = 9.
  info_langtext-curow     = 2.
*  info_langtext-message   = edids-statxt. "ii-statxt.
  CALL FUNCTION 'FORMAT_MESSAGE'
       EXPORTING
            id        = info_langtext-messageid
            lang      = '-D'
            no        = info_langtext-messagenr
            v1        = info_langtext-msgv1
            v2        = info_langtext-msgv2
            v3        = info_langtext-msgv3
            v4        = info_langtext-msgv4
       IMPORTING
            msg       = buffer
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  info_langtext-message   = buffer.
  info_langtext-title     = 'SAP R/3'.
  CALL FUNCTION 'HELP_START'
       EXPORTING
            help_infos   = info_langtext
       TABLES
            dynpselect   = int_dselc
            dynpvaluetab = int_dval
       EXCEPTIONS
            OTHERS       = 1.

ENDFORM.                    " SHOW_ERR_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  GET_IDOC_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_idoc_objects.
  DATA obj TYPE borident.
  DATA ai TYPE bdmod1tp.
  DATA: nb_tab TYPE TABLE OF neighbor, nb TYPE neighbor.
  DATA objtyp TYPE swo_objtyp.
  DATA ai_status TYPE i.

  LOOP AT aud_idoc_tab INTO ai.
    obj-objkey  = ai-docnum.
    obj-objtype = 'IDOC'.

    CLEAR objtyp.
    IF NOT ai-objname IS INITIAL.
      CALL METHOD own_sys->objname2obj
        EXPORTING
          objname    = ai-objname
        IMPORTING
          objtype    = objtyp.
    ENDIF.

    REFRESH nb_tab.
    CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
      EXPORTING
        object               = obj
*   ROLETYPE             =
*   RELATIONTYPE         =
*   MAX_HOPS             = 1
      TABLES
        neighbors            = nb_tab
      EXCEPTIONS
        internal_error       = 1
        no_logsys            = 2
        OTHERS               = 3.
    CHECK sy-subrc = 0.

    IF NOT objtyp IS INITIAL.
      READ TABLE nb_tab INTO nb
        WITH KEY objtype = objtyp.
      IF sy-subrc = 0.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    ai_status = ai-status.
    IF ai-objkey IS INITIAL AND ai_status < 50.
      READ TABLE nb_tab INTO nb
        WITH KEY roletype = 'OUTBELEG'.
      IF sy-subrc = 0.
        CLEAR ai-methodname.
        CALL METHOD own_sys->obj2objname
          EXPORTING
            objtype    = nb-objtype
          IMPORTING
            objname    = ai-objname.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    ai_status = ai-status.
    IF ai-objkey IS INITIAL AND ai_status >= 50.
      READ TABLE nb_tab INTO nb
        WITH KEY roletype = 'INBELEG'.
      IF sy-subrc = 0.
        CLEAR ai-methodname.
        CALL METHOD own_sys->obj2objname
          EXPORTING
            objtype    = nb-objtype
          IMPORTING
            objname    = ai-objname.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    IF ai-objname IS INITIAL.
      ai-objname = '-'.
    ENDIF.
    IF ai-objkey IS INITIAL.
      ai-objkey = '-'.
    ENDIF.
    MODIFY aud_idoc_tab FROM ai.
  ENDLOOP.

  PERFORM p1100_create_object.

ENDFORM.                    " GET_IDOC_OBJECTS
