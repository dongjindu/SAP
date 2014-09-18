REPORT zrmm_idoc_list MESSAGE-ID e0.
*&--------------------------------------------------------------------&*
*&  Program id   : ZRMM_IDOC_LIST.
*&  Developer    : Furong
*&  Description  : Idoc list
*&
*&--------------------------------------------------------------------&*
*& Date        Transport        Description
*& 03/2005                      initial program.
*
*&--------------------------------------------------------------------&*
INCLUDE <icon>.
*INCLUDE bdcrecxy .
INCLUDE zbdcrecx .
CLASS lcl_event_receiver DEFINITION DEFERRED.
TABLES: ekko.
DATA: BEGIN OF it_outtab0 OCCURS 0,
        docnum LIKE edidc-docnum,
        credat LIKE edidc-credat,
        cretim LIKE edidc-cretim,
        rcvprn LIKE edidc-rcvprn,
        sndprn LIKE edidc-sndprn,
*        direct like edidc-direct,
        status LIKE edidc-status,
        mestyp LIKE edidc-mestyp,
        matnr LIKE mara-matnr,
        lfimg  LIKE mbew-lbkum,
        ebeln LIKE ekko-ebeln,
        deldate LIKE ekpo-aedat,
        bolnr LIKE likp-bolnr,
        lifex LIKE likp-lifex,
        case LIKE mara-matnr,
        ebelp LIKE ekpo-ebelp,
        vendor LIKE ekko-lifnr,
        movetype LIKE mseg-bwart,
        netwr LIKE ekpo-netpr,
        lgort LIKE mard-lgort,
        invno LIKE ekko-ebeln,
        sano LIKE ekko-ebeln,
        rcno LIKE ekko-ebeln,
        asno LIKE ekko-ebeln,
        krate LIKE ekpo-netpr,
        currency LIKE ekko-waers,
        per LIKE ekpo-peinh,
        paydate LIKE edidc-credat,
        invdate LIKE edidc-credat,
        werks LIKE marc-werks,
        edatu LIKE ekpo-aedat,
        edatb LIKE ekpo-aedat,
        ezeit LIKE eket-uzeit,
        abnrd LIKE ekpo-aedat,
        statxt LIKE edids-statxt,
        light TYPE c,
        aletxt(40),
*-<     for DESADV display 04.03.2014 Victor
        refint LIKE edidc-refint,
        atdat  LIKE zmmt0102-atdat,
        attim  LIKE zmmt0102-attim,
        ifp03  LIKE zmmt0102-ifp03,
*->
        ct TYPE lvc_t_scol,
      END OF it_outtab0.

DATA: BEGIN OF wa_mara OCCURS 0,
        docnum LIKE edidc-docnum,
        matnr  LIKE mara-matnr,
        lfimg  LIKE mbew-lbkum,
        ebeln LIKE ekko-ebeln,
        deldate LIKE ekpo-aedat,
        bolnr LIKE likp-bolnr,
        lifex LIKE likp-lifex,
        case LIKE mara-matnr,
        abnrd LIKE ekpo-aedat,
        ebelp LIKE ekpo-ebelp,
        vendor LIKE ekko-lifnr,
        movetype LIKE mseg-bwart,
        netwr LIKE ekpo-netpr,
        lgort LIKE mard-lgort,
        invno LIKE ekko-ebeln,
        sano LIKE ekko-ebeln,
        rcno LIKE ekko-ebeln,
        asno LIKE ekko-ebeln,
        krate LIKE ekpo-netpr,
        currency LIKE ekko-waers,
        per LIKE ekpo-peinh,
        paydate LIKE edidc-credat,
        invdate LIKE edidc-credat,
        werks LIKE marc-werks,
        edatu LIKE ekpo-aedat,
        edatb LIKE ekpo-aedat,
        ezeit LIKE eket-uzeit,
        aletxt(40),
      END OF wa_mara.

DATA: it_mara LIKE wa_mara OCCURS 0 WITH HEADER LINE.

DATA: it_edidc TYPE edidc OCCURS 0 WITH HEADER LINE,
      it_edidd TYPE edid4 OCCURS 0 WITH HEADER LINE,
      it_edids TYPE edids OCCURS 0 WITH HEADER LINE,
      it_stadetail TYPE edids OCCURS 0 WITH HEADER LINE,
      it_teds2 TYPE teds2 OCCURS 0 WITH HEADER LINE.

DATA: wa_outtab LIKE it_outtab0 OCCURS 0 WITH HEADER LINE.
DATA: it_outtab LIKE it_outtab0 OCCURS 0 WITH HEADER LINE.
*- 04.03.2014 Victor
DATA: it_zmmt0102 LIKE zmmt0102 OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_idoc_no OCCURS 0,
       ifp01 LIKE zmmt0102-ifp01,  "IDoc No.
      END OF it_idoc_no.


DATA: BEGIN OF it_seg_v OCCURS 100.
        INCLUDE STRUCTURE int_seg.
DATA: END OF it_seg_v.

DATA: BEGIN OF segmtab OCCURS 50.
        INCLUDE STRUCTURE dntab.
DATA: END OF segmtab.

DATA: time_0 TYPE time VALUE '000000',
      time_24 TYPE time VALUE '240000'.

DATA: offset TYPE p VALUE 0,
      segnam LIKE dntab-tabname,
      w_mat_flag(1),
      w_po_flag(1),
      w_qty_flag(1),
      w_statxt LIKE t100-text,
*      w_direct like it_outtab-direct,
      w_matnr LIKE wa_outtab-matnr,
      w_chdate LIKE it_edids-logdat,
      w_chtime LIKE it_edids-logtim,
      qualf(3),
      tdid(3),
      iddat(3),
      string(255).

DATA: w_msgid LIKE t100-arbgb,
      w_msgno LIKE t100-msgnr.

DATA: w_lines TYPE i.

DATA: BEGIN OF it_stacust OCCURS 0,
        status LIKE stacust-status,
        statva LIKE stacust-statva,
      END OF it_stacust.

DATA: BEGIN OF it_stalight OCCURS 0,
        statva LIKE stalight-statva,
        stalight LIKE stalight-stalight,
      END OF it_stalight.

DATA: ok_code LIKE sy-ucomm,
      g_container TYPE scrfname VALUE 'CC_CONTAINER',
      g_dailog TYPE scrfname VALUE 'CC_DAILOG',
      grid1  TYPE REF TO cl_gui_alv_grid,
      grid2  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_dailog_container TYPE REF TO cl_gui_custom_container.

DATA: event_receiver TYPE REF TO lcl_event_receiver.

DATA: it_fieldcat1 TYPE lvc_t_fcat WITH HEADER LINE,
      it_fieldcat LIKE TABLE OF it_fieldcat1,
      it_fieldcat2 TYPE lvc_t_fcat WITH HEADER LINE,
      is_layout TYPE lvc_s_layo,
      it_sort   TYPE lvc_t_sort WITH HEADER LINE,
      is_layout2 TYPE lvc_s_layo,
      it_sort2   TYPE lvc_t_sort WITH HEADER LINE.

DATA: g_repid LIKE sy-repid,
      x_save,                     "for Parameter I_SAVE
      gs_variant TYPE disvariant. "for parameter IS_VARIANT

DATA: lt_rows TYPE lvc_t_row.

DATA: wa_lt_row LIKE LINE OF lt_rows.

DATA: it_color TYPE lvc_t_scol,
      wa_color LIKE LINE OF it_color.


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class lcl_event_receiver: local class to
*                         define and handle own functions.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

  PRIVATE SECTION.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class lcl_event_receiver (Implementation)

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'DETAILS' TO ls_toolbar-function.
    MOVE icon_detail TO ls_toolbar-icon.
    MOVE 'Details of IDoc Status' TO ls_toolbar-quickinfo.
    MOVE ' Status ' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'WE02' TO ls_toolbar-function.
*    MOVE icon_detail TO ls_toolbar-icon.
    MOVE 'SAP IDoc List' TO ls_toolbar-quickinfo.
    MOVE 'WE02' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "HANDLE_TOOLBAR
*-------------------------------------------------------------------
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'DETAILS'.
        PERFORM process_details.
      WHEN 'WE02'.
        PERFORM process_we02.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
*-----------------------------------------------------------------
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*
* lcl_event_receiver (Implementation)
*===================================================================

FIELD-SYMBOLS: <feld>.

*G_REPID = SY-REPID.
*GS_variant-REPORT = G_REPID.

SELECTION-SCREEN BEGIN OF BLOCK controlrecord_input
                          WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_cretim  FOR it_outtab-cretim DEFAULT time_0 TO time_24
,
                s_credat  FOR it_outtab-credat DEFAULT sy-datum TO
sy-datum.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_docnum  FOR it_outtab-docnum,
                s_status  FOR it_outtab-status.
SELECTION-SCREEN SKIP.
PARAMETERS : p_mestyp LIKE it_outtab-mestyp DEFAULT 'DESADV' OBLIGATORY.
SELECTION-SCREEN SKIP.

*PARAMETERS : p_in like it_outtab-direct radiobutton group grp1,
*             p_out like it_outtab-direct radiobutton group grp1.
*SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK controlrecord_input.
SELECTION-SCREEN BEGIN OF BLOCK datarecord_input
                          WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_matnr FOR it_outtab-matnr NO-EXTENSION.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK datarecord_input.

START-OF-SELECTION.

  PERFORM read-controlrecord.
  DESCRIBE TABLE it_edidc LINES w_lines.
  IF w_lines = 0.
    MESSAGE i892.
    EXIT.
  ENDIF.

  PERFORM read-statusrecord.

  PERFORM process_datarecord.

  SORT it_outtab BY matnr edatu.

  CALL SCREEN 100.



************************************
** FROM                           **
************************************
FORM read-controlrecord.
  CLEAR : it_idoc_no[], it_idoc_no.

  IF p_mestyp ='DESADV'.
    SELECT * FROM edidc INTO TABLE it_edidc
          WHERE   cretim IN s_cretim
          AND     credat IN s_credat
*       AND     direct = w_direct
          AND     docnum IN s_docnum
          AND     status IN s_status
          AND     mestyp = p_mestyp
          AND     sndprn IN s_lifnr.
*-< Victor added for layout change 04.03.2014
    IF it_edidc[] IS NOT INITIAL.
      LOOP AT it_edidc.
        it_idoc_no-ifp01  = it_edidc-docnum.
        APPEND it_idoc_no. CLEAR it_idoc_no.
      ENDLOOP.

      SELECT * INTO TABLE it_zmmt0102
      FROM zmmt0102
        FOR ALL ENTRIES IN it_idoc_no
      WHERE ifkey = 'MMIF302_ECC_OB'
        AND ifp01 = it_idoc_no-ifp01.

*-    MMIF301_ECC_OB : Idoc No->ifp02, Invoice No-> ifp06
      SELECT ifkey ifp02 AS ifp01 ifp06 AS ifp03 atdat attim
              APPENDING CORRESPONDING FIELDS OF TABLE it_zmmt0102
      FROM zmmt0102
        FOR ALL ENTRIES IN it_idoc_no
      WHERE ifkey = 'MMIF301_ECC_OB'
        AND ifp02 = it_idoc_no-ifp01.

    ENDIF.
    SORT it_zmmt0102 BY ifp01.
*->

  ELSE.
    SELECT * FROM edidc INTO TABLE it_edidc
          WHERE   cretim IN s_cretim
          AND     credat IN s_credat
*       AND     direct = w_direct
          AND     docnum IN s_docnum
          AND     status IN s_status
          AND     mestyp = p_mestyp
          AND     rcvprn IN s_lifnr.
  ENDIF.
ENDFORM.                    "READ-CONTROLRECORD

*---------------------------------------------------------------------*
*       FORM read-statusrecord                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read-statusrecord.
  SELECT * FROM stacust INTO CORRESPONDING FIELDS OF TABLE it_stacust.
  SELECT * FROM stalight INTO CORRESPONDING FIELDS OF TABLE it_stalight.
  SELECT * INTO TABLE it_edids FROM edids
               FOR ALL ENTRIES IN it_edidc
               WHERE docnum = it_edidc-docnum
                 AND status = it_edidc-status.

  SORT it_edids BY docnum status countr DESCENDING.
  IF it_teds2[] IS INITIAL.
    SELECT * FROM teds2 INTO TABLE it_teds2 WHERE langua = 'E'.
  ENDIF.
  LOOP AT it_edidc.
    MOVE-CORRESPONDING it_edidc TO wa_outtab.

    READ TABLE it_stacust WITH KEY status = it_edidc-status.
    READ TABLE it_stalight WITH KEY statva = it_stacust-statva.
*    1 Red
*    2 Yellow
*    3 Green
    CASE it_stalight-stalight.
      WHEN 1.
        wa_outtab-light = '2'.
      WHEN 2.
        wa_outtab-light = '3'.
      WHEN 3.
        wa_outtab-light = '1'.
    ENDCASE.
    CASE it_edids-statyp.
      WHEN 'E' OR 'W'.
        PERFORM build_color_red USING wa_outtab-statxt 'STATXT'.
        wa_outtab-ct = it_color.
        CLEAR it_color.
      WHEN OTHERS.
*        PERFORM BUILD_COLOR_WARNING using wa_outtab-statxt 'STATXT'.
*        wa_outtab-ct = it_color.
*        clear it_color.
    ENDCASE.

    READ TABLE it_teds2 WITH KEY status = it_edidc-status langua = 'E'.
    IF sy-subrc = 0.
      wa_outtab-statxt = it_teds2-descrp.
    ELSE.
      wa_outtab-statxt = 'No data in TEDS2 Table'.
    ENDIF.


*-<
    IF p_mestyp ='DESADV'.
      CLEAR : it_zmmt0102.
      READ TABLE it_zmmt0102 WITH KEY ifp01 = it_edidc-docnum
                                         BINARY SEARCH.
      IF sy-subrc = 0.
        wa_outtab-atdat = it_zmmt0102-atdat.
        wa_outtab-attim = it_zmmt0102-attim.
        wa_outtab-ifp03 = it_zmmt0102-ifp03.
      ENDIF.
    ENDIF.

*->
*    read table it_edids with key docnum = it_edidc-docnum
*                         status = it_edidc-status.
*    w_chdate = it_edids-logdat.
*    w_chtime = it_edids-logtim.
*    if sy-subrc = 0.

*       w_msgid = it_edids-stamid.
*       w_msgno = it_edids-stamno.
*       clear w_statxt.
*       perform read_message using w_msgid
*                                  w_msgno
*                                  it_edids-stapa1
*                                  it_edids-stapa2
*                                  it_edids-stapa3
*                                  it_edids-stapa4
*                         changing w_statxt.
*       wa_outtab-statxt = w_statxt.
    APPEND wa_outtab.
    CLEAR wa_outtab.
  ENDLOOP.
ENDFORM.                    "READ-STATUSRECORD

*---------------------------------------------------------------------*
*       FORM read-datarecord                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read-datarecord.
  CALL FUNCTION 'IDOC_READ_COMPLETELY'
       EXPORTING
            document_number          = wa_outtab-docnum
*    IMPORTING
*         IDOC_CONTROL             =
*         NUMBER_OF_DATA_RECORDS   =
*         NUMBER_OF_STATUS_RECORDS =
      TABLES
*          int_edids                =
           int_edidd                = it_edidd
       EXCEPTIONS
            document_not_exist       = 1
            document_number_invalid  = 2
            OTHERS                   = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
ENDFORM.                    "READ-DATARECORD

*---------------------------------------------------------------------*
*       FORM read-segmentfield                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read-segmentfield.
  segnam = it_edidd-segnam.
  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
      tabname = segnam
    TABLES
      nametab = segmtab
    EXCEPTIONS
      OTHERS  = 0.
  LOOP AT segmtab.
    offset = segmtab-offset.
    ASSIGN it_edidd-sdata+offset(segmtab-ddlen) TO <feld>
                           TYPE segmtab-inttype.
    string = <feld>.
    CHECK NOT string IS INITIAL.
    MOVE segmtab-fieldname TO it_seg_v-fieldname.
    MOVE string TO it_seg_v-string.
    APPEND it_seg_v.
    CASE wa_outtab-mestyp.
      WHEN 'DESADV'.
        CASE segmtab-fieldname.
          WHEN 'KDMAT'.
            wa_mara-docnum = wa_outtab-docnum.
            wa_mara-matnr = string.
            w_mat_flag = 'Y'.
          WHEN 'LFIMG'.
            wa_mara-lfimg = string.
          WHEN 'BSTNR'.
            wa_mara-ebeln = string.
            w_qty_flag = 'Y'.
          WHEN 'BOLNR'.
            wa_mara-bolnr = string.    " asn no
            w_po_flag = 'Y'.
          WHEN 'LIFEX'.
            wa_mara-lifex = string.
          WHEN 'MATNR'.
            wa_mara-case = string.
          WHEN 'NTANF'.
            wa_mara-deldate = string.
        ENDCASE.
** Added on 08/13/07
      WHEN 'ALEAUD'.
        CASE segmtab-fieldname.
          WHEN 'LOGSYS'.
            wa_mara-vendor = string.
            w_po_flag = 'Y'.
          WHEN 'DOCNUM'.
            wa_mara-matnr = string.
            wa_mara-docnum = wa_outtab-docnum.
            w_mat_flag = 'Y'.
          WHEN 'STAPA4_LNG'.
            wa_mara-asno = string.
          WHEN 'STAPA3'.
            wa_mara-case = string.
          WHEN 'STAPA2_LNG'.
            wa_mara-edatb = string.
          WHEN 'STAPA3_LNG'.
            wa_mara-ezeit = string.
          WHEN 'STAMID'.
            wa_mara-invno = string.
          WHEN 'STAMNO'.
            wa_mara-sano = string.
          WHEN 'STATXT'.
            wa_mara-aletxt = string.
        ENDCASE.
** end of change
      WHEN 'ORDERS' OR 'ORDCHG'.
        CASE segmtab-fieldname.
          WHEN 'BELNR'.
            wa_mara-ebeln = string.
            w_po_flag = 'Y'.
          WHEN 'POSEX'.
            wa_mara-ebelp = string.
          WHEN 'MENGE'.
            wa_mara-lfimg = string.
            w_qty_flag = 'Y'.
          WHEN 'WERKS'.
            wa_mara-werks = string.
          WHEN 'EDATU'.
            wa_mara-edatu = string.
          WHEN 'IDTNR'.
            wa_mara-docnum = wa_outtab-docnum.
            wa_mara-matnr = string.
            w_mat_flag = 'Y'.
        ENDCASE.
      WHEN 'DELJIT' OR 'DELFOR'.
        CASE segmtab-fieldname.
          WHEN 'VTRNR'.
            wa_mara-ebeln = string.
            w_po_flag = 'Y'.
          WHEN 'IDNKD'.
            wa_mara-docnum = wa_outtab-docnum.
            wa_mara-matnr = string.
            w_mat_flag = 'Y'.
          WHEN 'ABNRD'.
            wa_mara-abnrd = string.
          WHEN 'POSEX'.
            wa_mara-ebelp = string.
          WHEN 'KWERK'.
            wa_mara-werks = string.
          WHEN 'EDATUV'.
            wa_mara-edatu = string.
          WHEN 'EDATUB'.
            wa_mara-edatb = string.
          WHEN 'WMENG'.
            wa_mara-lfimg = string.
            w_qty_flag = 'Y'.
          WHEN 'EZEIT'.
            wa_mara-ezeit = string.
        ENDCASE.

      WHEN 'GSVERF'.
        CASE segmtab-fieldname.
          WHEN 'BELNR'.
            CASE segnam.
              WHEN 'E1EDK01'.
                wa_mara-docnum = wa_outtab-docnum.
                wa_mara-invno = string.
                w_po_flag = 'Y'.
              WHEN 'E1EDP02'.
                IF qualf = '001'.
                  wa_mara-sano = string.
                  CLEAR qualf.
                ELSE.
                  IF qualf = '010'.
                    wa_mara-rcno = string.
                    CLEAR qualf.
                  ELSE.
                    wa_mara-asno = string.
                    CLEAR qualf.
                  ENDIF.
                ENDIF.
            ENDCASE.
          WHEN 'QUALF'.
            qualf = string.
          WHEN 'TDID'.
            tdid = string.
          WHEN 'IDDAT'.
            iddat = string.
          WHEN 'RECIPNT_NO'.
            wa_mara-vendor = string.
          WHEN 'TDLINE'.
            IF tdid = '007'.
              wa_mara-movetype = string.
            ENDIF.
          WHEN 'DATUM'.
            IF iddat = '015'.
              wa_mara-invdate = string.
            ELSE.
              IF iddat = '044'.
                wa_mara-paydate = string.
              ENDIF.
            ENDIF.
          WHEN 'LGORT'.
            wa_mara-lgort = string.
          WHEN 'NETWR'.
            wa_mara-netwr = string.
          WHEN 'KRATE'.
            wa_mara-krate = string.
          WHEN 'PEINH'.
            wa_mara-per = string.
          WHEN 'CURCY'.
            wa_mara-currency = string.
          WHEN 'POSEX'.
            wa_mara-ebelp = string.
          WHEN 'MENGE'.
            wa_mara-lfimg = string.
            w_qty_flag = 'Y'.
          WHEN 'WERKS'.
            wa_mara-werks = string.
          WHEN 'EDATU'.
            wa_mara-edatu = string.
          WHEN 'IDTNR'.
            wa_mara-matnr = string.
            w_mat_flag = 'Y'.
        ENDCASE.

    ENDCASE.

  ENDLOOP.
*    describe table it_seg_v lines w-lines.
ENDFORM.                    "READ-SEGMENTFIELD

*---------------------------------------------------------------------*
*       FORM process_datarecord                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_datarecord.
  LOOP AT wa_outtab.
    REFRESH it_edidd.
    CLEAR it_edidd.
    REFRESH it_mara.
    CLEAR it_mara.
    PERFORM read-datarecord.
    IF sy-subrc = 0.
      w_po_flag = 'N'.
      w_mat_flag = 'N'.
      w_qty_flag = 'N'.
      CLEAR tdid.
      CLEAR iddat.
      CLEAR wa_mara.
      REFRESH wa_mara.
      LOOP AT it_edidd.
        PERFORM read-segmentfield.
        CASE wa_outtab-mestyp.
          WHEN 'DESADV'.
            IF w_mat_flag = 'Y' AND w_po_flag = 'Y' AND w_qty_flag = 'Y'
 .
              wa_mara-docnum = wa_outtab-docnum.
              APPEND wa_mara TO it_mara.
              CLEAR wa_mara-matnr.
              CLEAR wa_mara-lfimg.
              CLEAR wa_mara-ebeln.
              CLEAR wa_mara-case.
*            clear wa_mara-deldate.
              w_po_flag = 'N'.
              w_mat_flag = 'N'.
              w_qty_flag = 'N'.
            ENDIF.
            AT LAST.
              IF ( w_mat_flag = 'N' AND w_po_flag = 'Y' ) OR
                   it_mara[] IS INITIAL.
                wa_mara-docnum = wa_outtab-docnum.
                APPEND wa_mara TO it_mara.
                CLEAR wa_mara-matnr.
                CLEAR wa_mara-lfimg.
                CLEAR wa_mara-ebeln.
                CLEAR wa_mara-case.
                w_po_flag = 'N'.
                w_mat_flag = 'N'.
                w_qty_flag = 'N'.
              ENDIF.
            ENDAT.
          WHEN 'ORDERS' OR 'ORDCHG'.
            IF w_mat_flag = 'Y' AND w_po_flag = 'Y' AND w_qty_flag = 'Y'.
              IF wa_mara-lfimg <> 0.
                APPEND wa_mara TO it_mara.
              ENDIF.
              CLEAR wa_mara-matnr.
              CLEAR wa_mara-lfimg.
              CLEAR wa_mara-ebelp.
              CLEAR wa_mara-werks.
              CLEAR wa_mara-edatu.
**            clear wa_mara.
*            w_po_flag = 'N'.
              w_mat_flag = 'N'.
              w_qty_flag = 'N'.
            ENDIF.

          WHEN 'ALEAUD'.
            IF w_mat_flag = 'Y' AND w_po_flag = 'Y'.
              APPEND wa_mara TO it_mara.
              CLEAR wa_mara-matnr.
              w_mat_flag = 'N'.
              w_po_flag = 'N'.
            ENDIF.

          WHEN 'DELJIT' OR 'DELFOR'.
            IF w_mat_flag = 'Y' AND w_po_flag = 'Y' AND w_qty_flag = 'Y'.
              IF wa_mara-lfimg <> 0.
                APPEND wa_mara TO it_mara.
              ENDIF.
              CLEAR wa_mara-lfimg.
              CLEAR wa_mara-edatu.
              CLEAR wa_mara-edatb.
              CLEAR wa_mara-ezeit.
              w_qty_flag = 'N'.
            ENDIF.
          WHEN 'GSVERF'.
            IF w_mat_flag = 'Y' AND w_po_flag = 'Y' AND w_qty_flag = 'Y'.
              IF wa_mara-lfimg <> 0.
                APPEND wa_mara TO it_mara.
              ENDIF.
              CLEAR qualf.
              CLEAR wa_mara-matnr.
              CLEAR wa_mara-lfimg.
              CLEAR wa_mara-ebelp.
              CLEAR wa_mara-werks.
              CLEAR wa_mara-edatu.
              CLEAR wa_mara-netwr.
              CLEAR wa_mara-lgort.
              CLEAR wa_mara-sano.
              CLEAR wa_mara-rcno.
              CLEAR wa_mara-asno.
              CLEAR wa_mara-krate.
*            clear wa_mara_movetype.
              CLEAR wa_mara-per.
              CLEAR wa_mara-currency.
**            clear wa_mara.
*            w_po_flag = 'N'.
              w_mat_flag = 'N'.
              w_qty_flag = 'N'.
            ENDIF.

        ENDCASE.
      ENDLOOP.
      DESCRIBE TABLE it_mara LINES w_lines.
      IF w_lines > 0.
        SORT it_mara BY matnr.
        IF s_matnr-high = ' ' AND s_matnr-low = ' '.
          LOOP AT it_mara.
            MOVE-CORRESPONDING it_mara TO wa_outtab.
            MOVE-CORRESPONDING wa_outtab TO it_outtab.
            IF it_outtab-ezeit IS INITIAL.
              it_outtab-ezeit = '  '.
            ELSE.
              CONCATENATE it_outtab-ezeit '00' INTO it_outtab-ezeit.
            ENDIF.
            APPEND it_outtab.
          ENDLOOP.
        ENDIF.
        IF s_matnr-high = ' ' AND s_matnr-low <> ' '.
          w_matnr = s_matnr-low.
          LOOP AT it_mara WHERE matnr = w_matnr.
            MOVE-CORRESPONDING it_mara TO wa_outtab.
            MOVE-CORRESPONDING wa_outtab TO it_outtab.
            IF it_outtab-ezeit IS INITIAL.
              it_outtab-ezeit = '  '.
            ELSE.
              CONCATENATE it_outtab-ezeit '00' INTO it_outtab-ezeit.
            ENDIF.
            APPEND it_outtab.
          ENDLOOP.
        ENDIF.
        IF s_matnr-high <> ' ' AND s_matnr-low = ' '.
          w_matnr = s_matnr-high.
          LOOP AT it_mara WHERE matnr <= w_matnr.
            MOVE-CORRESPONDING it_mara TO wa_outtab.
            MOVE-CORRESPONDING wa_outtab TO it_outtab.
            IF it_outtab-ezeit IS INITIAL.
              it_outtab-ezeit = '  '.
            ELSE.
              CONCATENATE it_outtab-ezeit '00' INTO it_outtab-ezeit.
            ENDIF.
            APPEND it_outtab.
          ENDLOOP.
        ENDIF.
        IF s_matnr-high <> ' ' AND s_matnr-low <> ' '.
          LOOP AT it_mara WHERE matnr >= s_matnr-low
                            AND matnr <= s_matnr-high.
            MOVE-CORRESPONDING it_mara TO wa_outtab.
            MOVE-CORRESPONDING wa_outtab TO it_outtab.
            IF it_outtab-ezeit IS INITIAL.
              it_outtab-ezeit = '  '.
            ELSE.
              CONCATENATE it_outtab-ezeit '00' INTO it_outtab-ezeit.
            ENDIF.
            APPEND it_outtab.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "PROCESS_DATARECORD

*---------------------------------------------------------------------*
*       FORM read_message                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_MSG_ID                                                      *
*  -->  P_MSG_NO                                                      *
*  -->  P_MSG_VAR1                                                    *
*  -->  P_MSG_VAR2                                                    *
*  -->  P_MSG_VAR3                                                    *
*  -->  P_MSG_VAR4                                                    *
*  -->  P_MSG_TEXT                                                    *
*---------------------------------------------------------------------*
FORM read_message USING  p_msg_id LIKE t100-arbgb
                         p_msg_no LIKE t100-msgnr
                         p_msg_var1
                         p_msg_var2
                         p_msg_var3
                         p_msg_var4
              CHANGING   p_msg_text.

  DATA: msg_text(70) TYPE c.
  DATA: msg_var1 LIKE balm-msgv1.
  DATA: msg_var2 LIKE balm-msgv2.
  DATA: msg_var3 LIKE balm-msgv3.
  DATA: msg_var4 LIKE balm-msgv4.

  msg_var1 = p_msg_var1.
  msg_var2 = p_msg_var2.
  msg_var3 = p_msg_var3.
  msg_var4 = p_msg_var4.

  CONDENSE msg_var1 NO-GAPS.
  CONDENSE msg_var2 NO-GAPS.
  CONDENSE msg_var3 NO-GAPS.
  CONDENSE msg_var4 NO-GAPS.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid                  = p_msg_id
      msgnr                  = p_msg_no
      msgv1                  = msg_var1
      msgv2                  = msg_var2
      msgv3                  = msg_var3
      msg_var4               = msg_var4
    IMPORTING
      message_text_output    = msg_text
    EXCEPTIONS
      function_not_completed = 1
      message_not_found      = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  p_msg_text = msg_text.
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*       EXPORTING
**         PERCENTAGE = 0
*            text       = msg_text.

ENDFORM.                    "READ_MESSAGE


*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'T_100'.
  PERFORM build_fieldcat.
  PERFORM build_sort.
  PERFORM build_layout.
  APPEND LINES OF it_fieldcat1 TO it_fieldcat.
  g_repid = sy-repid.
  gs_variant-report = g_repid.

*  PERFORM build_top_of_page.
*  PERFORM build_sort.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.
    CALL METHOD grid1->set_table_for_first_display
*         EXPORTING I_STRUCTURE_NAME = 'ZMMMATCHECK'
          EXPORTING is_layout = is_layout
                    i_save                   = 'A'
                    is_variant               = gs_variant
          CHANGING it_fieldcatalog = it_fieldcat
                   it_sort   = it_sort[]
                   it_outtab = it_outtab[].


    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_user_command FOR grid1.
    SET HANDLER event_receiver->handle_toolbar FOR grid1.

    CALL METHOD grid1->set_toolbar_interactive.

  ENDIF.
  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = grid1.

ENDMODULE.                    "PBO OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                    "PAI INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code .
    WHEN 'EXIT'.
      CALL METHOD g_custom_container->free.
      IF NOT g_dailog_container IS INITIAL.
        CALL METHOD g_dailog_container->free.
      ENDIF.
      CALL METHOD cl_gui_cfw=>flush.
      IF sy-subrc NE 0.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = g_repid
            txt2  = sy-subrc
            txt1  = 'Error in Flush'(500).
      ENDIF.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT


*---------------------------------------------------------------------*
*       FORM build_fieldcat                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_fieldcat.
  CLEAR : it_fieldcat1.
  REFRESH : it_fieldcat1.
  CHECK it_fieldcat1[] IS INITIAL.

  IF p_mestyp = 'ALEAUD'.
  ELSE.
    PERFORM append_fieldcat USING 'MATNR'
                                   'IT_OUTTAB'
                                   18
                                   'Material Number'
                                   'Material Number'
                                   'Material Number'
                                   'CHAR'
                                   'X'
                                   ''
                                   ''
                                   ''
                                   ''.
  ENDIF.
  PERFORM append_fieldcat USING 'DOCNUM'
                                 'IT_OUTTAB'
                                 16
                                 'Idoc Number'
                                 'Idoc Number'
                                 'Idoc Number'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.

  IF p_mestyp <> 'GSVERF'.

    PERFORM append_fieldcat USING 'CREDAT'
                                  'IT_OUTTAB'
                                  10
                                  'Date Created'
                                  'Date'
                                  'Date'
                                  'DATS'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.

    PERFORM append_fieldcat USING 'CRETIM'
                                  'IT_OUTTAB'
                                  8
                                  'Time Created'
                                  'Time'
                                  'Time'
                                  'TIMS'
                                  ' '
                                  ''
                                  ''
                                  ''
                                  ''.
  ENDIF.

  PERFORM append_fieldcat USING 'STATUS'
                                'IT_OUTTAB'
                                2
                                'Current Status'
                                'Status'
                                'Status'
                                'CHAR'
                                ''
                                ''
                                'X'
                                ''
                                ''.

  PERFORM append_fieldcat USING 'MESTYP'
                                 'IT_OUTTAB'
                                 10
                                 'MESSAGE TYPE'
                                 'MESSAGE TYPE'
                                 'MESSAGE TYPE'
                                 'CHAR'
                                 ''
                                 ''
                                 'X'
                                 ''
                                 ''.
  CASE p_mestyp.
    WHEN 'DESADV'.

      PERFORM append_fieldcat USING 'SNDPRN'
                                    'IT_OUTTAB'
                                    6
                                    'Vendor'
                                    'Vendor'
                                    'Vendor'
                                    'CHAR'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.
      PERFORM append_fieldcat USING 'LFIMG'
                                    'IT_OUTTAB'
                                    13
                                    'Quantity'
                                    'Quantity'
                                    'Quantity'
                                    'QUAN'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.

      PERFORM append_fieldcat USING 'EBELN'
                                     'IT_OUTTAB'
                                     10
                                     'SA Number'
                                     'SA No.'
                                     'SA No.'
                                     'CHAR'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

      PERFORM append_fieldcat USING 'DELDATE'
                                     'IT_OUTTAB'
                                     10
                                     'Delivery Date'
                                     'Del.Date'
                                     'Del Date'
                                     'DATS'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

      PERFORM append_fieldcat USING 'BOLNR'
                                      'IT_OUTTAB'
                                      10
                                      'VND ASN'
                                      'VND ASN'
                                      'VND ASN'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'LIFEX'
                                      'IT_OUTTAB'
                                      10
                                      'Trailer No.'
                                      'Trailer No.'
                                      'Trailer No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'CASE'
                                      'IT_OUTTAB'
                                      15
                                      'Case No.'
                                      'Case No.'
                                      'Case No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

*-< Victor added on 04.03.2014
      PERFORM append_fieldcat USING 'REFINT'
                                      'IT_OUTTAB'
                                      15
                                      'Acknowledgement'
                                      'Acknowledgement'
                                      'Acknowledgement'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'ATDAT'
                                      'IT_OUTTAB'
                                      10
                                      'Date'
                                      'Date'
                                      'Date'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.
      PERFORM append_fieldcat USING 'ATTIM'
                                      'IT_OUTTAB'
                                      8
                                      'Time'
                                      'Time'
                                      'Time'
                                      'TIMS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'IFP03'
                                      'IT_OUTTAB'
                                      10
                                      'Inbound Delivery'
                                      'Inbound Delivery'
                                      'Inbound Delivery'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.
*->

** change

    WHEN 'ALEAUD'.

      PERFORM append_fieldcat USING 'CASE'
                                    'IT_OUTTAB'
                                    6
                                    'Vendor'
                                    'Vendor'
                                    'Vendor'
                                    'CHAR'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.
      PERFORM append_fieldcat USING 'MATNR'
                                    'IT_OUTTAB'
                                    18
                                    'ASN IDOC'
                                    'ASN IDOC'
                                    'ASN IDOC'
                                    'CHAR'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.
      PERFORM append_fieldcat USING 'INVNO '
                                       'IT_OUTTAB'
                                       10
                                       'MSG ID'
                                       'MSG ID'
                                       'MSG ID'
                                       'CHAR'
                                       ''
                                       ''
                                       ''
                                       ''
                                       ''.
      PERFORM append_fieldcat USING 'SANO '
                                        'IT_OUTTAB'
                                        6
                                        'MSG ID'
                                        'MSG ID'
                                        'MSG ID'
                                        'CHAR'
                                        ''
                                        ''
                                        ''
                                        ''
                                        ''.
      PERFORM append_fieldcat USING 'ALETXT'
                                        'IT_OUTTAB'
                                        30
                                        'MSG Text'
                                        'MSG Text'
                                        'MSG Text'
                                        'CHAR'
                                        ''
                                        ''
                                        ''
                                        ''
                                        ''.




      PERFORM append_fieldcat USING 'ASNO '
                                     'IT_OUTTAB'
                                     12
                                     'Vdr ASN'
                                     'Vdr ASN'
                                     'Vdr ASN'
                                     'CHAR'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

      PERFORM append_fieldcat USING 'CASE'
                                     'IT_OUTTAB'
                                     15
                                     'Case'
                                     'Case'
                                     'Case'
                                     'DATS'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

      PERFORM append_fieldcat USING 'EDATB'
                                      'IT_OUTTAB'
                                      10
                                      'Date(ASN)'
                                      'Date(ASN)'
                                      'Date(ASN)'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EZEIT'
                                      'IT_OUTTAB'
                                      10
                                      'Time(ASN)'
                                      'Time(ASN)'
                                      'Time(ASN)'
                                      'TIMS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.


** end of change
    WHEN 'DELJIT'.

      PERFORM append_fieldcat USING 'RCVPRN'
                                      'IT_OUTTAB'
                                      6
                                      'Vendor'
                                      'Vendor'
                                      'Vendor'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EBELN'
                                      'IT_OUTTAB'
                                      10
                                      'PO Number'
                                      'PO No.'
                                      'PO No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'LFIMG'
                                      'IT_OUTTAB'
                                      13
                                      'Quantity'
                                      'Quantity'
                                      'Quantity'
                                      'QUAN'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EDATU'
                                    'IT_OUTTAB'
                                    10
                                    'Delivery Date From'
                                    'Del Date From'
                                    'Del.Date Fr'
                                    'DATS'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.

      PERFORM append_fieldcat USING 'EDATB'
                                      'IT_OUTTAB'
                                      10
                                      'Delivery Date To'
                                      'Del.Date To'
                                      'Del Date To'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EZEIT'
                                      'IT_OUTTAB'
                                      8
                                      'Del. Time'
                                      'Del. Time'
                                      'Del. Time'
                                      'TIMS'
                                      ' '
                                      ''
                                      ''
                                      ''
                                      ''.


      PERFORM append_fieldcat USING 'WERKS'
                                     'IT_OUTTAB'
                                     4
                                     'Plant'
                                     'Plant'
                                     'Plant'
                                     'CHAR'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

    WHEN 'DELFOR'.
      PERFORM append_fieldcat USING 'RCVPRN'
                                      'IT_OUTTAB'
                                      6
                                      'Vendor'
                                      'Vendor'
                                      'Vendor'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EBELN'
                                      'IT_OUTTAB'
                                      10
                                      'PO Number'
                                      'PO No.'
                                      'PO No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'LFIMG'
                                      'IT_OUTTAB'
                                      13
                                      'Quantity'
                                      'Quantity'
                                      'Quantity'
                                      'QUAN'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EDATU'
                                      'IT_OUTTAB'
                                      10
                                      'From Delivery Date'
                                      'Fr Del.Date'
                                      'Fr Del.Date'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EDATB'
                                      'IT_OUTTAB'
                                      10
                                      'To Delivery Date'
                                      'To Del.Date'
                                      'To Del.Date'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'WERKS'
                                      'IT_OUTTAB'
                                      4
                                      'Plant'
                                      'Plant'
                                      'Plant'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

    WHEN 'GSVERF'.

      PERFORM append_fieldcat USING 'VENDOR'
                                    'IT_OUTTAB'
                                    6
                                    'Vendor'
                                    'Vendor'
                                    'Vendor'
                                    'CHAR'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.
      PERFORM append_fieldcat USING 'LFIMG'
                                    'IT_OUTTAB'
                                    13
                                    'Quantity'
                                    'Quantity'
                                    'Quantity'
                                    'QUAN'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.

* PERFORM append_fieldcat USING 'DELDATE'
*                                'IT_OUTTAB'
*                                10
*                                'Delivery Date'
*                                'Del.Date'
*                                'Del Date'
*                                'DATS'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.
      PERFORM append_fieldcat USING 'NETWR'
                                      'IT_OUTTAB'
                                      13
                                      'Value'
                                      'Value'
                                      'Value'
                                      'CURR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'KRATE'
                                      'IT_OUTTAB'
                                      13
                                      'Unit Price'
                                      'Unit Price'
                                      'Unit Price'
                                      'CURR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'PER'
                                      'IT_OUTTAB'
                                      3
                                      'Per'
                                      'Per'
                                      'Per'
                                      'DEC'
                                      ''
                                      ''
                                      'X'
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'CURRENCY'
                                      'IT_OUTTAB'
                                      3
                                      'Currency'
                                      'Currency'
                                      'Cur'
                                      'CUKY'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.


      PERFORM append_fieldcat USING 'SANO'
                                      'IT_OUTTAB'
                                      10
                                      'SA Number'
                                      'SA No.'
                                      'SA No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'INVNO'
                                      'IT_OUTTAB'
                                      10
                                      'Invoice No'
                                      'Invoice No'
                                      'Invoice No'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'RCNO'
                                      'IT_OUTTAB'
                                      10
                                      'Receipt No'
                                      'Receipt No'
                                      'Receipt No'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'ASNO'
                                      'IT_OUTTAB'
                                      10
                                      'VND ASN'
                                      'VND ASN'
                                      'VND ASN'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'MOVETYPE'
                                      'IT_OUTTAB'
                                      4
                                      'Movement Type'
                                      'M Ty'
                                      'M Ty'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'LGORT'
                                      'IT_OUTTAB'
                                      4
                                      'Storage Location'
                                      'Location'
                                      'Loca'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'INVDATE'
                                      'IT_OUTTAB'
                                      10
                                      'Invoice Date'
                                      'Inv Date'
                                      'Inv Date'
                                      'DATS'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.
      PERFORM append_fieldcat USING 'PAYDATE'
                                      'IT_OUTTAB'
                                      10
                                      'Payment Date'
                                      'Pay Date'
                                      'Pay Date'
                                      'DATS'
                                      ''
                                      ''
                                      'X'
                                      ''
                                      ''.


    WHEN OTHERS.
      PERFORM append_fieldcat USING 'EBELN'
                                      'IT_OUTTAB'
                                      10
                                      'PO Number'
                                      'PO No.'
                                      'PO No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EBELP'
                                      'IT_OUTTAB'
                                      5
                                      'Item No'
                                      'Item No'
                                      'Item No.'
                                      'CHAR'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.



      PERFORM append_fieldcat USING 'LFIMG'
                                      'IT_OUTTAB'
                                      13
                                      'Quantity'
                                      'Quantity'
                                      'Quantity'
                                      'QUAN'
                                      ''
                                      ''
                                      ''
                                      ''
                                      ''.

      PERFORM append_fieldcat USING 'EDATU'
                                     'IT_OUTTAB'
                                     10
                                     'Delivery Date'
                                     'Del Date'
                                     'Del Date'
                                     'DATS'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

      PERFORM append_fieldcat USING 'WERKS'
                                     'IT_OUTTAB'
                                     4
                                     'Plant'
                                     'Plant'
                                     'Plant'
                                     'CHAR'
                                     ''
                                     ''
                                     ''
                                     ''
                                     ''.

  ENDCASE.

  PERFORM append_fieldcat USING 'STATXT'
                                'IT_OUTTAB'
                                70
                                'Status Text'
                                'Status Text'
                                'Status Text'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.
ENDFORM.                    " build_fieldcat

*---------------------------------------------------------------------*
*       FORM append_fieldcat                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDNAME                                                   *
*  -->  P_TABNAME                                                     *
*  -->  P_OUTPUTLEN                                                   *
*  -->  P_TEXT_L                                                      *
*  -->  P_TEXT_M                                                      *
*  -->  P_TEXT_S                                                      *
*  -->  P_DATATYPE                                                    *
*  -->  P_KEY                                                         *
*  -->  P_KEY_SEL                                                     *
*  -->  P_NO_OUT                                                      *
*  -->  P_NO_ZERO                                                     *
*  -->  P_TEXT_FIELD                                                  *
*---------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_key_sel
                              p_no_out
                              p_no_zero
                              p_text_field.
  it_fieldcat1-fieldname      = p_fieldname.
  it_fieldcat1-tabname        = p_tabname.
  it_fieldcat1-outputlen      = p_outputlen.
  it_fieldcat1-scrtext_l      = p_text_l.
  it_fieldcat1-scrtext_m      = p_text_m.
  it_fieldcat1-scrtext_s      = p_text_s.
  it_fieldcat1-datatype       = p_datatype.
  it_fieldcat1-key            = p_key.
  it_fieldcat1-key_sel        = p_key_sel.
  it_fieldcat1-no_out         = p_no_out.
  it_fieldcat1-no_zero        = p_no_zero.
*  it_fieldcat1-text_fieldname = p_text_field.
  APPEND it_fieldcat1. CLEAR it_fieldcat1.
ENDFORM.                    " append_fieldcat1

*---------------------------------------------------------------------*
*       FORM BUILD_LAYOUT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_layout.
  is_layout-zebra = 'X'.
*  is_layout-ctab_fname = 'CT'.
  CASE p_mestyp.
    WHEN 'DESADV'.
      CONCATENATE 'IDoc List'  ' - ASN' INTO is_layout-grid_title.
    WHEN 'DELFOR'.
      CONCATENATE 'IDoc List'  ' - Forecast' INTO is_layout-grid_title.
    WHEN 'DELJIT'.
      CONCATENATE 'IDoc List'  ' - JIT' INTO is_layout-grid_title.
    WHEN 'ORDERS' OR 'ORDCHG'.
      CONCATENATE 'IDoc List'  ' - Emergency PO'
                                 INTO is_layout-grid_title.
    WHEN 'GSVERF'.
      CONCATENATE 'IDoc List'  ' - Receiving Advice'
                                 INTO is_layout-grid_title.
    WHEN 'ALEAUD'.
      CONCATENATE 'IDoc List'  ' - ALEAUD' INTO is_layout-grid_title.
  ENDCASE.
  is_layout-excp_fname = 'LIGHT'.

ENDFORM.                    "BUILD_LAYOUT

*---------------------------------------------------------------------*
*       FORM BUILD_SORT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sort.
  it_sort-spos = 1.
  it_sort-fieldname = 'DOCNUM'.
  APPEND it_sort.
  it_sort-spos = 2.
  it_sort-fieldname = 'MATNR'.
  APPEND it_sort.
  CLEAR it_sort.
ENDFORM.                    "BUILD_SORT

*---------------------------------------------------------------------*
*       FORM BUILD_COLOR_RED                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELD                                                       *
*  -->  P_FNAME                                                       *
*---------------------------------------------------------------------*
FORM build_color_red USING p_field p_fname.
  wa_color-color-col = 6.
  wa_color-color-int = 1.
  wa_color-fname = p_fname.
  APPEND wa_color TO it_color.
  CLEAR wa_color.
ENDFORM.                    "BUILD_COLOR_RED

*---------------------------------------------------------------------*
*       FORM BUILD_COLOR_WARNING                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELD                                                       *
*  -->  P_FNAME                                                       *
*---------------------------------------------------------------------*
FORM build_color_warning USING p_field p_fname.
  wa_color-color-col = 7.
  wa_color-color-int = 1.
  wa_color-fname = p_fname.
  APPEND wa_color TO it_color.
  CLEAR wa_color.
ENDFORM.                    "BUILD_COLOR_WARNING
*&---------------------------------------------------------------------*
*&      Form  show_status_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM show_status_table TABLES p_et_index_rows
                                STRUCTURE lvc_s_row.

  DATA: ls_selected_line LIKE lvc_s_row,
        lf_row_index TYPE lvc_index,
        ls_outtab LIKE LINE OF it_outtab.

  CLEAR it_stadetail.
  REFRESH it_stadetail.
  LOOP AT p_et_index_rows INTO ls_selected_line.
    lf_row_index = ls_selected_line-index.
    READ TABLE it_outtab INDEX lf_row_index INTO ls_outtab.

* select corresponding lines of table sbook
* and append new lines to global table
    PERFORM select_table_status USING ls_outtab
                               CHANGING it_stadetail[].
  ENDLOOP.

  LOOP AT it_stadetail.
    IF it_stadetail-statxt = ' '.
      READ TABLE it_teds2 WITH KEY status = it_stadetail-status
                                   langua = 'E'.
      IF sy-subrc = 0.
        it_stadetail-statxt = it_teds2-descrp.
      ELSE.
        it_stadetail-statxt = 'No data in TEDS2 Table'.
      ENDIF.
    ELSE.
      w_msgid = it_stadetail-stamid.
      w_msgno = it_stadetail-stamno.
      CLEAR w_statxt.
      PERFORM read_message USING w_msgid
                                 w_msgno
                                 it_stadetail-stapa1
                                 it_stadetail-stapa2
                                 it_stadetail-stapa3
                                 it_stadetail-stapa4
                        CHANGING w_statxt.
      it_stadetail-statxt = w_statxt.
    ENDIF.
    MODIFY it_stadetail.
  ENDLOOP.
* call dialog screen and display new alv control
  PERFORM build_fieldcat2.
  PERFORM build_sort2.
  PERFORM build_layout2.

  CALL SCREEN 101. " STARTING AT 3 3.

ENDFORM.                    " show_status_table
*&---------------------------------------------------------------------*
*&      Form  select_table_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB  text
*      <--P_IT_STADETAIL  text
*----------------------------------------------------------------------*
FORM select_table_status USING    p_ls_outtab LIKE LINE OF it_outtab
                         CHANGING p_it_stadetail LIKE it_stadetail[].

  DATA: wa_stadetail LIKE it_stadetail[].

* Select data from sbook according to a line of sflight
* and append that data to table p_gt_sbook
*
  SELECT * FROM edids INTO TABLE wa_stadetail
         WHERE  docnum  = p_ls_outtab-docnum.
*         AND    status  = p_ls_outtab-status

  APPEND LINES OF wa_stadetail TO p_it_stadetail.

ENDFORM.                    " select_table_status



*---------------------------------------------------------------------*
*       MODULE PBO_101 OUTPUT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE pbo_101 OUTPUT.
  g_repid = sy-repid.
  gs_variant-report = g_repid.

  IF g_dailog_container IS INITIAL.
    CREATE OBJECT g_dailog_container
      EXPORTING
        container_name              = g_dailog
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = sy-subrc
          txt1  = 'The control could not be created'(510).
    ENDIF.
* create an instance of alv control
    CREATE OBJECT grid2
      EXPORTING
        i_parent = g_dailog_container.
*
* change title
*
    is_layout-grid_title = 'Details of Status'(101).

    is_layout-sel_mode = ' '.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout       = is_layout2
        i_save          = 'A'
        is_variant      = gs_variant
      CHANGING
        it_fieldcatalog = it_fieldcat2[]
        it_sort         = it_sort2[]
        it_outtab       = it_stadetail[].

  ELSE.
    CALL METHOD grid2->refresh_table_display.
  ENDIF.                               "IF custom_container2 IS INITIAL.
  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = grid2.
  CALL METHOD cl_gui_cfw=>flush.
  IF sy-subrc NE 0.
* add your handling, for example
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = g_repid
        txt2  = sy-subrc
        txt1  = 'Error in Flush'(500).
  ENDIF.

ENDMODULE.                 " PBO_101  OUTPUT

*---------------------------------------------------------------------*
*       MODULE PAI_101 INPUT                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE pai_101 INPUT.
  CASE ok_code.
    WHEN 'RETURN'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " PAI_101  INPUT

*---------------------------------------------------------------------*
*       FORM build_fieldcat2                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_fieldcat2.
  CLEAR : it_fieldcat2.
  REFRESH : it_fieldcat2.
  CHECK it_fieldcat2[] IS INITIAL.
  PERFORM append_fieldcat2 USING 'DOCNUM'
                                'IT_STADETAIL'
                                16
                                'Idoc Number'
                                'Idoc Number'
                                'Idoc Number'
                                'CHAR'
                                'X'
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat2 USING 'STATUS'
                                 'IT_STADETAIL'
                                 2
                                 'Status'
                                 'Status'
                                 'Status'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat2 USING 'LOGDAT'
                                 'IT_STADETAIL'
                                 10
                                 'Date of Changed'
                                 'Date chgd'
                                 'Date chgd'
                                 'DATS'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat2 USING 'LOGTIM'
                                  'IT_STADETAIL'
                                  8
                                  'Time of Change'
                                  'Time Chg'
                                  'Time Chg'
                                  'TIMS'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.


  PERFORM append_fieldcat2 USING 'CREDAT'
                                  'IT_STADETAIL'
                                  10
                                  'Date of Creation'
                                  'Date Cred'
                                  'Date Cred'
                                  'DATS'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.

  PERFORM append_fieldcat2 USING 'CRETIM'
                                  'IT_STADETAIL'
                                  8
                                  'Time of Creation'
                                  'Time Cred'
                                  'Time Cred'
                                  'TIMS'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.
  PERFORM append_fieldcat2 USING 'STATXT'
                                  'IT_STADETAIL'
                                  100
                                  'Text of Status'
                                  'Text of Status'
                                  'Text of Status'
                                  'CHAR'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.

ENDFORM.                    " build_fieldcat2

*---------------------------------------------------------------------*
*       FORM append_fieldcat2                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDNAME                                                   *
*  -->  P_TABNAME                                                     *
*  -->  P_OUTPUTLEN                                                   *
*  -->  P_TEXT_L                                                      *
*  -->  P_TEXT_M                                                      *
*  -->  P_TEXT_S                                                      *
*  -->  P_DATATYPE                                                    *
*  -->  P_KEY                                                         *
*  -->  P_KEY_SEL                                                     *
*  -->  P_NO_OUT                                                      *
*  -->  P_NO_ZERO                                                     *
*  -->  P_TEXT_FIELD                                                  *
*---------------------------------------------------------------------*
FORM append_fieldcat2 USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_key_sel
                              p_no_out
                              p_no_zero
                              p_text_field.
  it_fieldcat2-fieldname      = p_fieldname.
  it_fieldcat2-tabname        = p_tabname.
  it_fieldcat2-outputlen      = p_outputlen.
  it_fieldcat2-scrtext_l      = p_text_l.
  it_fieldcat2-scrtext_m      = p_text_m.
  it_fieldcat2-scrtext_s      = p_text_s.
  it_fieldcat2-datatype       = p_datatype.
  it_fieldcat2-key            = p_key.
  it_fieldcat2-key_sel        = p_key_sel.
  it_fieldcat2-no_out         = p_no_out.
  it_fieldcat2-no_zero        = p_no_zero.
*  it_fieldcat2-text_fieldname = p_text_field.
  APPEND it_fieldcat2. CLEAR it_fieldcat2.
ENDFORM.                    " append_fieldcat2

*---------------------------------------------------------------------*
*       FORM BUILD_LAYOUT2                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_layout2.
  is_layout2-zebra = 'X'.
  is_layout2-grid_title = 'IDoc Status'.
ENDFORM.                    "BUILD_LAYOUT2

*---------------------------------------------------------------------*
*       FORM BUILD_SORT2                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sort2.
  it_sort2-spos = 1.
  it_sort2-fieldname = 'DOCNUM'.
  APPEND it_sort2.
  it_sort2-spos = 2.
  it_sort2-fieldname = 'STATUS'.
*  append it_sort2.
  CLEAR it_sort2.
ENDFORM.                    "BUILD_SORT2

*---------------------------------------------------------------------*
*       FORM PROCESS_DETAILS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_details.
  CLEAR lt_rows[].
  CLEAR wa_lt_row.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  CALL METHOD cl_gui_cfw=>flush.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = g_repid
        txt2  = sy-subrc
        txt1  = 'Error in Flush'.
  ELSE.
    PERFORM show_status_table TABLES lt_rows.
  ENDIF.
ENDFORM.                    "PROCESS_DETAILS

*---------------------------------------------------------------------*
*       FORM PROCESS_WE02                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_we02.
  DATA: ctu(1) VALUE 'X',
        mode(1) VALUE 'A',
        updat(1) VALUE 'S',
        messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  CLEAR lt_rows[].
  CLEAR wa_lt_row.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  READ TABLE lt_rows INTO wa_lt_row INDEX 1.
  IF sy-subrc = 0.
    CLEAR it_outtab0.
    READ TABLE it_outtab INTO it_outtab0 INDEX wa_lt_row-index.
    IF sy-subrc = 0.
*        SET PARAMETER ID 'DCN' FIELD IT_OUTTAB0-DOCNUM.
      PERFORM fill_dbcdata.
      PERFORM bdc_transaction TABLES messtab
                              USING  'WE02'
                                      ctu
                                      mode
                                      updat.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.
ENDFORM.                    "PROCESS_WE02

*---------------------------------------------------------------------*
*       FORM FILL_DBCDATA                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_dbcdata.
  DATA: wa_docnum(16),
        wa_credate(8).
  wa_docnum = it_outtab0-docnum.
  PERFORM user_date_format USING  sy-uname
                                it_outtab0-credat
                                CHANGING wa_credate.

  PERFORM bdc_dynpro      USING 'RSEIDOC2' '1000'.
*   perform bdc_field       using 'BDC_OKCODE'
*                                  '=ONLI'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DOCNUM-LOW'.
  PERFORM bdc_field       USING 'CRETIM-LOW'
                                '00:00:00'.
  PERFORM bdc_field       USING 'CRETIM-HIGH'
                                '24:00:00'.
  PERFORM bdc_field       USING 'CREDAT-LOW'
                                 wa_credate.
  PERFORM bdc_field       USING 'CREDAT-HIGH'
                                 wa_credate.
  PERFORM bdc_field       USING 'UPDTIM-LOW'
                                '00:00:00'.
  PERFORM bdc_field       USING 'UPDTIM-HIGH'
                                '24:00:00'.
  PERFORM bdc_field       USING 'DOCNUM-LOW'
                                 wa_docnum.
ENDFORM.                    "FILL_DBCDATA

*---------------------------------------------------------------------*
*       FORM USER_DATE_FORMAT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_USER)                                                 *
*  -->  VALUE(P_DATE)                                                 *
*  -->  VALUE(P_USERDATE)                                             *
*---------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate).
  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
    FROM usr01
    WHERE bname = p_user.
** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.
ENDFORM.                    " user_date_format
