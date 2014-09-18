************************************************************************
* Program Name      : ZEMMPM36E_IDEL
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.24.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K902658
* Addl Documentation:
* Description       : Inbound Delivery Create - KDWeb ASN
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.24.     Sung-Tae Lim     UD1K902658     Initial Coding
* 03/17/2005      Shiva            UD1K915000     Update error message
*                                                 if error occurs while
*                                                 creating delivery.
* 03/17/2005      Shiva            UD1K915032     Update error message
*                                                 if error occurs while
*                                               reading PO information.
* 03/21/2005      Shiva            UD1K915099   Type conflict for BAPI
*                                               reading error message.
* 04/25/2005      BSBAE            UD1K915715   Multiful Reprocessing
*                                               Probelm
* 01/28/2007      Manju            UD1K930445   Enhance KD ASN Process
*                                               to include BOL# in
*                                               delivery header.
* 10/06/2008      Vijay           UD1K944653    Dupilcate entries with
*                                               deletion indicator for
*                                               one item
************************************************************************
REPORT zemmpm36e_idel MESSAGE-ID zmmm.
INCLUDE: <icon>.

**---
INCLUDE : zrmmpmxxr_incl.

DATA: zsmm_zemmpm36e_idel_9000 LIKE zsmm_zemmpm36e_idel_9000.

**--- Tables, Views & Structures
DATA: it_9000 TYPE STANDARD TABLE OF zsmm_zemmpm36e_idel_9000
                                     WITH HEADER LINE.

DATA: it_9000_tar LIKE it_9000 OCCURS 0 WITH HEADER LINE.

DATA: it_9000_chpo LIKE it_9000 OCCURS 0 WITH HEADER LINE.

DATA: it_9100 LIKE it_9000 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_ekpo_short OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        werks LIKE ekpo-werks,
        lgort LIKE ekpo-lgort,
        menge LIKE ekpo-menge,
        meins LIKE ekpo-meins,
        lmein LIKE ekpo-lmein,
        umrez LIKE ekpo-umrez,
        umren LIKE ekpo-umren,
        matnr LIKE ekpo-matnr,
        ematn LIKE ekpo-ematn,                              "386409
        mfrnr LIKE ekpo-mfrnr,
        mfrpn LIKE ekpo-mfrpn,
        emnfr LIKE ekpo-emnfr,
        cuobj LIKE ekpo-cuobj,
        uebto LIKE ekpo-uebto,
        untto LIKE ekpo-untto,
        uebtk LIKE ekpo-uebtk,
        bwtar LIKE ekpo-bwtar,
        idnlf LIKE ekpo-idnlf,
        txz01 LIKE ekpo-txz01,
        mfrgr LIKE ekpo-mfrgr,
        gewei LIKE ekpo-gewei,
        voleh LIKE ekpo-voleh,
        ntgew LIKE ekpo-ntgew,
        brgew LIKE ekpo-brgew,
        volum LIKE ekpo-volum,
        ean11 LIKE ekpo-ean11,
        aktnr LIKE ekpo-aktnr,
        abeln LIKE ekpo-abeln,
        abelp LIKE ekpo-abelp,
        aurel LIKE ekpo-aurel,
        matkl LIKE ekpo-matkl,
        upvor LIKE ekpo-upvor,
        uptyp LIKE ekpo-uptyp,
        uebpo LIKE ekpo-uebpo,
        bstae LIKE ekpo-bstae,
        wepos LIKE ekpo-wepos,
        loekz LIKE ekpo-loekz,
        elikz LIKE ekpo-elikz,
        anlmg LIKE ekpo-menge,
        insmk LIKE ekpo-insmk,
        pstyp LIKE ekpo-pstyp,
        sobkz LIKE ekpo-sobkz,
        kzvbr LIKE ekpo-kzvbr,            "note 384051
        knttp LIKE ekpo-knttp,
        kzfme LIKE ekpo-kzfme,
      END OF it_ekpo_short.

DATA: BEGIN OF it_ekpo_short1 OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        werks LIKE ekpo-werks,
        lgort LIKE ekpo-lgort,
        menge LIKE ekpo-menge,
        meins LIKE ekpo-meins,
        lmein LIKE ekpo-lmein,
        umrez LIKE ekpo-umrez,
        umren LIKE ekpo-umren,
        matnr LIKE ekpo-matnr,
        ematn LIKE ekpo-ematn,                              "386409
        mfrnr LIKE ekpo-mfrnr,
        mfrpn LIKE ekpo-mfrpn,
        emnfr LIKE ekpo-emnfr,
        cuobj LIKE ekpo-cuobj,
        uebto LIKE ekpo-uebto,
        untto LIKE ekpo-untto,
        uebtk LIKE ekpo-uebtk,
        bwtar LIKE ekpo-bwtar,
        idnlf LIKE ekpo-idnlf,
        txz01 LIKE ekpo-txz01,
        mfrgr LIKE ekpo-mfrgr,
        gewei LIKE ekpo-gewei,
        voleh LIKE ekpo-voleh,
        ntgew LIKE ekpo-ntgew,
        brgew LIKE ekpo-brgew,
        volum LIKE ekpo-volum,
        ean11 LIKE ekpo-ean11,
        aktnr LIKE ekpo-aktnr,
        abeln LIKE ekpo-abeln,
        abelp LIKE ekpo-abelp,
        aurel LIKE ekpo-aurel,
        matkl LIKE ekpo-matkl,
        upvor LIKE ekpo-upvor,
        uptyp LIKE ekpo-uptyp,
        uebpo LIKE ekpo-uebpo,
        bstae LIKE ekpo-bstae,
        wepos LIKE ekpo-wepos,
        loekz LIKE ekpo-loekz,
        elikz LIKE ekpo-elikz,
        anlmg LIKE ekpo-menge,
        insmk LIKE ekpo-insmk,
        pstyp LIKE ekpo-pstyp,
        sobkz LIKE ekpo-sobkz,
        kzvbr LIKE ekpo-kzvbr,            "note 384051
        knttp LIKE ekpo-knttp,
        kzfme LIKE ekpo-kzfme,
      END OF it_ekpo_short1.

DATA : st_vbsk LIKE vbsk.

DATA : it_komdlgn LIKE komdlgn OCCURS 0 WITH HEADER LINE,
       it_vbfs    LIKE vbfs    OCCURS 0 WITH HEADER LINE,
       it_vbls    LIKE vbls    OCCURS 0 WITH HEADER LINE,
       it_lips    LIKE lips    OCCURS 0 WITH HEADER LINE.

DATA : it_ekkn LIKE ekkn OCCURS 0 WITH HEADER LINE.

DATA : wa_itab LIKE it_9000.

DATA : it_xeket LIKE beket OCCURS 0 WITH HEADER LINE.


*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.


**--- Variables
DATA : w_mode LIKE ctu_params-dismode VALUE 'A'.

DATA:      gf_dlv_type LIKE likp-lfart. "Delivery Type
DATA:      gf_ebtyp    LIKE t163d-ebtyp.                    "386409

DATA : w_qty_sum LIKE it_komdlgn-lfimg,
       w_success(4)   TYPE   n,
       w_error(4)     TYPE   n,
       w_ready(4)     TYPE   n,
       w_total(4)     TYPE   n.

* Begin of changes -  UD1K930445
DATA: BEGIN OF wa_asn,
      traid(11) TYPE c,
      cinvo(11) TYPE c,
      END OF wa_asn.
DATA: BEGIN OF wa_bol_inf,
         cinvo(11) TYPE c,
         traid(11) TYPE c,
         zfhblno(16) TYPE c,                                "UD1K919888
        END OF wa_bol_inf.
DATA: it_asn LIKE TABLE OF wa_asn,
      it_bol_inf LIKE TABLE OF wa_bol_inf WITH HEADER LINE.

FIELD-SYMBOLS: <fs_kdasn> LIKE LINE OF it_9000.

DATA: wa_9000 LIKE zsmm_zemmpm36e_idel_9000,
          it_err_9000 LIKE TABLE OF wa_9000,
          lt_rows TYPE lvc_t_row WITH HEADER LINE.

* End of changes -  UD1K930445


CONSTANTS : c_ibtyp LIKE t163d-ibtyp VALUE '2',
            c_check                  VALUE 'X'.

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

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : w_is_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSMM_ZEMMPM36E_IDEL_'.

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
*  PUBLIC SECTION.
*    METHODS:
*
*    handle_double_click
*        FOR EVENT double_click OF cl_gui_alv_grid
*            IMPORTING e_row
*                      e_column
*                      es_row_no.
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
ENDCLASS.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zedat FOR ztmm_kd_asn_main-zedat NO-EXTENSION
                             DEFAULT sy-datum.
SELECT-OPTIONS : s_zinv  FOR ztmm_kd_asn_main-zinvoice.
SELECT-OPTIONS : s_zslno FOR ztmm_kd_asn_main-zslno.
SELECTION-SCREEN SKIP.
* Begin of changes - UD1K930445
PARAMETER:       p_update AS CHECKBOX.                      "UD1K930445
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-003 FOR FIELD r1.
SELECTION-SCREEN POSITION 25.
PARAMETERS       r2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 27(25) text-004 FOR FIELD r2.
SELECTION-SCREEN POSITION 55.
PARAMETERS       r3 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT 57(10) text-005 FOR FIELD r3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.

PARAMETERS : p_mode LIKE ctu_params-dismode DEFAULT 'N'.

START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_9000[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.

* Begin of changes - UD1K930445
    IF p_update EQ c_check.
      PERFORM set_target_data TABLES lt_rows.
      PERFORM create_delivery TABLES it_9000_tar.
      PERFORM update_table TABLES it_9000_tar.
      PERFORM create_bol.
    ENDIF.
* End of changes - UD1K919888

    PERFORM count_rtn.
    PERFORM display_data.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_9000, it_9000[].

*--- Processing
  IF r1 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
             FROM ztmm_kd_asn_main
            WHERE zslno    IN s_zslno
              AND zedat    IN s_zedat
              AND zinvoice IN s_zinv
              AND zresult  EQ space.
  ENDIF.

*--- Re-Processing(Error)
  IF r2 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
             FROM ztmm_kd_asn_main
            WHERE zslno    IN s_zslno
              AND zedat    IN s_zedat
              AND zinvoice IN s_zinv
              AND zresult  EQ 'E'.
  ENDIF.

*--- Success
  IF r3 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
             FROM ztmm_kd_asn_main
            WHERE zslno    IN s_zslno
              AND zedat    IN s_zedat
              AND zinvoice IN s_zinv
              AND zresult  EQ 'S'.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  create_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_delivery TABLES pt_9000 STRUCTURE it_9000.
*--- sort by Container No.(traid) & Case No.(ematn)
  SORT pt_9000 BY traid ematn.

  LOOP AT pt_9000.
    CLEAR : wa_itab.
    MOVE : pt_9000 TO wa_itab.
    AT NEW ematn.     " Case Number
      CLEAR : it_komdlgn, it_komdlgn[].
    ENDAT.
*    PERFORM vbsk_fill.
    PERFORM get_inbound_delivery_type USING '2'.
    PERFORM it_ekpo_short_fill.
    PERFORM komdlgn_fill.
    AT END OF ematn.     " Case Number
      PERFORM call_function.
      PERFORM modify_itab.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " create_delivery
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table TABLES pt_9000 STRUCTURE it_9000.
*--- update table
  DATA : l_total TYPE i,
         l_succs TYPE i,
         l_error TYPE i.

  LOOP AT pt_9000.
    READ TABLE it_9000 WITH KEY traid = pt_9000-traid
                                ematn = pt_9000-ematn
                                matnr = pt_9000-matnr
                                ebeln = pt_9000-ebeln.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m04.
    ENDIF.

    CLEAR: ztmm_kd_asn_main.
    MOVE-CORRESPONDING it_9000 TO ztmm_kd_asn_main.
    MODIFY ztmm_kd_asn_main.
    IF sy-subrc NE 0.
      MOVE: 'E'      TO it_9000-zresult,
            text-009 TO it_9000-zmsg.
    ENDIF.

    CASE it_9000-zresult.
      WHEN 'S'.
        MOVE: icon_green_light TO it_9000-icon.
      WHEN 'E'.
        MOVE: icon_red_light TO it_9000-icon.
      WHEN space.
        MOVE: icon_yellow_light TO it_9000-icon.
    ENDCASE.

    MODIFY it_9000 INDEX sy-tabix.
  ENDLOOP.

* Also Update log table for the records which don't
* have  BOL data.
  LOOP AT it_9000 WHERE zresult = 'E'.
    MOVE-CORRESPONDING it_9000 TO ztmm_kd_asn_main.
    MODIFY ztmm_kd_asn_main.
    IF sy-subrc NE 0.
      MOVE: 'E'      TO it_9000-zresult,
            text-010 TO it_9000-zmsg.
    ENDIF.
  ENDLOOP.

*--- logging interface table
  DATA : st_ztca_if_log LIKE ztca_if_log.

  CLEAR : st_ztca_if_log.

  MOVE : sy-tcode TO st_ztca_if_log-tcode,
         l_total  TO st_ztca_if_log-total,
         l_succs  TO st_ztca_if_log-zsucc,
         l_error  TO st_ztca_if_log-error,
         sy-datum TO st_ztca_if_log-erdat,
         sy-uzeit TO st_ztca_if_log-erzet,
         sy-uname TO st_ztca_if_log-ernam,
         sy-datum TO st_ztca_if_log-aedat,
         sy-uzeit TO st_ztca_if_log-aezet,
         sy-uname TO st_ztca_if_log-aenam.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = st_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY icon traid ematn matnr.

  LOOP AT it_9000.
    CASE it_9000-zresult.
      WHEN 'S'.
        MOVE: icon_green_light TO it_9000-icon.
      WHEN 'E'.
        MOVE: icon_red_light TO it_9000-icon.
      WHEN space.
        MOVE: icon_yellow_light TO it_9000-icon.
    ENDCASE.

    MODIFY it_9000.
  ENDLOOP.

  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  get_inbound_delivery_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0263   text
*----------------------------------------------------------------------*
FORM get_inbound_delivery_type USING    besttyp.
*--- get inbound delivery type from T163G
  DATA: h_ibtyp LIKE t163d-ibtyp.
* Lieferart für Grob-WE aus Bestätigungssteuerung ermitteln
  h_ibtyp = besttyp.
  CALL FUNCTION 'ME_CONFIRMATION_DELIVERY_TYPE'
       EXPORTING
            i_func              = '1'
       CHANGING
            c_ibtyp             = h_ibtyp
            c_ebtyp             = gf_ebtyp                  "386409
            c_lfart             = gf_dlv_type
       EXCEPTIONS
            function_not_valid  = 01
            param_value_missing = 02
            no_item_found       = 03.
  IF sy-subrc = 0.
    IF gf_dlv_type = space.
      gf_dlv_type = 'EL'.
    ENDIF.
  ELSE.
    gf_dlv_type = 'EL'.
  ENDIF.
ENDFORM.                    " get_inbound_delivery_type
*&---------------------------------------------------------------------*
*&      Form  it_ekpo_short_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_ekpo_short_fill.

  CLEAR : it_ekpo_short, it_ekpo_short[].

** Changed by Furong on 03/07/08
  DATA: l_index LIKE sy-tabix.
*  SELECT
*         ebeln ebelp menge meins matnr werks lgort bstae loekz elikz
*         lmein umrez umren insmk pstyp sobkz knttp kzfme kzvbr"384051
*         ematn mfrnr mfrpn emnfr cuobj uebto untto uebtk bwtar idnlf
*         txz01 mfrgr gewei voleh ntgew brgew volum ean11 aktnr abeln
*         abelp aurel matkl upvor uptyp uebpo wepos          "386409
*          INTO CORRESPONDING FIELDS OF TABLE it_ekpo_short
*          FROM ekpo
*           WHERE ebeln EQ wa_itab-ebeln
**             AND ebelp EQ wa_itab-ebelp
*             AND matnr EQ wa_itab-matnr
**             AND werks IN s_werks
**             and matnr ne space
*"386409
**             AND lgort IN s_lgort
**             AND bstae IN r_bstae
*             AND loekz EQ space
*             AND elikz EQ space
*             AND retpo EQ space                             "327089
*           ORDER BY ebeln ebelp.
*  IF sy-subrc NE 0.
*    CONCATENATE text-008 wa_itab-matnr INTO it_9000-zmsg.
*    MOVE : 'E'           TO it_9000-zresult,
*           c_red         TO it_9000-linecolor.
**---
*    MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
*           sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
*           sy-uname TO it_9000-zbnam,     " User ID
*           'C'      TO it_9000-zmode.     " Data Characteristic Flag
**---
*    MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
*                                zresult zmsg linecolor
*                          WHERE traid EQ wa_itab-traid
*                            AND ematn EQ wa_itab-ematn
*                            AND matnr EQ wa_itab-matnr
*                            AND ebeln EQ wa_itab-ebeln.
*  ENDIF.

  SELECT
         ebeln ebelp menge meins matnr werks lgort bstae loekz elikz
         lmein umrez umren insmk pstyp sobkz knttp kzfme kzvbr"384051
         ematn mfrnr mfrpn emnfr cuobj uebto untto uebtk bwtar idnlf
         txz01 mfrgr gewei voleh ntgew brgew volum ean11 aktnr abeln
         abelp aurel matkl upvor uptyp uebpo wepos loekz elikz"386409
          INTO CORRESPONDING FIELDS OF TABLE it_ekpo_short
          FROM ekpo
           WHERE ebeln EQ wa_itab-ebeln
              AND matnr EQ wa_itab-matnr
*             AND loekz EQ space
*            AND ELIKZ EQ SPACE
             AND retpo EQ space                             "327089
           ORDER BY ebeln ebelp.
  IF sy-subrc NE 0.
    CONCATENATE text-m13 wa_itab-matnr INTO it_9000-zmsg.
    MOVE : 'E'           TO it_9000-zresult,
           c_red         TO it_9000-linecolor.
*---
    MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
           sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
           sy-uname TO it_9000-zbnam,     " User ID
           'C'      TO it_9000-zmode.     " Data Characteristic Flag
*---
    MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
                                zresult zmsg linecolor
                          WHERE traid EQ wa_itab-traid
                            AND ematn EQ wa_itab-ematn
                            AND matnr EQ wa_itab-matnr
                            AND ebeln EQ wa_itab-ebeln.
  ELSE.

******* added by Vijay UD1K94465 ***********************
    DATA: wa_ekpo LIKE it_ekpo_short.

    CLEAR: it_ekpo_short1, it_ekpo_short1[].

    it_ekpo_short1[] = it_ekpo_short[].

*    LOOP AT it_ekpo_short.
*      IF it_ekpo_short-ebeln = wa_ekpo-ebeln AND
*         it_ekpo_short-matnr = wa_ekpo-matnr.
*        IF it_ekpo_short-loekz = 'L'.
*          DELETE it_ekpo_short.
*        ELSE.
*          wa_ekpo-loekz = 'L'.
*          DELETE TABLE it_ekpo_short FROM wa_ekpo.
*        ENDIF.
*      ENDIF.
*      IF it_ekpo_short-loekz = 'L'.
*        wa_ekpo = it_ekpo_short.
*      ENDIF.
*    ENDLOOP.

    DELETE it_ekpo_short1 WHERE loekz = 'L'.

    SORT it_ekpo_short1 BY ebeln matnr.

    LOOP AT it_ekpo_short.
      IF it_ekpo_short-loekz = 'L'.
        READ TABLE it_ekpo_short1 WITH KEY ebeln = it_ekpo_short-ebeln
                                           matnr = it_ekpo_short-matnr
                                           BINARY SEARCH.

        IF sy-subrc = 0.
          DELETE it_ekpo_short.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR wa_ekpo.

    READ TABLE it_ekpo_short INDEX 1.


*****  changes end by Vijay   *******************

    IF it_ekpo_short-bstae IS INITIAL.     " CONF control
      CONCATENATE text-m20 wa_itab-matnr INTO it_9000-zmsg.
      MOVE : 'E'           TO it_9000-zresult,
             c_red         TO it_9000-linecolor.
      MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
             sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
             sy-uname TO it_9000-zbnam,     " User ID
             'C'      TO it_9000-zmode.     " Data Characteristic Flag
      MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
                                zresult zmsg linecolor
                          WHERE traid EQ wa_itab-traid
                            AND ematn EQ wa_itab-ematn
                            AND matnr EQ wa_itab-matnr
                            AND ebeln EQ wa_itab-ebeln.
      CLEAR: it_ekpo_short,it_ekpo_short[].
    ELSE.
      IF NOT it_ekpo_short-loekz IS INITIAL.     " item deleted
        CONCATENATE text-m19 wa_itab-matnr INTO it_9000-zmsg.
        MOVE : 'E'           TO it_9000-zresult,
               c_red         TO it_9000-linecolor.
        MOVE : sy-datum TO it_9000-zbdat,    " BDC Execute Date
               sy-uzeit TO it_9000-zbtim,    " BDC Execute Time
               sy-uname TO it_9000-zbnam,    " User ID
               'C'      TO it_9000-zmode.    " Data Characteristic Flag
        MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
                                 zresult zmsg linecolor
                           WHERE traid EQ wa_itab-traid
                             AND ematn EQ wa_itab-ematn
                             AND matnr EQ wa_itab-matnr
                             AND ebeln EQ wa_itab-ebeln.
        CLEAR: it_ekpo_short,it_ekpo_short[].
      ELSE.
        LOOP AT it_ekpo_short.
          l_index = sy-tabix.
          IF NOT it_ekpo_short-elikz IS INITIAL.
            CONCATENATE text-m21 wa_itab-matnr INTO it_9000-zmsg.
            MOVE : 'E'           TO it_9000-zresult,
                  c_red         TO it_9000-linecolor.
            MOVE : sy-datum TO it_9000-zbdat,  " BDC Execute Date
                   sy-uzeit TO it_9000-zbtim,  " BDC Execute Time
                   sy-uname TO it_9000-zbnam,  " User ID
                   'C'      TO it_9000-zmode.  " Data Char. Flag
            MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
                                     zresult zmsg linecolor
                               WHERE traid EQ wa_itab-traid
                                 AND ematn EQ wa_itab-ematn
                                 AND matnr EQ wa_itab-matnr
                                 AND ebeln EQ wa_itab-ebeln.
            DELETE it_ekpo_short INDEX l_index.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
** End of change
ENDFORM.                    " it_ekpo_short_fill
*&---------------------------------------------------------------------*
*&      Form  komdlgn_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM komdlgn_fill.

***  Changes added by Vijay

  DELETE it_ekpo_short WHERE loekz = 'L'.
*---
  LOOP AT it_ekpo_short.
    STATICS : h_grkor LIKE lips-grkor,      "Liefergruppe
              h_bsmng LIKE ekpo-menge.         "

    PERFORM read_ekko USING it_ekpo_short-ebeln.

*---
    IF NOT ekko-lifnr IS INITIAL.
      CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_12'
           EXPORTING
                pi_lifnr       = ekko-lifnr
                pi_ekorg       = ekko-ekorg
           IMPORTING
                pe_lfm1        = lfm1
           EXCEPTIONS
                no_entry_found = 1
                OTHERS         = 2.

      it_komdlgn-vsbed = lfm1-vsbed.     " Shipping conditions
    ELSE.
      CLEAR : it_komdlgn-vsbed.
    ENDIF.

*---
    it_komdlgn-lifnr = ekko-lifnr.
    it_komdlgn-inco1 = ekko-inco1.                          "363954
    it_komdlgn-inco2 = ekko-inco2.                          "363954
    it_komdlgn-exnum = ekko-exnum.                          "363954
    it_komdlgn-bukrs_best = ekko-bukrs.                     "363954

*---
    it_komdlgn-matnr   = it_ekpo_short-matnr.
    it_komdlgn-werks   = it_ekpo_short-werks.
    it_komdlgn-lgort   = it_ekpo_short-lgort.
*    xkomdlgn-charg     = ?
    it_komdlgn-vrkme   = it_ekpo_short-meins.
    it_komdlgn-meins   = it_ekpo_short-lmein.
    it_komdlgn-umvkz   = it_ekpo_short-umrez.
    it_komdlgn-umvkn   = it_ekpo_short-umren.

*---
    IF it_ekpo_short-matnr EQ space.                        "386409
      it_komdlgn-meins = it_ekpo_short-meins.
      it_komdlgn-umvkz = 1.
      it_komdlgn-umvkn = 1.
    ENDIF.

*---
    it_komdlgn-insmk = it_ekpo_short-insmk.
    it_komdlgn-kzfme = it_ekpo_short-kzfme.
    it_komdlgn-kzvbr = it_ekpo_short-kzvbr.           "note 384051

*--- get open qty
    CLEAR : it_xeket, it_xeket[], w_qty_sum.

    PERFORM get_open_qty USING it_ekpo_short-ebeln
                               it_ekpo_short-ebelp
*--- 2004/02/17 block by stlim
*                               wa_itab-zcreate.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*                               wa_itab-lfdat_la.
                               wa_itab-eta.
*--- 2004/02/17

*---
*    it_komdlgn-lfimg = wa_itab-lfimg.     "p_open_qty.

**--- block & insert by stlim (2004/05/13)
    IF wa_itab-lfimg EQ w_qty_sum.     " I/D qty = open qty
      MOVE : w_qty_sum TO it_komdlgn-lfimg.
    ELSEIF wa_itab-lfimg GT w_qty_sum.     " I/D qty > open qty
*      MOVE : w_qty_sum TO it_komdlgn-lfimg.
*
      MOVE : text-m03      TO it_9000-zmsg,
             'E'           TO it_9000-zresult,
             c_red         TO it_9000-linecolor.
*---
      MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
             sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
             sy-uname TO it_9000-zbnam,     " User ID
             'C'      TO it_9000-zmode.     " Data Characteristic Flag

      MODIFY it_9000 TRANSPORTING zbdat zbtim zbnam zmode
                                  zresult zmsg linecolor
                            WHERE traid EQ wa_itab-traid
                              AND ematn EQ wa_itab-ematn
                              AND matnr EQ wa_itab-matnr
                              AND ebeln EQ wa_itab-ebeln.
      EXIT.
    ELSEIF wa_itab-lfimg LT w_qty_sum.     " I/D qty < open qty
      MOVE : wa_itab-lfimg TO it_komdlgn-lfimg.
    ENDIF.
*    it_komdlgn-lfimg = w_qty_sum.     " open qty
**--- end of block & insert

*--- 2004/02/17 block by stlim
*    it_komdlgn-lfdat = wa_itab-zcreate.     "p_eindt.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*    it_komdlgn-lfdat = wa_itab-lfdat_la.

**--- blocked by stlim (2004/05/11)
    it_komdlgn-lfdat = wa_itab-eta.
    IF it_komdlgn-lfdat IS INITIAL.
      CLEAR : eket.
      SELECT SINGLE eindt INTO it_komdlgn-lfdat
                          FROM eket
                         WHERE ebeln EQ it_ekpo_short-ebeln
                           AND ebelp EQ it_ekpo_short-ebelp.
    ENDIF.
**--- end of block

**--- insert by stlim (2004/05/11)
*    CLEAR : eket.
*    SELECT SINGLE eindt INTO it_komdlgn-lfdat
*                        FROM eket
*                       WHERE ebeln EQ it_ekpo_short-ebeln
*                         AND ebelp EQ it_ekpo_short-ebelp.
**--- end of insert

*---
*    it_komdlgn-lfuhr = p_uzeit.
*    xkomdlgn-vstel = ?
*    xkomdlgn-vkorg = ?
*    xkomdlgn-vtweg = ?
*    xkomdlgn-spart = ?
    it_komdlgn-vgbel = it_ekpo_short-ebeln.
    it_komdlgn-vgpos = it_ekpo_short-ebelp.
    it_komdlgn-lfart = gf_dlv_type.
    it_komdlgn-vgtyp = 'V'.
    it_komdlgn-kzazu = 'X'.                "??? what's that for ????
    it_komdlgn-knttp = it_ekpo_short-knttp.
    it_komdlgn-sobkz = it_ekpo_short-sobkz.

*--- note 386409:
    SELECT * FROM t163g WHERE bstae EQ it_ekpo_short-bstae
                          AND ebtyp EQ gf_ebtyp.
      EXIT.
    ENDSELECT.
    IF sy-subrc = 0.
* Prüfen, ob Lieferavis WE-Zuordnung hat (Vorauss. für WE über VL32)
* und wepos prüfen
      IF t163g-wezuo EQ space OR it_ekpo_short-wepos EQ space.
        it_komdlgn-nowab = 'X'.
      ELSE.
        CLEAR it_komdlgn-nowab.
      ENDIF.
    ENDIF.

*---
    IF it_ekpo_short-matnr IS INITIAL OR it_ekpo_short-pstyp = '6'.
      it_komdlgn-posar = 'B'.
    ENDIF.

*---
    it_komdlgn-ematn = it_ekpo_short-ematn.
    it_komdlgn-mfrnr = it_ekpo_short-mfrnr.
    it_komdlgn-mfrpn = it_ekpo_short-mfrpn.
    it_komdlgn-emnfr = it_ekpo_short-emnfr.
    it_komdlgn-cuobj = it_ekpo_short-cuobj.
    it_komdlgn-uebto = it_ekpo_short-uebto.
    it_komdlgn-untto = it_ekpo_short-untto.
    it_komdlgn-uebtk = it_ekpo_short-uebtk.
*    it_komdlgn-lichn = p_licha.     " vendor batch number
*    it_komdlgn-charg = p_charg.
    it_komdlgn-bwtar = it_ekpo_short-bwtar.

*---
*    it_komdlgn-kdmat = it_ekpo_short-idnlf.
*    MOVE : wa_itab-ematn TO it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ematn wa_itab-zzkdwebpo INTO
*                              it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ebeln wa_itab-ematn INTO
*                              it_komdlgn-kdmat.
    CONCATENATE wa_itab-zzkdwebpo wa_itab-ematn INTO
                              it_komdlgn-kdmat.
*---

    it_komdlgn-arktx = it_ekpo_short-txz01.
    it_komdlgn-mfrgr = it_ekpo_short-mfrgr.
    it_komdlgn-gewei = it_ekpo_short-gewei.
    it_komdlgn-voleh = it_ekpo_short-voleh.
    it_komdlgn-ntgew = it_ekpo_short-ntgew * it_komdlgn-lfimg.
    it_komdlgn-brgew = it_ekpo_short-brgew * it_komdlgn-lfimg.
    it_komdlgn-volum = it_ekpo_short-volum * it_komdlgn-lfimg.
    it_komdlgn-ean11 = it_ekpo_short-ean11.
*    it_komdlgn-podrel = t163l-podrel.
    it_komdlgn-aktnr = it_ekpo_short-aktnr.
    it_komdlgn-abeln = it_ekpo_short-abeln.
    it_komdlgn-abelp = it_ekpo_short-abelp.
* xkomdlgn-ltssf = only sort criteria in vl31n
    it_komdlgn-aurel = it_ekpo_short-aurel.

*---
*    it_komdlgn-idnlf = it_ekpo_short-idnlf.
    MOVE : wa_itab-ematn TO it_komdlgn-idnlf.
*---

    it_komdlgn-matkl = it_ekpo_short-matkl.

*---
    CLEAR it_komdlgn-grkor.
    CLEAR it_komdlgn-kmpmg.
    CLEAR it_komdlgn-uepos.
    CLEAR it_komdlgn-uepvw.                                 "549736

*---
    IF it_ekpo_short-upvor CA '3X'.
      h_grkor = h_grkor + 1.
      it_komdlgn-grkor = h_grkor.
      h_bsmng = it_ekpo_short-menge.
    ENDIF.

    IF NOT it_ekpo_short-uebpo IS INITIAL AND
           it_ekpo_short-uptyp CA '3X'.
      it_komdlgn-uepvw = 'G'.                               "549736
      it_komdlgn-uepos = it_ekpo_short-uebpo.
      it_komdlgn-grkor = h_grkor.
      IF h_bsmng NE 0.
        it_komdlgn-kmpmg = it_ekpo_short-menge / h_bsmng.
      ENDIF.
    ENDIF.

*---
    IF it_ekpo_short-pstyp EQ '2'.
      it_komdlgn-sobkz = 'K'.
    ENDIF.
* Kontierungsfelder
    IF it_ekpo_short-sobkz EQ 'E' OR it_ekpo_short-sobkz EQ 'Q'.
      CALL FUNCTION 'MMPUR_EKKN_READ_EBELN_EBELP'
           EXPORTING
                pi_ebeln             = it_ekpo_short-ebeln
                pi_ebelp             = it_ekpo_short-ebelp
           TABLES
                pto_ekkn_po          = it_ekkn
           EXCEPTIONS
                no_records_requested = 1
                OTHERS               = 2.

      IF sy-subrc EQ 0.
        READ TABLE it_ekkn INDEX 1.
        it_komdlgn-ps_psp_pnr = it_ekkn-ps_psp_pnr.
        it_komdlgn-vbelv      = it_ekkn-vbeln.
        it_komdlgn-posnv      = it_ekkn-vbelp.
      ENDIF.
    ENDIF.

*--- others
    MOVE : '0005'        TO it_komdlgn-traty,
           wa_itab-traid TO it_komdlgn-traid,
           wa_itab-lifexpos TO it_komdlgn-lifexpos.

    MOVE : wa_itab-zinvoice TO it_komdlgn-bolnr.

*--- 2004/02/17 add by stlim
*--- add seal number
    MOVE : wa_itab-borgr_grp TO it_komdlgn-borgr_grp.
*---

* Begin of changes -  UD1K930445
* Populate BOL# in Delivery Header
    READ TABLE it_bol_inf WITH KEY traid = it_komdlgn-traid
                                   cinvo = it_komdlgn-bolnr
                                   BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_komdlgn-lifex = it_bol_inf-zfhblno.
    ENDIF.

* End of changes -  UD1K930445

    APPEND it_komdlgn.
    CLEAR : it_komdlgn.
  ENDLOOP.
ENDFORM.                    " komdlgn_fill
*&---------------------------------------------------------------------*
*&      Form  read_ekko
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKPO_SHORT_EBELN  text
*----------------------------------------------------------------------*
FORM read_ekko USING    p_it_9000_ebeln.
*---
  CALL FUNCTION 'ME_EKKO_SINGLE_READ'
       EXPORTING
            pi_ebeln         = p_it_9000_ebeln
       IMPORTING
            po_ekko          = ekko
       EXCEPTIONS
            no_records_found = 1
            OTHERS           = 2.
ENDFORM.                    " read_ekko
*&---------------------------------------------------------------------*
*&      Form  get_open_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKPO_SHORT_EBELN  text
*      -->P_IT_EKPO_SHORT_EBELP  text
*      -->P_WA_ITAB_ETA  text
*----------------------------------------------------------------------*
FORM get_open_qty USING    p_it_ekpo_short_ebeln
                           p_it_ekpo_short_ebelp
                           p_wa_itab-zreate.
*---
  DATA : h-meng1 LIKE ekes-dabmg,
         h-menge LIKE ekes-dabmg.

*---
  CLEAR : t163d.
  SELECT SINGLE * FROM t163d
                 WHERE ibtyp EQ c_ibtyp.

*---
  CLEAR : h-menge.
  SELECT * FROM ekes WHERE ebeln EQ p_it_ekpo_short_ebeln
                       AND ebelp EQ p_it_ekpo_short_ebelp
                       AND ebtyp EQ t163d-ebtyp.
    IF ekes-estkz EQ '4'.
      h-menge = h-menge - ekes-menge.
    ELSE.
      h-menge = h-menge + ekes-menge.
    ENDIF.
  ENDSELECT.

*---
  CLEAR : h-meng1.
  IF NOT p_wa_itab-zreate IS INITIAL.
    SELECT * FROM eket WHERE ebeln EQ p_it_ekpo_short_ebeln
                         AND ebelp EQ p_it_ekpo_short_ebelp
                         AND eindt LE p_wa_itab-zreate.
      h-meng1 = h-meng1 + eket-menge.
    ENDSELECT.
*    IF sy-subrc NE 0.
*      CLEAR eket.
*      SELECT SINGLE * FROM eket WHERE ebeln EQ p_it_ekpo_short_ebeln
*                                AND   ebelp EQ p_it_ekpo_short_ebelp.
*    ENDIF.
  ELSE.
*    CLEAR eket.
*    SELECT SINGLE * FROM eket WHERE ebeln EQ ekpo-ebeln
*                                AND   ebelp EQ ekpo-ebelp.
  ENDIF.

*---
  IF h-meng1 GT h-menge.
    w_qty_sum = h-meng1 - h-menge.
  ELSE.
    w_qty_sum = it_ekpo_short-menge - h-menge.
  ENDIF.
  IF w_qty_sum LT 0.
    w_qty_sum = 0.
  ENDIF.

**---
*  CALL FUNCTION 'ME_CONFIRMATION_ANL_QTY'
*       EXPORTING
*            i_ebeln = p_it_ekpo_short_ebeln
*            i_ebelp = p_it_ekpo_short_ebelp
*            i_eindt = p_wa_itab-zreate
*       TABLES
*            xeket   = it_xeket.
*
**---
*  LOOP AT it_xeket.
*    w_qty_sum = w_qty_sum + it_xeket-menge.
*  ENDLOOP.
ENDFORM.                    " get_open_qty
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function.
*---
  CLEAR : it_vbfs, it_vbfs[], it_vbls, it_vbls[], it_lips, it_lips[].

  CALL FUNCTION 'GN_DELIVERY_CREATE'
       EXPORTING
            vbsk_i   = st_vbsk
       TABLES
            xkomdlgn = it_komdlgn
            xvbfs    = it_vbfs
            xvbls    = it_vbls
            xxlips   = it_lips.
ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab.
  DATA: w_err_msg LIKE bapiret2-message,
        wa_return LIKE bapiret2,
        w_msgno LIKE bapiret2-number.

  LOOP AT it_komdlgn.
    READ TABLE it_lips WITH KEY vgbel = it_komdlgn-vgbel
                                vgpos = it_komdlgn-vgpos
                                matnr = it_komdlgn-matnr
                                lfimg = it_komdlgn-lfimg.
    IF sy-subrc EQ 0.
      MOVE : it_lips-vbeln TO it_9000-zmsg,
             'S'           TO it_9000-zresult,
             c_green       TO it_9000-linecolor.

      MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
             sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
             sy-uname TO it_9000-zbnam,     " User ID
             'C'      TO it_9000-zmode.     " Data Characteristic Flag

*---
      MODIFY it_9000 TRANSPORTING zbdat   zbtim zbnam zmode
                                  zresult zmsg  linecolor
                            WHERE traid EQ wa_itab-traid
                              AND ematn EQ wa_itab-ematn
                              AND ebeln EQ it_komdlgn-vgbel
                              AND matnr EQ it_komdlgn-matnr.

**--- insert by stlim (2004/05/11)
      UPDATE likp SET zzdepdt = wa_itab-lfdat_la
                      zzarrdt = wa_itab-eta
                WHERE vbeln EQ it_lips-vbeln.
      COMMIT WORK.
    ELSE.
      READ TABLE it_vbfs WITH KEY vbeln = it_komdlgn-vgbel
                                  posnr = it_komdlgn-vgpos
                                  msgv1 = it_komdlgn-matnr.
      IF sy-subrc EQ 0.
        w_msgno = it_vbfs-msgno.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            id                = it_vbfs-msgid
            number            = w_msgno
*       LANGUAGE          = SY-LANGU
            textformat        = 'NON'
*       LINKPATTERN       =
         message_v1        = it_vbfs-msgv1
         message_v2        = it_vbfs-msgv2
         message_v3        = it_vbfs-msgv3
         message_v4        = it_vbfs-msgv4
         IMPORTING
           message           = w_err_msg
           return            = wa_return .
*     TABLES
*       TEXT              =
        CLEAR: w_msgno.
        MOVE : w_err_msg     TO it_9000-zmsg,
               'E'           TO it_9000-zresult,
               c_red         TO it_9000-linecolor.
*---
        MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
               sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
               sy-uname TO it_9000-zbnam,     " User ID
               'C'      TO it_9000-zmode.     " Data Characteristic Flag
      ELSE.
        CLEAR: w_msgno.
        MOVE : text-m02      TO it_9000-zmsg,
               'E'           TO it_9000-zresult,
               c_red         TO it_9000-linecolor.
*---
        MOVE : sy-datum TO it_9000-zbdat,     " BDC Execute Date
               sy-uzeit TO it_9000-zbtim,     " BDC Execute Time
               sy-uname TO it_9000-zbnam,     " User ID
               'C'      TO it_9000-zmode.     " Data Characteristic Flag
      ENDIF.

      MODIFY it_9000 TRANSPORTING zbdat   zbtim zbnam zmode
                                  zresult zmsg  linecolor
                            WHERE traid EQ wa_itab-traid
                              AND ematn EQ wa_itab-ematn
                              AND ebeln EQ it_komdlgn-vgbel
                              AND matnr EQ it_komdlgn-matnr.
    ENDIF.
  ENDLOOP.



*
**---
*  READ TABLE it_lips INDEX 1.
*
**---
*  IF sy-subrc EQ 0.
*    MOVE : it_lips-vbeln TO IT_9000-zmsg,
*           'S'           TO IT_9000-zresult,
*           c_green       TO IT_9000-linecolor.
*
*    MOVE : sy-datum TO IT_9000-zbdat,     " BDC Execute Date
*           sy-uzeit TO IT_9000-zbtim,     " BDC Execute Time
*           sy-uname TO IT_9000-zbnam,     " User ID
*           'C'      TO IT_9000-zmode.     " Data Characteristic Flag
*
**---
*    MODIFY IT_9000 TRANSPORTING zbdat
*                                zbtim
*                                zbnam
*                                zmode
*                                zresult
*                                zmsg
*                                linecolor
*                                          WHERE traid EQ wa_itab-traid
*                                            AND ematn EQ wa_itab-ematn.
*
***--- insert by stlim (2004/05/11)
*    UPDATE likp SET zzdepdt = wa_itab-lfdat_la
*                    zzarrdt = wa_itab-eta
*              WHERE vbeln EQ it_lips-vbeln.
*    COMMIT WORK.
***--- end of insert
*  ELSE.
*    LOOP AT it_vbfs WHERE msgty = 'E'.
*      w_msgno = it_vbfs-msgno.
*      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
*        EXPORTING
*          id                = it_vbfs-msgid
*          number            = w_msgno
**       LANGUAGE          = SY-LANGU
*          textformat        = 'NON'
**       LINKPATTERN       =
*       message_v1        = it_vbfs-msgv1
*       message_v2        = it_vbfs-msgv2
*       message_v3        = it_vbfs-msgv3
*       message_v4        = it_vbfs-msgv4
*       IMPORTING
*         message           = w_err_msg
*         return            = wa_return .
**     TABLES
**       TEXT              =
*      CLEAR: w_msgno.
*      MOVE : w_err_msg     TO IT_9000-zmsg,
*             'E'           TO IT_9000-zresult,
*             c_red         TO IT_9000-linecolor.
**---
*      MOVE : sy-datum TO IT_9000-zbdat,     " BDC Execute Date
*             sy-uzeit TO IT_9000-zbtim,     " BDC Execute Time
*             sy-uname TO IT_9000-zbnam,     " User ID
*             'C'      TO IT_9000-zmode.     " Data Characteristic Flag
*
**---
*      MODIFY IT_9000 TRANSPORTING zbdat
*                                  zbtim
*                                  zbnam
*                                  zmode
*                                  zresult
*                                  zmsg
*                                  linecolor
*                                           WHERE traid EQ wa_itab-traid
*                                             AND ematn EQ wa_itab-ematn
  .
*    ENDLOOP.
*  ENDIF.
ENDFORM.                    " modify_itab
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      CLEAR: sy-ucomm.
      PERFORM execute_rtn.
    WHEN 'CHGN'.
      CLEAR: sy-ucomm.
      PERFORM change_po.
    WHEN 'REFR'.
      CLEAR: sy-ucomm.
      PERFORM refresh_po.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container.
  ASSIGN:      (w_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event.
  ELSE.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
  ENDIF.
ENDFORM.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container,
               'WC_CONTROL_'   p_dynnr INTO w_control,
               'WC_ALV_'       p_dynnr INTO w_alv.

  ASSIGN: (w_container) TO <container>,
          (w_control)   TO <control>,
          (w_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
         EXPORTING i_parent      = <container>
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'TRAID'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'EMATN'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ETA'         ' ',
                                  ' ' 'COLTEXT'     'ETA',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'EBELN'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZZKDWEBPO'   ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'LIFEXPOS'    ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'MEINS'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'BORGR_GRP'   ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'EXPVZ'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZRESULT'     ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZMSG'        ' ',
                                  'E' 'EMPHASIZE'   'C500'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv,
               c_structure  p_dynnr      INTO w_structure,
               'IT_'        p_dynnr '[]' INTO w_itab.

  ASSIGN: (w_alv)       TO <alv>,
          (w_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
     EXPORTING i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event.

ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0836   text
*      -->P_0837   text
*      -->P_0838   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO w_cnt.
      p_fieldcat-col_pos = w_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_rtn.
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  PERFORM set_target_data TABLES lt_rows.

  PERFORM create_delivery TABLES it_9000_tar.
  PERFORM update_table TABLES it_9000_tar.
  PERFORM count_rtn.
ENDFORM.                    " execute_rtn
*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM set_target_data TABLES pt_rows STRUCTURE lvc_s_row.
  CLEAR: it_9000_tar, it_9000_tar[].

* Begin of changes  - UD1K930445
*&--------------------------------------------------------------------&*
*& Since we have different length defined in the tables:
*&                   1. ZTMM_KD_ASN_MAIN.
*&                   2. ZTBLIT_INF.
*&have to create an intermediate table for efficent "SELECT" from table.
*& BOL won't be success until ASN is created.
*&--------------------------------------------------------------------&*
  LOOP AT it_9000 ASSIGNING <fs_kdasn>.
    wa_asn-traid = <fs_kdasn>-traid.
    wa_asn-cinvo = <fs_kdasn>-zinvoice.
    COLLECT wa_asn INTO it_asn.
  ENDLOOP.

  SELECT zfcivno zfcont
         zfhblno                                            "UD1K919888
    FROM ztblit_inf  INTO TABLE it_bol_inf
              FOR ALL ENTRIES IN it_asn
           WHERE zfcivno = it_asn-cinvo
           AND   zfcont  = it_asn-traid.
*                        and   zresult = 'S'.
*  if sy-subrc ne '0'.
*    MESSAGE e000(zz) WITH text-011.
*  endif.
  SORT it_bol_inf BY traid cinvo.

  LOOP AT it_9000 ASSIGNING <fs_kdasn>.
    READ TABLE it_bol_inf WITH KEY traid = <fs_kdasn>-traid
                                cinvo = <fs_kdasn>-zinvoice
                                              BINARY SEARCH
                                     TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      wa_9000 = <fs_kdasn>.
      wa_9000-zresult = 'E'.
      wa_9000-zmsg    = text-011.                           "TEXT-010
      APPEND wa_9000 TO it_err_9000.
      <fs_kdasn>-zresult = 'E'.
      <fs_kdasn>-zmsg = text-011.
    ELSE.
      IF r2 EQ 'X' .  "Reprocess
        <fs_kdasn>-zresult = ''.
        <fs_kdasn>-zmsg = ''.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  delete it_9000 where zresult = 'D'.

* End of changes - UD1K930445
  IF  p_update EQ ''.
    LOOP AT pt_rows WHERE index NE 0.
      READ TABLE it_9000 INDEX pt_rows-index.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m04.
      ENDIF.

      CHECK it_9000-icon EQ icon_red_light OR
            it_9000-icon EQ icon_yellow_light.

* If No BOL DATA Found - Don't create Delivery
      CHECK it_9000-zresult NE 'E'.
      MOVE it_9000 TO it_9000_tar.

      APPEND it_9000_tar.

    ENDLOOP.

  ELSE.

    LOOP AT  it_9000 .

*      CHECK it_9000-icon EQ icon_red_light OR
*            it_9000-icon EQ icon_yellow_light.

* If No BOL DATA Found - Don't create Delivery
      CHECK it_9000-zresult NE 'E'.
      MOVE it_9000 TO it_9000_tar.

      APPEND it_9000_tar.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  count_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn.
  CLEAR: w_total, w_success, w_error, w_ready.
  LOOP AT it_9000.
    CASE it_9000-zresult.
      WHEN 'S'.
        w_success = w_success + 1.
      WHEN 'E'.
        w_error = w_error + 1.
      WHEN space.
        w_ready = w_ready + 1.
    ENDCASE.

    w_total = w_total + 1.
  ENDLOOP.
ENDFORM.                    " count_rtn
*&---------------------------------------------------------------------*
*&      Form  change_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_po.
  DATA: it_rows TYPE lvc_t_row WITH HEADER LINE,
          it_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = it_rows[]
                     et_row_no     = it_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE it_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  CLEAR: it_9000_chpo, it_9000_chpo[].
  CLEAR: it_9100, it_9100[].

  LOOP AT it_rows WHERE index NE 0.
    READ TABLE it_9000 INDEX it_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m04.
    ENDIF.
    MOVE it_9000 TO it_9000_chpo.
    APPEND it_9000_chpo.
  ENDLOOP.
  LOOP AT it_9000_chpo.
    MOVE-CORRESPONDING it_9000_chpo TO it_9100.
    APPEND it_9100.
  ENDLOOP.
  CALL SCREEN 9100.
ENDFORM.                    " change_po

*&spwizard: declaration of tablecontrol 'TC_9100' itself
CONTROLS: tc_9100 TYPE TABLEVIEW USING SCREEN 9100.

*&spwizard: output module for tc 'TC_9100'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE tc_9100_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_9100 LINES tc_9100-lines.
ENDMODULE.

*&spwizard: input module for tc 'TC_9100'. do not change this line!
*&spwizard: modify table
MODULE tc_9100_modify INPUT.
  MODIFY it_9100
    INDEX tc_9100-current_line.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.

      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR sy-ucomm.
      PERFORM save_changed_po.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHANGED_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_changed_po.
  DELETE ztmm_kd_asn_main FROM TABLE it_9000_chpo.
  MODIFY ztmm_kd_asn_main FROM TABLE it_9100.
  MESSAGE s999 WITH text-s01.
ENDFORM.                    " SAVE_CHANGED_PO
*&---------------------------------------------------------------------*
*&      Form  refresh_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_po.
  PERFORM get_data.
  PERFORM display_data.
ENDFORM.                    " refresh_po
*&---------------------------------------------------------------------*
*&      Module  TC_9100_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_9100_change_field_attr OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'IT_9100-EBELN'.
      screen-input = 1.
    ELSE.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " TC_9100_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_BOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bol.

  DATA: BEGIN OF wa_ztblhd_log.
          INCLUDE STRUCTURE ztblhd_inf.
  DATA:    END OF wa_ztblhd_log.

  DATA: it_ztblhd_log LIKE TABLE OF wa_ztblhd_log WITH HEADER LINE.

  DATA: BEGIN OF it_zsblhd OCCURS 1000.
          INCLUDE STRUCTURE zsblhd_inf.
  DATA  END OF it_zsblhd.

  DATA: BEGIN OF it_zsblit OCCURS 1000.
          INCLUDE STRUCTURE zsblit_inf.
  DATA  END OF it_zsblit.


  SELECT DISTINCT a~zfblno b~zfcivno a~zfhblno
            a~zfmblno a~zfbldt a~zfshty
            a~zfvia a~zfvsl a~zf20ft a~zf40ft a~zf45ft a~zf40hq
            a~zfnewt a~zfnewtm a~zftovl a~zftovlm a~zfetd a~zfeta
            a~zfsprt a~zfaprt a~zfaprtc a~zfsprtc a~zuser a~zsdat
            a~zstim a~zedat a~zetim a~zmode a~zresult a~zmsg a~zzret
        INTO CORRESPONDING FIELDS OF TABLE it_ztblhd_log
        FROM   ztblhd_inf AS a INNER JOIN ztblit_inf AS b
         ON a~zfblno = b~zfblno
        WHERE  a~zedat        EQ   sy-datum AND
               a~zresult      EQ   'E'  .

  LOOP AT it_ztblhd_log.

* Create(B/L)
    CALL FUNCTION 'ZIM_WEB_TO_SAP_BL_TEST'
         EXPORTING
              zfdocno   = it_ztblhd_log-zfblno
         TABLES
              it_zsblhd = it_zsblhd
              it_zsblit = it_zsblit.
* If error found - skip and process next record
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " create_BOL
