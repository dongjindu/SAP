************************************************************************
* Program Name      : ZRIT_LOGON_SYNC_HR
* Creation Date     : 02/08/13
* Development Request No :
* Addl Documentation:
* Description       : Sync SAP logon with HR data
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrit_logon_sync_hr NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: usr02.

DATA: BEGIN OF it_data OCCURS 0,
      bname LIKE usr02-bname,
      pernr LIKE pa0000-pernr,
      kostl LIKE pa0001-kostl,
      email LIKE bapiaddr3-e_mail,
      gltgb LIKE bapilogond-gltgb,
      accnt LIKE bapilogond-accnt,
      end_update(1),
      attr_update(1),
      err_update(1),
      err_msg(255),
      END OF it_data.

*DATA: IT_ERROR LIKE TABLE OF IT_DATA WITH HEADER LINE.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE   i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bname FOR usr02-bname.
PARAMETERS: p_bukrs LIKE t001-bukrs DEFAULT 'H201' OBLIGATORY,
            p_valid AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: p_test AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM get_data.

  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH 'No data found'.
  ELSE.
    IF p_test IS INITIAL.
      PERFORM update_data.
    ELSE.
      PERFORM display_data.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: BEGIN OF lt_pernr OCCURS 0,
        bname LIKE usr02-bname,
*        pernr like pa0000-pernr,
        gltgb LIKE usr02-gltgb,
        END OF lt_pernr.

  DATA: l_stat2 LIKE pa0000-stat2,
        l_end_date LIKE pa0000-begda,
        l_pernr LIKE pa0000-pernr,
        l_bapibname LIKE bapibname-bapibname.

  DATA: lw_logondata LIKE bapilogond,
        LW_BAPIDEFAUL LIKE BAPIDEFAUL,
        lt_adsmtp LIKE TABLE OF bapiadsmtp WITH HEADER LINE.

  DATA: lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.

  REFRESH it_data.

  IF p_valid IS INITIAL.
    SELECT bname gltgb
      INTO TABLE lt_pernr
      FROM usr02
      WHERE bname IN s_bname.
  ELSE.
    SELECT bname gltgb
      INTO TABLE lt_pernr
      FROM usr02
      WHERE bname IN s_bname
        AND ( gltgb = '00000000' OR
            gltgb > sy-datum ).
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT lt_pernr.
      REFRESH:  lt_adsmtp, lt_return.
      CLEAR: l_stat2, l_end_date, l_pernr,
      lw_logondata .
      l_pernr = lt_pernr-bname.
      l_bapibname = lt_pernr-bname.

      SELECT SINGLE stat2 begda
        INTO (l_stat2, l_end_date)
        FROM pa0000
        WHERE pernr = l_pernr
          AND endda = '99991231'.

      IF sy-subrc = 0.
        IF l_stat2 = '0'.
** retired user
          IF lt_pernr-gltgb <> l_end_date.
            it_data-end_update = 'X'.
            it_data-gltgb = l_end_date.
            it_data-pernr = l_pernr.
            it_data-bname = lt_pernr-bname.
          ENDIF.
        ENDIF.

** Get info from HR data
        SELECT SINGLE usrid_long INTO it_data-email
          FROM pa0105
          WHERE pernr = l_pernr
          AND subty = '0010'.

        SELECT SINGLE kostl INTO it_data-kostl
          FROM pa0001
          WHERE pernr = l_pernr.

** get sap login use info

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username       = l_bapibname
*           CACHE_RESULTS  = 'X'
          IMPORTING
            logondata      = lw_logondata
            DEFAULTS       = LW_BAPIDEFAUL
*           ADDRESS        =
*           COMPANY        =
*           SNC            =
*           REF_USER       =
*           ALIAS          =
*           UCLASS         =
*           LASTMODIFIED   =
*           ISLOCKED       =
          TABLES
*           PARAMETER      =
*           PROFILES       =
*           ACTIVITYGROUPS =
            return         = lt_return
*           ADDTEL         =
*           ADDFAX         =
*           ADDTTX         =
*           ADDTLX         =
            addsmtp        = lt_adsmtp
*           ADDRML         =
*           ADDX400        =
*           ADDRFC         =
*           ADDPRT         =
*           ADDSSF         =
*           ADDURI         =
*           ADDPAG         =
*           ADDCOMREM      =
*           PARAMETER1     =
*           GROUPS         =
*           UCLASSSYS      =
*           EXTIDHEAD      =
*           EXTIDPART      =
*           SYSTEMS        =
          .

** check
        CLEAR:  lt_adsmtp.
        READ TABLE  lt_adsmtp WITH KEY e_mail = it_data-email.
        IF  lt_adsmtp-e_mail IS INITIAL
            OR  lt_adsmtp-e_mail <> it_data-email
            OR lw_logondata-accnt <> it_data-kostl
            OR LW_BAPIDEFAUL-KOSTL <> it_data-kostl+2(8).
          it_data-attr_update = 'X'.
          it_data-pernr = l_pernr.
          it_data-bname = lt_pernr-bname.
          it_data-attr_update = 'X'.
        ENDIF.
        IF it_data-end_update = 'X'
           OR it_data-attr_update = 'X'.
          APPEND it_data.
        ENDIF.
      ELSE.
        REFRESH lt_return.
        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username  = l_bapibname
          IMPORTING
            logondata = lw_logondata
            DEFAULTS  = LW_BAPIDEFAUL
          TABLES
            return    = lt_return.
*            addsmtp   =  lt_ADSMTP.

        IF sy-subrc = 0. " AND lw_logondata-accnt IS INITIAL.
          IF lt_pernr-bname+0(3) = 'HIS'.
            it_data-kostl = 'HISNA'.
          ELSEIF lt_pernr-bname+0(1) = 'T'.
            it_data-kostl = 'TEMP'.
          ELSEIF lt_pernr-bname+0(2) = 'A1'.
            it_data-kostl = 'VENDOR'.
          ELSEIF lt_pernr-bname+0(1) = 'P'.
            it_data-kostl = 'GLOVIS'.
          ELSEIF lt_pernr-bname+0(3) = 'SAP'.
            it_data-kostl = 'SAP'.
          ELSEIF lt_pernr-bname+0(4) = 'DDIC'.
            it_data-kostl = 'SAP'.
          ELSEIF lt_pernr-bname = 'TMSADM'.
            it_data-kostl = 'SAP'.
          ELSEIF lt_pernr-bname = 'EARLYWATCH'.
            it_data-kostl = 'SAP'.
          ELSEIF lt_pernr-bname = 'ALEREMOTE'.
            it_data-kostl = 'SAP'.
          ELSEIF lt_pernr-bname = 'WF-BATCH'.
            it_data-kostl = 'SAP'.
         ELSEIF lt_pernr-bname = 'HMMABACKUSER'.
            it_data-kostl = 'SAP'.
          ELSE.
            it_data-kostl = 'OTHERS'.
          ENDIF.

          it_data-pernr = lt_pernr-bname.
          it_data-bname = lt_pernr-bname.
          it_data-attr_update = 'X'.
          APPEND it_data.
        ENDIF.
      ENDIF.
      CLEAR: it_data.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'BNAME'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'User ID',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'KOSTL'       ' ',
                                  ' ' 'COLTEXT'     'Cost Center',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'GLTGB'       ' ',
                                  ' ' 'COLTEXT'     'End Dater',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ATTR_UPDATE'  ' ',
                                  ' ' 'COLTEXT'     'Update',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'END_UPDATE'       ' ',
                                  ' ' 'COLTEXT'     'Lock',
                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'EMAIL'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Email',
                                  'E' 'OUTPUTLEN'   '60'.

ENDFORM.                    "build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
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
      MESSAGE e000(zz) WITH 'Check field catalog'.
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
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
*     i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[]
      it_sort              = it_sort[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_data .
  DATA: lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_name LIKE bapibname-bapibname,
        lw_logondata LIKE bapilogond,
        lw_logondatax LIKE bapilogonx,
        lw_address LIKE bapiaddr3,
        lw_addressx LIKE bapiaddr3x,
        lw_defaults LIKE bapidefaul,
        lw_defaultsx LIKE bapidefax.

  LOOP AT it_data.

*    IF it_data-attr_update IS NOT INITIAL.
    REFRESH lt_return.
    CLEAR: lt_return, lw_logondata, lw_logondatax,
           lw_address,lw_addressx,  lw_defaults, lw_defaultsx.

    l_name = it_data-bname.

    IF it_data-gltgb IS NOT INITIAL.
      lw_logondata-gltgb = it_data-gltgb.
      lw_logondatax-gltgb = 'X'.
    ENDIF.

    lw_logondata-accnt = it_data-kostl.
    lw_logondatax-accnt = 'X'.

      LW_DEFAULTS-KOSTL = it_data-KOSTL+2(8).
      LW_DEFAULTSX-KOSTL = 'X'.

    lw_address-pers_no = l_name.
    lw_addressx-pers_no = 'X'.
    if it_data-email is not INITIAL.
       lw_address-e_mail = it_data-email.
       lw_addressx-e_mail = 'X'.
    endif.

    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username   = l_name
        logondata  = lw_logondata
        logondatax = lw_logondatax
       DEFAULTS   =  LW_DEFAULTS
        DEFAULTSX  =  LW_DEFAULTSX
        address    = lw_address
        addressx   = lw_addressx
      TABLES
        return     = lt_return.

    READ TABLE lt_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      it_data-err_update = 'E'.
      it_data-err_msg = lt_return-message.
      ROLLBACK WORK.
    ELSE.
      it_data-err_update = 'S'.
      COMMIT WORK.
    ENDIF.
*   ENDIF.
** lock the user
*    IF it_data-gltgb <= sy-datum and
*       it_data-end_update = 'X'..
*
*      REFRESH lt_return.
*      CLEAR: lt_return.
*      l_name = it_data-bname.
*      CALL FUNCTION 'BAPI_USER_LOCK'
*        EXPORTING
*          username = l_name
*        TABLES
*          return   = lt_return.
*
*      READ TABLE lt_return WITH KEY type = 'E'.
*      IF sy-subrc = 0.
*        it_data-err_update = 'E'.
*        it_data-err_msg = lt_return-message.
*      ELSE.
*        it_data-err_update = 'S'.
*      ENDIF.
*    ENDIF.
    MODIFY it_data TRANSPORTING err_update err_msg.
  ENDLOOP.
**
ENDFORM.                    " UPDATE_DATA
