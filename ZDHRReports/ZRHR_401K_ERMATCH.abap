*----------------------------------------------------------------------
* Program ID        : ZRHR_401K_ERMATCH
* Title             : [HR] 401K ER Matching Tracking Report
* Created on        : 08/12/14
* Created by        : Furong
* Specifications By :
* Description       :
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Issue No   Description
*
*&--------------------------------------------------------------------&*

REPORT zrhr_401k_ermatch MESSAGE-ID zmco.

TABLES: pernr, pa0001.
INFOTYPES: 0000,
           0001,0002.

DATA: BEGIN OF it_data OCCURS 0,
      pernr LIKE pernr-pernr,
      ename LIKE pa0001-ename,
      fpbeg LIKE sy-datum,
      fpend LIKE sy-datum,
      wt8337 LIKE pc207-betrg,
      wt102  LIKE pc207-betrg,
      cnpct  LIKE pc207-betrg,
      lmpct  LIKE pc207-betrg,
      missed LIKE pc207-betrg,
      regular  LIKE pc207-betrg,
      END OF it_data.

DATA: ls_pa0041 TYPE pa0041,
      ls_pa0041_ze TYPE pa0041,
      lv_date_revised  TYPE dats,
      lv_date_expected TYPE dats,
      l_field(20),
      l_num(2)  TYPE n.

DATA: lv_molga TYPE molga.
DATA: in_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE.
DATA: ls_rgdir LIKE LINE OF in_rgdir,
      lv_relid LIKE pcl2-relid,
      lv_seqnr LIKE pc261-seqnr,
      ls_result TYPE pay99_result.

DATA: ls_rt LIKE pc207 OCCURS 0 WITH HEADER LINE.
DATA: lv_8337  TYPE pc207-betrg,
      lv_102   TYPE pc207-betrg,
      lv_contb TYPE pc207-betrg,
      lv_limit TYPE pc207-betrg,
      lv_ercst TYPE pc207-betrg,
      lv_cnpct LIKE t74ff-cnpct,
      lv_lmpct LIKE t74ff-lmpct.

DATA: l_er_limit_step TYPE pc207-betrg.

DATA: l_endda LIKE sy-datum.

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
      w_cnt   TYPE i.

RANGES: r_datum FOR sy-datum.

FIELD-SYMBOLS: <fs_expected> TYPE any.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

START-OF-SELECTION.

GET pernr.

  PROVIDE * FROM p0001 BETWEEN sy-datum AND sy-datum.
    it_data-pernr = pernr-pernr.

* Check if TM has a date type 'ZD' in current PA0041.
    SELECT SINGLE * FROM pa0041 INTO ls_pa0041
       WHERE pernr = pernr-pernr
         AND ( dar01 = 'ZD' OR dar02 = 'ZD' OR dar03 = 'ZD'
               OR dar04 = 'ZD' OR dar05 = 'ZD' OR dar06 = 'ZD'
               OR dar07 = 'ZD' OR dar08 = 'ZD' OR dar09 = 'ZD'
               OR dar10 = 'ZD' OR dar11 = 'ZD' OR dar12 = 'ZD' ).
    IF sy-subrc = 0.
      SELECT SINGLE * FROM pa0041 INTO ls_pa0041_ze
      WHERE pernr = pernr-pernr
       AND ( dar01 = 'ZE' OR dar02 = 'ZE' OR dar03 = 'ZE'
          OR dar04 = 'ZE' OR dar05 = 'ZE' OR dar06 = 'ZE'
          OR dar07 = 'ZE' OR dar08 = 'ZE' OR dar09 = 'ZE'
          OR dar10 = 'ZE' OR dar11 = 'ZE' OR dar12 = 'ZE' ).

      CHECK sy-subrc = 0.
*Read the endda date of the pay period in which the missing match needs
*to be paid.


      DO 12 TIMES.
        l_num = sy-index.
        CLEAR l_field.

        CONCATENATE 'LS_PA0041_ZE-DAR' l_num INTO l_field.
        ASSIGN (l_field) TO <fs_expected>.

        IF sy-subrc EQ 0 AND <fs_expected> EQ 'ZE'.
          UNASSIGN <fs_expected>.
          CONCATENATE 'LS_PA0041-DAT' l_num INTO l_field.
          ASSIGN (l_field) TO <fs_expected>.
          IF sy-subrc EQ 0.
            l_endda = <fs_expected>.
            UNASSIGN <fs_expected>.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.

* Read MATCH_DATE_EXPECTED
      DO 12 TIMES.
        l_num = sy-index.
        CLEAR l_field.

        CONCATENATE 'LS_PA0041-DAR' l_num INTO l_field.
        ASSIGN (l_field) TO <fs_expected>.

        IF sy-subrc EQ 0 AND <fs_expected> EQ 'ZD'.
          UNASSIGN <fs_expected>.
          CONCATENATE 'LS_PA0041-DAT' l_num INTO l_field.
          ASSIGN (l_field) TO <fs_expected>.
          IF sy-subrc EQ 0.
            lv_date_expected = <fs_expected>.
            UNASSIGN <fs_expected>.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.

* Read Payroll Control Records
      CLEAR lv_molga.
      CALL FUNCTION 'CU_READ_RGDIR'
        EXPORTING
          persnr          = pernr-pernr
        IMPORTING
          molga           = lv_molga
        TABLES
          in_rgdir        = in_rgdir
        EXCEPTIONS
          no_record_found = 1
          OTHERS          = 2.

* Delete voided payroll data.
      DELETE in_rgdir WHERE voidr NE space.
      DELETE in_rgdir WHERE srtza NE 'A'. "Active

* Get MATCH_DATE_REVISED
      DELETE in_rgdir WHERE fpend <= lv_date_expected
                         OR fpend+0(6) = lv_date_expected+0(6).
*READ TABLE in_rgdir INDEX 1.
*lv_date_revised = in_rgdir-fpbeg.


* Cluster id for US
* Personnel Country Grouping
      CLEAR lv_relid.
      SELECT SINGLE relid INTO lv_relid
                    FROM t500l
                    WHERE molga = lv_molga.
      IF   lv_relid IS INITIAL.
        lv_relid = 'RU'.
      ENDIF.


*** Looping For ER Match
      LOOP AT in_rgdir INTO ls_rgdir.
        lv_seqnr = ls_rgdir-seqnr.

* Read Payroll cluster Data for each payroll control record
        CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
          EXPORTING
            clusterid                    = lv_relid
            employeenumber               = pernr-pernr
            sequencenumber               = lv_seqnr
            read_only_international      = 'X'
          CHANGING
            payroll_result               = ls_result
          EXCEPTIONS
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            error_reading_archive        = 8
            error_reading_relid          = 9
            OTHERS                       = 10.

*  clear : ws_8387, ws_401k,lv_8337, ws_bc31, ws_gsal .
        IF ls_rgdir-fpend NE l_endda.
          READ TABLE ls_result-inter-rt INTO ls_rt WITH KEY lgart =
          '8387'.
          CHECK sy-subrc NE 0.
          READ TABLE ls_result-inter-rt INTO ls_rt WITH KEY lgart =
          '8337'.
          CHECK sy-subrc EQ 0.
        ENDIF.

        it_data-ename = pernr-ename.
        it_data-fpbeg = ls_rgdir-fpbeg.
        it_data-fpend = ls_rgdir-fpend.

* ER Matching
        LOOP AT ls_result-inter-rt INTO ls_rt
          WHERE lgart = '8337' OR lgart = '/102'.
          CASE ls_rt-lgart.
            WHEN '8337'.
              lv_8337 = abs( ls_rt-betrg ).
            WHEN '/102'.
              lv_102 = abs( ls_rt-betrg ).
          ENDCASE.
        ENDLOOP.

        it_data-wt8337 = lv_8337.
        it_data-wt102 = lv_102.

*  IF ls_rgdir-fpend >= '20110321'.
*    lv_contb = lv_8337.
*    lv_limit = lv_102 * ( 4 / 100 ).
*  ELSEIF ls_rgdir-fpend >= '20100419' AND ls_rgdir-fpend < '20110321'.
*    lv_contb = lv_8337 * ( 80 / 100 ).
*    lv_limit = lv_102 * ( 32 / 1000 ).
*  ELSEIF ls_rgdir-fpend >= '20061218' AND ls_rgdir-fpend < '20100419'.
*    lv_contb = lv_8337 * ( 60 / 100 ).
*    lv_limit = lv_102 * ( 24 / 1000 ).
*  ELSEIF ls_rgdir-fpend >= '20040501' AND ls_rgdir-fpend < '20061218'.
*    lv_contb = lv_8337 * ( 60 / 100 ).
*    lv_limit = lv_102 * ( 18 / 1000 ).
*  ELSE.
**    ERROR
*  ENDIF.

        SELECT SINGLE cnpct INTO lv_cnpct
           FROM t74ff
          WHERE barea = '10'
            AND bplan = '401K'
            AND begda <= ls_rgdir-fpend
            AND endda >= ls_rgdir-fpend.

        SELECT SINGLE lmpct INTO lv_lmpct
        FROM t74ff
       WHERE barea = '10'
         AND bplan = '401K'
         AND begda <= ls_rgdir-fpend
         AND endda >= ls_rgdir-fpend.

        lv_contb = lv_8337 * ( lv_cnpct / 100 ).
        lv_limit = lv_102 * ( lv_lmpct  / 100 ).

        lv_ercst = lv_contb.
        IF lv_limit <= lv_contb.
          lv_ercst = lv_limit.
        ENDIF.

        IF ls_rgdir-fpend NE l_endda.
          it_data-missed = lv_ercst.
        ELSE.
          it_data-regular = lv_ercst.
        ENDIF.
        it_data-cnpct = lv_cnpct.
        it_data-lmpct = lv_lmpct.

        it_data-pernr = pernr-pernr.
        APPEND it_data.
        CLEAR: lv_ercst, it_data, lv_8337,lv_102,lv_contb,
               lv_limit,lv_cnpct,lv_lmpct.

*  ADD lv_ercst TO _ercst.

      ENDLOOP.

* If ZE is in the future, then the regular ER match is unknown.
      IF ls_rgdir-fpend < l_endda.
        it_data-pernr = pernr-pernr.
        it_data-ename = pernr-ename.
        it_data-fpbeg = l_endda - 13.
        it_data-fpend = l_endda.

        it_data-regular = 9999999.
        APPEND it_data.
        CLEAR: it_data, lv_ercst,lv_8337,lv_102,
               lv_contb,lv_limit,lv_cnpct,lv_lmpct.
      ENDIF.
    ENDIF.
    CLEAR: ls_rgdir, ls_pa0041, ls_pa0041_ze, l_endda, lv_date_expected.

  ENDPROVIDE.

END-OF-SELECTION.

  PERFORM dispaly_output.

*
*&---------------------------------------------------------------------*
*&      Form  DISPALY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dispaly_output .
  CALL SCREEN 0800.
ENDFORM.                    " DISPALY_OUTPUT

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
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
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

                                  'S' 'PERNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Team #',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ENAME'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Name',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'FPBEG'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Begin Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'FPEND'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'End Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'WT8337'       ' ',
                                  ' ' 'COLTEXT'     '8337 Amount',
                                  'E' 'OUTPUTLEN'   '14',

                                  'S' 'WT102'       ' ',
                                  ' ' 'COLTEXT'     '102 Amount',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'CNPCT'       ' ',
                                  ' ' 'COLTEXT'     'ER Percent',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'LMPCT'       ' ',
                                  ' ' 'COLTEXT'     'ER Limit Percent',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'MISSED'       ' ',
                                  ' ' 'COLTEXT'     'Missed ER',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'REGULAR'       ' ',
                                  ' ' 'COLTEXT'     'Regular ER',
                                  'E' 'OUTPUTLEN'   '10'.

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
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
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
