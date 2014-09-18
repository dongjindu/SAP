*----------------------------------------------------------------------
* Program ID        : ZACOU147
* Title             : [C0] MP Allocation Program
* Created on        : 3/19/2010
* Created by        : Valerian Utama
* Specifications By : Michael Yoon
* Description       : [C0] MP Allocation Program
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Request    Description
* 03/19/2010 VALERIAN   UD1K948687 Initial Coding
*            HIS20094
* 09/13/2010 VALERIAN   UD1K949809 Add Elantra's FSC and Component Sel.
* 12/13/2011 KDM        UD1K953483 Chnage BAPI header return logic.
*                                  --> so add clear internal table.
*&--------------------------------------------------------------------&*
REPORT zacou147.

CONSTANTS: c_plant      TYPE mard-werks VALUE 'P001',
           c_sloc       TYPE mard-lgort VALUE 'X551',
           c_sonata_old TYPE mara-prdha VALUE '0000100001',
           c_sonata_new TYPE mara-prdha VALUE '0000100004',
           c_santa_fe   TYPE mara-prdha VALUE '0000100002',
           c_elantra    TYPE mara-prdha VALUE '0000100006', "UD1K949809
           c_lf_sonata  TYPE mara-prdha VALUE '0000100007'.
* ALV Data
TYPE-POOLS: slis.
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_sort     TYPE slis_t_sortinfo_alv,
       gs_sort     TYPE slis_sortinfo_alv.

* Program Data
DATA: BEGIN OF t_product OCCURS 0,
        matnr   TYPE ztco_wip-matnr,
        version TYPE ztco_wip-version,
        outqty  TYPE ztco_wip-outqty,
        prdha   TYPE mara-prdha,
        confunit TYPE ztco_wip-confunit,
      END OF t_product.

DATA: BEGIN OF t_component OCCURS 0,
        matnr TYPE mara-matnr,
        labst TYPE mard-labst,
        maktg TYPE makt-maktg,
      END OF t_component.

DATA: BEGIN OF t_data OCCURS 0,
        prod     TYPE ztco_wip-matnr,
        version  TYPE ztco_wip-version,
        outqty   TYPE ztco_wip-outqty,
        outpct   TYPE p DECIMALS 1,
        comp     TYPE mara-matnr,
        labst    TYPE mard-labst,
        compqty  TYPE p DECIMALS 0,
        confunit TYPE ztco_wip-confunit,
        sel(1)   TYPE c,
      END OF t_data.

DATA: BEGIN OF t_data_result OCCURS 0.
        INCLUDE STRUCTURE t_data.
DATA:   msg(132) TYPE c,
        led(1)   TYPE c,
      END OF t_data_result.

DATA: v_postdate TYPE sy-datum,
      v_prod_tot TYPE ztco_wip-outqty,
      v_month    TYPE isellist-month.

* Parameters and Select-options
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-t01.
PARAMETERS: p_perio TYPE ztco_wip-yymm DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: p_mod1 RADIOBUTTON GROUP mode,
            p_mod2 RADIOBUTTON GROUP mode,
            p_mod3 RADIOBUTTON GROUP mode,                  "UD1K949809
            p_mod4 RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK sel.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_perio.
  v_month = sy-datum.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
       EXPORTING
            actual_month               = v_month
            start_column               = 42
            start_row                  = 2
       IMPORTING
            selected_month             = v_month
       EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            month_not_found            = 3
            OTHERS                     = 4.

  IF sy-subrc EQ 0 AND NOT v_month IS INITIAL.
    p_perio = v_month.
  ENDIF.

AT SELECTION-SCREEN.
  IF p_perio CA sy-abcde.
    MESSAGE e368(00) WITH text-m05 ' '.
  ELSEIF p_perio = 0.
    MESSAGE e368(00) WITH text-m02 ' '.
  ENDIF.

START-OF-SELECTION.
* Get Data from database
  PERFORM get_data.

* Process data
  PERFORM process_data.

* Display data
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_grid_display  TABLES t_data.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'PROD'      'Product'        18  'CHAR' '' '',
    'X'  'VERSION'   'Version'         4  'CHAR' '' '',
    ' '  'OUTQTY'    'Total'          15  'QUAN' '' '',
    ' '  'OUTPCT'    '% Product'      15  'QUAN' '' '',
    ' '  'COMP'      'Component'      18  'CHAR' '' '',
    ' '  'COMPQTY'   'Tot.Comp'       15  'QUAN' '' ''.

  READ TABLE ft_fieldcat INTO gs_fieldcat
         WITH KEY fieldname = 'COMPQTY'.
  IF sy-subrc EQ 0.
    gs_fieldcat-do_sum = 'X'.
    gs_fieldcat-qfieldname = 'CONFUNIT'.
    MODIFY ft_fieldcat FROM gs_fieldcat INDEX sy-tabix
           TRANSPORTING do_sum qfieldname.
  ENDIF.

  READ TABLE ft_fieldcat INTO gs_fieldcat
       WITH KEY fieldname = 'OUTQTY'.
  IF sy-subrc EQ 0.
    gs_fieldcat-qfieldname = 'CONFUNIT'.
    MODIFY ft_fieldcat FROM gs_fieldcat INDEX sy-tabix
           TRANSPORTING qfieldname.
  ENDIF.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.
  gs_layout-box_fieldname = 'SEL'.
  gs_layout-box_tabname = 'FT_OUTTAB'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = l_repid
            i_callback_pf_status_set = 'PF_STATUS_SET'
            i_callback_user_command  = 'USER_COMMAND'
            i_callback_top_of_page   = 'TOP_OF_PAGE'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat
            it_sort                  = gt_sort
       TABLES
            t_outtab                 = ft_outtab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-down      = &4.
    gs_sort-group     = &5.
    gs_sort-subtot    = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
            'OUTQTY'  ' ' ' ' 'X' ' ' ' ',
            'PROD'    ' ' 'X' ' ' ' ' ' ',
            'VERSION' ' ' 'X' ' ' ' ' 'X'.

ENDFORM.                    " SORT_BUILD

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.

  DATA: BEGIN OF t_data_sel OCCURS 0.
          INCLUDE STRUCTURE t_data.
  DATA  END OF t_data_sel.

  DATA: BEGIN OF t_data_post OCCURS 0.
          INCLUDE STRUCTURE t_data.
  DATA  END OF t_data_post.

  DATA: BEGIN OF l_data_post.
          INCLUDE STRUCTURE t_data.
  DATA  END OF l_data_post.

  DATA: ls_return LIKE bapiret2,
        lt_heads TYPE TABLE OF bapi_ppc_apoheads,
        lt_complists  TYPE TABLE OF bapi_ppc_apocomplists,
        ls_complists  TYPE bapi_ppc_apocomplists,
        lt_actlists   TYPE TABLE OF bapi_ppc_apoactlists,
        ls_actlists   TYPE bapi_ppc_apoactlists,
        ls_head       TYPE ppc_va_head.

  CASE fp_ucomm.
    WHEN '&DATA_SAVE'.
      LOOP AT t_data WHERE sel = 'X'
                       AND compqty <> 0.

        APPEND t_data TO t_data_sel.
      ENDLOOP.

      IF t_data_sel[] IS INITIAL.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  textline1 = text-m01.
        EXIT.
      ELSE.
        SORT t_data_sel BY outqty DESCENDING prod version
                            labst DESCENDING comp.
      ENDIF.

      LOOP AT t_data_sel.
        AT NEW version.
          CLEAR t_data_post.
          REFRESH t_data_post.
        ENDAT.

        APPEND t_data_sel TO t_data_post.

        AT END OF version.

* Post the Variance
          LOOP AT t_data_post.
            l_data_post = t_data_post.

            AT NEW version.
              CALL FUNCTION 'GUID_CREATE'
                   IMPORTING
                        ev_guid_32 = ls_head-headid.

* ... fill additional information
              GET TIME STAMP FIELD ls_head-conf_time.
              MOVE v_postdate TO ls_head-pstng_date.
              MOVE sy-uname TO ls_head-conf_username.
              CLEAR ls_head-flg_reversal.
              MOVE '3' TO ls_head-flg_info_dest.

              ls_head-head_matnr = l_data_post-prod.
              ls_head-prodplant  = c_plant.
              ls_head-version    = l_data_post-version.
              APPEND ls_head TO lt_heads.
            ENDAT.

            ls_complists-confquant = t_data_post-compqty.
            MOVE ls_complists-confquant
            TO ls_complists-delta_confquant.

            ls_complists-confunit = t_data_post-confunit.
            ls_complists-material = t_data_post-comp.
            ls_complists-plant    = c_plant.
            ls_complists-storageloc = c_sloc.
            MOVE ls_head-headid TO ls_complists-headid.
            MOVE '0' TO ls_complists-goodsmove_ind.
            MOVE 'X' TO ls_complists-sync_ind.
            APPEND ls_complists TO lt_complists.

            AT END OF version.
              CLEAR ls_return.

              CALL FUNCTION 'BAPI_MNFCTCONFRCVR_RECEIVE'
                   IMPORTING
                        return          = ls_return
                   TABLES
                        it_apoheads     = lt_heads
                        it_apocomplists = lt_complists
                        it_apoactlists  = lt_actlists.

              IF ls_return IS INITIAL.
                MESSAGE s022(ppc1va) INTO t_data_result-msg.
                t_data_result-led = '3'.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
              ELSE.
                MESSAGE ID ls_return-id
                      TYPE ls_return-type
                    NUMBER ls_return-number
                      WITH ls_return-message_v1
                           ls_return-message_v2
                           ls_return-message_v3
                           ls_return-message_v4
                      INTO t_data_result-msg.
                t_data_result-led = '1'.
              ENDIF.

              LOOP AT t_data_post.
                MOVE-CORRESPONDING t_data_post TO t_data_result.
                APPEND t_data_result.
              ENDLOOP.

              CLEAR : ls_return, ls_head.              ""UD1K953483
              CLEAR : ls_complists, ls_actlists.       ""UD1K953483
              REFRESH : lt_heads, lt_complists, lt_actlists. ""UD1K953483
            ENDAT.
          ENDLOOP.

        ENDAT.
      ENDLOOP.

      PERFORM display_data2.
      fs-exit = 'X'.                   "Exit the first screen
  ENDCASE.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.

  DELETE ft_extab WHERE fcode = '&DATA_SAVE '.
  SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL'
                 EXCLUDING ft_extab.

ENDFORM.                    "PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data2 .
  PERFORM fieldcat_init2     USING gt_fieldcat[].
  PERFORM sort_build2        USING gt_sort[].
  PERFORM alv_grid_display2  TABLES t_data_result.
ENDFORM.                    " DISPLAY_DATA2

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init2 USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DESCRIBE TABLE ft_fieldcat LINES l_pos.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    ' '  'MSG'       'Message'         132  'CHAR' '' ''.

ENDFORM.                    " fieldcat_init2

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display2 TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.
  gs_layout-confirmation_prompt = 'X'.

  gs_layout-lights_fieldname = 'LED'.
  gs_layout-lights_tabname   = 'FT_OUTTAB'.
  gs_layout-lights_rollname  = 'CATSSTATUS'.

  CLEAR:
  gs_layout-box_fieldname,
  gs_layout-box_tabname.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program     = l_repid
            i_callback_top_of_page = 'TOP_OF_PAGE'
            is_layout              = gs_layout
            it_fieldcat            = gt_fieldcat
            it_sort                = gt_sort
       TABLES
            t_outtab               = ft_outtab
       EXCEPTIONS
            program_error          = 1
            OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY2

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build2 USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
    'MSG'    ' ' 'X' ' ' ' ',
    'LED'    ' ' 'X' ' ' ' '.

ENDFORM.                    " SORT_BUILD2

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get Data from database
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
* Get Material Data (Components)
  CASE 'X'.
    WHEN p_mod1.                       "Sonata Model is selected

* Get Sonata Product (full specification)
      SELECT ztco_wip~matnr
             ztco_wip~version
             ztco_wip~outqty
             mara~prdha
             ztco_wip~confunit

             INTO CORRESPONDING FIELDS OF TABLE t_product

             FROM ztco_wip
             JOIN mara
               ON ztco_wip~matnr = mara~matnr

             WHERE ztco_wip~yymm = p_perio
               AND gubun = 'F'
               AND plant = c_plant
               AND ( mara~prdha = c_sonata_old
                OR   mara~prdha = c_sonata_new ).

      IF t_product[] IS INITIAL.
        MESSAGE e368(00) WITH text-m03 ' '.
        LEAVE PROGRAM.
      ENDIF.

* Get Sonata Component
      SELECT mard~matnr
             mard~labst
             makt~maktg

             INTO CORRESPONDING FIELDS OF TABLE t_component

             FROM mard
             JOIN makt
               ON mard~matnr = makt~matnr

*           WHERE mard~matnr LIKE 'MP%'                     "UD1K949809
            WHERE ( mard~matnr LIKE 'MP______A%' OR         "UD1K949809
                    mard~matnr LIKE 'MP______Q%' OR         "UD1K949809
                    mard~matnr LIKE 'MP______S%' )          "UD1K949809
              AND werks = c_plant
              AND lgort = c_sloc
              AND makt~spras = sy-langu
              AND makt~maktg NOT LIKE '%CM%'.

      IF t_component[] IS INITIAL.
        MESSAGE e368(00) WITH text-m04 ' '.
        LEAVE PROGRAM.
      ENDIF.

    WHEN p_mod2.                       "Santa Fe Model is selected

* Get Santa Fe Product (full specification)
      SELECT ztco_wip~matnr
             ztco_wip~version
             ztco_wip~outqty
             mara~prdha
             ztco_wip~confunit

             INTO CORRESPONDING FIELDS OF TABLE t_product

             FROM ztco_wip
             JOIN mara
               ON ztco_wip~matnr = mara~matnr

             WHERE ztco_wip~yymm = p_perio
               AND gubun = 'F'
               AND plant = c_plant
               AND mara~prdha = c_santa_fe.

      IF t_product[] IS INITIAL.
        MESSAGE e368(00) WITH text-m03 ' '.
        LEAVE PROGRAM.
      ENDIF.

* Get Santa Fe Component
      SELECT mard~matnr
             mard~labst
             makt~maktg

             INTO CORRESPONDING FIELDS OF TABLE t_component

             FROM mard
             JOIN makt
               ON mard~matnr = makt~matnr

            WHERE mard~matnr LIKE 'MP%'
              AND werks = c_plant
              AND lgort = c_sloc
              AND makt~spras = sy-langu
              AND makt~maktg LIKE '%CM%'.

      IF t_component[] IS INITIAL.
        MESSAGE e368(00) WITH text-m04 ' '.
        LEAVE PROGRAM.
      ENDIF.

* BEGIN OF UD1K949809
    WHEN p_mod3.                       "Elantra Model is selected

* Get Elantra Product (full specification)
      SELECT ztco_wip~matnr
             ztco_wip~version
             ztco_wip~outqty
             mara~prdha
             ztco_wip~confunit

             INTO CORRESPONDING FIELDS OF TABLE t_product

             FROM ztco_wip
             JOIN mara
               ON ztco_wip~matnr = mara~matnr

             WHERE ztco_wip~yymm = p_perio
               AND gubun = 'F'
               AND plant = c_plant
               AND mara~prdha = c_elantra.

      IF t_product[] IS INITIAL.
        MESSAGE e368(00) WITH text-m03 ' '.
        LEAVE PROGRAM.
      ENDIF.

* Get Elantra Component
      SELECT mard~matnr
             mard~labst
             makt~maktg

             INTO CORRESPONDING FIELDS OF TABLE t_component

             FROM mard
             JOIN makt
               ON mard~matnr = makt~matnr

            WHERE ( mard~matnr LIKE 'MP______X%' OR
                    mard~matnr LIKE 'MP______Y%' )
              AND werks = c_plant
              AND lgort = c_sloc
              AND makt~spras = sy-langu
              AND makt~maktg NOT LIKE '%CM%'.

      IF t_component[] IS INITIAL.
        MESSAGE e368(00) WITH text-m04 ' '.
        LEAVE PROGRAM.
      ENDIF.

* END OF UD1K949809

** Furong on 07/15/14 for LF Sonata (
  WHEN p_mod4.                       "Sonata Model is selected

* Get Sonata Product (full specification)
      SELECT ztco_wip~matnr
             ztco_wip~version
             ztco_wip~outqty
             mara~prdha
             ztco_wip~confunit
             INTO CORRESPONDING FIELDS OF TABLE t_product
             FROM ztco_wip
             JOIN mara
               ON ztco_wip~matnr = mara~matnr
             WHERE ztco_wip~yymm = p_perio
               AND gubun = 'F'
               AND plant = c_plant
               AND mara~prdha = c_lf_sonata.

      IF t_product[] IS INITIAL.
        MESSAGE e368(00) WITH text-m03 ' '.
        LEAVE PROGRAM.
      ENDIF.

* Get Sonata Component
      SELECT mard~matnr
             mard~labst
             makt~maktg
             INTO CORRESPONDING FIELDS OF TABLE t_component
             FROM mard
             JOIN makt
               ON mard~matnr = makt~matnr
             WHERE mard~matnr LIKE 'MP_____C%'
              AND werks = c_plant
              AND lgort = c_sloc
              AND makt~spras = sy-langu
              AND makt~maktg NOT LIKE '%CM%'.

      IF t_component[] IS INITIAL.
        MESSAGE e368(00) WITH text-m04 ' '.
        LEAVE PROGRAM.
      ENDIF.
** )

  ENDCASE.


* Get posting date
  CONCATENATE p_perio '01' INTO v_postdate.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = v_postdate
       IMPORTING
            last_day_of_month = v_postdate.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Process data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: v_compqty_tot TYPE p DECIMALS 0.

  SORT: t_product   BY outqty DESCENDING matnr version,
        t_component BY labst  DESCENDING matnr.

  CLEAR v_prod_tot.
  LOOP AT t_product.
    v_prod_tot = v_prod_tot + t_product-outqty.
  ENDLOOP.

  LOOP AT t_product.
    MOVE-CORRESPONDING t_product TO t_data.
    t_data-prod = t_product-matnr.
    LOOP AT t_component.
      MOVE-CORRESPONDING t_component TO t_data.
      t_data-comp = t_component-matnr.
      t_data-outpct = t_product-outqty / v_prod_tot * 100.
      t_data-compqty = t_product-outqty * t_component-labst
                                        / v_prod_tot.
      APPEND t_data.
    ENDLOOP.
  ENDLOOP.

* Make correction to adjust rounding factor
** Adjust if total component lower than stock.
  SORT t_data BY outqty DESCENDING prod version
                  labst DESCENDING comp.

  LOOP AT t_component.
    CLEAR v_compqty_tot.
    LOOP AT t_data WHERE comp = t_component-matnr.
      v_compqty_tot = v_compqty_tot + t_data-compqty.
    ENDLOOP.
    IF t_component-labst NE v_compqty_tot.
      v_compqty_tot = t_component-labst - v_compqty_tot.

      LOOP AT t_data WHERE comp = t_component-matnr
                       AND compqty = 0.

        IF v_compqty_tot GT 0.
          t_data-compqty = t_data-compqty + 1.
          v_compqty_tot = v_compqty_tot - 1.
          MODIFY t_data INDEX sy-tabix.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

      WHILE v_compqty_tot GT 0.
        LOOP AT t_data WHERE comp = t_component-matnr.
          IF v_compqty_tot GT 0.
            t_data-compqty = t_data-compqty + 1.
            v_compqty_tot = v_compqty_tot - 1.
            MODIFY t_data INDEX sy-tabix.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDWHILE.

    ENDIF.
  ENDLOOP.

** Adjust if total component higher than stock.
  SORT t_data BY outqty ASCENDING  prod version
                  labst DESCENDING comp.
  LOOP AT t_component.
    CLEAR v_compqty_tot.
    LOOP AT t_data WHERE comp = t_component-matnr.
      v_compqty_tot = v_compqty_tot + t_data-compqty.
    ENDLOOP.
    IF t_component-labst NE v_compqty_tot.
      v_compqty_tot = v_compqty_tot - t_component-labst.

      LOOP AT t_data WHERE comp = t_component-matnr
                       AND compqty GT 0.

        IF v_compqty_tot GT 0.
          t_data-compqty = t_data-compqty - 1.
          v_compqty_tot = v_compqty_tot - 1.
          MODIFY t_data INDEX sy-tabix.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

      WHILE v_compqty_tot GT 0.
        LOOP AT t_data WHERE comp = t_component-matnr.
          IF v_compqty_tot GT 0.
            t_data-compqty = t_data-compqty + 1.
            v_compqty_tot = v_compqty_tot - 1.
            MODIFY t_data INDEX sy-tabix.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDWHILE.

    ENDIF.
  ENDLOOP.

  SORT t_data BY outqty DESCENDING prod version
                  labst DESCENDING comp.

  FREE: t_product, t_component.
ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA: gt_listheader TYPE slis_t_listheader,
        gs_listheader TYPE slis_listheader.

  gs_listheader-typ = 'S'.
  gs_listheader-key = 'Period:'.
  gs_listheader-info = p_perio.
  APPEND gs_listheader TO gt_listheader.

  gs_listheader-typ = 'S'.
  gs_listheader-key = 'Model:'.

  CASE 'X'.
    WHEN p_mod1.
      gs_listheader-info = 'Sonata'.
    WHEN p_mod2.
      gs_listheader-info = 'Santa Fe'.
    WHEN p_mod3.
      gs_listheader-info = 'Elantra'.                       "UD1K949809
    WHEN p_mod4.
      gs_listheader-info = 'LF Sonata'.
  ENDCASE.

  APPEND gs_listheader TO gt_listheader.

  IF sy-ucomm IS INITIAL.
    gs_listheader-typ = 'A'.
    gs_listheader-key = 'DESCR'.
    gs_listheader-info = text-t02.
    APPEND gs_listheader TO gt_listheader.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
