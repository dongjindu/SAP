*----------------------------------------------------------------------
* Program ID        : ZACOU148
* Title             : [CO] - Maintain Table ZTCO_NAFTA
* Created on        : 4/29/2010
* Created by        : Valerian Utama
* Specifications By : Michael Yoon
* Description       : [CO] - Maintain Table ZTCO_NAFTA
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer    Description
* 4/29/2010  VALERIAN     Initial Coding
*            HIS20094
*&--------------------------------------------------------------------&*
REPORT zacou148.
TYPE-POOLS: slis.

TABLES: ztco_nafta.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,

      BEGIN OF gt_out OCCURS 0.
        INCLUDE STRUCTURE ztco_nafta.
DATA: sel(1)   TYPE c,
      END OF gt_out.

DATA: gt_out_save TYPE ztco_nafta OCCURS 0 WITH HEADER LINE.

SELECT-OPTIONS:
  s_kokrs FOR ztco_nafta-kokrs DEFAULT 'H201',
  s_bdatj FOR ztco_nafta-bdatj,
  s_poper FOR ztco_nafta-poper,
  s_artnr FOR ztco_nafta-artnr,
  s_verid FOR ztco_nafta-verid,
  s_werks FOR ztco_nafta-werks,
  s_compn FOR ztco_nafta-compn,
  s_indx  FOR ztco_nafta-indx.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM fieldcat_init    USING  gt_fieldcat[].
  PERFORM alv_grid_display TABLES gt_out.

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

*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.

  DELETE ft_extab WHERE fcode = '&DATA_SAVE '.
  SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL'
                 EXCLUDING ft_extab.

ENDFORM.                    "PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.

  DATA: BEGIN OF t_data_sel OCCURS 0.
          INCLUDE STRUCTURE ztco_nafta.
  DATA  END OF t_data_sel.

  CASE fp_ucomm.
    WHEN '&DATA_SAVE'.
      LOOP AT gt_out WHERE sel = 'X'.

        READ TABLE gt_out_save WITH KEY kokrs = gt_out-kokrs
                                        bdatj = gt_out-bdatj
                                        poper = gt_out-poper
                                        artnr = gt_out-artnr
                                        verid = gt_out-verid
                                        werks = gt_out-werks
                                        compn = gt_out-compn
                                        indx  = gt_out-indx
                                        BINARY SEARCH.

        IF sy-subrc EQ 0 AND
           gt_out_save-lifnr NE gt_out-lifnr.

          gt_out-aedat = sy-datum.
          gt_out-aenam = sy-uname.
          MODIFY gt_out TRANSPORTING aedat aenam.

          MOVE-CORRESPONDING gt_out TO t_data_sel.
          APPEND t_data_sel.
        ENDIF.
      ENDLOOP.

      IF t_data_sel[] IS INITIAL.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  textline1 = text-m01.
      ELSE.
        UPDATE ztco_nafta FROM TABLE t_data_sel.
        gt_out_save[] = gt_out[].
        SORT gt_out_save.

        LOOP AT gt_out WHERE sel = 'X'.
          CLEAR gt_out-sel.
          MODIFY gt_out TRANSPORTING sel.
        ENDLOOP.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  textline1 = text-m02.

        fs-refresh = 'X'.
      ENDIF.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA: gt_listheader TYPE slis_t_listheader,
        gs_listheader TYPE slis_listheader,

        lt_usr21 TYPE usr21,
        lt_adrp  TYPE adrp.

  SELECT SINGLE * FROM usr21 INTO lt_usr21
    WHERE bname = sy-uname.

  IF sy-subrc EQ 0.
    SELECT * FROM adrp INTO lt_adrp
    UP TO 1 ROWS
    WHERE persnumber = lt_usr21-persnumber
      AND date_from LT sy-datum
      AND date_to   GE sy-datum.
    ENDSELECT.
  ENDIF.

  gs_listheader-typ  = 'S'.
  gs_listheader-key  = 'User:'.
  CONCATENATE sy-uname '-' lt_adrp-name_first
                           lt_adrp-namemiddle
                           lt_adrp-name_last
              INTO gs_listheader-info SEPARATED BY space.
  CONDENSE gs_listheader-info.

  APPEND gs_listheader TO gt_listheader.

  gs_listheader-typ  = 'S'.
  gs_listheader-key  = 'Date:'.
  WRITE sy-datum TO gs_listheader-info.

  APPEND gs_listheader TO gt_listheader.

  gs_listheader-typ = 'A'.
  gs_listheader-key = 'DESCR'.
  gs_listheader-info = text-t01.
  APPEND gs_listheader TO gt_listheader.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING fieldcat TYPE slis_t_fieldcat_alv.
  DATA: l_repid TYPE sy-repid,
        ls_fieldcat TYPE slis_fieldcat_alv.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = l_repid
            i_structure_name       = 'ZTCO_NAFTA'
            i_client_never_display = 'X'
       CHANGING
            ct_fieldcat            = fieldcat
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE fieldcat INTO ls_fieldcat
                      WITH KEY fieldname = 'LIFNR'.
  IF sy-subrc EQ 0.
    ls_fieldcat-edit = 'X'.
    MODIFY fieldcat FROM ls_fieldcat INDEX sy-tabix
                    TRANSPORTING edit.
  ENDIF.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  SELECT * INTO TABLE gt_out
  FROM ztco_nafta
  WHERE kokrs IN s_kokrs
    AND bdatj IN s_bdatj
    AND poper IN s_poper
    AND artnr IN s_artnr
    AND verid IN s_verid
    AND werks IN s_werks
    AND compn IN s_compn
    AND indx  IN s_indx.

  IF gt_out[] IS INITIAL.
    MESSAGE s208(00) WITH 'Data Not Found'.
    STOP.
  ENDIF.

  SORT gt_out.
  gt_out_save[] = gt_out[].
ENDFORM.                    " get_data
