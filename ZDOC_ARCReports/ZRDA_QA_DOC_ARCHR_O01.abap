*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCHR_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.

  SET PF-STATUS '0210'.
  CASE g_proc.
    WHEN 'B'.
      CASE sy-dynnr.
        WHEN '0210'.
***          SET PF-STATUS '0210'.
          SET TITLEBAR  '210'.
        WHEN '0215'.
          SET TITLEBAR  '215'.
        WHEN '0217'.
          SET PF-STATUS '0217'.
          SET TITLEBAR  '217'.
      ENDCASE.
    WHEN 'L'.
      CASE sy-dynnr.
        WHEN '0220'.
          SET TITLEBAR  '220'.
        WHEN '0215'.
          SET TITLEBAR  '225'.
        WHEN '0227'.
          SET TITLEBAR  '217'.
          SET TITLEBAR  '227'.
      ENDCASE.
    WHEN 'C'.
      CASE sy-dynnr.
        WHEN '0230'.
          SET TITLEBAR  '230'.
        WHEN '0215'.
          SET TITLEBAR  '235'.
        WHEN '0237'.
          SET TITLEBAR  '217'.
          SET TITLEBAR  '237'.
      ENDCASE.
    WHEN 'R'.
      CASE sy-dynnr.
        WHEN '0230'.
          SET TITLEBAR  '240'.
        WHEN '0215'.
          SET TITLEBAR  '245'.
        WHEN '0237'.
          SET TITLEBAR  '217'.
          SET TITLEBAR  '247'.
      ENDCASE.
    WHEN 'V'.
      CASE sy-dynnr.
        WHEN '0250'.
          SET TITLEBAR  '250'.
        WHEN '0215'.
          SET TITLEBAR  '255'.
        WHEN '0257'.
          SET TITLEBAR  '217'.
          SET TITLEBAR  '257'.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " STATUS_0210  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  INIT_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0210 OUTPUT.

***  IF is_buyback-aclonly IS NOT INITIAL OR
***     g_aclnoonly        IS NOT INITIAL.
***    LOOP AT SCREEN.
***      IF screen-group4 = 'GR1'.
***        screen-active = 0.
***      ENDIF.
***      MODIFY SCREEN.
***    ENDLOOP.
***  ENDIF.
  CASE g_proc.
    WHEN 'C'.
      g_claimtitle = gc_claim.
      WRITE gc_claim to g_claimtitle.
    WHEN 'R'.
      g_claimtitle = gc_reclaim.
      WRITE gc_reclaim to g_claimtitle.
    WHEN 'V'.
      CASE sy-dynnr.
        WHEN '0230'.
      ENDCASE.
      LOOP AT SCREEN.
        IF screen-group4 = 'GR1'.
          screen-input = 0.
        ENDIF.
*        IF g_first IS INITIAL.
*          CASE 'X'.
*            WHEN cb1.
*              IF screen-group4 = 'CB2' OR screen-group4 = 'CB3' OR
*                 screen-group4 = 'CB4'.
*                screen-active = 0.
*              ENDIF.
*            WHEN cb2.
*              IF screen-group4 = 'CB1' OR screen-group4 = 'CB3' OR
*                 screen-group4 = 'CB4'.
*                screen-active = 0.
*              ENDIF.
*            WHEN cb3.
*              IF screen-group4 = 'CB1' OR screen-group4 = 'CB2' OR
*                 screen-group4 = 'CB4'.
*                screen-active = 0.
*              ENDIF.
*            WHEN cb4.
*              IF screen-group4 = 'CB1' OR screen-group4 = 'CB2' OR
*                 screen-group4 = 'CB3'.
*                screen-active = 0.
*              ENDIF.
*          ENDCASE.
*        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      g_first = 'X'.
  ENDCASE.

  IF g_fldname IS NOT INITIAL.
    SET CURSOR FIELD g_fldname.
  ENDIF.

ENDMODULE.                 " INIT_0210  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  ALV_PROCESS_0215  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_process_0215 OUTPUT.

  IF ct_0215 IS INITIAL.

*----Creating custom container instance
    CREATE OBJECT ct_0215
      EXPORTING
        container_name = 'CT_0215'.

*----Creating ALV Grid instance
    CREATE OBJECT g_grid1
      EXPORTING
        i_parent = ct_0215.
*        i_appl_events = 'X'.     " ALV  PAI -> PBO

*// Create an Instance of ALV Control
*    CREATE OBJECT g_alvgrid
*      EXPORTING
*        i_parent      = g_docking_container.
*        i_appl_events = 'X'.            " Fix variant

*--- Variant
    PERFORM set_variant1         CHANGING gs_variant1.
*--- Layout
    PERFORM set_layout1          CHANGING gs_layout1.
*--- SORT & SUBTOTAL
    PERFORM sort_subtotal1       CHANGING gt_sort1.
*--- Exclude Toolbar useless
    PERFORM set_toolbar1_exclude CHANGING gt_exclude1.
*--- Field Category
    PERFORM set_fieldcat1.
*--- Register F4 Field & Event
    PERFORM register_f4_field.
*--- Event Register
    PERFORM set_event_handler1.
*--- Cell color style
    PERFORM set_cellattribute.
*--- Drop Down
    PERFORM set_build_drdn CHANGING it_data[].
    PERFORM set_dropdown.

    CALL METHOD g_grid1->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZVDA_ARCH03L'
        is_layout            = gs_layout1
        it_toolbar_excluding = gt_exclude1[]
        i_save               = 'A'  " U
        i_default            = 'X'
        is_variant           = gs_variant1
      CHANGING
        it_sort              = gt_sort1[]
        it_fieldcatalog      = gt_fieldcat1[]
        it_outtab            = it_data[].
  ELSE.
    CALL METHOD g_grid1->refresh_table_display
      EXPORTING
        is_stable = gs_stable1.         " Refresh
  ENDIF.

*--- Registering Edit Event         " As soon as data changed
  CALL METHOD g_grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDMODULE.                 " ALV_PROCESS_0215  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  STATUS_0215  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0215 OUTPUT.

  SET PF-STATUS '215'.
  CASE g_proc.
    WHEN 'B'.
      SET TITLEBAR  '215'.
    WHEN 'L'.
      SET TITLEBAR  '225'.
    WHEN 'C'.
      SET TITLEBAR  '235'.
    WHEN 'R'.
      SET TITLEBAR  '245'.
    WHEN 'V'.
      SET TITLEBAR  '255'.
  ENDCASE.

ENDMODULE.                 " STATUS_0215  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  STATUS_0217  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0217 OUTPUT.

  SET PF-STATUS '0217'.
  CASE g_proc.
    WHEN 'B'. "Buyback
      SET TITLEBAR  '217'.
    WHEN 'L'. "Legal
      SET TITLEBAR  '227'.
    WHEN 'C'. "Claim
      SET TITLEBAR  '237'.
    WHEN 'R'. "Reclaim
      SET TITLEBAR  '247'.
    WHEN 'V'. "Vendor Files
      SET TITLEBAR  '257'.
  ENDCASE.

ENDMODULE.                 " STATUS_0217  OUTPUT
