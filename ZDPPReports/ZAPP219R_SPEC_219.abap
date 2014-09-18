************************************************************************
* Program Name      : ZAPP219R_SPEC_219
* Author            : Woon-mook Choi
* Creation Date     : 2003.08.04.
* Specifications By : Woon-mook Choi
* Development Request No :
* Type :
* Addl Documentation:
* Description       : Spec mgmt. - 219 Option Item display             *
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/27/2004 Tonkey       UD1K906384   Changed Table Relationship
*
*
************************************************************************
REPORT zapp219r_spec_219  MESSAGE-ID zmpp.
*
TABLES: ztbm_abxopvdt,
        ztbm_option_item,
        ztpp_veh_model.
*-------------------------------------------------------
INCLUDE <icon>.
INCLUDE: zapp000l_alv_top,    "ALV TOP
         zapp000l_alv_fob.    "ALV FOB
INCLUDE <list>.
*-------------------------------------------------------
DATA: BEGIN OF IT_APP219  OCCURS 0.             " from ztbm_219_desc
DATA:   model TYPE ztpp_veh_model-MODEL,   "Model
        name_219 TYPE ztbm_abxopvdt-clno,  "Column No.
        desc_219 TYPE ztbm_abxopvdt-clnm.  "Column Name
*        include structure  ztbm_219_desc.
DATA: END OF  IT_APP219.
*
DATA: BEGIN OF IT_OPT_APP219  OCCURS 0.          " from IT_APP219
        INCLUDE STRUCTURE  ztbm_option_item.
DATA    chkbox   TYPE  c.               "Checkbox
DATA: END OF  IT_OPT_APP219.

* 219 option table control screen display
DATA: BEGIN OF IT_OPT1_APP219  OCCURS  0,
        model    LIKE  ztbm_option_item-model,
        zcolumn  LIKE  ztbm_option_item-zcolumn,
        zcomment LIKE  ztbm_option_item-zcomment,
      END OF IT_OPT1_APP219.
DATA: BEGIN OF IT_OPT2_APP219  OCCURS  0,
        model    LIKE  ztbm_option_item-model,
        zcolumn  LIKE  ztbm_option_item-zcolumn,
        zcomment LIKE  ztbm_option_item-zcomment,
      END OF IT_OPT2_APP219.
*
DATA: G_IT_LINE_APP219    TYPE  i,  "internal table lines
      G_OPT1_LINES_APP219 TYPE  i,
      G_OPT2_LINES_APP219 TYPE  i.

CONTROLS: TC_APP219_01 TYPE TABLEVIEW USING SCREEN 300.
CONTROLS: TC_APP219_02 TYPE TABLEVIEW USING SCREEN 300.
* selection screens
*
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME.
PARAMETERS :  p_model  LIKE      ztbm_option_item-model
                                 OBLIGATORY
                                 MATCHCODE OBJECT zsh_veh_model.
SELECTION-SCREEN  END OF BLOCK blk1.
SELECTION-SCREEN  BEGIN OF BLOCK blk3 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11)  text-001.
SELECTION-SCREEN POSITION 33.
PARAMETERS : p_alv  RADIOBUTTON GROUP radi.  "ALV FORMAT
SELECTION-SCREEN  COMMENT 36(8)  text-002.
SELECTION-SCREEN POSITION 50.
PARAMETERS : p_tab  RADIOBUTTON GROUP radi.  "TABLE CONTROL
SELECTION-SCREEN  COMMENT 53(8)  text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN  END OF BLOCK blk3.
* -------------------------------------------------------------------
AT SELECTION-SCREEN ON p_model.
  SELECT SINGLE  * FROM  ztpp_veh_model
    WHERE model = p_model.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Not registered Vehicle Model !'.
    EXIT.
  ENDIF.
* -------------------------------------------------------------------
INITIALIZATION.
  g_repid = sy-repid.

* -------------------------------------------------------------------
START-OF-SELECTION.

  SELECT DISTINCT vh~model ab~clno ab~clnm
    INTO (IT_APP219-model, IT_APP219-name_219, IT_APP219-desc_219)
    FROM  ztbm_abxopvdt AS ab
            INNER JOIN ztpp_veh_model AS vh ON ab~carx = vh~model02
    WHERE  vh~model  EQ  p_model.

    APPEND IT_APP219.

  ENDSELECT.
*  select * from  ztbm_219_desc
*    into corresponding fields of table IT_APP219
*    where  model  eq  p_model.

  DESCRIBE TABLE  IT_APP219  LINES  G_IT_LINE_APP219.

  IF  G_IT_LINE_APP219  IS  INITIAL.
    MESSAGE i003 WITH 'There is no correct data to condition.'.
    EXIT.
  ENDIF.
  REFRESH: IT_OPT_APP219.
  LOOP AT IT_APP219.
    CLEAR IT_OPT_APP219.
    MOVE  IT_APP219-model    TO    IT_OPT_APP219-model.
    MOVE  IT_APP219-name_219 TO    IT_OPT_APP219-zcolumn.
    MOVE  IT_APP219-desc_219 TO    IT_OPT_APP219-zcomment.
    APPEND  IT_OPT_APP219.
  ENDLOOP.
* ------------------------------------------------------------------
END-OF-SELECTION.
  IF p_alv EQ 'X'.
    PERFORM  build_events.
    PERFORM  build_fieldcat    USING  'IT_OPT_APP219'.
    PERFORM  build_layout      USING  'X'   space   space.
* ALV FUNCTION CALL
    PERFORM  start_grid_viewer TABLES  IT_OPT_APP219.
  ENDIF.

  IF p_tab EQ 'X'.
    REFRESH: IT_OPT1_APP219, IT_OPT2_APP219.
    CLEAR  : IT_OPT1_APP219, IT_OPT2_APP219.

    LOOP AT IT_OPT_APP219.
      IF sy-tabix < 111.
        MOVE-CORRESPONDING IT_OPT_APP219 TO IT_OPT1_APP219.
        APPEND IT_OPT1_APP219.
      ELSE.
        MOVE-CORRESPONDING IT_OPT_APP219 TO IT_OPT2_APP219.
        APPEND IT_OPT2_APP219.
      ENDIF.
    ENDLOOP.
*
    DESCRIBE TABLE  IT_OPT1_APP219  LINES  G_OPT1_LINES_APP219.
    DESCRIBE TABLE  IT_OPT2_APP219  LINES  G_OPT2_LINES_APP219.

    CALL  SCREEN  0300.

  ENDIF.
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       Setting PF Status
*---------------------------------------------------------------------*
*  -->  P_RT_EXTAB                                                    *
*---------------------------------------------------------------------*
FORM pf_status_set USING p_rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZAPP219R'.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       Setting Commands
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  DATA : " sel_field like selfield-sel_tab_field,
         l_gubun   TYPE c.
*
*  case ucomm.
*    when '&IC1'.
*      perform detail_dataview.
*    when 'PRINT'.
*      perform sf_print_zsf_rqm001 using 'PRNT'.
*    when 'PREVIEW'.
*      perform sf_print_zsf_rqm001 using 'PREVIEW'.
*    when others.
*  endcase.

ENDFORM.                    " USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Creating field catalog
*----------------------------------------------------------------------*
*      -->P_0048   text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_intab.

  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc.

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*   MODEL TEXT FIELD CATALOG SETTING
                                  'S' 'MODEL'       ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '05',
                                  ' ' 'KEY'         'X',
                                  'E' 'SELTEXT_M'   'MODEL',
*   COLUMN FIELD CATALOG SETTING
                                  'S' 'ZCOLUMN'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'S',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'COLUMN',
*   COMMENT FIELD CATALOG SETTING
                                  'S' 'ZCOMMENT'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '25',
                                  ' ' 'FIX_COLUMN'  ' ',
                                  'E' 'SELTEXT_M'   'COMMENT'.
ENDFORM.                    " build_fieldcat

INCLUDE zapp219r_spec_219_pai.

INCLUDE zapp219r_spec_219_pbo.
