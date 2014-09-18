************************************************************************
* Program Name      : ZACO41R_NMHCC
* Author            : Byung Sung Bae
* Creation Date     : 2005.05.12
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902723
* Addl Documentation:
* Description       : This program is copied from ZACO05R_MHCC
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zaco05r_mhcc MESSAGE-ID zmco.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** TYPE-POOLS
TYPE-POOLS: vrm, slis.

** Tables
TABLES : ztco_nmhpcpost.

** Internal Table
DATA : it_ztco_nmhpcpost LIKE STANDARD TABLE OF ztco_nmhpcpost
                        WITH HEADER LINE .

** Global Variables


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
  PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY   ID cac OBLIGATORY,
               p_gjahr LIKE anlp-gjahr   MEMORY   ID gjr OBLIGATORY.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) text-002.
*   From Period.
  PARAMETERS: p_frper LIKE rku01g-perab OBLIGATORY.
  SELECTION-SCREEN COMMENT 52(05) text-003.
*   To Period.
  PARAMETERS: p_toper LIKE rku01g-perbi OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: p_selcon   AS LISTBOX VISIBLE LENGTH 20
                         DEFAULT '0' OBLIGATORY .

  SELECTION-SCREEN END OF BLOCK bl1.

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-004.
  SELECT-OPTIONS : s_mhdoc FOR ztco_nmhpcpost-mhdoc
                    MATCHCODE OBJECT zsh_co_mhdoc.
  SELECT-OPTIONS : s_erdat FOR ztco_nmhpcpost-erdat,
                   s_erzet FOR ztco_nmhpcpost-erzet,
                   s_ernam FOR ztco_nmhpcpost-ernam,
                   s_aedat FOR ztco_nmhpcpost-aedat,
                   s_aezet FOR ztco_nmhpcpost-aezet,
                   s_aenam FOR ztco_nmhpcpost-aenam.
  SELECTION-SCREEN END OF BLOCK bl2.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Show ListBox
  PERFORM show_list_box.

AT SELECTION-SCREEN.
* Check period range
  PERFORM check_period_range.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* read data
  PERFORM read_data_fr_ztco_nmhpcpost.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV Grid Control.
  PERFORM  call_alv_grid_control.


*
*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SHOW_LIST_BOX
*&---------------------------------------------------------------------*
*       List Box
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_list_box.
  DATA: lv_name       TYPE vrm_id,
        it_l_list       TYPE vrm_values,
        lv_value      LIKE LINE OF it_l_list.

  lv_name = 'P_SELCON'.

  lv_value-key  = '0'.
  lv_value-text = 'Total'.
  APPEND lv_value TO it_l_list.

  lv_value-key  = '1'.
  lv_value-text = 'Posted'.
  APPEND lv_value TO it_l_list.

  lv_value-key  = '2'.
  lv_value-text = 'Not Posted'.
  APPEND lv_value TO it_l_list.

  lv_value-key  = '3'.
  lv_value-text = 'Reversed'.
  APPEND lv_value TO it_l_list.

  lv_value-key  = '4'.
  lv_value-text = 'Not Reversed'.
  APPEND lv_value TO it_l_list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = lv_name
            values = it_l_list.
ENDFORM.                    " SHOW_LIST_BOX

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_range.
  IF p_frper > p_toper.
    MESSAGE e031.
  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_ZTCO_nMHPCPOST
*&---------------------------------------------------------------------*
*       Read data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fr_ztco_nmhpcpost.

  CLEAR : it_ztco_nmhpcpost, it_ztco_nmhpcpost[].

  CASE  p_selcon.
    WHEN '0'. " 'Total'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
               FROM ztco_nmhpcpost
              WHERE gjahr = p_gjahr
                AND perid BETWEEN p_frper and p_toper
* Common
                and  MHDOC in S_MHDOC
                AND  erdat IN s_erdat
                AND  erzet IN s_erzet
                AND  ernam IN s_ernam
                AND  aedat IN s_aedat
                AND  aezet IN s_aezet
                AND  aenam IN s_aenam .

    WHEN '1' OR '4' . " 'Posted / Not reversed'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
               FROM ztco_nmhpcpost
              WHERE gjahr = p_gjahr
                AND perid BETWEEN p_frper and p_toper
                and RUECK ne SPACE
                AND rmzhl NE space
                AND reversed EQ space
* Common
                AND  mhdoc IN s_mhdoc
                AND  erdat IN s_erdat
                AND  erzet IN s_erzet
                AND  ernam IN s_ernam
                AND  aedat IN s_aedat
                AND  aezet IN s_aezet
                AND  aenam IN s_aenam .

    WHEN '2'. " 'Not Posted'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
               FROM ztco_nmhpcpost
              WHERE gjahr = p_gjahr
                AND perid BETWEEN p_frper and p_toper
                and RUECK eq SPACE
                AND rmzhl EQ space
                AND reversed EQ space
* Common
                AND  mhdoc IN s_mhdoc
                AND  erdat IN s_erdat
                AND  erzet IN s_erzet
                AND  ernam IN s_ernam
                AND  aedat IN s_aedat
                AND  aezet IN s_aezet
                AND  aenam IN s_aenam .

    WHEN '3'. " 'Reversed'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
               FROM ztco_nmhpcpost
              WHERE gjahr = p_gjahr
                AND perid BETWEEN p_frper and p_toper
                and REVERSED ne SPACE
* Common
                AND  mhdoc IN s_mhdoc
                AND  erdat IN s_erdat
                AND  erzet IN s_erzet
                AND  ernam IN s_ernam
                AND  aedat IN s_aedat
                AND  aezet IN s_aezet
                AND  aenam IN s_aenam .

  ENDCASE.

ENDFORM.                    " READ_DATA_FR_ZTCO_nMHPCPOST

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_CONTROL
*&---------------------------------------------------------------------*
*       ALV Grid Control
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_grid_control.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                =
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
      i_structure_name                  = 'ZTCO_nMHPCPOST'
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_ztco_nmhpcpost
    EXCEPTIONS
      program_error                     = 1
      OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_GRID_CONTROL
