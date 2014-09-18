*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 02/11/2004  "mm/dd/yy
*& Specification By       : JIPARK
*& Pattern                : Report 1-15
*& Development Request No :
*& Addl documentation     :
*& Description  : Cash Plan Drill-Down - ZRFIT07 DB Read Option..
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zrfit15 MESSAGE-ID zmfi
               NO STANDARD PAGE HEADING LINE-SIZE 145. " LINE-COUNT 80.

**********************************************************************
* Data Declaration
**********************************************************************
TABLES : ztfi_pltm,  "zrfit07 drill-down plan data.
         t035t.      "group description

DATA : it_pltm LIKE ztfi_pltm OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_ddwn OCCURS 0,
        datum   LIKE sy-datum,          "(posting)date
        dispw   LIKE ztfi_pltm-dispw,   "currency
        wrshb   LIKE ztfi_pltm-wrshb,   "amt
        dmshb   LIKE ztfi_pltm-dmshb,   "local amt
        grupp   LIKE ztfi_pltm-grupp,   "group
        grptt(30),                      "group desc.
        ebene   LIKE ZTFI_PLTM-EBENE,   "LEVEL
        bnkko   LIKE ztfi_pltm-bnkko,   "bank acct
      END OF it_ddwn.

DATA : sv_waers LIKE t001-waers,
       sv_spras like t001-spras.

* using ALV reporting..
TYPE-POOLS : slis.

INCLUDE rvreuse_global_data.
INCLUDE rvreuse_local_data.
INCLUDE rvreuse_forms.

DATA : gs_layout    TYPE slis_layout_alv,
       gt_fieldcat  TYPE slis_t_fieldcat_alv,
       gt_field     TYPE slis_t_fieldcat_alv,
       g_fieldcat_s TYPE slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    TYPE slis_t_event,
       it_sort      TYPE slis_t_sortinfo_alv,
       g_save(1)    TYPE c,
       g_exit(1)    TYPE c,
       gx_variant   LIKE disvariant,
       g_variant    LIKE disvariant,
       g_repid      LIKE sy-repid,
       g_cnt(2)     TYPE n.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
  IMPORT it_pltm sv_waers sv_spras FROM MEMORY ID 'Z15'.

* get group desc.
  SORT it_pltm BY datum dispw.
  LOOP AT it_pltm.
    MOVE-CORRESPONDING it_pltm  TO  it_ddwn.

    SELECT SINGLE textl INTO it_ddwn-grptt
                  FROM t035t
                  WHERE spras = sv_spras
                  AND   grupp = it_pltm-grupp.
    APPEND it_ddwn. CLEAR it_ddwn.
  ENDLOOP.

* ALV HEADER & FIELD SETTING
  PERFORM : alvprn_basic01,
            alvprn_field01 USING 'IT_DDWN'.

* ALV DISPLAY
  PERFORM display_list.

************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.

************************************************************************
* Form END_OF_LIST
************************************************************************
FORM end_of_list.
  DESCRIBE TABLE it_pltm LINES sy-tabix.

  SKIP 1.
  WRITE:/ '*** Processed Document total : ', sy-tabix.
ENDFORM.
*&-------------------------------------------------------------------
*&      Form  alvprn_basic01
*&-------------------------------------------------------------------
FORM alvprn_basic01.
  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
  gs_layout-zebra            = 'X'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM   form_setting
   TABLES   gt_events
    USING : slis_ev_pf_status_set  c_status_set,
            slis_ev_user_command   c_user_command,
            slis_ev_end_of_list    c_end_of_list.

* PROGRAM  ID
  g_repid = sy-repid.
ENDFORM.                    " alvprn_basic01
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
FORM form_setting TABLES p_events_t LIKE gt_events
                   USING p_com   p_form.

  DATA : l_event_s    TYPE  slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_com
                            INTO l_event_s.
  IF   sy-subrc EQ 0.
    MOVE     p_form      TO   l_event_s-form.
    MODIFY   p_events_t  FROM l_event_s INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " form_setting
*&-------------------------------------------------------------------
*&      Form  alvprn_field01
*&-------------------------------------------------------------------
FORM alvprn_field01 USING p_intab.
  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  CLEAR g_cnt.
  PERFORM field_setting TABLES gt_fieldcat USING :
                                  'S' 'DATUM'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '10',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         'X',
                                  'E' 'SELTEXT_M'   'Date',

                                  'S' 'DISPW'       ' ',
                                  ' ' 'DDICTXT'     'S',
                                  ' ' 'OUTPUTLEN'   '04',
                                  'E' 'SELTEXT_M'   'Curr.',

                                  'S' 'WRSHB'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '15',
                                  ' ' 'CURRENCY'    it_pltm-dispw,
                                  ' ' 'SELTEXT_M'   'Doc.Amt.',
                                  'E' 'DO_SUM'      'X',

                                  'S' 'DMSHB'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '15',
                                  ' ' 'CURRENCY'    sv_waers,
                                  ' ' 'SELTEXT_M'   'Loc.Amt.',
                                  'E' 'DO_SUM'      'X',

                                  'S' 'GRUPP'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '05',
                                  ' ' 'KEY'         ' ',
                                  'E' 'SELTEXT_M'   'PlGrp.',

                                  'S' 'GRPTT'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '25',
                                  'E' 'SELTEXT_M'   'Description',

                                  'S' 'EBENE'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '3',
                                  ' ' 'KEY'         ' ',
                                  'E' 'SELTEXT_M'   'lvl',

                                  'S' 'BNKKO'       ' ',
                                  ' ' 'DDICTXT'     'M',
                                  ' ' 'OUTPUTLEN'   '07',
                                  ' ' 'KEY'         ' ',
                                  'E' 'SELTEXT_M'   'Account'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'PF_STATUS_SET'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            i_save                   = g_save
            is_variant               = g_variant
            it_events                = gt_events[]
       TABLES
            t_outtab                 = it_ddwn.

ENDFORM.                    " display_list
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat
                    USING p_gub p_fname p_con.

  DATA: l_col(40).

  IF p_gub = 'S'.
    CLEAR g_fieldcat_s.
    READ TABLE gt_field INTO g_fieldcat_s
                        WITH KEY fieldname  = p_fname.
    EXIT.
  ENDIF.

  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_FIELDCAT_S-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.


* DATA  APPEND
  CHECK  p_gub = 'E'.

  g_cnt = g_cnt + 1.
  g_fieldcat_s-col_pos = g_cnt.

  APPEND g_fieldcat_s TO p_fieldcat_t.
ENDFORM.                    " field_setting
