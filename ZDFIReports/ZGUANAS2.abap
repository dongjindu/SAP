REPORT zguanas2.


TABLES: indx, t881.
TYPE-POOLS: kkblo, ADK.

********   TYPES   ********
TYPES: BEGIN OF ty_data,
         rclnt  LIKE glu1-rclnt,
         rldnr  LIKE glu1-rldnr,
         rrcty  LIKE glu1-rrcty,
         rvers  LIKE glu1-rvers,

         ryear  LIKE glu1-ryear,
         poper  LIKE glu1-poper,
         rbukrs LIKE glu1-rbukrs,
         bukrs  LIKE glu1-bukrs,
         rcomp  LIKE glu1-rcomp,

         racct  LIKE glu1-racct,

         count  LIKE sy-dbcnt,
         pc(3)  TYPE p DECIMALS 2,
         rank   LIKE sy-dbcnt,
         arch_obj LIKE arch_def-object,
       END OF ty_data,
       ty_t_data TYPE ty_data OCCURS 0.


********   SELECTIONS   ********
PARAMETERS: p_tab LIKE t800a-tab.


********   GLOBAL DATA   ********
DATA: gt_statistics TYPE ty_t_data.
DATA: gv_tab_type   LIKE t800a-ttype,
      gv_comptab    LIKE t800a-comptab,
      gv_objtable   LIKE t800a-objtable,
      gv_table_sum  like t800a-tab.


********   AT SELECTION-SCREEN   ********
AT SELECTION-SCREEN.
  PERFORM table_check_and_table_kind_get(zguanas1)
                                         USING p_tab
                                      CHANGING gv_tab_type
                                               gv_comptab
                                               gv_objtable
                                               gv_table_sum.
********   START-OF-SELECTION   ********
START-OF-SELECTION.

  PERFORM statistisc_read TABLES gt_statistics
                           USING p_tab.

  PERFORM arch_obj_determine TABLES gt_statistics
                              USING p_tab
                                    gv_table_sum.

  PERFORM statistics_show TABLES gt_statistics
                          USING  p_tab
                                 gv_tab_type
                                 gv_comptab
                                 gv_objtable.

*&---------------------------------------------------------------------*
*&      Form  statistisc_read
*----------------------------------------------------------------------*
* <-- FT_STATISTICS     Erg.: Darum geht´s hier
*----------------------------------------------------------------------*
FORM statistisc_read TABLES ft_statistics   TYPE ty_t_data
                      USING value(fp_table) LIKE t800a-tab.

  DATA: lv_id(32) TYPE c.

  CONCATENATE 'ZGUANA' fp_table INTO lv_id.
  IMPORT statinfo TO ft_statistics[]
         FROM DATABASE indx(fi) CLIENT '999' ID lv_id.

ENDFORM.                               " statistisc_read


*&---------------------------------------------------------------------*
*&      Form  arch_obj_determine
*&---------------------------------------------------------------------*
*    Bestimme 'zuständiges' Achivierungsobjekt
*----------------------------------------------------------------------*
* <--> FT_STATISTICS     Darum geht´s hier
*----------------------------------------------------------------------*
FORM arch_obj_determine TABLES ft_statistics          TYPE ty_t_data
                         USING value(fp_table)        LIKE t800a-tab
                               value(fv_table_sum)    LIKE t800a-tab.

  DATA: BEGIN OF lt_rldnr_obj OCCURS 0,
          rldnr    LIKE glu1-rldnr,
          arch_obj LIKE arch_def-object,
        END OF lt_rldnr_obj.
  DATA: lv_fix   LIKE t881-fix,
        lv_tabix LIKE sy-tabix.
  CONSTANTS: lc_fi_sl_data LIKE arch_def-object VALUE 'FI_SL_DATA'.


  LOOP AT ft_statistics.
    READ TABLE lt_rldnr_obj with key rldnr = ft_statistics-rldnr
                            BINARY SEARCH.
    lv_tabix = sy-tabix.
    IF not sy-subrc IS INITIAL.
      lt_rldnr_obj-rldnr = ft_statistics-rldnr.
      CLEAR lt_rldnr_obj-arch_obj.
      SELECT SINGLE fix FROM t881 INTO lv_fix
                        WHERE rldnr = ft_statistics-rldnr.
      IF lv_fix IS INITIAL  AND  sy-subrc IS INITIAL.
        lt_rldnr_obj-arch_obj = lc_fi_sl_data.
      ELSEIF NOT lv_fix IS INITIAL  AND  sy-subrc IS INITIAL.
        PERFORM arch_obj_for_fix_rldnr_get USING fp_table
                                        CHANGING lt_rldnr_obj-arch_obj.
      ENDIF.
      insert lt_rldnr_obj index lv_tabix.
    ENDIF.
    ft_statistics-arch_obj = lt_rldnr_obj-arch_obj.
    MODIFY ft_statistics TRANSPORTING arch_obj.

  ENDLOOP.

ENDFORM.                               " arch_obj_determine

*&---------------------------------------------------------------------*
*&      Form  arch_obj_for_fix_rldnr_get
*&---------------------------------------------------------------------*
*   Arch.Obj für ein festes Ledger bestimmen.
*----------------------------------------------------------------------*
* -->  FP_TABLE
* <--  FV_ARCH_OBJ
*----------------------------------------------------------------------*
FORM arch_obj_for_fix_rldnr_get
                            USING value(fp_table) LIKE t800a-tab
                         CHANGING fv_arch_obj     LIKE arch_def-object.

  DATA: lt_objects TYPE adk_ccms_objects WITH HEADER LINE.

  CLEAR fv_arch_obj.

  CALL FUNCTION 'ADK_CCMS_GET_OBJECTS'
       EXPORTING
            table   = fp_table
       TABLES
            objects = lt_objects
       EXCEPTIONS
            OTHERS  = 1.

  check sy-subrc is initial.
  delete lt_objects where object = 'FI_SL_DATA'
                       or object = 'GLX-OBJEKT'.

  if fp_table cp 'GLPC+' AND sy-SAPRL ge '46A'.
    delete lt_objects where object = 'PCA_OBJECT'.
  elseif fp_table cp 'FILC+' and sy-saprl ge '45A'.
    delete lt_objects where object = 'FLC_OBJECT'.
  endif.

  read table lt_objects index 1.
  if sy-subrc is initial.
    fv_arch_obj = lt_objects-object.
  endif.

ENDFORM.                               " arch_obj_for_fix_rldnr_get

*&---------------------------------------------------------------------*
*&      Form  statistics_show
*&---------------------------------------------------------------------*
* --> FT_STATISTICS     Darum geht´s hier
* --> FV_TAB_TYPE       Tabellentyp
*&---------------------------------------------------------------------*
FORM statistics_show TABLES ft_statistics      TYPE ty_t_data
                     USING  value(fp_table)    LIKE t800a-tab
                            value(fv_tab_type) LIKE t800a-ttype
                            value(fv_comptab)  LIKE t800a-comptab
                            value(fv_objtable) LIKE t800a-objtable.

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat,
        lw_fieldcat TYPE kkblo_fieldcat,
        lt_sort     TYPE kkblo_t_sortinfo WITH HEADER LINE,
        lw_layout   TYPE kkblo_layout.

* field list
  lw_fieldcat-fieldname  = 'RANK'.
  lw_fieldcat-outputlen  = '1'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname   = 'RCLNT'.
  lw_fieldcat-ref_tabname = 'GLU1'.
  lw_fieldcat-no_sum      = 'X'.
  APPEND lw_fieldcat TO lt_fieldcat.

  lw_fieldcat-fieldname   = 'RLDNR'.
  APPEND lw_fieldcat TO lt_fieldcat.

  lw_fieldcat-fieldname = 'RRCTY'.
  APPEND lw_fieldcat TO lt_fieldcat.

  lw_fieldcat-fieldname = 'RVERS'.
  APPEND lw_fieldcat TO lt_fieldcat.

  lw_fieldcat-fieldname = 'RYEAR'.
  APPEND lw_fieldcat TO lt_fieldcat.

  IF fv_tab_type = 'SI' OR fv_tab_type = 'RI'.
    lw_fieldcat-fieldname = 'POPER'.
    APPEND lw_fieldcat TO lt_fieldcat.
  ELSEIF fv_tab_type = 'SP' OR fv_tab_type = 'RP' OR fv_tab_type = 'TT'.
  ELSE.
    RAISE wrong_table_type.
  ENDIF.

  IF fv_comptab IS INITIAL.
    IF fv_objtable IS INITIAL.
      lw_fieldcat-fieldname = 'BUKRS'. "lokal pool table
    ELSE.
      lw_fieldcat-fieldname = 'RBUKRS'."lokal transparent table
    ENDIF.
  ELSE.
    lw_fieldcat-fieldname = 'RCOMP'.   "global table
  ENDIF.
  APPEND lw_fieldcat TO lt_fieldcat.

  lw_fieldcat-fieldname = 'RACCT'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR lw_fieldcat.
  lw_fieldcat-inttype   = 'I'.
  lw_fieldcat-do_sum    = 'X'.
  lw_fieldcat-datatype  = 'INT4'.
  lw_fieldcat-fieldname = 'COUNT'.
  lw_fieldcat-reptext   = ''.
  lw_fieldcat-seltext   = fp_table.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR lw_fieldcat.
  lw_fieldcat-inttype   = 'P'.
  lw_fieldcat-do_sum    = 'X'.
  lw_fieldcat-datatype  = 'DEC'.
  lw_fieldcat-outputlen  = '6'.
  lw_fieldcat-fieldname = 'PC'.
  lw_fieldcat-reptext   = '     %'.
  lw_fieldcat-seltext   = '     %'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname     = 'ARCH_OBJ'.
  lw_fieldcat-ref_tabname   = 'ARCH_DEF'.
  lw_fieldcat-ref_FIELDNAME = 'OBJECT'.
  lw_fieldcat-no_sum        = 'X'.
  APPEND lw_fieldcat TO lt_fieldcat.

* sort list
  lt_sort-spos = 1.
  lt_sort-fieldname = 'RANK'.
  lt_sort-down   = 'X'.
  lt_sort-subtot = 'X'.
  APPEND lt_sort.

  CLEAR lt_sort.
  lt_sort-spos = 2.
  lt_sort-fieldname = 'RCLNT'.
  lt_sort-subtot = 'X'.
  APPEND lt_sort.

  lt_sort-spos = 3.
  lt_sort-fieldname = 'ARCH_OBJ'.
  clear lt_sort-comp.
  APPEND lt_sort.

  lt_sort-spos = 4.
  lt_sort-fieldname = 'RLDNR'.
  APPEND lt_sort.

  lt_sort-spos = 5.
  IF fv_comptab IS INITIAL.
    IF fv_objtable IS INITIAL.
      lt_sort-fieldname = 'BUKRS'.     "lokal pool table
    ELSE.
      lt_sort-fieldname = 'RBUKRS'.    "lokal transparent table
    ENDIF.
  ELSE.
    lt_sort-fieldname = 'RCOMP'.       "global table
  ENDIF.
  APPEND lt_sort.

  lt_sort-spos = 6.
  lt_sort-fieldname = 'RYEAR'.
  lt_sort-comp = 'X'.
  APPEND lt_sort.

  lw_layout-totals_only = 'X'.

  CONSTANTS: lc_callback_top LIKE arch_obj-exit_rout
                             VALUE 'TOP_OF_PAGE'.


  CALL FUNCTION 'K_KKB_LIST_DISPLAY'
       EXPORTING
            i_callback_program     = 'ZGUANAS2'
            i_callback_top_of_page = lc_callback_top
            i_tabname              = 'FT_STATISTICS'
            is_layout              = lw_layout
            it_fieldcat            = lt_fieldcat[]
            it_sort                = lt_sort[]
       TABLES
            t_outtab               = ft_statistics.



ENDFORM.                               " statistics_show

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_FAGE
*&---------------------------------------------------------------------*
FORM top_of_page.


  SKIP 1.
  NEW-LINE NO-SCROLLING. WRITE: sy-uline(70).
  NEW-LINE NO-SCROLLING. WRITE: 01(01) sy-vline, 03 'Date'(001),
                                15     sy-datum, 70(01) sy-vline.
  NEW-LINE NO-SCROLLING. WRITE: 01(01) sy-vline, 03 'System'(002),
                                15     sy-sysid, 70(01) sy-vline.
  NEW-LINE NO-SCROLLING. WRITE: 01(01) sy-vline, 70(01) sy-vline.

  NEW-LINE NO-SCROLLING. WRITE: 01(01) sy-vline,
                                03 'Analysis of table'(003),
                                30 p_tab,
                                48 'from'(004),
                                55 indx-aedat, 70(01) sy-vline.
  NEW-LINE NO-SCROLLING. WRITE: sy-uline(70).
  SKIP.

ENDFORM.                               " TOP_OF_FAGE
