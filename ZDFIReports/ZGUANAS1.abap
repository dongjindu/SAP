REPORT zguanas1 MESSAGE-ID gx.

tables: t800a, t881.
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
       END OF ty_data,
       ty_t_data TYPE ty_data OCCURS 0.

TYPES: BEGIN OF ty_data_short,
         rclnt  LIKE glpca-rclnt,
         rldnr  LIKE glpca-rldnr,
         ryear  LIKE glu1-ryear,
         rbukrs LIKE glu1-rbukrs,
         bukrs  LIKE glu1-bukrs,
         rcomp  LIKE glu1-rcomp,
         count  LIKE sy-dbcnt,
         rank   TYPE i,
       END OF ty_data_short,
       ty_t_data_short TYPE ty_data_short OCCURS 0.

TYPES: BEGIN OF ty_field_list,
         fieldname LIKE dfies-fieldname,
       END OF ty_field_list,
       ty_t_field_list TYPE ty_field_list OCCURS 0.


********   SELECTIONS   ********
PARAMETERS: p_tab LIKE t800a-tab.


********   GLOBAL DATA   ********
DATA: gt_statistics TYPE ty_t_data.
DATA: gv_tab_type  like t800a-ttype,
      gv_comptab   LIKE t800a-comptab,
      gv_objtable  like t800a-objtable,
      gv_table_sum like t800a-tab.



********   AT SELECTION-SCREEN   ********
AT SELECTION-SCREEN.
  PERFORM table_check_and_table_kind_get USING p_tab
                                      CHANGING gv_tab_type
                                               gv_comptab
                                               gv_objtable
                                               gv_table_sum.
********   START-OF-SELECTION   ********
START-OF-SELECTION.

  PERFORM statistisc_get TABLES gt_statistics
                         USING  p_tab
                                gv_tab_type
                                gv_comptab
                                gv_objtable.

  PERFORM statistics_save TABLES gt_statistics
                          USING  p_tab.


*&---------------------------------------------------------------------*
*&      Form  table_check_and_table_kind_get
*&---------------------------------------------------------------------*
*    Prüfe Tabelle, stelle Tabellenart fest
*----------------------------------------------------------------------*
* --> FP_TABLE          Einzelpostentabelle
* <-- FV_TAB_TYPE Erg.: Tabellentyp
* <-- FV_COMPTAB  Erg.: X= Globale Tab. (RCOMP statt (B)BUKRS im Bauch)
* <-- FV_OBJTABLE Erg.: Wenn gefüllt und COMPTAB SPACE => Pooltabelle
*                       (Pooltabellen haben BUKRS, transp. Tab. RBUKRS)
* <-- FV_TABLE_SUM Erg.: Dazugehörige Summentabelle
*----------------------------------------------------------------------*
FORM table_check_and_table_kind_get
                           USING    value(fp_table) LIKE t800a-tab
                           CHANGING fv_tab_type     LIKE t800a-ttype
                                    fv_comptab      LIKE t800a-comptab
                                    fv_objtable     LIKE t800a-objtable
                                    fv_table_sum    LIKE t800a-tab.


  CLEAR: fv_tab_type, fv_comptab, fv_objtable, fv_table_sum.

* only FI-SL tables are allowed
  SELECT single ttype comptab ntable objtable FROM t800a
         INTO (fv_tab_type, fv_comptab, fv_table_sum, fv_objtable)
         where tab      = fp_table.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE e091 WITH fp_table.
    ENDIF.

* only type TT, SI, SP, RI, RP are allowed
    IF fv_tab_type NE 'SI' and  fv_tab_type NE 'SP' and
       fv_tab_type NE 'RI' and  fv_tab_type NE 'RP' and
       fv_tab_type NE 'TT'.
      MESSAGE e093 WITH fv_tab_type.
    ENDIF.

* get summary table
    IF fv_tab_type = 'TT'.
      fv_table_sum = fp_table.
    ELSE.
      "table in T800A-NTABLE is the summary table
    ENDIF.

* only free ledgers are allowed (the analysis is for FI_SL_DATA)
    SELECT COUNT(*) FROM t881 WHERE tab = fv_table_sum
                                AND fix = space.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e092 WITH fp_table.
    ENDIF.

  ENDFORM.                             " table_check_and_table_kind_get

*&---------------------------------------------------------------------*
*&      Form  statistisc_get
*&---------------------------------------------------------------------*
* <-- FT_STATISTICS     Darum geht´s hier
* --> FP_TABLE          Einzelpostentabelle
* --> FV_TAB_TYPE       Tabellentyp
* --> FV_COMPTAB        X= Globale Tab. (RCOMP statt (B)BUKRS im Bauch)
* --> FV_OBJTABLE       Wenn gefüllt und COMPTAB SPACE => Pooltabelle
*&---------------------------------------------------------------------*
FORM statistisc_get TABLES ft_statistics      TYPE ty_t_data
                    USING  value(fp_table)    LIKE t800a-tab
                           value(fv_tab_type) LIKE t800a-ttype
                           value(fv_comptab)  LIKE t800a-comptab
                           value(fv_objtable) LIKE t800a-objtable.

  DATA: lt_fetch_data TYPE ty_t_data       WITH HEADER LINE,
        lt_data_short TYPE ty_t_data_short WITH HEADER LINE,
        lt_field_list TYPE ty_t_field_list.
  DATA: lv_cursor     TYPE cursor,
        lv_counter    LIKE sy-dbcnt,
        lv_cnt_total  LIKE sy-dbcnt,
        lv_pack_size  LIKE sy-dbcnt.

* get fieldlist and package size
  PERFORM field_list_build USING fp_table
                                 fv_tab_type
                                 fv_comptab
                                 fv_objtable
                        CHANGING lt_field_list[]
                                 lv_pack_size.

  OPEN CURSOR WITH HOLD lv_cursor FOR
       SELECT (lt_field_list) FROM (fp_table)
       CLIENT SPECIFIED.

  DO.
    FETCH NEXT CURSOR lv_cursor
          INTO CORRESPONDING FIELDS OF TABLE lt_fetch_data
          PACKAGE SIZE lv_pack_size.
    IF NOT sy-subrc IS INITIAL.
      EXIT.
    ENDIF.
    lv_cnt_total = sy-dbcnt.

    IF sy-batch IS INITIAL.
      lv_counter = sy-index * lv_pack_size.
      PERFORM progress_indicator USING fp_table
                                       lv_counter.
    ENDIF.

    LOOP AT lt_fetch_data.
      lt_fetch_data-count = 1.
      COLLECT lt_fetch_data INTO ft_statistics.
      MOVE-CORRESPONDING lt_fetch_data TO lt_data_short.
      COLLECT lt_data_short.
    ENDLOOP.

  ENDDO.


  SORT lt_data_short BY rclnt rldnr ryear rbukrs bukrs rcomp.

  LOOP AT ft_statistics.
    READ TABLE lt_data_short WITH KEY rclnt  = ft_statistics-rclnt
                                      rldnr  = ft_statistics-rldnr
                                      ryear  = ft_statistics-ryear
                                      rbukrs = ft_statistics-rbukrs
                                      bukrs  = ft_statistics-bukrs
                                      rcomp  = ft_statistics-rcomp
                                      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      ft_statistics-rank = lt_data_short-count.
    ENDIF.

    ft_statistics-pc = ( ft_statistics-count * 100 ) / lv_cnt_total.
    MODIFY ft_statistics.

  ENDLOOP.

ENDFORM.                               " statistisc_get

*&---------------------------------------------------------------------*
*&      Form  field_list_build
*&---------------------------------------------------------------------*
*   Welche Felder sollen f. d. Analyse selektiert werden. Hängt ab vom
*   Tabellentyp (Summen/EP) und davon, ob die Tab. global o. lokal ist.
* 1) Bei Spezialisten ohne RACCT wird FV_PCAK_SIZE nicht optimal sein
*   (vielleicht noch einiges andere auch ..)
*----------------------------------------------------------------------*
*  --> FP_TABLE            Einzelpostentabelle
*  --> FV_TAB_TYPE         Tabellentyp
*  --> FV_COMPTAB          X= Globale Tab. (RCOMP statt RBUKRS im Bauch)
*  --> FV_OBJTABLE         Wenn gefüllt und COMPTAB SPACE => Pooltabelle
*                          (Pooltab. haben BUKRS, transp. Tab RBUKRS)
*  <-- FT_FIELD_LIST    Erg.: Feldliste für den OPEN CURSOR
*  <-- FV_PACK_SIZE     Erg.: Package-Size für den FETCH
*----------------------------------------------------------------------*
FORM field_list_build USING  value(fp_table)    LIKE t800a-tab
                             value(fv_tab_type) LIKE t800a-ttype
                             value(fv_comptab)  LIKE t800a-comptab
                             value(fv_objtable) LIKE t800a-objtable
                    CHANGING ft_field_list      TYPE ty_t_field_list
                             fv_pack_size       LIKE sy-dbcnt.

  DATA: lw_field_list LIKE LINE OF ft_field_list.

  CLEAR: fv_pack_size, ft_field_list[].

  fv_pack_size = 23.

  lw_field_list-fieldname = 'RCLNT'.
  APPEND lw_field_list TO ft_field_list.
  lw_field_list-fieldname = 'RLDNR'.
  APPEND lw_field_list TO ft_field_list.
  lw_field_list-fieldname = 'RRCTY'.
  APPEND lw_field_list TO ft_field_list.
  lw_field_list-fieldname = 'RVERS'.
  APPEND lw_field_list TO ft_field_list.
  lw_field_list-fieldname = 'RYEAR'.
  APPEND lw_field_list TO ft_field_list.
  IF fv_tab_type = 'SI' OR fv_tab_type = 'RI'.
    lw_field_list-fieldname = 'POPER'.
    APPEND lw_field_list TO ft_field_list.
    ADD 3 TO fv_pack_size.
  ELSEIF fv_tab_type = 'SP' OR fv_tab_type = 'RP' OR fv_tab_type = 'TT'.
  ELSE.
    RAISE wrong_table_type.
  ENDIF.
  IF fv_comptab IS INITIAL.
    IF fv_objtable IS INITIAL.
      lw_field_list-fieldname = 'BUKRS'.     "lokal pool table
    ELSE.
      lw_field_list-fieldname = 'RBUKRS'.    "lokal transparent table
    ENDIF.
    ADD 4 TO fv_pack_size.
  ELSE.
    lw_field_list-fieldname = 'RCOMP'. "global table
    ADD 6 TO fv_pack_size.
  ENDIF.
  APPEND lw_field_list TO ft_field_list.
  lw_field_list-fieldname = 'RACCT'.   "1)
  APPEND lw_field_list TO ft_field_list.

  fv_pack_size = ( 28 * 1024 ) / fv_pack_size.

  perform field_list_check using fp_table
                        changing ft_field_list[].

ENDFORM.                               " field_list_build

*&---------------------------------------------------------------------*
*&      Form  field_list_check
*&---------------------------------------------------------------------*
*  Prüfen, ob die Tabelle alle zur Analyse gewählten Felder besitzt.
*  Vor allem beim Konto (RACCT) muß es nicht der Fall sein.
*----------------------------------------------------------------------*
*  <-- FT_FIELD_LIST    Erg.: Feldliste für den OPEN CURSOR
*----------------------------------------------------------------------*
FORM field_list_check using value(fp_table)  LIKE t800a-tab
                   CHANGING ft_field_list    TYPE ty_t_field_list.

  DATA: lt_dfies LIKE dfies OCCURS 0 WITH HEADER LINE.
  data: lw_field_list LIKE LINE OF ft_field_list.

  CALL FUNCTION 'GET_FIELDTAB'
       EXPORTING
            langu               = sy-langu
            tabname             = fp_table
       TABLES
            fieldtab            = lt_dfies
       EXCEPTIONS
            no_texts_found      = 2
            table_has_no_fields = 3
            table_not_activ     = 4.

  sort lt_dfies by fieldname.
  loop at ft_field_list into lw_field_list.
    read table lt_dfies with key fieldname = lw_field_list-fieldname
                        binary search.
    if not sy-subrc is initial.
      delete ft_field_list where fieldname = lw_field_list-fieldname.
    endif.
  endloop.

ENDFORM.                    " field_list_check

*----------------------------------------------------------------
* Form PROGRESS_INDICATOR
*----------------------------------------------------------------
FORM progress_indicator USING value(fp_table)  LIKE t800a-tab
                              value(fv_cnt)    LIKE sy-dbcnt.


  STATICS: ls_time_next  LIKE sy-uzeit.

  DATA: lv_001(40)     TYPE c,
        lv_time        LIKE sy-uzeit,
        lv_txt_cnt(20) TYPE c,
        lv_txt(95)     TYPE c.

  GET TIME FIELD lv_time.
  CHECK lv_time GE ls_time_next.


  ls_time_next = lv_time + 15.

  WRITE fv_cnt TO lv_txt_cnt.
  WRITE 'Entries read from'(001) TO lv_001.
  CONCATENATE  lv_001 fp_table lv_txt_cnt INTO lv_txt
               SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lv_txt.
ENDFORM.                               "PROGRESS_INDICATOR

*&---------------------------------------------------------------------*
*&      Form  statistics_save
* ID 32 Stellen
*&---------------------------------------------------------------------*
FORM statistics_save TABLES ft_statistics   TYPE ty_t_data
                     USING  value(fp_table) LIKE t800a-tab.

  TABLES: indx.
  DATA: lv_id(32) TYPE c.

  indx-aedat = sy-datum.
  indx-usera = sy-uname.
  CONCATENATE 'ZGUANA' fp_table INTO lv_id.
  EXPORT statinfo FROM ft_statistics
                  TO DATABASE indx(FI)
                  CLIENT '999' ID lv_id.

  skip 1.
  write: /'The analysis is finished.'(002).
  write: /'Start program ZGUANAS2 to see the results.'(003).

endform.                    " statistics_save
