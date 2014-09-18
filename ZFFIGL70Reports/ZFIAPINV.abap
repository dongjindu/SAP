REPORT zfiaplist                MESSAGE-ID fs.

TABLES:
  bkpf,
  aufk,
  anli,
  lfa1,
  kna1,
  bseg,
  bsas,
  bsis,
  bsec, "one time
  bsak, bsik, bsad, bsid,
  skb1,
  tzun,
  ztfi_fmal, "FM payment
  t001.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DATA : BEGIN OF $belnr OCCURS 0,
        gjahr LIKE bkpf-gjahr,
        belnr LIKE bkpf-belnr,
       END OF $belnr.


CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: i_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE,
      i_bseg LIKE bseg OCCURS 0 WITH HEADER LINE,
      i_skat LIKE skat OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_line OCCURS 0,
        paydoc TYPE belnr_d,

        accnt  LIKE bsik-lifnr,
        name1  LIKE lfa1-name1,   "vendor name

* header
        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
        budat  LIKE bkpf-budat,
        bldat  LIKE bkpf-bldat,
        blart  LIKE bkpf-blart,
        waers  LIKE bkpf-waers,
        xblnr  LIKE bkpf-xblnr,

        augdt  LIKE bseg-augdt,   "clearing date
        wrbtr  LIKE bseg-wrbtr,
        shkzg  LIKE bseg-shkzg,
        dmbtr  LIKE bseg-dmbtr,

        lifnr  LIKE bsik-lifnr,
        hkont  LIKE bseg-hkont,
        txt50  LIKE skat-txt50,

        sgtxt  LIKE bseg-sgtxt,
        koart  LIKE bseg-koart,
        fistl  LIKE bseg-fistl,

        flag,
      END OF i_line.

DATA: BEGIN OF t_line OCCURS 0,
        paydoc(10),
        accnt(10),
        name1(35),
        gjahr(4),
        belnr(10),
        budat(10),
        bldat(10),
        waers(5),
        xblnr(16),
        augdt(10),
        wrbtr(15),
        shkzg(1),
        dmbtr(15),
        lifnr(10),
        hkont(10),
        txt50(50),
        sgtxt(50),
        fistl(10),
        flag(1),
      END OF t_line.


* USING ALV REPORTING..
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
       g_cnt(2)     TYPE n,
       ls_sort      TYPE slis_sortinfo_alv,
       gt_sort      TYPE slis_t_sortinfo_alv.

DATA :
       gs_gridset             TYPE lvc_s_glay,
       gs_exit_caused_by_user TYPE slis_exit_by_user,
       gs_variant             TYPE disvariant,
       gs_fieldcat            TYPE slis_fieldcat_alv,
       gs_print               TYPE slis_print_alv,
       gs_sort                TYPE slis_sortinfo_alv,
       gs_sgroup              TYPE slis_sp_group_alv.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.


DATA: g_exit_caused_by_caller,
      gt_list_top_of_page TYPE slis_t_listheader,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
      g_tabname TYPE slis_tabname VALUE 'ITAB',
      g_boxname TYPE slis_fieldname VALUE 'BOX'.

DATA: g_koart LIKE bseg-koart.
DATA: i_fmal LIKE ztfi_fmal OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK h WITH FRAME TITLE text-10h.

PARAMETERS:
  p_bukrs  LIKE bkpf-bukrs MEMORY ID buk OBLIGATORY,
  p_gjahr  LIKE bkpf-gjahr OBLIGATORY.

SELECT-OPTIONS:
  s_cdate FOR bkpf-budat,   "cash date
  s_belnr FOR bkpf-belnr,
  s_budat FOR bkpf-budat,
  s_blart FOR bkpf-blart.
PARAMETERS p_iopen AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK h.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-101.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  par_r1 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(29) text-003 FOR FIELD par_r1.
PARAMETERS:
  par_file(50).

SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
PARAMETERS:
  par_r2 RADIOBUTTON GROUP 1 .
SELECTION-SCREEN:
  COMMENT 03(50) text-005 FOR FIELD par_r2,
  END OF LINE.
SELECTION-SCREEN END   OF BLOCK 1.


AT SELECTION-SCREEN ON p_bukrs.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e511 WITH p_bukrs.
  ENDIF.

TOP-OF-PAGE.
*  WRITE:
**      ....+....1....+....2....+....3....+....4....+....5....+....6
*    / 'Comp Year Document   Update status'.
*  ULINE.

INITIALIZATION.
  g_repid = sy-repid.

  par_file    = 'c:\temp\FIAPINV'.
  par_file+17  = '-'.
  WRITE sy-datlo TO par_file+18(6) YYMMDD.
  par_file+25 = '-'.
  par_file+26 = sy-timlo.
  CONCATENATE par_file '.txt' INTO par_file.
  CONDENSE par_file NO-GAPS.


START-OF-SELECTION.

  __process 'Please wait...' '10'.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  CHECK sy-subrc = 0.

  SELECT * FROM skat INTO TABLE i_skat
     WHERE spras = sy-langu
       AND ktopl = t001-ktopl.

  SORT i_skat BY saknr.


  __process 'Gather from ztfi_fmal...' '40'.

  SELECT * INTO TABLE i_fmal
               FROM ztfi_fmal
               WHERE bukrs = p_bukrs
*                 AND gjahr = p_gjahr
                 AND kngjahr = p_gjahr
                 AND datum IN s_cdate.

*  LOOP AT i_fmal.
*    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF i_bkpf
*                 WHERE bukrs = p_bukrs
*                   AND gjahr = i_fmal-kngjahr
*                   AND belnr = i_fmal-knbelnr
*                   AND budat IN s_budat.
*    CHECK sy-subrc = 0.
*    READ TABLE i_bkpf WITH KEY gjahr = i_fmal-kngjahr
*                               belnr = i_fmal-knbelnr.
*    IF sy-subrc <> 0.
*      APPEND i_bkpf.
*    ENDIF.
*  ENDLOOP.
*

  __cls $belnr.
  LOOP AT i_fmal.
    $belnr-gjahr = i_fmal-kngjahr.
    $belnr-belnr = i_fmal-knbelnr.
    COLLECT $belnr.
  ENDLOOP.

  SELECT * FROM bkpf INTO CORRESPONDING FIELDS OF TABLE i_bkpf
      FOR ALL ENTRIES IN $belnr
               WHERE bukrs = p_bukrs
                 AND gjahr = $belnr-gjahr
                 AND belnr = $belnr-belnr
                 AND budat IN s_budat.

  SORT i_bkpf BY gjahr belnr.

  DELETE ADJACENT DUPLICATES FROM i_bkpf
    COMPARING gjahr belnr.

  __process 'refine data...' '70'.

  SORT i_fmal BY kngjahr knbelnr.


  LOOP AT i_bkpf.
    CLEAR i_line.

    READ TABLE i_fmal WITH KEY kngjahr = i_bkpf-gjahr
                               knbelnr = i_bkpf-belnr
                               BINARY SEARCH.

    IF sy-subrc EQ 0.
      i_line-paydoc = i_fmal-belnr.
      i_line-fistl = i_fmal-fistl.
      IF i_fmal-payflg = '+'.
        i_line-shkzg = 'D'.
      ELSE.
        i_line-shkzg = 'C'.
      ENDIF.

    ENDIF.

* header info.
    MOVE-CORRESPONDING i_bkpf TO i_line.

    SELECT * FROM bseg INTO TABLE i_bseg
      WHERE bukrs = i_bkpf-bukrs
        AND gjahr = i_bkpf-gjahr
        AND belnr = i_bkpf-belnr.

* select vendor line
    LOOP AT i_bseg WHERE shkzg = 'H'.
      CHECK i_bseg-lifnr <> space.

      i_line-lifnr = i_bseg-lifnr.
      i_line-accnt = i_line-lifnr.
      i_line-augdt = i_bseg-augdt.
* ... if one-time
      SELECT SINGLE * FROM bsec
       WHERE bukrs = i_bkpf-bukrs
         AND gjahr = i_bkpf-gjahr
         AND belnr = i_bkpf-belnr.
      IF sy-subrc = 0.
        i_line-name1 = bsec-name1.
      ELSE.
        SELECT SINGLE * FROM lfa1 WHERE lifnr = i_bseg-lifnr.
        i_line-name1 = lfa1-name1.
      ENDIF.
    ENDLOOP.

    CHECK i_line-lifnr <> space.

* add.. debit items...
    LOOP AT i_bseg WHERE shkzg = 'S'.
      READ TABLE i_skat WITH KEY saknr = i_bseg-hkont BINARY SEARCH.
      i_line-txt50 = i_skat-txt50.
      i_line-hkont = i_bseg-hkont.
      i_line-dmbtr = i_bseg-dmbtr.
      i_line-wrbtr = i_bseg-wrbtr.
      i_line-sgtxt = i_bseg-sgtxt.
      APPEND i_line.
    ENDLOOP.

    CLEAR i_line.
  ENDLOOP.
  DATA $ix TYPE i.

  LOOP AT i_line.
    IF i_line-hkont IS INITIAL.
      $ix = sy-tabix.
      SELECT SINGLE hkont INTO i_line-hkont FROM bseg
      WHERE bukrs EQ p_bukrs
        AND belnr EQ i_line-paydoc
        AND gjahr EQ p_gjahr.
      MODIFY i_line INDEX $ix TRANSPORTING hkont.
    ENDIF.
  ENDLOOP.


  IF p_iopen EQ 'X'.
* add

    DATA $date TYPE datum.

    DATA: BEGIN OF it_add OCCURS 0,
            belnr  LIKE bseg-belnr,
            buzei  LIKE bseg-buzei,
            budat  LIKE bkpf-budat,
            bldat  LIKE bkpf-bldat,
            blart  LIKE bkpf-blart,
            waers  LIKE bkpf-waers,
            xblnr  LIKE bkpf-xblnr,
            lifnr  LIKE bsik-lifnr,
            sgtxt  LIKE bseg-sgtxt,
            koart  LIKE bseg-koart,
          END OF it_add.
    DATA $i_line LIKE i_line OCCURS 0 WITH HEADER LINE.

    CONCATENATE p_gjahr '1231' INTO $date.

    SELECT belnr buzei budat bldat blart waers
           xblnr lifnr sgtxt
             FROM bsik INTO TABLE it_add
               WHERE bukrs = p_bukrs
                 AND gjahr = p_gjahr
                 AND blart IN s_blart.
    SELECT belnr buzei budat bldat blart waers
           xblnr lifnr sgtxt
    FROM bsak APPENDING TABLE it_add
                   WHERE bukrs = p_bukrs
                     AND gjahr = p_gjahr
                     AND augdt >  $date
                     AND blart IN s_blart.


    LOOP AT it_add.
      SELECT  *    FROM bseg WHERE bukrs EQ p_bukrs
                          AND     belnr EQ it_add-belnr
                          AND    gjahr EQ p_gjahr
                          AND    buzei NE it_add-buzei.
        MOVE-CORRESPONDING  bseg TO  $i_line .
        MOVE-CORRESPONDING  it_add TO  $i_line .
        CLEAR $i_line-augdt.
        IF bseg-shkzg EQ 'H'.
          $i_line-shkzg = 'D'.
        ELSE.
          $i_line-shkzg = 'C'.
        ENDIF.

        $i_line-koart = 'K'.
        $i_line-flag = 'X'.
        SELECT SINGLE * FROM lfa1 WHERE lifnr = it_add-lifnr.
        $i_line-name1 = lfa1-name1.
        $i_line-accnt = it_add-lifnr.
        READ TABLE i_skat WITH KEY saknr = bseg-hkont BINARY SEARCH.
        $i_line-txt50 = i_skat-txt50.
        APPEND $i_line.
      ENDSELECT.
    ENDLOOP.

    APPEND LINES OF $i_line TO i_line.
  ENDIF.

  __process 'finalizing...' '90'.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.

  READ TABLE i_line INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Data not found!'.
    EXIT.
  ENDIF.

  IF par_r1 EQ 'X'.
    PERFORM create_file.
  ELSE.
    PERFORM display_s.
  ENDIF.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.
  DATA l_field(20).

  CASE p_ucomm.
    WHEN '&IC1'.
      GET CURSOR FIELD l_field.
*     CHECK l_field EQ 'I_LINE-BELNR'.
      READ TABLE i_line INDEX p_selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID:'BLN' FIELD i_line-belnr,
                         'BUK' FIELD p_bukrs,
                         'GJR' FIELD i_line-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.

************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'MENU'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alvprn_basic01.
  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.
  gs_layout-f2code           = '&IC1'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  DELETE gt_events WHERE name NE 'END_OF_PAGE'
                     AND name NE 'TOP_OF_PAGE'
                     AND name NE 'TOP_OF_LIST'
                     AND name NE 'END_OF_LIST'.
  LOOP AT gt_events ASSIGNING <ls_event>.
    CONCATENATE 'ALV_EVENT_'
                <ls_event>-name
                INTO <ls_event>-form.
  ENDLOOP.
ENDFORM.                    " alvprn_basic01
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

*  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
*                         = g_fieldcat_s-seltext_m.
  APPEND g_fieldcat_s TO p_fieldcat_t.
ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
FORM sort_build.
  CLEAR: gt_sort[], gt_sort.

*  CLEAR ls_sort.
*  ls_sort-fieldname = 'ACCNT'.
*  ls_sort-spos = 1.    "---> KEY
*  ls_sort-up = 'X'.
*  APPEND ls_sort TO gt_sort.

*  CLEAR ls_sort.
*  ls_sort-fieldname = 'MATNR'.
*  ls_sort-spos = 2.
*  ls_sort-up = 'X'.
*  ls_sort-subtot = 'X'.
* ls_sort-group = '*'.
*  APPEND ls_sort TO gt_sort.
ENDFORM.                    " sort_build
*&---------------------------------------------------------------------*
*&      Form  display_s
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_s.
* ALV HEADER & FIELD SETTING
  PERFORM alvprn_basic01.
  PERFORM fieldcat_init     USING gt_fieldcat[].

  SORT i_line BY accnt budat.
  PERFORM   sort_build.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'PF_STATUS_SET'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            it_sort                  = gt_sort[]
            it_events                = gt_events[]
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = i_line.


ENDFORM.                    " display_s
*&---------------------------------------------------------------------*
*&      Form  create_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_file.
  IF par_file EQ space.
    par_file    = 'c:\temp\FIAPINV'.
    par_file+8  = '-'.
    WRITE sy-datlo TO par_file+9(6) YYMMDD.
    par_file+15 = '-'.
    par_file+16 = sy-timlo.
    CONCATENATE par_file '.txt' INTO par_file.
    CONDENSE par_file NO-GAPS.
  ENDIF.

  __cls t_line.

  LOOP AT i_line.
    MOVE-CORRESPONDING i_line TO t_line.
    APPEND t_line.
  ENDLOOP.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename        = par_file
       TABLES
            data_tab        = t_line
       EXCEPTIONS
            file_open_error = 1
            OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'ERROR OPENING/DOWNLOADING TO PC FILE.'.
  ELSE.
    MESSAGE s000 WITH 'File has been created successfully!:' par_file.
  ENDIF.

ENDFORM.                    " create_file

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
  'X'  'PAYDOC'   'Pay.Doc#'        10  'CHAR' '' '' '',
  'X'  'ACCNT'    'Account.'             7  'CHAR' '' '' '',
  ' '  'NAME1'    'Ref.'           30  'CHAR' '' '' '',
  ' '  'WAERS'    'Cur.'             5  'CHARC' '' '' '',
  ' '  'WRBTR'    'Doc.Amt.'             15  'CURR' '' '' '',
  ' '  'SHKZG'    'D/C'        1  'CHAR' '' '' '',
  ' '  'DMBTR'    'Doc.Amt.'        15  'CURR' '' '' '',
  ' '  'BUDAT'    'PstDt'        10  'DATS' '' '' '',
  ' '  'BLDAT'    'DocDt'        10  'DATS' '' '' '',
  ' '  'AUGDT'    'AUGDT'        10  'DATS' '' '' '',
  ' '  'GJAHR'    'Year'          4  'NUMC' '' '' '',
  ' '  'BELNR'     'Document'                10 'CHAR' '' '' '',
  ' '  'BLART'    'DT'               2  'CHAR' '' '' '',
  ' '  'XBLNR'    'Reference'          3  'CHAR' '' '' '',
  ' '  'FISTL'    'CstCenter'         10  'CHAR' '' '' '',
  ' '  'HKONT'    'G/L Account'          7  'CHAR' '' '' '',
  ' '  'TXT50'    'G/L Acct.Long Text'       50  'CHAR' '' '' '',
  ' '  'SGTXT'    'Text'          50  'CHAR' '' '' '',
  ' '  'FLAG'     ''          1  'CHAR' '' '' ''.


  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname EQ 'PAYDOC'.
      gs_fieldcat-ref_tabname = 'ZTFI_FMAL'.
      gs_fieldcat-ref_fieldname = 'BELNR'.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'NAME1'.
      gs_fieldcat-ref_tabname = 'LFA1'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'ACCNT'.
      gs_fieldcat-ref_tabname = 'BSIK'.
      gs_fieldcat-ref_fieldname = 'LIFNR'.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'NAME1'.
      gs_fieldcat-ref_tabname = 'LFA1'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'GJAHR' OR
       gs_fieldcat-fieldname EQ 'BELNR' OR
       gs_fieldcat-fieldname EQ 'AUGDT' OR
       gs_fieldcat-fieldname EQ 'WRBTR' OR
       gs_fieldcat-fieldname EQ 'DMBTR' OR
       gs_fieldcat-fieldname EQ 'SGTXT' OR
       gs_fieldcat-fieldname EQ 'KOART' OR
       gs_fieldcat-fieldname EQ 'HKONT'.
      gs_fieldcat-ref_tabname = 'BSEG'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'BUDAT' OR
       gs_fieldcat-fieldname EQ 'BLDAT' OR
       gs_fieldcat-fieldname EQ 'BLART' OR
       gs_fieldcat-fieldname EQ 'WAERS' OR
       gs_fieldcat-fieldname EQ 'XBLNR'.
      gs_fieldcat-ref_tabname = 'BKPF'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'TXT50'.
      gs_fieldcat-ref_tabname = 'SKAT'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.

    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " fieldcat_init
