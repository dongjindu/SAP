*
*
* AP/AR/GL Lineitem list
*  - 1/22/08 PA segment added
*
* by Andy
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer  CTS No.    Description
* 06/20/2013    T00303   UD1K957462  U1: Apply Archiving
*======================================================================

REPORT zfiaplist                MESSAGE-ID fs.

TABLES:
  *bkpf,
  aufk,
  anli,
  lfa1, kna1, ska1,
  bseg,
  bsas,
  bsis,
  rseg,
  bsak, bsik, bsad, bsid,
  rbkp,
  skb1,
  tzun,
  eord,
  t001.

TABLES: ce4h201_acct. "PA segment
DATA: BEGIN OF gw_pa,
        paobjnr   LIKE ce4h201_acct-paobjnr,
        pakmland  LIKE ce4h201_acct-kmland,
        paprodh   LIKE ce4h201_acct-prodh,
        paartnr   LIKE ce4h201_acct-artnr,
        pakndnr   LIKE ce4h201_acct-kndnr,
      END OF gw_pa.

*DATA: i_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_bseg OCCURS 0,
        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
        buzei  LIKE bseg-buzei,
        menge  LIKE bseg-menge,
        matnr  LIKE bseg-matnr,
        lifnr  LIKE bseg-lifnr,
        kunnr  LIKE bseg-kunnr,
        empfb  LIKE bseg-empfb,
        paobjnr LIKE bseg-paobjnr,
        pasubnr LIKE bseg-pasubnr,
      END OF i_bseg.


DATA: BEGIN OF it_output OCCURS 0,
        accnt  LIKE bsik-lifnr, "Acct
        name1(20) TYPE c,  "name

        fkbe1  LIKE bseg-fkber, "Func Area(origin)

        fkber  LIKE bseg-fkber, "Func Area
        kostl  LIKE bseg-kostl, "CC
        aufnr  LIKE bseg-aufnr, "IO

        wrbtr  LIKE bseg-wrbtr,
        dmbtr  LIKE bseg-dmbtr,
        menge  LIKE bseg-menge,

        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
        buzei  LIKE bseg-buzei,

        xblnr  LIKE bkpf-xblnr, "Ref#
        awkey  LIKE bkpf-awkey,
        budat  LIKE bkpf-budat,
        bldat  LIKE bkpf-bldat,
        cpudt  LIKE bkpf-cpudt,
        usnam  LIKE bkpf-usnam,
        tcode  LIKE bkpf-tcode, "TCODE
        blart  LIKE bkpf-blart, "DocType

        ebeln  LIKE bseg-ebeln, "PO
        ebelp  LIKE bseg-ebelp, "ITM
        matnr  LIKE bseg-matnr,

        shkzg  LIKE bseg-shkzg, "Dr/Cr
        bschl  LIKE bseg-bschl, "postingkey
        xnegp  LIKE bseg-xnegp, "Negative
        waers  LIKE bkpf-waers, "Curr

        sgtxt  LIKE bseg-sgtxt,
        zuonr  LIKE bseg-zuonr, "Assign
        stgrd  LIKE bkpf-stgrd,  "rev.reason
        stblg  LIKE bkpf-stblg,  "rev.doc
        stjah  LIKE bkpf-stjah,  "rev.year
        koart  LIKE bseg-koart,  "Type
        umskz  LIKE bsik-umskz,  "SpecialGL
        lifnr  LIKE bsik-lifnr,  "Vendor
        kunnr  LIKE bsid-kunnr,  "Customer
        hkont  LIKE bsis-hkont,

* by ig.moon 3/2/2009 {
        kzust  TYPE kzust.
* }
        INCLUDE STRUCTURE gw_pa.
DATA: END OF it_output.

* by ig.moon 3/2/2009 {

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE eina-lifnr,
      END   OF it_matnr.

TYPES: BEGIN OF ty_bkpf ,
        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
        xblnr  LIKE bkpf-xblnr, "Ref#
        awkey  LIKE bkpf-awkey,
        awtyp  LIKE bkpf-awtyp,
        budat  LIKE bkpf-budat,
        bldat  LIKE bkpf-bldat,
        cpudt  LIKE bkpf-cpudt,
        usnam  LIKE bkpf-usnam,
        tcode  LIKE bkpf-tcode, "TCODE
        blart  LIKE bkpf-blart, "DocType
        waers  LIKE bkpf-waers, "Curr
        stgrd  LIKE bkpf-stgrd,  "rev.reason
        stblg  LIKE bkpf-stblg,  "rev.doc
        stjah  LIKE bkpf-stjah,  "rev.year
     END OF ty_bkpf.

DATA: i_bkpf  TYPE SORTED TABLE OF ty_bkpf WITH UNIQUE KEY gjahr belnr.
DATA: lv_bkpf TYPE ty_bkpf.

DATA big_out LIKE it_output OCCURS 0 WITH HEADER LINE.
* }

DATA: t_line LIKE it_output OCCURS 0 WITH HEADER LINE.
DATA: g_output LIKE it_output.

DATA: BEGIN OF gt_gl OCCURS 0,
        saknr    TYPE saknr,
        mitkz    TYPE mitkz,  "rec
        xkres    TYPE xkres,  "line item
        xopvw    TYPE xopvw,  "open item
        txt20    TYPE txt20_skat,
      END OF gt_gl.

DATA: it_eord LIKE eord OCCURS 0 WITH HEADER LINE.

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

CONSTANTS:
  c_fnam_cos_pf_status
         TYPE  slis_formname VALUE 'ALV_SET_PF_STATUS',
  c_fnam_cos_user_command
         TYPE slis_formname  VALUE 'ALV_USER_COMMAND',
  c_f2code
         LIKE sy-ucomm       VALUE '&ETA'.

DATA: g_exit_caused_by_caller,
      gt_list_top_of_page TYPE slis_t_listheader,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
      gs_exit_caused_by_user TYPE slis_exit_by_user,
      g_tabname TYPE slis_tabname VALUE 'ITAB',
      g_boxname TYPE slis_fieldname VALUE 'BOX'.

CONSTANTS: c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_kschl2 LIKE konp-kschl VALUE 'ZTIR',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_duty  LIKE konp-kschl  VALUE 'ZOA1',"Type of duty
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI'."Type of ETC rate

* Data Define
DATA: g_koart LIKE bseg-koart, "acc.type
      g_xkres LIKE skb1-xkres. "lineitem?
DATA: w_bkpf  LIKE bkpf.

*- U1 start
DATA: gt_bkpf_a TYPE TABLE OF bkpf WITH HEADER LINE,
      gt_bseg_a TYPE TABLE OF bseg WITH HEADER LINE,
      gt_ekpo_a TYPE TABLE OF ekpo WITH HEADER LINE,
      gt_konh_a TYPE TABLE OF konh WITH HEADER LINE,
      gt_konp_a TYPE TABLE OF konp WITH HEADER LINE,
      gs_bkpf_a TYPE ty_bkpf.
DATA: BEGIN OF gt_a018_a OCCURS 0,
      matnr TYPE matnr,
      lifnr TYPE lifnr,
      datbi TYPE kodatbi,
      datab TYPE kodatab,
      knumh TYPE knumh,
      END OF gt_a018_a.
DATA: BEGIN OF gt_konp_a2,
      knumh TYPE knumh,
      kschl TYPE kscha,
      kbetr TYPE kbetr_kond,
      kpein TYPE kpein,
      kmein TYPE kmein,
      kzust TYPE kzust,
      END OF gt_konp_a2.
*- U1 End

DATA: gv_answer(1).
DEFINE __popup.
  perform pop_up using
  &1 &2 &3
  changing gv_answer.
  check gv_answer eq 'J'.
END-OF-DEFINITION.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

*-------------------------------------------------------------*
* Input Screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_bukrs  LIKE bkpf-bukrs MEMORY ID buk OBLIGATORY.

SELECT-OPTIONS:
  s_hkont  FOR skb1-saknr NO INTERVALS MEMORY ID sak OBLIGATORY.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (10) text-013.
SELECTION-SCREEN POSITION 12.
PARAMETERS:  p_open TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN COMMENT  20(10) text-014.
SELECTION-SCREEN POSITION 32.
PARAMETERS  p_post TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.

PARAMETERS:  p_stida LIKE rfpdo-allgstid DEFAULT sy-datum.
SELECT-OPTIONS:
  s_budat FOR *bkpf-budat NO-EXTENSION.  "INDEX!!!

SELECTION-SCREEN SKIP 1.

PARAMETERS:
  p_rev     AS CHECKBOX,
*  p_noline  as checkbox,   "non-line item mgt data
  p_bseg    AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_vari  LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK bl1.

*line-item selection
SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
*  s_gjahr for bkpf-gjahr,
*  s_monat for bkpf-monat.
  s_blart FOR *bkpf-blart,
  s_belnr FOR *bkpf-belnr,
  s_zuonr FOR bseg-zuonr,
  s_xblnr FOR *bkpf-xblnr,
  s_lifnr FOR bseg-lifnr.
SELECTION-SCREEN END OF BLOCK bl4.


SELECT-OPTIONS :
            s_matnr2  FOR bseg-matnr.
PARAMETERS: p_vcust AS CHECKBOX             MODIF ID dis,
            p_povnd AS CHECKBOX             MODIF ID dis,
            p_eord  AS CHECKBOX             MODIF ID dis.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
*----------------------------------------------------------------------*
DATA:
  g_hkont  LIKE skb1-saknr.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM chk_opt.

AT SELECTION-SCREEN ON p_bukrs.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e511 WITH p_bukrs.
  ENDIF.

* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.


*----------------------------------------------------------------------*
TOP-OF-PAGE.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  g_repid = sy-repid.
* Get default variant
  CLEAR g_variant.
  g_variant-report = g_repid.
  gx_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  DATA $text(40).

  __cls big_out.

  PERFORM get_masters CHANGING sy-subrc.
  CHECK sy-subrc = 0.

  PERFORM get_assignment.

  LOOP AT gt_gl.

    __cls it_output.
    __cls : i_bkpf,  t_line,  i_bseg .

    g_hkont = gt_gl-saknr.
    g_koart = gt_gl-mitkz.
    g_xkres = gt_gl-xkres.
*    if p_noline = 'X'.
*      clear g_xkres.
*    endif.

    CONCATENATE 'Now processing:' g_hkont INTO $text.

    PERFORM show_progress  USING $text '0'.

* read lineitem
    CASE gt_gl-mitkz.
      WHEN 'K'.
        PERFORM vendor_item.
      WHEN 'D'.
        PERFORM customer_item.
      WHEN space.
        IF g_xkres = 'X'.
          PERFORM get_gl_item1.
          IF p_bseg = 'X'.
            PERFORM get_bseg_data.
          ENDIF.
        ELSE.
          PERFORM get_gl_item2.
        ENDIF.
      WHEN OTHERS.
*......nothing...
    ENDCASE.

    PERFORM process_line.

    APPEND LINES OF it_output TO big_out.
  ENDLOOP.

  __cls it_output.

  it_output[] = big_out[].

  PERFORM read_reason.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.

  PERFORM display_alv.
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
*                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.

  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.
ENDFORM.                    " field_setting
*---------------------------------------------------------------------*
*       FORM F4_FOR_VARIANT                                           *
*---------------------------------------------------------------------*
FORM f4_for_variant.
*
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = g_variant
      i_save              = g_save
*     it_default_fieldcat =
    IMPORTING
      e_exit              = g_exit
      es_variant          = gx_variant
    EXCEPTIONS
      not_found           = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    "f4_for_variant

*&---------------------------------------------------------------------*
*&      Form  vendor_item
*&---------------------------------------------------------------------*
FORM vendor_item.
* open items
  IF p_open = 'X'.
    SELECT * FROM bsik INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs = p_bukrs
                 AND hkont = g_hkont
                 AND budat <= p_stida.

    SELECT * FROM bsak APPENDING CORRESPONDING FIELDS OF TABLE t_line
                   WHERE bukrs = p_bukrs
                     AND hkont = g_hkont
                     AND budat <= p_stida
                     AND augdt >  p_stida.
* all items
  ELSE.
    SELECT * FROM bsik INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs = p_bukrs
*                 and gjahr in s_gjahr
*                 and monat in s_monat
                 AND hkont = g_hkont
                 AND budat IN s_budat
                 AND belnr IN s_belnr.

    SELECT * FROM bsak APPENDING CORRESPONDING FIELDS OF TABLE t_line
                   WHERE bukrs = p_bukrs
*                     and gjahr in s_gjahr
*                     and monat in s_monat
                     AND hkont = g_hkont
                     AND budat IN s_budat
                     AND belnr IN s_belnr.
  ENDIF.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bsak.
  ENDIF.
*- U1 End

ENDFORM.                    " vendor_item
*&---------------------------------------------------------------------*
*&      Form  customer_item
*&---------------------------------------------------------------------*
FORM customer_item.
* open items
  IF p_open = 'X'.
    SELECT * FROM bsid INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs = p_bukrs
                 AND hkont = g_hkont
                 AND budat <= p_stida.
    SELECT * FROM bsad APPENDING CORRESPONDING FIELDS OF TABLE t_line
                         WHERE bukrs = p_bukrs
                           AND hkont = g_hkont
                           AND budat <= p_stida
                           AND augdt >  p_stida.

  ELSE.
    SELECT * FROM bsid INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs = p_bukrs
*                 and gjahr in s_gjahr
*                 and monat in s_monat
                 AND hkont = g_hkont
                 AND budat IN s_budat
                 AND belnr IN s_belnr.

    SELECT * FROM bsad APPENDING CORRESPONDING FIELDS OF TABLE t_line
                         WHERE bukrs = p_bukrs
*                           and gjahr in s_gjahr
*                           and monat in s_monat
                           AND hkont = g_hkont
                           AND budat IN s_budat
                           AND belnr IN s_belnr.
  ENDIF.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bsad.
  ENDIF.
*- U1 End
ENDFORM.                    " customer_item
*&---------------------------------------------------------------------*
*&      Form  get_gl_item1
*&---------------------------------------------------------------------*
FORM get_gl_item1.
*index
*MANDT  Client
*BUKRS  Company Code
*HKONT  General ledger account
*BUDAT  Posting date in the document
*ZUONR  Assignment number

* open items
  IF p_open = 'X'.
    SELECT * FROM bsis INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs =  p_bukrs
                 AND hkont =  g_hkont
                 AND zuonr IN s_zuonr
                 AND budat <= p_stida
                 AND budat IN s_budat
                 AND belnr IN s_belnr
                 AND blart IN s_blart
                 AND xblnr IN s_xblnr.
    SELECT * FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE t_line
                WHERE bukrs =  p_bukrs
                  AND hkont =  g_hkont
                  AND zuonr IN s_zuonr
                  AND budat <= p_stida
                  AND budat IN s_budat
                  AND augdt >  p_stida
                  AND belnr IN s_belnr
                  AND blart IN s_blart
                  AND xblnr IN s_xblnr.
  ELSE.
    SELECT * FROM bsis INTO CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs =  p_bukrs
                 AND hkont =  g_hkont
                 AND budat IN s_budat
                 AND zuonr IN s_zuonr
*                 and gjahr in s_gjahr
*                 and monat in s_monat
                 AND belnr IN s_belnr
                 AND blart IN s_blart
                 AND xblnr IN s_xblnr.
    SELECT * FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE t_line
               WHERE bukrs = p_bukrs
                 AND hkont = g_hkont
                 AND budat IN s_budat
                 AND zuonr IN s_zuonr
*                 and gjahr in s_gjahr
*                 and monat in s_monat
                 AND belnr IN s_belnr
                 AND blart IN s_blart
                 AND xblnr IN s_xblnr.
  ENDIF.

  IF t_line[] IS NOT INITIAL.
    SELECT bukrs
           gjahr belnr xblnr awkey awtyp budat bldat
           cpudt usnam tcode blart waers stgrd stblg stjah
       FROM bkpf
       INTO CORRESPONDING FIELDS OF TABLE i_bkpf
       FOR ALL ENTRIES IN t_line
            WHERE bukrs = p_bukrs
              AND gjahr = t_line-gjahr
              AND belnr = t_line-belnr.
  ENDIF.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bsis.

    PERFORM archive_read_bsas.

    IF t_line[] IS NOT INITIAL.
      PERFORM archive_read_bkpf USING '1'.
    ENDIF.
  ENDIF.
*- U1 End

ENDFORM.                    " get_gl_item1
*&---------------------------------------------------------------------*
*&      Form  get_gl_item2
*&---------------------------------------------------------------------*
FORM get_gl_item2.

*  RANGES: lr_bstat FOR bkpf-bstat.

  SELECT bukrs
  gjahr belnr xblnr awkey awtyp budat bldat
  cpudt usnam tcode blart waers stgrd stblg stjah
  FROM bkpf
  INTO CORRESPONDING FIELDS OF TABLE i_bkpf
          WHERE bukrs = p_bukrs
*            AND bstat IN lr_bstat   "for index???
            AND budat IN s_budat
            AND blart IN s_blart
            AND belnr IN s_belnr
          %_HINTS ORACLE 'FIRST_ROWS(10)'
** Furong on 05/23/12 for sap tuning
ORACLE 'INDEX (BKPF "BKPF~0")'.
** End on 05/23/12

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bkpf USING '2'.
  ENDIF.
*- U1 End

*  IF sy-dbcnt > 2000.
*    __popup '' '2000+ data selected. Do you want to proceed?' ''.
*    IF gv_answer = 'N'. LEAVE PROGRAM.  ENDIF.
*  ENDIF.

  __cls t_line.
*  LOOP AT i_bkpf.
  SELECT * FROM bseg
           APPENDING CORRESPONDING FIELDS OF TABLE t_line
           FOR ALL ENTRIES IN i_bkpf
           WHERE bukrs = p_bukrs
             AND gjahr = i_bkpf-gjahr
             AND belnr = i_bkpf-belnr
             AND hkont = g_hkont
             AND zuonr IN s_zuonr.
*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bseg.
  ENDIF.
*- U1 End

*  ENDLOOP.

ENDFORM.                    " get_gl_item2
*&---------------------------------------------------------------------*
*&      Form  process_line
*&---------------------------------------------------------------------*
FORM process_line.

  LOOP AT t_line.
    it_output = t_line.

* Header
    IF skb1-mitkz = space AND skb1-xkres = space.
      READ TABLE i_bkpf INTO lv_bkpf
         WITH KEY gjahr = t_line-gjahr
                  belnr = t_line-belnr
                  BINARY SEARCH.
    ELSE.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lv_bkpf
          FROM bkpf
          WHERE bukrs = p_bukrs
            AND gjahr = t_line-gjahr
            AND belnr = t_line-belnr.
*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_bkpf_2.
      ENDIF.
*- U1 End
    ENDIF.

* Delete reversed document
    IF lv_bkpf-stblg <> space.
*.....get reverse reason.
      IF lv_bkpf-stgrd = space.
        SELECT SINGLE stgrd FROM bkpf INTO lv_bkpf-stgrd
            WHERE bukrs = p_bukrs
              AND gjahr = lv_bkpf-stjah
              AND belnr = lv_bkpf-stblg.
*- U1 Start
        IF p_arch EQ 'X'.
          PERFORM archive_read_bkpf_3.
        ENDIF.
*- U1 End
      ENDIF.
      IF lv_bkpf-stgrd = '01' OR lv_bkpf-stgrd = '03'.
*       delete it_output index sy-tabix.
        IF p_rev = space. CONTINUE.  ENDIF.
      ENDIF.
    ENDIF.

* Delete MM-IV reversed document
    IF lv_bkpf-awtyp = 'RMRP'.
      SELECT SINGLE * FROM rbkp
         WHERE belnr = lv_bkpf-awkey(10)
           AND gjahr = lv_bkpf-awkey+10(4).
      IF rbkp-stblg <> space.
        lv_bkpf-stgrd = '03'.
        IF p_rev = space. CONTINUE.  ENDIF.
*- U1 Start
      ELSEIF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_rbkp.
        IF lv_bkpf-stgrd = '03'..
          IF p_rev = space. CONTINUE.  ENDIF.
        ENDIF.
*- U1 End
      ENDIF.
    ENDIF.

* header info.
    MOVE-CORRESPONDING lv_bkpf TO it_output.
*    it_output-blart  = bkpf-blart.
*    it_output-tcode  = bkpf-tcode.
*    it_output-xblnr  = bkpf-xblnr.
*    it_output-stgrd  = bkpf-stgrd.  "rev.reason
*    it_output-stblg  = bkpf-stblg.  "rev.doc
*    it_output-stjah  = bkpf-stjah.  "rev.year

* Name
    CASE g_koart.
      WHEN 'K'.
        SELECT SINGLE * FROM lfa1 WHERE lifnr = it_output-lifnr.
        IF lfa1-xcpdk = 'X'.  "Onetime
          SELECT SINGLE name1 INTO it_output-name1 FROM bsec
             WHERE bukrs = p_bukrs
               AND gjahr = t_line-gjahr
               AND belnr = t_line-belnr
               AND buzei = t_line-buzei.
        ELSE.
          it_output-name1 = lfa1-name1.
        ENDIF.
        it_output-accnt = it_output-lifnr.
      WHEN 'D'.
        SELECT SINGLE * FROM kna1 WHERE kunnr = it_output-kunnr.
        it_output-name1 = kna1-name1.
        it_output-accnt = it_output-kunnr.
      WHEN OTHERS.
        READ TABLE gt_gl WITH KEY saknr = it_output-hkont
        BINARY SEARCH.

        it_output-name1 = gt_gl-txt20.
        it_output-accnt = it_output-hkont.
        it_output-fkbe1 = ska1-func_area.

        IF p_eord = 'X' AND it_output-lifnr = space
        AND it_output-matnr <> space.
          CLEAR it_eord.
          READ TABLE it_eord WITH KEY matnr = it_output-matnr
                             BINARY SEARCH.
          it_output-lifnr = it_eord-lifnr.
        ENDIF.

        IF p_vcust = 'X' AND it_output-lifnr = space.
          SELECT SINGLE lifnr INTO it_output-lifnr
           FROM bseg
           WHERE bukrs = p_bukrs
             AND gjahr = t_line-gjahr
             AND belnr = t_line-belnr
             AND koart = 'K'.
*- U1 Start
          IF p_arch EQ 'X' AND sy-subrc <> 0.
            PERFORM archive_read_bseg_2 USING 'K'.
          ENDIF.
*- U1 End
        ENDIF.

        IF p_povnd = 'X'.
          SELECT SINGLE lifnr INTO t_line-lifnr
            FROM ekko
            WHERE ebeln = t_line-zuonr(10).
*- U1 Start
          IF p_arch EQ 'X' AND sy-subrc <> 0.
            PERFORM archive_read_ekko.
          ENDIF.
*- U1 End
        ENDIF.


        IF p_vcust = 'X' AND it_output-kunnr = space.
          SELECT SINGLE kunnr INTO it_output-kunnr
           FROM bseg
           WHERE bukrs = p_bukrs
             AND gjahr = t_line-gjahr
             AND belnr = t_line-belnr
             AND koart = 'D'.
*- U1 Start
          IF p_arch EQ 'X' AND sy-subrc <> 0.
            PERFORM archive_read_bseg_2 USING 'K'.
          ENDIF.
*- U1 End
        ENDIF.
    ENDCASE.
*    select single * from aufk where aufnr = it_output-aufnr.
*    if sy-subrc = 0.
*      select single * from anli where objnr = aufk-objnr.
*      it_output-ktext = aufk-ktext.
*    else.
*      clear: anli-ANLN1.
*    endif.
*    it_output-anln1 = anli-anln1.

    it_output-koart = g_koart.

*Filter Vendor
    CHECK it_output-lifnr IN s_lifnr.

*GRIR...
    it_output-awkey  = lv_bkpf-awkey(10).
    IF lv_bkpf-awtyp = 'RMRP'
    AND ( it_output-bschl = '86' OR it_output-bschl = '96' ).
      DATA: l_buzei LIKE rseg-buzei.
      l_buzei = it_output-buzei - 1.
*FIXME multiple... entry...
      SELECT SINGLE * FROM rseg
        WHERE bukrs = p_bukrs
          AND belnr = lv_bkpf-awkey(10)
          AND gjahr = lv_bkpf-awkey+10(4)
          AND matnr = it_output-matnr
          AND ebeln = it_output-zuonr(10)
          AND ebelp = it_output-zuonr+10(5)
          AND menge = it_output-menge.
      IF sy-subrc = 0 AND rseg-lfbnr <> space.
        it_output-awkey  = rseg-lfbnr.
*- U1 Start
      ELSEIF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_rseg.
*- U1 End
      ENDIF.
*       select single * from ekbe
*         where ebeln = it_output-zuonr(10)
*           and ebelp = it_output-zuonr+10(5)
*           and gjahr = bkpf-awkey+10(4)
*           and belnr = bkpf-awkey(10)
    ENDIF.

    it_output-waers  = lv_bkpf-waers.  "Curr
* chg sign
    IF it_output-shkzg = 'H'.
      it_output-wrbtr = -1 * it_output-wrbtr.
      it_output-dmbtr = -1 * it_output-dmbtr.
      it_output-menge = -1 * it_output-menge.
    ENDIF.

* PA segment
    IF NOT t_line-paobjnr IS INITIAL.
      SELECT SINGLE paobjnr kmland prodh artnr kndnr
        INTO gw_pa
        FROM ce4h201_acct
        WHERE aktbo = 'X'
          AND paobjnr = t_line-paobjnr.

*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_ce4h201_acct.
      ENDIF.
*- U1 End

      MOVE-CORRESPONDING gw_pa TO it_output.
    ENDIF.

    APPEND it_output. "index sy-tabix.
    CLEAR it_output.
  ENDLOOP.

ENDFORM.                    " process_line
*&---------------------------------------------------------------------*
*&      Form  chk_opt
*&---------------------------------------------------------------------*
FORM chk_opt.
  IF sy-tcode = 'ZFI30'.
    CLEAR  p_eord.
    LOOP AT SCREEN.
      IF screen-group1 = 'DIS'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " chk_opt
*&---------------------------------------------------------------------*
*&      Form  get_masters
*&---------------------------------------------------------------------*
FORM get_masters CHANGING l_rc LIKE sy-subrc.

  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  CHECK sy-subrc = 0.

  SELECT skb1~saknr mitkz xkres xopvw skat~txt20
     INTO TABLE gt_gl
     FROM skb1
     INNER JOIN skat
        ON skat~saknr = skb1~saknr
       WHERE bukrs = p_bukrs
         AND skb1~saknr IN s_hkont
         AND spras = sy-langu
         AND ktopl = t001-ktopl.

  IF sy-dbcnt = 0.
    l_rc = 1.
  ELSE.
    SORT gt_gl BY saknr.
  ENDIF.

*allow only single date
  IF p_eord = 'X'.
    SELECT * INTO TABLE it_eord FROM eord
       WHERE vdatu <= s_budat-low
         AND bdatu >= s_budat-low
         AND ebeln = space.
    SORT it_eord BY matnr.
  ENDIF.


ENDFORM.                    " get_masters
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM alv_build_eventtab CHANGING et_alv_events TYPE slis_t_event.

  DATA: ls_alv_events    TYPE slis_alv_event,
        l_dummy_ucomm    LIKE sy-ucomm,
        l_dummy_selfield TYPE slis_selfield,
        l_dummy_excl_tab TYPE slis_t_extab.


  REFRESH: et_alv_events.

** event 'TOP_OF_LIST'.
*  CLEAR ls_alv_events.
*  ls_alv_events-name = slis_ev_top_of_list.
*  ls_alv_events-form = 'ALV_TOP_OF_LIST'.
*  APPEND ls_alv_events TO et_alv_events.
*
** event 'END_OF_LIST'.
*  CLEAR ls_alv_events.
*  ls_alv_events-name = slis_ev_end_of_list.
*  ls_alv_events-form = 'ALV_END_OF_LIST'.
*  APPEND ls_alv_events TO et_alv_events.

* event 'PF_STATUS_SET'.
  CLEAR ls_alv_events.
  ls_alv_events-name = slis_ev_pf_status_set.
  ls_alv_events-form = 'ALV_SET_PF_STATUS'.
  APPEND ls_alv_events TO et_alv_events.

* event 'USER_COMMAND'.
  CLEAR ls_alv_events.
  ls_alv_events-name = slis_ev_user_command.
  ls_alv_events-form = 'ALV_USER_COMMAND'.
  APPEND ls_alv_events TO et_alv_events.

* callback forms.
  IF 1 = 0.
*    PERFORM alv_top_of_list.
*    PERFORM alv_end_of_list.
    PERFORM alv_set_pf_status USING l_dummy_excl_tab.
    PERFORM alv_user_command  USING l_dummy_ucomm
                                    l_dummy_selfield.

  ENDIF.
ENDFORM.                               " ALV_BUILD_EVENTTAB
*---------------------------------------------------------------------*
*       FORM ALV_SET_PF_STATUS                                        *
*---------------------------------------------------------------------*
*       for ALV processing                                            *
*---------------------------------------------------------------------*
FORM alv_set_pf_status USING excl_tab TYPE slis_t_extab.

  DATA: l_excl_tab TYPE slis_extab.

  SET PF-STATUS 'ALV_STATUS' EXCLUDING excl_tab.

ENDFORM.                               " alv_set_pf_status
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
FORM sort_build.
  CLEAR: gt_sort[], gt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'ACCNT'.
  ls_sort-spos = 1.    "---> KEY
  ls_sort-up = 'X'.
  APPEND ls_sort TO gt_sort.

*  CLEAR ls_sort.
*  ls_sort-fieldname = 'MATNR'.
*  ls_sort-spos = 2.
*  ls_sort-up = 'X'.
*  ls_sort-subtot = 'X'.
* ls_sort-group = '*'.
*  APPEND ls_sort TO gt_sort.
ENDFORM.                    " sort_build
*ALV
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: l_cu LIKE t001-waers.
* FIELD SETTING
  l_cu = t001-waers.

  PERFORM field_setting TABLES gt_fieldcat USING :
   'KOART'  'AccType'    '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'UMSKZ'  'Sp.G/L'     '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'ACCNT'  'ACCOUNT'    '08' ' ' 'L'  ' '  '  ' ' ' ' ',
   'NAME1'  'NAME'       '20' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FKBER'  'FA'         '04' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KOSTL'  'CC'         '06' ' ' 'L'  ' '  '  ' ' ' ' ',
   'AUFNR'  'ORDER'      '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'DMBTR'  'L.Amount'   '15' ' ' 'R'  ' '  l_cu ' ' 'X',
   'ZUONR'  'Assignment' '18' ' ' 'L'  ' '  '  ' ' ' ' ',
   'MATNR'  'Material'   '18' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BLART'  'DT'         '02' ' ' 'L'  ' '  '  ' ' ' ' ',
   'TCODE'  'TCD'        '04' ' ' 'L'  ' '  '  ' ' ' ' ',
   'EBELN'  'PO'         '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'EBELP'  'POitm'      '05' ' ' 'L'  ' '  '  ' ' ' ' ',
   'GJAHR'  'Year'       '04' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BUDAT'  'Pst.Date'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BELNR'  'Document'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BUZEI'  'Item'       '05' ' ' 'L'  ' '  '  ' ' ' ' ',
   'XBLNR'  'Reference'  '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'AWKEY'  'OrgRef'     '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'SHKZG'  'D/C'        '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'MENGE'  'Qty'        '13' ' ' 'R'  ' '  '  ' ' ' 'X',
   'BSCHL'  'PstKey'     '02' ' ' 'L'  ' '  '  ' ' ' ' ',
   'STGRD'  'RV'         '02' ' ' 'L'  ' '  '  ' ' ' ' ',
   'XNEGP'  'X'          '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'SGTXT'  'line text'  '20' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BLDAT'  'Doc.Date'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'CPUDT'  'CPU.Date'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'USNAM'  'User'       '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'LIFNR'  'Vendor'     '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KUNNR'  'Customer'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
*   'WRBTR'  'T.Amount'   '15' ' ' 'R'  ' '  ' '  ' ' ' ',
   'FKBE1'  'FA_1'       '04' ' ' 'R'  ' '  '  ' ' ' ' ',

   'PAOBJNR'  'PA-SEG#'   '08' ' ' 'R'  ' '  '  ' ' ' ' ',
   'PAKMLAND' 'PA-LAND'   '03' ' ' 'L'  ' '  '  ' ' ' ' ',
   'PAPRODH'  'PA-PHIA'   '15' ' ' 'L'  ' '  '  ' ' ' ' ',
   'PAARTNR'  'PA-PROD'   '18' ' ' 'L'  ' '  '  ' ' ' ' ',
   'PAKNDNR'  'PA-CUST'   '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KZUST'    'RSN'       '05' ' ' 'C'  ' '  '  ' ' ' ' '.


* build event table.
  PERFORM alv_build_eventtab CHANGING gt_events.
  g_repid = sy-repid.

* PERFORM display_alv.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_user_command  = c_fnam_cos_user_command
      i_callback_pf_status_set = c_fnam_cos_pf_status
      it_events                = gt_events
      is_layout                = gs_layout
      is_variant               = g_variant
      it_fieldcat              = gt_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


ENDFORM.                    " display_alv
*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND                                         *
*---------------------------------------------------------------------*
*       user command for ALV processing                               *
*---------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA: BEGIN OF buztab OCCURS 20,
         bukrs(4),
         belnr(10),
         gjahr(4),
         buzei(3),
         flaen(10),
        END OF buztab.
  DATA: tcode(4),
        buztab-zeile        LIKE sy-tabix,
        x_nochange(1)       TYPE c,
        x_commit(1)         TYPE c.

  CHECK r_ucomm = '&IC1'.                       "Pick

  READ TABLE it_output INDEX rs_selfield-tabindex INTO g_output.
  CHECK sy-subrc = 0.

  MOVE 'FBL1' TO tcode.
  buztab-zeile = 1.  "sy-lilli - 5.
  REFRESH buztab.
  buztab-bukrs = p_bukrs.
  buztab-belnr = g_output-belnr.
  buztab-gjahr = g_output-gjahr.
  buztab-buzei = g_output-buzei.
  APPEND buztab.

  CALL DIALOG 'RF_ZEILEN_ANZEIGE'
    EXPORTING
      buztab
      buztab-zeile
      tcode

    IMPORTING
      buztab
      x_commit.

*        set parameter id:'BLN' field it_output-belnr,
*                         'BUK' field p_bukrs,
*                         'GJR' field it_output-gjahr.
*        call transaction 'FB03' and skip first screen.

ENDFORM.                               "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  get_assignment
*&---------------------------------------------------------------------*
FORM get_assignment.

*get assignment from material & PO
  DATA: l_ebeln LIKE ekpo-ebeln,
        l_ebelp LIKE ekpo-ebelp.

  IF NOT s_matnr2[] IS INITIAL.
    s_zuonr-sign = 'I'.
    s_zuonr-option = 'EQ'.
    SELECT ebeln ebelp INTO (l_ebeln, l_ebelp)
      FROM ekpo
      WHERE matnr IN s_matnr2.
      CONCATENATE l_ebeln l_ebelp INTO s_zuonr-low.
      APPEND s_zuonr.
    ENDSELECT.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekpo.
      s_zuonr-sign = 'I'.
      s_zuonr-option = 'EQ'.
      LOOP AT gt_ekpo_a.
        CONCATENATE gt_ekpo_a-ebeln gt_ekpo_a-ebelp INTO s_zuonr-low.
        APPEND s_zuonr.
      ENDLOOP.
    ENDIF.
*- U1 End
  ENDIF.

ENDFORM.                    " get_assignment
*&---------------------------------------------------------------------*
*&      Form  get_bseg_data
*&---------------------------------------------------------------------*
FORM get_bseg_data.
  DESCRIBE TABLE t_line LINES sy-index.

  CHECK sy-index > 0.

  SELECT  gjahr belnr buzei
          menge matnr lifnr
          kunnr empfb paobjnr
          pasubnr
  FROM bseg INTO CORRESPONDING FIELDS OF TABLE i_bseg
    FOR ALL ENTRIES IN t_line
    WHERE bukrs = p_bukrs
      AND gjahr = t_line-gjahr
      AND belnr = t_line-belnr
      AND buzei = t_line-buzei.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bseg_3.
  ENDIF.
*- U1 End

  DATA: $ix TYPE i.

  SORT i_bseg BY gjahr belnr buzei.

  LOOP AT t_line.
    $ix = sy-tabix.
    READ TABLE i_bseg WITH KEY gjahr = t_line-gjahr
                               belnr = t_line-belnr
                               buzei = t_line-buzei
                               BINARY SEARCH.

    t_line-menge = i_bseg-menge.
    t_line-matnr = i_bseg-matnr.
    t_line-lifnr = i_bseg-lifnr.
    t_line-kunnr = i_bseg-kunnr.

    t_line-paobjnr = i_bseg-paobjnr.


    MODIFY t_line INDEX $ix.
  ENDLOOP.

ENDFORM.                    " get_bseg_data
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = p_text
      textline2      = p_text2
      titel          = 'Check!'
      cancel_display = p_canc
    IMPORTING
      answer         = p_answer.


ENDFORM.                    " POP_UP

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
*&      Form  read_inforecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_reason.

  TYPES: BEGIN OF ty_lifnr,
           lifnr TYPE lifnr,
         END OF ty_lifnr.

  TYPES: BEGIN OF ty_a018,
           matnr TYPE matnr,
           lifnr TYPE lifnr,
           datbi TYPE kodatbi,
           datab TYPE kodatab,
           knumh TYPE knumh,
         END OF ty_a018.

  TYPES: BEGIN OF ty_konp,
           knumh TYPE knumh,
           kschl TYPE kscha,
           kbetr TYPE kbetr_kond,
           kpein TYPE kpein,
           kmein TYPE kmein,
           kzust TYPE kzust,
         END OF ty_konp.

  DATA: lt_lifnr TYPE TABLE OF ty_lifnr WITH HEADER LINE,
        lt_a018  TYPE TABLE OF ty_a018  WITH HEADER LINE,
        lt_konp  TYPE TABLE OF ty_konp  WITH HEADER LINE,
        l_date   TYPE sydatum,       " Date
        l_kbter  TYPE kbetr_kond,    " Rate
        l_kmein  TYPE kmein,         " UoM
        l_gpreis TYPE zgpreis,
        l_peinh  TYPE peinh,
        l_pmeht  TYPE pmeht,
        l_kpein  TYPE kpein,
        l_kzust  TYPE kzust,
        l_cnt    TYPE i.

  __cls : it_matnr.

  LOOP AT it_output.
    it_matnr-matnr = it_output-matnr.
    it_matnr-lifnr = it_output-lifnr.
    COLLECT it_matnr.
  ENDLOOP.

  CHECK sy-dbcnt <> 0.

  SELECT matnr lifnr datbi datab knumh
    INTO TABLE lt_a018
    FROM a018
    FOR ALL ENTRIES IN it_matnr
   WHERE kappl = 'M'
     AND kschl = 'PB00'
     AND matnr = it_matnr-matnr
     AND ekorg = c_ekorg
     AND esokz = '0'
     AND datab =< s_budat-low
     AND datbi >= s_budat-low.

  CHECK sy-dbcnt <> 0.

  SELECT konp~knumh konp~kschl konp~kbetr
         konp~kpein konp~kmein konh~kzust
    INTO TABLE lt_konp
    FROM konp
    INNER JOIN konh
       ON konh~knumh = konp~knumh
    FOR ALL ENTRIES IN lt_a018
   WHERE konp~knumh = lt_a018-knumh
     AND konp~kappl = 'M'
   AND ( konp~kschl = 'PB00' OR        " Gross price
         konp~kschl = 'ZTIR' ).

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_konp TABLES lt_konp lt_a018.
  ENDIF.
*- U1 End

  DATA : $ix TYPE i, $fr TYPE i, $knumh TYPE knumh.

  SORT lt_a018 BY matnr lifnr ASCENDING
                        datab DESCENDING.

  SORT lt_konp BY knumh.

  LOOP AT it_output.
    $ix = sy-tabix.
    CLEAR $knumh.
    READ TABLE lt_a018 WITH KEY matnr = it_output-matnr
                                lifnr = it_output-lifnr
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      $fr = sy-tabix.
      LOOP AT lt_a018 FROM $fr.
        IF lt_a018-matnr NE it_output-matnr OR
           lt_a018-lifnr NE it_output-lifnr.
          EXIT.
        ENDIF.
        $knumh = lt_a018-knumh.
        IF it_output-budat >= lt_a018-datab AND
           it_output-budat <= lt_a018-datbi.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF NOT $knumh IS INITIAL.
        READ TABLE lt_konp
        WITH KEY knumh = $knumh
        BINARY SEARCH.
        IF sy-subrc EQ 0.
          it_output-kzust = lt_konp-kzust.
          MODIFY it_output INDEX $ix TRANSPORTING kzust.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " read_inforecord
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf USING p_flag TYPE char1.

  TYPES: BEGIN OF ty_bkpf,
       bukrs TYPE bukrs,
       belnr TYPE belnr_d,
       gjahr TYPE gjahr,
       blart TYPE blart,
       bldat TYPE bldat,
       budat TYPE budat,
       awkey TYPE awkey,
         archivekey TYPE arkey,
         archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  IF p_flag = '1'.
    CHECK NOT t_line[] IS INITIAL.

    CLEAR lt_inx_bkpf[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
      FROM (l_gentab)
      FOR ALL ENTRIES IN t_line
     WHERE bukrs = p_bukrs
       AND gjahr = t_line-gjahr
       AND belnr = t_line-belnr.
  ELSEIF p_flag = '2'.
    CLEAR lt_inx_bkpf[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
      FROM (l_gentab)
     WHERE bukrs = p_bukrs
       AND budat IN s_budat
       AND blart IN s_blart
       AND belnr IN s_belnr.
  ENDIF.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT gt_bkpf_a.
    CLEAR gs_bkpf_a.
    MOVE-CORRESPONDING gt_bkpf_a TO gs_bkpf_a.
    INSERT gs_bkpf_a INTO TABLE i_bkpf.
*    APPEND gs_bkpf_a TO i_bkpf.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_2 .

  TYPES: BEGIN OF ty_bkpf,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        blart TYPE blart,
        bldat TYPE bldat,
        budat TYPE budat,
        awkey TYPE awkey,
          archivekey TYPE arkey,
          archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
   WHERE bukrs = p_bukrs
     AND gjahr = t_line-gjahr
     AND belnr = t_line-belnr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  READ TABLE gt_bkpf_a INDEX 1.
  CLEAR lv_bkpf.
  MOVE-CORRESPONDING gt_bkpf_a TO lv_bkpf.

ENDFORM.                    " ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_3 .

  TYPES: BEGIN OF ty_bkpf,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        blart TYPE blart,
        bldat TYPE bldat,
        budat TYPE budat,
        awkey TYPE awkey,
          archivekey TYPE arkey,
          archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
   WHERE bukrs = p_bukrs
     AND gjahr = lv_bkpf-stjah
     AND belnr = lv_bkpf-stblg.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  READ TABLE gt_bkpf_a INDEX 1.
  IF sy-subrc = 0.
    lv_bkpf-stgrd = gt_bkpf_a-stgrd.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_BKPF_3
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekko .

  TYPES: BEGIN OF ty_ekko,
       ebeln TYPE ebeln,
       lifnr TYPE elifn,
         archivekey TYPE arkey,
         archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekko.

  DATA: l_handle    TYPE sytabix,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekko TYPE TABLE OF ty_ekko,
        ls_inx_ekko TYPE ty_ekko.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKBE_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_ekko.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_ekko
    FROM (l_gentab)
   WHERE ebeln = t_line-zuonr(10).

  IF sy-subrc = 0.
    t_line-lifnr = ls_inx_ekko-lifnr.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekpo .

  TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         matnr TYPE matnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekpo.

  DATA: l_handle    TYPE sytabix,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekpo TYPE TABLE OF ty_ekpo,
        ls_inx_ekpo TYPE ty_ekpo.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKBE_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekpo[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekpo
    FROM (l_gentab)
   WHERE matnr IN s_matnr2.

  CHECK NOT lt_inx_ekpo[] IS INITIAL.

  SORT lt_inx_ekpo BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_inx_ekpo COMPARING ebeln ebelp.

  CLEAR: gt_ekpo_a, gt_ekpo_a[].
  LOOP AT lt_inx_ekpo INTO ls_inx_ekpo.
    MOVE-CORRESPONDING ls_inx_ekpo TO gt_ekpo_a.
    APPEND gt_ekpo_a.  CLEAR gt_ekpo_a.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_RSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_rseg .

  TYPES: BEGIN OF ty_rseg,
         belnr TYPE re_belnr,
         gjahr TYPE gjahr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_rseg.

  DATA: l_handle    TYPE sytabix,
        lt_rseg     TYPE TABLE OF rseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_rseg TYPE TABLE OF ty_rseg,
        ls_inx_rseg TYPE ty_rseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZRBKP_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_rseg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_rseg
    FROM (l_gentab)
   WHERE belnr = lv_bkpf-awkey(10)
     AND gjahr = lv_bkpf-awkey+10(4).

  CHECK NOT lt_inx_rseg[] IS INITIAL.

  LOOP AT lt_inx_rseg INTO ls_inx_rseg.
    CLEAR: lt_rseg, lt_rseg[].
    CALL FUNCTION 'ASH_MM_REBEL_READ'
      EXPORTING
        i_archivekey           = ls_inx_rseg-archivekey
        i_offset               = ls_inx_rseg-archiveofs
      TABLES
        et_rseg                = lt_rseg
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

    CLEAR lt_rseg.
    READ TABLE lt_rseg WITH KEY bukrs = p_bukrs
                                matnr = it_output-matnr
                                ebeln = it_output-zuonr(10)
                                ebelp = it_output-zuonr+10(5)
                                menge = it_output-menge.

    IF sy-subrc = 0 AND lt_rseg-lfbnr <> space.
      it_output-awkey  = lt_rseg-lfbnr.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_RSEG
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_rbkp .

  TYPES: BEGIN OF ty_rbkp,
         belnr TYPE re_belnr,
         gjahr TYPE gjahr,
         stblg TYPE re_stblg,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_rbkp.

  DATA: l_handle    TYPE sytabix,
        lt_rbkp     TYPE TABLE OF rbkp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_rbkp TYPE TABLE OF ty_rbkp,
        ls_inx_rbkp TYPE ty_rbkp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZRBKP_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_rbkp.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_rbkp
    FROM (l_gentab)
   WHERE belnr = lv_bkpf-awkey(10)
     AND gjahr = lv_bkpf-awkey+10(4).

  IF ls_inx_rbkp-stblg <> space.
    lv_bkpf-stgrd = '03'.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bseg .

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bseg.

  DATA: l_handle    TYPE sytabix,
        lt_bseg     TYPE TABLE OF bseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bseg TYPE TABLE OF ty_bseg,
        ls_inx_bseg TYPE ty_bseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBSEG_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT i_bkpf[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bseg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bseg
    FROM (l_gentab)
    FOR ALL ENTRIES IN i_bkpf
   WHERE bukrs = p_bukrs
     AND gjahr = i_bkpf-gjahr
     AND belnr = i_bkpf-belnr.

  CHECK NOT lt_inx_bseg[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bseg_a, gt_bseg_a[].
  LOOP AT lt_inx_bseg INTO ls_inx_bseg.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bseg-archivekey
        offset                    = ls_inx_bseg-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bseg, lt_bseg[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BSEG'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bseg
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bseg[] IS INITIAL.

    DELETE lt_bseg WHERE hkont <> g_hkont
                      OR zuonr NOT IN s_zuonr.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bseg INTO TABLE gt_bseg_a.
  ENDLOOP.

  SORT gt_bseg_a.
  DELETE ADJACENT DUPLICATES FROM gt_bseg_a COMPARING ALL FIELDS.

  LOOP AT gt_bseg_a.
    CLEAR t_line.
    MOVE-CORRESPONDING gt_bseg_a TO t_line.
    APPEND t_line.  CLEAR t_line.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BSEG
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSEG_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1988   text
*----------------------------------------------------------------------*
FORM archive_read_bseg_2  USING  p_koart.

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         koart TYPE koart,
         lifnr TYPE lifnr,
         kunnr TYPE kunnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bseg.

  DATA: l_handle    TYPE sytabix,
        lt_bseg     TYPE TABLE OF bseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bseg TYPE TABLE OF ty_bseg,
        ls_inx_bseg TYPE ty_bseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBSEG_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_bseg.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_bseg
    FROM (l_gentab)
   WHERE bukrs = p_bukrs
     AND gjahr = t_line-gjahr
     AND belnr = t_line-belnr
     AND koart = p_koart.

  CHECK sy-subrc = 0.

  IF p_koart = 'K'.
    it_output-lifnr = ls_inx_bseg-lifnr.
  ELSE.
    it_output-kunnr = ls_inx_bseg-kunnr.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_BSEG_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSEG_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bseg_3 .

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bseg.

  DATA: l_handle    TYPE sytabix,
        lt_bseg     TYPE TABLE OF bseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bseg TYPE TABLE OF ty_bseg,
        ls_inx_bseg TYPE ty_bseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBSEG_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT t_line[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bseg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bseg
    FROM (l_gentab)
    FOR ALL ENTRIES IN t_line
   WHERE bukrs = p_bukrs
     AND gjahr = t_line-gjahr
     AND belnr = t_line-belnr
     AND buzei = t_line-buzei.

  CHECK NOT lt_inx_bseg[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bseg_a, gt_bseg_a[].
  LOOP AT lt_inx_bseg INTO ls_inx_bseg.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bseg-archivekey
        offset                    = ls_inx_bseg-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bseg, lt_bseg[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BSEG'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bseg
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bseg[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bseg INTO TABLE gt_bseg_a.
  ENDLOOP.

  SORT gt_bseg_a BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_bseg_a COMPARING bukrs belnr gjahr buzei.

  LOOP AT gt_bseg_a.
    CLEAR i_bseg.
    MOVE-CORRESPONDING gt_bseg_a TO i_bseg.
    APPEND i_bseg.  CLEAR i_bseg.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BSEG_3
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CE4H201_ACCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ce4h201_acct .

  TYPES: BEGIN OF ty_ce4h201_acct,
         aktbo   TYPE aktbo,
         paobjnr TYPE rkeobjnr,
         pasubnr TYPE rkesubnr,
         kndnr   TYPE kunde_pa,
         artnr   TYPE artnr,
         kmland  TYPE land1_gp,
         prodh   TYPE prodh_d,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ce4h201_acct.

  DATA: l_handle    TYPE sytabix,
        lt_ce4h201_acct TYPE TABLE OF ce4h201_acct WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ce4h201_acct TYPE TABLE OF ty_ce4h201_acct,
        ls_inx_ce4h201_acct TYPE ty_ce4h201_acct.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZCE4H201_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ce4h201_acct[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ce4h201_acct
    FROM (l_gentab)
   WHERE aktbo = 'X'
     AND paobjnr = t_line-paobjnr.

  CHECK NOT lt_inx_ce4h201_acct[] IS INITIAL.

  READ TABLE lt_inx_ce4h201_acct INTO ls_inx_ce4h201_acct INDEX 1.

  gw_pa-paobjnr  = ls_inx_ce4h201_acct-paobjnr.
  gw_pa-pakmland = ls_inx_ce4h201_acct-kmland.
  gw_pa-paprodh  = ls_inx_ce4h201_acct-prodh.
  gw_pa-paartnr  = ls_inx_ce4h201_acct-artnr.
  gw_pa-pakndnr  = ls_inx_ce4h201_acct-kndnr.

ENDFORM.                    " ARCHIVE_READ_CE4H201_ACCT
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_KONP  text
*      -->P_LT_A018  text
*----------------------------------------------------------------------*
FORM archive_read_konp  TABLES  pt_konp STRUCTURE gt_konp_a2
                                pt_a018 STRUCTURE gt_a018_a.

  TYPES: BEGIN OF ty_konp,
         knumh    TYPE knumh,
         kopos    TYPE kopos,
         kappl    TYPE kappl,
         kschl    TYPE kscha,
         loevm_ko TYPE loevm_ko,
         kosrt    TYPE kosrt,
         kzust    TYPE kzust,
         erdat    TYPE erdat,
         ernam    TYPE ernam,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_konp.

  DATA: l_handle    TYPE sytabix,
        lt_konh     TYPE TABLE OF konh WITH HEADER LINE,
        lt_konp     TYPE TABLE OF konp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_konp TYPE TABLE OF ty_konp,
        ls_inx_konp TYPE ty_konp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT pt_a018[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_konp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_konp
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_a018
   WHERE knumh = pt_a018-knumh
     AND kappl = 'M'
     AND ( kschl = 'PB00' OR        " Gross price
           kschl = 'ZTIR' ).

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konh_a, gt_konh_a[], gt_konp_a, gt_konp_a[].
  LOOP AT lt_inx_konp INTO ls_inx_konp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_konp-archivekey
        offset                    = ls_inx_konp-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_konh, lt_konh[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONH'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konh
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konh[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: lt_konp, lt_konp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: lt_konh INTO TABLE gt_konh_a,
                     lt_konp INTO TABLE gt_konp_a.
  ENDLOOP.

  SORT: gt_konh_a, gt_konp_a.
  DELETE ADJACENT DUPLICATES FROM: gt_konh_a COMPARING ALL FIELDS,
                                   gt_konp_a COMPARING ALL FIELDS.
  LOOP AT gt_konp_a.
    CLEAR pt_konp.
    MOVE-CORRESPONDING gt_konp_a TO pt_konp.

    CLEAR gt_konh_a.
    READ TABLE gt_konh_a WITH KEY knumh = gt_konp_a-knumh.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING gt_konh_a TO pt_konp.

    APPEND pt_konp.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSAK
*&---------------------------------------------------------------------*
FORM archive_read_bsak .
  DATA: lt_bsak  LIKE bsak OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.

  PERFORM set_sel_condition_for_bsak CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
*     E_BSIS             =
*     E_BSAS             =
*     E_BSAD             = LT_BSAD
      e_bsak             = lt_bsak
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsak.
      MOVE-CORRESPONDING lt_bsak TO t_line.
      APPEND t_line.
      CLEAR: t_line,
             lt_bsak.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_BSAK
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSAK
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsak
                          CHANGING  pt_selections TYPE rsds_trange.

  IF p_open = 'X'.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             'I'
                                             'LE'
                                             'P_STIDA'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'AUGDT'
                                             'I'
                                             'GT'
                                             'P_STIDA'
                                             'P'
                                    CHANGING pt_selections.

  ELSE.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.
  ENDIF.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSAK
*&---------------------------------------------------------------------*
*&      Form  SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
FORM set_select_field_option
                         USING    p_tablename
                                  p_fieldname
                                  p_sign
                                  p_option
                                  p_selfield
                                  p_param
                         CHANGING pt_selections TYPE rsds_trange.

  DATA:  ls_selopt    LIKE rsdsselopt,
         lt_selopt    TYPE rsds_selopt_t,
         ls_frange    TYPE rsds_frange,
         lt_frange    TYPE rsds_frange_t,
         ls_trange    TYPE rsds_range,
         lt_trange    TYPE rsds_trange.

  DATA: l_name TYPE string.

  FIELD-SYMBOLS: <fs_itab>  TYPE ANY TABLE,
                 <fs_wa>    TYPE any,
                 <fs_value> TYPE any.

  ls_trange-tablename = p_tablename.

  CASE p_param.
    WHEN 'P'.
      l_name = p_selfield.
      ASSIGN (l_name) TO <fs_value>.

      IF NOT <fs_value> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        ls_selopt-sign = p_sign.
        ls_selopt-option = p_option.
        ls_selopt-low = <fs_value>.
        APPEND ls_selopt TO lt_selopt.

        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.

    WHEN 'S'.
      CONCATENATE p_selfield
                  '[]'
             INTO l_name.
      ASSIGN (l_name) TO <fs_itab>.

      IF NOT <fs_itab> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        LOOP AT <fs_itab> ASSIGNING <fs_wa>.
          MOVE-CORRESPONDING <fs_wa> TO ls_selopt.
*          LS_SELOPT = <FS_WA>.
          APPEND ls_selopt TO lt_selopt.
        ENDLOOP.
        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.
  ENDCASE.

  IF NOT lt_frange[] IS INITIAL .
    ls_trange-frange_t = lt_frange[].
    APPEND ls_trange TO lt_trange.
  ENDIF.

  APPEND LINES OF lt_trange TO pt_selections.


ENDFORM.                    " SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSAD
*&---------------------------------------------------------------------*
FORM archive_read_bsad .
  DATA: lt_bsad  LIKE bsad OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.

  PERFORM set_sel_condition_for_bsad CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
*     E_BSIS             =
*     E_BSAS             =
      e_bsad             = lt_bsad
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsad.
      MOVE-CORRESPONDING lt_bsad TO t_line.
      APPEND t_line.
      CLEAR: t_line,
             lt_bsad.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSAD
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSAD
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsad
                          CHANGING  pt_selections TYPE rsds_trange.

  IF p_open = 'X'.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             'I'
                                             'LE'
                                             'P_STIDA'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'AUGDT'
                                             'I'
                                             'GT'
                                             'P_STIDA'
                                             'P'
                                    CHANGING pt_selections.

  ELSE.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.
  ENDIF.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSAD

*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSAS
*&---------------------------------------------------------------------*
FORM archive_read_bsas .
  DATA: lt_bsas  LIKE bsas OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.

  PERFORM set_sel_condition_for_bsas CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
*     E_BSIS             =
      e_bsas             = lt_bsas
*     E_BSAD             = LT_BSAD
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsas.
      MOVE-CORRESPONDING lt_bsas TO t_line.
      APPEND t_line.
      CLEAR: t_line,
             lt_bsas.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSAS
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSAS
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsas
                          CHANGING  pt_selections TYPE rsds_trange.

  IF p_open = 'X'.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BLART'
                                            ' '
                                            ' '
                                            'S_BLART'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'XBLNR'
                                            ' '
                                            ' '
                                            'S_XBLNR'
                                            'S'
                                   CHANGING pt_selections.


    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'ZUONR'
                                             ' '
                                             ' '
                                             'S_ZUONR'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'AUGDT'
                                             'I'
                                             'GT'
                                             'P_STIDA'
                                             'P'
                                    CHANGING pt_selections.

  ELSE.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BLART'
                                            ' '
                                            ' '
                                            'S_BLART'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'XBLNR'
                                            ' '
                                            ' '
                                            'S_XBLNR'
                                            'S'
                                   CHANGING pt_selections.


    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'ZUONR'
                                             ' '
                                             ' '
                                             'S_ZUONR'
                                             'S'
                                    CHANGING pt_selections.

  ENDIF.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSAS
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
FORM archive_read_bsis .
  DATA: lt_bsis  LIKE bsis OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.

  PERFORM set_sel_condition_for_bsis CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
      e_bsis             = lt_bsis
*     E_BSAS             = LT_BSAS
*     E_BSAD             = LT_BSAD
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsis.
      MOVE-CORRESPONDING lt_bsis TO t_line.
      APPEND t_line.
      CLEAR: t_line,
             lt_bsis.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSIS
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsis
                          CHANGING  pt_selections TYPE rsds_trange.

  IF p_open = 'X'.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BLART'
                                            ' '
                                            ' '
                                            'S_BLART'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'XBLNR'
                                            ' '
                                            ' '
                                            'S_XBLNR'
                                            'S'
                                   CHANGING pt_selections.


    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'ZUONR'
                                             ' '
                                             ' '
                                             'S_ZUONR'
                                             'S'
                                    CHANGING pt_selections.

  ELSE.
    PERFORM set_select_field_option USING    'BKPF'
                                             'BUKRS'
                                             'I'
                                             'EQ'
                                             'P_BUKRS'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BKPF'
                                             'BUDAT'
                                             ' '
                                             ' '
                                             'S_BUDAT'
                                             'S'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BELNR'
                                            ' '
                                            ' '
                                            'S_BELNR'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'BLART'
                                            ' '
                                            ' '
                                            'S_BLART'
                                            'S'
                                   CHANGING pt_selections.

    PERFORM set_select_field_option USING   'BKPF'
                                            'XBLNR'
                                            ' '
                                            ' '
                                            'S_XBLNR'
                                            'S'
                                   CHANGING pt_selections.


    PERFORM set_select_field_option USING    'BSEG'
                                             'HKONT'
                                             'I'
                                             'EQ'
                                             'G_HKONT'
                                             'P'
                                    CHANGING pt_selections.

    PERFORM set_select_field_option USING    'BSEG'
                                             'ZUONR'
                                             ' '
                                             ' '
                                             'S_ZUONR'
                                             'S'
                                    CHANGING pt_selections.

  ENDIF.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSIS
