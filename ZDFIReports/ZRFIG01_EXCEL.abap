REPORT ZRFIG01_EXCEL     MESSAGE-ID zmfi
                         NO STANDARD PAGE HEADING
                         LINE-SIZE   130   LINE-COUNT   90.
TYPE-POOLS: slis.
INCLUDE <icon>.
INCLUDE <symbol>.
TABLES  bds_bar_in.

* Printing option for Label
DATA: zoptions LIKE	itcpo OCCURS 0 WITH HEADER LINE.
DATA: zprinter(4) VALUE 'RFL'.

CLASS cl_gui_resources DEFINITION LOAD.

************************************************************************
************************* Global data **********************************
************************************************************************

TABLES: t001,    "Company code
        t003t,   "Document Type Texts
        t005,    "Countris
        t007s,   "Tax Code Names
        bkpf,    "Accounting Document Header
        vbkpf,   "Document Header for Document Parking
        rbkp,     "Document Header: Invoice Receipt
        lfbw,     " Vendor / Company
        t059fb,
        t059z,
        with_item,
        t059p, bsik.

DATA: BEGIN OF new_dir OCCURS 0,
       $index  LIKE sy-tabix,
       dirname LIKE rlgrap-filename,
      END OF new_dir.

DATA: BEGIN OF it_bk OCCURS 100,
      blart    LIKE  bkpf-blart,       "Doc. Type.
      budat    LIKE  bkpf-budat,       "Posting Date
      bldat    LIKE  bkpf-bldat,       "doc. date
      cpudt    LIKE  bkpf-cpudt,       "Enrry data
      usnam    LIKE  bkpf-usnam,       "User Name
      ppnam    LIKE  bkpf-ppnam,       "Parked by
      waers    LIKE  bkpf-waers,       "Currency key
      tcode    LIKE  bkpf-tcode,       "Transaction Code
      xblnr    LIKE  bkpf-xblnr,       "Reference
      bstat    LIKE  bkpf-bstat,       "Doc. Status
      depart   LIKE  adcp-department,  "Dept Name
      xprfg    LIKE  vbkpf-xprfg,      "Doc. Complete
      stblg    LIKE  bkpf-stblg,       "Reverse doc no
      stjah    LIKE  bkpf-stjah,       "Reverse year
      stgrd    LIKE  bkpf-stgrd,       "Reverse reason
      bukrs    LIKE  bkpf-bukrs,       "Company Code
      gjahr    LIKE  bkpf-gjahr,       "Fiscal Year
      belnr    LIKE  bkpf-belnr,       "Doc. No.
      awkey    LIKE bkpf-awkey,        "Object key
      awtyp    LIKE bkpf-awtyp,        "Reference procedure UD1K922256
      revtx(7),                        "Reverse UD1K922256
END OF it_bk.

DATA : BEGIN OF it_vbkpf OCCURS 0,
         belnr  LIKE vbkpf-belnr,
         xprfg  LIKE vbkpf-xprfg,
       END OF it_vbkpf.

DATA: BEGIN OF it_bkpf OCCURS 0.
        INCLUDE STRUCTURE it_bk.
DATA:   chkbox TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF it_bkpf.

DATA : BEGIN OF it_zsfiivprnhd OCCURS 0.
        INCLUDE STRUCTURE zsfiivprnhd.
DATA : END OF it_zsfiivprnhd.

DATA : BEGIN OF it_zsfiivprndt OCCURS 0.
        INCLUDE STRUCTURE zsfiivprndt.
DATA : END OF it_zsfiivprndt.

DATA : BEGIN OF it_zsfiivprnmemo OCCURS 0.
        INCLUDE STRUCTURE zsfiivprnmemo.
DATA : END OF it_zsfiivprnmemo.

DATA : BEGIN OF it_zsfiivprnetc OCCURS 0.
        INCLUDE STRUCTURE zsfiivprnetc.
DATA : END OF it_zsfiivprnetc.

DATA : wa_t_cnt TYPE i,
       wa_cnt   TYPE i.

DATA : BEGIN OF it_cb OCCURS 0,
        kokrs LIKE cobk-kokrs,
        belnr LIKE cobk-belnr,
        gjahr LIKE cobk-gjahr,
        budat LIKE cobk-budat,
        usnam LIKE cobk-usnam,
        bldat LIKE cobk-bldat,
        cpudt LIKE cobk-cpudt,
        blart LIKE cobk-blart,
        kwaer LIKE cobk-kwaer,
        bltxt LIKE cobk-bltxt,
       END OF it_cb.

DATA: BEGIN OF it_ebeln OCCURS 0,
            ebeln TYPE ebeln,
      END   OF it_ebeln.

DATA tmp_dir LIKE rlgrap-filename.
DATA tmp_file LIKE rlgrap-filename.

DATA: BEGIN OF sap_gui.
        INCLUDE STRUCTURE rfcsi.
DATA: END OF sap_gui.
INCLUDE ole2incl.

DATA: excel  TYPE ole2_object,
      books  TYPE ole2_object,
      sheet  TYPE ole2_object,
      cells  TYPE ole2_object,
      subrc LIKE sy-subrc.

DATA out_times TYPE i.
DATA tab_lines LIKE sy-tabix.
DATA pri_fr_line TYPE i.
DATA pri_to_line TYPE i.
DATA x_lin TYPE i.
DATA answer.

****************************** constants ******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __dos.                          " clear & refresh
  append dos_command.clear dos_command.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_bukrs  FOR   bkpf-bukrs DEFAULT 'H201' MEMORY ID buk,
  s_belnr  FOR   bkpf-belnr,
  s_rbelnr FOR   rbkp-belnr,
  s_gjahr  FOR   bkpf-gjahr MEMORY ID gjr.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-012 FOR FIELD r_a.
PARAMETER  r_a RADIOBUTTON GROUP r1." normal
SELECTION-SCREEN COMMENT 38(10) text-013 FOR FIELD r_b.
PARAMETER  r_b RADIOBUTTON GROUP r1." Reposting
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b0.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_blart  FOR   bkpf-blart MEMORY ID bar,
  s_budat  FOR   bkpf-budat,
  s_xblnr  FOR   bkpf-xblnr,
  s_cpudt  FOR   bkpf-cpudt.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-020.
PARAMETER   p_own TYPE c AS CHECKBOX." DEFAULT 'X'.

PARAMETER : p_parked AS CHECKBOX,
            p_posted AS CHECKBOX.
PARAMETER : p_limit  TYPE i  DEFAULT '250'.

PARAMETERS     filename(50) DEFAULT
'C:\SapXls\IV_template.xls'
NO-DISPLAY.

PARAMETERS scr AS CHECKBOX DEFAULT 'X'.
PARAMETERS nex AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b2.

*-------------------------------------------------------*
* Include for ALV
*-------------------------------------------------------*
INCLUDE: zrfig01i_alv.

*-------------------------------------------------------*
AT SELECTION-SCREEN.
*-------------------------------------------------------*
  IF s_bukrs-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-014.
  ENDIF.

  IF s_gjahr-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-015.
  ENDIF.
  IF s_cpudt-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-016.
  ENDIF.
  IF p_parked IS INITIAL AND p_posted IS INITIAL.
    MESSAGE e000(zmfi) WITH text-017.
  ENDIF.

*-------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-------------------------------------------------------*
** Authority check
  AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                  FIELD '*'.
  IF sy-subrc <> 0.
    AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                    FIELD 'FI'.
    IF sy-subrc <> 0.
      p_own = 'X'.
      LOOP AT SCREEN.
        IF screen-name = 'P_OWN'.
          screen-input =  0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-------------------------------------------------------*
INITIALIZATION.
*-------------------------------------------------------*
  p_parked = 'X'.
  p_posted = 'X'.

  s_gjahr-low = sy-datum(4).
  APPEND s_gjahr.

  s_cpudt-low  = sy-datum.
  s_cpudt-high = sy-datum.
  APPEND s_cpudt.

  wa_repid = sy-repid.
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.

*-------------------------------------------------------*
START-OF-SELECTION.
*-------------------------------------------------------*
*-------------------------------------------------------*
END-OF-SELECTION.
*-------------------------------------------------------*
  IF NOT r_a IS INITIAL.
    IF s_rbelnr-low IS INITIAL.
      PERFORM read_bkpf.
    ELSE.
      PERFORM read_rbkp.
    ENDIF.
  ELSEIF NOT r_b IS INITIAL.
    PERFORM read_cobk.
  ENDIF.

  DESCRIBE TABLE it_bkpf LINES sy-index.
  IF sy-index = 0.
    MESSAGE s000(zmfi) WITH text-018.
  ELSEIF sy-index  > p_limit.
    MESSAGE s000(zmfi) WITH 'Too many documents selected'.
    EXIT.
  ENDIF.

  PERFORM make_alv_bkpf.

  CHECK sy-xcode EQ '&PRT'.

  DATA : $awkey TYPE awkey,
         $barcode(20).

  LOOP AT it_bkpf WHERE chkbox EQ 'X'.

    __cls : it_zsfiivprnhd,
            it_zsfiivprndt,
            it_zsfiivprnmemo,
            it_zsfiivprnetc.

    CALL FUNCTION 'Z_FI_FILL_ACCOUNT_DOCUMENT'
         EXPORTING
              bukrs           = it_bkpf-bukrs
              gjahr           = it_bkpf-gjahr
              belnr           = it_bkpf-belnr
              p_parked        = p_parked
              p_posted        = p_posted
              p_own           = p_own
              r_reposting     = r_b
         TABLES
              zsfiivprnhd     = it_zsfiivprnhd
              zsfiivprndt     = it_zsfiivprndt
              zsfiivprnmemo   = it_zsfiivprnmemo
              zsfiivprnetc    = it_zsfiivprnetc
         EXCEPTIONS
              no_doc_found    = 1
              invalid_comapny = 2
              OTHERS          = 3.

    IF sy-subrc EQ 0.
      CONCATENATE it_bkpf-bukrs it_bkpf-awkey(10) it_bkpf-gjahr
       INTO $awkey.
      CLEAR bds_bar_in.

      SELECT SINGLE * FROM bds_bar_in WHERE
              object_key EQ $awkey.
      CONCATENATE '*' bds_bar_in-barcode '*' INTO $barcode.

      PERFORM print_iv TABLES it_zsfiivprnhd
                              it_zsfiivprndt
                              it_zsfiivprnmemo
                              it_zsfiivprnetc
                       USING  $barcode.
    ENDIF.

  ENDLOOP.

*&------------------------------------------------------*
*&      Form  READ BKPF
*&------------------------------------------------------*
FORM read_bkpf.
  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

  REFRESH it_bkpf. CLEAR it_bkpf.

* check and read parked data
  IF p_parked = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = 'V'.
    APPEND r_bstat.
  ENDIF.
* check and read posted data
  IF p_posted = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = ' '.
    APPEND r_bstat.
    r_bstat-low    = 'A'.
    APPEND r_bstat.
    r_bstat-low    = 'B'.
    APPEND r_bstat.
    r_bstat-low    = 'D'.
    APPEND r_bstat.
    r_bstat-low    = 'S'.
    APPEND r_bstat.
  ENDIF.

  IF p_own EQ 'X'.
    r_usnam-sign = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low = sy-uname.
    APPEND r_usnam.
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    APPEND r_ppnam.
  ENDIF.
  SELECT bukrs belnr gjahr bstat
         budat usnam ppnam tcode xblnr
         bldat cpudt blart bktxt waers
         stblg stjah stgrd awkey awtyp
    FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE it_bk
   WHERE bukrs IN s_bukrs
     AND gjahr IN s_gjahr
     AND belnr IN s_belnr
     AND blart IN s_blart
     AND budat IN s_budat
     AND xblnr IN s_xblnr
     AND cpudt IN s_cpudt
     AND bstat IN r_bstat
     AND ( usnam IN r_usnam OR ppnam IN r_ppnam ).

*====JHS MODIFY 2004.01.26
  CLEAR : wa_t_cnt.
  DESCRIBE TABLE it_bk LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbkpf
    FROM vbkpf
    FOR ALL ENTRIES IN it_bk
    WHERE belnr = it_bk-belnr.
  ENDIF.

  LOOP AT it_bk.
*----2004/01/26.   jhs modify
    IF it_bk-bstat = 'V'.
      READ TABLE it_vbkpf WITH KEY belnr = it_bk-belnr
                                   xprfg = 'X'.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
* Begin of changes - UD1K922256
    IF it_bk-awtyp  EQ 'RMRP' .
      SELECT SINGLE *
          FROM rbkp
         WHERE stblg = it_bk-awkey(10)
           AND stjah = it_bk-awkey+10(4).
      IF sy-subrc EQ 0.
        it_bk-revtx = 'REV'.
      ENDIF.
    ENDIF.
* End of changes - UD1K922256
    MOVE-CORRESPONDING it_bk TO it_bkpf.
    APPEND it_bkpf. CLEAR it_bkpf.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_COBK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cobk.

  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

  __cls : it_bkpf, it_cb.

  SELECT kokrs belnr gjahr budat usnam bldat cpudt
         blart kwaer bltxt
     FROM cobk
      INTO CORRESPONDING FIELDS OF TABLE it_cb
    WHERE kokrs IN s_bukrs
      AND gjahr IN s_gjahr
      AND belnr IN s_belnr
*     AND blart IN s_blart
      AND vrgng IN ('RKU1', 'RKU2', 'RKU3')
      AND budat IN s_budat
*      and xblnr in s_xblnr
      AND cpudt IN s_cpudt.
*     AND bstat IN r_bstat
*     AND ( usnam IN r_usnam OR ppnam IN r_ppnam ).

  CLEAR : wa_t_cnt.
  DESCRIBE TABLE it_cb LINES wa_t_cnt.
  IF wa_t_cnt <> 0.
*    MESSAGE w000(zmfi) WITH text-018.
*  ELSE.
    LOOP AT it_cb.
      MOVE-CORRESPONDING it_cb TO it_bkpf.
      MOVE it_cb-kokrs         TO it_bkpf-bukrs.
      MOVE it_cb-kwaer         TO it_bkpf-waers.
      APPEND it_bkpf.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_COBK
*&---------------------------------------------------------------------*
*&      Form  read_rbkp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rbkp.
  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

  __cls it_bkpf.

* check and read parked data
  IF p_parked = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = 'V'.
    APPEND r_bstat.
  ENDIF.
* check and read posted data
  IF p_posted = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = ' '.
    APPEND r_bstat.
    r_bstat-low    = 'A'.  "parked
    APPEND r_bstat.
    r_bstat-low    = 'B'.  "park & completed
    APPEND r_bstat.
    r_bstat-low    = 'E'.  "parked & released
    APPEND r_bstat.
  ENDIF.

  IF p_own EQ 'X'.
    r_usnam-sign = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low = sy-uname.
    APPEND r_usnam.
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    APPEND r_ppnam.
  ENDIF.

  SELECT bukrs belnr gjahr
         budat usnam tcode xblnr
         bldat cpudt blart bktxt waers
         stblg stjah
    FROM rbkp
    INTO CORRESPONDING FIELDS OF TABLE it_bk
   WHERE bukrs IN s_bukrs
     AND gjahr IN s_gjahr
     AND belnr IN s_rbelnr
     AND blart IN s_blart
     AND budat IN s_budat
     AND xblnr IN s_xblnr
     AND cpudt IN s_cpudt
     AND rbstat IN r_bstat
     AND ( usnam IN r_usnam ).

  LOOP AT it_bk.
    MOVE-CORRESPONDING it_bk TO it_bkpf.
    APPEND it_bkpf.
  ENDLOOP.

ENDFORM.                    " read_rbkp

*&---------------------------------------------------------------------*
*&      Form  print_iv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSFIIVPRNHD  text
*      -->P_IT_ZSFIIVPRNDT  text
*      -->P_IT_ZSFIIVPRNMEMO  text
*      -->P_IT_ZSFIIVPRNETC  text
*----------------------------------------------------------------------*
FORM print_iv TABLES   p_it_zsfiivprnhd STRUCTURE zsfiivprnhd
                       p_it_zsfiivprndt STRUCTURE zsfiivprndt
                       p_it_zsfiivprnmemo STRUCTURE zsfiivprnmemo
                       p_it_zsfiivprnetc STRUCTURE zsfiivprnetc
              USING    p_bar_code.

  x_lin = 15.

  DATA subrc.
  DATA tab_lines LIKE sy-tabix.

  PERFORM get_tmp_file_name CHANGING subrc.
  CHECK subrc NE '4'.

*  DO 30 TIMES.
*    READ TABLE p_it_zsfiivprndt INDEX 1.
*    APPEND  p_it_zsfiivprndt.
*  ENDDO.

  DESCRIBE TABLE p_it_zsfiivprndt LINES tab_lines.
  CHECK tab_lines > 0.

  PERFORM calc_sheet USING tab_lines CHANGING out_times subrc.
  pri_fr_line = 1.

  DATA : $curr_page(3),
         $out_times(3).

  DO out_times TIMES.

    $curr_page = sy-index.
    $out_times = out_times.

    CONCATENATE $curr_page '/' $out_times
      INTO p_it_zsfiivprnhd-total_page.
    MODIFY p_it_zsfiivprnhd INDEX 1 TRANSPORTING total_page.

    pri_to_line = sy-index * x_lin.

    PERFORM file_delete.

    PERFORM write_excel TABLES
                         p_it_zsfiivprnhd
                         p_it_zsfiivprndt
                         p_it_zsfiivprnmemo
                         p_it_zsfiivprnetc
                        USING pri_fr_line pri_to_line p_bar_code.

    pri_fr_line =  pri_to_line + 1.

    IF scr EQ true AND nex EQ true AND sy-index < out_times.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
                defaultoption = 'Y'
                textline1     = 'Please confirm,'
                textline2     = 'Continue to next page for printing?'
                titel         = 'Next page'
                start_column  = 25
                start_row     = 6
           IMPORTING
                answer        = answer.

      IF answer NE 'J'. EXIT. ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.                    " print_iv
*---------------------------------------------------------------------*
*       FORM WRITE_EXCEL                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FROM_LINE                                                     *
*  -->  TO_LINE                                                       *
*---------------------------------------------------------------------*
FORM write_excel TABLES
                       p_it_zsfiivprnhd STRUCTURE zsfiivprnhd
                       p_it_zsfiivprndt STRUCTURE zsfiivprndt
                       p_it_zsfiivprnmemo STRUCTURE zsfiivprnmemo
                       p_it_zsfiivprnetc STRUCTURE zsfiivprnetc
                 USING from_line to_line
                       p_bar_code.

  PERFORM excel_start.

  IF subrc EQ 0.
    PERFORM excel_sheet USING 2 CHANGING subrc.

    PERFORM write_header TABLES p_it_zsfiivprnhd.

    PERFORM excel_sheet USING 3 CHANGING subrc.

    PERFORM write_item TABLES p_it_zsfiivprndt
                       USING from_line to_line.

    PERFORM excel_sheet USING 4 CHANGING subrc.

    PERFORM write_memo  TABLES p_it_zsfiivprnmemo
                       USING from_line to_line.

    PERFORM excel_sheet USING 5 CHANGING subrc.

    PERFORM write_etc  TABLES p_it_zsfiivprnetc
                       USING from_line to_line.

    PERFORM excel_sheet USING 6 CHANGING subrc.

    PERFORM write_barcode USING from_line to_line  p_bar_code.


    PERFORM excel_sheet USING 1 CHANGING subrc.

    CALL METHOD OF sheet 'SAVEAS'
                     EXPORTING #1 = tmp_file.
    IF scr EQ true.
      SET PROPERTY OF  excel 'VISIBLE' = 1.
    ELSE.
      PERFORM excel_print.
      PERFORM excel_quit.
    ENDIF.
  ENDIF.

  PERFORM free_excel.

ENDFORM.                               " WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  EXCEL_START
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_start.

  CREATE OBJECT  excel  'EXCEL.APPLICATION'.
  IF sy-subrc NE 0.MESSAGE e001 WITH sy-msgli.ENDIF.
  SET PROPERTY OF  excel 'VISIBLE' = 0.
  CALL METHOD  OF  excel 'WORKBOOKS' = books.
  CALL METHOD  OF  books 'OPEN' EXPORTING #1 = filename.

ENDFORM.                               " EXCEL_START
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*
FORM fill_cells USING i j val.

  CALL METHOD OF excel 'CELLS' = cells EXPORTING #1 = i #2 = j.
  SET PROPERTY OF cells 'VALUE' = val.
  IF cells-handle > 0.
    FREE OBJECT cells.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCEL_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_print.
  CALL METHOD OF sheet 'Printout'.
ENDFORM.                               " EXCEL_PRINT
*&---------------------------------------------------------------------*
*&      Form  EXCEL_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_sheet USING p_sheet CHANGING $subrc.
  IF sheet-handle > 0.
    FREE OBJECT sheet.
  ENDIF.
  CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = p_sheet.
  IF sy-subrc NE 0. $subrc = sy-subrc. EXIT. ENDIF.
  CALL METHOD OF sheet 'Activate'.
ENDFORM.                               " EXCEL_SHEET
*&---------------------------------------------------------------------*
*&      Form  EXCEL_QUIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_quit.
  CALL METHOD OF excel 'QUIT'.
ENDFORM.                               " EXCEL_QUIT
*&---------------------------------------------------------------------*
*&      Form  FREE_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_excel.
  FREE OBJECT cells.
  FREE OBJECT books.
  FREE OBJECT sheet.
  FREE OBJECT excel.
ENDFORM.                               " FREE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  FILE_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_delete.
  DATA subrc LIKE sy-subrc.


  DO 100 TIMES.
    CALL FUNCTION 'WS_QUERY'
         EXPORTING
              filename       = tmp_file
              query          = 'FE'
         IMPORTING
              return         = subrc
         EXCEPTIONS
              inv_query      = 1
              no_batch       = 2
              frontend_error = 3
              OTHERS         = 4.
    IF subrc NE 1. EXIT. ENDIF.
    CALL FUNCTION 'WS_FILE_DELETE'
         EXPORTING
              file = tmp_file.
  ENDDO.
ENDFORM.                               " FILE_DELETE
*&---------------------------------------------------------------------*
*&      Form  write_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_header TABLES  p_it_zsfiivprnhd STRUCTURE zsfiivprnhd.

  DATA: l_pos TYPE i.

  DEFINE __fill_header.
    l_pos = l_pos + 1.
    perform fill_cells using 1 l_pos &1.
  END-OF-DEFINITION.

  READ TABLE p_it_zsfiivprnhd INDEX 1.

  __fill_header :
                  p_it_zsfiivprnhd-zpage,
                  p_it_zsfiivprnhd-bukrs,
                  p_it_zsfiivprnhd-zcompname,
                  p_it_zsfiivprnhd-zprndate,
                  p_it_zsfiivprnhd-zprntime,
                  p_it_zsfiivprnhd-doc_no,
                  p_it_zsfiivprnhd-doc_type,
                  p_it_zsfiivprnhd-pst_date,
                  p_it_zsfiivprnhd-doc_date,
                  p_it_zsfiivprnhd-inv_ref,
                  p_it_zsfiivprnhd-doc_curr,
                  p_it_zsfiivprnhd-zpostedby,
                  p_it_zsfiivprnhd-zparkedby,
                  p_it_zsfiivprnhd-total_page.

ENDFORM.                    " write_header
*&---------------------------------------------------------------------*
*&      Form  write_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*----------------------------------------------------------------------*
FORM write_item TABLES p_it_zsfiivprndt STRUCTURE zsfiivprndt
              USING    p_from_line
                       p_to_line.

  DATA: l_pos TYPE i.
  DATA: $ix TYPE i.
  DEFINE __fill_item.
    l_pos = l_pos + 1.
    perform fill_cells using &1 l_pos &2.
  END-OF-DEFINITION.

  $ix = 1.

  LOOP AT p_it_zsfiivprndt FROM p_from_line.
    IF sy-tabix > p_to_line.
      EXIT.
    ENDIF.
    ADD 1 TO $ix.
    __fill_item $ix :
                p_it_zsfiivprndt-zpage,
                p_it_zsfiivprndt-zivlineno,
                p_it_zsfiivprndt-gl_acct,
                p_it_zsfiivprndt-gl_acct_name,
                p_it_zsfiivprndt-acct_det_info,
                p_it_zsfiivprndt-taxcode,
                p_it_zsfiivprndt-rate,
                p_it_zsfiivprndt-costcenter,
                p_it_zsfiivprndt-orderno,
                p_it_zsfiivprndt-assignment1,
                p_it_zsfiivprndt-assignment2,
                p_it_zsfiivprndt-text,
                p_it_zsfiivprndt-sgtxt,
                p_it_zsfiivprndt-zivdate,
                p_it_zsfiivprndt-debit,
                p_it_zsfiivprndt-credit,
                p_it_zsfiivprndt-debit_t,
                p_it_zsfiivprndt-credit_t,
                p_it_zsfiivprndt-f_total,
                p_it_zsfiivprndt-f_debit,
                p_it_zsfiivprndt-f_credit.
    l_pos = 0.
  ENDLOOP.

ENDFORM.                    " write_item
*&---------------------------------------------------------------------*
*&      Form  write_etc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_ZSFIIVPRNMEMO  text
*      -->P_P_IT_ZSFIIVPRNETC  text
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*----------------------------------------------------------------------*
FORM write_memo TABLES   p_it_zsfiivprnmemo STRUCTURE zsfiivprnmemo
               USING    p_from_line
                        p_to_line.

  DATA: l_pos TYPE i.
  DATA: $ix TYPE i.
  DEFINE __fill_item.
    l_pos = l_pos + 1.
    perform fill_cells using &1 l_pos &2.
  END-OF-DEFINITION.

  LOOP AT p_it_zsfiivprnmemo.
    $ix = sy-tabix.
    __fill_item $ix :
                  p_it_zsfiivprnmemo-zpage,
                  p_it_zsfiivprnmemo-zmemo,
                  p_it_zsfiivprnmemo-zprcwarnings,
                  p_it_zsfiivprnmemo-debit_sum,
                  p_it_zsfiivprnmemo-credit_sum,
                  p_it_zsfiivprnmemo-tax_debit_sum,
                  p_it_zsfiivprnmemo-tax_credit_sum.
    l_pos = 0.
  ENDLOOP.

ENDFORM.                    " write_etc
*&---------------------------------------------------------------------*
*&      Form  write_etc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_ZSFIIVPRNETC  text
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*----------------------------------------------------------------------*
FORM write_etc TABLES   p_it_zsfiivprnetc STRUCTURE zsfiivprnetc
               USING    p_from_line
                        p_to_line.


  DATA: l_pos TYPE i.
  DATA: $ix TYPE i.
  DEFINE __fill_item.
    l_pos = l_pos + 1.
    perform fill_cells using &1 l_pos &2.
  END-OF-DEFINITION.

  LOOP AT p_it_zsfiivprnetc.
    $ix = sy-tabix.
    __fill_item $ix :
      p_it_zsfiivprnetc-zpage,
      p_it_zsfiivprnetc-text77.
    l_pos = 0.
  ENDLOOP.

ENDFORM.                    " write_etc
*&---------------------------------------------------------------------*
*&      Form  write_barcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*      -->P_BAR_CODE  text
*----------------------------------------------------------------------*
FORM write_barcode USING    p_from_line
                            p_to_line
                            p_bar_code.

  PERFORM fill_cells USING 1 1 p_bar_code.

ENDFORM.                    " write_barcode

*---------------------------------------------------------------------*
*       FORM FIND_DIR                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_$DIRNAME                                                    *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM find_dir USING    p_$dirname
              CHANGING p_subrc.

  CALL FUNCTION 'WS_QUERY'
       EXPORTING
            filename       = p_$dirname
            query          = 'DE'
       IMPORTING
            return         = p_subrc
       EXCEPTIONS
            inv_query      = 1
            no_batch       = 2
            frontend_error = 3
            OTHERS         = 4.

ENDFORM.                               " FIND_DIR

*---------------------------------------------------------------------*
*       FORM MK_DIR                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  NEW_DIR                                                       *
*  -->  VALID_DIR                                                     *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
FORM mk_dir TABLES   new_dir STRUCTURE new_dir
            USING    valid_dir
          CHANGING   subrc.

  DATA $len TYPE i.
  DATA dos_command(79) OCCURS 0 WITH HEADER LINE.
  DATA winsys(3).
  SORT new_dir BY $index DESCENDING.

  PERFORM find_dir USING valid_dir CHANGING subrc.
  IF subrc NE 1.EXIT.ENDIF.

  WRITE '@ECHO OFF'            TO dos_command.__dos.
  WRITE  valid_dir             TO dos_command(1).
  WRITE: ':' TO dos_command+1.__dos.
  WRITE: 'CD' TO dos_command,'\' TO dos_command+3.__dos.
  $len = strlen( valid_dir ).
  IF $len > 2.
    WRITE: 'CD' TO dos_command,valid_dir TO dos_command+3.__dos.
  ENDIF.
  LOOP AT new_dir.
    WRITE: 'MD' TO dos_command,new_dir-dirname TO dos_command+3.__dos.
    WRITE: 'CD' TO dos_command,new_dir-dirname TO dos_command+3.__dos.
  ENDLOOP.

  CALL FUNCTION 'WS_QUERY'
       EXPORTING
            query  = 'WS'
       IMPORTING
            return = winsys.

  CHECK ( winsys(2) EQ 'WN') OR ( winsys(2) EQ 'PM' ).

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename            = 'C:\_$$IV$$.BAT'
            filetype            = 'ASC'
       TABLES
            data_tab            = dos_command
       EXCEPTIONS
            file_open_error     = 1
            file_write_error    = 2
            invalid_filesize    = 3
            invalid_table_width = 4
            invalid_type        = 5
            no_batch            = 6
            unknown_error       = 7
            OTHERS              = 8.


  CALL FUNCTION 'WS_EXECUTE'
       EXPORTING
            commandline = ''
            inform      = 'X'
            program     = 'C:\_$$IV$$.BAT'.

  CALL FUNCTION 'WS_FILE_DELETE'
       EXPORTING
            file   = 'C:\_$$IV$$.BAT'
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.                               " MK_DIR
*&---------------------------------------------------------------------*
*&      Form  get_tmp_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tmp_file_name CHANGING subrc.

  CLEAR : tmp_file, tmp_dir.

  tmp_dir = 'C:\TEMP\'.
  PERFORM find_dir USING tmp_dir CHANGING subrc.
  IF subrc NE '1'.
    PERFORM make_directory CHANGING tmp_dir subrc.
    PERFORM find_dir USING tmp_dir CHANGING subrc.
    IF subrc NE '1'.
      MESSAGE e011 WITH
       'Could not make the tmp directory for I/V printing'.
      subrc = '4'.
      EXIT.
    ENDIF.
  ELSE.
    tmp_file = 'C:\TEMP\$XLSTMP$$.XLS'.
  ENDIF.

ENDFORM.

" get_tmp_file_name
*&---------------------------------------------------------------------*
*&      Form  MAKE_DIRECTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2226   text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
FORM make_directory CHANGING p_p_directory p_subrc.

  DATA  valid_dir LIKE rlgrap-filename.
  DATA  save_dir(50).
  save_dir = valid_dir = p_p_directory.
  CLEAR subrc.

  PERFORM get_valid_dir TABLES new_dir CHANGING valid_dir subrc.
  IF subrc EQ 0.
    PERFORM mk_dir TABLES new_dir USING valid_dir CHANGING subrc.
    PERFORM get_dirname USING p_p_directory CHANGING p_p_directory.
    PERFORM find_dir USING p_p_directory CHANGING subrc.
  ENDIF.
  p_p_directory = save_dir.

ENDFORM.                    " MAKE_DIRECTORY

*&---------------------------------------------------------------------*
*&      Form  GET_VALID_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$DIRNAME  text                                             *
*      <--P_$DIRNAME  text                                             *
*----------------------------------------------------------------------*
FORM get_valid_dir TABLES new_dir STRUCTURE new_dir
                 CHANGING $dirname subrc.
  __cls new_dir.
  PERFORM get_dirname USING $dirname CHANGING $dirname.
  PERFORM find_dir USING $dirname CHANGING subrc.
  CHECK subrc NE 1.
  WHILE subrc NE 1.
    IF $dirname EQ space. subrc = 0. EXIT. ENDIF.
    IF subrc NE 1.
      new_dir-dirname = $dirname.
      new_dir-$index  = sy-index.
      APPEND new_dir.
    ENDIF.
    PERFORM get_dirname USING $dirname CHANGING $dirname.
    PERFORM find_dir USING $dirname CHANGING subrc.
  ENDWHILE.

  READ TABLE new_dir INDEX 1.
  subrc = sy-subrc.

ENDFORM.                               " GET_VALID_DIR

*&---------------------------------------------------------------------*
*&      Form  GET_DIRNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_P_DIRECTORY  text                                        *
*      <--P_$DIRNAME  text                                             *
*----------------------------------------------------------------------*
FORM get_dirname USING    p_p_directory
                 CHANGING $dirname.
  DATA $strlen TYPE i.
  DATA $offset TYPE i.
  DATA $point  TYPE i.
  DATA $str.
  PERFORM filter_dir CHANGING p_p_directory.
  $strlen = strlen( p_p_directory ).

  DO $strlen TIMES.
    $offset = $strlen - sy-index.
    $str    = p_p_directory+$offset(1).
    IF $str EQ '\'.
      $point = $offset.
      EXIT.
    ENDIF.
  ENDDO.
  IF $point > 0.
    $dirname = p_p_directory+0($point).
  ELSE.
    CLEAR $dirname.
  ENDIF.
ENDFORM.                               " GET_DIRNAME

*&---------------------------------------------------------------------*
*&      Form  FILTER_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_P_DIRECTORY  text                                        *
*----------------------------------------------------------------------*
FORM filter_dir CHANGING p_directory.

  DO 256 TIMES.
    REPLACE '\\' WITH '\' INTO p_directory.
  ENDDO.

ENDFORM.                               " FILTER_DIR
*&---------------------------------------------------------------------*
*&      Form  CALC_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB_LINES  text
*      <--P_OUT_TIMES  text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
FORM calc_sheet USING
                         p_tab_lines
                CHANGING p_out_times
                         p_subrc.
  DATA $p_time TYPE p DECIMALS 2.
  DATA $p_mod  TYPE p DECIMALS 2.
  $p_time = p_tab_lines / x_lin.

  IF $p_time > 0.
    $p_mod  = $p_time * 100.
    $p_mod  = $p_mod MOD 100.
    $p_mod  = $p_mod / 100.
    $p_time = $p_time - $p_mod.
  ENDIF.
  p_out_times = $p_time.
  $p_mod  = p_tab_lines MOD x_lin.
  IF  $p_mod > 0.
    ADD 1 TO p_out_times.
  ENDIF.
ENDFORM.                               " CALC_SHEET
