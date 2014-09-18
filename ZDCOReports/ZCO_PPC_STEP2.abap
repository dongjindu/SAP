*&---------------------------------------------------------------------*
*& Report  Z_PPC_FILL_STEP2                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*& OSS 438183 2006
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  z_ppc_fill_step2  LINE-SIZE 255.
TABLES: ppc_step2, ppc_ord_inf, ppc_res_hdr.
*----------------------------------------------------------------------*
* This report is a individual correction report which upload components
* into hte PPC_STEP2 table. These components might by posted afterwards
* using the Tr. PPCGO2
* the report requires the input file in the presentation server in
* folder C:\ named as TP085448_modif_MSDOS.csv (hardcoded)
* This file should be a comma separated MS-DOS file exported from Excel
* with field contents
* Component Plant Stloc Quant Uom Posting date Finished Material
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_inp,
        matnr     TYPE matnr,
        werks     TYPE werks_d,
        lgort     TYPE lgort_d,
        confquant TYPE ppc_confquant,
        confunit  TYPE ppc_confunit,
        post_date TYPE budat,
        matnr_h   TYPE matnr,
        version   TYPE verid,
END OF ty_inp.
DATA: ls_inp TYPE ty_inp,
      lt_inp TYPE STANDARD TABLE OF ty_inp.
DATA: l_length TYPE i.

TYPES: BEGIN OF ty_inp2,
    matnr TYPE matnr,
    werks TYPE werks_d,
    lgort TYPE lgort_d,
    confquant(10) TYPE c,
    confunit      TYPE ppc_confunit,
    post_date(10) TYPE c,
    matnr_h TYPE matnr,
    version TYPE verid,  "ADD
END OF ty_inp2.
DATA:  lt_inp2 TYPE STANDARD TABLE OF ty_inp2,
       ls_inp2 TYPE ty_inp2.

TYPES tab_type(250) TYPE c.
DATA: lt_tab TYPE TABLE OF tab_type,
      ls_tab TYPE tab_type.
DATA: lt_tab2 TYPE STANDARD TABLE OF tab_type,
      ls_tab2 TYPE tab_type.

DATA: BEGIN OF ls_mat_h,
        matnr_h TYPE matnr,
        version TYPE verid,  "ADD
      END OF ls_mat_h,
      lt_mat_h LIKE ls_mat_h OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ls_ord,
        materialnr TYPE matnr,
        version    TYPE verid,   "ADD
        accassobj  TYPE ppc_accassobj_int,
      END OF ls_ord,
lt_ord LIKE ls_ord OCCURS 0,
l_tabix TYPE i.

PARAMETERS p_test(1) TYPE c DEFAULT 'X'.

START-OF-SELECTION.
* store excel file as comma separated MS-DOS file
* and save in the c:\ folder as .CSV file
* Do manually correction with the notepad
* -correct the wrong formated postdate ( replace '/' -> '.' )
* -correct the wrong formated Finished product material like
*   "#6B28AACRWG76D 01

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      codepage                = 'IBM'
*     FILENAME                = 'C:\TP085448_modif_MSDOS.csv'
      filename                = 'C:\temp\ppc.csv'
      filetype                = 'ASC'
    IMPORTING
      filelength              = l_length
    TABLES
      data_tab                = lt_tab
    EXCEPTIONS
      conversion_error        = 1
      file_open_error         = 2
      file_read_error         = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      OTHERS                  = 11.

  break c5024598.

  FIELD-SYMBOLS: <comp> TYPE ANY.
  LOOP AT lt_tab INTO ls_tab.
    CLEAR lt_tab2.
*    split LS_TAB at ';' into table LT_TAB2.
    SPLIT ls_tab AT ',' INTO TABLE lt_tab2.
    LOOP AT lt_tab2 INTO ls_tab2.
      CASE sy-tabix.
        WHEN 1.
          ls_inp2-matnr = ls_tab2.
        WHEN 2.
          ls_inp2-werks = ls_tab2.
        WHEN 3.
          ls_inp2-lgort = ls_tab2.
        WHEN 4.
          ls_inp2-confquant = ls_tab2.
        WHEN 5.
          ls_inp2-confunit = ls_tab2.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input                = ls_inp2-confunit
           IMPORTING
             OUTPUT               = ls_inp2-confunit.
        WHEN 6.
          ls_inp2-post_date = ls_tab2.
        WHEN 7.
          ls_inp2-matnr_h = ls_tab2.
        WHEN 8.  "ADD
          ls_inp2-version = ls_tab2. "ADD
      ENDCASE.
    ENDLOOP.
    APPEND ls_inp2 TO lt_inp2.
  ENDLOOP.

  LOOP AT lt_inp2 INTO ls_inp2.
    TRANSLATE ls_inp2-confquant USING ',.'.
    MOVE-CORRESPONDING ls_inp2 TO ls_inp.
*   MM/DD/YYYY    -> yyyymmdd
    ls_inp-post_date(4) = ls_inp2-post_date+6(4). "YYYY
    ls_inp-post_date+4(2) = ls_inp2-post_date(2). "MM
    ls_inp-post_date+6(2) = ls_inp2-post_date+3(2). "DD
    APPEND ls_inp TO lt_inp.
    lt_mat_h-matnr_h = ls_inp-matnr_h.  "CHANGED
    lt_mat_h-version = ls_inp-version.  "ADD
    COLLECT lt_mat_h.
  ENDLOOP.

  break c5024598.

* ok, now the file is formated and stored in the table lt_inp

  DATA:   lf_postid TYPE ppc_matpostid,
          ls_ppcstep2  TYPE ppc_step2,
          lt_ppcstep2  TYPE TABLE OF ppc_step2,
          lf_tstmp TYPE timestamp.

* fill the PPC_STEP2 table:

* take a big postid number which is not yet used in that number range
  lf_postid = 9990000000.
  GET TIME STAMP FIELD lf_tstmp.

  SELECT DISTINCT materialnr version accassobj     "CHANGED
         INTO CORRESPONDING FIELDS OF TABLE lt_ord
  FROM ppc_ord_inf
  FOR ALL ENTRIES IN lt_mat_h
  WHERE materialnr = lt_mat_h-matnr_h
    AND version    = lt_mat_h-version.  "ADD

  SORT lt_ord BY materialnr version.    "CHANGED

* fields from table PPC_STEP2
*MANDT
* MATPOSTID
*ACCASSOBJ
* POST_DATE
* MATNR
* WERKS
* LGORT
*PRVBE
*CHARG
* CONFQUANT
*DELTA_CONFQUANT
* CONFUNIT
*SOBKZ
*KDAUF
*KDPOS
*PSPNR
*KZVBR
*KZBWS
* CRNAME
* CRTIME
* POSTNAME
* POSTTIME
*---------
* available fields from the input
*        MATNR     type MATNR,
*        WERKS     type WERKS_D,
*        LGORT     type LGORT_D,
*        CONFQUANT type PPC_CONFQUANT,
*        CONFUNIT  type PPC_CONFUNIT,
*        POST_DATE type BUDAT,
*        MATNR_H   type MATNR,
  LOOP AT lt_inp INTO ls_inp.
    l_tabix = sy-tabix.
    WRITE: /1 l_tabix.
    CLEAR ls_ppcstep2.
*   MATNR; WERKS, LGORT, CONFQUANT, CONFUNIT,
    MOVE-CORRESPONDING ls_inp TO ls_ppcstep2.
    ADD 1 TO lf_postid.
    ls_ppcstep2-mandt = sy-mandt.
    ls_ppcstep2-matpostid = lf_postid.
    ls_ppcstep2-postname = sy-uname.
    ls_ppcstep2-crname = sy-uname.
    MOVE lf_tstmp TO ls_ppcstep2-crtime.
    MOVE lf_tstmp TO ls_ppcstep2-posttime.

* PRVBE let's keep blank, I think we do not need this
* charg should be blank, since charge relevant components had been
* posted by the normal way.

* search accessobj
    READ TABLE lt_ord INTO ls_ord WITH KEY materialnr = ls_inp-matnr_h
                                           version    = ls_inp-version
                                  BINARY SEARCH.
    IF NOT sy-subrc IS INITIAL.
*      message E001(00) with 'Incorrect assebmly'.
      WRITE: 15 'BAD'.
    ENDIF.
    ls_ppcstep2-accassobj = ls_ord-accassobj.
* NEED TO BE CHECKED FIELD SOBKZ KDAUF KDPOS PSPNR... ARE they used?
* I think we can keep them as blank
* do we have sales order valuation? salesorder spec.stock?
    APPEND ls_ppcstep2 TO lt_ppcstep2.
    WRITE: 20 ls_ppcstep2-matnr,
              ls_ppcstep2-lgort,
              ls_ppcstep2-confquant,
              ls_ppcstep2-confunit,
              ls_ppcstep2-post_date,
              ls_ord-materialnr,
              ls_ord-version.
  ENDLOOP.
  break c5024598.
  IF p_test IS INITIAL.
    WRITE: /1 'PPC_STEP2 update has been commited'.
    INSERT ppc_step2 FROM TABLE lt_ppcstep2.
    COMMIT WORK.
  ENDIF.
