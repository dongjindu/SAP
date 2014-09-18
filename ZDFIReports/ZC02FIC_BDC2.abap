*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  04/03/2012  Valerian  UD1K954413  [FI] - Fix ABAP Dump and BDC Error
*
*prepared by Andy Choi
*fi bdc:RFBIBL01
*layout program sample:RFFMFV00
*menu sample: RFTS6500. copy L_ALV to MENU and modify
*
* cost center = fund center
*
REPORT ytk3 .

SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE text-s10.
PARAMETERS : locfile LIKE rlgrap-filename DEFAULT
  'c:\temp\gl.txt'.
PARAMETERS : v_bukrs LIKE bbkpf-bukrs  MEMORY ID buk.
PARAMETERS : p_curr  LIKE bkpf-waers   MEMORY ID fws.
SELECTION-SCREEN END OF BLOCK sb.

*ARAMETERS : single as checkbox default ' '.


*only check batch file
PARAMETERS : p_dmode LIKE ctu_params-dismode DEFAULT 'E'.
*A: show all dynpros
*E: show dynpro on error only
*N: do not display dynpro

PARAMETERS : p_umode LIKE ctu_params-updmode DEFAULT 'S'.
*S: synchronously
*A: asynchronously
*L: local
PARAMETERS : p_park AS CHECKBOX DEFAULT ' '. "park/posting
PARAMETERS : fl_check    LIKE rfpdo-rfbichck.

TABLES: t001.

************************************************************************
CONSTANTS: v VALUE '|'.

DATA : processed TYPE c VALUE ' ',
       l_break   TYPE i VALUE 0,
       l_current TYPE i VALUE 0.
*DATA : BEGIN OF REC OCCURS 0,
*          budat(10) type c,
*          bldat(10) type c,
*          newbs like RF05A-NEWBS,
*          newko like RF05A-NEWKO,
*          wrbtr(14)  type c,
*          zuonr like BSEG-ZUONR,
*          zfbdt(10) type c,
*       END OF REC.

DATA : wt_sw LIKE bwith-wt_withcd.     "withholding

*BDC structure (all)
DATA : BEGIN OF htab OCCURS 0,
*        MARK,
*       LFDNR     like WPTST-LFDNR,  "seq.doc
        end_flag  LIKE bbseg-xskrl,

        bldat     LIKE bbkpf-bldat,
        blart     LIKE bbkpf-blart,
        budat     LIKE bbkpf-budat,
        waers     LIKE bbkpf-waers,
        kursf     LIKE bbkpf-kursf,
        belnr     LIKE bbkpf-belnr,
        xblnr     LIKE bbkpf-xblnr,
        bktxt     LIKE bbkpf-bktxt,
        newbs     LIKE bbseg-newbs,  "posting key
        anbwa     LIKE bbseg-anbwa,  "transction type (aa)
        newko     LIKE bbseg-newko,  "account
        newum     LIKE bbseg-newum,  "sp g/l
        wrbtr     LIKE bbseg-wrbtr,  "amt
        dmbtr     LIKE bbseg-dmbtr,
        wskto     LIKE bbseg-wskto,  "d/c amt
        mwskz     LIKE bbseg-mwskz,  "tx code
        bupla     LIKE bbseg-bupla,  "biz place
        hwste     LIKE bbtax-hwste,  "tax amt
        pernr     LIKE bbseg-pernr,
        kostl     LIKE bbseg-kostl,  "cost center
        aufnr     LIKE bbseg-aufnr,  "order#
        ebeln     LIKE bbseg-ebeln,
        ebelp     LIKE bbseg-ebelp,
        zfbdt     LIKE bbseg-zfbdt,  "baseline date
        valut     LIKE bbseg-valut,  "value date
        zterm     LIKE bbseg-zterm,
        zlsch     LIKE bbseg-zlsch,  "pay method
        zlspr     LIKE bbseg-zlspr,  "pay block
        zuonr     LIKE bbseg-zuonr,  "assign
        sgtxt     LIKE bbseg-sgtxt,  "text
        xnegp     LIKE bbseg-xnegp,
        wt_withcd LIKE bwith-wt_withcd,
        name1     LIKE bbseg-name1,
        ort01     LIKE bbseg-ort01,
        bankl     LIKE bbseg-bankl,
        bankn     LIKE bbseg-bankn,
        stcd1     LIKE bbseg-stcd1,
        fistl     LIKE bbseg-fistl,
       END OF htab.

DATA : BEGIN OF utab OCCURS 0.
        INCLUDE STRUCTURE zfibdc.
*        lfdnr     LIKE wptst-lfdnr,  "seq.doc
*        end_flag  LIKE bbseg-xskrl,
*        bldat     LIKE bbkpf-bldat,
*        blart     LIKE bbkpf-blart,
*        budat     LIKE bbkpf-budat,
**       WAERS     LIKE BBKPF-WAERS,
**       KURSF     LIKE BBKPF-KURSF,
**       BELNR     LIKE BBKPF-BELNR,
*        xblnr     LIKE bbkpf-xblnr,
*        bktxt     LIKE bbkpf-bktxt,
*        newbs     LIKE bbseg-newbs,  "posting key
*        anbwa     LIKE bbseg-anbwa,  "transction type (aa)
*        newko     LIKE bbseg-newko,  "account
*        newum     LIKE bbseg-newum,  "sp g/l
*        wrbtr     LIKE bbseg-wrbtr,  "amt
**       DMBTR     LIKE BBSEG-DMBTR,
**       WSKTO     LIKE BBSEG-WSKTO,  "d/c amt
*        mwskz     LIKE bbseg-mwskz,  "tx code
**       BUPLA     LIKE BBSEG-BUPLA,  "biz place
*        hwste     LIKE bbtax-hwste,  "tax amt
**       HWSTE     LIKE BBTAX-HWSTE,
**       PERNR     LIKE BBSEG-PERNR,
*        kostl     LIKE bbseg-kostl,  "cost center
*        aufnr     LIKE bbseg-aufnr,  "order#
*        ebeln     LIKE bbseg-ebeln,
*        ebelp     LIKE bbseg-ebelp,
*        zfbdt     LIKE bbseg-zfbdt,  "baseline date
*        valut     LIKE bbseg-valut,  "value date
*        zterm     LIKE bbseg-zterm,
*        zlsch     LIKE bbseg-zlsch,  "pay method
*        zlspr     LIKE bbseg-zlspr,  "pay block
*        zuonr     LIKE bbseg-zuonr,  "assign
*        sgtxt     LIKE bbseg-sgtxt,  "text
**       XNEGP     LIKE BBSEG-XNEGP,
**       WT_WIThcd LIKE BWITH-WT_WITHCD,
**       NAME1     LIKE BBSEG-NAME1,
**       ORT01     LIKE BBSEG-ORT01,
**       BANKL     LIKE BBSEG-BANKL,
**       BANKN     LIKE BBSEG-BANKN,
**       STCD1     LIKE BBSEG-STCD1,
**       FISTL     LIKE BBSEG-FISTL,
DATA:    END OF utab.

DATA : BEGIN OF itab OCCURS 0.
        INCLUDE STRUCTURE htab.
DATA : END OF itab.

DATA : BEGIN OF itax OCCURS 0,
        hwste  LIKE bset-fwste,
        mwskz  LIKE bbtax-mwskz,
        bschl  LIKE bbtax-bschl,
       END OF itax.

DATA : imsi_blart LIKE bkpf-blart,
       imsi_budat LIKE bkpf-budat,
       imsi_bldat LIKE bkpf-bldat.


************************************************************************
* Data declaration
TYPE-POOLS: slis.

* Global structure of list
TYPES:        BEGIN OF ud_struct,
                        scarr LIKE scarr,
                        spfli LIKE spfli,
                END OF ud_struct.

TABLES: scarr, spfli.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gt_outtab  TYPE ud_struct OCCURS 0 WITH HEADER LINE.
DATA: g_repid LIKE sy-repid.
DATA: g_exit_caused_by_caller,
      gt_events      TYPE slis_t_event,
      gt_list_top_of_page TYPE slis_t_listheader,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
      gs_exit_caused_by_user TYPE slis_exit_by_user,
      gs_layout TYPE slis_layout_alv,
      g_tabname TYPE slis_tabname VALUE 'ITAB',
      g_boxname TYPE slis_fieldname VALUE 'BOX'.


************************************************************************
* MACRO
************************************************************************
DEFINE append_fld.
  &1 = &1 + 1.
  clear ls_fieldcat.
  ls_fieldcat-key           = ' '.
  ls_fieldcat-col_pos       =  &1.
  ls_fieldcat-fieldname     =  &2.
  ls_fieldcat-ref_fieldname =  &3.
  ls_fieldcat-ref_tabname   =  &4.
  append ls_fieldcat to  rt_fieldcat.
END-OF-DEFINITION.

DEFINE append_key.
  &1 = &1 + 1.
  clear ls_fieldcat.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-col_pos       =  &1.
  ls_fieldcat-fieldname     =  &2.
  ls_fieldcat-ref_fieldname =  &3.
  ls_fieldcat-ref_tabname   =  &4.
  append ls_fieldcat to  rt_fieldcat.
END-OF-DEFINITION.


************************************************************************
INCLUDE zc02fic_bdci.
*include ytk3i1.

*----------------------------------
* Initialization fieldcatalog
INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR locfile.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.*,*.*.'
            mode             = 'O'
            title            = 'Select File'
       IMPORTING
            filename         = locfile
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.



  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*&--------------------------------------------------------------------
*&      start-of-selection.
*&--------------------------------------------------------------------
START-OF-SELECTION.
  SELECT SINGLE * FROM t001 WHERE bukrs = v_bukrs.
  IF p_curr IS INITIAL.
    p_curr = t001-waers.
  ENDIF.
  PERFORM upload_pc_file.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************

END-OF-SELECTION.
*  PERFORM DISPLAY_DATA.


  g_repid = sy-repid.
  gs_layout-box_fieldname = g_boxname.

  PERFORM fieldcat_init USING gt_fieldcat[].
  PERFORM eventtab_build USING gt_events[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = g_repid
            i_structure_name        = 'ZFIBDC'
*            is_layout               = gs_layout
*            it_fieldcat             = gt_fieldcat[]
            it_events               = gt_events[]
       IMPORTING
            e_exit_caused_by_caller = g_exit_caused_by_caller
            es_exit_caused_by_user  = gs_exit_caused_by_user
       TABLES
            t_outtab                = utab.


*MESSAGE i000(zmsg0) WITH ans.
  DO.
    PERFORM rewrite.
  ENDDO.

  IF sy-subrc = 0.
    IF g_exit_caused_by_caller = 'X'.


*      MESSAGE i000(zmsg0) WITH g_exit_caused_by_caller.
*      leave.
    ELSE.
      IF sy-tcode = 'XXXX'.
*     MESSAGE i000(zmsg0) WITH g_exit_caused_by_caller.
      ENDIF.
    ENDIF.
  ENDIF.

AT USER-COMMAND.
  MESSAGE i000(zmsg0) WITH sy-ucomm.
  PERFORM rewrite.

AT PF3.
  PERFORM rewrite.

*DATA: LIN LIKE SY-LILLI.
* READ LINE LIN INDEX 2.
*  MESSAGE i000(zmsg0) WITH LIN.

*
*&--------------------------------------------------------------------
*& Include
*&--------------------------------------------------------------------
*nclude ytk3i2.
INCLUDE ZRFBIBL022.                                         "UD1K954413
* INCLUDE rfbibl02.                                         "UD1K954413

*&--------------------------------------------------------------------
*&      at user-command.
*&--------------------------------------------------------------------
FORM user_command
     USING rf_ucomm    LIKE sy-ucomm
           rs_selfield TYPE slis_selfield.

*MESSAGE i000(zmsg0) WITH rf_ucomm.
  CASE rf_ucomm.
    WHEN 'AUSW'.  "double click

    WHEN 'UMS'.   "transfer
      PERFORM bdc_process.

  ENDCASE.
  CLEAR rf_ucomm.

ENDFORM.
*

*&--------------------------------------------------------------------
FORM pf_status_set
     USING rt_extab TYPE slis_t_extab.
* EXEC ; run
  SET PF-STATUS 'MENU'.
ENDFORM.

*&--------------------------------------------------------------------
*&      Form  UPLOAD_PC_FILE
*&--------------------------------------------------------------------
FORM upload_pc_file.
  DATA: cnt TYPE i.


  CALL FUNCTION 'WS_UPLOAD'
   EXPORTING
            filename                = locfile
            filetype                = 'DAT'
    TABLES
      data_tab                      = utab
*   EXCEPTIONS
*     CONVERSION_ERROR              = 1
*     FILE_OPEN_ERROR               = 2
*     FILE_READ_ERROR               = 3
*     INVALID_TYPE                  = 4
*     NO_BATCH                      = 5
*     UNKNOWN_ERROR                 = 6
*     INVALID_TABLE_WIDTH           = 7
*     GUI_REFUSE_FILETRANSFER       = 8
*     CUSTOMER_ERROR                = 9
*     OTHERS                        = 10
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  DESCRIBE TABLE utab LINES cnt.
  CHECK cnt > 0.

*  DELETE REC INDEX 1.  " delete header info
ENDFORM.                    " UPLOAD_PC_FILE

***include
*----------------------------------------------------------------------*
***INCLUDE MZIN01F01 .
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
  DATA: cnt TYPE i.
  DATA: l_blart  LIKE t003-blart.

  LOOP AT utab.
    MOVE-CORRESPONDING utab TO htab.
    APPEND htab.
  ENDLOOP.
  DESCRIBE TABLE htab LINES cnt.
  CHECK cnt > 0.

  anz_mode = p_dmode.
  update   = p_umode.

* PERFORM EXCEL_UPLOAD.
  PERFORM select_field_name.

  CLEAR : s_cnt, e_cnt, etab, etab[].

  CLEAR : itab, itab[], imsi_bldat, imsi_blart, imsi_budat.

  LOOP AT htab.

    IF NOT ( htab-blart IS INITIAL ).
      CLEAR : l_blart.
      SELECT SINGLE * FROM t003
        WHERE blart = htab-blart
          AND xsybl = ''.

      IF sy-subrc = 0. l_blart =  htab-blart. ENDIF.
    ENDIF.

    CHECK l_blart <> ''.
*<-- logic 추가  2000.10.09 bk
    MOVE-CORRESPONDING htab TO itab.
    PERFORM check_itab.
    IF itab-blart = ' '.
      itab-bldat = imsi_bldat.
      itab-blart = imsi_blart.
      itab-budat = imsi_budat.
    ENDIF.
    APPEND itab.
    IF imsi_blart = ' '.
      imsi_bldat = htab-bldat.
      imsi_blart = htab-blart.
      imsi_budat = htab-budat.
    ENDIF.
    CHECK  itab-end_flag = 'X' OR itab-end_flag = 'x'.

    CLEAR: imsi_bldat, imsi_blart, imsi_budat.
*.. DATA CLEAR
    PERFORM clear_data.

*.. BGR00, BBKPF
    PERFORM fill_bgr00_record.
    PERFORM fill_bbkpf_record.

    CLEAR : itax, itax[], wt_sw.
    LOOP AT itab.
*.. BBSEG
      PERFORM fill_bbseg_record.
*.. COMPUTE TAX AMOUNT
      PERFORM compute_tax_amount.
*.... BWITH(withholding)
      PERFORM fill_bwith_record.
    ENDLOOP.

*.... BBTAX
    PERFORM fill_bbtax_record.
*.... BWITH(withholding)
*   PERFORM FILL_BWITH_RECORD.

    PERFORM loop_at_table_tfile.

    COMMIT WORK.

    CLEAR : itab, itab[].

  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE i000(zmsg0) WITH 'not selected.'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_ALL'.

  LEAVE TO LIST-PROCESSING.
  NEW-PAGE LINE-SIZE 255
           LINE-COUNT 75.

* display some info.
  WRITE:/7   'Date : ', sy-datum,
         120 'Page : ', sy-pagno,
        /7   'User : ', sy-uname.
*   MESSAGE i000(zmsg0) WITH 'hello written.'.
  PERFORM write_result_list.
ENDFORM.                               " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_ITAB
*&---------------------------------------------------------------------*
*FORM upload_htab.
*
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            codepage = 'ASC'
*            filename = 'C:\My Documents\test1.txt'
*            filetype = 'DAT'
**    IMPORTING
**         FILESIZE            = FLENGTH
**         ACT_FILENAME        = ACT_FILENAME
*       TABLES
*            data_tab            = utab
*       EXCEPTIONS
*            file_open_error     = 1
*            file_write_error    = 2
*            invalid_file_size   = 3
*            invalid_table_width = 4
*            invalid_type        = 5.
*
*  IF utab[] IS INITIAL.
*    WRITE:/ 'No Data.'.
*    EXIT.
*  ELSE.
*    CLEAR: htab, htab[].
*    LOOP AT utab.
*      CHECK NOT utab IS INITIAL.
*      MOVE-CORRESPONDING utab TO htab.
*      APPEND htab.
*      CLEAR  htab.
*    ENDLOOP.
*  ENDIF.
*
**andy
**  DESCRIBE TABLE htab LINES tabc-lines.
*
*ENDFORM.                               " UPLOAD_ITAB

************************************************************************
*        Interne Perform-Routinen
************************************************************************
FORM auglv_pruefen.
  IF tfill_041a = 0.
    SELECT * FROM t041a.
      xt041a-auglv = t041a-auglv.
      APPEND xt041a.
    ENDSELECT.
  ENDIF.

  tabix = 0.

  LOOP AT xt041a WHERE auglv = bbkpf-auglv.
    tabix = sy-tabix.
    EXIT.
  ENDLOOP.

ENDFORM.

*-----------------------------------------------------------------------
*        FORM BBKPF_ERWEITERUNG_PRUEFEN.
*-----------------------------------------------------------------------
FORM bbkpf_erweiterung_pruefen.

  IF bbkpf-sende(1) NE nodata.
*------- BBKPF-Erweiterung zu 4.5B: KURSF_M(10)
    IF bbkpf-kursf_m(1) NE nodata.
*------- BBKPF-Erweiterung zu 4.0C: STGRD(2),
      IF bbkpf-stgrd(1) NE nodata.
*------- BBKPF-Erweiterung zu 4.0A: BRNCH(4), NUMPG(3)
        IF bbkpf-brnch(1) NE nodata.
*------- BBKPF-Erweiterung zu 3.0A: DOCID(10), BARCD(40),STODT
          IF bbkpf-docid(1) NE nodata.
*------- BBKPF-Erweiterung zu 2.2A: XMWST(1)
            IF bbkpf-xmwst(1) NE nodata.
*------- BBKPF-Erweiterung zu 2.1A: VBUND --
              bbkpf-vbund(1) = nodata.
            ENDIF.

            bbkpf-xmwst(1) = nodata.
          ENDIF.

          bbkpf-docid(1) = nodata.
          bbkpf-barcd(1) = nodata.
          bbkpf-stodt(1) = nodata.
        ENDIF.

        bbkpf-brnch(1) = nodata.
        bbkpf-numpg(1) = nodata.
      ENDIF.

      bbkpf-stgrd(1) = nodata.
    ENDIF.

    bbkpf-kursf_m(1) = nodata.

    IF xmess_bbkpf-sende NE 'X'.
      xmess_bbkpf-sende = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM BBSEG_ERWEITERUNG_PRUEFEN.
*-----------------------------------------------------------------------
FORM bbseg_erweiterung_pruefen.
  IF bbseg-sende(1) NE nodata.         "레코드 종료
*    지시?
    IF bbseg-wenr(1) NE nodata.
      IF bbseg-pycur(1) NE nodata.
        IF bbseg-dtaws(1) NE nodata.
          IF bbseg-xnegp(1) NE nodata.
            IF bbseg-fipex(1) NE nodata.
              IF bbseg-rstgr(1) NE nodata.
                IF bbseg-vbund(1) NE nodata.
                  IF bbseg-wdate(1) NE nodata.
                    IF bbseg-xref1(1) NE nodata.
                      IF bbseg-pprct(1) NE nodata.
                        IF bbseg-xegdr(1) NE nodata.

                          bbseg-nplnr(1)   = nodata.
                          bbseg-vornr(1)   = nodata.
                        ENDIF.

                        bbseg-xegdr(1)   = nodata.
                        bbseg-recid(1)   = nodata.
                      ENDIF.

                      bbseg-pprct(1)  = nodata.
                      bbseg-projk(1)  = nodata.
                      bbseg-uzawe(1)  = nodata.
                      bbseg-txjcd(1)  = nodata.
                      bbseg-fistl(1)  = nodata.
                      bbseg-geber(1)  = nodata.
                      bbseg-dmbe2(1)  = nodata.
                      bbseg-dmbe3(1)  = nodata.
                      bbseg-pargb(1)  = nodata.
                    ENDIF.

                    bbseg-xref1(1)   = nodata.
                    bbseg-xref2(1)   = nodata.
                    bbseg-kblpos(1)  = nodata.
                    bbseg-kblnr(1)   = nodata.

                  ENDIF.
                  bbseg-wdate(1)   = nodata.
                  bbseg-wgbkz(1)   = nodata.
                  bbseg-xaktz(1)   = nodata.
                  bbseg-wname(1)   = nodata.
                  bbseg-wort1(1)   = nodata.
                  bbseg-wbzog(1)   = nodata.
                  bbseg-wort2(1)   = nodata.
                  bbseg-wbank(1)   = nodata.
                  bbseg-wlzbp(1)   = nodata.
                  bbseg-diskp(1)   = nodata.
                  bbseg-diskt(1)   = nodata.
                  bbseg-winfw(1)   = nodata.
                  bbseg-winhw(1)   = nodata.
                  bbseg-wevwv(1)   = nodata.
                  bbseg-wstat(1)   = nodata.
                  bbseg-wmwkz(1)   = nodata.
                  bbseg-wstkz(1)   = nodata.
                ENDIF.

                bbseg-vbund(1)   = nodata.
                bbseg-fkber(1)   = nodata.
                bbseg-dabrz(1)   = nodata.
                bbseg-xstba(1)   = nodata.
              ENDIF.

              bbseg-rstgr(1)   = nodata.
            ENDIF.

            bbseg-fipex(1)   = nodata.

          ENDIF.

          bbseg-xnegp(1)   = nodata.
          bbseg-gricd(1)   = nodata.
          bbseg-grirg(1)   = nodata.
          bbseg-gityp(1)   = nodata.
          bbseg-fityp(1)   = nodata.
          bbseg-stcdt(1)   = nodata.
          bbseg-stkzn(1)   = nodata.
          bbseg-stcd3(1)   = nodata.
          bbseg-stcd4(1)   = nodata.
          bbseg-xref3(1)   = nodata.
          bbseg-kidno(1)   = nodata.
          bbseg-dtws1(1)   = nodata.
          bbseg-dtws2(1)   = nodata.
          bbseg-dtws3(1)   = nodata.
          bbseg-dtws4(1)   = nodata.
        ENDIF.

        bbseg-dtaws(1)   = nodata.
      ENDIF.

      bbseg-pycur(1)   = nodata.
      bbseg-pyamt(1)   = nodata.
      bbseg-bupla(1)   = nodata.
      bbseg-secco(1)   = nodata.
      bbseg-lstar(1)   = nodata.
      bbseg-egdeb(1)   = nodata.
    ENDIF.
    bbseg-wenr(1)   = nodata.
    bbseg-genr(1)   = nodata.
    bbseg-grnr(1)   = nodata.
    bbseg-menr(1)   = nodata.
    bbseg-mive(1)   = nodata.
    bbseg-nksl(1)   = nodata.
    bbseg-empsl(1)   = nodata.
    bbseg-svwnr(1)   = nodata.
    bbseg-sberi(1)   = nodata.
    bbseg-kkber(1)   = nodata.
    bbseg-empfb(1)   = nodata.
    bbseg-kursr_m(1)   = nodata.

    IF xmess_bbseg-sende NE 'X'.
      xmess_bbseg-sende = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM BBTAX_ERWEITERUNG_PRUEFEN.
*-----------------------------------------------------------------------
FORM bbtax_erweiterung_pruefen.

  IF bbtax-sende(1) NE nodata.
    bbtax-h2ste(1) = nodata.
    bbtax-h3ste(1) = nodata.

    IF xmess_bbtax-sende NE 'X'.
      xmess_bbtax-sende = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------
*        Form  BELEG_ABSCHLIESSEN
*-----------------------------------------------------------------------
FORM beleg_abschliessen.

  IF bbkpf-tcode = 'FB01'
  OR bbkpf-tcode = 'FBB1'                                   "P30K125019
  OR bbkpf-tcode = 'FBS1'
  OR bbkpf-tcode = 'FBV1'.             "4.0
    CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
         EXPORTING
              i_tcode  = bbkpf-tcode
         IMPORTING
              e_subrc  = subrc
              e_msgid  = msgid
              e_msgty  = msgty
              e_msgno  = msgno
              e_msgv1  = msgv1
              e_msgv2  = msgv2
              e_msgv3  = msgv3
              e_msgv4  = msgv4
         TABLES
              t_ftpost = ftpost
              t_fttax  = fttax
              t_blntab = xblntab
         EXCEPTIONS
              OTHERS   = 1.

  ELSEIF bbkpf-tcode = 'FB05'.
    IF NOT xftclear IS INITIAL.
      ftclear = save_ftclear.
      APPEND ftclear.
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
         EXPORTING
              i_auglv   = bbkpf-auglv
              i_tcode   = bbkpf-tcode
         IMPORTING
              e_subrc   = subrc
              e_msgid   = msgid
              e_msgty   = msgty
              e_msgno   = msgno
              e_msgv1   = msgv1
              e_msgv2   = msgv2
              e_msgv3   = msgv3
              e_msgv4   = msgv4
         TABLES
              t_ftpost  = ftpost
              t_ftclear = ftclear
              t_fttax   = fttax
              t_blntab  = xblntab
         EXCEPTIONS
              OTHERS    = 1.
  ENDIF.

*------- Call Transaction ----------------------------------------------
  IF subrc IS INITIAL AND sy-subrc IS INITIAL.
*-- 성공했을때 message -> ?   by hj.lee
    CLEAR xblntab.

    READ TABLE xblntab INDEX 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = sy-msgid
                msgnr               = sy-msgno
                msgv1               = sy-msgv1
                msgv2               = sy-msgv2
                msgv3               = sy-msgv3
                msgv4               = sy-msgv4
           IMPORTING
                message_text_output = etab-text.
    ELSE.
      etab-text = xblntab-belnr.
    ENDIF.

    READ TABLE itab INDEX 1.
    MOVE-CORRESPONDING itab TO etab.
    APPEND etab.

    s_cnt = s_cnt + 1.

  ELSE.
    READ TABLE itab INDEX 1.
    MOVE-CORRESPONDING itab TO etab.

    IF NOT subrc IS INITIAL.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = msgid
                msgnr               = msgno
                msgv1               = msgv1
                msgv2               = msgv2
                msgv3               = msgv3
                msgv4               = msgv4
           IMPORTING
                message_text_output = etab-text.
    ELSE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = sy-msgid
                msgnr               = sy-msgno
                msgv1               = sy-msgv1
                msgv2               = sy-msgv2
                msgv3               = sy-msgv3
                msgv4               = sy-msgv4
           IMPORTING
                message_text_output = etab-text.
    ENDIF.

* 2000.04.20 JI.PARK
* ERROR를 WRITE하기 위하여 추가.

    etab-err_check = 'ERROR'.
********************************

    APPEND etab.
    CLEAR  etab.

    e_cnt = e_cnt + 1.

*   IF ERR_FLAG = 'X'.
    PERFORM error_processing.
*   ENDIF.

  ENDIF.

  REFRESH: ftpost, ftclear, fttax, xblntab.
  CLEAR:   ftpost, ftclear, fttax, xblntab.

ENDFORM.

*-----------------------------------------------------------------------
*        Form  DATENSATZ_PRUEFEN
*-----------------------------------------------------------------------
FORM datensatz_pruefen.
  satz2_count = satz2_count + 1.

ENDFORM.

*-----------------------------------------------------------------------
*        Form  DATENSATZ_TRANSPORTIEREN
*-----------------------------------------------------------------------
FORM datensatz_transportieren.
  CASE wa+2(09).

*-----------------------------------------------------------------------
*        BBSEG Belegsegment
*-----------------------------------------------------------------------
    WHEN 'BSEG'.
      save_tbnam = 'BBSEG'.

      PERFORM bbseg_erweiterung_pruefen.
**S>08/04/11 Paul dummy field change to dummyx.

      IF  bbseg-dummyx(1) NE nodata
      AND bbseg-dummyx    NE space
      AND bbseg-newko(1) NE nodata
      AND bbseg-newko    NE space
      AND bbseg-dummyx    EQ bbseg-newko.
        CLEAR bbseg-dummyx.
        bbseg-dummyx = nodata.
      ENDIF.
      IF  bbseg-dummyx(1) NE nodata
      AND bbseg-dummyx    NE space.
        bbseg-newko = bbseg-dummyx.
        CLEAR bbseg-dummyx.
        bbseg-dummyx = nodata.
      ENDIF.
**E<Paul

*------ Kontoart ermitteln
      PERFORM kontoart_ermitteln.
      bbseg_count = bbseg_count + 1.

*------ Quellensteuer: Z?ler inkrementieren (Debitor/Kreditor-Zeilen)
      IF xtbsl-koart = 'D' OR
         xtbsl-koart = 'K'.
        wt_count = wt_count + 1.
      ENDIF.

      PERFORM fill_ftpost_with_bbseg_data USING bbseg_count.
      PERFORM fill_fttax_from_bbseg.

*-----------------------------------------------------------------------
*        BWITH Quellensteuer
*-----------------------------------------------------------------------
    WHEN 'WITH'.
      IF bwith-witht    EQ space
      OR bwith-witht    EQ nodata.
        PERFORM dump_wa USING 'BBKPF'.
        PERFORM dump_wa USING 'BWITH'.
      ENDIF.

      PERFORM fill_ftpost_with_bwith_data USING bbseg_count.

*-----------------------------------------------------------------------
*        BBSEG Belegsteuern
*-----------------------------------------------------------------------
    WHEN 'BTAX'.
      IF bbseg_tax = 'X'.
        PERFORM dump_wa USING 'BBKPF'.
        PERFORM dump_wa USING 'BBSEG'.
      ENDIF.
      IF bbkpf-xmwst = 'X'.
        PERFORM dump_wa USING 'BBKPF'.
        PERFORM dump_wa USING 'BBTAX'.
      ENDIF.

      PERFORM fill_fttax_with_bbtax_data.

*-----------------------------------------------------------------------
*        BSELK Selektionskopf (FB05)
*-----------------------------------------------------------------------
    WHEN 'SELK'.
      CLEAR xftclear.
      PERFORM move_bselk_to_save_ftclear.
*-------- Selektion mit Avis , Nach Alter sortieren ---------------
      IF bselk-avsid(1) NE nodata.     "Avis
        ftclear = save_ftclear.
        APPEND ftclear.
        CLEAR save_tbnam.
      ELSE.
        xftclear = 'X'.
        save_tbnam = 'BSELK'.
      ENDIF.

*-----------------------------------------------------------------------
*        BSELP Selektionspositionen (FB05)
*-----------------------------------------------------------------------
    WHEN 'SELP'.
      IF save_tbnam NE space.
        CASE save_tbnam.
          WHEN 'BSELP'.
          WHEN 'BSELK'.
          WHEN OTHERS.
            PERFORM dump_wa USING 'BBKPF'.
        ENDCASE.
      ENDIF.
      save_tbnam = 'BSELP'.
      PERFORM fill_ftclear_with_bselp_data.
  ENDCASE.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  DUMP_WA
*-----------------------------------------------------------------------
FORM dump_wa USING table.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            langu          = sy-langu
            tabname        = table
       TABLES
            nametab        = nametab
       EXCEPTIONS
            no_texts_found = 1.
  LOOP AT nametab.
    CLEAR char.
    char(5)    = nametab-tabname.
    char+5(1)  = '-'.
    char+6(10) = nametab-fieldname.
    ASSIGN (char) TO <f1>.
    wert = <f1>.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  KOPFSATZ_BEARBEITEN
*-----------------------------------------------------------------------
FORM kopfsatz_bearbeiten.
  bbkpf =  i_bbkpf.
  bbseg =  i_bbseg.
  bbtax =  i_bbtax.
  bselk =  i_bselk.
  bselp =  i_bselp.
  bwith =  i_bwith.
  CLEAR: satz2_count, satz2_cnt_akt, wt_count, bbseg_count.
  CLEAR: save_tbnam, fcode, bbseg_tax.
  REFRESH: ftpost, ftclear, fttax.
  REFRESH: t_bbkpf, t_bbseg, t_bbtax, t_bwith.

  bbkpf = wa.

  PERFORM bbkpf_erweiterung_pruefen.

  beleg_count = beleg_count + 1.
  count = count + 1.

*------ Place to set a soft break-point -------------------------------
  IF beleg_count = beleg_break.
    beleg_break = beleg_count.
  ENDIF.

*------- FB05: --> Ausgleichsvorgang auf G?tigkeit ?erpruefne --------
  IF  bbkpf-tcode  = 'FB05'.
    PERFORM auglv_pruefen.
  ENDIF.

  bukrs = bbkpf-bukrs.

  PERFORM fill_ftpost_with_bbkpf_data.
  bbkpf_ok = 'X'.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MAPPE_OEFFNEN
*-----------------------------------------------------------------------
FORM mappe_oeffnen.

*-------- Interne Buchungsschnittstelle initialisieren
  CALL FUNCTION 'POSTING_INTERFACE_START'
       EXPORTING
            i_function = function
            i_client   = bgr00-mandt
            i_group    = bgr00-group
            i_xbdcc    = xbdcc
            i_holddate = bgr00-start
            i_keep     = bgr00-xkeep
            i_mode     = anz_mode
            i_update   = update
            i_user     = bgr00-usnam.

  group_open = 'X'.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MAPPE_PRUEFEN_OEFFNEN
*-----------------------------------------------------------------------
FORM mappe_pruefen_oeffnen.
  CLEAR bgr00.
  bgr00 = wa.
  group_count = group_count + 1.


*------- Sonderzeichen NODATA pr?en/?ernehmen -----------------------
  IF bgr00-nodata = space.
    nodata = c_nodata.
  ELSE.
    nodata = bgr00-nodata.
  ENDIF.

  PERFORM mappe_oeffnen.

*------- Flags, Z?ler initialisieren ----------------------------------
  CLEAR: xnewg, beleg_count, satz2_count, satz2_cnt_akt, wt_count.

*------- Initialstrukturen erzeugen (NODATA-Sonderzeichen --------------
  IF nodata NE nodata_old.
    PERFORM init_strukturen_erzeugen(rfbibli0) USING nodata.
    PERFORM init_bbkpf(rfbibli0) USING i_bbkpf.
    PERFORM init_bbseg(rfbibli0) USING i_bbseg.
    PERFORM init_bbtax(rfbibli0) USING i_bbtax.
    PERFORM init_bselk(rfbibli0) USING i_bselk.
    PERFORM init_bselp(rfbibli0) USING i_bselp.
    PERFORM init_bwith(rfbibli0) USING i_bwith.
    nodata_old = nodata.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MAPPE_SCHLIESSEN
*-----------------------------------------------------------------------
FORM mappe_schliessen.
  IF group_open = 'X'.
    CALL FUNCTION 'POSTING_INTERFACE_END'.
    CLEAR group_open.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MAPPEN_WECHSEL
*-----------------------------------------------------------------------
FORM mappen_wechsel.
  PERFORM mappe_schliessen.
  PERFORM mappe_pruefen_oeffnen.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MESSAGE_AUSGEBEN
*-----------------------------------------------------------------------
FORM message_call_transaction.

*------- neuer Eintrag aus T100 ----------------------------------------
  IF t100-sprsl NE sy-langu
  OR t100-arbgb NE sy-msgid
  OR t100-msgnr NE sy-msgno.
    CLEAR: text, text1, text2, text3, msgvn.
    SELECT SINGLE * FROM t100 WHERE sprsl = sy-langu
                              AND   arbgb = sy-msgid
                              AND   msgnr = sy-msgno.
    IF sy-subrc = 0.
      text = t100-text.
      DO 4 TIMES VARYING msgvn FROM sy-msgv1 NEXT sy-msgv2.
        IF text CA '$'.
          REPLACE '$' WITH msgvn INTO text.
          CONDENSE text.
        ENDIF.
        IF text CA '&'.
          REPLACE '&' WITH msgvn INTO text.
          CONDENSE text.
        ENDIF.
      ENDDO.
      text1 = text(40).
      text2 = text+40(40).
      text3 = text+80(40).
    ENDIF.

*------- gleicher Eintrag aus T100 -------------------------------------
  ELSE.
    IF text NE space.
      CLEAR: text, text1, text2, text3, msgvn.
      text = t100-text.
      DO 4 TIMES VARYING msgvn FROM sy-msgv1 NEXT sy-msgv2.
        IF text CA '$'.
          REPLACE '$' WITH msgvn INTO text.
          CONDENSE text.
        ENDIF.
        IF text CA '&'.
          REPLACE '&' WITH msgvn INTO text.
          CONDENSE text.
        ENDIF.
      ENDDO.
      text1 = text(40).
      text2 = text+40(40).
      text3 = text+80(40).
    ENDIF.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  SAVE_DATENSATZ_BEARBEITEN
*-----------------------------------------------------------------------
FORM save_datensatz_bearbeiten.

  satz2_cnt_akt = satz2_count.

ENDFORM.

*-----------------------------------------------------------------------
*        Form  KONTOART_ERMITTELN.
*-----------------------------------------------------------------------
FORM kontoart_ermitteln.

  LOOP AT xtbsl WHERE bschl = bbseg-newbs.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM tbsl WHERE bschl = bbseg-newbs.
    IF sy-subrc = 0.
      xtbsl = tbsl.
      APPEND xtbsl.
    ELSE.
      PERFORM dump_wa USING 'BBKPF'.
      PERFORM dump_wa USING 'BBSEG'.
    ENDIF.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  MOVE_BSELK_TO_SAVE_FTCLEAR.
*-----------------------------------------------------------------------
FORM move_bselk_to_save_ftclear.
  CLEAR save_ftclear.
  IF bselk-agkon(1) NE nodata.
    save_ftclear-agkon = bselk-agkon.
  ENDIF.
  IF bselk-agkoa(1) NE nodata.
    save_ftclear-agkoa = bselk-agkoa.
  ENDIF.
  IF bselk-xnops(1) NE nodata.
    save_ftclear-xnops = bselk-xnops.
  ENDIF.
  IF bselk-agbuk(1) NE nodata.
    save_ftclear-agbuk = bselk-agbuk.
  ENDIF.
  IF bselk-agums(1) NE nodata.
    save_ftclear-agums = bselk-agums.
  ENDIF.
  IF bselk-avsid(1) NE nodata.
    save_ftclear-avsid = bselk-avsid.
  ENDIF.
  IF bselk-xfifo(1) NE nodata.
    save_ftclear-xfifo = bselk-xfifo.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*        Form  FILL_FTTAX_FROM_BBSEG.
*-----------------------------------------------------------------------
FORM fill_fttax_from_bbseg.
  CLEAR fttax.
  IF       xtbsl-koart  =  'S'
  AND (    bbseg-wmwst(1) NE nodata
        OR bbseg-mwsts(1) NE nodata ).
    IF bbkpf-xmwst  = 'X'.
      PERFORM dump_wa USING 'BBKPF'.
    ENDIF.

    bbseg_tax = 'X'.

    IF bbseg-wmwst = '*' OR bbseg-mwsts = '*'.
      READ TABLE ftpost INDEX 1.
      IF ftpost-fnam NE 'BKPF-XMWST'.
        CLEAR: ftpost.
        ftpost-stype = 'K'.
        ftpost-count = '001'.
        ftpost-fnam = 'BKPF-XMWST'.
        ftpost-fval = 'X'.
        INSERT ftpost INDEX 1.
      ENDIF.
    ELSE.
      IF bbseg-wmwst(1) NE nodata.
        fttax-fwste = bbseg-wmwst.
      ENDIF.
      IF bbseg-mwsts(1) NE nodata.
        fttax-hwste = bbseg-mwsts.
      ENDIF.
      fttax-mwskz = bbseg-mwskz.
      fttax-bschl = bbseg-newbs.
      APPEND fttax.
    ENDIF.
  ENDIF.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  LOOP_AT_TABLE_TFILE
*&---------------------------------------------------------------------*
FORM loop_at_table_tfile.
  DATA: counter(5) TYPE n.                                 "note 147122
  CLEAR group_count .

  PERFORM set_global_variable.

  DO.
    counter = counter + 1.

    IF counter > 1.
      EXIT.
    ENDIF.

    LOOP AT tfile.
      wa = tfile-rec.
      IF sy-tabix = 1 AND counter = 1.
        save_bgr00 = wa.
      ENDIF.

      CASE wa(1).
        WHEN '0'.                      "GENERAL INFO
          PERFORM letzten_beleg_abschliessen.
          PERFORM mappen_wechsel.
        WHEN '1'.                      "BKPF
          PERFORM letzten_beleg_abschliessen.
          PERFORM kopfsatz_bearbeiten.
          REFRESH ertab.
          ertab = wa.
          APPEND ertab.
        WHEN '2'.                      "BSEG
          PERFORM datensatz_pruefen.
          satz2_cnt_akt = satz2_count - 1.
          PERFORM wa_daten_uebertragen.
          PERFORM datensatz_transportieren.
          ertab = wa.
          APPEND ertab.
        WHEN OTHERS.
*------- ung?tiger Satztyp --------------------------------------------
          satz2_count = satz2_count + 1.
          PERFORM dump_wa USING 'BBKPF'.
      ENDCASE.
    ENDLOOP.
*   REFRESH TFILE.
  ENDDO.
  PERFORM letzten_beleg_abschliessen.
  PERFORM mappe_schliessen.
*------- letzter CALL_BI_END_AKT_NUMBER  -------------------------------
  PERFORM call_bi_end_akt_number.
  COMMIT WORK.
  CALL FUNCTION 'DEQUEUE_ALL'.
  CLEAR commit_count.
ENDFORM.                               " LOOP_AT_TABLE_TFILE

*&---------------------------------------------------------------------*
*&      Form  ERROR_PROCESSING
*&---------------------------------------------------------------------*
FORM error_processing.

  REFRESH: efile.                      ", TFILE.

  function = 'B'.                      "? HJ.LEE
  CLEAR: startnum, all_commit, commit_count, numerror, count.
* TFILE = SAVE_BGR00.
* INSERT TFILE INDEX 1.

  PERFORM loop_at_table_tfile(rfbibl01). "for BATCH
  CLEAR : tfile, tfile[].
  function = 'C'.                      "? HJ.LEE

* ENDIF.
ENDFORM.                               " ERROR_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  LETZTEN_BELEG_ABSCHLIESSEN
*&---------------------------------------------------------------------*
FORM letzten_beleg_abschliessen.

  IF count > startnum.
*.. ftpost : bdcdata
    CLEAR tfill_ftpost.
    DESCRIBE TABLE ftpost LINES tfill_ftpost.
    IF tfill_ftpost > 0.
      PERFORM beleg_abschliessen.
    ENDIF.
  ENDIF.
  CLEAR: bbkpf_ok.
ENDFORM.                               " LETZTEN_BELEG_ABSCHLIESSEN

*&---------------------------------------------------------------------*
*&      Form  FAST_ENTRY
*&---------------------------------------------------------------------*
FORM fast_input.
  DATA: bukrs LIKE bkpf-bukrs,
        gjahr LIKE bkpf-gjahr,
        belnr LIKE bkpf-belnr.

* CHECK FL_CHECK = SPACE.

  IF bbkpf-tcode = 'FB01' OR bbkpf-tcode = 'FBB1'.
*  Batch-Input FLAG for Check doc type
    sy-binpt = 'X'.
    CALL FUNCTION 'AC_DOCUMENT_DIRECT_INPUT'
         EXPORTING
              i_nodata      = nodata
         IMPORTING
              e_bukrs       = bukrs
              e_gjahr       = gjahr
              e_belnr       = belnr
         TABLES
              t_bbkpf       = t_bbkpf
              t_bbseg       = t_bbseg
              t_bbtax       = t_bbtax
              t_bwith       = t_bwith
         EXCEPTIONS
              error_message = 01.
    IF sy-subrc IS INITIAL.
      MESSAGE i312(f5) WITH belnr bukrs.
    ELSE.
      IF sy-msgty = 'A'.
        MESSAGE ID     sy-msgid
                TYPE   'A'
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        IF sy-batch IS INITIAL.
          MESSAGE ID     sy-msgid
                  TYPE   'I'
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
      PERFORM export_error_data.
    ENDIF.
  ELSE.
    PERFORM export_error_data.
  ENDIF.

*------- Commit Work? ------------------------------------------------
  commit_count = commit_count + 1.
  IF commit_count = max_commit.
    PERFORM call_bi_end_akt_number.
    COMMIT WORK.
    CALL FUNCTION 'DEQUEUE_ALL'.
    CLEAR commit_count.
  ENDIF.

  REFRESH: t_bbkpf,
           t_bbseg,
           t_bwith,
           t_bbtax.
  CLEAR:   t_bbkpf,                                         "30C
           t_bbseg,                                         "30C
           t_bwith,
           t_bbtax.                                         "30C
ENDFORM.                               " FAST_ENTRY

*&---------------------------------------------------------------------*
*&      Form  CALL_BI_END_AKT_NUMBER
*&---------------------------------------------------------------------*
FORM call_bi_end_akt_number.
  CHECK tbist_aktiv = 'X'.

  all_commit = all_commit + commit_count.
  PERFORM execute_bi_end_akt_number.
  CLEAR commit_count.
  CLEAR numerror.

ENDFORM.                               " CALL_BI_END_AKT_NUMBER

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_BI_END_AKT_NUMBER
*&---------------------------------------------------------------------*
FORM execute_bi_end_akt_number.

  CALL FUNCTION 'BI_END_AKT_NUMBER'
       EXPORTING
            jobname          = jobid
            actual_number    = all_commit
            number_of_errors = numerror"eine relative Zahl,
               "Anzahl Fehler seit dem letzem(!) COMMIT, wird im FB
                                       "kumuliert.
*           EXTERNAL_NUMBER  = 'interne Nummernvergabe'
       EXCEPTIONS
            internal_error   = 01
            not_found        = 02.

  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      ROLLBACK WORK.
    WHEN 2.
      ROLLBACK WORK.
  ENDCASE.

ENDFORM.                               " EXECUTE_BI_END_AKT_NUMBER

*&---------------------------------------------------------------------*
*&      Form  EXPORT_ERROR_DATA
*&---------------------------------------------------------------------*
FORM export_error_data.
  DESCRIBE TABLE ertab LINES tfill_ertab.
  CHECK tfill_ertab > 0.

  IMPORT save_bgr00 efile FROM DATABASE terrd(fi) ID jobid.

  LOOP AT ertab.
    efile = ertab.
    APPEND efile.
  ENDLOOP.

  EXPORT save_bgr00 efile TO DATABASE terrd(fi) ID jobid.
  numerror = numerror + 1.
ENDFORM.                               " EXPORT_ERROR_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_BI_CLOSE_ENTRY
*&---------------------------------------------------------------------*
FORM call_bi_close_entry.

  DATA: counter(5) TYPE n.
  IF tbist_aktiv = 'X' AND sy-batch = 'X'.
    CALL FUNCTION 'BI_CLOSE_ENTRY'
         EXPORTING
              jobname        = jobid
         EXCEPTIONS
              internal_error = 01
              not_found      = 02.

    CASE sy-subrc.
      WHEN 0.
        DELETE FROM DATABASE terrd(fi) ID jobid.
        DO.
          counter = counter + 1.
          jobid_ext+27(5) = counter.
          DELETE FROM DATABASE tfsave(fi) ID jobid_ext.
          IF sy-subrc <> 0 .
            EXIT.
          ENDIF.
        ENDDO.
      WHEN 1.
      WHEN 2.
    ENDCASE.
  ELSE.
    DELETE FROM DATABASE terrd(fi) ID jobid.
  ENDIF.
ENDFORM.                               " CALL_BI_CLOSE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  BSELP_FIELD_LENGHT_CONVERT
*&---------------------------------------------------------------------*
FORM bselp_field_lenght_convert CHANGING p_wa.
  DATA: wa_tmp(2600) TYPE c,           " help work area
        offset_o TYPE i,               " old offset (<4.0A)
        offset_n TYPE i.               " new offset (>= 4.0A)

  CLEAR wa_tmp.
  wa_tmp(31) = p_wa(31).
  offset_n = 31.
  offset_o = 31.
  DO 18 TIMES.
    wa_tmp+offset_n(5) = p_wa+offset_o(5).
    offset_n = offset_n + 30.
    offset_o = offset_o + 5.
    DO 2 TIMES.
      wa_tmp+offset_n(20) = p_wa+offset_o(20).
      offset_n = offset_n + 30.
      offset_o = offset_o + 20.
    ENDDO.
  ENDDO.
  CLEAR p_wa.
  p_wa = wa_tmp.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
FORM clear_data.

  CLEAR: xeof, xnewg,
         group_count, beleg_count,
         satz2_count, satz2_cnt_akt,
         wt_count,
         commit_count, count.

ENDFORM.                               " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_BGR00_RECORD
*&---------------------------------------------------------------------*
FORM fill_bgr00_record.
  CLEAR : tfile, tfile[].

  CLEAR : *bgr00.
  MOVE : '0'        TO  *bgr00-stype,
         'ROUP'     TO  *bgr00-group,
         sy-mandt   TO  *bgr00-mandt,
         sy-uname   TO  *bgr00-usnam,
         '/'        TO  *bgr00-nodata.

  tfile = *bgr00.
  APPEND tfile.

ENDFORM.                               " FILL_BGR00_RECORD
*&---------------------------------------------------------------------*
*&      Form  FILL_BBKPF_RECORD
*&---------------------------------------------------------------------*
FORM fill_bbkpf_record.
  DATA v_tcode LIKE bbkpf-tcode.       "트랜잭션코?
  CLEAR v_tcode.

*.. 2000.05.24 JI.PARK
  IF p_park = 'X'.
    v_tcode = 'FBV1'.
  ELSE.
    v_tcode = 'FB01'.
  ENDIF.
*...

  MOVE itab-blart  TO  *bbkpf-blart.

  MOVE : '1'         TO  *bbkpf-stype,
*        'FB01'      TO  *BBKPF-TCODE,
         v_tcode     TO  *bbkpf-tcode,
         itab-bldat  TO  *bbkpf-bldat,
         itab-blart  TO  *bbkpf-blart,
*
         v_bukrs     TO  *bbkpf-bukrs,
         itab-budat  TO  *bbkpf-budat,

* -- 2001.01.02 JINA Ahn
*        'KRW'       TO  *bbkpf-waers,
         p_curr  TO  *bbkpf-waers,
* --
*        HTAB-BELNR  TO  *BBKPF-BELNR,
* -- 2001.01.02 JINA Ahn
         itab-xblnr  TO  *bbkpf-xblnr,
         itab-bktxt  TO  *bbkpf-bktxt,
* --
         '/'         TO  *bbkpf-sende.

  PERFORM fill_nodata_to_bkpf.

  tfile = *bbkpf.
  APPEND tfile.

ENDFORM.                               " FILL_BBKPF_RECORD
*&---------------------------------------------------------------------*
*&      Form  FILL_NODATA_TO_BKPF
*&---------------------------------------------------------------------*
FORM fill_nodata_to_bkpf.

  LOOP AT bbkpf_tab.
    CLEAR : fname.                     ", <FN>.
    CONCATENATE '*BBKPF-' bbkpf_tab-fieldname INTO fname.
    ASSIGN (fname) TO <fn>.
    IF <fn> IS INITIAL.
      MOVE '/' TO <fn>.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " FILL_NODATA_TO_BKPF
*&---------------------------------------------------------------------*
*&      Form  SET_GLOBAL_VARIABLE
*&---------------------------------------------------------------------*
FORM set_global_variable.

* -----  read currency exchange rate prefixes --------------------------
  CALL FUNCTION 'RATE_GET_PREFIXES'
       EXPORTING
            client   = sy-mandt
       IMPORTING
            prefix_p = prefix_p
            prefix_m = prefix_m.

ENDFORM.                               " SET_GLOBAL_VARIABLE
*&---------------------------------------------------------------------*
*&      Form  COMPUTE_TAX_AMOUNT
*&---------------------------------------------------------------------*
FORM compute_tax_amount.
  CHECK mwskz_flag IS INITIAL.         " 'X' -> MWSKZ = <, >
  CHECK NOT itab-mwskz IS INITIAL.
* CHECK NOT ITAB-HWSTE IS INITIAL.

  MOVE : itab-mwskz  TO  itax-mwskz,
         itab-hwste  TO  itax-hwste.

  IF itab-newbs = '40'.
    itax-bschl = '40'.
  ELSEIF itab-newbs = '50'.
    itax-bschl = '50'.
  ENDIF.

  COLLECT itax.
ENDFORM.                               " COMPUTE_TAX_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  FILL_BBSEG_RECORD
*&---------------------------------------------------------------------*
FORM fill_bbseg_record.
  DATA: o_lifnr LIKE lfa1-lifnr.
  CLEAR : *bbseg, tfile.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = itab-newko
       IMPORTING
            output = o_lifnr
       EXCEPTIONS
            OTHERS = 1.

  IF mwskz_flag = 'X'.
     *bbseg-xstba = 'X'.   "Determine tax base?
  ENDIF.

*withholding vendor
*fixme...
*  SELECT SINGLE *      FROM lfbw
*         WHERE lifnr = o_lifnr
*           AND bukrs = v_bukrs
*           AND witht = 'K1'
*           AND wt_subjct = 'X'.
*  IF sy-subrc = 0.
*    wt_sw    = itab-wt_withcd. "for withholding tax : k1, k2
*  ENDIF.

* One Time Vendor
  SELECT SINGLE *      FROM lfa1
         WHERE lifnr = o_lifnr AND xcpdk = 'X'.
  IF sy-subrc = 0.                "name,address,bank
    MOVE : itab-name1 TO *bbseg-name1,
           itab-ort01 TO *bbseg-ort01,
           itab-bankl TO *bbseg-bankl,
           itab-stcd1 TO *bbseg-stcd1,
           itab-bankn TO *bbseg-bankn.
  ENDIF.

*asset transaction type.
  IF itab-newbs = '70' OR itab-newbs = '75'.
    IF itab-anbwa = space.
      MOVE '100'      TO  *bbseg-anbwa.      "acquisition
    ELSE.
      MOVE itab-anbwa TO  *bbseg-anbwa.    "acquisition
    ENDIF.
  ENDIF.


  MOVE : '2'           TO  *bbseg-stype,
         'BBSEG'       TO  *bbseg-tbnam,
*        '0'           TO  *BBSEG-SKFBT,
         itab-zfbdt    TO  *bbseg-zfbdt,    "due on date / baseline
         itab-newbs    TO  *bbseg-newbs,
         itab-newko    TO  *bbseg-newko,
*        itab-wskto    to  *bbseg-wskto,
         itab-newum    TO  *bbseg-newum,    "special gl
         itab-wrbtr    TO  *bbseg-wrbtr,
*        iTAB-DMBTR    TO  *BBSEG-DMBTR,
         itab-mwskz    TO  *bbseg-mwskz,
         itab-bupla    TO  *bbseg-bupla,
         itab-pernr    TO  *bbseg-pernr,
         itab-kostl    TO  *bbseg-kostl,
         itab-fistl    TO  *bbseg-fistl,
         itab-aufnr    TO  *bbseg-aufnr,
         itab-ebeln    TO  *bbseg-ebeln,
         itab-ebelp    TO  *bbseg-ebelp,
         itab-valut    TO  *bbseg-valut,
         itab-zterm    TO  *bbseg-zterm,
         itab-zlsch    TO  *bbseg-zlsch,
         itab-zlspr    TO  *bbseg-zlspr,
*        ITAB-PYCUR    TO  *BBSEG-PYCUR,
*\         ITAB-ZZHKONT  TO  *BBSEG-ZZHKONT,
*\         ITAB-ZZBTSID  TO  *BBSEG-ZZBTSID,
         itab-zuonr    TO  *bbseg-zuonr,
         itab-sgtxt    TO  *bbseg-sgtxt,
*        ITAB-XNEGP    TO  *BBSEG-XNEGP,
         '/'           TO  *bbseg-sende.

*Cash discount & Amount Eligible for Cash Discount
  IF itab-newbs = '01' OR itab-newbs = '11' OR
     itab-newbs = '31' OR itab-newbs = '21' .
    MOVE :  itab-wskto    TO  *bbseg-wskto,
            itab-wrbtr    TO  *bbseg-skfbt,
            '/'           TO  *bbseg-mwskz.

  ENDIF.

*temp:clear taxcode of posting key 39,29
  IF itab-newbs = '29' OR itab-newbs = '39'.
    MOVE: '/'          TO  *bbseg-mwskz.

*only special g/l (except D/P-A,BoE-W) has due on date
*    tables: t074u.
*    select single * from t074u where koart = 'K'
*                                 and umskz = itab-newum.
*    if t074u-umsks <> 'A' and t074u-umsks <> 'W'.
*      move: '/'          TO  *bbseg-zfbdt.
*    endif.
  ENDIF.


*if w/h tax exist
  IF wt_sw NE ' '.
    MOVE: '001'       TO  *bbseg-bupla,"bizplace
          '001'       TO  *bbseg-secco."seccode
  ENDIF.

  PERFORM fill_nodata_to_bseg.

  tfile = *bbseg.
  APPEND tfile.

ENDFORM.                               " FILL_BBSEG_RECORD

*&---------------------------------------------------------------------*
*&      Form  FILL_NODATA_TO_BSEG
*&---------------------------------------------------------------------*
FORM fill_nodata_to_bseg.

  DATA initial_value(15).

  LOOP AT bbseg_tab.
    CLEAR : fname.                     "<FN>.
    CONCATENATE '*BBSEG-' bbseg_tab-fieldname INTO fname.
    ASSIGN (fname) TO <fn>.
    IF <fn> IS INITIAL.
      MOVE '/' TO <fn>.
    ENDIF.

*    IF fname = '*BBSEG-VALUT' AND *bbseg-valut = '00000000'.
*      MOVE '/' TO <fn>.
*    ENDIF.
*    IF fname = '*BBSEG-ZFBDT' AND *bbseg-zfbdt = '00000000'.
*      MOVE '/' TO <fn>.
*    ENDIF.
*    IF fname = '*BBSEG-ZBD1T' AND *bbseg-zbd1t = '000'.
*      MOVE '/' TO <fn>.
*    ENDIF.
*    IF fname = '*BBSEG-AUFNR' AND *bbseg-aufnr = '000000000000'.
*      MOVE '/' TO <fn>.
*    ENDIF.

    PERFORM get_initial_value USING bbseg_tab-fieldname
                           CHANGING initial_value.

    IF <fn> EQ initial_value.
      MOVE '/' TO <fn>.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " FILL_NODATA_TO_BSEG
*&---------------------------------------------------------------------*
*&      Form  FILL_BBTAX_RECORD
*&---------------------------------------------------------------------*
FORM fill_bbtax_record.

  LOOP AT itax.
    CLEAR : *bbtax, tfile.

    WRITE itax-hwste  TO  *bbtax-fwste DECIMALS 0.

    MOVE : '2'         TO  *bbtax-stype,
           'BBTAX'     TO  *bbtax-tbnam,
*          ITAX-HWSTE  TO  *BBTAX-HWSTE,
           itax-mwskz  TO  *bbtax-mwskz,
           itax-bschl  TO  *bbtax-bschl,
           '/'         TO  *bbtax-sende.

    PERFORM fill_nodata_to_btax.
    tfile = *bbtax.
    APPEND tfile.

  ENDLOOP.

ENDFORM.                               " FILL_BBTAX_RECORD

*&---------------------------------------------------------------------*
*&      Form  FILL_NODATA_TO_BTAX
*&---------------------------------------------------------------------*
FORM fill_nodata_to_btax.

  LOOP AT bbtax_tab.
    CLEAR : fname.                     "FN>.
    CONCATENATE '*BBTAX-' bbtax_tab-fieldname INTO fname.
    ASSIGN (fname) TO <fn>.
    IF <fn> IS INITIAL.
      MOVE '/' TO <fn>.
    ENDIF.
  ENDLOOP.


ENDFORM.                               " FILL_NODATA_TO_BTAX
*&---------------------------------------------------------------------*
*&      Form  SELECT_FIELD_NAME
*&---------------------------------------------------------------------*
FORM select_field_name.
*.. BBKPF, BBSEG, BBTAX 구성 FIELD NAME select..

  CLEAR : bbkpf_tab, bbkpf_tab[],
          bbseg_tab, bbseg_tab[],
          bbtax_tab, bbtax_tab[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE bbkpf_tab
    FROM dd03l
   WHERE tabname = 'BBKPF'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE bbseg_tab
    FROM dd03l
   WHERE tabname = 'BBSEG'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE bbtax_tab
    FROM dd03l
   WHERE tabname = 'BBTAX'.

* BEGIN OF UD1K954413
  DELETE bbkpf_tab WHERE fieldname = '.INCLUDE'.
* END OF UD1K954413
ENDFORM.                               " SELECT_FIELD_NAME
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT_LIST
*&---------------------------------------------------------------------*
FORM write_result_list.

  SKIP 2.
  WRITE:/7 '*** Log ***'.
  SKIP.
  WRITE:/7 'Success :', s_cnt,
        /7 'Error   :', e_cnt.

  SKIP 3.
  WRITE:/7 '*** BDC RESULT LIST ***'.
  SKIP 1.

  WRITE:/8    'Doc Date',
         19   'Type',
         24   'Post. Date',
         35   'PK',
         42   'Account',
         61   'Amount',
         82   'Tax',
         91   'Cost Ctr',
         102  'Reference',
         128  'Header Text',
         145  'Message Text'.
  SKIP.
  LOOP AT etab.
    WRITE:/2   etab-err_check,         "ERROR check
           8   etab-bldat,             "date
           19  etab-blart,             "Doc type
           24  etab-budat,             "Doc date
           35  etab-newbs,             "account
           42  etab-newko,             "post key
           61  etab-wrbtr,             "amt
           82  etab-mwskz,             "tax code
           91  etab-kostl,             "cost center
           117  etab-zterm,               "pay term
           129  etab-zlsch,               "special
           102  etab-xblnr,            "ref
           128  etab-bktxt,            "text
           145  etab-text COLOR 3.     "log

  ENDLOOP.

ENDFORM.                               " WRITE_RESULT_LIST
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITAB
*&---------------------------------------------------------------------*
FORM check_itab.
  CLEAR : newko, mwskz_flag.

  newko = itab-newko.
  SELECT SINGLE * FROM  skb1
         WHERE  bukrs  = v_bukrs
         AND    saknr  = newko.
  SELECT SINGLE * FROM  ska1
         WHERE  ktopl  = 'LGCA'
         AND    saknr  = skb1-saknr.


  IF skb1-mwskz IS INITIAL.
    CLEAR : itab-mwskz, itab-bupla.
  ELSEIF skb1-mwskz = '<' OR skb1-mwskz = '>'.
    mwskz_flag = 'X'.
  ENDIF.


*  IF itab-newbs <> '40' AND itab-newbs <> '50'.
*    itab-fistl = itab-kostl.
*    CLEAR itab-kostl.
*  ENDIF.
ENDFORM.                               " CHECK_ITAB
*&---------------------------------------------------------------------*
*&      Form  FILL_BWITH_RECORD
*&---------------------------------------------------------------------*
FORM fill_bwith_record.
  CLEAR : *bwith, tfile.

  CHECK wt_sw NE ' '.

  DO 2 TIMES.
    IF sy-index = 1.
      MOVE 'K1'        TO  *bwith-witht.
    ELSE.
      MOVE 'K2'        TO  *bwith-witht.
    ENDIF.
    MOVE : '2'         TO  *bwith-stype,
           'BWITH'     TO  *bwith-tbnam,
            wt_sw      TO  *bwith-wt_withcd,
           '/'         TO  *bwith-sende.

    PERFORM fill_nodata_to_bwith.
    tfile = *bwith.
    APPEND tfile.
  ENDDO.
  CLEAR wt_sw.
ENDFORM.                               " FILL_BWITH_RECORD

*&---------------------------------------------------------------------*
*&      Form  FILL_NODATA_TO_BWITH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_nodata_to_bwith.
  LOOP AT bwith_tab.
    CLEAR : fname.                     "FN>.
    CONCATENATE '*BWITH-' bwith_tab-fieldname INTO fname.
    ASSIGN (fname) TO <fn>.
    IF <fn> IS INITIAL.
      MOVE '/' TO <fn>.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " FILL_NODATA_TO_BWITH



*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_list_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODE_SET
*&---------------------------------------------------------------------*
FORM mode_set USING    mode.
  IF mode EQ 0.
    FORMAT INTENSIFIED ON.
  ELSE.
    FORMAT INTENSIFIED OFF.
  ENDIF.
ENDFORM.                    " MODE_SET
*-----------------------------------------------------------------------
*    Forms
*-----------------------------------------------------------------------

* Initialization fieldcatalog

FORM fieldcat_init
      USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: pos TYPE i VALUE 1.

* Build layout for list display
  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-no_keyfix         = 'X'.
  gs_layout-get_selinfos      = 'X'.
  gs_layout-f2code            = 'AUSW'.
  gs_layout-box_fieldname     = g_boxname.
*


*
  append_key:
   pos 'LFDNR'     'LFDNR'     'WPTST',
   pos 'END_FLAG'  'XSKRL'     'BBSEG'.

  append_fld:
   pos 'BELNR'     'BELNR'     'BBKPF',
   pos 'BLART'     'BLART'     'BBKPF',
   pos 'BUDAT'     'BUDAT'     'BBKPF',
   pos 'WAERS'     'WAERS'     'BBKPF',
   pos 'KURSF'     'KURSF'     'BBKPF',
   pos 'XBLNR'     'XBLNR'     'BBKPF',
   pos 'BKTXT'     'BKTXT'     'BBKPF',
   pos 'NEWBS'     'NEWBS'     'BBSEG',
   pos 'NEWKO'     'NEWKO'     'BBSEG',
   pos 'NEWUM'     'NEWUM'     'BBSEG',
   pos 'WRBTR'     'WRBTR'     'BBSEG',
   pos 'MWSKZ'     'MWSKZ'     'BBSEG',
   pos 'HWSTE'     'HWSTE'     'BBTAX',
   pos 'KOSTL'     'KOSTL'     'BBSEG',
   pos 'AUFNR'     'AUFNR'     'BBSEG',
   pos 'ZFBDT'     'ZFBDT'     'BBSEG',
   pos 'VALUT'     'VALUT'     'BBSEG',
   pos 'ZTERM'     'ZTERM'     'BBSEG',
   pos 'ZLSCH'     'ZLSCH'     'BBSEG',
   pos 'ZLSPR'     'ZLSPR'     'BBSEG',
   pos 'ZUONR'     'ZUONR'     'BBSEG',
   pos 'SGTXT'     'SGTXT'     'BBSEG'.

ENDFORM.   "fieldcat_init
************************************************************************
FORM eventtab_build
     USING rt_events TYPE slis_t_event.
* Registration of events to happen during list display
  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = rt_events.

*  READ TABLE rt_events WITH KEY name = slis_ev_top_of_page
*                           INTO ls_event.
*  IF sy-subrc = 0.
*    MOVE g_top_of_page TO ls_event-form.
*    APPEND ls_event TO rt_events.
*  ENDIF.

  READ TABLE rt_events WITH KEY name = slis_ev_user_command
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE g_user_command TO ls_event-form.
    APPEND ls_event TO rt_events.
  ENDIF.
  READ TABLE rt_events WITH KEY name =
                           slis_ev_pf_status_set
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE g_status_set TO ls_event-form.
    APPEND ls_event TO rt_events.
  ENDIF.
ENDFORM.



*---------------------------------------------------------------------*
*       FORM rewrite                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM rewrite.
  DATA: ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            text_question = 'Go Back to the list?'
            text_button_1 = 'Yes'(001)
            text_button_2 = 'No'(002)
       IMPORTING
            answer        = ans.
  IF sy-subrc <> 0.
  ENDIF.

  IF ans  = '1'.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
         EXPORTING
              i_callback_program      = g_repid
              is_layout               = gs_layout
              it_fieldcat             = gt_fieldcat[]
              it_events               = gt_events[]
         IMPORTING
              e_exit_caused_by_caller = g_exit_caused_by_caller
              es_exit_caused_by_user  = gs_exit_caused_by_user
         TABLES
              t_outtab                = utab.

  ELSEIF ans = '2'.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_initial_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BBSEG_TAB_FIELDNAME  text
*      <--P_INITIAL_VALUE  text
*----------------------------------------------------------------------*
FORM get_initial_value USING    p_fieldname
                       CHANGING p_initial_value.
  DATA : $datatype TYPE datatype_d,
         $leng TYPE ddleng .

  CLEAR p_initial_value.

  SELECT SINGLE datatype leng INTO ($datatype,$leng)
  FROM dd03l
   WHERE tabname EQ 'BSEG'
     AND fieldname EQ p_fieldname.

  IF sy-subrc EQ 0 AND ( $datatype EQ 'NUMC' OR $datatype EQ 'DATS' ) .
    DO $leng TIMES.
      CONCATENATE '0' p_initial_value INTO p_initial_value.
    ENDDO.
  ENDIF.

  CONDENSE p_initial_value.

ENDFORM.                    " get_initial_value
