*----------------------------------------------------------------------
* Program ID        : ZAFIU141
* Title             : [FI] Outstanding check balance in Wachovia bank
*                     format (copy from program RFCHKN10 and modified).
*                     Use logic in prog.ZAFIU127 to format the check
*                     file to be downloaded.
* Created on        : 03/30/2010
* Created by        : Valerian Utama
* Specifications By : Calvin Kong
* Description       : Check Extract Creation
*----------------------------------------------------------------------
***********************************************************************
*                                                                     *
*        Schecknachweisreport mit ALV-Benutzung                       *
*        Checkregister with ALV                                       *
* CHANGE : 10/03/2011 BY KDM : Standard Program chnage : Index KDM    *
***********************************************************************

REPORT rfchkn10
  LINE-COUNT (1)
  LINE-SIZE 132
  NO STANDARD PAGE HEADING
  MESSAGE-ID fibl.

*---------------------------------------------------------------------*
* Declarations                                                        *
*---------------------------------------------------------------------*
*** : Standard Program Change(KDM)
TYPE-POOLS ABAP.
DATA g_acc_mode TYPE abap_bool.               "Accessibility mode

*---------------------------------------------------------------------*
* Declarations                                                        *
*---------------------------------------------------------------------*
* DECO
INCLUDE rfchki02.

* Defines
INCLUDE rfchki99.

* datadeclaration and selectionscreen
INCLUDE rfchki15.

* Begin of HIS20094
SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE text-t01.
PARAMETERS: p_dwld AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK file.
* End of HIS20094

* Subroutines that fill ALV_HEADER and ALV_ITEM
INCLUDE rfchki18.

* Listviewer subroutines (Fieldcat, Layout, Events, Sort)
INCLUDE rfchki17.

* Event handler (callback subroutines)
INCLUDE rfchki19.

* At Selection Screen
INCLUDE rfchki20.

*---------------------------------------------------------------------*
* Schecks selektieren                                                 *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA:
    l_repid TYPE repid,
    l_subrc LIKE sy-subrc.
  IF NOT par_xper EQ space.
    l_repid = sy-repid.                "Reportberechtigung im HR
    CALL FUNCTION 'HR_PROGRAM_CHECK_AUTHORIZATION'
         EXPORTING
              repid = l_repid
         IMPORTING
              subrc = l_subrc.
    IF l_subrc NE 0.
      MESSAGE e637(fibl) WITH sy-repid.
    ENDIF.
  ENDIF.


* Include: selection is prepared
  INCLUDE rfchki16.

* Buchungskreis nachlesen und
* besondere Behandlung der Schecknummer-Parameter/Betrags-Select-Option
* get company code from database and doe some checks on the input
  INCLUDE rfchki10.

  SELECT SINGLE * FROM t005 WHERE land1 EQ t001-land1.

  IF par_xaus NE space.                "nur nicht eingelöste Schecks
    CONCATENATE 'XBANC = ' '''' '''' INTO g_lcond SEPARATED BY space.
    APPEND g_lcond TO g_cond.
    CONCATENATE 'AND VOIDR = ' '''00''' INTO
      g_lcond SEPARATED BY space.
    APPEND g_lcond TO g_cond.
  ENDIF.

* Selektion der Daten
* dataselection
  SELECT * FROM payr INTO CORRESPONDING FIELDS OF TABLE alv_header
    WHERE   ichec EQ space
      AND   zbukr IN sel_zbuk
      AND   hbkid IN sel_hbki
      AND   hktid IN sel_hkti
      AND   checf LE par_chkt
      AND   chect GE par_chkf
      AND   (g_cond)
      AND   laufd IN sel_laud
      AND   laufi IN sel_laui
      AND   vblnr IN sel_vbln
      AND   zaldt IN sel_zald
      AND   waers IN sel_waer
      AND   rwbtr IN sel_rwbt
      AND   bancd IN sel_bncd
      AND   extrd IN sel_xdat
      AND   extrt IN sel_xtim
      AND ( pridt IN sel_cpud
         OR voidd IN sel_cpud )
      and ( prius IN sel_user
         OR voidu IN sel_user )
      AND   voidr IN sel_void
      AND   voidr IN sel_echt
      AND   rzawe IN sel_zawe
      AND   uzawe IN sel_uzaw
      AND   pernr IN sel_pern                       .     "#EC PORTABLE
  IF sy-subrc NE 0.
    MESSAGE s509.
    EXIT.
  ENDIF.

  LOOP AT alv_header.
    IF NOT alv_header-laufi CP '+++++P'.
      AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'"Anzeigeberechtigung im FI
        ID 'BUKRS' FIELD alv_header-zbukr
        ID 'ACTVT' FIELD '03'.
      IF sy-subrc NE 0.
        MESSAGE i515(fibl) WITH alv_header-zbukr.
        DELETE alv_header WHERE zbukr = alv_header-zbukr.
        CONTINUE.
      ENDIF.
    ENDIF.
* feinere Berechtigunsprüfung für HR
    IF alv_header-laufi+5(1) = 'P' AND alv_header-pernr NE 0.
      CALL FUNCTION 'HR_CHECK_AUTHORITY_PERNR'
           EXPORTING
                pernr  = alv_header-pernr
                begda  = alv_header-laufd
                endda  = alv_header-laufd
           EXCEPTIONS
                OTHERS = 4.
      IF sy-subrc <> 0.
        flg_pernr_err = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF alv_header-xmanu NE space.
      alv_header-rzawe = space.
    ENDIF.
*   Fuellen der ALV-Tabellen mit Daten
*   fill ALV tables
    PERFORM write_zahlweg.
    PERFORM write_bank.
    PERFORM write_zahlung.
    alv_header-expa = 'X'.             " Einzelposten sichtbar
    MODIFY alv_header.
  ENDLOOP.

  IF flg_pernr_err = 'X'.
    IF sy-batch = space.
      MESSAGE i675(fs).
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Begin of HIS20094
  DATA: par_file TYPE string.

  IF NOT p_dwld IS INITIAL.
    PERFORM browser CHANGING par_file.
    PERFORM create_bank_if_file USING par_file.
  ENDIF.
* End of HIS20094

* Forms zur List-Viewer Gestaltung
* set layout of list
  PERFORM set_layout CHANGING alv_layout.  "ALV allg. Aussehen der Liste
  PERFORM set_field  CHANGING alv_fieldcat. "ALV Feldkatalog
  PERFORM set_sort   CHANGING alv_sort."Sort. und Summ.-reihenfolge
  PERFORM set_event  CHANGING alv_events.   "Ereignisse von ALV
  PERFORM set_print  CHANGING alv_print.

* - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - *
*                            ALV Aufrufe                               *
*    Hier.seq. Liste wegen HR Entkopplung in FB ausgelagert            *
* - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - *

* Falls Einzelposten gewuenscht, bilde hier. seq. Liste
* when payed items are selected, call hier. seq. list
* thsi call is put in an function for decoupling reasons
  IF par_epos = 'X'.
    CHECK g_application_system_type = 'T'.
    CALL FUNCTION 'FIBL_CHECK_RFCHKN10_DISPLAY_FI'
         EXPORTING
              is_layout       = alv_layout
              it_fieldcat     = alv_fieldcat
              i_flg_summ      = par_summ
              it_excluding    = alv_excluding
              it_sort         = alv_sort
              i_save          = alv_save
              is_variant      = alv_variant_hs  "?
              it_events       = alv_events
              is_keyinfo      = alv_key
              is_print        = alv_print
              i_caller_report = ge_repid
              i_caller_form   = con_form_user_command
         TABLES
              t_outtab_header = alv_header.
  ELSE.

* Keine Einzelposten ---> einstufige Liste
* no items ---> one level list
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
            i_callback_program       =  ge_repid
            i_callback_user_command  =  con_form_user_command
*           i_structure_name         =  'PAYR_ALV1'
            is_layout                =  alv_layout
            it_fieldcat              =  alv_fieldcat
            it_sort                  =  alv_sort
            i_save                   =  alv_save
            is_variant               =  alv_variant
            it_events                =  alv_events
            is_print                 =  alv_print
         TABLES
            t_outtab                 =  alv_header.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PAR_FILE  text
*----------------------------------------------------------------------*
FORM browser CHANGING filename.

  DATA: it_tfile TYPE filetable ,
        gd_subrc TYPE i.

  DATA  $filename TYPE string.

  DATA: l_filename TYPE string,
        l_path     TYPE string,
        l_action   TYPE i.

  $filename = filename.

* Create default File Name
  CONCATENATE 'CHK-' sy-datum+2(6) '-' sy-uzeit INTO $filename.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
*     default_extension = '*.txt'
      default_file_name = $filename
      file_filter       = '*.*'
      initial_directory = 'c:\temp\'
    CHANGING
      filename          = l_filename
      path              = l_path
      fullpath          = filename
      user_action       = l_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  FILTER_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$RWBTR  text
*----------------------------------------------------------------------*
FORM filter_number CHANGING p_number.

  DATA: output(10) TYPE c,
        f2(10) TYPE c,
        f3 TYPE c.
  DATA: len TYPE i,
        i TYPE i.

  len = strlen( p_number ).

  DO len TIMES.
    f3 = p_number+i(1).
    IF f3 CA '0123456789.'.
      CONCATENATE f2 f3 INTO f2.
      len = len - 1.
    ENDIF.
    i = i + 1.
  ENDDO.
  CONDENSE f2.

  p_number = f2.

ENDFORM.                    " FILTER_NUMBER

*&---------------------------------------------------------------------*
*&      Form  create_bank_if_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bank_if_file USING filename.
  DATA: lc_chect(10) TYPE n,
        lc_rwbtr(10) TYPE n,
        lc_laufd(8).

  DATA: l_ind_c(1)  TYPE c,
        l_rwbtr(10) TYPE n.

  DATA: $filename TYPE string.

  DATA: BEGIN OF it_datatab OCCURS 0,
          col1(100) TYPE c,
        END OF it_datatab.

  CHECK NOT filename IS INITIAL.

  $filename = filename.

  DATA : $i TYPE i,
         $ii(10),
         $c,
         $rwbtr(30),
         $znme1(40).

  LOOP AT alv_header.
    IF alv_header-hbkid EQ 'WA1'.
      IF NOT alv_header-voidr IS INITIAL.
        $c = 'C'.
      ELSE.
        $c = 'I'.
      ENDIF.
      WRITE alv_header-rwbtr TO $rwbtr CURRENCY alv_header-waers.

      PERFORM filter_number CHANGING $rwbtr.
      $znme1 = alv_header-znme1.

      WRITE sy-tabix TO $ii.
      CONDENSE $ii.
      DO 10 TIMES.
        REPLACE ',' WITH '' INTO $znme1.
      ENDDO.

      PERFORM get_output USING alv_header-chect
                               alv_header-rwbtr
                               alv_header-laufd
                      CHANGING lc_chect
                               lc_rwbtr
                               lc_laufd.
      CONCATENATE
          $ii ','
          'WBCS' alv_header-bankn ','
          lc_chect ','
          $rwbtr ','
          lc_laufd(2) '/'  lc_laufd+2(2) '/' lc_laufd+4
          ','
          $c ','
          $znme1 ','
          '                               '
          INTO it_datatab-col1.
    ELSE.

      IF alv_header-voidr EQ space.
        l_ind_c = 'I'.
      ELSE.
        l_ind_c = 'V'.
      ENDIF.

      PERFORM get_output USING alv_header-chect
                               alv_header-rwbtr
                               alv_header-laufd
                      CHANGING lc_chect
                               lc_rwbtr
                               lc_laufd.

      CONCATENATE
          l_ind_c          ','
          alv_header-bankn ','
          lc_chect ','
          lc_chect ','
          lc_rwbtr          ','
          lc_laufd ','
          alv_header-znme1 INTO it_datatab-col1.
    ENDIF.
    APPEND it_datatab.
    CLEAR it_datatab.
  ENDLOOP.

  READ TABLE it_datatab INDEX 1.
  IF sy-subrc NE 0.
    LOOP AT alv_header.
      IF alv_header-hbkid EQ 'WA1'.
        IF NOT alv_header-voidr IS INITIAL.
          $c = 'C'.
        ELSE.
          $c = 'I'.
        ENDIF.

        PERFORM get_output USING alv_header-chect
                                 alv_header-rwbtr
                                 alv_header-laufd
                        CHANGING lc_chect
                                 lc_rwbtr
                                 lc_laufd.

        WRITE sy-tabix TO $ii.
        CONDENSE $ii.
        CONCATENATE
            $ii ','
            'WBCS' alv_header-bankn ','
            lc_chect ','
            lc_rwbtr ','
            lc_laufd ','
            $c ','
            alv_header-znme1 ','
            '                               '
            INTO it_datatab-col1.
      ELSE.

        IF alv_header-voidr EQ space.
          l_ind_c = 'I'.
        ELSE.
          l_ind_c = 'V'.
        ENDIF.

        PERFORM get_output USING alv_header-chect
                                 alv_header-rwbtr
                                 alv_header-laufd
                        CHANGING lc_chect
                                 lc_rwbtr
                                 lc_laufd.

        CONCATENATE
            l_ind_c ','
            alv_header-bankn ','
            lc_chect ','
            lc_chect ','
            lc_rwbtr ','
            lc_laufd ','
            alv_header-laufd ','
            alv_header-znme1 INTO it_datatab-col1.
      ENDIF.
      APPEND it_datatab.
      CLEAR it_datatab.
    ENDLOOP.
  ENDIF.

  IF alv_header-hbkid EQ 'WA1'.
    CLEAR it_datatab-col1.
    DESCRIBE TABLE it_datatab LINES $i.
    WRITE $i TO it_datatab-col1.
    CONCATENATE it_datatab-col1 ',' INTO it_datatab-col1.
    CONDENSE it_datatab-col1.
    IF $i > 0.
      INSERT it_datatab INDEX 1.
    ENDIF.
  ENDIF.


  CALL FUNCTION 'GUI_DOWNLOAD'
   EXPORTING
        filename         = $filename
        filetype         = 'ASC'
*       APPEND           = 'X'
        write_field_separator = 'X'
*       CONFIRM_OVERWRITE = 'X'
   TABLES
        data_tab         = it_datatab[]
   EXCEPTIONS
        file_open_error  = 1
        file_write_error = 2
        OTHERS           = 3.

ENDFORM.                    " create_bank_if_file

*&---------------------------------------------------------------------*
*&      Form  get_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV_HEADER_CHECT  text
*      -->P_ALV_HEADER_RWBTR  text
*      -->P_ALV_HEADER_LAUFD  text
*      <--P_LC_CHECT  text
*      <--P_LC_RWBTR  text
*      <--P_LC_LAUFD  text
*----------------------------------------------------------------------*
FORM get_output USING    p_alv_header_chect TYPE chect
                         p_alv_header_rwbtr TYPE rwbtr
                         p_alv_header_laufd TYPE laufd
                CHANGING p_lc_chect TYPE n
                         p_lc_rwbtr TYPE n
                         p_lc_laufd TYPE c.

  DATA : $chect(15),
         $rwbtr(15),
         $laufd(8),
         l_rwbtr TYPE rwbtr.

  l_rwbtr = - p_alv_header_rwbtr.
  $chect = p_alv_header_chect.
  $rwbtr = l_rwbtr.

  PERFORM check_pure_num CHANGING :
      $chect,
      $rwbtr.

  p_lc_chect = $chect .
  p_lc_rwbtr = $rwbtr .

  $laufd = p_alv_header_laufd.

  PERFORM change_date_format CHANGING $laufd .

  p_lc_laufd = $laufd.
ENDFORM.                    " get_output

*---------------------------------------------------------------------*
*       FORM check_pure_num                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  N_VALUE                                                       *
*---------------------------------------------------------------------*
FORM check_pure_num CHANGING n_value.
  DATA num(12) VALUE ' 0123456789'.
  DATA $char(1).
  DATA $strlen TYPE i.
  DATA $strlen2 TYPE i.
  DATA $offset TYPE i.

  CONDENSE n_value NO-GAPS.
  $strlen = strlen( n_value ).

  DO $strlen TIMES.
    $offset = sy-index - 1.
    $strlen2 =  strlen( n_value ).
    IF $strlen2 <= $offset.
      EXIT.
    ENDIF.
    $char = n_value+$offset(1).
    IF $char CN num.
      REPLACE $char WITH '' INTO n_value.
    ENDIF.
  ENDDO.

  CONDENSE n_value NO-GAPS.

ENDFORM.                    " CHECK_NUM

*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATE_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$LAUFD  text
*      <--P_GT_OUT_$LAUFD  text
*      <--P_=  text
*      <--P_$LAUFD  text
*----------------------------------------------------------------------*
FORM change_date_format CHANGING p_$laufd.

  SHIFT p_$laufd BY 4 PLACES CIRCULAR.

ENDFORM.                    " CHANGE_DATE_FORMAT
