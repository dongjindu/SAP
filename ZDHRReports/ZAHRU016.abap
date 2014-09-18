*&----------------------------------------------------------------------
*& Program ID        : ZAHRU016
*& Title             : [HR] - Outbound data for Medgate Data Transfer
*& Created by        : Valerian Utama
*& Created on        : 05/21/2012
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : Extract outbound data for Medgate Data Transfer
*&                     (*.csv File).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 05/21/2012  Valerian  UD1K954799  Initial Program Development
*& 06/11/2012  Valerian  UD1K955004  Improve the program logic
*& 07/18/2012  Valerian  UD1K955233  Fix the Work Phone data retrieval
*&                                   Add RFC Functionalities
*& 11/28/2012  Valerian  UD1K955849  Enclose "Personnel Number" and
*&                                   "Supervisor" with quotes
*&----------------------------------------------------------------------

REPORT  zahru016 MESSAGE-ID zmfi.

CONSTANTS:
  c_dlmtd(1)     TYPE c VALUE ',',
  c_dfile        TYPE string VALUE 'Megdate_',
  c_dpath        TYPE string VALUE 'C:\TEMP\MEDGATE\',
  c_dextn        TYPE string VALUE '.CSV',
  c_unix_pre(30) TYPE c VALUE '/sapmnt/',
  c_unix_suf(30) TYPE c VALUE '/kronos/kronosftp',
  c_golive_date  TYPE datum VALUE '20100312'.

TYPE-POOLS: slis.

FIELD-SYMBOLS: <fs> TYPE any.

DATA: v_file      TYPE string,
      v_ans       TYPE i.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

DATA : BEGIN OF it_file_dlmtd OCCURS 0,
         record(500) TYPE c,
       END OF it_file_dlmtd.

DATA : BEGIN OF it_file OCCURS 0,
*         pernr                 LIKE pernr-pernr,           "UD1K955849
          pernrc(10)            TYPE c,                     "UD1K955849
          nachn                 LIKE tmlfgt-bezei,
          vorna                 LIKE tmlfgt-bezei,
          stras                 LIKE tmlfgt-bezei,
          locat                 LIKE tmlfgt-bezei,
          ort01                 LIKE tmlfgt-bezei,
          state                 LIKE pa0006-state,
          bezei                 LIKE tmlfgt-bezei,
          pstlz                 LIKE pa0006-pstlz,
          homph                 LIKE pa0006-num01,
          worph                 LIKE pa0006-num01,
          celph                 LIKE pa0006-num01,
          email                 LIKE pa0105-usrid_long,
          perid                 LIKE pa0002-perid,
          gesch                 LIKE pa0002-gesch,
          birdt(10)             TYPE c,
          hirdt(10)             TYPE c,
          terdt(10)             TYPE c,
          wstat(2)              TYPE c,
          wstatt                LIKE tmlfgt-bezei,
          famst                 LIKE pa0002-famst,
          famstt                LIKE tmlfgt-bezei,
          racky                 LIKE pa0077-racky,
          rackyt                LIKE tmlfgt-bezei,
          bukrs                 LIKE pa0001-bukrs,
          bukrst                LIKE tmlfgt-bezei,
          kostl                 LIKE pa0001-kostl,
          kostlt                LIKE tmlfgt-bezei,
          orgeh                 LIKE pa0001-orgeh,
          orgeht                LIKE tmlfgt-bezei,
          usrid                 LIKE pa0105-usrid,
          exmpt                 LIKE t5u13-exmpt,
          exmptt                LIKE tmlfgt-bezei,
          stell                 LIKE pa0001-stell,
          stellt                LIKE tmlfgt-bezei,
          schkz                 LIKE pa0007-schkz,
          schkzt                LIKE tmlfgt-bezei,
          persk                 LIKE pa0001-persk,
          perskt                LIKE tmlfgt-bezei,
       END OF it_file.

DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE it_file.
DATA : pernr TYPE pernr-pernr,                              "UD1K955849
       com01 TYPE pa0006-com01,
       num01 TYPE pa0006-num01,
       com02 TYPE pa0006-com02,
       num02 TYPE pa0006-num02,
       com03 TYPE pa0006-com03,
       num03 TYPE pa0006-num03,
       com04 TYPE pa0006-com04,
       num04 TYPE pa0006-num04,
       com05 TYPE pa0006-com05,
       num05 TYPE pa0006-num05,
       com06 TYPE pa0006-com06,
       num06 TYPE pa0006-num06,
       gbdat TYPE pa0002-gbdat,
       werks TYPE pa0001-werks,
       btrtl TYPE pa0001-btrtl,
       persg TYPE pa0001-persg,
       stat2 TYPE pa0000-stat2,
       END OF it_data.

* BEGIN OF UD1K955233
DATA: BEGIN OF it_phone OCCURS 0,
       pernr TYPE pernr-pernr,
       com01 TYPE pa0006-com01,
       num01 TYPE pa0006-num01,
       com02 TYPE pa0006-com02,
       num02 TYPE pa0006-num02,
       com03 TYPE pa0006-com03,
       num03 TYPE pa0006-num03,
       com04 TYPE pa0006-com04,
       num04 TYPE pa0006-num04,
       com05 TYPE pa0006-com05,
       num05 TYPE pa0006-num05,
       com06 TYPE pa0006-com06,
       num06 TYPE pa0006-num06,
      END OF it_phone.
* END OF UD1K955233

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-pernr,
                s_orgeh FOR it_data-orgeh,
                s_werks FOR it_data-werks,
                s_btrtl FOR it_data-btrtl,
                s_persg FOR it_data-persg,
                s_persk FOR it_data-persk,
                s_stat2 FOR it_data-stat2.
SELECTION-SCREEN SKIP.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS: p_pres RADIOBUTTON GROUP serv USER-COMMAND rad
                                        MODIF ID chk DEFAULT 'X',
            p_appl RADIOBUTTON GROUP serv MODIF ID chk.
SELECTION-SCREEN SKIP.
PARAMETERS: p_file(1024) TYPE c LOWER CASE MODIF ID chk
                                VISIBLE LENGTH 45.
* BEGIN OF UD1K955233
SELECTION-SCREEN SKIP.
PARAMETERS: p_eai  RADIOBUTTON GROUP serv MODIF ID chk,
            p_dest TYPE rfcdest MODIF ID chk.
* END OF UD1K955233
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-t03.
PARAMETERS: p_gdat TYPE datum OBLIGATORY DEFAULT c_golive_date.
SELECTION-SCREEN END OF BLOCK blk3.

INITIALIZATION.
  CONCATENATE c_dpath c_dfile sy-datum sy-uzeit c_dextn INTO p_file.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'CHK'.
      IF NOT p_test IS INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
* BEGIN OF UD1K955233
      ELSEIF sy-tcode = 'ZHR_MEDGATE_DATA' AND
           ( screen-name = 'P_APPL' OR screen-name = 'P_EAI'
                                    OR screen-name = 'P_DEST').
*     ELSEIF sy-tcode = 'ZHR_MEDGATE_DATA' AND screen-name = 'P_APPL'.
* END OF UD1K955233
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'P_GDAT'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CHECK sy-ucomm NE 'CHCK'.
  IF p_test IS INITIAL AND
     p_file IS INITIAL.
    MESSAGE e000 WITH 'Please Enter Filename'.
  ENDIF.

* BEGIN OF UD1K955233
  IF sy-tcode = 'ZHR_MEDGATE_DATA' AND
   ( NOT p_appl IS INITIAL OR NOT p_eai IS INITIAL ).
    MESSAGE e000 WITH text-m01 text-m02.
  ENDIF.

*  IF sy-tcode = 'ZHR_MEDGATE_DATA' AND NOT p_appl IS INITIAL.
*    MESSAGE e000 WITH text-m01 text-m02.
*  ENDIF.
* END OF UD1K955233

  v_file = p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  IF NOT p_pres IS INITIAL.
    PERFORM browser CHANGING p_file v_ans.
  ELSE.
    PERFORM display_unix CHANGING p_file v_ans.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  PERFORM output_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
FORM select_data.

  SELECT a~pernr c~nachn c~vorna d~stras d~locat d~ort01 d~state
         d~pstlz c~perid c~gesch c~gbdat a~stat2 c~famst b~kostl
         b~orgeh b~stell e~schkz b~persk b~werks b~btrtl b~persg
         d~com01 d~num01 d~com02 d~num02 d~com03 d~num03 d~com04
         d~num04 d~com05 d~num05 d~com06 d~num06

    INTO CORRESPONDING FIELDS OF TABLE it_data

    FROM pa0000 AS a JOIN pa0001 AS b
                       ON b~pernr = a~pernr

                     JOIN pa0002 AS c
                       ON c~pernr = a~pernr

                     LEFT JOIN pa0006 AS d
                       ON d~pernr = a~pernr
                      AND d~endda = '99991231'
                      AND d~subty = '5'

                     JOIN pa0007 AS e
                       ON e~pernr = a~pernr

   WHERE a~pernr IN s_pernr
     AND a~endda = '99991231'
     AND b~endda = '99991231'
     AND c~endda = '99991231'
     AND e~endda = '99991231'
     AND b~orgeh IN s_orgeh
     AND b~werks IN s_werks
     AND b~btrtl IN s_btrtl
     AND b~persg IN s_persg
     AND b~persk IN s_persk
     AND a~stat2 IN s_stat2.

* BEGIN OF UD1K955233
* Get phone information from Permanent Address
  IF NOT it_data[] IS INITIAL.
    SELECT pernr com01 num01 com02 num02 com03 num03
           com04 num04 com05 num05 com06 num06
      INTO TABLE it_phone
      FROM pa0006
      FOR ALL ENTRIES IN it_data
     WHERE pernr = it_data-pernr
       AND subty = '1'
       AND endda = '99991231'.

    SORT it_phone BY pernr.
  ENDIF.
* END OF UD1K955233
ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec     TYPE i,
        l_comxx   TYPE p0006-com01,
        l_numxx   TYPE p0006-num01,
        l_darxx   TYPE ps0041-dar01,
        l_datxx   TYPE ps0041-dat01,
        l_mofid   TYPE t001p-mofid,
        l_mosid   TYPE t001p-mosid,
        l_zeity   TYPE t503-zeity,
        lw_ps0041 TYPE ps0041,
        l_usrid   TYPE pa0105-pernr.                        "UD1K955849

  DATA: BEGIN OF lt_termination OCCURS 0,
          begda TYPE pa0000-begda,
          massn TYPE pa0000-massn,
        END OF lt_termination.

* Move data to output structure
  DESCRIBE TABLE it_data LINES l_rec.
  LOOP AT it_data.
    PERFORM show_progress2 USING 'Processing Data...' l_rec.

* Get Date of Termination
    IF it_data-stat2 = '0'.
      CLEAR: lt_termination, lt_termination[].
      SELECT begda massn INTO TABLE lt_termination
        FROM pa0000
       WHERE pernr = it_data-pernr.

      SORT lt_termination BY begda DESCENDING.
      LOOP AT lt_termination WHERE massn = 'ZE'
                                OR massn = 'ZX'
                                OR massn = 'ZW'
                                OR massn = 'ZY'
                                OR massn = 'ZI'
                                OR massn = 'Z5'
                                OR massn = 'Z7'
                                OR massn = 'Z8'.

        WRITE lt_termination-begda TO it_data-terdt.
        EXIT.
      ENDLOOP.

* Exclude TM who is terminated before the golive date
      CHECK lt_termination-begda >= p_gdat.
    ENDIF.

* Insert quote to name and address
    PERFORM insert_quote CHANGING: it_data-nachn,
                                   it_data-vorna,
                                   it_data-stras,
                                   it_data-locat,
                                   it_data-ort01.
* Get State Description
    SELECT SINGLE bezei INTO it_data-bezei
      FROM t005u
     WHERE spras = 'E'
       AND land1 = 'US'
       AND bland = it_data-state.
    PERFORM insert_quote CHANGING it_data-bezei.

* Get Phone Numbers
* BEGIN OF UD1K955233
*    DO 6 TIMES VARYING l_comxx FROM it_data-com01
*                               NEXT it_data-com02
*
*               VARYING l_numxx FROM it_data-num01
*                               NEXT it_data-num02.
*      CASE l_comxx.
*        WHEN 'HOME'.
*          it_data-homph = l_numxx.
*        WHEN 'WORK'.
*          it_data-worph = l_numxx.
*        WHEN 'WCEL'.
*          it_data-celph = l_numxx.
*      ENDCASE.
*    ENDDO.

    READ TABLE it_phone WITH KEY pernr = it_data-pernr
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      DO 6 TIMES VARYING l_comxx FROM it_phone-com01
                                 NEXT it_phone-com02

                 VARYING l_numxx FROM it_phone-num01
                                 NEXT it_phone-num02.
        CASE l_comxx.
          WHEN 'HOME'.
            it_data-homph = l_numxx.
          WHEN 'WORK'.
            it_data-worph = l_numxx.
          WHEN 'WCEL'.
            it_data-celph = l_numxx.
        ENDCASE.
      ENDDO.
    ENDIF.
* END OF UD1K955233

* Get e-Mail
    SELECT usrid_long INTO it_data-email
      FROM pa0105
     UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND subty = '0010'
       AND endda = '99991231'.
    ENDSELECT.

* Get Gender
    IF it_data-gesch = '1'.
      it_data-gesch = '0'.
    ELSEIF it_data-gesch = '2'.
      it_data-gesch = '1'.
    ELSE.
      CLEAR it_data-gesch.
    ENDIF.

* Get Date of Birth
    WRITE it_data-gbdat TO it_data-birdt.

* Get Date of Hire
    SELECT * INTO CORRESPONDING FIELDS OF lw_ps0041
      FROM pa0041
     UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND endda = '99991231'.
    ENDSELECT.

    IF sy-subrc = 0.
      DO 12 TIMES VARYING l_darxx FROM lw_ps0041-dar01
                                  NEXT lw_ps0041-dar02

                  VARYING l_datxx FROM lw_ps0041-dat01
                                  NEXT lw_ps0041-dat02.
        CASE l_darxx.
          WHEN 'Z1'.
            IF it_data-hirdt IS INITIAL.
              WRITE l_datxx TO it_data-hirdt.
            ENDIF.
          WHEN 'ZB'.
            WRITE l_datxx TO it_data-hirdt.
            EXIT.
        ENDCASE.
      ENDDO.
    ENDIF.

* Get Work Status Code and Description
    it_data-wstat  = 'FT'.
    it_data-wstatt = 'Fulltime'.
    PERFORM insert_quote CHANGING it_data-wstatt.

* Get Marital Status Description
    SELECT SINGLE ftext INTO it_data-famstt
      FROM t502t
     WHERE sprsl = 'E'
       AND famst = it_data-famst.
    PERFORM insert_quote CHANGING it_data-famstt.

* Get Ethnic Group Code and Description
    SELECT racky INTO it_data-racky
      FROM pa0077
     UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND endda = '99991231'.
    ENDSELECT.

    IF sy-subrc = 0.
      SELECT SINGLE ltext INTO it_data-rackyt
        FROM t505s
       WHERE sprsl = 'E'
         AND molga = '10'
         AND racky = it_data-racky.
      PERFORM insert_quote CHANGING it_data-rackyt.
    ENDIF.

* Get Geographic Location code and Description.
    it_data-bukrs  = 'HMMA'.
    it_data-bukrst = 'HMMA-Hyundai Motor Manufacturing of AL LLC'.
    PERFORM insert_quote CHANGING it_data-bukrst.

* Get Cost Center Description
    SELECT SINGLE ltext INTO it_data-kostlt
      FROM cskt
     WHERE spras = 'E'
       AND kokrs = 'H201'
       AND kostl = it_data-kostl
       AND datbi = '99991231'.
    PERFORM insert_quote CHANGING it_data-kostlt.

* Get Organization Description
    SELECT stext INTO it_data-orgeht
      FROM hrp1000
     UP TO 1 ROWS
     WHERE plvar = '01'
       AND otype = 'O'
       AND objid = it_data-orgeh
       AND endda = '99991231'
       AND langu = 'E'.
    ENDSELECT.
    PERFORM insert_quote CHANGING it_data-orgeht.

* Get Supervisor
    SELECT usrid INTO it_data-usrid                         "UD1K955849
       FROM pa0105
      UP TO 1 ROWS
      WHERE pernr = it_data-pernr
        AND subty = '0013'
        AND endda = '99991231'.
    ENDSELECT.
    l_usrid = it_data-usrid.                                "UD1K955849
    it_data-usrid = l_usrid.                                "UD1K955849
    PERFORM insert_quote CHANGING it_data-usrid.            "UD1K955849

* Get Job Type and Description
    SELECT SINGLE exmpt INTO it_data-exmpt
      FROM t5u13
     WHERE stell = it_data-stell
       AND endda = '99991231'.

    CASE it_data-exmpt.
      WHEN '1'.
        it_data-exmptt = 'Exempt'.
      WHEN '2'.
        it_data-exmptt = 'Non-exempt'.
    ENDCASE.
    PERFORM insert_quote CHANGING it_data-exmptt.

* Get Job Position Description
    SELECT stext INTO it_data-stellt
      FROM hrp1000
     UP TO 1 ROWS
     WHERE plvar = '01'
       AND otype = 'C'
       AND objid = it_data-stell
       AND endda = '99991231'
       AND langu = 'E'.
    ENDSELECT.
    PERFORM insert_quote CHANGING it_data-stellt.

* Get Shift Code Description
    SELECT SINGLE mofid mosid INTO (l_mofid, l_mosid)
      FROM t001p
     WHERE werks = it_data-werks
       AND btrtl = it_data-btrtl.
    IF sy-subrc = 0.
      SELECT SINGLE zeity INTO l_zeity
        FROM t503
       WHERE persg = it_data-persg
         AND persk = it_data-persk.
    ENDIF.

    IF sy-subrc = 0.
      SELECT SINGLE rtext INTO it_data-schkzt
        FROM t508s
       WHERE zeity = l_zeity
         AND mofid = l_mofid
         AND mosid = l_mosid
         AND schkz = it_data-schkz
         AND sprsl = 'E'.
    ENDIF.
    PERFORM insert_quote CHANGING it_data-schkzt.

* Get Employee Type Description
    SELECT SINGLE ptext INTO it_data-perskt
      FROM t503t
     WHERE sprsl = 'E'
       AND persk = it_data-persk.
    PERFORM insert_quote CHANGING it_data-perskt.

* Move relevant data
    MOVE-CORRESPONDING it_data TO it_file.

    it_file-pernrc = it_data-pernr.                         "UD1K955849
    PERFORM insert_quote CHANGING it_file-pernrc.           "UD1K955849

    APPEND it_file. CLEAR it_file.
  ENDLOOP.

  FREE it_data.

  DESCRIBE TABLE it_file LINES l_rec.
  MESSAGE s000 WITH 'Total record(s) extracted:' l_rec.

ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES it_file.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       Transfer Data to Desktop/PC or UNIX/Presentation Server
*----------------------------------------------------------------------*
FORM transfer_data.
  DATA: l_totrec TYPE i,
        l_filter(50) TYPE c VALUE 'des -e -k abc123forme',
        l_return TYPE zmms0053.                             "UD1K955233

* BEGIN OF UD1K955233
  IF NOT p_eai IS INITIAL.
    DESCRIBE TABLE it_file LINES l_totrec.
    CALL FUNCTION 'ZFHR_MEDGATE'
      DESTINATION p_dest
      IMPORTING
        e_return                       = l_return
      TABLES
        t_data                         = it_file
      EXCEPTIONS
        call_function_destination_no_t = 1
        call_function_no_dest          = 2
        call_function_remote_error     = 3
        rfc_no_authority               = 4
        OTHERS                         = 5.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error when calling RFC destination:'
                         p_dest.

    ELSEIF l_return-type <> 'S'.
      MESSAGE e000
         WITH l_return-message(50)     l_return-message+50(50)
              l_return-message+100(50) l_return-message+150(50).
    ELSE.
      MESSAGE s000 WITH 'Data has been sent to:' p_dest
                         l_totrec 'Record(s)'.
    ENDIF.

  ELSE.
* END OF UD1K955233
    LOOP AT it_file.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE it_file TO <fs>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF NOT it_file_dlmtd IS INITIAL.
          CONCATENATE it_file_dlmtd-record c_dlmtd <fs>
                 INTO it_file_dlmtd-record.
        ELSE.
          it_file_dlmtd-record = <fs>.
        ENDIF.
      ENDDO.

      APPEND it_file_dlmtd. CLEAR it_file_dlmtd.
    ENDLOOP.

    DESCRIBE TABLE it_file_dlmtd LINES l_totrec.

    CASE 'X'.
      WHEN p_appl.
*       BEGIN OF VALTEST
        OPEN DATASET v_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
             FILTER l_filter.
*       OPEN DATASET v_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*       END OF VALTEST
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        LOOP AT it_file_dlmtd.
          TRANSFER it_file_dlmtd TO v_file.
        ENDLOOP.

        CLOSE DATASET v_file.

      WHEN p_pres.
        IF NOT sy-batch IS INITIAL.
          MESSAGE s000
             WITH 'Writing File to Desktop Not Possible'
                  'in Background Mode' ' ' ' '.
          EXIT.
        ENDIF.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = v_file
          TABLES
            data_tab                = it_file_dlmtd
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.

        IF sy-subrc <> 0.
          IF sy-subrc = 15.
            MESSAGE s011 WITH 'Access Denied'.
          ELSEIF NOT sy-msgid IS INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            MESSAGE s011 WITH 'Error when creating the file'.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF sy-subrc EQ 0.
      MESSAGE s000 WITH 'File is written to:' v_file l_totrec
                        'Record(s)'.
    ENDIF.
  ENDIF.                                                    "UD1K955233
ENDFORM.                    " TRANSFER_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'IT_FILE'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = ft_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set Key field.
  LOOP AT ft_fieldcat INTO gs_fieldcat
                     WHERE fieldname = 'PERNR'
                        OR fieldname = 'NACHN'
                        OR fieldname = 'VORNA'.
    gs_fieldcat-key = 'X'.
    MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING key.
  ENDLOOP.

* Change Description
  PERFORM change_desc USING 'NACHN'  'Last Name'.
  PERFORM change_desc USING 'VORNA'  'First Name'.
  PERFORM change_desc USING 'STRAS'  'Home Addr1'.
  PERFORM change_desc USING 'LOCAT'  'Home Addr2'.
  PERFORM change_desc USING 'ORT01'  'City'.
  PERFORM change_desc USING 'HOMPH'  'Home Phone'.
  PERFORM change_desc USING 'WORPH'  'Work Phone'.
  PERFORM change_desc USING 'CELPH'  'Work Cell'.
  PERFORM change_desc USING 'EMAIL'  'Work e-Mail'.
  PERFORM change_desc USING 'BIRDT'  'Date of Birth'.
  PERFORM change_desc USING 'HIRDT'  'Date of Hire'.
  PERFORM change_desc USING 'TERDT'  'Date of Termination'.
  PERFORM change_desc USING 'WSTAT'  'Work Status'.
  PERFORM change_desc USING 'PERNRC' 'PersNo.'.             "UD1K955849

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab TYPE table.

  DATA: l_repid TYPE sy-repid.
  DATA : gs_layout TYPE slis_layout_alv.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = ft_outtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       Browse Desktop/PC Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM browser CHANGING filename TYPE c answer TYPE i.
  DATA  $filename TYPE string.
  DATA: l_path  TYPE string,
        l_fpath TYPE string,
        l_dfile TYPE string.

  CONCATENATE c_dfile sy-datum sy-uzeit c_dextn INTO l_dfile.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
      default_extension = 'csv'
      default_file_name = l_dfile
      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory = c_dpath
    CHANGING
      filename          = $filename
      path              = l_path
      fullpath          = l_fpath
      user_action       = answer
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF answer = 0.
    filename = l_fpath.
  ELSE.
    MESSAGE s118(ba).
  ENDIF.
ENDFORM.                    " BROWSER

*&---------------------------------------------------------------------*
*&      Form  output_data
*&---------------------------------------------------------------------*
*       Display data or write data to file
*----------------------------------------------------------------------*
FORM output_data.
  IF it_file[] IS INITIAL.
    MESSAGE s020.
    STOP.
  ENDIF.

  IF p_test IS INITIAL.
    PERFORM transfer_data.
  ELSE.
    PERFORM display_data.
  ENDIF.
ENDFORM.                    " output_data

*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM display_unix CHANGING filename TYPE c answer TYPE i.
  DATA: BEGIN OF it_filename OCCURS 0,
          path(1024) TYPE c,
        END OF it_filename.

  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  APPEND it_filename.

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
                     ID 'VALUE' FIELD it_filename-path.
  APPEND it_filename.

  CALL FUNCTION 'POPUP_WITH_TABLE'
    EXPORTING
      endpos_col   = '100'
      endpos_row   = '10'
      startpos_col = '1'
      startpos_row = '1'
      titletext    = 'Select UNIX Directory'
    IMPORTING
      choice       = filename
    TABLES
      valuetab     = it_filename
    EXCEPTIONS
      break_off    = 1
      OTHERS       = 2.

  answer = sy-subrc.

  IF sy-subrc = 0.
    CONCATENATE filename '/' c_dfile sy-datum sy-uzeit c_dextn
           INTO filename.
  ELSE.
    MESSAGE s549(fibl).
  ENDIF.
ENDFORM.                    " display_unix
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  DATA: gs_fieldcat TYPE slis_fieldcat_alv.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l    = p_desc.
    gs_fieldcat-seltext_m    = p_desc.
    gs_fieldcat-seltext_s    = p_desc.
    gs_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*       Show progress indicator
*----------------------------------------------------------------------*
*      -->PF_TEXT  Display Text
*      -->PF_VAL   Percentage calculated base
*----------------------------------------------------------------------*
FORM show_progress2 USING   pf_text TYPE c
                            pf_val  TYPE i.

  DATA:    l_prctx(3) TYPE c,
           l_tex1(50) TYPE c.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval - 1.
  ENDIF.

  IF NOT pf_val IS INITIAL.
    IF NOT l_percent IS INITIAL.
      l_prctx = l_percent.
    ELSE.
      l_prctx = 1.
      l_percent = 1.
    ENDIF.

    CONCATENATE pf_text l_prctx '%' INTO l_tex1 SEPARATED BY space.
  ELSE.
    l_tex1 = pf_text.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percent
      text       = l_tex1.

  IF l_percent = 1.
    CLEAR l_percent.
  ENDIF.
ENDFORM.                    " SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*&      Form  INSERT_QUOTE
*&---------------------------------------------------------------------*
*       Insert quote to string
*----------------------------------------------------------------------*
*      <--P_SFIELD  String Field
*----------------------------------------------------------------------*
FORM insert_quote  CHANGING p_sfield TYPE c.
  CHECK NOT p_sfield IS INITIAL.
  CONCATENATE '"' p_sfield '"' INTO p_sfield.
ENDFORM.                    " INSERT_QUOTE
