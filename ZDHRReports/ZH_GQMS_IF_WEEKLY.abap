REPORT zh_gqms_if_weekly MESSAGE-ID zmco.

*-----------------------------------------------------------------------
* Name: ZH_GQMS_IF_1 - HR GQMS weekly Interface
* Tech. Resource: Imtiaz Ahmad
* Desc: HR GQMS weekly interface
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZH_GQMS_IF_1
*  Author         : ig.moon
*  Creation Data  : 12/03/2007
*  Requirements by: Imtiaz Ahmad
*  Description    : HR GQMS weekly interface.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
* 2011.09.15   yn.kim         UP1K920005            ECC6.0 Upgrade.

***********************************************************************

*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES: p0001,             "Infotype 0001 - org. data
        zshrgqms1 ,        "Structure for GQMS Interface
        *rfcdes.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
PARAMETERS : par_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-101.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  par_r1 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(29) text-003 FOR FIELD par_r1.
PARAMETERS:
  par_file(50).

PARAMETERS p_call AS CHECKBOX.

SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
PARAMETERS:
  par_r2 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(50) text-005 FOR FIELD par_r2,
  END OF LINE.
PARAMETERS : par_dest LIKE rfcdes-rfcdest
                       DEFAULT 'WMHR01'.
SELECTION-SCREEN END   OF BLOCK 1.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

****************************** Global Data *****************************

DATA : BEGIN OF itab OCCURS 0,
        pernr LIKE p0001-pernr,
        plans LIKE p0001-plans,
        ename LIKE p0001-ename,
        sname LIKE p0001-sname,
        aedtm LIKE p0000-aedtm,
       END OF itab.

DATA : BEGIN OF i_dept OCCURS 0,
        objid LIKE hrp1001-objid,
        stext LIKE hrp1000-stext,
       END OF i_dept.

DATA i_zshrgqms1 TYPE TABLE OF zshrgqms1 WITH HEADER LINE.
DATA ihrp1000 LIKE hrp1000 OCCURS 0 WITH HEADER LINE.

DATA ihrp1000_o LIKE hrp1000 OCCURS 0 WITH HEADER LINE.

DATA it500p LIKE t500p  OCCURS 0 WITH HEADER LINE.
DATA it001p LIKE t001p  OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF i_mc_stext OCCURS 0,
        mc_stext LIKE hrp1000-mc_stext,
        objid LIKE hrp1000-objid,
       END OF i_mc_stext.

INITIALIZATION.

  par_file    = 'c:\temp\H_GQ_W'.
  par_file+16  = '-'.
  WRITE sy-datlo TO par_file+17(6) YYMMDD.
  par_file+24 = '-'.
  par_file+25 = sy-timlo.
  CONCATENATE par_file '.txt' INTO par_file.
  CONDENSE par_file NO-GAPS.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
  __cls : itab, i_zshrgqms1.
  DATA par_date_w LIKE par_date.
  par_date_w = par_date - 7.

*  select a~pernr a~plans a~ename a~sname a~aedtm
*        from pa0001 as a
*        inner join pa0000 as b
*        on b~pernr eq a~pernr
*      into corresponding fields of table itab
*    where a~pernr in s_pernr
*      and a~endda >= par_date
*      and a~begda <= par_date
*      and b~stat2 eq '3'
*      and b~begda <= par_date
*      and b~endda >= par_date.

  __cls : ihrp1000, ihrp1000_o.

  SELECT * INTO TABLE ihrp1000
  FROM hrp1000 WHERE plvar EQ '01'
                AND  ( otype EQ 'C' OR otype EQ 'O' )
                AND  istat EQ '1'
                AND  begda <= par_date
                AND  endda >= par_date
                AND  langu EQ sy-langu .

  LOOP AT ihrp1000.
    IF ihrp1000-otype EQ 'O'.
      ihrp1000_o = ihrp1000.
      APPEND ihrp1000_o.
    ENDIF.
  ENDLOOP.

  LOOP AT ihrp1000.

    IF ihrp1000-otype EQ 'O'.
*      i_zshrgqms1-qz31cody = 'G002'.
      CONTINUE.

    ELSE.
      i_zshrgqms1-qz31cody = 'G003'.
    ENDIF.

    i_zshrgqms1-qz31code = ihrp1000-objid.
    i_zshrgqms1-qz31hkmc = 'A'.
    i_zshrgqms1-qz31desk = ihrp1000-stext.
    i_zshrgqms1-qz31dese = ihrp1000-stext.
    i_zshrgqms1-qz31desc = ihrp1000-stext.

    i_zshrgqms1-qz31con1 = space. "ihrp1000-short.
    i_zshrgqms1-qz31con2 = space.
    i_zshrgqms1-qz31con3 = space.

    i_zshrgqms1-qz31uflg = space.

    IF ihrp1000-aedtm >= par_date_w AND
       ihrp1000-aedtm <= par_date.
      i_zshrgqms1-qz31dflg = 'U'.
    ELSE.
      i_zshrgqms1-qz31dflg = space.
    ENDIF.

    i_zshrgqms1-qz31yrdt = par_date.
    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
    i_zshrgqms1-qz31rpdt = par_date.
    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
    APPEND i_zshrgqms1.
    CLEAR i_zshrgqms1.
  ENDLOOP.


* 6/8/2009 by ig.moon {

  DATA $mc_stext LIKE hrp1000-mc_stext.
  __cls i_mc_stext.

  LOOP AT ihrp1000_o.
    IF ihrp1000_o-mc_stext CP '*COORDINATOR'.
      i_mc_stext-mc_stext = ihrp1000_o-mc_stext.
      i_mc_stext-objid = ihrp1000_o-objid.
      APPEND i_mc_stext.
    ENDIF.
  ENDLOOP.

  IF p_call EQ true.
    EXPORT i_mc_stext TO MEMORY ID 'ZHGQMSWEEKLYMC'.
  ENDIF.

  LOOP AT i_mc_stext.
    REPLACE 'COORDINATOR' WITH '' INTO i_mc_stext-mc_stext.
    MODIFY i_mc_stext INDEX sy-tabix.
  ENDLOOP.

  SORT i_mc_stext.

  LOOP AT ihrp1000_o.

    CHECK ihrp1000_o-mc_stext CP '*DEPARTMENT'.
    $mc_stext = ihrp1000_o-mc_stext.
    REPLACE 'DEPARTMENT' WITH '' INTO $mc_stext.

*    READ TABLE i_mc_stext WITH KEY mc_stext = $mc_stext BINARY SEARCH.
*    CHECK sy-subrc EQ 0.

    i_zshrgqms1-qz31cody = 'G002'.
    i_zshrgqms1-qz31code = ihrp1000_o-objid.
    i_zshrgqms1-qz31hkmc = 'A'.
    i_zshrgqms1-qz31desk = ihrp1000_o-stext.
    i_zshrgqms1-qz31dese = ihrp1000_o-stext.
    i_zshrgqms1-qz31desc = ihrp1000_o-stext.
    i_zshrgqms1-qz31con1 = space. "ihrp1000_o-short.
    i_zshrgqms1-qz31con2 = space.
    i_zshrgqms1-qz31con3 = space.
    i_zshrgqms1-qz31uflg = space.

    IF ihrp1000_o-aedtm >= par_date_w AND
       ihrp1000_o-aedtm <= par_date.
      i_zshrgqms1-qz31dflg = 'U'.
    ELSE.
      i_zshrgqms1-qz31dflg = space.
    ENDIF.

    i_zshrgqms1-qz31yrdt = par_date.
    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
    i_zshrgqms1-qz31rpdt = par_date.
    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
    APPEND i_zshrgqms1.
    CLEAR i_zshrgqms1.

  ENDLOOP.

* }

  __cls it001p.

  SELECT * INTO TABLE it001p
  FROM t001p WHERE molga EQ '10' AND werks EQ '1010'.


  LOOP AT it001p.

    i_zshrgqms1-qz31cody = 'G001'.
    i_zshrgqms1-qz31code = it001p-btrtl.
    i_zshrgqms1-qz31hkmc = 'A'.
    i_zshrgqms1-qz31desk = it001p-btext.
    i_zshrgqms1-qz31dese = it001p-btext.
    i_zshrgqms1-qz31desc = it001p-btext.
    i_zshrgqms1-qz31con1 = space.
    i_zshrgqms1-qz31con2 = space.
    i_zshrgqms1-qz31con3 = space.
    i_zshrgqms1-qz31uflg = space.
    i_zshrgqms1-qz31dflg = space.
    i_zshrgqms1-qz31yrdt = par_date.
    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
    i_zshrgqms1-qz31rpdt = par_date.
    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
    APPEND i_zshrgqms1.
    CLEAR i_zshrgqms1.

  ENDLOOP.

  __cls it500p.

  SELECT * INTO TABLE it500p
  FROM t500p.

  LOOP AT it500p.
    i_zshrgqms1-qz31cody = 'G004'.
    i_zshrgqms1-qz31code = it500p-persa.
    i_zshrgqms1-qz31hkmc = 'A'.
    i_zshrgqms1-qz31desk = it500p-name1.
    i_zshrgqms1-qz31dese = it500p-name1.
    i_zshrgqms1-qz31desc = it500p-name1.
    i_zshrgqms1-qz31con1 = space.
    i_zshrgqms1-qz31con2 = space.
    i_zshrgqms1-qz31con3 = space.
    i_zshrgqms1-qz31uflg = space.
    i_zshrgqms1-qz31dflg = space.
    i_zshrgqms1-qz31yrdt = par_date.
    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
    i_zshrgqms1-qz31rpdt = par_date.
    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
    APPEND i_zshrgqms1.
    CLEAR i_zshrgqms1.

  ENDLOOP.

*  SORT itab BY plans pernr ASCENDING aedtm DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM itab
*    COMPARING plans.
*
*  LOOP AT itab.
*    i_zshrgqms1-qz31cody = 'G003'.
*    i_zshrgqms1-qz31code = itab-plans.
*    i_zshrgqms1-qz31hkmc = 'A'.
*    i_zshrgqms1-qz31desk = itab-ename.
*    i_zshrgqms1-qz31dese = itab-sname.
*    i_zshrgqms1-qz31desc = itab-sname.
*    i_zshrgqms1-qz31con1 = space.
*    i_zshrgqms1-qz31con2 = space.
*    i_zshrgqms1-qz31con3 = space.
*    i_zshrgqms1-qz31uflg = 'U'.
*    i_zshrgqms1-qz31dflg = space.
*    i_zshrgqms1-qz31yrdt = par_date.
*    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
*    i_zshrgqms1-qz31rpdt = par_date.
*    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
*    APPEND i_zshrgqms1.
*    CLEAR i_zshrgqms1.
*  ENDLOOP.

  __cls i_dept.

*  select a~objid b~stext into table i_dept
*  from hrp1001 as a inner join hrp1000 as b
*  on b~OTYPE eq a~OTYPE
*  and b~ENDDA eq a~ENDDA
*  and b~objid eq a~objid
*               where a~OTYPE eq 'O'
*                and a~ENDDA eq '99991231'
*                and a~RELAT eq 'Z01' .

  LOOP AT i_dept.
    i_zshrgqms1-qz31cody = 'G002'.
    i_zshrgqms1-qz31code = i_dept-objid.
    i_zshrgqms1-qz31hkmc = 'A'.
    i_zshrgqms1-qz31desk = i_dept-stext.
    i_zshrgqms1-qz31dese = i_dept-stext.
    i_zshrgqms1-qz31desc = i_dept-stext.
    i_zshrgqms1-qz31con1 = space.
    i_zshrgqms1-qz31con2 = space.
    i_zshrgqms1-qz31con3 = space.
    i_zshrgqms1-qz31uflg = space.
    i_zshrgqms1-qz31dflg = space.
    i_zshrgqms1-qz31yrdt = par_date.
    i_zshrgqms1-qz31yrja = 'HMMAEAI'.
    i_zshrgqms1-qz31rpdt = par_date.
    i_zshrgqms1-qz31rpja = 'HMMAEAI'.
    APPEND i_zshrgqms1.
    CLEAR i_zshrgqms1.
  ENDLOOP.

*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*

  IF p_call EQ true.
    DELETE i_zshrgqms1 WHERE qz31cody <> 'G002'.

    LOOP AT i_zshrgqms1.
      TRANSLATE : i_zshrgqms1-qz31desk TO UPPER CASE,
                  i_zshrgqms1-qz31dese TO UPPER CASE,
                  i_zshrgqms1-qz31desc TO UPPER CASE.
      MODIFY  i_zshrgqms1 INDEX sy-tabix.
    ENDLOOP.

    EXPORT i_zshrgqms1 TO MEMORY ID 'ZHGQMSWEEKLY'.
    LEAVE PROGRAM.
  ENDIF.

  IF par_r1 EQ true.
    PERFORM create_file.
  ELSE.
    PERFORM gqms_eai.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  gqms_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gqms_eai.

  SELECT SINGLE * FROM rfcdes  INTO *rfcdes
                WHERE rfcdest EQ par_dest.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No RFC Destination was found!'.
    EXIT.
  ENDIF.
  CALL FUNCTION 'Z_HR_GQMS_IF_WEEKLY'
    DESTINATION par_dest
    TABLES
      zshrgqms1 = i_zshrgqms1.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'I/F error was occured!'.
  ENDIF.

ENDFORM.                    " gqms_eai
*&---------------------------------------------------------------------*
*&      Form  create_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_file.

  DATA : l_filename   TYPE  string.

  IF par_file EQ space.
    par_file    = 'HR_GQMS'.
    par_file+8  = '-'.
    WRITE sy-datlo TO par_file+9(6) YYMMDD.
    par_file+15 = '-'.
    par_file+16 = sy-timlo.
    CONCATENATE par_file '.txt' INTO par_file.
    CONDENSE par_file NO-GAPS.
  ENDIF.

  l_filename = par_file.

*// === 2011.09.15 ECC6 Upgrade change by yn.kim ===== //*
  l_filename = par_file.

***  CALL FUNCTION 'WS_DOWNLOAD'
***       EXPORTING
***            filename        = par_file
***       TABLES
***            data_tab        = i_zshrgqms1
***       EXCEPTIONS
***            file_open_error = 1
***            OTHERS          = 2.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                        = l_filename
      filetype                        = 'DAT'
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      data_tab                        = i_zshrgqms1
    EXCEPTIONS
      file_write_error                = 1
      no_batch                        = 2
      gui_refuse_filetransfer         = 3
      invalid_type                    = 4
      no_authority                    = 5
      unknown_error                   = 6
      header_not_allowed              = 7
      separator_not_allowed           = 8
      filesize_not_allowed            = 9
      header_too_long                 = 10
      dp_error_create                 = 11
      dp_error_send                   = 12
      dp_error_write                  = 13
      unknown_dp_error                = 14
      access_denied                   = 15
      dp_out_of_memory                = 16
      disk_full                       = 17
      dp_timeout                      = 18
      file_not_found                  = 19
      dataprovider_exception          = 20
      control_flush_error             = 21
      OTHERS                          = 22.
*//  =============== change end =================== //*

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'ERROR OPENING/DOWNLOADING TO PC FILE.'.
  ELSE.
    MESSAGE s000 WITH 'File was created successfully!:' par_file.
  ENDIF.

ENDFORM.                    " create_file
