REPORT ZRMM_ALEAUD_IDOC_CREATE .

***************************************************************
* Date       Developer   Request     Description
* 01/24/07   Manju       UD1K930471  This prorgam will send out
*                                    ALEAUD IDOC's for all
*                                    DESADV's in error state
* 03/12/07   Manju       UD1K940020  Providing Reporting options
****************************************************************

TABLES : BDAUDSTATE,EDIDC,EDIDS,SSCRFIELDS.

TYPE-POOLS: AUDIT.
INCLUDE <ICON>.
data: TIME_0 type time value '000000',
      TIME_24 type time value '240000'.


* EVENT Definition
* Begin of changes - UD1K940020
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

  PRIVATE SECTION.

ENDCLASS.
*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.



    CLEAR ls_toolbar.
    MOVE 'WE02' TO ls_toolbar-function.
    MOVE 'SAP IDoc List' TO ls_toolbar-quickinfo.
    MOVE 'WE02' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
*


  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'WE02'.
        PERFORM PROCESS_WE02.
    ENDCASE.
  ENDMETHOD.
endclass.

* end of changes - UD1K940020

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-BL1 .
SELECT-OPTIONS:
                S_SNDPRN FOR edidc-SNDPRN,
                S_RCVPRN for edidc-RCVPRN.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
                S_MESTYP FOR BDAUDSTATE-MESS_TYPE DEFAULT 'DESADV'
obligatory,
                S_DOCNUM for EDIDC-DOCNUM,
                S_STATUS for EDIDC-STATUS DEFAULT '51',
                S_MESCOD FOR BDAUDSTATE-MESS_CODE,
                S_MESFCT FOR BDAUDSTATE-MESS_FUNCT.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: S_UPDDAT FOR EDIDC-UPDDAT default sy-datum ,
                s_UPDTIM for EDIDC-UPDTIM DEFAULT TIME_0 TO TIME_24,
                s_STATYP for EDIDS-statyp ,
                s_STAMID for edids-STAMID,
                S_STAMNO for EDIDS-STAMNO,
                s_uname  for EDIDS-uname,
                S_MESTY1 FOR BDAUDSTATE-MESS_TYPE DEFAULT 'ALEAUD'
no-display,
                s_date for sy-datum default sy-datum no-display.
selection-screen end of block b1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-BL2 .
Parameters : P_R1 radiobutton group r1,                     "UD1K940020
             p_R2 radiobutton group r1.                     "UD1K940020

selection-screen end of block b2.

constants : c_status(2) value 68.
* Internal table Declaration
data : begin of it_data occurs 0.
        INCLUDE STRUCTURE EDIDC.
data:  end of it_data.

data : begin of it_status occurs 0.
        INCLUDE STRUCTURE EDIDS.
data:  end of it_status.


data : begin of it_tab occurs 0,
       SNDPRN like edidc-SNDPRN,
       REFNR  like E1EDL55-REFNR,
       DOCNUM like edidc-docnum,
       flag(1) type c,
       end of it_tab.

data : begin of IT_TAB1 occurs 0,
       SNDPRN like edidc-SNDPRN,
       REFNR  like E1EDL55-REFNR,
       MESTYP like EDIDC-MESTYP,
       DOCNUM like edidc-docnum,
       status like edids-status,
       CREDAT like edids-credat,
       cretim like edids-CRETIM,
       profile(10) type c,
       end of it_tab1.


data : LEFT_DATE LIKE EDIDC-UPDDAT,
       LEFT_TIME LIKE EDIDC-UPDTIM,
       RIGHT_DATE LIKE EDIDC-UPDDAT,
       RIGHT_TIME LIKE EDIDC-UPDTIM.


DATA: t_edidd LIKE edidd OCCURS 0 WITH HEADER LINE,
      idoc_control_complete LIKE edidc,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*      T_IDOC_CONTROL like edidc occurs 0 with header line .

data : begin of  T_IDOC_CONTROL occurs 0,
       MESTYP like edidc-mestyp,
*       MESCOD like edidc-mescod,
*       MESFCT like edidc-mesfct,
       CREDAT like edidc-credat,
       CRETIM like edidc-cretim,
       DOCNUM like edidc-docnum,
       STATUS like edidc-status,
       RCVPOR like edidc-RCVPOR,
       RCVPRT like Edidc-rcvprt,
       RCVPRN like edidc-rcvprn,
       SNDPOR like edidc-SNDPOR,
       SNDPRT like edidc-SNDPRT,
       SNDPRN like edidc-SNDPRN,
       end of T_IDOC_CONTROL.

* ALV Declarations
*  Begin of changes - UD1K940020
data:  G_REPID like sy-repid,
      it_fieldcat1 TYPE LVC_T_FCAT WITH HEADER LINE,
      it_fieldcat like table of it_fieldcat1,
      is_layout type LVC_S_LAYO,
      it_sort   TYPE LVC_T_SORT WITH HEADER LINE,
      is_layout2 type LVC_S_LAYO,
      it_sort2   TYPE LVC_T_SORT WITH HEADER LINE,
      OK_CODE LIKE SY-UCOMM,
      gs_variant TYPE DISVARIANT,
      L_CONTAINER TYPE SCRFNAME VALUE 'MY_CONT',
      G_DAILOG TYPE SCRFNAME VALUE 'CC_DAILOG',
      GRID1  TYPE REF TO CL_GUI_ALV_GRID,
      GRID2  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      event_receiver TYPE REF TO lcl_event_receiver.
* end of changes - UD1K940020

data :    T_IDOC_DATA LIKE EDIDD OCCURS 0 WITH HEADER LINE,
          IDOC_CONTROL like  EDIDC,
          it_edidd type edid4 occurs 0 with header line,
          IDOC_SEG LIKE E1STATE,
          IDOC_OBJ_SEG LIKE E1PRTOB,
          IDOC_HDR_SEG LIKE E1ADHDR,
          wa_E1EDL55 like E1EDL55,
          WA_E1EDl24 LIKE  E1EDl24,
          wa_E1ADRM1 like E1ADRM1,
          l_PARTNER_ID like wa_E1ADRM1-PARTNER_ID,
          l_NAME1 like wa_E1ADRM1-NAME1,
          l_NAME2 like wa_E1ADRM1-NAME1,
          idoc_status LIKE bdidocstat,
          error_occured ,
          l_cnt type i,
          l_cnt1 type i,
          l_cnt2 type i,
          str_buf(273)  type  c,
          l_RCVPOR like EDP13-RCVPOR,
          l_MESTYP like edidc-MESTYP,
          l_docnum like EDIDD-docnum.

DATA: SEND_CONTROL LIKE EDIDC OCCURS 0 WITH HEADER LINE,
      i_idoc_status like BDIDOCSTAT OCCURS 0 WITH HEADER LINE.

AT SELECTION-SCREEN.
  if S_UPDDAT is initial and SSCRFIELDS-uCOMM eq 'ONLI'
     and SY-BATCH IS INITIAL.
    MESSAGE e000(zz) WITH text-A12.
    stop.
  endif.

initialization.
* Default Status Message id
  s_STAMID-option = 'EQ'.
  s_stamid-sign = 'I'.
  s_STAMID-low = 'BORGR'.
  append s_STAMID.
  s_STAMID-low = 'ME'.
  append s_STAMID.
  s_STAMID-low = 'S5'.
  append s_STAMID.

* Default Status Message Number
  S_STAMNO-option = 'EQ'.
  S_STAMNO-sign = 'I'.
  S_STAMNO-low = '503'.
  append s_stamno.

  S_STAMNO-low = '161'.
  append s_stamno.

  S_STAMNO-low = '701'.
  append s_stamno.

  S_STAMNO-low = '706'.
  append s_stamno.

*  S_STAMNO-low = '735'.
*  append s_stamno.

  S_STAMNO-low = '777'.
  append s_stamno.

  S_STAMNO-low = '3'.
  append s_stamno.



start-of-selection.

* if program is scheduled in Background to execute in regular
* intervals.
  IF NOT SY-BATCH IS INITIAL AND S_UPDDAT[] IS INITIAL.
    PERFORM get_TIME_INTERVAL
       CHANGING
          LEFT_DATE
          LEFT_TIME
          RIGHT_DATE
          RIGHT_TIME.

    SELECT a~MESTYP
           a~CREDAT a~CRETIM
           a~DOCNUM a~STATUS a~RCVPOR a~RCVPRT a~RCVPRN
           a~SNDPOR a~SNDPRT a~SNDPRN
                 FROM EDIDC as a inner join edids as b on
                           a~docnum = b~docnum and
                           a~status = b~status
                 INTO corresponding fields of  TABLE T_IDOC_CONTROL
      WHERE ( UPDDAT = LEFT_DATE AND UPDTIM > LEFT_TIME
          OR UPDDAT > LEFT_DATE )
          AND ( UPDDAT < RIGHT_DATE
      OR UPDDAT = RIGHT_DATE AND UPDTIM <= RIGHT_TIME )
          AND SNDPRN IN S_SNDPRN
          AND MESTYP IN S_MESTYP
          AND MESCOD IN S_MESCOD
          AND MESFCT IN S_MESFCT
          AND a~STATUS in S_STATUS
          and b~STATYP in  s_STATYP
          and b~STAMID in s_STAMID
          and b~STAMNO in  S_STAMNO
          and b~UNAME  in  s_uname
        ORDER BY a~SNDPRN a~MESTYP
        a~CREDAT a~CRETIM.

  else.
    select   a~MESTYP
             a~CREDAT
             a~CRETIM   a~DOCNUM a~STATUS a~RCVPOR a~RCVPRT a~RCVPRN
             a~SNDPOR a~SNDPRT a~SNDPRN
             from EDIDC as a inner join EDIDS as b on
                      a~docnum = b~docnum and
                      a~status = b~status
                      into table T_IDOC_CONTROL
               where  a~UPDDAT in s_upddat  and
                     a~UPDTIM in s_UPDTIM  and
                     a~DOCNUM in s_DOCNUM and
                     a~status in s_status and
                     SNDPRN IN S_SNDPRN and
                     MESTYP IN S_MESTYP and
                     MESCOD IN S_MESCOD and
                     MESFCT IN S_MESFCT and
                     b~STATYP in  s_STATYP and
                     b~STAMID in s_STAMID and
                     b~STAMNO in  S_STAMNO and
                     b~UNAME  in  s_uname.
  endif.



end-of-selection.

* CREATE ALEAUD IDOC  for DESADV IDOCS in status 51
  Perform Create_IDOC.
*&---------------------------------------------------------------------*
*&      Form  Create_IDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Create_IDOC.

* Collect all IDOCs with same Bill of Lading Number
* to create one ALEAUD IDOC per Bill of Lading number
  perform collect_idoc_by_bol.

* In background always set Reporting Option to Create
* ALEAUD IDOC's
* Begin of changes - UD1K940020
  if sy-batch eq 'X' .
    P_R2 =  'X' .P_R1 =  '' .
  endif.

* IF Display Option is choosen
  if P_R1  eq 'X'  .
    call screen 100.
  else.
*  end of changes - UD1K940020

* Group selected DESADV IDOC's by Bill of lading in order
* to create one ALEAUD IDOC per Bill of lading number.
    perform create_IDOC_PER_BOL.
 write :/ 'No. of Distinct ASNs', l_cnt1, 'No. of DESADV IDOCS', l_cnt2.
    write :/ 'No. of ALEAUDs IDOC Generated', l_cnt.
  endif.
ENDFORM.                    " Create_IDOC
*&---------------------------------------------------------------------*
*&      Form  TIME_INTERVAL_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LEFT_DATE  text
*      <--P_LEFT_TIME  text
*      <--P_RIGHT_DATE  text
*      <--P_RIGHT_TIME  text
*----------------------------------------------------------------------*
FORM GET_TIME_INTERVAL CHANGING
    LEFT_DATE LIKE EDIDC-UPDDAT
    LEFT_TIME LIKE EDIDC-UPDTIM
    RIGHT_DATE LIKE EDIDC-UPDDAT
    RIGHT_TIME LIKE EDIDC-UPDTIM.

  DATA: JOB_NAME LIKE TBTCJOB-JOBNAME,
        JOB_COUNT LIKE TBTCJOB-JOBCOUNT,
        JOB_HEAD LIKE TBTCJOB,
        CMP_TIME LIKE EDIDC-UPDTIM.

* Get Job details - Works only in Background
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
       IMPORTING
            JOBCOUNT = JOB_COUNT
            JOBNAME  = JOB_NAME.

* Get execution time details for the job
  CALL FUNCTION 'BP_JOB_READ'
       EXPORTING
            JOB_READ_JOBCOUNT = JOB_COUNT
            JOB_READ_JOBNAME  = JOB_NAME
            JOB_READ_OPCODE   = 19
       IMPORTING
            JOB_READ_JOBHEAD  = JOB_HEAD.

*     sdltime is the time when the job was scheduled, this is either the
*     time, when the job was put to the batch list or the time when the
*     predeccessor started

* 1 minute substracted only to find out previous day if time is >
* 23:59:00 and <= 24:00:00
  LEFT_TIME = JOB_HEAD-SDLTIME - 60.  "60 seconds = 1 minutes
  CMP_TIME = -60.                     "23:59:00 Internal REP
  IF LEFT_TIME < CMP_TIME.
    LEFT_DATE = JOB_HEAD-SDLDATE.
  ELSE.
* if Time is > 23:59:00 and < 24:00:00
* change  Date to previous date
    LEFT_DATE = JOB_HEAD-SDLDATE - 1.
  ENDIF.
*    strttime is the time when the job was started

* 1 minute substracted only to find out previous day if time is >
* 23:59:00 and <= 24:00:00
  RIGHT_TIME = JOB_HEAD-STRTTIME - 60. "60 seconds = 1 minutes
  CMP_TIME = -60.                      "23:59:00 Internal REP
  IF RIGHT_TIME < CMP_TIME.
    RIGHT_DATE = JOB_HEAD-STRTDATE.
  ELSE.
* if Time is > 23:59:00 and < 24:00:00
* change  Date to previous day
* Date has to be changed
    RIGHT_DATE = JOB_HEAD-STRTDATE - 1.
  ENDIF.
  write : / 'SysDate', sy-datum, 'System Time', sy-uzeit .
  write :/ 'Left Date',LEFT_DATE,'Left Time',left_time.
  write :/ 'Right Date',RIGHT_DATE,'Right Time',right_time.

ENDFORM.                    " TIME_INTERVAL_GET
*&---------------------------------------------------------------------*
*&      Form  collect_idoc_by_bol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_idoc_by_bol.
  Loop at T_IDOC_CONTROL.

    CALL FUNCTION 'IDOC_READ_COMPLETELY'
         EXPORTING
              document_number         = T_IDOC_CONTROL-docnum
         TABLES
              int_edidd               = it_edidd
         EXCEPTIONS
              document_not_exist      = 1
              document_number_invalid = 2
              OTHERS                  = 3.
    it_tab1-mestyp = T_IDOC_CONTROL-mestyp.
    it_tab1-status = T_IDOC_CONTROL-status.
    it_tab1-credat = T_IDOC_CONTROL-credat.
    it_tab1-CRETIM = T_IDOC_CONTROL-CRETIM.
    it_tab1-profile = 'Y'.
* Check whether Partner profile exists.
    select single RCVPOR into l_RCVPOR from EDP13
       where RCVPRN = T_IDOC_CONTROL-SNDPRN  and
             RCVPRT = 'LI' and
             RCVPFC = ''   and
             MESTYP = 'ALEAUD'.
    if sy-subrc ne 0 and sy-batch eq 'X'.
      write :/ ' Maintain Partner profile for Message type ALEAUD for',
     T_IDOC_CONTROL-SNDPRN.
      continue.
    elseif sy-subrc ne 0 and sy-batch eq ''.
      it_tab1-profile = 'N' .
    endif.


    loop at it_edidd.

      case it_edidd-SEGNAM.
        when 'E1EDL55'.   " BOL  number
          wa_E1EDL55 =  it_edidd-sdata.
          if  wa_E1EDL55-QUALF = '001'.
            it_tab-SNDPRN = T_IDOC_CONTROL-SNDPRN.
            it_tab-REFNR  = wa_E1EDL55-REFNR.
            it_tab-DOCNUM = T_IDOC_CONTROL-docnum.
            append  it_tab.
            it_tab1-SNDPRN = T_IDOC_CONTROL-SNDPRN.
            it_tab1-REFNR  = wa_E1EDL55-REFNR.
            it_tab1-DOCNUM = T_IDOC_CONTROL-docnum.
            collect  it_tab1.
            exit.
          endif.
      endcase.
    endloop.
    clear : it_edidd[], wa_E1EDL55.
  endloop.

  sort it_tab by SNDPRN REFNR DOCNUM .
  delete adjacent duplicates from it_tab.
ENDFORM.                    " collect_idoc_by_bol
*&---------------------------------------------------------------------*
*&      Form  create_IDOC_PER_BOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_IDOC_PER_BOL.

  data :  L_REFNR  like E1EDL55-REFNR.

  loop at   it_tab.
    l_docnum  = it_tab-DOCNUM.
    L_REFNR = it_tab-REFNR.


    CALL FUNCTION 'IDOC_READ_COMPLETELY'
         EXPORTING
              document_number         = it_tab-docnum
         IMPORTING
              IDOC_CONTROL            = IDOC_CONTROL
         TABLES
              int_edidd               = it_edidd
              INT_EDIDS               = it_status
         EXCEPTIONS
              document_not_exist      = 1
              document_number_invalid = 2
              OTHERS                  = 3.
************************************************************************
* Note :- Assumption is that DESADV IDOC will have only one E1EDL24
*segment most of the time. How ever in some cases you might find
*multiple E1EDL24 segments for the same material ( Case Number).
* In case of multiple E1EDL24 segments for the same material , just read
* last material of E1EDL24 segment.
*Segment E1STATE in ALEAUD IDOC type is populated based on Number of
*error message rather than Material occurence  & error message.
* As of today splitting of IDOC based on different case numbers is
*happening at See Burger Side. In other words one DESADV IDOC per
*material number ( Case Number) .
* In future if above process changes then program needs to be changed
************************************************************************
    loop at it_edidd.
      case it_edidd-SEGNAM.
        when 'E1EDL55'.   " ASN Number
          wa_E1EDL55 =  it_edidd-sdata.
          if wa_E1EDL55-QUALF eq '001'.
            wa_E1EDL55 =  it_edidd-sdata.
          else.
            clear wa_E1EDL55.
          endif.
        when 'E1ADRM1'.
          wa_E1ADRM1 = it_edidd-sdata.
          if  wa_E1ADRM1-PARTNER_Q = 'LF'.
            l_PARTNER_ID = wa_E1ADRM1-PARTNER_ID.
            l_NAME1 = wa_E1ADRM1-name1.
          elseif wa_E1ADRM1-PARTNER_Q = 'WE' .
            l_NAME2 = wa_E1ADRM1-name1.
          endif.
        when 'E1EDL24'. " Case #
          WA_E1EDL24 = it_edidd-sdata.
      endcase.
    endloop.


* Control Record
    idoc_control_complete-DOCTYP = 'ALEAUD01'.
    idoc_control_complete-MESTYP = 'ALEAUD'.
    idoc_control_complete-DIRECT = '1'.
    idoc_control_complete-OUTMOD = '2'.

* Sender of original DESADV willl be receiver
    idoc_control_complete-RCVPRT = IDOC_CONTROL-SNDPRT.
    idoc_control_complete-RCVPRN = IDOC_CONTROL-SNDPRN.
* Select Port
    select single RCVPOR into l_RCVPOR from EDP13
       where RCVPRN = T_IDOC_CONTROL-SNDPRN  and
             RCVPRT = 'LI' and
             MESTYP = idoc_control_complete-MESTYP.
    if sy-subrc eq 0.
      idoc_control_complete-RCVPOR = l_RCVPOR.
    endif.


* Receiver of original DESADV ( SAP)  wil be the sender
    idoc_control_complete-SNDPOR = IDOC_CONTROL-RCVPOR.
*   idoc_control_complete-SNDPOR = 'SAPUD1'.
    idoc_control_complete-SNDPRT = IDOC_CONTROL-RCVPRT.
    idoc_control_complete-SNDPRN = IDOC_CONTROL-RCVPRN.
*    idoc_control_complete-SNDPRN = 'UD1CLNT300'..



* Populate E1ADHDR segment
    IDOC_HDR_SEG-MESTYP = 'ALEAUD'.
    IDOC_HDR_SEG-MESCOD = ''.
    IDOC_HDR_SEG-MESFCT = ''.
    IDOC_HDR_SEG-MESTYP_LNG = sy-langu.
    T_IDOC_DATA-SEGNAM = 'E1ADHDR'.
    T_IDOC_DATA-SDATA = IDOC_HDR_SEG.
    Append T_IDOC_DATA.

*Populate E1STATE Segment with multiple error message per that IDOC
* along with IDOCs with same BOL

* IDOC ERROR Message
    loop at it_status where DOCNUM eq  it_tab-docnum and
                            STATUS in  S_STATUS  and
*                           STATYP in  s_STATYP  and
                            STAMID in  s_STAMID  and
                            STAMNO in  S_STAMNO.
      str_buf = it_status-statxt.
* Replace Paramter with actaul value
      if  str_buf CA '&'.
        if it_status-STAMNO eq '706'.
          pack   it_status-STAPA1 to it_status-STAPA1.
          replace '&1' with it_status-STAPA1 into str_buf.
        else.
          replace '&' with it_status-STAPA1 into str_buf.
        endif.
      endif.

      if  str_buf CA '&'.
        if it_status-STAMNO eq '706'.
          replace '&2' with it_status-STAPA2 into str_buf.
        else.
          replace '&' with it_status-STAPA2 into str_buf.
        endif.
      endif.
      if  str_buf CA '&'.
        replace '&' with it_status-STAPA3 into str_buf.
      endif.
      if  str_buf CA '&'.
        replace '&' with it_status-STAPA4 into str_buf.
      endif.

*CURRENT:  Ordered material 857103K100LK differs from material
*857100A400 in deli
*NEW: Ordered material 857103K100LK - ASN material 857100A400

* Begin  of changes -  UD1K940020
      if it_status-STAMID eq 'ME' and it_status-STAMNO eq '777'.
        replace 'differs from' with ' - ASN' into str_buf.
        replace 'in delivery' with '' into str_buf.
      endif.
* End of changes - UD1K940020
      condense str_buf.
* Remove special Characters
      replace '&' with space into str_buf.
      replace '*' with space into str_buf.
      replace '~' with space into str_buf.
      replace '>' with space into str_buf.

      it_status-STATXT = str_buf.
      IDOC_SEG-DOCNUM = it_tab-docnum.
      IDOC_SEG-STATUS = it_status-STATUS.


      IDOC_SEG-STACOD = it_status-STACOD.
      IDOC_SEG-STATXT = it_status-STATXT.    " Error Message
*     IDOC_SEG-STAPA1 = wa_E1EDL55-REFNR.    "ASN Number
*     IDOC_SEG-STAPA2 = wa_e1EDl20-LIFEX.    "Trailer Number
      IDOC_SEG-STAPA3 = WA_E1EDl24-MATNR.    "Case Number
      IDOC_SEG-STAPA4 = ''.
      IDOC_SEG-STATYP = it_status-STATYP.    "Error Type
      IDOC_SEG-STAMQU = it_status-STAMQU.    "Error code
      IDOC_SEG-STAMID = it_status-stamid.
      IDOC_SEG-STAMNO = it_status-STAMNO.
      IDOC_SEG-STAPA1_LNG = l_name2.          "Name
* DEFAULT VALUE
      IF IDOC_SEG-STAPA1_LNG  IS INITIAL.
        IDOC_SEG-STAPA1_LNG = 'Hyundai Car Assembly, AL'.
      ENDIF.
      IDOC_SEG-STAPA2_LNG = it_status-CREDAT. "IDOC creation Dt.
      IDOC_SEG-STAPA3_LNG = it_status-cretim. "IDOC creation time
*     IDOC_SEG-STAPA4_LNG = sy-langu.         "Language
      IDOC_SEG-STAPA4_LNG = wa_E1EDL55-REFNR.  "ASN Number
      T_IDOC_DATA-SEGNAM = 'E1STATE'.
      T_IDOC_DATA-SDATA = IDOC_SEG.
      Append T_IDOC_DATA.

* Populate  E1PRTOB segment
      IDOC_OBJ_SEG-DOCNUM  = it_tab-docnum."Original IDOC No of ASN
      IDOC_OBJ_SEG-LOGSYS  = l_PARTNER_ID.   " Partner ID
      IDOC_OBJ_SEG-OBJTYPE = ''.
      IDOC_OBJ_SEG-OBJKEY = l_name1.         " Partner Name
      T_IDOC_DATA-SEGNAM = 'E1PRTOB'.
      T_IDOC_DATA-SDATA = IDOC_OBJ_SEG.
      Append T_IDOC_DATA.
    endloop.

    l_cnt2 = l_cnt2 + 1.

* For Every Bill of Lading Number create one ALEAUD IDOC
    at end of REFNR.
      l_cnt1 = l_cnt1 + 1.
      CALL FUNCTION 'IDOC_CREATE_ON_DATABASE'
           EXPORTING
                idoc_status             = idoc_status
                error_occured           = error_occured
           TABLES
                idoc_data               = T_IDOC_DATA
           CHANGING
                idoc_control            = idoc_control_complete
           EXCEPTIONS
                idoc_input_inconsistent = 1
                OTHERS                  = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

*      clear SEND_CONTROL[].
      APPEND idoc_control_complete TO SEND_CONTROL.

* Submit IDOC Immediately -> Or you have to schedule RSEOUT00 program
* to PUSH IDOC's generated in the System to Seeburger. ( Provided your
*Partner profile settings is transfer IDOCS Immedaitely)
*      CALL FUNCTION 'EDI_OUTPUT_NEW'
*           TABLES
*                I_EDIDC = SEND_CONTROL
*                I_EDIDD = t_edidd
*           EXCEPTIONS
*                OTHERS  = 0.
*      commit work.

* Change status of Original DESADV IDOC to 68
*      loop at it_tab where REFNR =   L_REFNR.
*        l_docnum = it_tab-docnum.
*        i_idoc_status-DOCNUM = it_tab-docnum.
*        i_idoc_status-status = c_status.
*        append i_idoc_status.
*        call function 'IDOC_STATUS_WRITE_TO_DATABASE'
*             EXPORTING
*                  idoc_number               = l_docnum
*                  NO_DEQUEUE_FLAG           = 'X'
*             TABLES
*                  idoc_status               = i_idoc_status
*             EXCEPTIONS
*                  idoc_foreign_lock         = 1
*                  idoc_not_found            = 2
*                  idoc_status_records_empty = 3
*                  idoc_status_invalid       = 4
*                  db_error                  = 5
*                  others                    = 6.
*        clear :  i_idoc_status[],i_idoc_status.
*        commit work.
*      endloop.
      it_tab-flag = 'X'.
      modify it_tab transporting flag where REFNR = it_tab-REFNR.
      l_cnt = l_cnt + 1.
      clear : it_edidd[],it_status[],it_edidd,it_status,
              wa_E1EDL55,WA_E1EDL24,l_PARTNER_ID,l_NAME1,l_NAME2,
              Idoc_status,T_IDOC_DATA[]
              ,idoc_status,idoc_control_complete,
              T_IDOC_DATA[],T_IDOC_DATA,i_idoc_status[],l_docnum.
    endat.
  endloop.


  loop at it_tab where flag  =   'X'.
    l_docnum = it_tab-docnum.
    i_idoc_status-DOCNUM = it_tab-docnum.
    i_idoc_status-status = c_status.
    append i_idoc_status.
    call function 'IDOC_STATUS_WRITE_TO_DATABASE'
         EXPORTING
              idoc_number               = l_docnum
              NO_DEQUEUE_FLAG           = 'X'
         TABLES
              idoc_status               = i_idoc_status
         EXCEPTIONS
              idoc_foreign_lock         = 1
              idoc_not_found            = 2
              idoc_status_records_empty = 3
              idoc_status_invalid       = 4
              db_error                  = 5
              others                    = 6.
    clear :  i_idoc_status[],i_idoc_status.

  endloop.

  CALL FUNCTION 'EDI_OUTPUT_NEW'
       TABLES
            I_EDIDC = SEND_CONTROL
            I_EDIDD = t_edidd
       EXCEPTIONS
            OTHERS  = 0.

*     submit RSEOUT00 AND  RETURN
*       with MESTYP in S_MESTY1
*       with UPDDAT in s_date
*       with OUTMOD eq '2'.
  commit work.
  clear: SEND_CONTROL[], SEND_CONTROL.
ENDFORM.                    " create_IDOC_PER_BOL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'MAN100'.
  SET TITLEBAR 'T100'.
  PERFORM set_fieldcat.
  PERFORM set_SORT.
  PERFORM set_LAYOUT.


  G_REPID = SY-REPID.
  gs_variant-REPORT = G_REPID.
  append lines of it_fieldcat1 to it_fieldcat.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = L_CONTAINER.
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING IS_LAYOUT = is_layout
                    I_SAVE                   = 'A'
                    IS_VARIANT               = GS_VARIANT
          CHANGING IT_FIELDCATALOG = it_fieldcat
                   IT_SORT   = it_sort[]
                   IT_OUTTAB = IT_TAB1[].

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_user_command FOR grid1.
    SET HANDLER event_receiver->handle_toolbar FOR grid1.

    CALL METHOD grid1->set_toolbar_interactive.
  endif.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fieldcat.
  CLEAR : it_fieldcat1.
  REFRESH : it_fieldcat1.
  CHECK it_fieldcat1[] IS INITIAL.


  PERFORM append_fieldcat USING 'SNDPRN'
                                 'IT_TAB1'
                                 10
                                 'Sender Partner'
                                 'Sender Partner'
                                 'Sender Partner'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'REFNR'
                                 'IT_TAB1'
                                 35
                                 'ASN Number'
                                 'ASN Number'
                                 'ASN Number'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'MESTYP'
                                 'IT_TAB1'
                                 10
                                 'MESTYP'
                                 'MESTYP'
                                 'MESTYP'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'DOCNUM '
                                 'IT_TAB1'
                                 16
                                 'IDOC Number'
                                 'IDOC Number'
                                 'IDOC Number'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'STATUS '
                                 'IT_TAB1'
                                 2
                                 'Status'
                                 'Status'
                                 'Status'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'PROFILE '
                                 'IT_TAB1'
                                 10
                                 'Partner Profile'
                                 'Partner Profile'
                                 'Partner Profile'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'CREDAT'
                                 'IT_TAB1'
                                 10
                                 'Creation Date'
                                 'Creation Date'
                                 'Creation Date'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'CRETIM'
                                 'IT_TAB1'
                                 10
                                 'Creation TIME'
                                 'Creation TIME'
                                 'Creation TIME'
                                 'CHAR'
                                 'X'
                                 ''
                                 ''
                                 ''
                                 ''.


ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1496   text
*      -->P_1497   text
*      -->P_18     text
*      -->P_1499   text
*      -->P_1500   text
*      -->P_1501   text
*      -->P_1502   text
*      -->P_1503   text
*      -->P_1504   text
*      -->P_1505   text
*      -->P_1506   text
*      -->P_1507   text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_KEY_SEL
                              p_no_out
                              p_no_zero
                              p_text_field.
  it_fieldcat1-fieldname      = p_fieldname.
  it_fieldcat1-tabname        = p_tabname.
  it_fieldcat1-outputlen      = p_outputlen.
  it_fieldcat1-SCRTEXT_l      = p_text_l.
  it_fieldcat1-SCRTEXT_m      = p_text_m.
  it_fieldcat1-SCRTEXT_s      = p_text_s.
  it_fieldcat1-datatype       = p_datatype.
  it_fieldcat1-key            = p_key.
  it_fieldcat1-KEY_SEL        = p_KEY_SEL.
  it_fieldcat1-no_out         = p_no_out.
  it_fieldcat1-no_zero        = p_no_zero.
*  it_fieldcat1-text_fieldname = p_text_field.
  APPEND it_fieldcat1. CLEAR it_fieldcat1.
ENDFORM.                    " append_fieldcat1
*&---------------------------------------------------------------------*
*&      Form  set_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_SORT.

  it_sort-SPOS = 1.
  it_sort-FIELDNAME = 'SNDPRN'.
  append it_sort.

  it_sort-SPOS = 2.
  it_sort-FIELDNAME = 'REFNR'.
  append it_sort.
*  it_sort-SPOS = 2.
*  it_sort-FIELDNAME = 'ASN'.
*  append it_sort.
*  clear it_sort.

ENDFORM.                    " set_SORT
*&---------------------------------------------------------------------*
*&      Form  set_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_LAYOUT.

ENDFORM.                    " set_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  case sy-ucomm.

    when 'BACK'.
      set screen 0.
  endcase.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DETAILS.

ENDFORM.                    " PROCESS_DETAILS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_WE02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_WE02.
  DATA: CTU(1) VALUE 'X',
         MODE(1) VALUE 'A',
         UPDAT(1) VALUE 'S',
         MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
         lt_rows TYPE lvc_t_row,
         WA_lt_ROW LIKE LINE OF lt_rows.
  ranges : r_date for edids-credat.
  CLEAR lt_rows[].
  CLEAR WA_lt_row.

* Get Selected Rows
  CALL METHOD grid1->get_selected_rows
              IMPORTING et_index_rows = lt_rows.
  READ TABLE lt_ROWS INTO WA_lt_ROW INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_TAB1  INDEX WA_LT_ROW-INDEX.
    IF SY-SUBRC = 0.
      r_date-sign = 'I'.
      r_date-option = 'BT'.
      r_date-low = it_tab1-credat.
      r_date-high = it_tab1-credat.
      append r_date.
* CALL WE02 TCODE for the IDOC selected
      submit RSEIDOC2  with CREDAT in r_date
                       with docnum eq IT_TAB1-docnum
            and return.
      clear r_date[].
    ENDIF.
  endif.

ENDFORM.                    " PROCESS_WE02
