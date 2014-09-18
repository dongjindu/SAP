REPORT ZR_PROGRAM_EXECUTION_LIST .
*****************************************************************
*  Date        Developer    Request        Description
* 03/07/2007   Manju        UD1K930954     Initial Coding
*
* This Program lists all the Z program that was executed for a
* given Period.
*****************************************************************
TABLES : tstc.

* Data Declarations
data : s3 type i,
       l_date(10)  type c,
       L_task(1),
       l_tcode(20) type c,
       l_cnt type i,
       l_cnt1 type i,
       i_directory  TYPE TABLE OF sapwldir with header line.
DATA: TT_RESPTI_HITL  LIKE SAPWLHITL OCCURS 0 WITH HEADER LINE,
      TT_DBCALLS_HITL LIKE SAPWLHITL OCCURS 0 WITH HEADER LINE,
      IT_RESPTI_HITL  LIKE SAPWLHITL  OCCURS 0 WITH HEADER LINE,
     TT_APPLICATION_STATISTIC like SAPWLUSTCX OCCURS 0 WITH HEADER LINe,
     IT_APPLICATION_STATISTIC like SAPWLUSTCX OCCURS 0 WITH HEADER LINe,
     WA_APPLICATION_STATISTIC like SAPWLUSTCX OCCURS 0 WITH HEADER LINe.


data : begin of it_tab occurs 0,
      name like trdir-name,
      include like D010INC-include,
     end of it_tab.

data : begin of wa_tab occurs 0,
 name like trdir-name,
 include like D010INC-include,
end of wa_tab.

data : begin of it_report occurs 0,
        name like trdir-name,
       end of it_report.


constants : repname(2) value 'Z%'.

* Selection Screen
PARAMETERS:
       P_TTYPE     LIKE SAPWLPFNRM-TASKTYPE default ''. "Task type
SELECTION-SCREEN ULINE.
Parameters :     p_r1 radiobutton group g1,
                 p_r2 radiobutton group g1 default 'X'.


SELECTION-SCREEN ULINE.
PARAMETERS: P_Sdate LIKE SY-DATUM obligatory ,             "ab Tag
            p_eDAte LIKE SY-DATUM obligatory.

SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-BL1.
SELECTION-SCREEN COMMENT /1(30) TEXT-COM.
PARAMETERS:
            P_INCL    as checkbox,
            P_r3 as checkbox,
            p_c1 as checkbox ,
            p_c2 as checkbox.
SELECTION-SCREEN END OF BLOCK BL1.


* F4 value for task Type
AT SELECTION-SCREEN ON VALUE-REQUEST for  P_TTYPE.

  DATA : BEGIN OF value_tab OCCURS 0,
            TTYPE  like SAPWLPFNRM-TASKTYPE,
            Desc like SAPWLPFNRM-REPORT ,
           END OF value_tab,
           l_dyname like sy-repid.
  DATA : T_RETURN TYPE STANDARD TABLE OF DDSHRETVAL WITH HEADER LINE.

  l_dyname = sy-repid.

  value_tab-TTYPE = ''.
  value_tab-desc = 'Both Dialog & Background Programs'.
  append value_tab.

  value_tab-TTYPE = '01'.
  value_tab-desc = 'Dialog programs'.
  append value_tab.

  value_tab-TTYPE = '04'.
  value_tab-desc = 'Background Programs'.
  append value_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'TTYPE'
            dynpprog        = l_dyname
            dynpnr          = '1000'
            dynprofield     = 'P_TTYPE'
            window_title    = 'TASK TYPE'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

* Start of selection


start-of-selection.

  CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
       EXPORTING
            I_DATUM_BIS   = p_edate
            I_DATUM_VON   = p_sdate
            I_KZ_INCL_BIS = ''
       IMPORTING
            E_MONATE      = s3.

* Select All programs that executed for a given Period
  if p_c1 eq space  and  p_c2  eq space.
    perform select_data.
  endif.

* List All Z PROGRAMS in the system along with its Includes
  if p_c1 eq 'X'.
    select a~PROG b~include  into table it_tab from
      D010SINF as a inner join  D010INC as b
       on a~PROG = b~MASTER
      where  a~prog like  repname and
            a~SUBC eq '1' and
            b~include in ( select prog from D010SINF
                           where  prog = b~include and
                                  SUBC = 'I' and
                                  prog like 'Z%' ).
* List All Z INCLUDES in the system
  elseif p_c2 eq 'X'.
    select distinct include  into table it_tab
           from D010INC where INCLUDE like repname.
  endif.



end-of-selection.

* Write Output for Option ZPROGRAM & INCLUDE
  if p_c1 eq 'X'.
    perform write_output.
  endif.
* write output for Option only Z Includes
  if p_c2 eq 'X'.
    perform write_output1.

  endif.


*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

* Since SAP Stores SAP Program Execution based on
* Month / Week and Day wise.
* Get All  the possible dates for which program STATISTICS
* exists - Then based on input dates get required data.

  CALL FUNCTION 'SAPWL_WORKLOAD_GET_DIRECTORY'
       TABLES
            DIRECTORY = i_directory.

* We are interested in getting monthwise statictics with HOSTID as TOTAL
  delete   i_directory where PERIODTYPE ne 'M'.
  delete   i_directory where HOSTID  ne 'TOTAL'.

* Monthwise program STATISTICS is stored from begining of month till
* end of Month.
  concatenate  p_sdate+0(4) p_sdate+4(2) '01' into l_date.
  .
* Delete All records that are less than lower range Date
  delete   i_directory where startdate < l_date.


* Based on input dates - Get Program STATISTICS month wise and store
* it in intermediate internal table.
  loop at i_directory where startdate between l_date and p_edate.

    CALL FUNCTION 'SAPWL_WORKLOAD_GET_STATISTIC'
         EXPORTING
              PERIODTYPE            = 'M'  "Month
              HOSTID                = 'TOTAL'
              STARTDATE             = i_directory-startdate
         TABLES
              HITLIST_DBCALLS       = TT_DBCALLS_HITL
              HITLIST_RESPTI        = TT_RESPTI_HITL
              APPLICATION_STATISTIC = TT_APPLICATION_STATISTIC
         EXCEPTIONS
              UNKNOWN_PERIODTYPE    = 1
              NO_DATA_FOUND         = 2
              OTHERS                = 3.

    append lines of  TT_RESPTI_HITL to IT_RESPTI_HITL.
   append lines of TT_APPLICATION_STATISTIC to WA_APPLICATION_STATISTIC.
  endloop.

  sort IT_RESPTI_HITl by date.

* Retain all STATISTIC records with account type CUMUL and programs of
*Dialog & Background Type
* IT_APPLICATION_STATISTIC-ENTRY_ID is combination of TCODE + program +
* Background Job + TYpe ( R- Report , T - CODE ) Meaning if program is
*executed using TCODE only TCODE will be available , using TCODE get
*program name
  loop at WA_APPLICATION_STATISTIC where ACCOUNT eq 'cumul.'.
    check WA_APPLICATION_STATISTIC-TTYPE eq '01' or
          WA_APPLICATION_STATISTIC-TTYPE eq '04' .
    move  WA_APPLICATION_STATISTIC to
    IT_APPLICATION_STATISTIC .
    IT_APPLICATION_STATISTIC-account = ''.
    collect IT_APPLICATION_STATISTIC.
    if  IT_APPLICATION_STATISTIC-ENTRY_ID+72(1)  eq 'R'.
      it_report-name = IT_APPLICATION_STATISTIC-ENTRY_ID+0(40).
    else.
      select single * from tstc where TCODE eq
        IT_APPLICATION_STATISTIC-ENTRY_ID+0(20).
      if sy-subrc eq 0.
        it_report-name = tstc-PGMNA.
      endif.
    endif.
    collect it_report.
  endloop.

* Only Z Programs Option
  if   p_r2  eq 'X'.
    delete IT_RESPTI_HITl where  REPORT+0(1) ne  'Z'.
    delete IT_APPLICATION_STATISTIC where  ENTRY_ID+0(1)
     ne  'Z' ." or ENTRY_ID+0(1) ne 'Y'.
  endif.

* Task Type ( Dialog or Background )
  if not  P_TTYPE is initial.
    delete IT_APPLICATION_STATISTIC where  TTYPE ne p_TTYPE.
  endif.


* Select Includes option is choosen
  if P_INCL  eq 'X'.
    if not it_report[] is initial.
      select  MASTER include  into table wa_tab
         from D010INC for all entries in it_report
          where master eq it_report-name  and
                include like repname.
    endif.
  endif.

  if P_r3  eq ''.
* Write Output
    perform write_output2.
  else.
* If distinct program & include option is choosen
    perform write_output3.
  endif.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  write_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output.
  sort it_tab by name.
  format color 1.
  write :/(30) 'Program Name',30(30) 'Includes'.
  skip 1.
  format reset.
  loop at it_tab.
    at new name.
      write :/(30) it_tab-name.
      l_cnt = l_cnt + 1.
    endat.
    write :/(30) '',30(30) it_tab-include.
    l_cnt1 = l_cnt1 + 1.
    at end of name.
      format color 2.
*    uline at 1(60).
      write :/
   '__________________________________________________________'.
      format reset.
    endat.
  endloop.
  write :/ 'No of Programs',l_cnt color 3,
         'No of Includes', l_cnt1 COLOR 3.
ENDFORM.                    " write_output
*&---------------------------------------------------------------------*
*&      Form  write_output1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output1.
  sort it_tab by name.

  format color 1.
  write :/(30) 'Include Name'.
  skip 1.
  format reset.
  loop at it_tab.
    write :/(30) it_tab-name.
    l_cnt1 = l_cnt1 + 1.
  endloop.
  write :/ 'No of Programs',l_cnt color 3,
         'No of Includes', l_cnt1 COLOR 3.
ENDFORM.                    " write_output1
*&---------------------------------------------------------------------*
*&      Form  write_output2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output2.
  sort IT_APPLICATION_STATISTIC by TTYPE ENTRY_ID.
  format color 1.
  write :/1(5) 'TType',
          7(20) 'TCODE',
          28(40) 'Program Name',
          70(40) 'Background Job',
          111(21) 'Program Exec. Count'.
  format reset.
  loop at   IT_APPLICATION_STATISTIC.
    if    IT_APPLICATION_STATISTIC-TTYPE eq '01' .
      L_task = 'D'.
      l_tcode = 'SE38'.
    elseif  IT_APPLICATION_STATISTIC-TTYPE eq '04'.
      L_task = 'B'.
    endif.
    FORMAT COLOR  COL_NORMAL .

    if  IT_APPLICATION_STATISTIC-ENTRY_ID+72(1)  eq 'T'.
      select single * from tstc where TCODE eq
           IT_APPLICATION_STATISTIC-ENTRY_ID+0(20).
      if sy-subrc eq 0.
        IT_APPLICATION_STATISTIC-ENTRY_ID+20(40) = tstc-PGMNA.
      endif.
      write :/1(5) L_task,
              7(20) IT_APPLICATION_STATISTIC-ENTRY_ID+0(20),
              28(40) tstc-PGMNA,
*              70(40) IT_APPLICATION_STATISTIC-ENTRY_ID+40(32),
              70(40) '',
              111(21) IT_APPLICATION_STATISTIC-COUNT.
    else.
      write :/1(5) L_task,
              7(20) l_TCODE,
              28(40) IT_APPLICATION_STATISTIC-ENTRY_ID+0(40),
              70(40) IT_APPLICATION_STATISTIC-ENTRY_ID+40(32),
              111(21) IT_APPLICATION_STATISTIC-COUNT.
    endif.

    clear l_TCODE.
    if   P_INCL  eq 'X'.
      loop at wa_tab where name  eq
IT_APPLICATION_STATISTIC-ENTRY_ID+0(40).
        FORMAT   reset.
        FORMAT COLOR  COL_POSITIVE.

        write:/50(40)  wa_tab-include.
        FORMAT  reset.
      endloop.
*         write :/ .
    endif.
  endloop.
ENDFORM.                    " write_output2
*&---------------------------------------------------------------------*
*&      Form  write_output3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output3.
  select  MASTER include  into table wa_tab
      from D010INC for all entries in it_report
       where master eq it_report-name  and
             include like repname.
  format color 1.
  write :/1(40) 'Program',
          41(40) 'Includes'.
  format reset.
  sort it_report by name.
  sort wa_tab by name include.
  loop at it_report.
    check it_report+0(1) eq 'Z' or
          it_report+0(1) eq 'Y' .
    l_cnt  = l_cnt + 1.
    FORMAT COLOR  COL_NORMAL .
    write :/1(40)  it_report-name.
    loop at wa_tab where name eq it_report-name.
      format COLOR  COL_POSITIVE.
      write :/41(40) wa_tab-include.
      format reset.
      l_cnt1 = l_cnt1 + 1.
    endloop.
  endloop.
  skip 2.
  write :/ 'No of Programs',l_cnt color 3,
          'No of Includes', l_cnt1 COLOR 3.
ENDFORM.                    " write_output3
