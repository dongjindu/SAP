REPORT ZRPM_CREATE_MEASUREMENT_DOC line-size 180 .

***************************************************************
* Date         Developer   Request         Description
* 03/08/2007   Manju       UD1K931005      Initial Coding
* 06/18/2007   Manju       UD1K940851      Report changes
****************************************************************
TABLES:ZPM_DIE_MAINT, EQUI.
selection-screen begin of block b1 with frame title text-001.
select-options : s_date for sy-datum default sy-datum,
                 s_dieno for ZPM_DIE_MAINT-ZDIE_NUMBER,
                 s_EQUNR for EQUI-EQUNR.
selection-screen end of block b1.

DATA:
*T_DIE_MAINT LIKE ZPM_DIE_MAINT OCCURS 0 WITH HEADER LINE,
      begin of it_DIE_MAINT occurs 0,
      ZDIE_NUMBER like equZ-TIDNR,	
      ZRUN_DATE like ZPM_DIE_MAINT-ZRUN_DATE, 	
      ZRUN_TIME like ZPM_DIE_MAINT-ZRUN_TIME,	
      ZSTROKE_COUNT like ZPM_DIE_MAINT-ZSTROKE_COUNT,	
      ZEAI_DATE like ZPM_DIE_MAINT-ZEAI_DATE,	
      ZEAI_STATUS like ZPM_DIE_MAINT-ZEAI_STATUS,	
      end of it_die_maint,
      begin of it_DIE_MAINT1 occurs 0,
      ZDIE_NUMBER like equZ-TIDNR,	
      ZRUN_DATE like ZPM_DIE_MAINT-ZRUN_DATE, 	
      ZRUN_TIME like ZPM_DIE_MAINT-ZRUN_TIME,	
      ZSTROKE_COUNT  type i,
      ZEAI_DATE like ZPM_DIE_MAINT-ZEAI_DATE,	
      ZEAI_STATUS like ZPM_DIE_MAINT-ZEAI_STATUS,	
      end of it_die_maint1,

      MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      begin of it_equi occurs 0,
       EQUNR like EQUI-EQUNR,
       TIDNR like EQUZ-TIDNR,
      end of it_equi,
      l_message(80),
      l_mess(80),
      l_scnt like  ZPM_DIE_MAINT-ZSTROKE_COUNT .



Start-of-selection.

SELECT ZDIE_NUMBER ZRUN_DATE ZRUN_TIME   ZSTROKE_COUNT
       ZEAI_DATE  ZEAI_STATUS
      FROM ZPM_DIE_MAINT INTO TABLE IT_DIE_MAINT
WHERE ZRUN_DATE in  s_DATE and
      ZDIE_NUMBER in s_dieno.

end-of-selection.

loop at IT_DIE_MAINT.
move-corresponding IT_DIE_MAINT to IT_DIE_MAINT1.
IT_DIE_MAINT1-ZRUN_TIME = ''.
IT_DIE_MAINT1-ZEAI_DATE = ''.
collect IT_DIE_MAINT1.
endloop.

  if not IT_DIE_MAINT1[] is initial.
* Changes - UD1K940851
*    select equnr matnr into table it_equi from equi
*       for all entries in IT_DIE_MAINT
*        where equnr in  s_EQUNR  and
*              matnr = IT_DIE_MAINT-ZDIE_NUMBER .
    select equnr TIDNR  into table it_equi from equZ
       for all entries in IT_DIE_MAINT1
        where  TIDNR  = IT_DIE_MAINT1-ZDIE_NUMBER and
               equnr in  s_EQUNR.


  endif.

  sort IT_DIE_MAINT1 by ZDIE_NUMBER.

  format color 3.
  write :/40(20) ' Error Log'.
  skip 3.
  format color 1.
  write : /1(18) 'DIE Number',
           20(18) 'Equipment Number',
           40(80) 'Error Message'.
  format reset.
  format color 2.
  loop at it_equi  .


    loop at IT_DIE_MAINT1 where ZDIE_NUMBER = it_equi-TIDNR.
*    check sy-subrc eq 0.
    l_scnt = IT_DIE_MAINT1-ZSTROKE_COUNT.
    CALL FUNCTION 'ZPM_CREATE_MEASUREMENT_READ'
     EXPORTING
       CTU                = 'X'
       MODE               = 'N'
       UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
*   NODATA             = '/'
       DFTIM_001          = IT_DIE_MAINT1-ZRUN_TIME
       DFDAT_002          = IT_DIE_MAINT1-ZRUN_DATE
       DFRDR_003          =  sy-uname
       EQUNR_004          = it_equi-EQUNR
       DFTIM_005          = '14:03:49'
       DFDAT_006          = '2007/03/01'
       DFRDR_007          =  sy-uname
       DFTIM_008          = '14:03:49'
       DFDAT_009          = '2007/03/01'
       DFRDR_010          = sy-uname
       RDCNT_01_011       = l_scnt
       DFTIM_012          = '14:03:49'
       DFDAT_013          = '2007/03/01'
       DFRDR_014          = sy-uname
* IMPORTING
*   SUBRC              =
     TABLES
       MESSTAB            = MESSTAB .


* Write Message Back to screen.
*    read table  messtab with key  MSGTYP = 'E'.
*    if sy-subrc eq 0.

    loop at messtab.
      concatenate messtab-MSGtyp messtab-msgnr 'Measuring Document'
      messtab-msgv1 'Created' into l_message separated by space.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = messtab-msgid
                msgnr               = messtab-msgnr
                msgv1               = messtab-msgv1
                msgv2               = messtab-msgv2
                msgv3               = messtab-msgv3
                msgv4               = messtab-msgv4
           IMPORTING
                message_text_output = l_mess.
*     write :/ l_mess.
    endloop.


    write : /1(18) IT_DIE_MAINT-ZDIE_NUMBER,
             20(18) it_equi-EQUNR,
             40(50) l_message,
             95(80) l_mess .
clear : messtab[], messtab,l_message.
endloop.
*    endif.

    clear : messtab[], messtab,l_message.
  endloop.
