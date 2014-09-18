*----------------------------------------------------------------------*
*   INCLUDE ZXCATU03                                                   *
*----------------------------------------------------------------------*
* Author            : Yongping Li
* Creation Date     : 09/14/2004
* Specifications By : Naveen
* Development Request No :UD1K914840
* Description       : Blocking the input of overtime during working time
*&--------------------------------------------------------------------&*
*& Date        User     Transport            Description
*& 10/11/2005  Shiva    UD1K917914        1.Commented the check for
*&                                        absence type '1020'.
*&                                        2. Added validation for
*&                                        personnel day off for
*&                                        new absence type '1006'.
*& 10/13/2005  Shiva    UD1K917942        convert hours to days for
*&                                        the validation.
*& 10/17/2005  Shiva    UD1K917982        use deduction value for
*&                                        days remaining calculaion.
*& 11/09/2005  Furong                     1. Chang type from 1006 to
*                                         1026.
*                                         2 check daily work schedule
*& 01/30/2006 Hassan   UD1K919110         1.Added validation for new
*                                          absence type 1021.
*& 03/13/2006 Hassan   UD1K919707         1.Added validation for new
*                                         absence type 1036.
*& 03/31/2006 Hassan   UD1K919414         1.Add validation for Costacct
*                                         and Timeadmn Profiles.
*& 05/30/2006 Hassan   UD1K920897         Add new code to block Managers
*                                         from being paid over time.Also
*                                         created a table ZHRJOBKEY.
*& 06/28/2006 Hassan   UD1K921218         Add new code for time profile
*                                         COSTACCT. Added vlidation for
*                                         A/A type 1055 and 1056.
*& 06/29/2006 Hassan                      Block new Absence code 1022
*&                                        from HMMA and COSTACCT Profil,
*                                         only allowed in TIMEADMN.
*& 07/26/2006 Hassan  UD1K921533          Added vlidation for A/A type
*                                         1061-1065 in Costacct profile.
*& 09/28/2006 Hassan  UD1K922316          BLOCK 1020-1023 FROM NON-EXEPT
* 10/04/2006  Manju   UD1K922381          Allow only half day or full
*                                         day increment for A/A type
*                                         1023/1024
* 10/05/2006  Manju   UD1K922414          Don't Validate during deletion
* 10/05/2006  Hassan  UD1K922414          Allow 1045 for U0 and U3 EE
* 10/23/2006  Hassan  UD1K922686          Allow 1046 for U2 EE
* 01/02/2007  Hassan  UD1K930169          Remove half a day from 1037 &
*                                         1038
* 02/27/2007  Furong  UD1K930898          Restrict OT entry
* 08/20/2008  ig.moon UD1K944356          allow 1033 ( military unpaid )
* 08/20/2010  Valerian UD1K949696         allow 1069 Attendance Type
*                                         only for Exempt TMs
* 09/21/2010  Valerian UD1K949815         Overtime check only if Public
*                                         holiday class = '0'.
* 09/09/2011  Valerian UD1K952938         Shift 1018 is not allowed to
*                                         enter overtime 1001
*&--------------------------------------------------------------------&*

*DATA: l_schkz TYPE T550A-TPROG.
*DATA: L_TEXT(40) TYPE C VALUE 'Do not enter over time during work time'
.
*DATA: IT_T550A TYPE TABLE OF T550A WITH HEADER LINE.
*DATA: IT_WORKSCHEDULE LIKE PTPSP OCCURS 0 WITH HEADER LINE.
*DATA: L_ERROR(30) TYPE C VALUE 'ERROR OCCURS ON FUNCTION CALL'.
*DATA: L_SCHEDULE_TYPE LIKE T550A-TPROG.
*DATA: L_RETURN LIKE SY-SUBRC.
*DATA: T_NO_SCHEDULE(30) TYPE C VALUE 'NO WORK SCHEDULE FOUND FOR '.
*DATA: T_TEXT1(30) TYPE C VALUE 'Please Enter a time Period!'.
*DATA: L_MOTPR LIKE T550A-MOTPR.
*
*if     fields-catshours <> '00.00'  and
*       fields-beguz     eq space and
*       fields-enduz     eq space.
*  message e099(zz).
*endif.
*
****************************************************
***CHECK THE WORK SCHEDULE TYPE BY FUNCTION
***************************************************
**IF FIELDS-AWART = '1001'.
*CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
*     EXPORTING
*          PERNR         = FIELDS-PERNR
*          BEGDA         = FIELDS-WORKDATE
*          ENDDA         = FIELDS-WORKDATE
*     IMPORTING
*          WARNING_OCCURED = L_RETURN
*     TABLES
*         PERWS          = IT_WORKSCHEDULE.
*
* L_MOTPR  = IT_WORKSCHEDULE-MOTPR.
*
* IF SY-SUBRC <> 0.
*    MESSAGE E000(ZZ) WITH L_ERROR.
* ELSEIF ( L_MOTPR = '09' AND ( FIELDS-AWART = '1001' OR
*                               FIELDS-AWART = '1002' OR
*                               FIELDS-AWART = '1003' OR
*                               FIELDS-AWART = '1004' OR
*                               FIELDS-AWART = '1005'   ) ) OR
*        ( L_MOTPR = '10' AND ( FIELDS-AWART = '1001' OR
*                               FIELDS-AWART = '1002'   ) ).
*    READ TABLE IT_WORKSCHEDULE WITH KEY DATUM = FIELDS-WORKDATE.
*    IF SY-SUBRC <> 0.
*      MESSAGE E000(ZZ) WITH T_NO_SCHEDULE FIELDS-PERNR.
*    ENDIF.
*    L_SCHEDULE_TYPE = IT_WORKSCHEDULE-TPROG.
*
*    IF L_SCHEDULE_TYPE <> '1008'. " Holiday: no restriction
**   READ THE WORK SCHEDULE TIME
*    SELECT * Into TABLE IT_T550A
*      FROM T550A
*       WHERE TPROG EQ L_SCHEDULE_TYPE and
*             MOTPR EQ IT_WORKSCHEDULE-MOTPR AND
*             VARIA EQ SPACE.
*
*    IF SY-SUBRC EQ 0.
*      READ TABLE IT_T550A INTO IT_T550A INDEX 1.
*
*       IF IT_T550A-SOEND GT IT_T550A-SOBEG.
*         IF ( FIELDS-BEGUZ GT IT_T550A-SOBEG
*            AND FIELDS-BEGUZ LT IT_T550A-SOEND ) OR
*           ( FIELDS-ENDUZ GT IT_T550A-SOBEG
*            AND FIELDS-ENDUZ LT IT_T550A-SOEND ) OR
*            ( FIELDS-BEGUZ LE IT_T550A-SOBEG
*             AND FIELDS-ENDUZ GE IT_T550A-SOEND
*             AND FIELDS-ENDUZ GT FIELDS-BEGUZ ) OR
*            ( FIELDS-ENDUZ LT FIELDS-BEGUZ
*              AND FIELDS-BEGUZ LT IT_T550A-SOBEG ).
*
*           MESSAGE E000(ZZ)  WITH L_TEXT.
*
*         ENDIF.
*       ELSE.
*         IF ( ( FIELDS-BEGUZ GT IT_T550A-SOBEG
*             AND FIELDS-BEGUZ LE '240000' )
*            OR FIELDS-BEGUZ LT IT_T550A-SOEND ) OR
*           ( ( FIELDS-ENDUZ GT IT_T550A-SOBEG
*             AND FIELDS-ENDUZ LE '240000' )
*            OR FIELDS-ENDUZ LT IT_T550A-SOEND ) OR
*           ( FIELDS-BEGUZ LE IT_T550A-SOBEG
*             AND FIELDS-ENDUZ GE IT_T550A-SOEND
*             AND FIELDS-BEGUZ GT FIELDS-ENDUZ ).
*
*            MESSAGE E000(ZZ)  WITH L_TEXT.
*         ENDIF.
*
*       ENDIF.   " IT_T550A-SOEND GT IT_T550A-SOBEG
*
*    ENDIF.    "SY-SUBRC EQ 0 ON LINE 52
*   ENDIF.   " L_WORKSCHEDULE_TYPE = '1008'
* ENDIF.    " SY-SUBRC = 0 ON LINE 35
*
** ENDIF.     " FIELDS-AWART = '1001'

* Don't allow user enter the following absence type
* 1027 1031 1032 1033 1034 1035 1037 1038
TABLES: zhrjobkey.
TABLES: zptimepm.
TABLES: catsdb.
TABLES:catsfields.
DATA: lt_513s LIKE t513s OCCURS 0 WITH HEADER LINE.
DATA: lt_p0001 LIKE pa0001 OCCURS 0 WITH HEADER LINE.

DATA: l_text(50) TYPE c.
DATA: BEGIN OF wa_pa2006,
        pernr LIKE pa2006-pernr,
        ktart LIKE pa2006-ktart,
        anzhl LIKE pa2006-anzhl,
        kverb LIKE pa2006-kverb,
      END OF wa_pa2006.
DATA: it_pa2006 LIKE TABLE OF wa_pa2006.

DATA: it_t551a LIKE t551a.
DATA: w_awart LIKE cats_comm-awart,
      w_pernr LIKE cats_comm-pernr,
      w_anzhl LIKE cats_comm-catshours,
      w_rkostl LIKE cats_comm-rkostl,
      w_days  TYPE p DECIMALS 2,
      w_remn  TYPE p DECIMALS 2,
      w_workdate LIKE cats_comm-workdate,
      w_stell LIKE pa0001-stell,
      w_begda LIKE zptimepm-begda,
*      w_begda like cats_comm-beguz,
      s_awart LIKE cats_comm-awart,
      s_stell LIKE pa0001-stell,
      s_persk LIKE pa0001-persk,
      s_begda LIKE pa0001-begda,
      s_subty LIKE pa2001-awart,
      l_day(1),
      l_field_name(15),
      l_tprog LIKE t551a-tprg1,
      l_arbst LIKE pa0007-arbst,
      l_schkz LIKE pa0007-schkz.

DATA: w_fistl LIKE fmzuob-fistl,
      w_wbs(24) TYPE c,
      w_acct(24) TYPE c,
      l_incr TYPE pa0007-arbst,
      w_continue TYPE boolean.

DATA: it_0000 TYPE TABLE OF p0000,
      it_0001 TYPE TABLE OF p0001,
      wa_0001 TYPE p0001,
*      WA_1011 TYPE HRP_1011,
*      W_BEGDA(10) TYPE C,
      w_endda(10) TYPE c,
      w_plan(8)   TYPE c.

****************************************************************
*INTERNAL TABLES
****************************************************************
DATA: BEGIN OF it_dis OCCURS 0,
*  pernr       like pa0001-pernr,
  stell       LIKE pa0001-stell,
*  persk       like pa0001-persk,
  begda       LIKE pa0002-begda,
  endda       LIKE pa0002-endda,
  END OF it_dis.

DATA: BEGIN OF rt_dis OCCURS 0,
*  pernr       like pa0001-pernr,
*  stell       LIKE pa0001-stell,
  persk       LIKE pa0001-persk,
  subty       LIKE pa2001-awart,
  begda       LIKE zptimepm-begda,
  endda       LIKE pa0002-endda,
  END OF rt_dis.

DATA: BEGIN OF st_dis OCCURS 0,
*  pernr       like pa0001-pernr,
*  stell       LIKE pa0001-stell,
  persk       LIKE pa0001-persk,
  subty       LIKE pa2001-awart,
  begda       LIKE zptimeam-begda,
  endda       LIKE pa0002-endda,
  END OF st_dis.

**************************************************************

INFOTYPES: 0000,
           0001,
           0007,
           0041,
           2001 MODE n,                "Absences
           2002,
           2003 MODE n,                "Substitution
           2004 MODE n,                "On-Call Duty
           2005 MODE n.                "Overtime

* { by ig.moon 11/18/2009
DATA: BEGIN OF PSP OCCURS 33.
        INCLUDE STRUCTURE PC2BA.
DATA: END OF PSP.
* }
data rc type i.
* }


*DATA: BEGIN OF lt_p0001 OCCURS 0,
*        pernr  LIKE pa0001-pernr,
*        stell  LIKE zhrjobkey-stell,
*        begda  LIKE pa0001-begda,
*        endda  like pa0001-endda,
*        messg(80) TYPE c,
*      END OF lt_p0001.

DATA: BEGIN OF lt_zhmanaovert OCCURS 0,
         pernr  LIKE pa0001-pernr,
         stell  LIKE zhrjobkey-stell,
         begda  LIKE pa0001-begda,
         endda  LIKE pa0001-endda,
         messg(80) TYPE c,
       END OF lt_zhmanaovert.

DATA $tprog TYPE  tprog.
DATA: w_schkz LIKE pa0007-schkz.
DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.

*********************************************************
* Commented out codes exit in A/A list. Since they are
* already delemited in IMG, we do not block them here.
*********************************************************
FIELD-SYMBOLS: <l_field>.
IF sap_tcats-variant = 'HMMA' AND
 ( fields-awart = '1002' OR
*      FIELDS-AWART = '1020' or
   fields-awart = '1021' OR
   fields-awart = '1022' OR
   fields-awart = '1027' OR
   fields-awart = '1031' OR
   fields-awart = '1032' OR

* disbled by ig.moon 08/20/2008 {
*   fields-awart = '1033' OR
* }

   fields-awart = '1034' OR
   fields-awart = '1035' OR
   fields-awart = '1036' OR
   fields-awart = '1037' OR
   fields-awart = '1038' OR
   fields-awart = '1041' OR
   fields-awart = '1042' OR
   fields-awart = '1043' OR
   fields-awart = '1044' OR
*   fields-awart = '1045' OR
*   fields-awart = '1046' OR
   fields-awart = '1047' OR
   fields-awart = '1048' OR
   fields-awart = '1049' OR
   fields-awart = '1051' OR
   fields-awart = '1055' OR
   fields-awart = '1056' OR
   fields-awart = 'PE10' OR
   fields-awart = 'PE20' OR
   fields-awart = 'PE30' OR
*   fields-awart = 'PE31' OR
*   fields-awart = 'PE32' OR
*   fields-awart = 'PE33' OR
   fields-awart = 'PE40' OR
*   fields-awart = 'PE41' OR
*   fields-awart = 'PE42' OR
*   fields-awart = 'PE43' OR
   fields-awart = 'PE50' OR
   fields-awart = 'PE60' OR
   fields-awart = 'PE70' OR
   fields-awart = 'UEA0' OR
   fields-awart = 'UEB0' OR
*   fields-awart = 'UEB1' OR
*   fields-awart = 'UEB2' OR
*   fields-awart = 'UEB3' OR
*   fields-awart = 'UEB4' OR
   fields-awart = 'UEC0' OR
*   fields-awart = 'UEC1' OR
*   fields-awart = 'UEC2' OR
*   fields-awart = 'UEC3' OR
*   fields-awart = 'UEC4' OR
*   fields-awart = 'UEC5' OR
   fields-awart = 'UED0' OR
   fields-awart = 'UEE0' OR
   fields-awart = 'UEF0' OR
   fields-awart = 'UEG0' OR
   fields-awart = 'UEH0' OR
*   fields-awart = 'UEH1' OR
*   fields-awart = 'UEH2' OR
*   fields-awart = 'UEH3' OR
   fields-awart = 'UEI0' OR
   fields-awart = 'UEJ0' OR
   fields-awart = 'UEK0' OR
   fields-awart = 'UEL0' OR
   fields-awart = 'UEM0' OR
   fields-awart = 'UEN0' OR
   fields-awart = 'UEO0' OR
*   fields-awart = 'UEO1' OR
*   fields-awart = 'UEO2' OR
   fields-awart = 'UEP0' OR
*   fields-awart = 'UEP1' OR
*   fields-awart = 'UEP2' OR
*   fields-awart = 'UEP3' OR
*   fields-awart = 'UEP4' OR
   fields-awart = 'UEQ0' OR
   fields-awart = 'UER0' OR
   fields-awart = 'UES0' OR
   fields-awart = 'UES1' ).

  MESSAGE e001(zmhr) WITH text-001.

ENDIF.

******************************************

IF sap_tcats-variant = 'HMMA'
AND
 ( fields-awart = '1033' OR
   fields-awart = '1017' OR
   fields-awart = '1057' OR
   fields-awart = '1058' ).

  MESSAGE e001(zmhr) WITH text-005.

ENDIF.

FIELD-SYMBOLS: <m_field>.
IF sap_tcats-variant = 'TIMEADMN' AND
 ( fields-awart = '1055' OR
   fields-awart = '1056' OR
   fields-awart = 'PE10' OR
   fields-awart = 'PE20' OR
   fields-awart = 'PE30' OR
*   fields-awart = 'PE31' OR
*   fields-awart = 'PE32' OR
*   fields-awart = 'PE33' OR
   fields-awart = 'PE40' OR
*   fields-awart = 'PE41' OR
*   fields-awart = 'PE42' OR
*   fields-awart = 'PE43' OR
   fields-awart = 'PE50' OR
   fields-awart = 'PE60' OR
   fields-awart = 'PE70' OR
   fields-awart = 'UEA0' OR
   fields-awart = 'UEB0' OR
*   fields-awart = 'UEB1' OR
*   fields-awart = 'UEB2' OR
*   fields-awart = 'UEB3' OR
*   fields-awart = 'UEB4' OR
   fields-awart = 'UEC0' OR
*   fields-awart = 'UEC1' OR
*   fields-awart = 'UEC2' OR
*   fields-awart = 'UEC3' OR
*   fields-awart = 'UEC4' OR
*   fields-awart = 'UEC5' OR
   fields-awart = 'UED0' OR
   fields-awart = 'UEE0' OR
   fields-awart = 'UEF0' OR
   fields-awart = 'UEG0' OR
   fields-awart = 'UEH0' OR
*   fields-awart = 'UEH1' OR
*   fields-awart = 'UEH2' OR
*   fields-awart = 'UEH3' OR
   fields-awart = 'UEI0' OR
   fields-awart = 'UEJ0' OR
   fields-awart = 'UEK0' OR
   fields-awart = 'UEL0' OR
   fields-awart = 'UEM0' OR
   fields-awart = 'UEN0' OR
   fields-awart = 'UEO0' OR
*   fields-awart = 'UEO1' OR
*   fields-awart = 'UEO2' OR
   fields-awart = 'UEP0' OR
*   fields-awart = 'UEP1' OR
*   fields-awart = 'UEP2' OR
*   fields-awart = 'UEP3' OR
*   fields-awart = 'UEP4' OR
   fields-awart = 'UEQ0' OR
   fields-awart = 'UER0' OR
   fields-awart = 'UES0' OR
   fields-awart = 'UES1' ).

  MESSAGE e001(zmhr) WITH text-001.

ENDIF.
******************************************
*requirements by Ahmar Khan for
*Labor allocation
FIELD-SYMBOLS: <n_field>.
IF sap_tcats-variant = 'COSTACCT' AND
 ( fields-awart = '1001' OR
   fields-awart = '1002' OR
   fields-awart = '1003' OR
   fields-awart = '1004' OR
   fields-awart = '1005' OR
   fields-awart = '1018' OR
   fields-awart = '1019' OR
   fields-awart = '1020' OR
   fields-awart = '1021' OR
   fields-awart = '1022' OR
   fields-awart = '1023' OR
   fields-awart = '1024' OR
   fields-awart = '1025' OR
   fields-awart = '1026' OR
   fields-awart = '1027' OR
   fields-awart = '1029' OR
   fields-awart = '1030' OR
   fields-awart = '1031' OR
   fields-awart = '1032' OR
   fields-awart = '1033' OR
   fields-awart = '1034' OR
   fields-awart = '1035' OR
   fields-awart = '1036' OR
   fields-awart = '1037' OR
   fields-awart = '1038' OR
   fields-awart = '1041' OR
   fields-awart = '1042' OR
   fields-awart = '1043' OR
   fields-awart = '1044' OR
   fields-awart = '1045' OR
   fields-awart = '1046' OR
   fields-awart = '1047' OR
   fields-awart = '1048' OR
   fields-awart = '1049' OR
   fields-awart = '1050' OR
   fields-awart = '1051' OR
   fields-awart = '1052' OR
   fields-awart = '1053' OR
   fields-awart = '2000' OR
   fields-awart = '1061' OR
   fields-awart = '1062' OR
   fields-awart = '1063' OR
   fields-awart = '1064' OR
   fields-awart = '1065' OR                                 "UD1K949696
   fields-awart = '1069' ).                                 "UD1K949696
*  fields-awart = '1055' OR
*  fields-awart = '1056' ).
*
  MESSAGE e022(zmhr) .

ENDIF.

IF
  sap_tcats-variant = 'COSTACCT' AND
  fields-rkostl EQ space .
  MESSAGE e021(zmhr) .
ENDIF.


******************************************************

w_awart = fields-awart.
w_pernr = fields-pernr.
*w_BEGDA   = FIELDS-WORKDATE.
*w_begda = zptimepm-begda.
w_anzhl = fields-catshours.
w_workdate = fields-workdate.

*********************************************************
* Add by Hassan to Block Manager's Over Time in CAT2

**********************************************************
CLEAR: it_dis, it_dis[].

IF sy-ucomm NE 'DELE'.


  SELECT SINGLE stell INTO s_stell  FROM pa0001
         WHERE pernr = w_pernr AND
         begda <= sy-datum AND
         endda >= sy-datum .

  SELECT stell INTO TABLE it_dis
             FROM zhrjobkey.

  READ TABLE it_dis WITH KEY stell = s_stell.
  IF sy-subrc = 0 AND
  ( w_awart = '1001' OR
     w_awart = '1003' OR
     w_awart = '1004' OR
     w_awart = '1005' OR
     w_awart = '1006' OR
     w_awart = '1007' ).


    MESSAGE e020(zmhr).

  ENDIF.
*****************************************************************
** changed by Hassan on 09/28/06 requested by Ahmar
** change to block 1020-1023 attendance for exempt EE
***********************************************************
  CLEAR: rt_dis, rt_dis[].

*if sy-ucomm ne 'DELE'.                                      "UD1K922414

  SELECT SINGLE persk begda INTO (s_persk,s_begda)
         FROM pa0001
         WHERE pernr = w_pernr AND
               begda <= sy-datum AND
               endda >= sy-datum .

  SELECT persk subty begda endda INTO TABLE rt_dis
          FROM zptimepm.

  READ TABLE rt_dis WITH KEY persk = s_persk.
*                           subty = s_subty.

** Changed by Furong     >> UD1K930985
*  IF sy-subrc <> 0 AND
*     s_begda > rt_dis-begda AND
*      (  fields-awart = '1020' OR
*         fields-awart = '1021' OR
*         fields-awart = '1022' OR
*         fields-awart = '1058' OR
*         fields-awart = '1046' ) .   "<<<UD1K922686

  IF sy-subrc <> 0 AND
     s_begda > rt_dis-begda AND
      (  fields-awart = '1020' OR
         fields-awart = '1021' OR
         fields-awart = '1022' OR
         fields-awart = '1069' OR                           "UD1K949696
         fields-awart = '1046' ) .   "<<<UD1K922686

** End of change
    MESSAGE e018(zmhr).
*  endif.
  ENDIF.

****************************************************************
  CLEAR: st_dis, st_dis[].
***
  SELECT SINGLE persk begda INTO (s_persk,s_begda)
         FROM pa0001
         WHERE pernr = w_pernr AND
               begda <= sy-datum AND
               endda >= sy-datum .

  SELECT persk subty begda endda INTO TABLE st_dis
          FROM zptimeam.

  READ TABLE st_dis WITH KEY persk = s_persk.

  IF sy-subrc <> 0 AND
     s_begda > st_dis-begda AND

   ( fields-awart EQ '1064' OR
     fields-awart EQ '1045' ).   "<<<UD1K922414

    MESSAGE e019(zmhr) .

  ENDIF.
ENDIF.
*endif.


*****************************************************************
** changed by Furong on 11/08/05 requested by Ahmar
**   change from attendance type 1006 to 1026
*if w_awart eq '1006'.

*IF w_awart EQ '1026'.
*
*** end of change
**  IMPORT it_pa2006 FROM MEMORY ID 'itab1'.
**  IF sy-subrc NE 0.
*    SELECT pernr ktart anzhl kverb FROM pa2006
*                             INTO TABLE it_pa2006
*                             WHERE ( ktart = '10'      AND
*                                     endda GE sy-datum AND
*                                     begda LE sy-datum )
*                             OR    ( ktart = '13'      AND
*                                     endda GE sy-datum AND
*                                     begda LE sy-datum )
*                        ORDER BY pernr ktart.
**    EXPORT it_pa2006 TO MEMORY ID 'itab1'.
**  ENDIF.

CASE w_awart.
*    when '1023'.
*      read table it_pa2006 into wa_pa2006 with key pernr = w_pernr
*                                                   ktart = '13'
*                                                   binary search.
*      if w_anzhl gt wa_pa2006-anzhl.
*        message E001(zmhr) with text-003.
*      endif.


** changed by Furong on 11/08/05 requested by Ahmar
**   change from attendance type 1006 to 1026
*    when '1006'.

************************************************************************
*In case Work Schedule Rule is '4001', Absence code '1026','1019',
* '2000',*'2001' allow to enter for Tue,Wed and Thursday, which are
*their weekend.
************************************************************************
  WHEN '1026' OR '1019' OR '2000' OR '2001'.

    IF w_anzhl > 0.

      SELECT pernr ktart anzhl kverb FROM pa2006
                               INTO TABLE it_pa2006
                               WHERE pernr EQ w_pernr
                               AND ( ( ktart = '10'      AND
                                     endda GE sy-datum AND
                                     begda LE sy-datum ) OR
                                   ( ktart = '13'      AND
                                     endda GE sy-datum AND
                                     begda LE sy-datum ) )
                          ORDER BY pernr ktart.

      IF sy-subrc NE 0.
      ELSE.


* holiday

        clear : iholiday[], iholiday.
        CALL FUNCTION 'HOLIDAY_GET'
             EXPORTING
                  holiday_calendar = 'U1'
                  date_from        = w_workdate
                  date_to          = w_workdate
             TABLES
                  holidays         = iholiday.



        SELECT SINGLE arbst schkz INTO (l_arbst, l_schkz)
               FROM pa0007
               WHERE pernr = w_pernr
                 AND begda <= w_workdate
                 AND endda >= w_workdate.

        CALL FUNCTION 'DATE_COMPUTE_DAY'
             EXPORTING
                  date = w_workdate
             IMPORTING
                  day  = l_day.

        IF ( l_day EQ '5' OR l_day EQ '6' OR
           l_day EQ '7' OR l_day EQ '1' ) AND ( l_schkz EQ '4001' ).

          read table iholiday index 1.
          if sy-subrc eq 0 and iholiday-HOLIDAY eq 'X'.
          else.
            MESSAGE e001(zmhr) WITH text-004.
          endif.
        ENDIF.

        w_schkz = l_schkz.

*        CALL FUNCTION 'Z_CO_GET_DWS_IG'
*             EXPORTING
*                  schkz                          = w_schkz
*                  datum                          = fields-workdate
*             IMPORTING
*                  tprog                          = $tprog
*             EXCEPTIONS
*                  not_found_work_schedule_rules  = 1
*                  invalid_date                   = 2
*                  not_found_period_work_schedule = 3
*                  OTHERS                         = 4.
*
*        IF sy-subrc <> 0.
*          $tprog = w_schkz.
*        ENDIF.

* logic for checking daily schedule start {
*
        CALL FUNCTION 'HR_READ_INFOTYPE'
             EXPORTING
                  pernr           = fields-pernr
                  infty           = '0000'
                  begda           = fields-workdate
                  endda           = fields-workdate
             IMPORTING
                  subrc           = rc
             TABLES
                  infty_tab       = p0000
             EXCEPTIONS
                  infty_not_found = 1
                  OTHERS          = 2.

        CALL FUNCTION 'HR_READ_INFOTYPE'
             EXPORTING
                  pernr           = fields-pernr
                  infty           = '0001'
                  begda           = fields-workdate
                  endda           = fields-workdate
             IMPORTING
                  subrc           = rc
             TABLES
                  infty_tab       = p0001
             EXCEPTIONS
                  infty_not_found = 1
                  OTHERS          = 2.

        CALL FUNCTION 'HR_READ_INFOTYPE'
             EXPORTING
                  pernr           = fields-pernr
                  infty           = '0007'
                  begda           = fields-workdate
                  endda           = fields-workdate
             IMPORTING
                  subrc           = rc
             TABLES
                  infty_tab       = p0007
             EXCEPTIONS
                  infty_not_found = 1
                  OTHERS          = 2.

        CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
             EXPORTING
                  pernr             = fields-pernr
                  begda             = fields-workdate
                  endda             = fields-workdate
                  switch_activ      = 1
                  i0001_i0007_error = '0'
                  read_cluster      = 'X'
             TABLES
                  i0000             = p0000
                  i0001             = p0001
                  i0007             = p0007
                  perws             = psp
             EXCEPTIONS
                  error_occured     = 1
                  abort_occured     = 2
                  OTHERS            = 3.
        IF sy-subrc <> 0.
        ENDIF.

        $tprog = psp-tprog.
* }

*        SELECT SINGLE * INTO it_t551a FROM t551a
*               WHERE zmodn = l_schkz.
*
*        IF sy-subrc EQ 0.
*          CONCATENATE 'IT_T551A-TPRG' l_day INTO l_field_name.
*          ASSIGN (l_field_name) TO <l_field>.

*          l_tprog = <l_field>.

        l_tprog = $tprog.

        read table iholiday index 1.
        if sy-subrc eq 0 and iholiday-HOLIDAY eq 'X'.
        else.
          IF l_tprog <> '1008' and l_tprog <> '1009'
          and l_tprog <> '1011' and l_tprog <> '1007'.
            MESSAGE e001(zmhr) WITH text-006.
          ENDIF.
        endif.
*        ENDIF.

        IF old_data <> 'X'.


          IF l_arbst > 0.
            w_days = w_anzhl / l_arbst.
          ELSE.
            w_days = w_anzhl / 8.
          ENDIF.

          IF w_awart EQ '1026' OR w_awart EQ '1019'.

            READ TABLE it_pa2006 INTO wa_pa2006
            WITH KEY pernr = w_pernr
                     ktart = '10'
                    BINARY SEARCH.

            w_remn = wa_pa2006-anzhl - wa_pa2006-kverb.
            IF w_days GT w_remn.
              MESSAGE e001(zmhr) WITH text-002.
            ENDIF.

*          ELSEIF w_awart EQ '2000' OR w_awart EQ '2001'.
*           READ TABLE it_pa2006 INTO wa_pa2006 WITH KEY pernr = w_pernr
*                                                         ktart = '13'
*                                                         BINARY SEARCH.
          ENDIF.

*          break-point.
*          w_remn = wa_pa2006-anzhl - wa_pa2006-kverb.
*          IF w_days GT w_remn
*               and wa_pa2006-ktart = '10'.
*            MESSAGE e001(zmhr) WITH text-002.
*          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

ENDCASE.
*ENDIF.

*break-point.

* Begin of  changes -  UD1K922381
IF fields-awart EQ '1018' OR
 fields-awart EQ '1019' OR
 fields-awart EQ '1023' OR
 fields-awart EQ '1024' OR
 fields-awart EQ '1069' OR                                  "UD1K949696
 fields-awart EQ '1026' .
*   fields-awart eq '1037' OR
*   fields-awart eq '1038' .

  IF sy-ucomm NE 'DELE'.                                    "UD1K922414

    SELECT SINGLE arbst INTO l_arbst
           FROM pa0007
           WHERE pernr = fields-pernr
             AND begda <= fields-workdate
             AND endda >= fields-workdate.
    IF sy-subrc EQ 0 AND fields-catshours > 0.

      l_incr = l_arbst / 2.  " Half day
      IF NOT ( ( fields-catshours EQ l_arbst )  OR
               ( fields-catshours EQ l_incr ) ).
        MESSAGE e017(zmhr) .
      ENDIF.
    ENDIF.
  ENDIF.

ENDIF.
* End of changes -  UD1K922381
CLEAR: wa_pa2006, w_awart, w_pernr, w_anzhl, w_days, w_remn.

* Added by Furong on 02/27/2007 Requested by Ahmar
* Request no: UD1K930898
* Help desk ticket: 7389586A77

* logic for checking daily schedule start {
CALL FUNCTION 'HR_READ_INFOTYPE'
     EXPORTING
          pernr           = fields-pernr
          infty           = '0000'
          begda           = fields-workdate
          endda           = fields-workdate
     IMPORTING
          subrc           = rc
     TABLES
          infty_tab       = p0000
     EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

CALL FUNCTION 'HR_READ_INFOTYPE'
     EXPORTING
          pernr           = fields-pernr
          infty           = '0001'
          begda           = fields-workdate
          endda           = fields-workdate
     IMPORTING
          subrc           = rc
     TABLES
          infty_tab       = p0001
     EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

CALL FUNCTION 'HR_READ_INFOTYPE'
     EXPORTING
          pernr           = fields-pernr
          infty           = '0007'
          begda           = fields-workdate
          endda           = fields-workdate
     IMPORTING
          subrc           = rc
     TABLES
          infty_tab       = p0007
     EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
     EXPORTING
          pernr             = fields-pernr
          begda             = fields-workdate
          endda             = fields-workdate
          switch_activ      = 1
          i0001_i0007_error = '0'
          read_cluster      = 'X'
     TABLES
          i0000             = p0000
          i0001             = p0001
          i0007             = p0007
          perws             = psp
     EXCEPTIONS
          error_occured     = 1
          abort_occured     = 2
          OTHERS            = 3.
IF sy-subrc <> 0.
ENDIF.

$tprog = psp-tprog.
* }

*SELECT SINGLE schkz INTO w_schkz
*     FROM pa0007
*     WHERE pernr = fields-pernr
*       AND begda <= fields-workdate
*       AND endda >= fields-workdate.
*
*IF sy-subrc EQ 0.

******  IF w_schkz = '2006'.
********                      >>UD1K931032
******  ELSE.
******    IF w_schkz = '1000' OR w_schkz = '1001' OR
******       w_schkz = '1004' OR w_schkz = '2001' OR
******       w_schkz = '2004' OR w_schkz = '4001'
******** Changed by Furong on 01/08/09
******       or w_schkz = '2009_01'.
******** end of change
******      IF fields-awart = '1003' OR fields-awart = '1004' OR
******         fields-awart = '1006' OR fields-awart = '1007'.
******        MESSAGE e024(zmhr).
******      ENDIF.
******    ELSE.
******      IF  w_schkz = '1008' OR  w_schkz = '1010'.
******      ELSE.
******        IF fields-awart = '1001' OR fields-awart = '1005'.
******          MESSAGE e024(zmhr).
******        ENDIF.
******      ENDIF.
******    ENDIF.
******  ENDIF.

*  CALL FUNCTION 'Z_CO_GET_DWS_IG'
*       EXPORTING
*            schkz                          = w_schkz
*            datum                          = fields-workdate
*       IMPORTING
*            tprog                          = $tprog
*       EXCEPTIONS
*            not_found_work_schedule_rules  = 1
*            invalid_date                   = 2
*            not_found_period_work_schedule = 3
*            OTHERS                         = 4.
*
*  IF sy-subrc <> 0.
*    $tprog = w_schkz.
*  ENDIF.

* Overtime check for 1st shift
IF psp-ftkla = '0'.                                         "UD1K949815
  IF $tprog = '0001' OR $tprog = '1010' OR
     $tprog = '1011' OR $tprog = '1016' OR
     $tprog = '1000' OR $tprog = '1001' OR $tprog = '1008' OR
     $tprog = '3001' OR $tprog = '3002' OR $tprog = '2004' OR
     $tprog = '1018'.                                       "UD1K952938

    IF fields-awart = '1003' OR fields-awart = '1004' OR
       fields-awart = '1006' OR fields-awart = '1007'.
       IF OLD_DATA NE 'X'.
      MESSAGE e024(zmhr).
       ENDIF.
    ENDIF.
  ELSE.
    if $tprog eq '1007'.
    else.
* Overtime check for 2nd shift
      IF fields-awart = '1001' OR fields-awart = '1005'.
        MESSAGE e024(zmhr).
      ENDIF.
    endif.
  ENDIF.
ENDIF.                                                      "UD1K949815
*ENDIF.
