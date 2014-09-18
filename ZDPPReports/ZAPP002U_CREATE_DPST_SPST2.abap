************************************************************************
* Program Name      : ZAPP002U_CREATE_DPST_SPST2
* Author            : Byung Sung,Bae
* Creation Date     : 2003.11.27.
* Specifications By : Byung Sung,Bae
* Pattern           : Report 1-1
* Development Request No : UD1K904164
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZAPP002U_CREATE_DPST_SPST2 MESSAGE-ID zmpp.

*----- Global variables & Structures
data: wa_date   like   ztpp_dvrt1-rp01.                  "Working date

*----- Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-b01.
PARAMETERS: P_date LIKE sy-datum DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK bl1.

*----- Input value check
at selection-screen.
  perform check_p_date.

*----- Start of Selection
start-of-selection.

*EXEC SQL PERFORMING APPEND_ITAB.
*  SELECT A.PLNT, A.LINE, B.P_MODEL, B.P_BODY_SERIAL, B.P_SEQUENCE_DATE,
*         B.P_SEQ_SERIAL, B.PLNUM,
*       A.RP01, A.RP02, A.RP03, A.RP04, A.RP05, A.RP06, A.RP07, A.RP08,
*       A.RP09, A.RP10, A.RP11, A.RP12, A.RP13, A.RP14, A.RP15, A.RP16,
*         A.RP17, A.RP18,
*         B.COMP001,B.COMP002,B.COMP003,B.COMP004,B.COMP005,
*         B.COMP006,B.COMP007,B.COMP008,B.COMP009,B.COMP010,
*         B.COMP011,B.COMP012,B.COMP023,B.COMP014,B.COMP015,
*         B.COMP016,B.COMP017,B.COMP028,B.COMP019,B.COMP020,
*         B.COMP021,B.COMP022,B.COMP033,B.COMP024,B.COMP025,
*         B.COMP026,B.COMP027,B.COMP038,B.COMP029,B.COMP030,
*         B.COMP031,B.COMP032,B.COMP043,B.COMP034,B.COMP035,
*         B.COMP036,B.COMP037,B.COMP048,B.COMP039,B.COMP040,
*         B.COMP041,B.COMP042,B.COMP053,B.COMP044,B.COMP045,
*         B.COMP046,B.COMP047,B.COMP058,B.COMP049,B.COMP050,
*         B.COMP051,B.COMP052,B.COMP063,B.COMP054,B.COMP055,
*         B.COMP056,B.COMP057,B.COMP068,B.COMP059,B.COMP060,
*         B.COMP061,B.COMP062,B.COMP073,B.COMP064,B.COMP065,
*         B.COMP066,B.COMP067,B.COMP078,B.COMP069,B.COMP070,
*         B.COMP071,B.COMP072,B.COMP083,B.COMP074,B.COMP075,
*         B.COMP076,B.COMP077,B.COMP088,B.COMP079,B.COMP080,
*         B.COMP081,B.COMP082,B.COMP093,B.COMP084,B.COMP085,
*         B.COMP086,B.COMP087,B.COMP098,B.COMP089,B.COMP090,
*         B.COMP091,B.COMP092,B.COMP003,B.COMP094,B.COMP095,
*         B.COMP096,B.COMP097,B.COMP098,B.COMP099,B.COMP100,
*         B.COMP101,B.COMP102,B.COMP103,B.COMP104,B.COMP105,
*         B.COMP106,B.COMP107,B.COMP108,B.COMP109,B.COMP110,
*         B.COMP111,B.COMP112,B.COMP123,B.COMP114,B.COMP115,
*         B.COMP116,B.COMP117,B.COMP128,B.COMP119,B.COMP120,
*         B.COMP121,B.COMP122,B.COMP133,B.COMP124,B.COMP125,
*         B.COMP126,B.COMP127,B.COMP138,B.COMP129,B.COMP130,
*         B.COMP131,B.COMP132,B.COMP143,B.COMP134,B.COMP135,
*         B.COMP136,B.COMP137,B.COMP148,B.COMP139,B.COMP140,
*         B.COMP141,B.COMP142,B.COMP153,B.COMP144,B.COMP145,
*         B.COMP146,B.COMP147,B.COMP158,B.COMP149,B.COMP150,
*         B.COMP151,B.COMP152,B.COMP163,B.COMP154,B.COMP155,
*         B.COMP156,B.COMP157,B.COMP168,B.COMP159,B.COMP160,
*         B.COMP161,B.COMP162,B.COMP173,B.COMP164,B.COMP165,
*         B.COMP166,B.COMP167,B.COMP178,B.COMP169,B.COMP170,
*         B.COMP171,B.COMP172,B.COMP183,B.COMP174,B.COMP175,
*         B.COMP176,B.COMP177,B.COMP188,B.COMP179,B.COMP180
*    INTO
*    FROM ZTPP_DVRT1 A, ZTPP_VBOM B
*   WHERE A.MANDT         =    :SY-MANDT
*     AND A.STATUS        =    '00'
*     AND A.RP01          LIKE :WA_DATE
*     AND B.MANDT         =    A.MANDT
*     AND B.P_MODEL       =    A.MODL
*     AND B.P_BODY_SERIAL =    A.BODY_SER
*   UNION
*  SELECT
*    INTO
*    FROM ZTPP_DVRT1 C, ZTPP_VBOM D
*   WHERE C.MANDT         = :SY-MANDT
*     AND C.STATUS        between '00' AND '18'.
*     AND D.MANDT         = C.MANDT
*     AND D.P_MODEL       = C.MODL
*     AND D.P_BODY_SERIAL = C.BODY_SER
*
*ENDEXEC.
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form APPEND_ITAB.

endform.                    " APPEND_ITAB
*&---------------------------------------------------------------------*
*&      Form  check_p_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_p_date.
  concatenate p_date '%' into wa_date.
endform.                    " check_p_date
