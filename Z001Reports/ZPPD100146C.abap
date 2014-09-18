* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR PD Individual Infotype Load
* Version 1.0  - August 2000

* PD/Org Infotype 1001
* Authors : Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPPD100146C MESSAGE-ID ZP.

* SELECTION SCREEN
** PARAMETER


TABLES : T77AR.
PARAMETER : FILE1001 LIKE  RLGRAP-FILENAME DEFAULT
             'C:\WINDOWS\SAP\1001.txt' .


* data decleration
** Tables

** internal tables

DATA : BEGIN OF _P1001 OCCURS 10.
        INCLUDE STRUCTURE P1001.
DATA : MANZL1(3),MANZL2(3),MANZL3(3),MANZL4(3),MANZL5(3),SHORT(12),
       INHRT(1),DISPO(5),SEMIN(1),TEILN(1),TAGNR(3),ZBLID(8),BEGUZ(6),
       ENDUZ(6),RESID(15),KKOST(16),KWAER(5),KOKRS(4),KOSTL(10),
      BEZEK(30),BELNR(10),ZDATA(50),CHARA(4),EXPER(2),APSTV(1),STREA(2),
      STEXT(40),EVATY(4),CRITX(25),JVALU(2),BLCID(2),
      BEGUZ1(6),ENDUZ1(6), BEDART(10),DIENST(2),
  ANZCL(5), REPPR(10),ACTIVE(1),GENERATE(1),EXCLUDED(1),FUNC_AREA(20),
    GTIME(6),MESTYP(30),PROZT1(5),ZUSATZ(1),VERKOBJ(1),LANGU(1),
    BEGDA1(10),ENDDA1(10), BUDAT(10), SUBTY(4), DPATT(40) .

DATA : END OF _P1001.

DATA: BEGIN OF BDC_DATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

** Data
DATA : TRCODE LIKE TSTC-TCODE.
DATA  DELIMITER TYPE X VALUE '09' .
DATA  CNT TYPE I VALUE 0.

* Source Code

PERFORM READ_DATA.
PERFORM INIT_BDC USING 'HRPD1001' SY-UNAME.
LOOP AT _P1001.
  PERFORM POPULATE_BDC.
*  perform determine_tcode changing trcode _p1001-otype.
  PERFORM INSERT_BDC TABLES BDC_DATA USING 'PP02'.
  CNT = CNT + 1.
ENDLOOP.
PERFORM CLOSE_PROGRAM.


** FORMS

*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_BDC.

  PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'SAPMH5A0' '1000' ' '         ,
                         ' ' 'PPHDR-OTYPE' _P1001-OTYPE ' ', "OBJ TYPE
                         ' ' 'PPHDR-PLVAR' '01' ' '        , "PLAN VERSN
                         ' ' 'PM0D1-SEARK' _P1001-OBJID ' ', "OBJ ID
                         ' ' 'PPHDR-INFTY' '1001'       ' ', "INFOTYPE
                         ' ' 'PPHDR-SUBTY' _P1001-SUBTY ' ', "SUBTYPE
                         ' ' 'PPHDR-ISTAT' '1' ' ',
                         ' ' 'PPHDR-BEGDA' _P1001-BEGDA1 ' ', "BEGIN DT
                         ' ' 'PPHDR-ENDDA' _P1001-ENDDA1 ' ', "END DT
                         ' ' 'PM0D1-DPATT' _P1001-DPATT ' ' , "DATA SMPL
                         ' ' 'BDC_OKCODE' '/05' ' '        . "CREATE-F5
  PERFORM DYNPRO TABLES BDC_DATA USING:
                         'X' 'MP100100' '2000' ' ',
                         ' ' 'P1001-RSIGN' _P1001-RSIGN ' ',
                         ' ' 'P1001-RELAT' _P1001-RELAT ' ',
                         ' ' 'P1001-SCLAS' _P1001-SCLAS ' ',
                         ' ' 'P1001-SOBID' _P1001-SOBID ' ',
                         ' ' 'P1001-PRIOX' _P1001-PRIOX ' ',
                         ' ' 'P1001-PROZT' _P1001-PROZT1 ' ',
                         ' ' 'BDC_OKCODE' '/11' ' '        .


* To decide which subscreen will appear check - t77ar  - Hemang
* Also if object type is K , subscreen 5010 will appear...

  SELECT SINGLE  * FROM T77AR WHERE RELAT = _P1001-RELAT.
  IF SY-SUBRC EQ 0 .
    CASE T77AR-DYNNR .
      when '4021' .
        PERFORM DYNPRO TABLES BDC_DATA USING:
                    'X' 'MP100100' '4021' ' '        ,
                    ' ' 'PAD21-MANZL' _P1001-MANZL1 ' ',
                    ' ' 'BDC_OKCODE' '/11' ' '       .

      WHEN '4022'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4022' ' '         ,
                   ' ' 'PAD22-INHRT' _P1001-INHRT ' ' ,
                    ' ' 'PAD22-DISPO' _P1001-DISPO ' ' ,
                    ' ' 'PAD22-MANZL' _P1001-MANZL2 ' ' ,
                    ' ' 'Q1023-SEMIN' _P1001-SEMIN ' ' ,
                    ' ' 'Q1023-TEILN' _P1001-TEILN ' ' .
      IF _P1001-TAGNR NE SPACE.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PAD22-TAGNR' _P1001-TAGNR ' ' .
      ENDIF.
      IF _P1001-ZBLID NE SPACE.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PAD22-ZBLID' _P1001-ZBLID  ' ' .
      ENDIF.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'BDC_OKCODE' '/11' ' '       .

      WHEN '4023'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4023' ' '         ,
                    ' ' 'PAD23-BEGUZ' _P1001-BEGUZ' ' ,
                    ' ' 'PAD23-ENDUZ' _P1001-ENDUZ' ' ,
                    ' ' 'Q1001-RESID' _P1001-RESID' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '        .


      WHEN '4025'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4025' ' '         ,
                    ' ' 'PAD25-MANZL' _P1001-MANZL3 ' ' ,
                    ' ' 'PAD25-KKOST' _P1001-KKOST ' ' ,
                    ' ' 'PAD25-KWAER' _P1001-KWAER ' ' ,
                    ' ' 'PAD25-KOKRS' _P1001-KOKRS ' ' ,
                    ' ' 'PAD25-KOSTL' _P1001-KOSTL ' ' .
      IF _P1001-BEZEK NE SPACE.
            PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PFSDY-BEZEK' _P1001-BEZEK ' ' .
      ENDIF.
        PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PAD25-BELNR' _P1001-BELNR' ' ,
                    ' ' 'PAD25-ZDATA' _P1001-ZDATA ' ' ,
                 ' ' 'BDC_OKCODE' '/11' ' '            .

      WHEN '4031'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4031' ' '         ,
                    ' ' 'P1001-PRIOX' _P1001-PRIOX ' ' ,
                    ' ' 'PAD31-CHARA' _P1001-CHARA ' ' ,
                    ' ' 'PAD31-EXPER' _P1001-EXPER ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '        .

      WHEN '4027'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4027' ' '         ,
                    ' ' 'PAD27-LANGU' _P1001-LANGU  ' ' .
       IF _P1001-MANZL4 NE SPACE.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PAD22-MANZL' _P1001-MANZL4 ' ' .
       ENDIF.
              PERFORM DYNPRO TABLES BDC_DATA USING:
                    ' ' 'PAD27-BUDAT' _P1001-BUDAT  ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '        .

      WHEN '4032' .
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4032' ' '         ,
                    ' ' 'PAD31-CHARA' _P1001-CHARA ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '        .

      WHEN '4048'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4048' ' '         ,
                    ' ' 'P1001-PRIOX' _P1001-PRIOX ' ' ,
                    ' ' 'PAD48-APSTV' _P1001-APSTV ' ' ,
                    ' ' 'PAD48-STREA' _P1001-STREA ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '         .

      WHEN '4050'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4050' ' '         ,
                    ' ' 'QREVIEW-EVATY' _P1001-EVATY ' ' ,
                    ' ' 'QREVIEW-CRITX' _P1001-CRITX ' ' ,
                    ' ' 'QREVIEW-JVALU' _P1001-JVALU ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '           .

      WHEN '4051'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4051' ' '         ,
                    ' ' 'PAD51-BLCID' _P1001-BLCID ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '          .


      WHEN '4053'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4053' ' '         ,
                    ' ' 'PAD53-MANZL' _P1001-MANZL5 ' ' ,
                    ' ' 'PAD53-BEGUZ' _P1001-BEGUZ1 ' ' ,
                    ' ' 'PAD53-ENDUZ' _P1001-ENDUZ1 ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '          .

      WHEN '4063'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4063' ' '         ,
                    ' ' 'PAD63-BEDART' _P1001-BEDART ' ' ,
                    ' ' 'PAD63-DIENST' _P1001-DIENST ' ' ,
                    ' ' 'PAD63-ANZCL' _P1001-ANZCL ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '           .

      WHEN '4210'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4210' ' '         ,
                    ' ' 'PADD2-ACTIVE' _P1001-ACTIVE ' ' ,
                    ' ' 'PADD2-REPPR' _P1001-REPPR ' ' ,
                 ' ' 'BDC_OKCODE' '/11' ' '            .

      WHEN '4270'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4270' ' '         ,
        ' ' 'PADD3-GENERATE' _P1001-GENERATE ' ' ,
        ' ' 'PADD3-EXCLUDED' _P1001-EXCLUDED ' ' ,
        ' ' 'PADD3-FUNC_AREA' _P1001-FUNC_AREA ' ' ,
        ' ' 'PADD3-GTIME' _P1001-GTIME ' ' ,
                       ' ' 'BDC_OKCODE' '/11' ' '         .

      WHEN '4283'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '4283' ' '         ,
                    ' ' 'PADXN-MESTYP' _P1001-MESTYP ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '           .

      WHEN '5010'.
        PERFORM DYNPRO TABLES BDC_DATA
                USING: 'X' 'MP100100' '5010' ' '         ,
                    ' ' 'PKEYK-KOSTL' _P1001-KOSTL ' ' ,
                    ' ' 'PKEYK-KOKRS' _P1001-KOKRS ' ' ,
                    ' ' 'P1001-PROZT' _P1001-PROZT1 ' ' ,
                    ' ' 'BDC_OKCODE' '/11' ' '        .

    ENDCASE.
  ENDIF.

*if _p1001-sobid = 'K' or _p1001-sobid = ' K' or _p1001-sobid = 'K '.)
IF _P1001-SCLAS = 'K' OR _P1001-SCLAS = ' K' OR _P1001-SCLAS = 'K '."SRB
    PERFORM DYNPRO TABLES BDC_DATA
            USING: 'X' 'MP100100' '5010' ' '         ,
                ' ' 'PKEYK-KOSTL' _P1001-KOSTL ' ' ,
                ' ' 'PKEYK-KOKRS' _P1001-KOKRS ' ' ,
                ' ' 'P1001-PROZT' _P1001-PROZT1 ' ' ,
                ' ' 'BDC_OKCODE' '/11' ' '        .
  ENDIF.
  PERFORM DYNPRO TABLES BDC_DATA USING:
                'X' 'SAPMH5A0' '1000' ' '         ,
                ' ' 'BDC_OKCODE' 'BACK' ' '       . "BACK
ENDFORM.                               " POPULATE_BDC

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  DATA : BEGIN OF WA OCCURS 100,
         STR(1000),
         END OF WA.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = FILE1001
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*    IMPORTING
*         FILELENGTH              =
       TABLES
            DATA_TAB                = WA
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            FILE_OPEN_ERROR         = 2
            FILE_READ_ERROR         = 3
            INVALID_TABLE_WIDTH     = 4
            INVALID_TYPE            = 5
            NO_BATCH                = 6
            UNKNOWN_ERROR           = 7
            GUI_REFUSE_FILETRANSFER = 8
            OTHERS                  = 9.
DATA : T.

  LOOP AT WA.
    SPLIT WA-STR AT DELIMITER INTO
    _P1001-OTYPE
  _P1001-BEGDA1 _P1001-ENDDA1 _P1001-OBJID
  _P1001-SUBTY _P1001-DPATT
  _P1001-RSIGN _P1001-RELAT
  _P1001-SCLAS _P1001-SOBID _P1001-SHORT _P1001-STEXT _P1001-PRIOX
  _P1001-PROZT1  _P1001-ZUSATZ _P1001-VERKOBJ _P1001-MANZL1
  _P1001-INHRT _P1001-DISPO _P1001-MANZL2 _P1001-SEMIN _P1001-TEILN
  _P1001-TAGNR _P1001-ZBLID _P1001-BEGUZ _P1001-ENDUZ _P1001-RESID
  _P1001-MANZL3 _P1001-KKOST _P1001-KWAER _P1001-KOKRS _P1001-KOSTL
  _P1001-BEZEK _P1001-BELNR _P1001-ZDATA _P1001-LANGU _P1001-MANZL4
  _P1001-BUDAT
*              _p1001-priox                " duplicate fld
   _P1001-CHARA _P1001-EXPER
*             _p1001-chara  _p1001-priox   " duplicate flds
   _P1001-APSTV _P1001-STREA _P1001-EVATY _P1001-CRITX
  _P1001-JVALU _P1001-BLCID _P1001-MANZL5 _P1001-BEGUZ1 _P1001-ENDUZ1
  _P1001-BEDART _P1001-DIENST _P1001-ANZCL _P1001-ACTIVE _P1001-REPPR
  _P1001-GENERATE _P1001-EXCLUDED _P1001-FUNC_AREA _P1001-GTIME
   _P1001-MESTYP  T.
*  _p1001-kostl _p1001-kokrs _p1001-prozt1 ." duplicate fld

IF _P1001-OBJID NE SPACE.
    APPEND _P1001.
ENDIF.
  ENDLOOP.

ENDFORM.                               " READ_DATA

* include for commonly used forms
INCLUDE ZPPDUTIL.
