***********************************************************************
* Form routines for the file handling in report RFKQSU40
***********************************************************************

DATA:
      COUNT_W TYPE I,
*NOTE 454478 BEGIN
      count_q type i.
*NOTE 454478 END

*---------------------------------------------------------------------*
*       FORM FILE_AT_LAST                                             *
*---------------------------------------------------------------------*
*       Creates the Y record and closes the file                      *
*---------------------------------------------------------------------*
FORM FILE_AT_LAST.

* Note 454478 begin
** f record
  f+0(1)   = 'F'.

  unpack count_w to f+1(3).

  f+4(3)   = '001'.

*Note 803878

  f+7(763) = space.
  file_count = file_count + 1.
  unpack file_count to f+770(8).
  f+778(2) = space.

*Note 803878

  if file_error = 0.
    transfer f to tapename.
    close dataset tapename.
    append f to 1042_rec.
  endif.
* Note 454478 end

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FILE_CREATE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FILE_ERROR                                                    *
*---------------------------------------------------------------------*
FORM FILE_CREATE USING FILE_ERROR.

  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING  "Logical File must be maintained and
         "transported using transaction SF01
            LOGICAL_FILENAME = 'FI_TAX1042S'
       IMPORTING
            FILE_NAME        = TAPENAME
       EXCEPTIONS
            FILE_NOT_FOUND   = 1.
  FILE_ERROR = SY-SUBRC.
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM FILE_END_BUKRS                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM FILE_END_BUKRS.

* Note 454478 begin
** C record
  c+0(1)   = 'C'.

  unpack count_q to c+1(8).
  perform round_amount using rtab-qsshh
                       changing rounded_amount.
  unpack rounded_amount to c+15(15).
  perform round_amount using rtab-qbshh
                       changing rounded_amount.
  unpack rounded_amount to c+30(15).

*Note 803878
*  c+45(735)   = ' '.
  c+45(725)   = ' '.

  file_count = file_count + 1.
  unpack file_count to c+770(8).

  c+778(2) = space.
*Note 803878

  if file_error = 0.
    transfer c to tapename.
    append c to 1042_rec.
*    add 1 to count_w.
  endif.
* Note 454478 end

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FILE_INIT                                                *
*---------------------------------------------------------------------*
*       Opens the file and initializes all records                    *
*---------------------------------------------------------------------*
*  -->  ERROR when opening the file                                   *
*---------------------------------------------------------------------*


FORM FILE_INIT USING ERROR.

  OPEN DATASET TAPENAME FOR OUTPUT IN TEXT MODE.

  ERROR = SY-SUBRC.
  IF ERROR NE '0'.
    MESSAGE I600 WITH TEXT-F01.
  ENDIF.

  T(1)      = 'T'.
  T+1(4)    = FISCAL.
*  t+1(2)    = fiscal+2(2).
*  t+3(2)    = '01'.
  T+5(9)    = TR_TIN.
  T+14(40)  = TR_NAME.
  T+54(40)  = TR_ADDR.
  T+94(20)  = TR_CITY.
  T+114(2)  = TR_STAT.

*Note 454478 begin
  t+116(4)  = ' '.
  tr_zip1 = tr_zip.
perform correct_zip using tr_zip1.
  t+120(9)  = tr_zip1.
** contactor's name & tel
  t+129(40) = contact.
perform correct_tel using tel.
  t+169(20) = tel.
  t+189(5)  = tr_tcc.
** test indicator
if not testfil is initial.
   t+194(1) = 'TEST'.
endif.
*  t+198(582) = space.  "Note 803878
*Note 454478 end

*Note 803878
t+198(1) = space.
t+199(571) = space.
file_count = 1.
unpack file_count to t+770(8).
t+778(2) = space.
*Note 803878

  IF ERROR EQ '0'.
    TRANSFER T TO TAPENAME.
    append t to 1042_rec.
  ENDIF.

  Q(1)      = 'Q'.
*Note 454478 begin
  q+1(2)  = '00'.
*  q+384(35) = py_name.
  q+666(40) = py_name.
  if py_tin > 0.
     q+706(9) = py_tin.
  else.
     q+706(9) = space.
  endif.
*  q+419(9)  = py_tin.
*  q+449(10) = '0000000000'.
*  q+463(37) = space.

  w(1)      = 'W'.
*  w+1(2)    = space.
  w+1(2)    = '00'.
  w+268(4)  = fiscal.
*   w+147(4)  = fiscal.
*  w+1(2)    = fiscal+2(2).
*Note 454478 end

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM FILE_INIT_AGENT                                          *
*---------------------------------------------------------------------*
*       Initializes withholding agent info for all record types       *
*---------------------------------------------------------------------*


FORM FILE_INIT_AGENT.

*Note 454478 begin
*  w+3(9)        = q+1(9)       = t001z-paval.
   w+3(9)        = t001z-paval.
*Note 803878
*   w+12(1)       = '1'.
   w+12(1)       = space.
*Note 803878

  condense sadr-name1.
*  w+12(35)      = q+10(35)     = sadr-name1.
   w+13(40)      = sadr-name1.
  condense sadr-name2.
    w+53(40)      = sadr-name2.
  condense sadr-name3.
    w+93(40)      = sadr-name3.
*  w+47(35)      = q+45(35)     = sadr-stras.
   w+133(40)     = sadr-stras.
   w+173(40)     = sadr-strs2.
  condense sadr-ort01.
*  w+82(20)      = q+80(20)     = sadr-ort01.
   w+213(40)      = sadr-ort01.
*  w+102(2)                     = sadr-regio.

  case agenttab-addrs.
    when '004'.                        "004 equals USA
*      q+100(2)                     = sadr-regio.
      w+253(2)       = sadr-regio.
*Note 497879 begin
      sadr-pstlz1 = sadr-pstlz.
      perform correct_zip using sadr-pstlz1.
*      w+104(9)      = q+102(9)     = sadr-pstlz.
      w+259(9)      = sadr-pstlz1.
*Note 497879 END
    when others.
*      q+100(2)                     = space.
      w+259(9)      = '000000000'.
  endcase.
  w+255(4)       = space.
  perform correct_tel using sadr-telf1.
  w+272(20)      = sadr-telf1.
  w+292(1)       = '0'.
*  w+293(487)     = space.         "Note 803878

*Note 803878
  w+293(477) = space.
  file_count = file_count + 1.
  unpack file_count to w+770(8).
  w+778(2) = space.
*Note 803878

  if file_error = 0.
    transfer w to tapename.
    append w to 1042_rec.
    add 1 to count_w.
  endif.
*Note 454478 end

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM FILE_INIT_RECIPIENT                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*


FORM FILE_INIT_RECIPIENT.

*Note 454478 begin
  q+59(12)   = ' '.
**Account number
  q+71(20)   = lfa1-lifnr.
*  q+121(15)   = lfa1-lifnr.
  q+398(2)    = ktab-qland.

  condense lfa1-name1.
  q+93(40)   = lfa1-name1.
*  q+138(45)   = lfa1-name1.
*  q+138(35)   = lfa1-name1.

  condense lfa1-name2.
 q+133(40)   = lfa1-name2.
*  q+183(45)   = lfa1-name2.
*  q+173(35)   = lfa1-name2.

*  condense lfa1-name3.
   q+173(40)   = lfa1-name3.
*  q+208(35)   = lfa1-name3.

  condense lfa1-stras.
   q+213(40)   = lfa1-stras.
   q+253(40)   = space.
*  q+228(55)   = lfa1-stras.
*  q+243(40)   = lfa1-stras.

  condense lfa1-ort01.
   q+293(40)   = lfa1-ort01.
*  q+283(25)   = lfa1-ort01.
*Note 497879 begin
   select * from t005 where land1 = lfa1-land1.
*  read table t005 with key lfa1-land1.
        lfa1-pstlz1 = lfa1-pstlz.
        perform correct_zip using lfa1-pstlz1.
*Note 497879 end
  case t005-addrs.
    when '004'.                                            "USA
      q+333(2) = lfa1-regio.
      q+335(2) = space.
*Note 615662 begin
      Q+337(2) = '  '.
*Note 615662 end
    when '005'.                        "CANADA
*      q+308(2) = lfa1-regio.
      q+333(2)   = space.
      q+335(2) = lfa1-regio.
*Note 497879 begin
*Note 619867 begin
        q+337(2) = ktab-qland. "Note # 716970
*      q+337(2)    = lfa1-land1.
*Note 619867 end
*Note 497879 end
    when others.
*      select single * from t005u where land1 = lfa1-land1
*                                 and bland = lfa1-regio
*                                 and spras = agenttab-spras.
*      q+308(15) = t005u-bezei.
      q+333(2)   = space.
      q+335(2) = space.
*Note 497879 BEGIN
*Note 619867 begin
      q+337(2)    = ktab-qland. "Note # 716970
*      q+337(2) = lfa1-land1.
*Note 619867 end
  endcase.
  q+339(9)    = lfa1-pstlz1.
  endselect.
*Note 497879 end
*Recipient's Country of Residence
    q+358(40)  = t005r-qltxt.
*    q+332(2)   = space.
*    q+334(20)  = t005t-landx.

*Recipient code
   q+91(2) = ktab-qsrec.
*SSN/EIN
*Note 593071 begin
   q+348(9) = tin_code.
*NOTE 599923 begin
   if tin_type = 2.
      q+357(1) = '1'.
   elseif tin_type = 1.
      q+357(1) = '2'.
   else.
      q+348(9) = space.
      q+357(1) = space.
*NOTE 599923 end
   endif.
*Note 593071 end
*  q+356(2) = ktab-qsrec.
* Note 454478 end
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM FILE_WRITE_Q                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

DATA:
      QSATZ LIKE X059-QSATZ,
      NETTO LIKE RTAB-ALLOW.

FORM FILE_WRITE_Q.

*Note 454478 begin
  q+3(2) = rtab-qscod.
*  q+354(2) = rtab-qscod.

  if rtab-qscod = '21' or
     rtab-qscod = '22' or
     rtab-qscod = '23'.
    q+91(2) = '01'.
*    q+356(2) = '01'.
  endif.
*Gross income
   perform round_amount using    rtab-qsshh
                        changing rounded_amount.
   unpack rounded_amount to q+5(12).
*  unpack rtab-qsshh to q+358(11).
*Note 497879 begin
*Tax rate
  if rtab-qbshh = 0.
     qsatz = 0.
  else.
     qsatz = x059-qsatz.
  endif.
  unpack qsatz to q+41(4).
*Note 497879 end
*Note 492783 begin
*Exemption code
  if qsatz eq 0.
      q+45(2) = ktab-qsbgr.
  elseif
     qsatz ge 31.
     q+45(2) = '  '.
  elseif
     ( qsatz ge 1 ) and ( qsatz le 30 ).
     q+45(2) = '00'.
  endif.
*Note 492783 end
*  qsatz = x059-qsatz / 100.
*  unpack qsatz to q+371(2).
*Us tax withhold
  perform round_amount using rtab-qbshh
                       changing rounded_amount.
  unpack rounded_amount to q+47(12).
*unpack rtab-qbshh to q+373(11).

*  if rtab-qsskz = '15'.
*    unpack rtab-allow to q+429(10).
*    netto = rtab-qsshh - rtab-allow.
*    unpack netto to q+439(10).
*Note 494042 begin
  if ( rtab-qscod = '15' ) or ( rtab-qscod = '16' ).
*Note 494042 end
    perform round_amount using rtab-allow
                         changing rounded_amount.
    unpack rounded_amount to q+17(12).
    netto = rtab-qsshh - rtab-allow.
    perform round_amount using netto
                         changing rounded_amount.
    unpack rounded_amount to q+29(12).
  else.
    q+17(12) = space.
    q+29(12) = space.
  endif.

  q+400(266) = space.
  q+715(12) = '000000000000'.
*Note 492783 begin
  q+737(2) = '  '.
*Note 492783 end
*    Q-RP_EXEMPT,
*    Q-RP_RATE(2),
*Note 803878
q+761(10) = space.

file_count = file_count + 1.
unpack file_count to q+770(8).

q+778(2) = space.
*Note 803878

  if file_error = 0.
    transfer q to tapename.
    append q to 1042_rec.
    add 1 to count_q.
  endif.
* Note 454478 end

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ROUND_AMOUNT                                             *
*---------------------------------------------------------------------*
FORM round_amount USING    i_amount type p
                  CHANGING e_amount type p.
 e_amount = i_amount.
ENDFORM.
