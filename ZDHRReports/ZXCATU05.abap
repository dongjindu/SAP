
*----------------------------------------------------------------------*
*   INCLUDE ZXCATU05                                                   *
*----------------------------------------------------------------------*
* Author            : IG.MOON
* Creation Date     : 08/20/2008
* Specifications By : Imtiaz Ahmad
* Description       : Enhancement for military unpaid code 1033
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(DATEFROM) LIKE  CATSFIELDS-DATEFROM
*"     VALUE(DATETO) LIKE  CATSFIELDS-DATETO
*"     VALUE(SAP_TCATS) LIKE  TCATS STRUCTURE  TCATS OPTIONAL
*"  TABLES
*"      CHECK_TABLE STRUCTURE  CATS_COMM
*"      I_MESSAGES STRUCTURE  CATS_MESG
*"----------------------------------------------------------------------

* diabled by ig.moon according to function request 9/15/2008

**DATA BEGIN OF l_check_table OCCURS 50.
**        INCLUDE STRUCTURE cats_comm.
**DATA   $workdate TYPE catsdate.
**DATA END OF l_check_table.
**DATA  $ix TYPE i.
**
**DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
**DATA imisdate TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
**
**DATA: $start_date LIKE sy-datum,
**      $end_date   LIKE sy-datum.
**
**DATA $daytot TYPE catshours.
**DATA $maxhrs TYPE catshours.
**DATA $flag.
**
***" move to temporary work table
**LOOP AT check_table.
**  MOVE-CORRESPONDING check_table TO l_check_table.
**  l_check_table-$workdate = l_check_table-workdate.
**  APPEND l_check_table.
**ENDLOOP.
**
***" delete other types
**DELETE l_check_table WHERE awart <> '1033'.
**
***" don't need to proceed if no data.
**READ TABLE l_check_table INDEX 1.
**CHECK sy-subrc EQ 0.
**
***" get start date
**$start_date = l_check_table-workdate.
**LOOP AT l_check_table.ENDLOOP.
***" get end date
**$end_date = l_check_table-workdate.
**
**CALL FUNCTION 'HOLIDAY_GET'
**     EXPORTING
**          factory_calendar = 'HM'
**          date_from        = $start_date
**          date_to          = $end_date
**     TABLES
**          holidays         = iholiday.
**
**IF sy-subrc <> 0.ENDIF.
**
**SORT  l_check_table BY workdate.
**
**LOOP AT iholiday.
**
**  READ TABLE l_check_table WITH KEY
**      workdate = iholiday-date BINARY SEARCH.
**
**  IF sy-subrc NE 0.
**    imisdate-date = iholiday-date.
**    APPEND imisdate.
**  ENDIF.
**
**ENDLOOP.
**
**SORT imisdate BY date.
**
**DATA: w_schkz LIKE pa0007-schkz.
**
**CLEAR l_check_table-workdate.
**MODIFY l_check_table TRANSPORTING workdate
**      WHERE NOT workdate IS initial.
**
**LOOP AT l_check_table.
**
**  AT NEW pernr.
**    $flag = 'X'.
**  ENDAT.
**
**  IF $flag EQ 'X'.
**    CLEAR :$flag .
**    LOOP AT imisdate.
**      l_check_table-$workdate = imisdate-date.
**      CLEAR : l_check_table-catsquantity,
**              l_check_table-catshours .
**      APPEND l_check_table.
**    ENDLOOP.
**  ENDIF.
**
**ENDLOOP.
**
**SORT l_check_table BY pernr $workdate.
**
**LOOP AT l_check_table.
**
**  AT NEW pernr.
**    $flag = 'X'.
**  ENDAT.
**
**  IF $flag EQ 'X'.
**    CLEAR :$flag, $daytot .
**    $maxhrs = '24'.
**
**    SELECT SINGLE schkz INTO w_schkz
**           FROM pa0007
**           WHERE pernr = l_check_table-pernr
**             AND begda <= l_check_table-$workdate
**             AND endda >= l_check_table-$workdate.
**
**    IF sy-subrc EQ 0 AND w_schkz = '4001'. " Calc with 10 H
**      $maxhrs = '30'.
**    ELSE.
**      $maxhrs = '24'.
**    ENDIF.
**
**  ENDIF.
**
**  READ TABLE iholiday WITH KEY
**      date = l_check_table-$workdate BINARY SEARCH.
**
**  IF sy-subrc EQ 0.
**    CLEAR $daytot .
**    CONTINUE.
**  ENDIF.
**
**  IF l_check_table-catshours IS INITIAL.
**    CLEAR $daytot .
**    CONTINUE.
**  ENDIF.
**
**  ADD l_check_table-catshours TO $daytot .
**
**  IF $daytot > $maxhrs.
**    IF l_check_table-$workdate BETWEEN datefrom AND dateto.
**      CLEAR i_messages.
**      i_messages-pernr = l_check_table-pernr.
**      i_messages-catsdate = l_check_table-$workdate.
**      i_messages-msgty = 'E'.
**      i_messages-msgid = 'ZMHR'.
**      i_messages-msgno = '000'.
**      i_messages-msgv2 =
**        'Total time for type ''1033'' exceeds 3 days.'.
**      i_messages-msgv3 = ''.
**      i_messages-msgv4 = ''.
**      APPEND i_messages.
**    ENDIF.
**  ENDIF.
**ENDLOOP.

DATA BEGIN OF l_check_table OCCURS 50.
        INCLUDE STRUCTURE cats_comm.
DATA   $workdate TYPE catsdate.
DATA END OF l_check_table.
DATA  $ix TYPE i.

DATA iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.
DATA imisdate TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE.

DATA: $start_date LIKE sy-datum,
      $end_date   LIKE sy-datum.

DATA $daytot TYPE catshours.
DATA $maxhrs TYPE catshours.
DATA $flag.
DATA $art01 TYPE art01.
DATA w_schkz LIKE pa0007-schkz.
DATA $text(100).
data $maxhrs_text(10).

*" move to temporary work table
LOOP AT check_table.

  IF check_table-workdate BETWEEN datefrom AND dateto.

    SELECT SINGLE art01 INTO $art01 FROM t554s
                               WHERE subty EQ check_table-awart
                                 AND endda >= check_table-workdate
                                 AND begda <= check_table-workdate.
    IF sy-subrc EQ 0 AND $art01 EQ 'A'.
      MOVE-CORRESPONDING check_table TO l_check_table.
      l_check_table-$workdate = l_check_table-workdate.
      APPEND l_check_table.
    ENDIF.

  ENDIF.

ENDLOOP.


*" don't need to proceed if no data.
READ TABLE l_check_table INDEX 1.
CHECK sy-subrc EQ 0.

*break-point.

LOOP AT l_check_table.

  AT NEW pernr.
    $flag = 'X'.
  ENDAT.

  IF $flag EQ 'X'.
    CLEAR :$flag, $daytot .

    SELECT SINGLE schkz INTO w_schkz
           FROM pa0007
           WHERE pernr = l_check_table-pernr
             AND begda <= l_check_table-$workdate
             AND endda >= l_check_table-$workdate.

    IF sy-subrc EQ 0 AND ( w_schkz = '4001' " Calc with 10 H
                        or w_schkz(4) = '8000' ).
      $maxhrs = '10'.
    ELSE.
      $maxhrs = '8'.
    ENDIF.

  ENDIF.

  ADD l_check_table-catshours TO $daytot .

  IF $daytot > $maxhrs.
    $maxhrs = $daytot - $maxhrs.
    $maxhrs_text = $maxhrs.
    condense $maxhrs_text.
    concatenate  'Absence hrs exceed the' $maxhrs_text
    'of hrs in a full-day' into $text separated by space.
    MESSAGE e000(ZMCO) WITH $text.
  ENDIF.


ENDLOOP.
