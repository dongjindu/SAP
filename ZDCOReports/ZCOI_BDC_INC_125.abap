*----------------------------------------------------------------------*
*   INCLUDE ZCOI_BDC_INC                                               *
*----------------------------------------------------------------------*
*INITIALIZATION.
*==============
*  ctu     = ' '.
*  session = 'X'.
*  group = 'GROUP NAME'.

*MAIN LOGIC
*==========
*FORM CREATE_SESSION.
*  DATA : l_cnt TYPE i.
*  LOOP AT itab.
*    AT FIRST.
*      PERFORM open_group.
*    ENDAT.
*    ADD 1 TO l_cnt.
*
*    PERFORM POST_BDC.
*
*    IF l_cnt = p_scount.
*      PERFORM close_group.
*      PERFORM open_group.
*      CLEAR l_cnt.
*    ENDIF.
*  ENDLOOP.
*
*  PERFORM close_group.
*ENDFORM.
*
*FORM POST_BDC.
*  PERFORM bdc_field       USING:
*          'X'  'SAPMF05A'     '0700',
*          ' '  'BDC_CURSOR'   'RF05A-NEWBS',
*          ' '  'BDC_OKCODE'   '=BU'.
*  ...
*  PERFORM bdc_transaction USING 'FB01'.
*ENDFORM.


***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input
*  for programs doing a data transfer by CALL TRANSACTION USING
*Create Session
parameters session(1) type c default ' '  no-display.
*Call transaction
parameters ctu(1)     type c default ' '  no-display.
parameters p_scount TYPE i DEFAULT 5000  no-display.
                          "No of records per session
parameters group(12) no-display.   "Group name of session
* Run-mode
* A: show all bdc_fields
* E: show bdc_field on error only
* N: do not display bdc_field
parameters ctumode like ctu_params-dismode default 'E'. " no-display.

* user for session in batch
parameters user(12) default sy-uname no-display.
parameters cupdate like ctu_params-updmode default 'S'. " no-display.
"S: synchronously
"A: asynchronously
"L: local
parameters holddate  like sy-datum no-display.
* 'X' = keep   session if finished
parameters keep(1)   type c default ' '    no-display.
"' ' = delete session if finished
parameters nodata    default '/' lower case no-display.          "nodata

* 'X' = no transaction logging
parameters smalllog(1) type c default 'X' no-display.

*       Batchinputdata of single transaction
data:   bdcdata like bdcdata    occurs 0 with header line.
*       messages of call transaction
data:   messtab like bdcmsgcoll occurs 0 with header line.
*       error session opened (' ' or 'X')
data:   e_group_opened.
*       message texts

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
form open_group.
  if session = 'X'.
    skip.
    write: /(20) 'Create group'(i01), group.
    skip.
*   open batchinput group
    call function 'BDC_OPEN_GROUP'
         exporting
              client   = sy-mandt
              group    = group
              user     = user
              keep     = keep
              holddate = holddate.
    write: /(30) 'BDC_OPEN_GROUP'(i02),
            (12) 'returncode:'(i05),
                 sy-subrc.
  endif.
endform.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
form close_group.
  if session = 'X'.
*   close batchinput group
    call function 'BDC_CLOSE_GROUP'.
    write: /(30) 'Close session',
            (12) 'Return code =',
                 sy-subrc.
  else.
    if e_group_opened = 'X'.
      call function 'BDC_CLOSE_GROUP'.
      write: /.
      write: /(30) 'Error session created'.
    endif.
  endif.
endform.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
form bdc_transaction using tcode.
  data: l_mstring(480).
  data: l_subrc like sy-subrc.
* batch input session
  if session = 'X'.
    call function 'BDC_INSERT'
         exporting
              tcode     = tcode
         tables
              DYNPROTAB = bdcdata.
    if smalllog <> 'X'.
      write: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    endif.
* call transaction using
  else.
    refresh messtab.
    call transaction tcode using bdcdata
                     mode   ctumode
                     update cupdate
                     messages into messtab.
    l_subrc = sy-subrc.
    if smalllog <> 'X'.
    else.
      format color col_key.
      write: / 'Return code =',
               l_subrc,
               'RECORD:',
               sy-index.
      format color col_normal.
      loop at messtab.
        replace '&' with messtab-msgv1 into l_mstring.
        replace '&' with messtab-msgv2 into l_mstring.
        replace '&' with messtab-msgv3 into l_mstring.
        replace '&' with messtab-msgv4 into l_mstring.
        condense l_mstring.
        write: / messtab-msgtyp, l_mstring(150).
      endloop.

    endif.
*********************************************************
** Erzeugen fehlermappe ************************************************
    if l_subrc <> 0 and group <> space.
      if e_group_opened = ' '.
        call function 'BDC_OPEN_GROUP'
             exporting
                  client   = sy-mandt
                  group    = group
                  user     = user
                  keep     = keep
                  holddate = holddate.
        e_group_opened = 'X'.
      endif.
      call function 'BDC_INSERT'
           exporting
                tcode     = tcode
           tables
                bdc_fieldtab = bdcdata.
    endif.
  endif.
  refresh bdcdata.
endform.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field  using fbegin fnam fval.
  if fbegin = 'X'.
    clear bdcdata.
    move : fnam  to bdcdata-program,
           fval  to bdcdata-dynpro,
           'X'   to bdcdata-dynbegin.
    append bdcdata.
  else.
*   IF fval <> nodata.
    clear bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    append bdcdata.
*   ENDIF.
  endif.
endform.
