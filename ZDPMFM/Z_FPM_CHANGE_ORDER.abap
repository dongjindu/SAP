FUNCTION Z_FPM_CHANGE_ORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE OPTIONAL
*"     VALUE(ZSPM_COMP) LIKE  ZSPM_COMP STRUCTURE  ZSPM_COMP
*"     VALUE(COMPLETE) TYPE  CHECK
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      ZSPM_TIME_CONF STRUCTURE  ZSPM_TIME_CONF OPTIONAL
*"      ZSPM_COUNTER STRUCTURE  ZSPM_COUNTER OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

*&--------------------------------------------------------------------&*
*& Date         User    Transport           Description
*& 08/26/2005   Shiva   UD1K917434          Included the logic to
*                                           handle the page down.
*& 08/26/2005  Shiva    UD1K917436          Read no. of lines from "Z"
*                                          table and then page down.
*& 08/26/2005  Shiva    UD1K917438         Dummy for new client built.
*& 08/29/2005  Shiva    UD1K917456         To handle for information
*                                          more than a page.
*&--------------------------------------------------------------------&*
  DATA: WA_AUSVN(10),
        WA_AUSBS(10),
        WA_GSTRP(10),
        WA_GLTRP(10),
        WA_ANZZL(3).

  DATA: WA_TABIX(2) TYPE N,
        WA_FNAME LIKE SCREEN-NAME.

***Start of Malfunction (Date)
  IF NOT ZSPM_COMP-AUSVN IS INITIAL.
    WRITE : ZSPM_COMP-AUSVN TO WA_AUSVN MM/DD/YYYY.
  ENDIF.
***End of Malfunction (Date)
  IF NOT ZSPM_COMP-AUSBS  IS INITIAL.
    WRITE : ZSPM_COMP-AUSBS TO WA_AUSBS MM/DD/YYYY.
  ENDIF.


  DATA : BEGIN OF IT_START_ITME OCCURS 0,
           POINT LIKE CXZEIT-FSAV,
         END OF IT_START_ITME.

  DATA : BEGIN OF IT_END_ITME OCCURS 0,
           POINT LIKE CXZEIT-FSAV,
         END OF IT_END_ITME.

  DATA : WA_POINT LIKE CXZEIT-FSAV.

  DATA : WA_START_DATE LIKE SY-DATUM,
         WA_START_TIME LIKE SY-UZEIT,
         WA_END_DATE LIKE SY-DATUM,
         WA_END_TIME LIKE SY-UZEIT.

  data: wa_opt like ctu_params.

  data: w_no_lines type i,
        w_cur_line type i,
        w_rem type i,
        w_index(2) type n.

****  Basic start date
  CLEAR : IT_START_ITME,IT_START_ITME[].

  select single nlvis from ztpmocln into w_no_lines.

  LOOP AT ZSPM_TIME_CONF.
    CLEAR : WA_POINT.
****Modification by 100565
    If  ZSPM_TIME_CONF-ISDD IS INITIAL.
      ZSPM_TIME_CONF-ISDD =  ZSPM_COMP-AUSVN.
    endif.
    if  ZSPM_TIME_CONF-ISDZ IS INITIAL.
      ZSPM_TIME_CONF-ISDZ = ZSPM_COMP-AUZTV.
*ZSPM_TIME_CONF-ISDZ = '090000'.
    endif.
    if ZSPM_TIME_CONF-IEDD IS INITIAL.
      ZSPM_TIME_CONF-IEDD = ZSPM_COMP-AUSBS.
    endif.
    if ZSPM_TIME_CONF-IEDZ IS INITIAL.
      ZSPM_TIME_CONF-IEDZ = ZSPM_COMP-AUZTB.
*ZSPM_TIME_CONF-IEDZ = '093000'.
    endif.
    modify zspm_time_conf.
**********end modification by 100565
    IF NOT ZSPM_TIME_CONF-ISDD IS INITIAL AND
       NOT ZSPM_TIME_CONF-ISDZ IS INITIAL.

      CALL FUNCTION 'DATE_TIME_CONVERT'
           EXPORTING
                DATE          = ZSPM_TIME_CONF-ISDD
                TIME          = ZSPM_TIME_CONF-ISDZ
           IMPORTING
                POINT_IN_TIME = WA_POINT.

      MOVE WA_POINT TO IT_START_ITME-POINT.
      APPEND IT_START_ITME.

    ENDIF.
  ENDLOOP.

  SORT IT_START_ITME BY POINT ASCENDING .
  READ TABLE IT_START_ITME INDEX 1.

  CALL FUNCTION 'POINT_IN_TIME_CONVERT'
       EXPORTING
            POINT_IN_TIME = IT_START_ITME-POINT
       IMPORTING
            DATE          = WA_START_DATE
            TIME          = WA_START_TIME.

  WRITE : WA_START_DATE TO WA_GSTRP MM/DD/YYYY.


*  SORT ZSPM_TIME_CONF BY ISDD ASCENDING .
*  READ TABLE ZSPM_TIME_CONF INDEX 1.
*  IF NOT ZSPM_TIME_CONF-ISDD IS INITIAL.
*    WRITE : ZSPM_TIME_CONF-ISDD TO WA_GSTRP MM/DD/YYYY.
*  ENDIF.

****  Basic finish date
  CLEAR : IT_END_ITME,IT_END_ITME[].

  LOOP AT ZSPM_TIME_CONF.
    CLEAR : WA_POINT.
    IF NOT ZSPM_TIME_CONF-IEDD IS INITIAL AND
       NOT ZSPM_TIME_CONF-IEDZ IS INITIAL.

      CALL FUNCTION 'DATE_TIME_CONVERT'
           EXPORTING
                DATE          = ZSPM_TIME_CONF-IEDD
                TIME          = ZSPM_TIME_CONF-IEDZ
           IMPORTING
                POINT_IN_TIME = WA_POINT.

      MOVE WA_POINT TO IT_END_ITME-POINT.
      APPEND IT_END_ITME.

    ENDIF.
  ENDLOOP.

  SORT IT_END_ITME BY POINT DESCENDING.
  READ TABLE IT_END_ITME INDEX 1.

  CALL FUNCTION 'POINT_IN_TIME_CONVERT'
       EXPORTING
            POINT_IN_TIME = IT_END_ITME-POINT
       IMPORTING
            DATE          = WA_END_DATE
            TIME          = WA_END_TIME.

  WRITE : WA_END_DATE TO WA_GLTRP MM/DD/YYYY.

*  SORT ZSPM_TIME_CONF BY IEDD DESCENDING.
*  READ TABLE ZSPM_TIME_CONF INDEX 1.
*  IF NOT ZSPM_TIME_CONF-IEDD IS INITIAL.
*    WRITE : ZSPM_TIME_CONF-IEDD TO WA_GLTRP MM/DD/YYYY.
*  ENDIF.

  CLEAR: ZSPM_TIME_CONF.

  SORT ZSPM_TIME_CONF BY VORNR.

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '0101'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'CAUFVD-AUFNR'
                                ZSPM_COMP-AUFNR. "//Order Number

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=VGUE'.
  PERFORM BDC_FIELD       USING 'CAUFVD-KTEXT'
                                ZSPM_COMP-KTEXT. "//Short text

*perform bdc_field       using 'CAUFVD-INGPR'
*                              AUFNR_001.
*perform bdc_field       using 'CAUFVD-VAPLZ'
*                              AUFNR_001.
*perform bdc_field       using 'CAUFVD-VAWRK'
*                              AUFNR_001.

  PERFORM BDC_FIELD       USING 'CAUFVD-GSTRP'
                                WA_GSTRP.
  PERFORM BDC_FIELD       USING 'CAUFVD-GLTRP'
                                WA_GLTRP.

*perform bdc_field       using 'CAUFVD-PRIOK'
*                              AUFNR_001.
  .
*perform bdc_field       using 'CAUFVD-TPLNR'
*                              AUFNR_001.
*perform bdc_field       using 'CAUFVD-EQUNR'
*                              AUFNR_001.

*  PERFORM BDC_FIELD       USING 'VIQMEL-AUSVN'
*                                 WA_AUSVN.
**                                 "//Start of Malfunction (Date)
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTV'
*                                ZSPM_COMP-AUZTV.
**                                 "//Start of Malfunction (Time)
*  PERFORM BDC_FIELD       USING 'VIQMEL-MSAUS'
*                                ZSPM_COMP-MSAUS.
**                                 "//Breakdown Indicator
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUSBS'
*                                WA_AUSBS.
**                                "//End of Malfunction (Date)
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTB'
*                                ZSPM_COMP-AUZTB.
**                                 "//End of Malfunction (Time)

*perform bdc_field       using 'VIQMEL-MAUEH'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-LTXA1'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-INDET'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-ARBPL'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-WERKS'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-STEUS'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-ARBEH'
*                              AUFNR_001.
*perform bdc_field       using 'AFVGD-DAUNE'
*                              AUFNR_001.

  LOOP AT ZSPM_TIME_CONF.
    MOVE : SY-TABIX TO WA_TABIX.
    w_cur_line = sy-tabix.
    if sy-tabix eq 1.
      w_index = 1.
    endif.
*** Number of capacities required
    WRITE : ZSPM_TIME_CONF-ANZZL TO WA_ANZZL.
    if w_cur_line > w_no_lines.
      w_rem = w_cur_line mod w_no_lines.
      if w_rem = 1.
        w_index = 1.
        PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=P+'.
      endif.
    endif.
    CLEAR : WA_FNAME.
    CONCATENATE 'AFVGD-LTXA1(' w_index ')' INTO WA_FNAME.

    PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING WA_FNAME
                                  ZSPM_TIME_CONF-LTXA1.

    CLEAR: WA_FNAME.
    IF ZSPM_TIME_CONF-LTXA1 NE SPACE.
      CONCATENATE 'AFVGD-ANZZL(' w_index ')' INTO WA_FNAME.
      PERFORM BDC_FIELD       USING  WA_FNAME
                                     WA_ANZZL.
    ENDIF.
    w_index = w_index + 1.
  ENDLOOP.

*  LOOP AT ZSPM_TIME_CONF.
*    IF SY-TABIX EQ 1.
*      PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=P+'.
*      PERFORM BDC_FIELD       USING 'AFVGD-LTXA1(01)'
*                                    ZSPM_TIME_CONF-LTXA1.
*      PERFORM BDC_FIELD       USING 'AFVGD-ANZZL(01)'
*                                    ZSPM_TIME_CONF-ANZZL.
*    ELSE.
*
*      PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=P+'.
*      PERFORM BDC_FIELD       USING 'AFVGD-LTXA1(02)'
*                                    ZSPM_TIME_CONF-LTXA1.
*      PERFORM BDC_FIELD       USING 'AFVGD-ANZZL(02)'
*                                    ZSPM_TIME_CONF-ANZZL.
*    ENDIF.
*  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=IHKZ'.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=IHOM'.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=10\TAB05'.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMTXT'
                                ZSPM_COMP-KTEXT.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMNAM'
                                ZSPM_COMP-QMNAM.
*perform bdc_field       using 'RIWO1-TPLNR'
*                              AUFNR_001.
*perform bdc_field       using 'RIWO1-EQUNR'
*                              AUFNR_001.
*perform bdc_field       using 'VIQMEL-INGRP'
*                              AUFNR_001.
*perform bdc_field       using 'VIQMEL-IWERK'
*                              AUFNR_001.
*perform bdc_field       using 'RIWO00-GEWRK'
*                              AUFNR_001.
*perform bdc_field       using 'RIWO00-SWERK'
*                              AUFNR_001.
*perform bdc_field       using 'VIQMEL-QMDAT'
*                              AUFNR_001.
*perform bdc_field       using 'VIQMEL-MZEIT'
*                              AUFNR_001.
*perform bdc_field       using 'RIWO00-HEADKTXT'
*                              AUFNR_001.

**** input malfunction time
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=10\TAB10'.
  PERFORM BDC_FIELD       USING 'VIQMEL-AUSVN'
                                 WA_AUSVN.
*                                 "//Start of Malfunction (Date)
  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTV'
                                ZSPM_COMP-AUZTV.
*                                 "//Start of Malfunction (Time)
  PERFORM BDC_FIELD       USING 'VIQMEL-MSAUS'
                                ZSPM_COMP-MSAUS.
*                                 "//Breakdown Indicator
  PERFORM BDC_FIELD       USING 'VIQMEL-AUSBS'
                                WA_AUSBS.
*                                "//End of Malfunction (Date)
  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTB'
                                ZSPM_COMP-AUZTB.
*                                 "//End of Malfunction (Time)
  PERFORM BDC_FIELD       USING 'VIQMEL-MSAUS'
                                'X'.
  "//Breakdown Indicator
  PERFORM BDC_FIELD       USING 'VIQMEL-MAUEH'
                                'MIN'.
  "//Unit for Breakdown Duration

  IF ZSPM_COMP-OTEIL NE SPACE AND
     ZSPM_COMP-FECOD NE SPACE AND
     ZSPM_COMP-MNCOD NE SPACE AND
     ZSPM_COMP-URCOD NE SPACE.

***** input Part of Object
***** Inupt long text...
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  'PX01'.

*  PERFORM BDC_FIELD       USING 'VIQMFE-OTGRP(01)'
*                                ZSPM_COMP-OTGRP.
    PERFORM BDC_FIELD       USING 'VIQMFE-OTEIL(01)'
                                  ZSPM_COMP-OTEIL.
*                                "//Part of Object
*  PERFORM BDC_FIELD       USING 'VIQMFE-FEGRP(01)'
*                                ZSPM_COMP-FEGRP.
    PERFORM BDC_FIELD       USING 'VIQMFE-FETXT(01)'
                                  ZSPM_COMP-FETXT.
*                                "//Notification Item Short Text
    PERFORM BDC_FIELD       USING 'VIQMFE-FECOD(01)'
                                  ZSPM_COMP-FECOD.
*                                "//Problem

    PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=TXBA'.
    PERFORM BDC_FIELD       USING 'RSTXT-TXLINE(02)'
                                  '.'.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=20\TAB02'.

***** Task List Tab
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=20\TAB03'.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
**** Enter Tasks.............
    LOOP AT ZSPM_COUNTER.
      IF SY-TABIX EQ 1.

        PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM BDC_FIELD       USING 'VIQMSM-MNCOD(01)'
                                      '01'.

        PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=SX01'.

        PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=TXBA'.
        PERFORM BDC_FIELD       USING 'RSTXT-TXLINE(03)'
                                      '.'.

        PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      'PEND'.
      ELSE.
*        PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                      'PEND'.
*        PERFORM BDC_FIELD       USING 'VIQMSM-MNCOD(02)'
*                                      '01'.
*        PERFORM BDC_FIELD       USING 'VIQMSM-MATXT(02)'
*                                      '.'.
      ENDIF.
    ENDLOOP.

    IF COMPLETE EQ 'X'.
**** Select All
      PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    'MALL'.
*** Complete All
*    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                  '=MADF'.

*    PERFORM BDC_FIELD       USING 'RIWO00-SELEK(01)'
*                                'X'.
      PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
    ENDIF.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=20\TAB02'.
**** Task Complete ..............

**** input Cause Code...
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=UX01'.
*  PERFORM BDC_FIELD       USING 'VIQMUR-URGRP(01)'
*                                ZSPM_COMP-URGRP.
    PERFORM BDC_FIELD       USING 'VIQMUR-URCOD(01)'
                                  ZSPM_COMP-URCOD.
*                                "//Cause Code
    PERFORM BDC_FIELD       USING 'VIQMUR-URTXT(01)'
                                  ZSPM_COMP-URTXT.
*                                "//Cause Text

    PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=TXBA'.
    PERFORM BDC_FIELD       USING 'RSTXT-TXLINE(02)'
                                  '.'.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=20\TAB04'.

**** Activity Code...
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=MX01'.
*  PERFORM BDC_FIELD       USING 'VIQMMA-MNGRP(01)'
*                                ZSPM_COMP-MNGRP.
    PERFORM BDC_FIELD       USING 'VIQMMA-MNCOD(01)'
                                  ZSPM_COMP-MNCOD.
*                                "//Activity Code
    PERFORM BDC_FIELD       USING 'VIQMMA-MATXT(01)'
                                  ZSPM_COMP-MATXT.
*                                "//Activity Text

    PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=TXBA'.
    PERFORM BDC_FIELD       USING 'RSTXT-TXLINE(02)'
                                  '.'.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=RW00'.

  ELSE.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=RW00'.
  ENDIF.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM BDC_TRANSACTION TABLES MESSTAB
                          USING 'IW32'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
