*----------------------------------------------------------------------*
***INCLUDE MZAHR0015F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_main_data.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
     FROM zthr_pcp08
     WHERE  zyear = w_zyear
       AND  zvers = w_zvers
       AND  zpera = w_werks .

  IF sy-subrc NE 0.
    PERFORM data_401k_data_seletion.
  ENDIF.

ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  Descrion_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descrion_date.
  LOOP AT it_9000.
    SELECT SINGLE ktext INTO it_9000-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_9000-zcost
                  AND datbi = '99991231'.
    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO it_9000-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND ( zgrup = '1260' OR zgrup = '1270' )
             AND zctxt EQ  it_9000-zcost.
    ENDIF.

    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_9000-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    it_9000-zjobk = hrp1000-short.

    IF  it_9000-a_q_hce <> 0.
      it_9000-a_r_hce = it_9000-a_b_hce / it_9000-a_q_hce .
    ENDIF.
    IF  it_9000-a_q_nhce <> 0.
      it_9000-a_r_nhce = it_9000-a_b_nhce / it_9000-a_q_nhce .
    ENDIF.

    IF it_9000-a_a_hce <> 0.
      it_9000-a_ar_hce =   it_9000-a_p_hce / it_9000-a_a_hce .
    ENDIF.
    IF it_9000-a_a_nhce <> 0.
      it_9000-a_ar_nhce =   it_9000-a_p_nhce / it_9000-a_a_nhce.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " Descrion_date
*&---------------------------------------------------------------------*
*&      Form  data_sorting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_sorting.
  DATA : BEGIN OF it_sort OCCURS 0,
         val1 LIKE zthr_pcp02-zval1,
         val2 LIKE zthr_pcp02-zval2,
         END OF it_sort.

  CLEAR : it_sort, it_sort[].
  SELECT zval1 zval2 INTO (it_sort-val1, it_sort-val2)
   FROM zthr_pcp02
   WHERE zmodl = '02'
     AND zgrup = '1240' .
    APPEND it_sort.
  ENDSELECT.

  LOOP AT it_9000.
* OBJID
    READ TABLE it_sort WITH KEY val2 = it_9000-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_9000-zval1.
      MODIFY it_9000.
    ENDIF.
  ENDLOOP.
*SORT IT_AHC01  ASCENDING  BY ZCOST .
  SORT it_9000
                ASCENDING BY  zcost  zval1   ASCENDING .


ENDFORM.                    " data_sorting
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN_LOADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_loading.
  DATA : wa_filename    LIKE rlgrap-filename .


  DATA: BEGIN OF it_down  OCCURS 0,
      zcost(40),
      ktext(40),
      zhedc(40),
      zjobk(40),
      a_q_hce(40),
      a_q_nhce(40),
      a_q_noq(40),
      a_b_hce(40),
      a_b_nhce(40),
      a_r_hce(40),
      a_r_nhce(40),
      a_p_hce(40),
      a_p_nhce(40),
      a_a_hce(40),
      a_a_nhce(40),
      a_ar_hce(40),
      a_ar_nhce(40),
      p_q_hce(40),
      p_q_nhce(40),
      p_q_noq(40),
      p_r_hce(40),
      p_r_nhce(40),
      p_b_hce(40),
      p_b_nhce(40),
      p_a_hce(40),
      p_a_nhce(40),
      p_ar_hce(40),
      p_ar_nhce(40),
      p_p_hce(40),
      p_p_nhce(40),
      END OF it_down.

  CLEAR : it_down , it_down[].

  LOOP AT it_9000.
    MOVE-CORRESPONDING it_9000   TO it_down.
    APPEND it_down . CLEAR it_down.
  ENDLOOP.

  it_down-zcost = ' '.
  it_down-ktext = ' '.
  it_down-zhedc = ' '.
  it_down-zjobk = ' '.
  it_down-a_q_hce = 'Actual'.
  it_down-a_q_nhce = ' '.
  it_down-a_q_noq = ' '.
  it_down-a_b_hce = ' '.
  it_down-a_b_nhce = ' '.
  it_down-a_r_hce = ' '.
  it_down-a_r_nhce = ' '.
  it_down-a_p_hce = ' '.
  it_down-a_p_nhce = ' '.
  it_down-a_a_hce = ' '.
  it_down-a_a_nhce = ' '.
  it_down-a_ar_hce = ' '.
  it_down-a_ar_nhce = ' '.

  it_down-p_q_hce = 'Plan '.
  it_down-p_q_nhce = ' '.
  it_down-p_q_noq = ' '.
  it_down-p_b_hce = ' '.
  it_down-p_b_nhce = ' '.
  it_down-p_r_hce = ' '.
  it_down-p_r_nhce = ' '.
  it_down-p_a_hce = ' '.
  it_down-p_a_nhce = ' '.
  it_down-p_p_hce = ' '.
  it_down-p_p_nhce = ' '.
  it_down-p_ar_hce = ' '.
  it_down-p_ar_nhce = ' '.
  INSERT it_down INDEX  1.
* 1
  it_down-zcost = ' '.
  it_down-ktext = ' '.
  it_down-zhedc = ' '.
  it_down-zjobk = ' '.
  it_down-a_q_hce = 'Qualified person'.
  it_down-a_q_nhce = ' '.
  it_down-a_q_noq = ' '.
  it_down-a_b_hce = 'Participant '.
  it_down-a_b_nhce = ' '.
  it_down-a_r_hce = 'Participant Rate'.
  it_down-a_r_nhce = ' '.
  it_down-a_a_hce = 'Actual amount '.
  it_down-a_a_nhce = ' '.
  it_down-a_p_hce = 'Actual pay '.
  it_down-a_p_nhce = ' '.
  it_down-a_ar_hce = 'Actual pay rate '.
  it_down-a_ar_nhce = ' '.

  it_down-p_q_hce = 'Qualified person '.
  it_down-p_q_nhce = ' '.
  it_down-p_q_noq = ' '.
  it_down-p_r_hce = 'Rate'.
  it_down-p_r_nhce = ' '.
  it_down-p_b_hce = 'Plan person'.
  it_down-p_b_nhce = ' '.
  it_down-p_a_hce = 'Plan amount '.
  it_down-p_a_nhce = ' '.
  it_down-p_ar_hce = 'Plan rate  '.
  it_down-p_ar_nhce = ' '.
  it_down-p_p_hce = 'Plan pay  '.
  it_down-p_p_nhce = ' '.
  INSERT it_down INDEX  2.

* 2
  it_down-zcost = 'Cost Center '.
  it_down-ktext = 'Name '.
  it_down-zhedc = 'Head count '.
  it_down-zjobk = 'Job '.
  it_down-a_q_hce = 'HCE'.
  it_down-a_q_nhce = 'NHCE'.
  it_down-a_q_noq = 'NOQ'.
  it_down-a_b_hce = 'HCE'.
  it_down-a_b_nhce = 'NHCE'.
  it_down-a_r_hce = 'HCE'.
  it_down-a_r_nhce = 'NHCE'.
  it_down-a_a_hce = 'HCE'.
  it_down-a_a_nhce = 'NHCE'.
  it_down-a_p_hce = 'HCE'.
  it_down-a_p_nhce = 'NHCE'.
  it_down-a_ar_hce = 'HCE'.
  it_down-a_ar_nhce = 'NHCE'.

  it_down-p_q_hce = 'HCE'.
  it_down-p_q_nhce = 'NHCE'.
  it_down-p_q_noq = 'NOQ'.
  it_down-p_b_hce = 'HCE'.
  it_down-p_b_nhce = 'NHCE'.
  it_down-p_r_hce = 'HCE'.
  it_down-p_r_nhce = 'NHCE'.
  it_down-p_a_hce = 'HCE'.
  it_down-p_a_nhce = 'NHCE'.
  it_down-p_p_hce = 'HCE'.
  it_down-p_p_nhce = 'NHCE'.
  it_down-p_ar_hce = 'HCE'.
  it_down-p_ar_nhce = 'NHCE'.
  INSERT it_down INDEX  3.

  IF wa_filename IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD wa_filename.
    IF sy-subrc NE 0.CLEAR  wa_filename.ENDIF.
  ENDIF.


  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = wa_filename
            def_path         = wa_filename
            mask             = ',*.xls.'
            mode             = 'S'
            title            = sy-title
       IMPORTING
            filename         = wa_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " EXCEL_DOWN_LOADING
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_401k
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_401k.
  DATA : it_00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.
  DATA : w_head TYPE i,
         w_head2 TYPE i,
         w_index TYPE sy-tabix,
         w_amunt LIKE zthr_pcp08-p_p_nhce,
         w_minus LIKE zthr_pcp08-p_p_nhce,
         w_moth  LIKE zthr_pcp08-p_p_nhce,
         w_other LIKE zthr_pcp08-p_p_nhce.

  DATA : it_401k LIKE zthr_pcp08 OCCURS 0 WITH HEADER LINE.


  SELECT * INTO TABLE it_00
           FROM zthr_pcp00
           WHERE zpera = w_werks
             AND zyear = w_zyear
             AND zvers = w_zvers
             AND NOT ( zperg = '9' AND zsubg = 'U2' ).

*  IF w_save = 'X'.
*    MESSAGE e000(zmhr) WITH 'Saved 401k'.
*  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE e000(zmhr) WITH 'NO date for update'.
  ENDIF.

  CLEAR : it_401k[], it_401k.

  LOOP AT it_9000 .
    MOVE-CORRESPONDING it_9000 TO it_401k.
    MOVE :
          w_zyear      TO  it_401k-zyear,
          w_zvers      TO  it_401k-zvers,
          w_werks      TO  it_401k-zpera,
          'USD'         TO it_401k-ancur,
          sy-datum      TO it_401k-erdat,
          sy-uname      TO it_401k-ernam,
          sy-uzeit      TO it_401k-erzet.

    APPEND it_401k. CLEAR it_401k.

  ENDLOOP.

  SORT it_00 BY zcost zobjc.

  LOOP AT it_9000.
    CLEAR : w_amunt, w_other, w_index, w_minus.
    w_amunt = it_9000-p_p_hce + it_9000-p_p_nhce.

    LOOP AT it_00 WHERE zcost = it_9000-zcost
                    AND zobjc = it_9000-zobjc
                    AND zsenr = it_9000-zsenr.
      w_index = sy-tabix.
      it_00-act10 = ( w_amunt / 12 ).
*     ( ( w_amunt / it_9000-zhedc )  * it_00-zhedc ) / 12    .

      w_minus = it_00-act10 + w_minus.

      MODIFY it_00.
    ENDLOOP.
    w_other = w_amunt - w_minus .
    it_00-act10 = it_00-act10 + w_other.
    MODIFY it_00 INDEX w_index.
  ENDLOOP.




  MODIFY zthr_pcp00 FROM TABLE it_00.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .
  ELSE.
    w_input = ''.
    w_save  = 'X'.
    MODIFY zthr_pcp08 FROM TABLE it_401k.
    IF sy-subrc EQ 0.
      COMMIT WORK.
      MESSAGE s001(zmhr) WITH 'Transaction was processed successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE e000(zmhr) WITH ' Dont Save !! call IT team ' .

    ENDIF.
  ENDIF.


ENDFORM.                    " SAVE_DATA_401k
*&---------------------------------------------------------------------*
*&      Form  data_401k_data_seletion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_401k_data_seletion.

  DATA : l_month TYPE i,
         l_401kmonth TYPE i,
         l_401km     TYPE i,
         l_year      TYPE i,
         l_zsaly LIKE zthr_pcpxx-zsaly,
         l_being TYPE d,
         l_end   TYPE d,
         r_rate LIKE zthr_pcp02-zval1,
         r_eepct LIKE pa0169-eepct,
         r_eep1 LIKE pa0169-eepct VALUE '0.6'.
  DATA : z_hedc LIKE zthr_pcp00-zhedc,
         z_zval2  LIKE zthr_pcp02-zval2,
         z_count LIKE zthr_pcp00-zhedc.


  CONCATENATE w_zyear '1231' INTO l_end.

  SELECT zcode zval1 INTO (zthr_pcp02-zcode, zthr_pcp02-zval1)
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1120'.
    IF zthr_pcp02-zcode = '10000'.
      MOVE zthr_pcp02-zval1 TO l_month.                     "12month
    ELSE.
      MOVE zthr_pcp02-zval1 TO l_zsaly .   "
    ENDIF.
  ENDSELECT.

  SELECT * INTO TABLE it_pcpxx
     FROM zthr_pcpxx
     WHERE zyear = w_zyear
*      and ZMONS = w_ZMONS
       AND zvers = w_zvers
      AND NOT ( zperg = '9' AND zsubg = 'U2' ) .


  LOOP AT it_pcpxx.
    MOVE : it_pcpxx-zcost TO it_9000-zcost,
           it_pcpxx-zobjc TO it_9000-zobjc,
           '1'            TO it_9000-zhedc,
           it_pcpxx-zsenr TO it_9000-zsenr.

* month calulation
    l_being = it_pcpxx-begda.
    CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
         EXPORTING
              beg_da   = l_being
              end_da   = l_end
         IMPORTING
              no_month = l_401kmonth
              no_year  = l_year.
    l_401kmonth = l_401kmonth + ( l_year * 12 ).


    l_401km = l_401kmonth - 12 .

    IF l_401km < 0.
      l_401km = 0.
    ENDIF.
** actual data
    IF l_401km > l_month.
      IF it_pcpxx-znhce > l_zsaly.
*  hce
        MOVE : '1'            TO it_9000-a_q_hce.
        it_9000-a_a_hce = it_pcpxx-zanhce.
        it_9000-a_p_hce =  it_pcpxx-z401k.
        IF it_9000-a_p_hce <> 0 .
          MOVE : '1'            TO it_9000-a_b_hce.
        ENDIF.
      ELSE.
        MOVE : '1'            TO it_9000-a_q_nhce.
        it_9000-a_a_nhce = it_pcpxx-zanhce.
        it_9000-a_p_nhce =  it_pcpxx-z401k.
        IF it_9000-a_p_nhce <> 0 .
          MOVE : '1'            TO it_9000-a_b_nhce.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE : '1'            TO it_9000-a_q_noq.
    ENDIF.
* Plan-------------
*Get Data : Increase pay rate
    DATA : it_temp LIKE zthr_pcp02 OCCURS 0 WITH HEADER LINE .
    REFRESH it_temp.
    CLEAR r_rate.

    SELECT  * INTO TABLE it_temp
      FROM zthr_pcp02
      WHERE zmodl = '02'
        AND zgrup = '1050'.

    READ TABLE  it_temp WITH KEY zval1 =  it_pcpxx-zobjc.
    IF sy-subrc = 0. "Production team member
      SELECT SINGLE zval1 INTO  r_rate
       FROM zthr_pcp02
         WHERE zmodl = '02'
           AND zgrup = '1060'
           AND zcode = '10030'.
    ELSE.
      SELECT SINGLE zval1 INTO  r_rate
        FROM zthr_pcp02
         WHERE zmodl = '02'
           AND zgrup = '1060'
           AND zcode = '10020'.
    ENDIF.

    IF l_401kmonth > l_month.
      it_pcpxx-znhce  = it_pcpxx-znhce +
             ( it_pcpxx-znhce  * r_rate ).
      IF it_pcpxx-znhce > l_zsaly.
*  hce
        MOVE : '1'            TO it_9000-p_q_hce.
        it_9000-p_a_hce = it_pcpxx-znhce .

      ELSE.
        MOVE : '1'            TO it_9000-p_q_nhce.
        it_9000-p_a_nhce = it_pcpxx-znhce .

      ENDIF.
    ELSE.
      MOVE : '1'            TO it_9000-p_q_noq.
    ENDIF.
    COLLECT it_9000. CLEAR it_9000.
  ENDLOOP.
  SORT it_9000 BY zcost zsenr.
*New cost center
  DATA : BEGIN OF lt_pcp00 OCCURS 0,
          zcost LIKE zthr_pcp00-zcost,
          zobjc LIKE zthr_pcp00-zobjc,
          zsenr LIKE zthr_pcp00-zsenr,
          zhedc LIKE zthr_pcp00-zhedc,
         END OF lt_pcp00 .
  CLEAR lt_pcp00[].

  SELECT zcost zobjc zsenr SUM( zhedc ) AS zhedc
     INTO TABLE lt_pcp00
        FROM zthr_pcp00
          WHERE zyear EQ w_zyear
            AND zvers EQ w_zvers
            AND NOT ( zperg = '9' AND zsubg = 'U2' )
            GROUP by zcost zobjc zsenr.
**because of exist new costcenter
*  LOOP AT lt_pcp00.
*    SELECT SINGLE * FROM csks WHERE kokrs EQ 'H201'
*                         AND kostl EQ lt_pcp00-zcost.
*    IF sy-subrc = 0.
*      DELETE  lt_pcp00.
*    ENDIF.
*  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_pcp00 COMPARING zcost zobjc zsenr .

  LOOP AT lt_pcp00.
    CLEAR : z_count,z_hedc,z_zval2.
    SELECT SINGLE * FROM zthr_pcp02
       WHERE zmodl EQ '02'
         AND ( zgrup = '1260' OR zgrup = '1270' )
         AND zctxt EQ lt_pcp00-zcost
         AND zval4 EQ lt_pcp00-zobjc.
    IF sy-subrc = 0 and lt_pcp00-zsenr  = 1.

      SELECT zval2 INTO z_zval2
        FROM zthr_pcp02
         WHERE zmodl EQ '02'
           AND ( zgrup = '1260' OR zgrup = '1270' )
           AND zctxt EQ lt_pcp00-zcost
           AND zval4 EQ lt_pcp00-zobjc.

        MOVE z_zval2+10(2) TO z_hedc.
        z_count = z_count + z_hedc.
      ENDSELECT.

      it_9000-zcost = lt_pcp00-zcost.
      it_9000-zobjc = lt_pcp00-zobjc.
      it_9000-zsenr = lt_pcp00-zsenr.
      it_9000-zhedc = z_count.
      it_9000-a_q_noq = z_count.

      APPEND it_9000.CLEAR : z_count,z_hedc.
    ENDIF.
  ENDLOOP.

  DATA : f_zhedc LIKE it_9000-zhedc.
  CLEAR f_zhedc.
  LOOP AT it_9000.
    READ TABLE lt_pcp00 WITH KEY zcost = it_9000-zcost
                                 zobjc = it_9000-zobjc
                                 zsenr = it_9000-zsenr.

    IF sy-subrc = 0.
      f_zhedc = ( lt_pcp00-zhedc / 12 ) - it_9000-zhedc.
      it_9000-a_q_noq = it_9000-a_q_noq + f_zhedc.
      it_9000-zhedc   = it_9000-zhedc  + f_zhedc.
      MODIFY it_9000 TRANSPORTING zhedc a_q_noq
       WHERE  zcost = it_9000-zcost
         AND  zobjc = it_9000-zobjc
         AND  zsenr = it_9000-zsenr.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " data_401k_data_seletion
