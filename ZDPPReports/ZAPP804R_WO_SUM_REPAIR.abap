************************************************************************
* Program Name      : ZAPP804R_WO_SUM_REPAIR
* Author            : BOBBY
* Creation Date     : 2004.01.26.
* Specifications By : B. Choi
* Pattern           : 2.1
* Development Request No : UD1K906372
* Addl Documentation:
* Description       : Table ZTPP_WOSUM Repair..
* Modification Logs
* Date       Developer    RequestNo    Description
*02/27/06    Manju
*01/26/07    Furong                    Overflow of itab it_temp1
************************************************************************
REPORT  zapp804r_wo_sum_repair    MESSAGE-ID zmpp.

TABLES : ztpp_wosum,ausp.  "ERP_WO QTY SUMMARY
INCLUDE <icon>.
INCLUDE <list>.

***********************************************************************
* GLOVAL VARIABLES DEFINITION.
***********************************************************************
DATA: wa_flag             TYPE  c.
DATA: BEGIN OF  it_data    OCCURS 0,
        objek LIKE ausp-objek,
        atwrt LIKE ausp-atwrt,
      END OF it_data.
DATA : l_data  LIKE TABLE OF zspp_vin_value
                         WITH HEADER LINE.

FIELD-SYMBOLS: <wa_field> TYPE ANY.
DATA: lt_wosum     LIKE TABLE OF ztpp_wosum          WITH HEADER LINE,
      lt_ausp      LIKE TABLE OF ausp                WITH HEADER LINE,
      l_pack(10)   TYPE c             ,
      l_name(40)   TYPE c             ,
      l_status(2)  TYPE n             ,
      l_size       TYPE i             ,
      l_atwrt      LIKE ausp-atwrt    ,
      l_worder     LIKE mara-matnr    ,
      w_char14(14),
      ll_count TYPE i.


DATA it_ztpp_wosum TYPE HASHED TABLE OF ztpp_wosum
WITH UNIQUE KEY wo_ser nation dealer extc intc .

TYPES : BEGIN OF it_line1,
          atinn LIKE ausp-atinn,
          objek LIKE ausp-objek,
        END OF it_line1.

DATA it_temp1 TYPE HASHED TABLE OF it_line1
WITH UNIQUE KEY atinn objek.

DATA : wa_temp LIKE LINE OF it_temp1.

DATA : wa_ztpp_wonum LIKE LINE  OF it_ztpp_wosum.

DATA : w_atwrt LIKE ausp-atwrt.

ranges: r_objek for ausp-objek.
***********************************************************************
* SELECTION SCREEN DEFINITION.
***********************************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
*PARAMETERS: p_month(5)     TYPE  c.
SELECT-OPTIONS s_wo FOR w_char14. "ZTPP_Wosum-wo_ser.
SELECTION-SCREEN  END OF BLOCK blk1.

***********************************************************************
INITIALIZATION.
***********************************************************************

***********************************************************************
AT SELECTION-SCREEN.
***********************************************************************

***********************************************************************
START-OF-SELECTION.
***********************************************************************
  PERFORM read_vehicle_master .
*  IF WA_FLAG = 'X'.
*    ROLLBACK WORK.
*    MESSAGE e000 WITH text-002 .
*  ELSE.
  COMMIT WORK.
  MESSAGE i001 WITH text-003 .
*  ENDIF.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
TOP-OF-PAGE.
*---------------------------------------------------------------------*
*

*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle_master.
  DATA : l_count      LIKE ztpp_wosum-seqqty,
         l_atinn1     LIKE ausp-atinn    ,    " P_WORK_ORDER
         l_atinn2     LIKE ausp-atinn    ,    " P_EXT_COLOR
         l_atinn3     LIKE ausp-atinn    ,    " P_INT_COLOR
         l_atinn4     LIKE ausp-atinn    ,    " P_USAGE_CAR NE 'D' & 'S'
         l_atinn5     LIKE ausp-atinn    ,    " P_RP_STATUS
         l_atinna     LIKE ausp-atinn    ,    " P_RP01_ACTUAL_DATE
         l_atinnb     LIKE ausp-atinn    ,    " P_RP02_ACTUAL_DATE
         l_atinnc     LIKE ausp-atinn    ,    " P_RP03_ACTUAL_DATE
         l_atinnd     LIKE ausp-atinn    ,    " P_RP04_ACTUAL_DATE
         l_atinne     LIKE ausp-atinn    ,    " P_RP05_ACTUAL_DATE
         l_atinnf     LIKE ausp-atinn    ,    " P_RP06_ACTUAL_DATE
         l_atinng     LIKE ausp-atinn    ,    " P_RP17_ACTUAL_DATE
         l_atinnh     LIKE ausp-atinn    ,    " P_RP18_ACTUAL_DATE
         l_atinni     LIKE ausp-atinn    ,    " P_RP19_ACTUAL_DATE
         l_atinnj     LIKE ausp-atinn    ,    " P_RP20_ACTUAL_DATE
         l_atinnk     LIKE ausp-atinn    ,    " P_RP21_ACTUAL_DATE
         l_atinnl     LIKE ausp-atinn    ,    " P_RP22_ACTUAL_DATE
         l_atinnm     LIKE ausp-atinn    ,    " P_RP23_ACTUAL_DATE
         l_atinnn     LIKE ausp-atinn    ,    " P_RP24_ACTUAL_DATE
         l_atinno     LIKE ausp-atinn    ,    " P_RP25_ACTUAL_DATE
         l_atinnp     LIKE ausp-atinn    ,    " P_RP26_ACTUAL_DATE
         l_atinnq     LIKE ausp-atinn    ,    " P_RP27_ACTUAL_DATE
         l_atinnr     LIKE ausp-atinn    .    " P_RP28_ACTUAL_DATE

*  CONCATENATE p_month '%' INTO      l_pack       .
  DATA: w_lenth TYPE i.
*  LOOP AT s_wo.
**    SEARCH s_wo-low FOR '*' ABBREVIATED..
**    IF sy-subrc <> 0.
*
*    if s_wo-low IS INITIAL.
*      if not s_wo-high IS INITIAL.
*        CONCATENATE s_wo-high '*' INTO s_wo-high.
*        MODIFY s_wo.
*      endif.
*    else.
*      if s_wo-high IS INITIAL.
*        s_wo-option = 'CP'.
*        CONCATENATE s_wo-low '*' INTO s_wo-low.
*        MODIFY s_wo.
*      else.
*        CONCATENATE s_wo-low '*' INTO s_wo-low.
*        CONCATENATE s_wo-high '*' INTO s_wo-high.
*        MODIFY s_wo.
*      endif.
*    endif.
*
*  ENDLOOP.

  PERFORM read_atinn USING 'P_WORK_ORDER'       l_atinn1.
  PERFORM read_atinn USING 'P_EXT_COLOR'        l_atinn2.
  PERFORM read_atinn USING 'P_INT_COLOR'        l_atinn3.
  PERFORM read_atinn USING 'P_USAGE_CAR'        l_atinn4.
  PERFORM read_atinn USING 'P_RP_STATUS'        l_atinn5.
*requested by hur, changed by wskim,on 2004.11.04
*-----Start
  REFRESH lt_ausp.CLEAR l_count.
*-----End
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
   WHERE klart = '002'
     AND atinn = l_atinn1
*     AND atwrt LIKE l_pack .
     AND atwrt IN s_wo.
*
  " Processing the Sequence Quantity Update...
  LOOP AT lt_ausp.
*-----It'll be checked at 'P_USAGE_CAR'
    SELECT SINGLE atwrt INTO l_atwrt
     FROM ausp
    WHERE objek = lt_ausp-objek
      AND klart = lt_ausp-klart
      AND atinn = l_atinn4.             "  P_USAGE_CAR

*    CONCATENATE lt_ausp-atwrt l_atwrt INTO lt_ausp-atwrt .
    IF sy-subrc = 0 AND ( l_atwrt = 'D' OR l_atwrt = 'S' ).
      DELETE lt_ausp  .
      CONTINUE.CLEAR l_atwrt.
    ENDIF.
*    " Attached the Color Information..
    SELECT SINGLE atwrt INTO l_atwrt
     FROM ausp
    WHERE objek = lt_ausp-objek
      AND atinn = l_atinn2             "P_EXT_COLOR
      AND klart = lt_ausp-klart .
    CONCATENATE lt_ausp-atwrt l_atwrt INTO lt_ausp-atwrt .

    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = lt_ausp-objek
       AND atinn = l_atinn3             "P_INT_COLOR
       AND klart = lt_ausp-klart .
    CONCATENATE lt_ausp-atwrt l_atwrt INTO lt_ausp-atwrt .

    MODIFY lt_ausp TRANSPORTING atwrt.
  ENDLOOP.
*-----End

*  SELECT  * into table it_ztpp_wosum
*    FROM ztpp_wosum.


  SORT lt_ausp BY atwrt.
  LOOP AT lt_ausp.
    IF lt_ausp-atwrt = l_worder.
      l_count = l_count + 1  .
      CONTINUE  .
    ELSE.
      IF l_worder = space.
        l_worder = lt_ausp-atwrt.
        l_count = 1.  CONTINUE.
      ENDIF.
      SELECT SINGLE *
        FROM ztpp_wosum
       WHERE wo_ser = l_worder(9)
         AND nation = l_worder+09(3)
         AND dealer = l_worder+12(2)
         AND extc   = l_worder+14(2)
         AND intc   = l_worder+16(2) .
*     read table it_ztpp_wosum into wa_ZTPP_WONUM
*         with table key wo_ser = l_worder(9)
*              nation = l_worder+09(3)
*              dealer = l_worder+12(2)
*              extc   = l_worder+14(2)
*              intc   = l_worder+16(2) .
* ztpp_wosum-seqqty  = wa_ZTPP_WONUM-seqqty.
      IF ztpp_wosum-seqqty NE l_count.
        PERFORM write_mismatch_wosum  USING l_worder l_count.
      ENDIF.
      l_worder = lt_ausp-atwrt.
      l_count = 1.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_ausp LINES l_size.
  IF l_size > 0.
    SELECT SINGLE *
      FROM ztpp_wosum
     WHERE wo_ser = l_worder(9)
       AND nation = l_worder+09(3)
       AND dealer = l_worder+12(2)
       AND extc   = l_worder+14(2)
       AND intc   = l_worder+16(2) .

    IF ztpp_wosum-seqqty NE l_count.
      PERFORM write_mismatch_wosum  USING l_worder l_count.
    ENDIF.
  ENDIF.
*requested by hur, changed by wskim,on 11/11/2004
*Reason : add new logic "update Seq quty at Work order head
*-----Start
  PERFORM work_head_update ."USING pa_worder pa_count.
*-----End
  " Processing the Status Quantity Update...
  CLEAR: l_status, l_count, l_size.

  LOOP AT lt_ausp.
    MOVE-CORRESPONDING lt_ausp TO it_data.
    APPEND it_data.
  ENDLOOP.

** end of change
  REFRESH lt_ausp. CLEAR lt_ausp.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_wosum
    FROM ztpp_wosum
    WHERE wo_ser IN s_wo.
*   WHERE wo_ser LIKE l_pack.

  PERFORM read_atinn USING 'P_RP01_ACTUAL_DATE' l_atinna.
  PERFORM read_atinn USING 'P_RP02_ACTUAL_DATE' l_atinnb.
  PERFORM read_atinn USING 'P_RP03_ACTUAL_DATE' l_atinnc.
  PERFORM read_atinn USING 'P_RP04_ACTUAL_DATE' l_atinnd.
  PERFORM read_atinn USING 'P_RP05_ACTUAL_DATE' l_atinne.
  PERFORM read_atinn USING 'P_RP06_ACTUAL_DATE' l_atinnf.
  PERFORM read_atinn USING 'P_RP17_ACTUAL_DATE' l_atinng.
  PERFORM read_atinn USING 'P_RP18_ACTUAL_DATE' l_atinnh.
  PERFORM read_atinn USING 'P_RP19_ACTUAL_DATE' l_atinni.
  PERFORM read_atinn USING 'P_RP20_ACTUAL_DATE' l_atinnj.
  PERFORM read_atinn USING 'P_RP21_ACTUAL_DATE' l_atinnk.
  PERFORM read_atinn USING 'P_RP22_ACTUAL_DATE' l_atinnl.
  PERFORM read_atinn USING 'P_RP23_ACTUAL_DATE' l_atinnm.
  PERFORM read_atinn USING 'P_RP24_ACTUAL_DATE' l_atinnn.
  PERFORM read_atinn USING 'P_RP25_ACTUAL_DATE' l_atinno.
  PERFORM read_atinn USING 'P_RP26_ACTUAL_DATE' l_atinnp.
  PERFORM read_atinn USING 'P_RP27_ACTUAL_DATE' l_atinnq.
  PERFORM read_atinn USING 'P_RP28_ACTUAL_DATE' l_atinnr.

*  PERFORM read_at_once USING l_atinna l_atinnb l_atinnc
*                             l_atinnd l_atinne l_atinnf
*                             l_atinng l_atinnh l_atinni
*                             l_atinnj l_atinnk l_atinnl
*                             l_atinnm l_atinnn l_atinnp
*                             l_atinno l_atinnq l_atinnr.
  CLEAR ll_count.
  LOOP AT lt_wosum.
    ADD 1 TO ll_count.
    CLEAR: w_atwrt, r_objek[].
    CONCATENATE lt_wosum-wo_ser lt_wosum-nation
         lt_wosum-dealer lt_wosum-extc lt_wosum-intc
         INTO w_atwrt.
** change by Furong on 01/29/2007
    r_objek-sign = 'I'.
    r_objek-option = 'EQ'.
    LOOP AT it_data WHERE atwrt EQ w_atwrt.
      r_objek-low = it_data-objek.
      append r_objek.
    endloop.
    if r_objek[] is initial.
       continue.
    endif.
** end of change
    PERFORM read_count_rp  USING l_atinna   lt_wosum-rp01tq.
    PERFORM read_count_rp  USING l_atinnb   lt_wosum-rp02tq.
    PERFORM read_count_rp  USING l_atinnc   lt_wosum-rp03tq.
    PERFORM read_count_rp  USING l_atinnd   lt_wosum-rp04tq.
    PERFORM read_count_rp  USING l_atinne   lt_wosum-rp05tq.
    PERFORM read_count_rp  USING l_atinnf   lt_wosum-rp06tq.
    PERFORM read_count_rp  USING l_atinng   lt_wosum-rp07tq.
    PERFORM read_count_rp  USING l_atinnh   lt_wosum-rp08tq.
    PERFORM read_count_rp  USING l_atinni   lt_wosum-rp09tq.
    PERFORM read_count_rp  USING l_atinnj   lt_wosum-rp10tq.
    PERFORM read_count_rp  USING l_atinnk   lt_wosum-rp11tq.
    PERFORM read_count_rp  USING l_atinnl   lt_wosum-rp12tq.
    PERFORM read_count_rp  USING l_atinnm   lt_wosum-rp13tq.
    PERFORM read_count_rp  USING l_atinnn   l_count        .
    lt_wosum-rp14tq =      l_count                         .

** change by Furong on 06/24/05
*    PERFORM read_count_rp  USING l_atinno   l_count        .
    PERFORM read_count_rp  USING l_atinnp   l_count        .
** end of change

    lt_wosum-rp14tq =      lt_wosum-rp14tq + l_count       .
** change by Furong on 06/24/05
*    PERFORM read_count_rp  USING l_atinnp   l_count        .
    PERFORM read_count_rp  USING l_atinno   l_count        .
** end of change

    lt_wosum-rp15tq =      l_count                         .
    PERFORM read_count_rp  USING l_atinnq   l_count        .
    lt_wosum-rp15tq =      lt_wosum-rp15tq + l_count       .
    PERFORM read_count_rp  USING l_atinnr   lt_wosum-rp16tq.
    ztpp_wosum = lt_wosum.
    MODIFY ztpp_wosum FROM ztpp_wosum.
    IF ll_count >= 10000.
      COMMIT WORK.
      CLEAR ll_count.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_VEHICLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  WRITE_MISMATCH_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM write_mismatch_wosum USING    pa_worder  pa_count.
  IF wa_flag = space.
    ULINE AT 053 .
    WRITE AT: /001(18) 'WORK ORDER' ,
               019(01) sy-vline ,
               020(06) 'WO SEQ. QTY',
               030(01) sy-vline ,
               031(10) 'VM SEQ. QTY',
               041(01) sy-vline     ,
               042(10) 'Return Code',
               052(01) sy-vline     .
    wa_flag = 'X'  .
    ULINE AT 053 .
  ENDIF.
  ADD 1 TO ll_count.
  UPDATE ztpp_wosum    SET seqqty = pa_count
                     WHERE wo_ser = pa_worder(9)
                       AND nation = pa_worder+09(3)
                       AND dealer = pa_worder+12(2)
                       AND extc   = pa_worder+14(2)
                       AND intc   = pa_worder+16(2) .
  IF ll_count >= 10000.
    COMMIT WORK.
    CLEAR ll_count.
  ENDIF.
*requested by hur, changed by wskim,on 11/11/2004
*Reason : add new logic "update Seq quty at Work order  color
*-----Start 1
  PERFORM work_color_update USING pa_worder pa_count.
*-----End
  WRITE AT: /001(18) pa_worder,
             019(01) sy-vline ,
             020(10) ztpp_wosum-seqqty,
             030(01) sy-vline ,
             031(10) pa_count ,
             042(10) sy-subrc ,
             052(01) sy-vline .
  ULINE AT 053 .
ENDFORM.                    " WRITE_MISMATCH_WOSUM

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0498   text
*      -->P_L_ATINNA  text
*----------------------------------------------------------------------*
FORM read_atinn USING    pa_atnam   pa_atinn.
  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_atnam .
ENDFORM.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Form  READ_COUNT_RP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ATINNA  text
*      -->P_LT_WOSUM_RP01TQ  text
*----------------------------------------------------------------------*
FORM read_count_rp USING    pa_atinn   pa_count .
*requested by hur, changed by wskim,on 2004.11.04
*reason : coding error and for good performance
*-----Start
  CLEAR pa_count.
*
*  DATA : BEGIN OF it_temp  OCCURS 0,
*          objek LIKE ausp-objek,
*         END OF it_temp.
*  REFRESH it_temp.
*
  SELECT count( * ) INTO pa_count
        FROM ausp
         WHERE  klart = '002'
          and atinn = pa_atinn
          AND atwrt NE space
          and objek in r_objek.

*  LOOP AT it_data WHERE atwrt EQ p_atwrt.
*** Chnaged by Furong on 11/10/2005 for performance issue
**   READ TABLE it_temp WITH KEY objek = it_data-objek.
**    READ TABLE it_temp WITH KEY objek = it_data-objek BINARY SEARCH.
*    READ TABLE it_temp1 INTO  wa_temp WITH TABLE KEY atinn = pa_atinn
*                                       objek = it_data-objek  .
*
*** end of change
*    IF sy-subrc = 0.
*      pa_count = pa_count + 1.
*    ENDIF.
*  ENDLOOP.
*
*  SELECT COUNT( * ) INTO pa_count
*    FROM ausp
*       WHERE objek IN it_data
*          AND atinn = pa_atinn
*          AND klart = '002'
*          AND atwrt NE space   .
*-----End

ENDFORM.                    " READ_COUNT_RP
*&---------------------------------------------------------------------*
*&      Form  work_head_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_WORDER  text
*      -->P_PA_COUNT  text
*----------------------------------------------------------------------*
FORM work_head_update ."USING    lp_worder
  "       lp_count.
  DATA : BEGIN OF it_headc OCCURS 0 ,
          atwrt(14)," LIKE ausp-atwrt,
          count TYPE i,
         END OF it_headc.
  DATA : l_count TYPE i.
  DATA: pa_material             LIKE mara-matnr  .

  l_count = 1.
  LOOP AT lt_ausp.
    MOVE lt_ausp-atwrt(14) TO it_headc-atwrt.
    MOVE l_count TO it_headc-count.
    COLLECT it_headc.
  ENDLOOP.

  CLEAR pa_material.
  LOOP AT it_headc.
    REFRESH l_data.
    l_data-atnam = 'P_SEQ_QTY'.
    l_data-atwrt = it_headc-count.
    APPEND l_data.
    pa_material = it_headc-atwrt.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = pa_material
              ctype        = '001'
              mode         = 'W'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.
  ENDLOOP.

ENDFORM.                    " work_head_update
*&---------------------------------------------------------------------*
*&      Form  work_color_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_WORDER  text
*      -->P_PA_COUNT  text
*----------------------------------------------------------------------*
FORM work_color_update USING    lp_worder
                                lp_count.
  DATA: pa_material             LIKE mara-matnr  .
  REFRESH l_data.
  CLEAR pa_material.
  l_data-atnam = 'P_SEQ_QTY'.
  l_data-atwrt = lp_count.
  APPEND l_data.
  pa_material = lp_worder(18).

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            ctype        = '001'
            mode         = 'W'
       TABLES
            val_table    = l_data
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

ENDFORM.                    " work_color_update
*&---------------------------------------------------------------------*
*&      Form  Read_at_once
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_at_once USING l_atinna  l_atinnb l_atinnc l_atinnd l_atinne
                        l_atinnf  l_atinng l_atinnh l_atinni l_atinnj
                        l_atinnk  l_atinnl l_atinnm l_atinnn l_atinnp
                        l_atinno l_atinnq l_atinnr.
  RANGES : r_atinn FOR ausp-atinn.
  r_atinn-option = 'EQ'.
  r_atinn-sign = 'I'.
  r_atinn-low = l_atinna .
  APPEND r_atinn.
  r_atinn-low = l_atinnb .
  APPEND r_atinn.
  r_atinn-low = l_atinnc  .
  APPEND r_atinn.
  r_atinn-low = l_atinnd  .
  APPEND r_atinn.
  r_atinn-low = l_atinne .
  APPEND r_atinn.
  r_atinn-low = l_atinnf .
  APPEND r_atinn.
  r_atinn-low = l_atinng .
  APPEND r_atinn.
  r_atinn-low = l_atinnh  .
  APPEND r_atinn.
  r_atinn-low = l_atinni  .
  APPEND r_atinn.
  r_atinn-low = l_atinnj  .
  APPEND r_atinn.
  r_atinn-low = l_atinnk  .
  APPEND r_atinn.
  r_atinn-low = l_atinnl  .
  APPEND r_atinn.
  r_atinn-low = l_atinnm  .
  APPEND r_atinn.
  r_atinn-low = l_atinnn  .
  APPEND r_atinn.
  r_atinn-low = l_atinnp  .
  APPEND r_atinn.
  r_atinn-low = l_atinno  .
  APPEND r_atinn.
  r_atinn-low = l_atinnq  .
  APPEND r_atinn.
  r_atinn-low = l_atinnr  .
  APPEND r_atinn.
** change by Furong on 01/26/2007
*  SELECT atinn objek  INTO TABLE it_temp1
*      FROM ausp
*       WHERE  klart = '002'
*        AND atinn IN r_atinn
*        AND atwrt NE space   .
*
  SELECT atinn objek  INTO TABLE it_temp1
      FROM ausp
      for all entries in it_data
       WHERE  klart = '002'
        AND atinn IN r_atinn
        AND atwrt NE space
        and objek = it_data-objek.
** end of change
ENDFORM.                    " Read_at_once
