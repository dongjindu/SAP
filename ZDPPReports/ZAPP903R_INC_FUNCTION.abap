*----------------------------------------------------------------------*
***INCLUDE ZAPP704C_INC_FUNCTION .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GENERATE_DVRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_dvrt.
  DATA: l_data              LIKE TABLE OF it_data      WITH HEADER LINE.

  " Make the DVRT Layout
  SORT it_data BY status DESCENDING rd07 rs07 rd06 rs06 rd05 rs05 rd04
                                    rs04 rd03 rs03 rd02 rs02 rd01 rs01
                                    seq_date  seq_serial              .

  l_data[] = it_data[].

  " Fill the Time-Stamp.... in the production-line..
  CLEAR: wa_serial.
  LOOP AT l_data .
    wa_serial = wa_serial +  1 .
    l_data-serial = wa_serial.
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.
  it_data[] = l_data[].  CLEAR: l_data, l_data[].
ENDFORM.                    " GENERATE_DVRT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_count               TYPE i.

  GET TIME.
  WRITE AT: /001(40) 'Start of the Scheduling Table........' ,
             041(11) sy-datum,
             053(10) sy-uzeit.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data  TO    ztpp_input_plan    .
    MODIFY ztpp_input_plan      FROM  ztpp_input_plan    .
  ENDLOOP.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  PROCESSING_REMAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_RP  text
*----------------------------------------------------------------------*
FORM processing_remain               .
  DATA: l_flag            TYPE c     ,
        l_loops           TYPE i     ,
        l_index           TYPE i     ,
        l_pos             TYPE i     ,
        l_max             TYPE i     ,
        l_hours           TYPE i     ,
        lt_7jb            LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_7jb
    FROM ztpp_pmt07jb_a
   WHERE gubb NE '*'   .

  " 1 Hour * 20 & 10 Hour *  2 & 20 Hours.
  " Current Time check & Define the Loop count...
  CLEAR: l_flag.
  l_loops = 20 - wa_hour .
  DO l_loops TIMES.
    IF l_flag = 'X'.  EXIT.  ENDIF.
    l_pos = l_index + p_uph .
    IF l_pos >= l_max.
      l_pos = l_max.
      l_flag = 'X'.
    ENDIF.
    l_hours = l_hours + 1 .
    LOOP AT lt_7jb FROM l_index TO l_pos.
      CLEAR: it_sum.
      it_sum-hours = l_hours .
*     it_sum-vm_model = Lt_7jb-modl .
*     it_sum-vm_bodyser = Lt_7jb-body_ser.
      CONCATENATE lt_7jb-ordr lt_7jb-dist lt_7jb-extc lt_7jb-intc
             INTO it_sum-worder .
      it_sum-extc       = lt_7jb-extc       .
      it_sum-intc       = lt_7jb-intc       .
*     it_sum-status     = Lt_7jb-status     .
      it_sum-rp         = it_alc-rp         .
      it_sum-knnam      = it_alc-knnam      .
      CONCATENATE it_alc-type_alc it_alc-code INTO it_sum-code .
*     PERFORM get_alc_value  USING it_sum-worder  it_sum-code
*                         CHANGING it_sum-vals                 .
      APPEND it_sum.
    ENDLOOP.
    l_index = l_pos .
  ENDDO.
ENDFORM.                    " PROCESSING_REMAIN
