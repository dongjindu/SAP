************************************************************************
* Program Name      : ZAPP704C_VEHICLE_SCHEDULING
* Author            : Bobby
* Creation Date     : 2003.09.23.
* Specifications By : Bobby
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : APP704: VEHICLE SCHEDULING
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  zapp704c_vehicle_scheduling   MESSAGE-ID zmpp.

TABLES: zvpp_vehicle,
        ztpp_dvrt1  ,
        ztpp_dvrt2  ,
        ausp ,
        crhd .

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_sctype     TYPE zpp_tack    OBLIGATORY
                                        ,     " Basis of Scheduling
             p_plnnr      LIKE rc271-plnnr OBLIGATORY
                                           ,  " Reference Rate Routing
             p_arbpll     LIKE crhd-arbpl  OBLIGATORY
                                           .  " Line Design
SELECT-OPTIONS:
             s_arbpls     FOR  crhd-arbpl  OBLIGATORY
                                           .  " Shop
PARAMETERS:
             p_dvrt       TYPE zpp_dvrt    OBLIGATORY
                                        ,     " DVRT
             p_ptype      TYPE zplan_type  OBLIGATORY
                                          .   " Plan Order date Update
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: wa_wdate     TYPE d,
            wa_uzeit     TYPE t.
SELECTION-SCREEN END OF BLOCK b2.

DATA: wa_total_tack       TYPE i                                   ,
      wa_uph              LIKE zvpp_ld-lrate                       ,
      wa_time_flg         TYPE c                                   ,
      wa_ctimestamp(14)   TYPE c                                   ,
      wa_serial           LIKE ztpp_dvrt1-serial                   ,
      wa_etime            TYPE t                                   ,
      wa_stime            TYPE t                                   ,
      it_7jb              LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE,
      it_ld               LIKE TABLE OF zvpp_ld        WITH HEADER LINE,
      it_mas_line         LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_shop         LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_wc           LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_capa         LIKE TABLE OF zvpp_capacity  WITH HEADER LINE,
      it_mas_rrr          LIKE TABLE OF zvpp_rp1       WITH HEADER LINE,
      it_mas_rr           LIKE TABLE OF zvpp_rp2       WITH HEADER LINE,
      it_37p              LIKE TABLE OF tc37p          WITH HEADER LINE.

DATA: BEGIN OF it_tims    OCCURS 0,
        arbpl              LIKE crhd-arbpl,       " Shop
        wdate              TYPE d         ,       " Valid Date
        cdate              TYPE d         ,       " Calendar Date
        tagnr              LIKE kapa-tagnr,       " Day
        tprog              LIKE kapa-tprog,       " Shift Profile
        s_stime            TYPE t         ,
        s_etime            TYPE t         ,
        sprog              LIKE kapa-tprog,
        paunr              LIKE tc37p-paunr,
        w_stime            TYPE t         ,
        c_stime            LIKE kapa-begzt,
        w_etime            TYPE t         ,
        c_etime            LIKE kapa-begzt,
        flag               TYPE c         ,
      END OF it_tims .

DATA: BEGIN OF it_temp    OCCURS 0,
        objek             LIKE ausp-objek,      " EQUI-EQUNR
        atinn             LIKE ausp-atinn,      " CHARACTERISTIC
        atwrt             LIKE ausp-atwrt,      " CHARACTERISTIC VAL
        atflv             LIKE ausp-atflv,      " Date & Number  VAL
      END OF it_temp .

DATA: BEGIN OF it_takts   OCCURS 0,
        sortb             LIKE zvpp_rp1-sortb,  " Sort String
        line              LIKE crhd-arbpl,
        shop              LIKE crhd-arbpl,
        arbpl             LIKE crhd-arbpl,
        takts             TYPE i         ,
        t_time            TYPE p DECIMALS 1,    " Time Stamp..
        l_time            TYPE p DECIMALS 1,    " Time Stamp..
      END OF it_takts .

DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_dvrt1 .
DATA:   vin(18)               TYPE c         ,      " VINN No.
        fsc                   LIKE mara-matnr,      " FSC
        rp                    LIKE ztpp_status-vm_id,
        arbpl                 LIKE zvpp_rp1-arbpl,  " Work Center(RP)
        usr00                 LIKE zvpp_rp1-usr00,  " Supply Area
*       ac_date               LIKE ausp-atwrt    ,  " Confirm Date&Time
        rp_no(2)              TYPE c             ,  " RP No
*       ac_shop               LIKE sy-datum      ,  " Confirm Date
        uph                   LIKE zvpp_ld-lrate ,  " UPH
      END OF it_data.

DATA: wa_data                 LIKE it_data       ,
      it_break                LIKE TABLE OF it_tims    WITH HEADER LINE,
      it_data2                LIKE TABLE OF it_data    WITH HEADER LINE.

FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.

INITIALIZATION.
  PERFORM setting_default.

AT SELECTION-SCREEN ON wa_uzeit.
  PERFORM check_start_timestamp.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM test_write.
  PERFORM loading_master.
  PERFORM calc_scheduling.
  PERFORM generate_dvrt.
  PERFORM update_date  .

END-OF-SELECTION.
  PERFORM display_data.
  GET TIME.
  WRITE AT: /001(40) 'End of the All Processing ...........' ,
             041(11) sy-datum,
             053(10) sy-uzeit.

*&---------------------------------------------------------------------*
*&      Form  SETTING_DEFAULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_default.
  p_sctype = '2'    .
  p_plnnr  = 'RP'   .
  p_arbpll = '1'    .
  s_arbpls = 'IBTB' .  s_arbpls-high = 'T'.  APPEND s_arbpls.
  p_dvrt   = '1'    .
  p_ptype  = 'N'    .
  wa_etime = '040000'.
  wa_stime = '063000'.
  GET TIME.
  wa_wdate = sy-datum.
  wa_uzeit = sy-uzeit.
ENDFORM.                    " SETTING_DEFAULT

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_loops             TYPE i                ,
        l_lnsid             LIKE zvpp_ld-lnid     ,
        l_arbpl             LIKE crhd-arbpl       ,
        lw_tims             LIKE it_tims          ,
        l_tack              LIKE TABLE OF it_takts     WITH HEADER LINE.

  GET TIME.
  WRITE AT: /001(40) 'Start of the Reading Master Information',
             041(11) sy-datum,
             053(10) sy-uzeit.

  CONCATENATE wa_wdate wa_uzeit  INTO wa_ctimestamp.

  " Select the Reference Rate Routing   Information...
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_rrr
    FROM zvpp_rp1
   WHERE plnnr = p_plnnr .

  " Read Line Design  Information....
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_line
    FROM crhd
   WHERE verwe = '0010' .

  " Read Shop   Information.....
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_shop
    FROM crhd
   WHERE verwe = '0011'
     AND arbpl IN s_arbpls .

  " Read the Shop's Capacity - Working & Break Time Shcedule..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_capa
    FROM zvpp_capacity
   WHERE arbpl IN s_arbpls
     AND datub GE wa_wdate .

  PERFORM create_capacity.
*  READ TABLE it_tims INDEX 1  INTO lw_tims.
*  LOOP AT it_tims.
*    IF it_tims-wdate = lw_tims-wdate.
*      lw_tims = it_tims.
*    ELSE.
*      it_tims-w_stime = lw_tims-w_etime.
*      lw_tims = it_tims.
*      MODIFY it_tims   .
*    ENDIF.
*  ENDLOOP.

  " Read Rate Routing Information.....
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_rr
    FROM zvpp_rp2
   WHERE matnr = 'AA'    .

  " Read the value of UPH
  CLEAR: it_ld, it_ld[].
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ld
    FROM zvpp_ld
   WHERE ld_perst <= wa_wdate
     AND ld_pered >= wa_wdate .

  IF sy-subrc = 0.
    READ TABLE it_ld INDEX 1.
    wa_uph = it_ld-lrate    .
  ENDIF.

  " Founding the Value of the tack......
  CLEAR: wa_total_tack, l_arbpl.
  IF p_sctype = '1'  .                     " Actual no of tack...
    CLEAR: wa_total_tack  .
    READ TABLE it_ld INDEX 1.
    SELECT SINGLE arbpl INTO it_takts-line
      FROM crhd
     WHERE objid = it_ld-lnid.

    LOOP AT it_mas_rr.
      it_takts-takts = it_mas_rr-takt .
      it_takts-arbpl = it_mas_rr-arbpl.
      READ TABLE it_mas_rrr WITH KEY arbpl = it_mas_rr-arbpl.
      it_takts-sortb = it_mas_rrr-sortb .
      READ TABLE it_ld WITH KEY arbpl = it_mas_rr-arbpl.
      SELECT SINGLE LNSID_UP  inTO l_lnsid
        FROM ZVPP_ld_old
       WHERE ARBPL = it_mas_rr-arbpl .
      select single arbpl into it_takts-shop
        from crhd
       where objid = l_lnsid.

      it_takts-t_time = 60 * 60                  / wa_uph .   " Sec
*     it_takts-l_time = 60 * it_mas_rr-takt * 60 / wa_uph .   " Sec
      it_takts-l_time = it_ld-takt                        .   " Sec
      APPEND it_takts.
    ENDLOOP.
  ELSE .                                    " Plan no of tack...
    SORT it_ld BY lnid                .
    READ TABLE it_ld INDEX 1 .

    SELECT SINGLE arbpl INTO it_takts-line
      FROM crhd
     WHERE objid = it_ld-lnid .

    loop at it_mas_rrr.
      it_takts-arbpl = it_mas_rrr-arbpl.    " it_ld-arbpl.
      SELECT SINGLE LNSID_UP  inTO l_lnsid
        FROM ZVPP_ld_old
       WHERE ARBPL = it_mas_rrr-arbpl .
      select single arbpl into it_takts-shop
        from crhd
       where objid = l_lnsid.
      READ TABLE it_mas_rr  WITH KEY arbpl = it_mas_rrr-arbpl.
      it_takts-takts = it_mas_rr-takt .
      it_takts-sortb = it_mas_rrr-sortb .
      it_takts-t_time = 60 * 60               / wa_uph .   " Sec
      it_takts-l_time = 60 * it_mas_rr-takt * 60 / wa_uph. " Sec
*     it_takts-l_time = 60 * it_ld-takt  * 60 / wa_uph .   " Sec
      APPEND it_takts.
    ENDLOOP.
  ENDIF.

  " Founding the Sign-Off Reporting Point & Read the VINN
  LOOP AT it_mas_rrr WHERE steus = 'PP99' .
    PERFORM read_vehicle_master  USING it_mas_rrr-sortb(2).
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  LOADING_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM loading_master.
  DELETE FROM ztpp_dvrt1 WHERE modl NE space.
  IF p_dvrt = 2    .
    DELETE FROM ztpp_dvrt2 WHERE modl NE space..
  ENDIF.
ENDFORM.                    " LOADING_MASTER

*&---------------------------------------------------------------------*
*&      Form  CALC_SCHEDULING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_scheduling.
  DATA:l_sdate                TYPE d,
       l_tdate                TYPE d.

  " DVRT2 Appending...
  CHECK  p_dvrt = '2' .
  l_sdate = wa_wdate.    l_tdate = wa_wdate + 31 .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE sqdt > l_sdate
     AND sqdt < l_tdate
     AND gubb NE '*'   .

  SORT it_7jb BY sqdt ssr1 .
ENDFORM.                    " CALC_SCHEDULING

*&---------------------------------------------------------------------*
*&      Form  GENERATE_DVRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_dvrt.
  DATA: l_sdat              LIKE sy-datum         ,
        l_time              LIKE sy-uzeit         ,
        l_addtime           LIKE sy-uzeit         ,
        l_flag              TYPE c                ,
        l_flag2             TYPE c                ,
        l_dname(30)         TYPE c                ,
        l_no(2)             TYPE n                ,
        l_no2(2)            TYPE n                ,
        l_pre(2)            TYPE n                ,
        l_status            LIKE ztpp_dvrt1-status,
        l_data              LIKE TABLE OF it_data      WITH HEADER LINE.

  GET TIME.
  WRITE AT: /001(40) 'Start of the Creating the TimeStamp..' ,
             041(11) sy-datum,
             053(10) sy-uzeit.

  " Make the DVRT Layout
  SORT it_data BY status DESCENDING rp18 rp17 rp16 rp15 rp14 rp13 rp12
                         rp11 rp10  rp09 rp08 rp07 rp06 rp05 rp04 rp03
                         rp02 rp01  seq_date  seq_serial              .

  l_data[] = it_data[].

  " Fill the Time-Stamp.... in the production-line..
  LOOP AT l_data WHERE status > '00'.
    wa_serial = wa_serial +  1 .
    " Find the lead time
    l_no = 1.                     CLEAR: wa_time_flg         .
    l_sdat = l_data-rp01(8).      l_time = l_data-rp01+8(6)  .
    CONCATENATE l_data-rp01       'A'      INTO  l_data-rp01 .
    l_flag2   = 'X'             .
    IF l_data-status = l_status .
      l_flag  = 'X'             .
    ELSE.
      CLEAR: l_flag .
      l_status = l_data-status.
    ENDIF.
    DO 17 TIMES.
      l_no = l_no + 1 .
      CONCATENATE 'L_DATA-RP'              l_no  INTO l_dname.
      ASSIGN (l_dname)                     TO    <wa_dfield> .
      IF <wa_dfield> > space .
        CONCATENATE <wa_dfield>   'A'      INTO  <wa_dfield> .
        l_sdat = <wa_dfield>(8).  l_time = <wa_dfield>+8(6)  .
        CONTINUE.
      ELSE.
*       READ TABLE it_takts WITH KEY sortb = l_no .
        l_no2 = l_no - 1 .
        READ TABLE it_takts WITH KEY sortb = l_no2.
        IF l_flag = 'X' .
*            l_pre  = l_no - 1.
            CONCATENATE 'WA_DATA-RP'   l_no  INTO l_dname.
            ASSIGN (l_dname)                 TO   <wa_dfield> .
            l_sdat = <wa_dfield>(8).         l_time = <wa_dfield>+8(6).
            clear: l_flag, l_flag2 .
*          ENDIF.
        ELSE.
          IF l_flag2 = 'X'.
            l_sdat = wa_wdate      .         l_time = wa_uzeit.
            CLEAR: it_takts-l_time,  l_flag2 .
          ELSE.
            l_pre  = l_no - 1.
            CONCATENATE 'L_DATA-RP'   l_pre INTO l_dname.
            ASSIGN (l_dname)                 TO   <wa_dfield> .
            l_sdat = <wa_dfield>(8).         l_time = <wa_dfield>+8(6).
          ENDIF.
        ENDIF.
        l_addtime = it_takts-l_time .
        " Find the Workcenter for the Reporting Point..
        PERFORM check_timestamp  USING l_sdat  l_time  l_addtime
                                       it_takts-shop   .      " l_arbpl
        CONCATENATE 'L_DATA-RP'  l_no INTO l_dname.
        ASSIGN (l_dname)              TO   <wa_dfield> .
        CONCATENATE l_sdat l_time 'P' INTO <wa_dfield> .
        if l_no = 1.
           l_data-b = l_sdat.
        endif.
        if l_no = 7.
           l_data-t = l_sdat.
        endif.
      ENDIF.
    ENDDO.
    l_data-serial = wa_serial.
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.

  " Fill the Time-Stamp.... Waiting VIN before the production-line..
  CONCATENATE wa_wdate wa_uzeit INTO wa_data-rp01.
  LOOP AT l_data WHERE status = '00' AND mitu = space .
    l_no = 1.
    wa_serial = wa_serial + 1.
    READ TABLE it_takts WITH KEY sortb = l_no .
    l_sdat = wa_data-rp01(8).  l_time = wa_data-rp01+8(6)  .
    l_addtime = it_takts-t_time .
    PERFORM check_timestamp  USING l_sdat  l_time  l_addtime
                                   it_takts-shop      .      " l_arbpl
    CONCATENATE l_sdat l_time 'P'  INTO l_data-rp01   .
    CONCATENATE 'L_DATA-RP'  l_no  INTO l_dname.
    ASSIGN (l_dname)               TO   <wa_dfield> .
    l_data-b = l_sdat.

    DO 17 TIMES.
      l_sdat = <wa_dfield>(8).  l_time = <wa_dfield>+8(6) .
      l_no2 = l_no.             l_no = l_no + 1 .
      READ TABLE it_takts WITH KEY sortb = l_no2.
      l_addtime = it_takts-l_time .
      " Find the Workcenter for the Reporting Point..
      PERFORM check_timestamp  USING l_sdat  l_time  l_addtime
                                     it_takts-shop      .      " l_arbpl
      CONCATENATE 'L_DATA-RP'  l_no INTO l_dname.
      ASSIGN (l_dname)              TO   <wa_dfield> .
      CONCATENATE l_sdat l_time 'P' INTO <wa_dfield> .
        if l_no = 1.
           l_data-b = l_sdat.
        endif.
        if l_no = 7.
           l_data-t = l_sdat.
        endif.
    ENDDO.
    l_data-serial = wa_serial + 1 .
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.
  it_data[] = l_data[].  CLEAR: l_data, l_data[].

  CHECK p_dvrt = '2'  .
  " Fill the Time-Stamp.... (7JB DATA...)
  CLEAR: l_data, l_data[].

  LOOP AT it_7jb.
    wa_serial = wa_serial + 1 .
    l_data-seq_date  = it_7jb-sqdt.
    l_data-seq_code  = it_7jb-sqcd.
    l_data-seq_serial = it_7jb-ssr1.
    l_data-status    = '00'       .
    l_data-plnt      = it_7jb-plnt.
    CONCATENATE it_7jb-ordr it_7jb-dist INTO l_data-work_order.
    l_data-extc      = it_7jb-extc.
    l_data-intc      = it_7jb-intc.
    l_data-line      = it_7jb-line.
    l_data-modl      = it_7jb-modl.
    l_data-mi        = it_7jb-bmdl.
    l_data-ocnn      = it_7jb-ocnn.
    l_data-vers      = it_7jb-vers.
    l_data-serial    = wa_serial  .
    APPEND l_data.
  ENDLOOP.

  LOOP AT l_data .
    wa_serial = wa_serial +  1 .
    l_no = 1.
    READ TABLE it_takts WITH KEY sortb = l_no .
    l_sdat = wa_data-rp01(8).  l_time = wa_data-rp01+8(6)  .
    l_addtime = it_takts-t_time .
    PERFORM check_timestamp  USING l_sdat  l_time  l_addtime
                                   it_takts-shop      .      " l_arbpl .
    CONCATENATE l_sdat l_time 'P'  INTO l_data-rp01   .
    CONCATENATE 'L_DATA-RP'  l_no  INTO l_dname.
    ASSIGN (l_dname)               TO   <wa_dfield> .
    l_data-b = l_sdat.

    DO 17 TIMES.
      l_sdat = <wa_dfield>(8).  l_time = <wa_dfield>+8(6) .
      l_no2  = l_no.            l_no = l_no + 1 .
      READ TABLE it_takts WITH KEY sortb = l_no2.
      l_addtime = it_takts-l_time .
      " Find the Workcenter for the Reporting Point..
      PERFORM check_timestamp  USING l_sdat  l_time  l_addtime
                                     it_takts-shop      .      " l_arbpl

      CONCATENATE 'L_DATA-RP'  l_no INTO l_dname.
      ASSIGN (l_dname)              TO   <wa_dfield> .
      CONCATENATE l_sdat l_time 'P' INTO <wa_dfield> .
      if l_no = 7.
         l_data-t = l_sdat.
      endif.
    ENDDO.
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.
  it_data2[] = l_data[].
ENDFORM.                    " GENERATE_DVRT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_date  .
  DATA: l_n(10)             TYPE n,
        l_plnum             LIKE bapi_pldord-pldord_num,
        l_header            LIKE bapiplaf_i2 ,
        l_headerx           LIKE bapiplaf_i2x,
        l_return            LIKE bapireturn1 ,
        l_sdat              LIKE plaf-psttr  ,
        l_fdat              LIKE plaf-pedtr  .

  CHECK p_ptype = 'Y' .
  GET TIME.
  WRITE AT: /001(40) 'Start of the Pland-Order Creating....' ,
             041(11) sy-datum,
             053(10) sy-uzeit.
  LOOP AT it_data.
    SELECT SINGLE psttr pedtr INTO (l_sdat, l_fdat)
      FROM plaf
     WHERE plnum = it_data-plnum.
    IF it_data-rp01(8) = l_sdat  AND
       it_data-rp18(8) = l_fdat  .
      CONTINUE.
    ELSE.
      l_plnum = l_n = it_data-plnum.
      l_header-order_start_date = it_data-rp01(8)  .
      l_header-order_fin_date   = it_data-rp18(8)  .
      l_headerx-order_start_date = 'X' .
      l_headerx-order_fin_date   = 'X' .

      CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
           EXPORTING
                plannedorder = l_plnum
                headerdata   = l_header
                headerdatax  = l_headerx
           IMPORTING
                return       = l_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .

      WRITE AT: /001(132) l_return-message    .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPDATE_DATE

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
    MOVE-CORRESPONDING it_data  TO  ztpp_dvrt1      .
    ZTPP_DVRT1-ZUSER = SY-UNAME                     .
    ZTPP_DVRT1-ZSDAT = SY-DATUM                     .
    ZTPP_DVRT1-ZSTIM = SY-UZEIT                     .
    MODIFY ztpp_dvrt1  FROM  ztpp_dvrt1             .
  ENDLOOP.

  DESCRIBE TABLE it_data2 LINES l_count.
  IF l_count > 0.
    APPEND LINES OF it_data2  TO it_data           .
    it_data2[] = it_data[]                         .
  ENDIF.

  LOOP AT it_data2.
    MOVE-CORRESPONDING it_data2  TO  ztpp_dvrt2     .
    ZTPP_DVRT2-ZUSER = SY-UNAME                     .
    ZTPP_DVRT2-ZSDAT = SY-DATUM                     .
    ZTPP_DVRT2-ZSTIM = SY-UZEIT                     .
    MODIFY ztpp_dvrt2  FROM  ztpp_dvrt2             .
  ENDLOOP.
ENDFORM.                    " DISPLAY_DATA


* INCLUDE PROGRAM.....
INCLUDE zapp704c_inc_function.

*&---------------------------------------------------------------------*
*&      Form  test_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_write.
  LOOP AT it_tims.
    WRITE AT: /001(010) it_tims-arbpl,
               011(010) it_tims-wdate,
               022(010) it_tims-cdate,
               033(006) it_tims-tagnr,
               040(009) it_tims-s_stime,
               050(009) it_tims-s_etime,
               060(009) it_tims-w_stime,
               070(009) it_tims-w_etime,
               080(005) it_tims-flag.
  ENDLOOP.
ENDFORM.                    " test_write

*&---------------------------------------------------------------------*
*&      Form  CHECK_START_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_start_timestamp.
  DATA: l_date              TYPE d,
        l_cdate             TYPE d,
        l_timstamp(20)      TYPE c.

  l_date = wa_wdate.
  PERFORM call_workday  USING 'B'      l_date  .
  IF l_date = wa_wdate.                " Working Date...
    IF wa_uzeit < '063500' .
      wa_uzeit = '063500' .
      CONCATENATE l_date wa_uzeit INTO l_timstamp SEPARATED BY space.
      MESSAGE w003 WITH 'Start Timestamp is Reset...'
                        l_timstamp    ' is Start time..'.
    ENDIF.
  ELSE.                                " Holiday
    l_cdate = l_date = wa_wdate - 1 .
    PERFORM call_workday  USING 'B'      l_date  .
    IF wa_uzeit < '040000' AND l_date = l_cdate  .
    ELSE.
      PERFORM call_workday  USING 'B'      wa_wdate.
      wa_uzeit = '063500' .
      CONCATENATE wa_wdate wa_uzeit INTO
                                       l_timstamp SEPARATED BY space.
      MESSAGE w003 WITH 'Start Timestamp is Reset...'
                        l_timstamp    ' is Start time..'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_START_TIMESTAMP
