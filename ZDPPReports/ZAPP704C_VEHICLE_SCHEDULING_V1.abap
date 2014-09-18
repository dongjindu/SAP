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
* Back-Up: Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  zapp704c_vehicle_scheduling   .

TABLES: zvpp_vehicle,
        ytpp_dvrt1  ,
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

DATA: wa_total_tack       TYPE i                                   ,
      wa_uph              LIKE zvpp_ld-lrate                       ,
      it_7jb              LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE,
      it_ld               LIKE TABLE OF zvpp_ld_old    WITH HEADER LINE,
      it_mas_line         LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_shop         LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_wc           LIKE TABLE OF crhd           WITH HEADER LINE,
      it_mas_capa         LIKE TABLE OF zvpp_capacity  WITH HEADER LINE,
      it_mas_rrr          LIKE TABLE OF zvpp_rp1       WITH HEADER LINE,
      it_mas_rr           LIKE TABLE OF zvpp_rp2       WITH HEADER LINE.

DATA: BEGIN OF it_capa    OCCURS 0,
        arbpl             LIKE crhd-arbpl,      " Shop
        ftime             TYPE t         ,      " From Time
        ttime             TYPE t         ,      " To   Time
        wflag             TYPE c         ,      " Break & Working
      END OF it_capa .

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
      it_data2                LIKE TABLE OF it_data    WITH HEADER LINE.

FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.

INITIALIZATION.
  PERFORM setting_default.

START-OF-SELECTION.
  PERFORM get_data.
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
  s_arbpls = 'IEQB' .  APPEND s_arbpls.
  p_dvrt   = '1'    .
  p_ptype  = 'N'    .
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
        l_lnsid             LIKE zvpp_ld_old-lnsid_up ,
        l_arbpl             LIKE crhd-arbpl       ,
        l_tack              LIKE TABLE OF it_takts     WITH HEADER LINE.

  GET TIME.
  WRITE AT: /001(40) 'Start of the Reading Master Information',
             041(11) sy-datum,
             053(10) sy-uzeit.
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
     AND datub GE sy-datum .

  PERFORM CREATE_CAPACITY.

  " Read Rate Routing Information.....
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mas_rr
    FROM zvpp_rp2
   WHERE matnr = 'AA'    .

  " Read the value of UPH
  CLEAR: it_ld, it_ld[].
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ld
    FROM zvpp_ld_old
   WHERE ldcnt    =  1
     AND ld_perst <= sy-datum
     AND ld_pered >= sy-datum .

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
      SELECT SINGLE arbpl INTO it_takts-shop
        FROM crhd
       WHERE objid = it_ld-lnsid_up.

      it_takts-t_time = 60 * 60                  / wa_uph .   " Sec
      it_takts-l_time = 60 * it_mas_rr-takt * 60 / wa_uph .   " Sec
      APPEND it_takts.
    ENDLOOP.
  ELSE .                                    " Plan no of tack...
    SORT it_ld BY lnid lnsid_up lnsid .
    READ TABLE it_ld INDEX 1 .
    l_lnsid = it_ld-lnsid_up .

    SELECT SINGLE arbpl INTO it_takts-shop
      FROM crhd
     WHERE objid = it_ld-lnsid_up.

    SELECT SINGLE arbpl INTO it_takts-line
      FROM crhd
     WHERE objid = it_ld-lnid .

    LOOP AT it_ld  .
      IF l_lnsid NE it_ld-lnsid_up .
        SELECT SINGLE arbpl INTO it_takts-shop
          FROM crhd
         WHERE objid = it_ld-lnsid_up.
        l_lnsid = it_ld-lnsid_up .
      ENDIF.
      it_takts-takts = it_ld-takts.
      it_takts-arbpl = it_ld-arbpl.
      READ TABLE it_mas_rrr WITH KEY arbpl = it_ld-arbpl.
      it_takts-sortb = it_mas_rrr-sortb .
      it_takts-t_time = 60 * 60               / wa_uph .   " Sec
      it_takts-l_time = 60 * it_ld-takts * 60 / wa_uph .   " Sec
      APPEND it_takts.
    ENDLOOP.
  ENDIF.

  " Founding the Sign-Off Reporting Point & Read the VINN
  GET TIME.
  WRITE AT: /001(40) 'Start of the Reading Vehicle Master..' ,
             041(11) sy-datum,
             053(10) sy-uzeit.
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
  " DVRT2 Appending...
  CHECK  p_dvrt = '2' .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE gubb = space  .

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
  DATA: l_loops             TYPE i                ,
        l_atinn0            LIKE ausp-atinn       ,
        l_vals              LIKE ausp-atwrt       ,
        l_sdat              LIKE sy-datum         ,
        l_stim              LIKE sy-uzeit         ,
        l_time              LIKE sy-uzeit         ,
        l_addtime           LIKE sy-uzeit         ,
        l_prevals           LIKE ztpp_dvrt1-rp01  ,
        l_dname(30)         TYPE c                ,
        l_no(2)             TYPE n                ,
        l_pre(2)            TYPE n                ,
        l_data              LIKE TABLE OF it_data      WITH HEADER LINE.

  GET TIME.
  WRITE AT: /001(40) 'Start of the Creating the TimeStamp..' ,
             041(11) sy-datum,
             053(10) sy-uzeit.
  " Make the DVRT Layout
  SORT it_data BY rp DESCENDING rp18 rp17 rp16 rp15 rp14 rp13 rp12
                     rp11 rp10  rp09 rp08 rp07 rp06 rp05 rp04 rp03
                     rp02 rp01  seq_date  seq_serial              .

  l_data[] = it_data[].
  READ TABLE l_data INDEX 1.
  MOVE-CORRESPONDING l_data TO wa_data.
  DO 18 TIMES.
    l_no = l_no + 1 .
    CONCATENATE 'L_DATA-RP'     l_no        INTO l_dname.
    ASSIGN (l_dname)          TO    <wa_dfield>         .
    IF <wa_dfield> > space     .
      l_prevals = <wa_dfield> .
      CONTINUE.
    ELSE.
      READ TABLE it_takts WITH KEY sortb = l_no .
      CONCATENATE 'WA_DATA-RP'     l_no     INTO l_dname.
      ASSIGN (l_dname)          TO    <wa_dfield>       .
      l_sdat = l_prevals(8)  .  l_time = l_prevals+8(6) .
      l_addtime = it_takts-l_time .
      CALL FUNCTION 'C14B_ADD_TIME'
           EXPORTING
                i_starttime = l_time
                i_startdate = l_sdat
                i_addtime   = l_addtime
           IMPORTING
                e_endtime   = l_time
                e_enddate   = l_sdat.

      CONCATENATE l_sdat l_time  'P'       INTO <wa_dfield> .
    ENDIF.
  ENDDO.

  MOVE-CORRESPONDING wa_data TO  l_data.
  MODIFY l_data INDEX 1.

  " Fill the Time-Stamp.... in the production-line..
  LOOP AT l_data WHERE status > '00'.
    " Find the lead time
    l_no = 0.
*   l_sdat = l_data-rp01(8). l_time = l_data-rp01+8(6) .
    DO 18 TIMES.
      l_no = l_no + 1 .
      CONCATENATE 'L_DATA-RP'     l_no        INTO l_dname.
      ASSIGN (l_dname)          TO    <wa_dfield>          .
      IF <wa_dfield> > space .
        CONCATENATE <wa_dfield>           'A' INTO <wa_dfield> .
        CONTINUE.
      ELSE.
        READ TABLE it_takts WITH KEY sortb = l_no .
        CONCATENATE 'WA_DATA-RP'  l_no     INTO l_dname.
        ASSIGN (l_dname)                   TO    <wa_dfield> .
        l_sdat = <wa_dfield>(8).  l_time = <wa_dfield>+8(6) .
*       l_addtime = it_takts-l_time .
        l_addtime = it_takts-t_time .
        CALL FUNCTION 'C14B_ADD_TIME'
             EXPORTING
                  i_starttime = l_time
                  i_startdate = l_sdat
                  i_addtime   = l_addtime
             IMPORTING
                  e_endtime   = l_time
                  e_enddate   = l_sdat.

        CONCATENATE 'L_DATA-RP'  l_no INTO l_dname.
        ASSIGN (l_dname)              TO   <wa_dfield> .
        CONCATENATE l_sdat l_time 'P' INTO <wa_dfield> .
      ENDIF.
    ENDDO.
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.

  LOOP AT l_data WHERE rp = 'RP01'.
    MOVE-CORRESPONDING l_data TO wa_data  .
  ENDLOOP.

  " Fill the Time-Stamp.... Waiting VIN before the production-line..
  GET TIME.
  l_stim = sy-uzeit .       l_sdat = sy-datum.
  LOOP AT l_data WHERE status = '00'.
    l_no = 0.
    DO 18 TIMES.
      l_no = l_no + 1 .
      READ TABLE it_takts WITH KEY sortb = l_no .
      CONCATENATE 'WA_DATA-RP'     l_no     INTO l_dname.
      ASSIGN (l_dname)          TO    <wa_dfield>          .
      l_sdat = <wa_dfield>(8).  l_time = <wa_dfield>+8(6) .
*     l_addtime = it_takts-l_time .
      l_addtime = it_takts-t_time .
      CALL FUNCTION 'C14B_ADD_TIME'
           EXPORTING
                i_starttime = l_time
                i_startdate = l_sdat
                i_addtime   = l_addtime
           IMPORTING
                e_endtime   = l_time
                e_enddate   = l_sdat.

      CONCATENATE 'L_DATA-RP'  l_no INTO l_dname.
      ASSIGN (l_dname)              TO   <wa_dfield> .
      CONCATENATE l_sdat l_time 'P' INTO <wa_dfield> .
    ENDDO.
    MODIFY l_data.
    MOVE-CORRESPONDING l_data TO wa_data.
  ENDLOOP.
  it_data[] = l_data[].

  CHECK p_dvrt = '2'  .
  " Fill the Time-Stamp.... (7JB DATA...)
  CLEAR: l_data, l_data[].
  LOOP AT it_7jb.
    l_data-seq_date  = it_7jb-sqdt.
    l_data-seq_code  = it_7jb-sqcd.
    l_data-body_ser  = it_7jb-ssr1.
    l_data-plnt      = it_7jb-plnt.
    l_data-line      = it_7jb-line.
    l_data-modl      = it_7jb-modl.
    l_data-mi        = it_7jb-bmdl.
    l_data-ocnn      = it_7jb-ocnn.
    l_data-vers      = it_7jb-vers.
    APPEND l_data.
  ENDLOOP.

  LOOP AT l_data .
    l_no = 0.
    DO 18 TIMES.
      l_no = l_no + 1 .
*     CONCATENATE 'L_DATA-RP'   l_no        INTO l_dname.
*     ASSIGN (l_dname)          TO          <wa_dfield> .

      READ TABLE it_takts WITH KEY sortb = l_no .
      CONCATENATE 'WA_DATA-RP'     l_no     INTO l_dname.
      ASSIGN (l_dname)             TO        <wa_dfield>.
      l_addtime = it_takts-t_time .
      CALL FUNCTION 'C14B_ADD_TIME'
           EXPORTING
                i_starttime = l_time
                i_startdate = l_sdat
                i_addtime   = l_addtime
           IMPORTING
                e_endtime   = l_time
                e_enddate   = l_sdat.

      CONCATENATE 'L_DATA-RP'  l_no   INTO l_dname.
      ASSIGN (l_dname)                TO   <wa_dfield> .
      CONCATENATE  l_sdat      l_time INTO <wa_dfield> .
    ENDDO.
    MODIFY l_data.
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
    MOVE-CORRESPONDING it_data  TO  ytpp_dvrt1      .
    MODIFY ytpp_dvrt1  FROM  ytpp_dvrt1             .
    WRITE: /, it_data-objek, it_data-seq_serial     ,
              it_data-arbpl, it_data-uph,
                             it_data-rp01, it_data-rp02, it_data-rp03,
                             it_data-rp04, it_data-rp05, it_data-rp06,
                             it_data-rp07, it_data-rp08, it_data-rp09,
                             it_data-rp10, it_data-rp11, it_data-rp12,
                             it_data-rp13, it_data-rp14, it_data-rp15,
                             it_data-rp16, it_data-rp17, it_data-rp18.
  ENDLOOP.

  DESCRIBE TABLE it_data2 LINES l_count.
  IF l_count > 0.
    APPEND LINES OF it_data2  TO it_data           .
    it_data2[] = it_data[]                         .
  ENDIF.

  LOOP AT it_data2.
    MOVE-CORRESPONDING it_data2  TO  ztpp_dvrt2     .
    MODIFY ztpp_dvrt2  FROM  ztpp_dvrt2             .
    WRITE: /, it_data2-objek, it_data2-seq_serial     ,
              it_data2-arbpl, it_data2-uph,
                            it_data2-rp01, it_data2-rp02, it_data2-rp03,
                            it_data2-rp04, it_data2-rp05, it_data2-rp06,
                            it_data2-rp07, it_data2-rp08, it_data2-rp09,
                            it_data2-rp10, it_data2-rp11, it_data2-rp12,
                            it_data2-rp13, it_data2-rp14, it_data2-rp15,
                            it_data2-rp16, it_data2-rp17, it_data2-rp18.
  ENDLOOP.
ENDFORM.                    " DISPLAY_DATA


* INCLUDE PROGRAM.....
INCLUDE zapp704c_inc_function_V1.
