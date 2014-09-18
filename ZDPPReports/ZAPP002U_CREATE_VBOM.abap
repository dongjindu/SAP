************************************************************************
* Program Name      : ZAPP002U_CREATE_VBOM
* Author            : Byung Sung,Bae
* Creation Date     : 2003.11.14.
* Specifications By : Byung Sung,Bae
* Pattern           : Report 1-1
* Development Request No : UD1K904164
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  zapp002u_create_vbom MESSAGE-ID zmpp.
TABLES: ztpp_vbom,
        ztpp_compkey,
        resb.

*----- Dataset
FIELD-GROUPS: header, order_qty.

INSERT: resb-plnum resb-prvbe resb-matnr INTO header,
        resb-bdmng resb-meins            INTO order_qty.

*----- Internal Tables
DATA: BEGIN OF it_vehicle OCCURS 0,        "Vehicle Master
        p_plan_order      LIKE   ausp-atwrt,
        objek             LIKE   ausp-objek,
        p_sequence_date   LIKE   ausp-atwrt,
        p_seq_serial      LIKE   ausp-atwrt,
        p_work_order      LIKE   ausp-atwrt,
        p_ext_color       LIKE   ausp-atwrt,
        p_int_color       LIKE   ausp-atwrt,

      END   OF it_vehicle.

DATA : BEGIN OF it_plpo OCCURS 0,           "Referance Rate Routing RP
         plnkn LIKE plpo-plnkn,
         steus LIKE plpo-steus,
         arbid LIKE plpo-arbid,
         arbpl LIKE crhd-arbpl,
         sortb LIKE crhd-sortb,
       END OF it_plpo.

DATA: BEGIN OF it_resb OCCURS 0,            "Planned Order Info
*        rsnum   LIKE   resb-rsnum,          "Requirement Doc.
*        rspos   LIKE   resb-rspos,          "Item No
*        rsart   LIKE   resb-rsart,          "Record Type
        plnum   LIKE   resb-plnum,          "Planned Order
        matnr   LIKE   resb-matnr,          "Component
        prvbe   LIKE   resb-prvbe,          "Supply Area
        bdmng   LIKE   resb-bdmng,          "Quantity
        meins   LIKE   resb-meins,          "UoM
      END   OF it_resb.

DATA: BEGIN OF it_resb_error OCCURS 0.
        INCLUDE STRUCTURE mseg.
DATA:   msg(50),
      END   OF it_resb_error.

DATA: BEGIN OF it_max_compkey OCCURS 0,
        p_model     LIKE   ztpp_compkey-p_model,
        prvbe       LIKE   ztpp_compkey-prvbe,
        compkey     LIKE   ztpp_compkey-compkey,
        compkey_seq LIKE   ztpp_compkey-compkey_seq,
      END   OF it_max_compkey.

DATA: BEGIN OF it_character OCCURS 0,           "Characteristic
        atinn   LIKE   cabn-atinn,              "Characteristic
        atnam   LIKE   cabn-atnam,              "Characteristic name
        atbez   LIKE   cabnt-atbez,             "Characteristic Desc.
        anzst   LIKE   cabn-anzst,              "Length
      END   OF it_character.

DATA: it_compkey LIKE ztpp_compkey OCCURS 0 WITH HEADER LINE,
      it_vbom    LIKE ztpp_vbom    OCCURS 0 WITH HEADER LINE.

*----- Structures
DATA: BEGIN OF wa_vehicle,
        objek   LIKE   ausp-objek,
        atinn   LIKE   ausp-atinn,
        atwrt   LIKE   ausp-atwrt,
      END   OF wa_vehicle.

*----- Global variables
DATA: wa_rp(2),
      wa_atinn LIKE cabn-atinn,             "Characteristic
      wa_compkey(50),                       "Compent Key
      wa_status          LIKE   cabn-atinn,
      wa_sequence_date   LIKE   cabn-atinn,
      wa_sequence_serial LIKE   cabn-atinn,
      wa_plan_order      LIKE   cabn-atinn,
      wa_work_order      LIKE   cabn-atinn,
      wa_ext_color       LIKE   cabn-atinn,
      wa_int_color       LIKE   cabn-atinn.

*----- Field symbols
FIELD-SYMBOLS: <fs_compkey>.

*----- Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE text-m07.
SELECT-OPTIONS: s_date FOR sy-datum OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK bl1.

*----- Start of Selection
START-OF-SELECTION.
  PERFORM read_data.
*  PERFORM set_component_key.
*  PERFORM set_vbom.
  loop at it_vbom.
    write:/ it_vbom.
  endloop.
  PERFORM insert_table.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM read_rate_routing_rp.          "
*  PERFORM read_compkey.
  PERFORM get_characteristic.
  PERFORM read_vehicle_master.
  PERFORM read_resb.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_rate_routing_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rate_routing_rp.
  SELECT pp~plnkn pp~steus pp~arbid ch~arbpl  ch~sortb
    INTO TABLE it_plpo
    FROM plpo AS pp INNER JOIN crhd AS ch
      ON pp~arbid = ch~objid
   WHERE pp~plnty EQ 'M'
     AND pp~plnnr EQ 'RP'
     AND pp~werks EQ 'P001'.

  " Set Sign off RP
  READ TABLE it_plpo WITH KEY steus = 'PP99'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m03.
  ENDIF.

  MOVE: it_plpo-sortb TO wa_rp.
ENDFORM.                    " read_rate_routing_rp
*&---------------------------------------------------------------------*
*&      Form  read_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle_master.
  PERFORM get_vehicle_master.
ENDFORM.                    " read_vehicle_master
*&---------------------------------------------------------------------*
*&      Form  generate_sql
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristic.
  DATA: lw_clint   LIKE   klah-clint,     "Class
        lw_objek   LIKE   kssk-objek.     "Class

  SELECT SINGLE clint INTO lw_clint
    FROM klah
   WHERE class = 'P_VEHICLE_MASTER'
     AND klart = '002'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m02.
  ENDIF.

  MOVE: lw_clint TO lw_objek.

  SELECT d~atinn d~atnam e~atbez d~anzst
    INTO CORRESPONDING FIELDS OF TABLE it_character
    FROM ( (
           kssk AS b INNER JOIN ksml AS c
                        ON b~clint = c~clint
                       AND b~adzhl = c~adzhl )
                     INNER JOIN cabn AS d
                        ON c~imerk = d~atinn
                       AND c~adzhl = d~adzhl )
                     INNER JOIN cabnt AS e
                        ON d~atinn = e~atinn
                       AND d~adzhl = e~adzhl
   WHERE b~objek = lw_objek
     AND b~klart = '002'
     AND e~spras = sy-langu.

  SORT it_character BY atnam.
ENDFORM.                    " generate_sql
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vehicle_master.
*----- Read Vin No, Sequence Date, Work Order, Plan Order, Ext Color,
*-----      Int Color from Vin Master

  READ TABLE it_character WITH KEY atnam = 'P_RP_STATUS'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_RP_STATUS' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_status.

  READ TABLE it_character WITH KEY atnam = 'P_SEQUENCE_DATE'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_SEQUENCE_DATE' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_sequence_date.

  READ TABLE it_character WITH KEY atnam = 'P_SEQUENCE_SERIAL'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_SEQUENCE_SERIAL' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_sequence_serial.

  READ TABLE it_character WITH KEY atnam = 'P_PLAN_ORDER'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_PLAN_ORDER' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_plan_order.

  READ TABLE it_character WITH KEY atnam = 'P_WORK_ORDER'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_WORK_ORDER' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_work_order.

  READ TABLE it_character WITH KEY atnam = 'P_EXT_COLOR'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_EXT_COLOR' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_ext_color.

  READ TABLE it_character WITH KEY atnam = 'P_INT_COLOR'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01 'P_INT_COLOR' text-m02.
  ENDIF.
  MOVE: it_character-atinn TO wa_int_color.

  EXEC SQL PERFORMING APPEND_IT_VEHICLE.
    SELECT B.OBJEK, B.ATINN, B.ATWRT
        INTO :WA_VEHICLE-OBJEK,           :WA_VEHICLE-ATINN,
             :WA_VEHICLE-ATWRT
        FROM (SELECT OBJEK
                FROM AUSP
               WHERE MANDT = :SY-MANDT
                 AND ATINN = :WA_STATUS
                 AND ATWRT < :WA_RP
*                 AND ATWRT < 'V18'
                 AND KLART = '002') A,
             AUSP B
       WHERE B.MANDT = :SY-MANDT
         AND B.OBJEK = A.OBJEK
         AND B.KLART = '002'
         AND B.ATINN IN (:WA_SEQUENCE_DATE,:WA_SEQUENCE_SERIAL,
                         :WA_PLAN_ORDER,   :WA_WORK_ORDER,
                         :WA_EXT_COLOR,    :WA_INT_COLOR)
  ENDEXEC.

  SORT it_vehicle BY p_plan_order.
ENDFORM.                    " get_vehicle_master
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_it_vehicle.
  CLEAR: it_vehicle.

  READ TABLE it_vehicle WITH KEY objek = wa_vehicle-objek.
  IF sy-subrc EQ 0.
    PERFORM set_value.
    MODIFY it_vehicle INDEX sy-tabix.
  ELSE.
    MOVE: wa_vehicle-objek TO it_vehicle-objek.
    PERFORM set_value.
    APPEND it_vehicle.
  ENDIF.
ENDFORM.                    " APPEND_IT_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  READ_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resb.
  LOOP AT it_vehicle.
    EXEC SQL PERFORMING APPEND_RESB.
      SELECT /*+ ORDERED*/
             A.PLNUM, B.MATNR, B.PRVBE, B.MEINS, SUM(B.BDMNG)
*             A.PLNUM, B.MATNR, B.PRVBE, B.MEINS, B.BDMNG
        INTO :RESB-PLNUM, :RESB-MATNR, :RESB-PRVBE, :RESB-MEINS,
             :RESB-BDMNG
        FROM PLAF A, RESB B
       WHERE A.MANDT = :SY-MANDT
         AND A.PLNUM = :IT_VEHICLE-P_PLAN_ORDER
         and A.KDAUF <> ' '
*         AND A.PLNUM = '0000095162'
         AND B.MANDT = A.MANDT
         AND B.RSNUM = A.RSNUM
         AND B.XLOEK = ' '
*       AND B.KDAUF > ' '
*         AND B.KDPOS = 0
         AND B.DUMPS = ' '
       GROUP BY A.PLNUM, B.MATNR, B.PRVBE, B.MEINS
    ENDEXEC.
  ENDLOOP.
*  LOOP AT it_vehicle.
*    SELECT B~rsnum B~rspos B~rsart B~plnum B~matnr B~prvbe
*           B~bdmng B~meins
*      APPENDING CORRESPONDING FIELDS OF TABLE it_RESB
*      FROM PLAF AS A INNER JOIN RESB AS B
*        ON A~RSNUM = B~RSNUM
*    WHERE A~PLNUM = it_vehicle-P_PLAN_ORDER
*      AND B~XLOEK = ' '
**      AND B~KDAUF <> ' '
*      AND B~DUMPS = ' '.                "Not Phantom
*  ENDLOOP.


*  LOOP AT it_vehicle.
*    SELECT a~equnr
*      INTO TABLE it_temp
*      FROM equz AS a INNER JOIN iloa AS b
*                        ON a~iloan = b~iloan
*                     INNER JOIN resb AS c
*                        ON b~eqfnr = c~rsnum
*    WHERE a~equnr = it_vehicle-objek.
*  ENDLOOP.

*  EXEC SQL PERFORMING APPEND_RESB.
*    SELECT
**    /*+ ORDERED*/
*           B.RSNUM, B.RSPOS, B.RSART, B.PLNUM, B.MATNR, B.PRVBE,
*           B.BDMNG, B.MEINS
*      INTO :RESB-RSNUM, :RESB-RSPOS, :RESB-RSART, :RESB-PLNUM,
*           :RESB-MATNR, :RESB-PRVBE, :RESB-BDMNG, :RESB-MEINS
*      FROM RESB B
**      , MARA A
*     WHERE
**     A.MANDT = :SY-MANDT
**       AND A.MTART = 'ROH'
**       AND
*B.MANDT = :SY-MANDT
**       AND B.MATNR = A.MATNR
**       AND B.WERKS = 'P001'
*       AND B.XLOEK = ' '
*       AND B.KDAUF = ' '
*       AND B.KDPOS = 0
*  ENDEXEC.


*  SELECT a~rsnum a~rspos a~rsart a~plnum a~matnr a~prvbe a~bdmng
*a~meins
**    INTO CORRESPONDING FIELDS OF resb
*    INTO CORRESPONDING FIELDS OF TABLE it_resb
*    FROM resb as a inner join mara as b
*      on a~matnr = b~matnr
*   WHERE a~xloek = ' '
*     AND a~kdauf = ' '
*     AND a~kdpos = 0
*     and b~mtart = 'ROH'.
**  ENDSELECT.
*  IF sy-subrc NE 0.
*    MESSAGE e003 WITH text-m04.
*  ENDIF.

  SORT BY resb-plnum resb-prvbe resb-matnr.

*  LOOP.
*    AT NEW resb-prvbe.
*      WRITE:/ resb-plnum, resb-prvbe.
*    ENDAT.
*    WRITE:/ sy-index.
*  ENDLOOP.
ENDFORM.                    " READ_RESB
*&---------------------------------------------------------------------*
*&      Form  set_component_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_component_key.
*  LOOP.
  READ TABLE it_vehicle WITH KEY p_plan_order = resb-plnum
                                 BINARY SEARCH.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING resb TO it_resb_error.
    it_resb_error-msg = text-m05.
*      APPEND it_resb_error.
  ENDIF.

*  SORT it_compkey BY p_model prvbe component.

  READ TABLE it_compkey WITH KEY p_model    = it_vehicle-objek(3)
                                 prvbe      = resb-prvbe
                                 IDNRK  = resb-matnr.
*                                 BINARY SEARCH.
  IF sy-subrc EQ 0.
    CHECK 1 = 0.
*    it_compkey-bdmng = resb-bdmng.
*    it_compkey-meins = resb-meins.
*    it_compkey-aedat = sy-datum.
*    it_compkey-aezet = sy-uzeit.
*    it_compkey-aenam = sy-uname.
*    MODIFY it_compkey INDEX sy-tabix.
  ELSE.
    CLEAR: it_compkey.
    READ TABLE it_max_compkey WITH KEY p_model = it_vehicle-objek(3)
                                       prvbe   = resb-prvbe.
    IF sy-subrc EQ 0.
      IF it_max_compkey-compkey_seq EQ 9.
        it_max_compkey-compkey     = it_max_compkey-compkey + 1.
        it_max_compkey-compkey_seq = 0.
      ELSE.
        it_max_compkey-compkey_seq = it_max_compkey-compkey_seq + 1.
      ENDIF.
      MODIFY it_max_compkey INDEX sy-tabix.
    ELSE.
      it_max_compkey-p_model = it_vehicle-objek(3).
      it_max_compkey-prvbe   = resb-prvbe.
      it_max_compkey-compkey = 1.
      APPEND it_max_compkey.
    ENDIF.

    it_compkey-p_model     = it_vehicle-objek(3).
    it_compkey-prvbe       = resb-prvbe.
    it_compkey-IDNRK   = resb-matnr.
    it_compkey-compkey     = it_max_compkey-compkey.
    it_compkey-compkey_seq = it_max_compkey-compkey_seq.
    it_compkey-bdmng       = resb-bdmng.
    it_compkey-meins       = resb-meins.
    it_compkey-erdat       = sy-datum.
    it_compkey-erzet       = sy-uzeit.
    it_compkey-ernam       = sy-uname.
    it_compkey-aedat       = sy-datum.
    it_compkey-aezet       = sy-uzeit.
    it_compkey-aenam       = sy-uname.

    APPEND it_compkey.

*    SORT it_compkey BY p_model prvbe component.
  ENDIF.
*  ENDLOOP.

*  LOOP AT it_resb.
*    READ TABLE it_vehicle WITH KEY p_plan_order = it_resb-plnum
*                                   BINARY SEARCH.
*    IF sy-subrc NE 0.
*      MOVE-CORRESPONDING it_resb TO it_resb_error.
*      it_resb_error-msg = text-m05.
**      APPEND it_resb_error.
*    ENDIF.
*
*    READ TABLE it_compkey WITH KEY p_model    = it_vehicle-objek(3)
*                                   prvbe      = it_resb-prvbe
*                                   component  = it_resb-matnr.
*    IF sy-subrc EQ 0.
*      it_compkey-bdmng = it_resb-bdmng.
*      it_compkey-meins = it_resb-meins.
*      it_compkey-aedat = sy-datum.
*      it_compkey-aezet = sy-uzeit.
*      it_compkey-aenam = sy-uname.
*      MODIFY it_compkey INDEX sy-tabix.
*    ELSE.
*      CLEAR: it_compkey.
*      READ TABLE it_max_compkey WITH KEY p_model = it_vehicle-objek(3)
*                                         prvbe   = it_resb-prvbe.
*      IF sy-subrc EQ 0.
*        it_max_compkey-compkey = it_max_compkey-compkey + 1.
*        MODIFY it_max_compkey INDEX sy-tabix.
*      ELSE.
*        it_max_compkey-p_model = it_vehicle-objek(3).
*        it_max_compkey-prvbe   = it_resb-prvbe.
*        it_max_compkey-compkey = 1.
*        APPEND it_max_compkey.
*      ENDIF.
*
*      it_compkey-p_model = it_vehicle-objek(3).
*      it_compkey-prvbe   = it_resb-prvbe.
*      it_compkey-component = it_resb-matnr.
*      it_compkey-compkey   = it_max_compkey-compkey.
*      it_compkey-bdmng = it_resb-bdmng.
*      it_compkey-meins = it_resb-meins.
*      it_compkey-erdat = sy-datum.
*      it_compkey-erzet = sy-uzeit.
*      it_compkey-ernam = sy-uname.
*      it_compkey-aedat = sy-datum.
*      it_compkey-aezet = sy-uzeit.
*      it_compkey-aenam = sy-uname.
*
*      APPEND it_compkey.
*    ENDIF.
*  ENDLOOP.
*
*  MODIFY ztpp_compkey FROM TABLE it_compkey.
ENDFORM.                    " set_component_key
*&---------------------------------------------------------------------*
*&      Form  set_vbom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_vbom.
*  LOOP.
  CLEAR: it_resb_error.

  READ TABLE it_vehicle WITH KEY p_plan_order = resb-plnum
                                 BINARY SEARCH.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING resb TO it_resb_error.
    it_resb_error-msg = text-m05.
    APPEND it_resb_error.
*      CONTINUE.
    CHECK 1 = 0.
  ENDIF.

  READ TABLE it_compkey WITH KEY p_model    = it_vehicle-objek(3)
                                 prvbe      = resb-prvbe
                                 IDNRK  = resb-matnr.
*                                 BINARY SEARCH.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING resb TO it_resb_error.
    it_resb_error-msg = text-m06.
    APPEND it_resb_error.
    CHECK 1 = 0.
*      CONTINUE.
  ENDIF.

*  SORT IT_VBOM BY P_MODEL P_BODY_SERIAL P_SEQUENCE_DATE P_SEQ_SERIAL.

  READ TABLE it_vbom WITH KEY p_model     = it_vehicle-objek(3)
                          p_body_serial   = it_vehicle-objek+3
                          p_sequence_date = it_vehicle-p_sequence_date
                          p_seq_serial    = it_vehicle-p_seq_serial.
*                          BINARY SEARCH.
  IF sy-subrc EQ 0.
    CONCATENATE 'IT_VBOM-COMP' it_compkey-compkey '+'
                it_compkey-compkey_seq '(1)'
           INTO wa_compkey.
    ASSIGN (wa_compkey) TO <fs_compkey>.
    MOVE: 'X' TO <fs_compkey>,
          sy-datum TO it_vbom-aedat,
          sy-uzeit TO it_vbom-aezet,
          sy-uname TO it_vbom-aenam.
    MODIFY it_vbom INDEX sy-tabix.
  ELSE.
    CONCATENATE 'IT_VBOM-COMP' it_compkey-compkey '+'
                it_compkey-compkey_seq '(1)'
           INTO wa_compkey.
    ASSIGN (wa_compkey) TO <fs_compkey>.

    MOVE: it_vehicle-objek(3)        TO it_vbom-p_model,
          it_vehicle-objek+3         TO it_vbom-p_body_serial,
          it_vehicle-p_sequence_date TO it_vbom-p_sequence_date,
          it_vehicle-p_seq_serial    TO it_vbom-p_seq_serial,
          it_vehicle-p_work_order    TO it_vbom-p_work_order,
          it_vehicle-p_ext_color     TO it_vbom-p_ext_color,
          it_vehicle-p_int_color     TO it_vbom-p_int_color,
          it_vehicle-p_plan_order    TO it_vbom-plnum,
          'X'                        TO <fs_compkey>,
          sy-datum                   TO it_vbom-aedat,
          sy-uzeit                   TO it_vbom-aezet,
          sy-uname                   TO it_vbom-aenam.

    APPEND it_vbom.
  ENDIF.
*  ENDLOOP.
*    LOOP at it_resb.
*    CLEAR: it_resb_error.
*
*    READ TABLE it_vehicle WITH KEY p_plan_order = it_resb-plnum
*                                   BINARY SEARCH.
*    IF sy-subrc NE 0.
*      MOVE-CORRESPONDING it_resb TO it_resb_error.
*      it_resb_error-msg = text-m05.
*      APPEND it_resb_error.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE it_compkey WITH KEY p_model    = it_vehicle-objek(3)
*                                   prvbe      = it_resb-prvbe
*                                   component  = it_resb-matnr.
*    IF sy-subrc NE 0.
*      MOVE-CORRESPONDING it_resb TO it_resb_error.
*      it_resb_error-msg = text-m06.
*      APPEND it_resb_error.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE it_vbom WITH KEY p_model     = it_vehicle-objek(3)
*                            p_body_serial   = it_vehicle-objek+3
*                            p_sequence_date =
*it_vehicle-p_sequence_date
*                            p_seq_serial    = it_vehicle-p_seq_serial.
*    IF sy-subrc EQ 0.
*      CONCATENATE 'IT_VBOM-COMP' it_compkey-compkey '+'
*                  it_compkey-compkey_seq '(1)'
*             INTO wa_compkey.
*      ASSIGN (wa_compkey) TO <fs_compkey>.
*      MOVE: 'X' TO <fs_compkey>,
*            sy-datum TO it_vbom-aedat,
*            sy-uzeit TO it_vbom-aezet,
*            sy-uname TO it_vbom-aenam.
*      MODIFY it_vbom index sy-tabix.
*    ELSE.
*      CONCATENATE 'IT_VBOM-COMP' it_compkey-compkey '+'
*                  it_compkey-compkey_seq '(1)'
*             INTO wa_compkey.
*      ASSIGN (wa_compkey) TO <fs_compkey>.
*
*      MOVE: it_vehicle-objek(3)        TO it_vbom-p_model,
*            it_vehicle-objek+3         TO it_vbom-p_body_serial,
*            it_vehicle-p_sequence_date TO it_vbom-p_sequence_date,
*            it_vehicle-p_seq_serial    TO it_vbom-p_seq_serial,
*            it_vehicle-p_work_order    TO it_vbom-p_work_order,
*            it_vehicle-p_ext_color     TO it_vbom-p_ext_color,
*            it_vehicle-p_int_color     TO it_vbom-p_int_color,
*            it_vehicle-p_plan_order    TO it_vbom-plnum,
*            'X'                        TO <fs_compkey>,
*            sy-datum                   TO it_vbom-aedat,
*            sy-uzeit                   TO it_vbom-aezet,
*            sy-uname                   TO it_vbom-aenam.
*
*      APPEND it_vbom.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " set_vbom
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value.
  CASE wa_vehicle-atinn.
    WHEN wa_sequence_date.                            "P_SEQUENCE_DATE
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_sequence_date.
    WHEN wa_sequence_serial.                          "P_SEQUENCE_SERIAL
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_seq_serial.
    WHEN wa_plan_order.                               "P_PLAN_ORDER
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_plan_order.
    WHEN wa_work_order.                               "P_WORK_ORDER
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_work_order.
    WHEN wa_ext_color.                                "P_EXT_COLOR
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_ext_color.
    WHEN wa_int_color.                                "P_INT_COLOR
      MOVE: wa_vehicle-atwrt TO it_vehicle-p_int_color.
  ENDCASE.
ENDFORM.                    " SET_VALUE
*&---------------------------------------------------------------------*
*&      Form  read_compkey
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_compkey.
  DATA: lt_max_compkey LIKE it_max_compkey OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE it_compkey
    FROM ztpp_compkey.

  SELECT p_model prvbe MAX( compkey )
    INTO TABLE it_max_compkey
    FROM ztpp_compkey
   GROUP BY p_model prvbe.

  SELECT p_model prvbe compkey MAX( compkey_seq )
    INTO TABLE lt_max_compkey
    FROM ztpp_compkey
   GROUP BY p_model prvbe compkey.

  LOOP AT lt_max_compkey.
    READ TABLE it_max_compkey WITH KEY p_model = lt_max_compkey-p_model
                                       prvbe   = lt_max_compkey-prvbe
                                       compkey = lt_max_compkey-compkey.
    IF sy-subrc EQ 0.
      MOVE: lt_max_compkey-compkey_seq TO it_max_compkey-compkey_seq.
      MODIFY it_max_compkey INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_compkey
*&---------------------------------------------------------------------*
*&      Form  APPEND_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_resb.
  EXTRACT order_qty.
  PERFORM set_component_key.
  PERFORM set_vbom.
ENDFORM.                    " APPEND_RESB
*&---------------------------------------------------------------------*
*&      Form  insert_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_table.
  DELETE FROM ztpp_compkey WHERE p_model   > ' '.
  DELETE FROM ztpp_vbom
   WHERE p_sequence_date IN s_date.

  INSERT ztpp_compkey FROM TABLE it_compkey ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e003 WITH text-m15.
  ENDIF.
  INSERT ztpp_vbom    FROM TABLE it_vbom ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e003 WITH text-m16.
  ELSE.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m17.
  ENDIF.
ENDFORM.                    " insert_table
