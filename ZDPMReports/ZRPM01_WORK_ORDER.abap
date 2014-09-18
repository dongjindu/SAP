

************************************************************************
* Program Name      : ZRPM01_WORK_ORDER
* Author            : Myoung ho, Park
* Creation Date     : 2003.11.24.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Print Daily work order list (SmartForm)and
*                     Countermeasure report(Excel)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/29/05    Furong Wang               Add in long text in work order
* 10-24-2005  haseeb      UD1K922698    Print person responsible on
*                                       smart form.
*
************************************************************************
REPORT zrpm01_work_order .

TABLES: zspm_woko,   "//Work Order Header
        zspm_wopo,  "//Work Order Items
        viaufkst,   "//PM Order Selection by Status
        diaufk,     "//PM Order
        rihea,      "//PM hierarchy selection/list screen
        iflo,       "//Functional Location (View)
        viafvc,     "//PM: MaintOperations
        viqmel,     "//Notification Header
        afvc,       "//Operation within an order
        afru,       "//Order completion confirmations
        afko,
        eqkt,
        v_username,
        iflotx,
        ihpa,
        usr02,
        zlongtext,
        pa0001,
        diadr,
        qmma,
        crfhd.

*** internal table for orders list
*DATA : IT_TEMP_ORDER LIKE ZSPM_WOPO OCCURS 0 WITH HEADER LINE.
* Modification by 100565
DATA: BEGIN OF it_temp_order OCCURS 0.
        INCLUDE STRUCTURE zspm_wopo.
DATA: ausvn LIKE viqmel-ausvn,
      ausbs LIKE viqmel-ausbs,
      auztv LIKE viqmel-auztv,
      auztb LIKE viqmel-auztb,
      auszt LIKE viqmel-auszt,
      maueh LIKE viqmel-maueh,
      interval TYPE i,
      idaur LIKE afru-idaur,
      eqktx LIKE eqkt-eqktx,
      pltxt LIKE iflotx-pltxt,
*       ENAME LIKE PA0001-ENAME,
  END OF it_temp_order.
DATA: ztran(4).
*DATA : IT_ORDER LIKE ZSPM_WOPO OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_order OCCURS 0.
        INCLUDE STRUCTURE zspm_wopo.
DATA: ausvn LIKE viqmel-ausvn,
      ausbs LIKE viqmel-ausbs,
      auztv LIKE viqmel-auztv,
      auztb LIKE viqmel-auztb,
      auszt LIKE viqmel-auszt,
      maueh LIKE viqmel-maueh,
      interval TYPE i,
      idaur LIKE afru-idaur,
      eqktx LIKE eqkt-eqktx,
      pltxt LIKE iflotx-pltxt,
** Added by Furong on 07/13/09
     plgrp LIKE viaufkst-plgrp,
** End of change
*       ENAME LIKE PA0001-ENAME,
  END OF it_order.

* end of modification

DATA : it_order_num LIKE zspm_wopo OCCURS 0 WITH HEADER LINE.

*** internal table for selected opreations list
DATA : it_zspm_wopo LIKE it_order OCCURS 0 WITH HEADER LINE.

*** internal table for All opreations list
DATA : it_temp LIKE it_order OCCURS 0 WITH HEADER LINE.

*** Header info structure
DATA : wa_zspm_woko LIKE zspm_woko.

DATA: wa_index LIKE sy-tabix.

*** Range for Maintenance processing stage
RANGES: r_iphas  FOR viaufks-iphas.

** for read long text

DATA: it_longtext LIKE zlongtext OCCURS 0 WITH HEADER LINE.

*data: begin of it_longtext occurs 0,
*      aufnr like afko-aufnr,
*      vornr like afvc-vornr,
*      LTXA1 like zspm_wopo-LTXA1,
*      longtext like tline-tdline,
*      end of it_longtext.

DATA: BEGIN OF wa_afko OCCURS 0,
        aufnr LIKE afko-aufnr,
        vornr LIKE afvc-vornr,
        aufpl LIKE afko-aufpl,
        aplzl LIKE afvc-aplzl,
      END OF wa_afko.

DATA: it_afko LIKE wa_afko OCCURS 0 WITH HEADER LINE.

DATA: it_tline LIKE tline OCCURS 0 WITH HEADER LINE.

** For ALV
TYPE-POOLS: slis.

DATA : gv_repid LIKE sy-repid.
DATA : gv_status       TYPE slis_formname VALUE 'PF_STATUS'.
DATA : gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : gv_layout       TYPE slis_layout_alv.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA : gv_col_pos TYPE i.

DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat,
       it_eventcat          TYPE slis_t_event,
       wa_eventcat          LIKE LINE OF it_eventcat.

DATA : it_events            TYPE slis_t_event,
       it_event_exit      TYPE slis_t_event_exit.
DATA : it_symbol TYPE icon.
DATA : wa_interval TYPE i.

**** Confirmation counter
DATA: wa_rmzhl LIKE afru-rmzhl,
      wa_stokz LIKE afru-stokz.
*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : dy_ofn LIKE rihea-dy_ofn.
SELECTION-SCREEN COMMENT 3(29) text-002 FOR FIELD dy_ofn.
SELECTION-SCREEN POSITION 40.
****Outstanding (Notification or Order)
PARAMETER : dy_mab LIKE rihea-dy_mab.
SELECTION-SCREEN COMMENT 50(10) text-003 FOR FIELD dy_mab.
****Completed (notifications or orders)
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK block1.



SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.

*** 12/10/2003
*PARAMETER : P_BEBER LIKE VIAUFKST-BEBER OBLIGATORY, "//Plant section

*PARAMETER : P_INGRP LIKE VIQMEL-INGRP OBLIGATORY, "//Planner Group
*            P_TPLNR LIKE DIAUFK-TPLNR OBLIGATORY.
**                                            "//Functional location
** Changed by Furong on 07/13/09
SELECT-OPTIONS :  s_equnr FOR crfhd-equnr MATCHCODE OBJECT equi,
                  plgrp FOR viaufkst-plgrp  .
** End of change
SELECT-OPTIONS : s_ingrp FOR viqmel-ingrp NO-EXTENSION NO INTERVALS
                                          OBLIGATORY ,
                 s_tplnr FOR diaufk-tplnr NO-EXTENSION NO INTERVALS.

SELECT-OPTIONS : s_auart  FOR diaufk-auart,   "//Order type
                 s_datuv  FOR rihea-termab.   "//Period

SELECT-OPTIONS : s_steus FOR  viafvc-steus.   "//Control key

SELECTION-SCREEN END OF BLOCK block2.

****addition by 100565
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-010.

SELECT-OPTIONS:

                s_bname FOR usr02-bname MATCHCODE OBJECT user_comp.

SELECTION-SCREEN END OF BLOCK block3.
* end of addition

******************* INITIALIZATION ********************************
*******************************************************************
*** default check Outstanding
*** Period current month
INITIALIZATION.
  dy_ofn = 'X'.
  CONCATENATE sy-datum(6) '01' INTO s_datuv-low.
  s_datuv-high = sy-datum.
  APPEND s_datuv.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      CLEAR : sy-ucomm.
      CLEAR : it_temp_order, it_temp_order[].
      CLEAR : it_order, it_order[].
      PERFORM read_data.
**** { INSERT 2003/12/29...
      PERFORM check_countermeasure.
****  INSERT 2003/12/29... }

***Addition by 100565
* Calculate malfunction duration
      LOOP AT it_order.
        CLEAR: wa_interval.

        IF it_order-ausvn <> '00000000' AND it_order-auztv  <> '000000' AND
               it_order-ausbs <> '00000000'  AND it_order-auztb <> '000000'.

          CALL FUNCTION 'Z_FCA_GET_TIME_INTERVAL'
            EXPORTING
              s_date   = it_order-ausvn
              s_time   = it_order-auztv
              e_date   = it_order-ausbs
              e_time   = it_order-auztb
            IMPORTING
              interval = wa_interval.
        ENDIF.
*Get Equipment Text
        SELECT SINGLE eqktx INTO it_order-eqktx
                 FROM  eqkt
                 WHERE equnr = it_order-equnr.

* Get Function Location Text
        SELECT SINGLE pltxt INTO it_order-pltxt
                 FROM  iflotx
                 WHERE tplnr = it_order-tplnr.
* calculate Line down time.
        SELECT SINGLE MAX( rmzhl ) INTO wa_rmzhl
             FROM  afru
             WHERE aufnr = it_order-aufnr
             AND   vornr = '0010'.

        SELECT SINGLE idaur INTO it_order-idaur
               FROM  afru
               WHERE aufnr = it_order-aufnr
               AND   vornr = '0010'
               AND   rmzhl = wa_rmzhl.
* GET EMPLOYEE NAME FROM EMPLOYEE ID
        IF it_order-qmnam NE space.
          SELECT SINGLE ename INTO it_order-ename
                   FROM  pa0001
                   WHERE pernr = it_order-qmnam.
        ENDIF.

*MOVE WA_RMZHL TO IT_ORDER-RMZHL.
        MOVE wa_interval TO it_order-interval.
        MODIFY it_order.
      ENDLOOP.
***End Addition
      CALL SCREEN 0100.
    WHEN 'F03'.
      CLEAR: sy-ucomm.
      LEAVE SCREEN.

    WHEN  '%012'.
      CLEAR: sy-ucomm.
    WHEN  '%013'.
      CLEAR: sy-ucomm.
    WHEN  '%014'.
      CLEAR: sy-ucomm.
    WHEN  '%015'.
      CLEAR: sy-ucomm.
    WHEN  '%016'.
      CLEAR: sy-ucomm.
    WHEN  '%017'.
      CLEAR: sy-ucomm.
    WHEN  '%018'.
      CLEAR: sy-ucomm.

    WHEN OTHERS.
      CLEAR: sy-ucomm.
      LEAVE SCREEN.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.

  DATA : wa_rueck LIKE afru-rueck,
         wa_rmzhl LIKE afru-rmzhl.



** {{ 12/10/2003

*** Set select Condtion for check-box
*  RANGES : R_AUERU FOR AFRU-AUERU.
*
*  CLEAR: R_AUERU, R_AUERU[].
*
*  IF DY_OFN EQ 'X' AND DY_MAB EQ 'X'.
*
*  ELSEIF DY_OFN EQ 'X' AND DY_MAB EQ ' '.
*    R_AUERU-SIGN   = 'I'.
*    R_AUERU-OPTION = 'EQ'.
*    R_AUERU-LOW    = 'X'.
*    APPEND R_AUERU.
*
*  ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ 'X'.
*    R_AUERU-SIGN   = 'I'.
*    R_AUERU-OPTION = 'EQ'.
*    R_AUERU-LOW    = ' '.
*    APPEND R_AUERU.
*
*  ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ ' '.
*    MESSAGE E000(ZMPM) WITH TEXT-M01.
*  ENDIF.


  IF dy_ofn EQ ' ' AND dy_mab EQ ' '.
    MESSAGE e000(zmpm) WITH text-m01.
  ENDIF.

  PERFORM fill_r_iphas.

*  selec  single max()
*          form AFRU
*          where AUFNR =


  IF dy_ofn EQ 'X' AND dy_mab EQ ' '.
***  Selection Outstanding (not exits AFRU)
*** I0076         E        DLFL   Deletion flag
*** I0043         E        LKD    Locked
*** I0002         E        REL    Released
*** I0001         E        CRTD   Created
*** I0009         E        CNF    Confirmed


    SELECT DISTINCT a~aufnr a~auart a~tplnr a~equnr a~vornr
                    b~msgrp c~ktext b~qmnum b~ausvn b~ausbs b~auztv
                    b~auztb b~auszt b~maueh
                    b~qmnam                                 "100565
            INTO CORRESPONDING FIELDS OF TABLE it_temp_order
            FROM viauf_afvc AS a
                  INNER JOIN viqmel AS b
                  ON a~aufnr = b~aufnr
                      INNER JOIN caufv AS c
                      ON b~aufnr = c~aufnr
            WHERE a~tplnr IN s_tplnr
            AND   a~ingpr IN s_ingrp
            AND   a~auart IN s_auart
            AND   a~addat IN s_datuv
            AND   a~iphas IN r_iphas
            AND   a~steus IN s_steus
** Changed by Furong on 07/13/09
            AND   a~equnr IN s_equnr
** end of change
            AND   NOT EXISTS ( SELECT * FROM jest
                              WHERE objnr = a~objnr
                              AND ( ( stat  = 'I0076' AND inact <> 'X' )
                              OR    ( stat  = 'I0043' AND inact <> 'X' )
                              OR  ( stat  = 'I0009' AND inact <> 'X' ) )
                              )
            AND   EXISTS ( SELECT * FROM jest
                           WHERE objnr = a~objnr
                           AND   ( stat  = 'I0002' AND inact <> 'X' ) ).
    IF sy-subrc EQ 0.
      LOOP AT it_temp_order.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*               FROM  AFRU
*               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*               AND   VORNR = IT_TEMP_ORDER-VORNR
*               AND   RUECK = ( SELECT MAX( RUECK )
*                               FROM  AFRU
*                               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*                               AND   VORNR = IT_TEMP_ORDER-VORNR )
*               GROUP BY RUECK.

*** tunning
        SELECT SINGLE rueck  rmzhl  INTO (wa_rueck, wa_rmzhl)
               FROM  afru
               WHERE aufnr = it_temp_order-aufnr
               AND   vornr = it_temp_order-vornr
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF sy-subrc EQ 0.
          SELECT  SINGLE *
                  FROM afru
                  WHERE aufnr = it_temp_order-aufnr
                  AND   vornr = it_temp_order-vornr
                  AND   rueck = wa_rueck
                  AND   rmzhl = wa_rmzhl
                  AND   aueru EQ 'X'
                  AND   stokz EQ ' '
                  AND   stzhl EQ space.
          IF sy-subrc EQ 0.

          ELSE.
            MOVE-CORRESPONDING it_temp_order TO it_order.
            IF it_temp_order-equnr IS INITIAL.
              MOVE it_temp_order-tplnr TO it_order-equnr.
            ENDIF.
            MOVE ''  TO it_order-vornr.
            COLLECT it_order.
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING it_temp_order TO it_order.
          IF it_temp_order-equnr IS INITIAL.
            MOVE it_temp_order-tplnr TO it_order-equnr.
          ENDIF.
          MOVE ''  TO it_order-vornr.
          COLLECT it_order.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e000(zmpm) WITH text-m02.
    ENDIF.
*    SELECT DISTINCT A~AUFNR A~AUART A~TPLNR A~EQUNR B~MSGRP C~KTEXT
*            INTO CORRESPONDING FIELDS OF TABLE IT_ORDER
*            FROM VIAUF_AFVC AS A
*                  INNER JOIN VIQMEL AS B
*                  ON A~AUFNR = B~AUFNR
*                      INNER JOIN CAUFV AS C
*                      ON B~AUFNR = C~AUFNR
*            WHERE A~TPLNR IN S_TPLNR
*            AND   A~INGPR IN S_INGRP
*            AND   A~AUART IN S_AUART
*            AND   A~ADDAT IN S_DATUV
*            AND   A~IPHAS IN R_IPHAS
*            AND   A~STEUS IN S_STEUS
*            AND   NOT EXISTS ( SELECT * FROM AFRU
*                               WHERE AUFNR = A~AUFNR
*                               AND   AUERU EQ 'X'
*                               AND   STOKZ EQ ' ' ).

  ELSEIF dy_ofn EQ ' ' AND dy_mab EQ 'X'.
***  Selection Complete
    SELECT DISTINCT a~aufnr a~auart a~tplnr a~equnr a~vornr
                    b~msgrp c~ktext b~qmnum a~iedd b~qmnam
                    b~ausvn b~ausbs b~auztv                 "100565
                    b~auztb b~auszt b~maueh                 "100565
                    b~qmnam                                 "100565
            INTO CORRESPONDING FIELDS OF TABLE it_temp_order
            FROM viauf_afvc AS a
                  INNER JOIN viqmel AS b
                  ON a~aufnr = b~aufnr
                      INNER JOIN caufv AS c
                      ON b~aufnr = c~aufnr
            WHERE a~tplnr IN s_tplnr
            AND   a~ingpr IN s_ingrp
            AND   a~auart IN s_auart
            AND   a~addat IN s_datuv
            AND   a~iphas IN r_iphas
            AND   a~steus IN s_steus
** Changed by Furong on 07/13/09
            AND   a~equnr IN s_equnr
** end of change
            AND   NOT EXISTS ( SELECT * FROM jest
                       WHERE objnr = a~objnr
                       AND   ( ( stat  = 'I0076' AND inact <> 'X' )
                       OR   ( stat  = 'I0043' AND inact <> 'X' ) ) )
            AND   EXISTS ( SELECT * FROM jest
                       WHERE objnr = a~objnr
                       AND   ( stat  = 'I0002' AND inact <> 'X' ) ).

    IF sy-subrc EQ 0.
      LOOP AT it_temp_order.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*             AND   VORNR = IT_TEMP_ORDER-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*                             AND   VORNR = IT_TEMP_ORDER-VORNR )
*             GROUP BY RUECK.

        SELECT SINGLE rueck  rmzhl  INTO (wa_rueck, wa_rmzhl)
               FROM  afru
               WHERE aufnr = it_temp_order-aufnr
               AND   vornr = it_temp_order-vornr
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF sy-subrc EQ 0.
          SELECT  SINGLE *
                  FROM afru
                  WHERE aufnr = it_temp_order-aufnr
                  AND   vornr = it_temp_order-vornr
                  AND   rueck = wa_rueck
                  AND   rmzhl = wa_rmzhl
                  AND   aueru EQ 'X'
                  AND   stokz EQ ' '
                  AND   stzhl EQ space.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING it_temp_order TO it_order.
            IF it_temp_order-equnr IS INITIAL.
              MOVE it_temp_order-tplnr TO it_order-equnr.
            ENDIF.
            MOVE ''  TO it_order-vornr.
            COLLECT it_order.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e000(zmpm) WITH text-m02.
    ENDIF.

  ELSEIF dy_ofn EQ 'X' AND dy_mab EQ 'X'.
****  Selection Outstanding & Complete
    SELECT DISTINCT a~aufnr a~auart a~tplnr a~equnr a~vornr
                    b~msgrp c~ktext b~qmnum a~iedd b~qmnam
                    b~ausvn b~ausbs b~auztv                 "100565
                    b~auztb b~auszt b~maueh                 "100565
                    b~qmnam                                 "100565
            INTO CORRESPONDING FIELDS OF TABLE it_temp_order
            FROM viauf_afvc AS a
                  INNER JOIN viqmel AS b
                  ON a~aufnr = b~aufnr
                      INNER JOIN caufv AS c
                      ON b~aufnr = c~aufnr
            WHERE a~tplnr IN s_tplnr
            AND   a~ingpr IN s_ingrp
            AND   a~auart IN s_auart
            AND   a~addat IN s_datuv
            AND   a~iphas IN r_iphas
            AND   a~steus IN s_steus
** Changed by Furong on 07/13/09
            AND   a~equnr IN s_equnr
** end of change
            AND   NOT EXISTS ( SELECT * FROM jest
                      WHERE objnr = a~objnr
                      AND   ( ( stat  = 'I0076' AND inact <> 'X' )
                      OR  ( stat  = 'I0043' AND inact <> 'X' ) ) )
            AND   EXISTS ( SELECT * FROM jest
                      WHERE objnr = a~objnr
                      AND  ( stat  = 'I0002' AND inact <> 'X' ) ).
    IF sy-subrc NE 0.
      MESSAGE e000(zmpm) WITH text-m02.
    ELSE.
      LOOP AT it_temp_order.
        MOVE-CORRESPONDING it_temp_order TO it_order.
        IF it_temp_order-equnr IS INITIAL.
          MOVE it_temp_order-tplnr TO it_order-equnr.
        ENDIF.
        MOVE ''  TO it_order-vornr.
        COLLECT it_order.
      ENDLOOP.
    ENDIF.
  ENDIF.
*  SELECT DISTINCT A~AUFNR B~AUART A~BEBER A~TPLNR A~EQUNR  A~MSGRP
*          B~KTEXT
*         INTO CORRESPONDING FIELDS OF TABLE IT_ORDER
*         FROM VIQMEL AS A
*              INNER JOIN CAUFV AS B
*              ON A~AUFNR = B~AUFNR
*                   INNER JOIN AFIH AS C
*                   ON B~AUFNR = C~AUFNR
*                      INNER JOIN AFRU AS D
*                      ON C~AUFNR = D~AUFNR
*                         INNER JOIN AFVC AS E
*                         ON  B~AUFPL = E~AUFPL
**          WHERE A~BEBER =  P_BEBER
*          WHERE A~TPLNR =  P_TPLNR
*          AND   A~INGRP =  P_INGRP
*          AND   B~AUART IN S_AUART
*          AND   C~ADDAT IN S_DATUV
*          AND   D~AUERU IN R_AUERU
*          AND   E~STEUS IN S_STEUS.
  IF sy-subrc EQ 0.

*    LOOP AT IT_ORDER.
*      WA_INDEX = SY-TABIX.
*      IF NOT IT_ORDER-EQUNR IS INITIAL.
*        SELECT SINGLE EQKTX INTO IT_ORDER-TXTCDOT
*                      FROM EQKT
*                      WHERE EQUNR = IT_ORDER-EQUNR
*                      AND   SPRAS = SY-LANGU.
*        IF SY-SUBRC EQ 0.
*          MODIFY IT_ORDER INDEX WA_INDEX TRANSPORTING TXTCDOT.
*        ENDIF.
*      ELSE.
*        SELECT SINGLE PLTXT INTO IT_ORDER-TXTCDOT
*                      FROM IFLOTX
*                      WHERE TPLNR = IT_ORDER-TPLNR
*                      AND   SPRAS = SY-LANGU.
*        IF SY-SUBRC EQ 0.
*          MODIFY IT_ORDER INDEX WA_INDEX TRANSPORTING TXTCDOT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
  ENDIF.


**** read Plant section text(Shop)
*  SELECT SINGLE FING
*         INTO  ZSPM_WOKO-FING
*         FROM  T357
*         WHERE BEBER = P_BEBER.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
  SET PF-STATUS space.

  gv_repid = sy-repid.

* Preparation of ALV
  PERFORM pre_report_adj.

* Call ALV LIST
  PERFORM call_alv_list.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.
* Building Field Cat.
  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].


  PERFORM build_fieldcat USING
     'IT_ORDER' 'TPLNR'  'X'     space space
     space    '12'     'Functional Location'  space space space space
space space space space space space space space space.
****ADDITION BY 100565
  PERFORM build_fieldcat USING
      'IT_ORDER' 'EQKTX'  ' '     space space
      space    '40'     'Equipment'  space space space space space
  space space space space space space space space.

  PERFORM build_fieldcat USING
      'IT_ORDER' 'PLTXT'  ' '     space space
      space    '40'     'Functional Location'  space space space space
  space space space space space space  space space space.


***END
  PERFORM build_fieldcat USING
    'IT_ORDER' 'AUFNR'  'X'     space space
    space    '12'     'Order Number'  space space space space space
space space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'AUART'  ' '     space space
     space    '4'     'Order Type'  space space space space space space
space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'EQUNR'  ' '     space space
     space    '18'     'Object'  space space space space space space
space space  space space space space space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'TXTCDOT'  ' '     SPACE SPACE
*     SPACE    '40'     'Object Name'  SPACE SPACE SPACE space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'MSGRP'  ' '     space space
     space    '8'     'Process'  space space space space space space
space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'KTEXT'  ' '     space space
     space    '40'     'Order description'  space space space space
space space space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'ZCOUNTER'  ' '     space space
     space    ' '     'Countermeasure'  space space space 'X' space
space space space space space space  space space.



*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'VORNR'  ' '     SPACE SPACE
*     SPACE    '4'     'OpAc'  SPACE SPACE SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'LTXA1'  ' '     SPACE SPACE
*    SPACE    '40'     'OP Desc'  SPACE SPACE SPACE.
********addition by 100565***
  PERFORM build_fieldcat USING
    'IT_ORDER' 'AUSVN'  ' '     space space
     space    '8'     'Mal Start Date'  space space space space space
space space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'AUSBS'  ' '     space space
    space    '8'     'Mal End Date'  space space space space space space
 space space space space space space space.

  PERFORM build_fieldcat USING
  'IT_ORDER' 'AUZTV'  ' '     space space
   space    '8'     'Mal Start Time'  space space space space space
space space space space space space space space.

  PERFORM build_fieldcat USING
    'IT_ORDER' 'AUZTB'  ' '     space space
    space    '8'     'Mal End Time'  space space space space space space
 space space space space space space space.

  PERFORM build_fieldcat USING
      'IT_ORDER' 'INTERVAL'  ' '     space space
   space    '8'     'Mal Interval'  space space space space space space
   space space space space space space space.

  PERFORM build_fieldcat USING
     'IT_ORDER' 'IDAUR'  ' '     space space
     space    '8'     'Line Down Time'  space space space space space
 space space space space space space space space.

  PERFORM build_fieldcat USING
     'IT_ORDER' 'ENAME'  ' '     space space
     space    '20'     'Employee'  space space space space space
 space space space space space space space space.
****end addition by 100565***
** Changed by Furong on 07/13/09
  PERFORM build_fieldcat USING
      'IT_ORDER' 'PLGRP'  ' '     space space
     space   '20'    'Plan Grp for Task'  space space space space space
  space space space space space space space space.
** End of change

*** Sort
  SORT it_order BY aufnr.
  CLEAR: it_order .

  it_sort-fieldname = 'TPLNR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'AUFNR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'AUART'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'EQUNR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

*  IT_SORT-FIELDNAME = 'TXTCDOT'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.


  it_sort-fieldname = 'MSGRP'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'KTEXT'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.


*  IT_SORT-FIELDNAME = 'VORNR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*
*  IT_SORT-FIELDNAME = 'LTXA1'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.

  it_sort-fieldname = 'AUSVN'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'AUSBS'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.

  it_sort-fieldname = 'AUZTV'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'AUZTB'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'INTERVAL'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'IDAUR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'EQKTX'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'PLTXT'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'PLGRP'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

*** Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.

****
  gv_layout-box_fieldname = 'CHECK'.
*  GV_LAYOUT-LIGHTS_FIELDNAME = 'ZCOUNTER'.

ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_pf_status_set = gv_status
      i_callback_user_command  = gv_user_command
      it_fieldcat              = it_fieldcat[]
      it_sort                  = it_sort[]
      i_save                   = 'A'
      it_events                = it_events
      it_event_exit            = it_event_exit  "
      is_layout                = gv_layout
    TABLES
      t_outtab                 = it_order
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110)
*                            VALUE(P_0120).
                            value(p_0120)
                            value(p_0121)
                            value(p_0122)
                            value(p_0123)
                            value(p_0124)
                            value(p_0125)
                            value(p_0126)
                            value(p_0127)
                            value(p_0128)
                            value(p_0129).


  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-icon        = p_0120.
  wa_fieldcat-col_pos     = gv_col_pos.


  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.
  DATA: it_commentary TYPE slis_t_listheader WITH HEADER LINE.

  it_commentary-typ = 'H'.
  it_commentary-key = 'Absentees'.
  it_commentary-info = 'List of Orders'.
  APPEND it_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_commentary[].


***addition by 100565
  DATA: it_commentary1 TYPE slis_t_listheader WITH HEADER LINE.
  DATA: BEGIN OF it_name OCCURS 0,
  nametext(60),
  END OF it_name.

* IT_COMMENTARY1-TYP = 'H'.
*IT_COMMENTARY1-INFO = 'List of Orders'.
*APPEND IT_COMMENTARY1.
*CLEAR IT_COMMENTARY1-INFO.

  it_commentary1-typ = 'A'.
  it_commentary1-info = 'List of Absentees'.
  APPEND it_commentary1.
  CLEAR it_commentary1-info.

  it_commentary1-typ = 'A'.
  it_commentary1-info = '  '.
  APPEND it_commentary1.
  CLEAR it_commentary1-info.

  IF s_bname NE space.
    SELECT DISTINCT name_text FROM v_username INTO it_commentary1-info WHERE
                            bname IN s_bname.
      it_commentary1-typ = 'A'.
      APPEND it_commentary1.
      CLEAR it_commentary1-info.
    ENDSELECT.
  ENDIF.
*IT_COMMENTARY1-TYP = 'H'.
*IT_COMMENTARY1-INFO = 'List of Orders'.
*APPEND IT_COMMENTARY1.
*CLEAR IT_COMMENTARY1-INFO.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_commentary1[].




*         CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*          EXPORTING
**            I_INTERFACE_CHECK              = ' '
**            I_BYPASSING_BUFFER             =
**            I_BUFFER_ACTIVE                = ' '
**            I_CALLBACK_PROGRAM             = ' '
**            I_CALLBACK_PF_STATUS_SET       = ' '
**            I_CALLBACK_USER_COMMAND        = ' '
**            I_STRUCTURE_NAME               = S_BNAME
**            IS_LAYOUT                      =
*            IT_FIELDCAT                    =
**            IT_EXCLUDING                   =
**            IT_SPECIAL_GROUPS              =
**            IT_SORT                        =
**            IT_FILTER                      =
**            IS_SEL_HIDE                    =
**            I_DEFAULT                      = 'X'
**            I_SAVE                         = ' '
**            IS_VARIANT                     =
**            IT_EVENTS                      =
**            IT_EVENT_EXIT                  =
**            IS_PRINT                       =
**            IS_REPREP_ID                   =
**            I_SCREEN_START_COLUMN          = 0
**            I_SCREEN_START_LINE            = 0
**            I_SCREEN_END_COLUMN            = 0
**            I_SCREEN_END_LINE              = 0
**          IMPORTING
**            E_EXIT_CAUSED_BY_CALLER        =
**            ES_EXIT_CAUSED_BY_USER         =
*           TABLES
*             T_OUTTAB                       = S_BNAME
**          EXCEPTIONS
**            PROGRAM_ERROR                  = 1
**            OTHERS                         = 2
*                   .
*         IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*         ENDIF.
*          .
*
**DATA: IT_COMMENTARY_1 TYPE SLIS_T_LISTHEADER WITH HEADER LINE.
**IT_COMMENTARY_1-TYP = 'H'.
**  IT_COMMENTARY_1-INFO = S_BNAME.
**
**  APPEND IT_COMMENTARY_1.
**
**  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
**       EXPORTING
**            IT_LIST_COMMENTARY = IT_COMMENTARY_1[].
*
**Write:/ 'List of Absentees'.
**Write:/ S_BNAME.

ENDFORM.                    "BASIC_TOP_OF_PAGE
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'BALVLIST'  EXCLUDING extab. " OF PROGRAM 'ZAPM08_ANBD'.
ENDFORM.                    "PF_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       User Command for ALV List
*----------------------------------------------------------------------*
FORM user_command  USING r_ucomm
                         rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'IC1'.
***  Work Order List (Smartform)
      CLEAR: r_ucomm.
      PERFORM check_order.
      PERFORM call_work_order_list.
***  Call SmartForm 'ZSPM01_WORK_ORDER2'
***  PM Work Order Form

** Insert by furong
      PERFORM get_longtext.
** end of insert
*UD1K922698  10-24-2005 haseeb
      PERFORM presponse.
*UD1K922698  10-24-2005 haseeb

      PERFORM print_form.

** Changed by Furong on 06/29/10
      PERFORM print_attachment.

** End of change
    WHEN 'COUNT'.
***  Countermeasure report (Excel)
      CLEAR: r_ucomm.
      PERFORM call_countermeasure_report.

    WHEN 'F03' OR  'F15' OR 'F12'.
      CLEAR: r_ucomm.
      CALL SCREEN '1000'.
    WHEN '&IC1'.
***  Call Transaction 'IW33' : Display PM Order
      CLEAR: r_ucomm.
      READ TABLE it_order INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'ANR' FIELD it_order-aufnr.
      CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDFORM.                 " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CALL_WORK_ORDER_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_work_order_list.
  DATA: wa_rueck LIKE afru-rueck,
        wa_rmzhl LIKE afru-rmzhl,
        wa_first_item.

  CLEAR: it_zspm_wopo, it_zspm_wopo[].

  LOOP AT it_order WHERE check = 'X'.

    PERFORM read_operation.

    MOVE-CORRESPONDING it_order TO it_temp.

    IF  it_order-equnr IS INITIAL .
      MOVE it_order-tplnr TO it_temp-equnr.
    ENDIF.

    MODIFY it_temp  TRANSPORTING  auart
                                  equnr
                                  msgrp
                                  ktext
                                  qmnam
                                  iedd
*                                  MNCOD
                     WHERE aufnr NE ' '.

    CLEAR: wa_first_item.

    LOOP AT it_temp .
      IF dy_ofn EQ 'X' AND dy_mab EQ ' '.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP-AUFNR
*             AND   VORNR = IT_TEMP-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP-AUFNR
*                             AND   VORNR = IT_TEMP-VORNR )
*             GROUP BY RUECK.
        SELECT SINGLE rueck  rmzhl  INTO (wa_rueck, wa_rmzhl)
               FROM  afru
               WHERE aufnr = it_temp-aufnr
               AND   vornr = it_temp-vornr
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF sy-subrc EQ 0.
          SELECT  SINGLE *
                  FROM afru
                  WHERE aufnr = it_temp-aufnr
                  AND   vornr = it_temp-vornr
                  AND   rueck = wa_rueck
                  AND   rmzhl = wa_rmzhl
                  AND   aueru EQ 'X'
                  AND   stokz EQ ' '
                  AND   stzhl EQ space.
          IF sy-subrc EQ 0.
          ELSE.
            IF wa_first_item EQ ' '.
              wa_first_item = 'X'.
            ELSE.
              CLEAR: it_temp-aufnr, it_temp-auart,
                     it_temp-equnr, it_temp-msgrp, it_temp-ktext.
            ENDIF.
            MOVE-CORRESPONDING it_temp TO it_zspm_wopo.
            APPEND it_zspm_wopo.
          ENDIF.
        ELSE.
          IF wa_first_item EQ ' '.
            wa_first_item = 'X'.
          ELSE.
            CLEAR: it_temp-aufnr, it_temp-auart,
                   it_temp-equnr, it_temp-msgrp, it_temp-ktext.
          ENDIF.
          MOVE-CORRESPONDING it_temp TO it_zspm_wopo.
          APPEND it_zspm_wopo.
        ENDIF.
      ELSEIF dy_ofn EQ ' ' AND dy_mab EQ 'X'.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP-AUFNR
*             AND   VORNR = IT_TEMP-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP-AUFNR
*                             AND   VORNR = IT_TEMP-VORNR )
*             GROUP BY RUECK.
        SELECT SINGLE rueck  rmzhl  INTO (wa_rueck, wa_rmzhl)
               FROM  afru
               WHERE aufnr = it_temp-aufnr
               AND   vornr = it_temp-vornr
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.
        IF sy-subrc EQ 0.
          SELECT  SINGLE *
                  FROM afru
                  WHERE aufnr = it_temp-aufnr
                  AND   vornr = it_temp-vornr
                  AND   rueck = wa_rueck
                  AND   rmzhl = wa_rmzhl
                  AND   aueru EQ 'X'
                  AND   stokz EQ ' '
                  AND   stzhl EQ space.
          IF sy-subrc EQ 0.
            IF wa_first_item EQ ' '.
              wa_first_item = 'X'.
            ELSE.
              CLEAR: it_temp-aufnr, it_temp-auart,
                     it_temp-equnr, it_temp-msgrp, it_temp-ktext.
            ENDIF.
            MOVE-CORRESPONDING it_temp TO it_zspm_wopo.

            CASE it_temp-mncod.
              WHEN '01'.
                MOVE 'v' TO it_zspm_wopo-bad.
              WHEN '02'.
                MOVE 'v' TO it_zspm_wopo-comp.
              WHEN '03'.
                MOVE 'v' TO it_zspm_wopo-good.
              WHEN OTHERS.
            ENDCASE.

            APPEND it_zspm_wopo.
          ENDIF.
        ENDIF.
      ELSEIF dy_ofn EQ 'X' AND dy_mab EQ 'X'.

        IF wa_first_item EQ ' '.
          wa_first_item = 'X'.
        ELSE.
          CLEAR: it_temp-aufnr, it_temp-auart,
                 it_temp-equnr, it_temp-msgrp, it_temp-ktext.
        ENDIF.
        MOVE-CORRESPONDING it_temp TO it_zspm_wopo.

        CASE it_temp-mncod.
          WHEN '01'.
            MOVE 'v' TO it_zspm_wopo-bad.
          WHEN '02'.
            MOVE 'v' TO it_zspm_wopo-comp.
          WHEN '03'.
            MOVE 'v' TO it_zspm_wopo-good.
          WHEN OTHERS.

        ENDCASE.
        APPEND it_zspm_wopo.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " CALL_WORK_ORDER_LIST
*&---------------------------------------------------------------------*
*&      Form  READ_OPERATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_operation.
  CLEAR: it_temp, it_temp[].

  SELECT  a~aufnr b~vornr b~ltxa1 c~qmnum
          INTO CORRESPONDING FIELDS OF TABLE it_temp
          FROM caufv AS a
               INNER JOIN afvc AS b
               ON  a~aufpl = b~aufpl
*              AND A~APLZL = B~APLZL
                   INNER JOIN viqmel AS c
                   ON a~aufnr = c~aufnr
           WHERE a~aufnr = it_order-aufnr.
  LOOP AT  it_temp.
    SELECT SINGLE *
                  FROM qmma
                  WHERE qmnum = it_temp-qmnum.
    MOVE qmma-mncod TO it_temp-mncod.
    MODIFY it_temp.
  ENDLOOP.

ENDFORM.                    " READ_OPERATION
*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_form.
  DATA : wa_formname TYPE tdsfname VALUE 'ZSPM01_WORK_ORDER2_LTEXT'.
  DATA : wa_funcname TYPE rs38l_fnam .

  CLEAR: it_zspm_wopo.

**** read Functional location text
  CLEAR: iflo.

  SELECT  SINGLE *
          FROM   iflo
          WHERE  tplnr = it_order-tplnr
          AND    spras = sy-langu
          AND    lvorm = space.

**** Read Name of the Maintenance Planner Group
  SELECT SINGLE innam INTO zspm_woko-innam
         FROM   t024i
         WHERE  iwerk = iflo-iwerk
         AND    ingrp = s_ingrp-low.

*** 12/10/2003 }}

**** Read Description of functional location
  READ TABLE it_zspm_wopo INDEX 1.
  SELECT SINGLE pltxt INTO zspm_woko-pltxt
         FROM iflotx
         WHERE tplnr = it_order-tplnr
         AND   spras = sy-langu.

*** Reported by
  SELECT SINGLE b~name_text
         INTO wa_zspm_woko-ernam
         FROM usr21 AS a
              INNER JOIN adrp AS b
              ON a~persnumber = b~persnumber
         WHERE a~bname = sy-uname.

  IF dy_mab EQ 'X'.
    READ TABLE it_zspm_wopo INDEX 1.
    MOVE : sy-datum        TO wa_zspm_woko-gstrp,
        zspm_woko-fing     TO wa_zspm_woko-fing,
        zspm_woko-pltxt    TO wa_zspm_woko-pltxt,
        zspm_woko-innam    TO wa_zspm_woko-innam,
        it_zspm_wopo-qmnam TO wa_zspm_woko-ernam,
        it_zspm_wopo-iedd  TO wa_zspm_woko-erdat.

  ELSE.
    MOVE : sy-datum        TO wa_zspm_woko-gstrp,
           zspm_woko-fing  TO wa_zspm_woko-fing,
           zspm_woko-pltxt TO wa_zspm_woko-pltxt,
           zspm_woko-innam TO wa_zspm_woko-innam,
           ' '             TO wa_zspm_woko-ernam,
           sy-datum        TO wa_zspm_woko-erdat.
  ENDIF.


*** SAP Smart Forms: Form call
*** Get SmartForm's function name (like : /1BCDWB/SF00000036 )
*** Because funtion name is dependent on system client...
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = wa_formname
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = wa_funcname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK sy-subrc = 0.

*** Call Smart Form ZSPM01_WORK_ORDER2 Call function...
  CALL FUNCTION wa_funcname
    EXPORTING
      i_zspm_woko      = wa_zspm_woko
    TABLES
      it_zspm_wopo     = it_zspm_wopo
      it_longtext      = it_longtext
      it_order_num     = it_order_num
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " PRINT_FORM
*&---------------------------------------------------------------------*
*&      Form  CALL_COUNTERMEASURE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_countermeasure_report.
  DATA: wa_count TYPE i.

*** call transaction 'ZPMR02' Breakdwon Countermeasure report
  LOOP AT it_order WHERE check = 'X'.
    wa_count = wa_count + 1.
    IF wa_count > 1.
      MESSAGE e000(zdpm) WITH text-m03.
    ENDIF.
  ENDLOOP.

  READ TABLE it_order WITH KEY check = 'X'.
  IF sy-subrc EQ 0.
    SET PARAMETER ID 'ANR' FIELD it_order-aufnr.
    CALL TRANSACTION 'ZPMR02' AND SKIP FIRST SCREEN .
  ENDIF.
ENDFORM.                    " CALL_COUNTERMEASURE_REPORT
*&---------------------------------------------------------------------*
*&      Form  FILL_R_IPHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_r_iphas.
  REFRESH r_iphas.

  r_iphas-sign   = 'I'.
  r_iphas-option = 'EQ'.

  r_iphas-low = '2'.                   "FREI
  APPEND r_iphas.
  r_iphas-low = '3'.                   "technisch abgeschlossen
  APPEND r_iphas.
*--- Bei IW48 nur Auftr?e die r?kgemeldet werden k?nen -----------*
  IF sy-tcode <> 'IW48'.
    r_iphas-low = '0'.                 "OFFEN
    APPEND r_iphas.
    r_iphas-low = '1'.                 "ZUR?KGEST
    APPEND r_iphas.
    r_iphas-low = '4'.                 "L?chkennzeichen
    APPEND r_iphas.
    r_iphas-low = '6'.                 "kaufm?nisch abgeschlossen(ab46)
    APPEND r_iphas.
  ENDIF.

ENDFORM.                    " FILL_R_IPHAS
*&---------------------------------------------------------------------*
*&      Form  CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_countermeasure.
  DATA: l_plgrp LIKE viaufkst-plgrp.
  LOOP AT it_order.
    MOVE : sy-tabix TO wa_index.

    SELECT SINGLE *
           FROM viqmel
           WHERE qwrnum = it_order-qmnum
           AND  NOT EXISTS ( SELECT * FROM jest
                               WHERE objnr   = viqmel~objnr
                               AND   ( stat  = 'I0076'
                               OR      stat  = 'I0043' ) ).
    IF sy-subrc EQ 0.
      MOVE : '@01@' TO it_order-zcounter.
      MODIFY it_order INDEX wa_index.
    ENDIF.
** Changed by Furong on 07/13/09
*    IF PLGRP[] IS INITIAL.
*    ELSE.
    SELECT SINGLE plgrp INTO l_plgrp
      FROM afko
      WHERE aufnr = it_order-aufnr.
    IF sy-subrc = 0 AND l_plgrp IN plgrp.
      MOVE : l_plgrp TO it_order-plgrp.
      MODIFY it_order INDEX wa_index.
    ELSE.
      DELETE it_order INDEX wa_index.
    ENDIF.
*    ENDIF.
** End of change
  ENDLOOP.
ENDFORM.                    " CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_order.

ENDFORM.                    " CHECK_ORDER

*** INSERT BY FURONG

FORM read_longtext TABLES   p_tline STRUCTURE tline
                   USING    p_thead    STRUCTURE thead.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            client                  = sy-mandt
            id                      = p_thead-tdid
            language                = sy-langu
            name                    = p_thead-tdname
            object                  = p_thead-tdobject
            archive_handle          = 0
            local_cat               = ' '
*       IMPORTING
*            HEADER                  =
       TABLES
            lines                   = p_tline
       EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  get_longtext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_longtext.
  DATA : lw_thead LIKE thead.
  DATA: wa_aufnr LIKE afko-aufnr.
  DATA: w_neworder(1).

  REFRESH it_afko.
  REFRESH it_longtext.
  REFRESH it_order_num.
  CLEAR it_order_num.
  CLEAR it_afko.
  CLEAR it_longtext.
  SELECT DISTINCT a~aufnr b~vornr a~aufpl b~aplzl
                                  INTO CORRESPONDING FIELDS
                                  OF TABLE it_afko FROM afko AS a
                                  INNER JOIN afvc AS b
                                  ON a~aufpl = b~aufpl
                                  FOR ALL ENTRIES IN it_zspm_wopo
*                                  where aufnr = it_zspm_wopo-aufnr
                                  WHERE b~vornr = it_zspm_wopo-vornr.
  LOOP AT it_zspm_wopo.
    REFRESH it_tline.
    CLEAR it_tline.
    CLEAR lw_thead.
    IF NOT it_zspm_wopo-aufnr IS INITIAL.
      wa_aufnr = it_zspm_wopo-aufnr.
      it_order_num-aufnr = it_zspm_wopo-aufnr.
      w_neworder = 'Y'.
      APPEND it_order_num.
      CLEAR it_order_num.
    ELSE.
*      clear w_neworder.
      it_zspm_wopo-aufnr = wa_aufnr.
      MODIFY it_zspm_wopo.
    ENDIF.
    READ TABLE it_afko WITH KEY aufnr = wa_aufnr
                       vornr = it_zspm_wopo-vornr.
    IF sy-subrc = 0.
      CONCATENATE sy-mandt it_afko-aufpl it_afko-aplzl
                  INTO lw_thead-tdname.
      MOVE : sy-mandt TO lw_thead-tdspras,
             'AVOT' TO lw_thead-tdid,
             'AUFK' TO lw_thead-tdobject.
      PERFORM read_longtext  TABLES  it_tline
                             USING   lw_thead.
      IF sy-subrc = 0 AND NOT it_tline[] IS INITIAL.
        it_zspm_wopo-ltext = 'Y'.
*        it_longtext-aufnr = it_zspm_wopo-aufnr.
        it_longtext-aufnr = wa_aufnr.
        it_longtext-neworder = w_neworder.
        it_longtext-vornr = it_zspm_wopo-vornr.
        it_longtext-ltxa1 = it_zspm_wopo-ltxa1.
        it_longtext-newop = 'X'.
        LOOP AT it_tline.
          it_longtext-longtext = it_tline-tdline.
          APPEND it_longtext.
          CLEAR it_longtext-newop.
          CLEAR w_neworder.
        ENDLOOP.
        CLEAR it_longtext.
        MODIFY it_zspm_wopo.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM it_order_num.
  SORT it_order_num BY aufnr.
  SORT it_zspm_wopo BY aufnr vornr.
  LOOP AT it_order_num.
    it_order_num-ltext = 'Y'.
    MODIFY it_order_num.
    EXIT.
  ENDLOOP.
  LOOP AT it_longtext.
    CLEAR it_longtext-neworder.
    MODIFY it_longtext.
    EXIT.
  ENDLOOP.

ENDFORM.                    " get_longtext
*&---------------------------------------------------------------------*
*&      Form  PRESPONSE
*&---------------------------------------------------------------------*
*       text   *UD1K922698  10-24-2005 haseeb
*  Pass Person responsible to the smart form.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM presponse.
*
  DATA : wa_ihpa LIKE ihpa-parnr,
         wa_pa0001 LIKE pa0001-ename.
  DATA : ordtemp(14) TYPE c.
  FIELD-SYMBOLS  <fs> LIKE it_zspm_wopo.
  CLEAR: it_zspm_wopo.
  LOOP AT it_zspm_wopo ASSIGNING <fs>.

    CONCATENATE 'OR' <fs>-aufnr INTO ordtemp.
    SELECT SINGLE parnr INTO wa_ihpa FROM ihpa WHERE objnr = ordtemp
          AND parvw = 'VW' AND obtyp = 'ORI'.
    SELECT SINGLE ename INTO wa_pa0001 FROM pa0001
          WHERE pernr = wa_ihpa.
    <fs>-ename =   wa_pa0001.
    CLEAR wa_pa0001.
    CLEAR wa_ihpa.
  ENDLOOP.

ENDFORM.                    " PRESPONSE
*&---------------------------------------------------------------------*
*&      Form  print_attachment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_attachment.

  DATA: l_hide(1),
        l_objkey(15),
        l_plnnr LIKE afko-plnnr,
        l_plnal LIKE  afko-plnal.
  DATA: it_attachment LIKE TABLE OF plko WITH HEADER LINE.

  l_hide = 'Y'.

  LOOP AT it_order_num.
    CLEAR: l_plnnr, l_plnal.
    REFRESH: it_attachment.

    SELECT SINGLE plnnr plnal INTO (l_plnnr, l_plnal)
     FROM afko
     WHERE aufnr = it_order_num-aufnr.

    SELECT * INTO TABLE it_attachment
     FROM plko
     WHERE ( plnty = 'E' OR plnty = 'A' )
       AND plnnr = l_plnnr
       AND plnal = l_plnal
       AND loekz = ' '.
*  IFLO-ppsid.
    LOOP AT it_attachment.
      CONCATENATE it_attachment-plnty it_attachment-plnnr
                  it_attachment-plnal INTO l_objkey.

      PERFORM retrieve_and_print_documents
       USING 'BUS1019'
*            'E9000005201'  " Probably Equipment Number
              l_objkey
              l_hide.
*    WAIT UP TO 8 SECONDS.
    ENDLOOP.
  ENDLOOP.


*  SELECT SINGLE PLNNR INTO L_PLNNR
*   FROM AFKO
*   WHERE AUFNR = IT_ORDER-AUFNR.
*
*  SELECT * INTO TABLE IT_ATTACHMENT
*   FROM PLKO
*   WHERE ( PLNTY = 'E' OR PLNTY = 'A' )
*     AND PLNNR = L_PLNNR
*     AND LOEKZ = ' '.
**  IFLO-ppsid.
*  LOOP AT IT_ATTACHMENT.
*    CONCATENATE IT_ATTACHMENT-PLNTY IT_ATTACHMENT-PLNNR
*                IT_ATTACHMENT-PLNAL INTO L_OBJKEY.
*
*    PERFORM RETRIEVE_AND_PRINT_DOCUMENTS
*     USING 'BUS1019'
**            'E9000005201'  " Probably Equipment Number
*            L_OBJKEY
*            L_HIDE.
**    WAIT UP TO 8 SECONDS.
*  ENDLOOP.

ENDFORM.                    " print_attachment
*********************************************************************
*     Form  RETRIEVE_AND_PRINT_DOCUMENTS
*********************************************************************
FORM retrieve_and_print_documents USING objtyp
                                        objkey
                                        hide.

  TABLES: plko, sood.
  TYPE-POOLS ole2 .


** Furong on 01/30/12
**  DATA: DOCUMENTS TYPE STANDARD TABLE OF NEIGHBOR.
*  DATA: DOCUMENT  TYPE NEIGHBOR.
**  DATA: DOCS      LIKE LINE OF DOCUMENTS.
*  DATA: DOCS      TYPE NEIGHBOR.
*  DATA:   XOBJECT LIKE BORIDENT.
  DATA: documents TYPE obl_t_link.
  DATA: document  TYPE obl_t_link.
  DATA: docs      LIKE LINE OF documents.
  DATA: xobject TYPE sibflporb.
** end on 01/30/12

  DATA: objects TYPE STANDARD TABLE OF sood4.
  DATA: object  TYPE sood4.
  DATA: word TYPE ole2_object.
  DATA: excel TYPE ole2_object.
  DATA: workbooks TYPE ole2_object.
  DATA: app TYPE ole2_object.
  DATA: filename(200) TYPE c.

  DATA: g_bor_object TYPE sibflporb,
        g_bor_logsys TYPE logsys.

** Furong on 01/30/12
*        DOCUMENTS Type OBL_T_LINK.
** End on 01/30/12

* If background, then do even try it!
*  check sy-batch = 'X'.

** Changed on 01/30/12 by Furong  for ECC
* Retrieve Documents
*  XOBJECT-OBJTYPE = OBJTYP.
*  XOBJECT-OBJKEY  = OBJKEY.
  xobject-instid = objkey.
  xobject-typeid = objtyp.
  xobject-catid = 'BO'.

  CLEAR document. REFRESH documents.
  .
*  CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
*       EXPORTING
*           OBJECT       = XOBJECT
*           ROLETYPE     = 'APPLOBJ'
*           RELATIONTYPE = 'ATTA'
*       TABLES
*            NEIGHBORS    = DOCUMENTS
*       EXCEPTIONS
*            INTERNAL_ERROR       = 1
*            NO_LOGSYS            = 2
*            OTHERS               = 3.


  TRY.

      DATA : l_rol TYPE oblroltype.


      l_rol = 'GOSAPPLOBJ'.

      CALL METHOD cl_binary_relation=>read_links_of_binrel
        EXPORTING
          is_object   = xobject
*         ip_logsys   = g_bor_logsys
          ip_relation = 'ATTA'
          ip_role     = l_rol
        IMPORTING
          et_links    = documents.

    CATCH cx_obl_parameter_error .

    CATCH cx_obl_internal_error .

    CATCH cx_obl_model_error .

  ENDTRY.

** End on 01/30/12

*  TRY.
*      DATA : l_rol TYPE oblroltype.
*      l_rol = 'GOSAPPLOBJ'.
*      CALL METHOD cl_binary_relation=>read_links_of_binrel
*        EXPORTING
*          is_object   = g_bor_object
*          ip_logsys   = g_bor_logsys
*          ip_relation = 'ATTA'
*          ip_role     = l_rol
*        IMPORTING
*          et_links    = DOCUMENTS.
*    CATCH cx_obl_parameter_error .
*    CATCH cx_obl_internal_error .
*    CATCH cx_obl_model_error .
*  ENDTRY.

* If no documents, then exit..
  IF documents[] IS INITIAL.
    EXIT.
  ENDIF.

  LOOP AT documents INTO docs.

** Changed on 01/30/12 by Furong  for ECC

*    SELECT SINGLE * FROM SOOD
*                    WHERE OBJTP = DOCS-OBJKEY+17(3)
*                      AND OBJYR = DOCS-OBJKEY+20(2)
*                      AND OBJNO = DOCS-OBJKEY+22(12).

    SELECT SINGLE * FROM sood
                    WHERE objtp = docs-instid_b+17(3)
                      AND objyr = docs-instid_b+20(2)
                      AND objno = docs-instid_b+22(12).
** eND ON 01/30/12

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

* Create the Microsoft Application Object
    CASE sood-file_ext.
      WHEN 'DOC'.
        CREATE OBJECT word 'WORD.BASIC'.
      WHEN 'XLS'.
        CREATE OBJECT excel 'EXCEL.APPLICATION'.
    ENDCASE.

* Open Document.....
    CLEAR object.
    CLEAR objects. REFRESH objects.

** Changed on 01/30/12 by Furong  for ECC
*    OBJECT = DOCS-OBJKEY.  APPEND OBJECT TO OBJECTS.
    object = docs-instid_b.  APPEND object TO objects.
** end on 01/30/12


    CALL FUNCTION 'SO_DOCUMENT_DISPATCH_MANAGER'
      EXPORTING
        activity = 'DISP'
      TABLES
        objects  = objects.

    IF hide =  'Y'.
*
* Hide Microsoft Application
      CASE sood-file_ext.
        WHEN 'DOC'.
          CALL METHOD OF
              word
              'APPHIDE'.
        WHEN 'XLS'.
          SET PROPERTY OF excel 'VISIBLE' = 0.
      ENDCASE.


      WAIT UP TO 2 SECONDS.

* Print the Document
      CASE sood-file_ext.
        WHEN 'DOC'.
          CALL METHOD OF
              word
              'FILEPRINT'.
        WHEN 'XLS'.
          GET PROPERTY OF excel 'ACTIVEWORKBOOK' = workbooks.
          CALL METHOD OF
              workbooks
              'PRINTOUT'.
      ENDCASE.

*      WAIT UP TO 8 SECONDS.

* Close Microsoft Application
      CASE sood-file_ext.
        WHEN 'DOC'.
          CALL METHOD OF
              word
              'APPCLOSE'.
        WHEN 'XLS'.
          CALL METHOD OF
              excel
              'QUIT'.
      ENDCASE.

    ELSE.

* Minimize Microsoft Application
      CASE sood-file_ext.
        WHEN 'DOC'.
          CALL METHOD OF
              word
              'APPMINIMIZE'.
        WHEN 'XLS'.
          GET PROPERTY OF excel 'APPLICATION' = app.
*          set property of app 'WINDOWSTATE' = 2.
      ENDCASE.

    ENDIF.

* Free the Object
    CASE sood-file_ext.
      WHEN 'DOC'.
        FREE OBJECT word.
      WHEN 'XLS'.
        FREE OBJECT excel.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    "RETRIEVE_AND_PRINT_DOCUMENTS
