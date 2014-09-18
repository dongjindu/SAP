*&---------------------------------------------------------------------*
*& Report  ZRIMMATTREE1                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입자재별 Hierachy PROGRAM                           *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.16                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. LDB를 사용하여 프로그램.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMMATTREE1 NO STANDARD PAGE HEADING .

INCLUDE <ctldef>.
INCLUDE cnt4defs.
INCLUDE ZRIM01T02 .

TYPE-POOLS: cntl, stree.

TABLES: dwtree, rseu0, demotree, demotreet, seucomm, snodetext.
TABLES: trdir, tstc, demo_rb, tstct.

TABLES: LFA1,
        ZTREQIT_TMP,
        ZTREQST,      ">
        T023T,        ">자재그룹.
        T001,         ">회사코드.
        T001W,        ">플랜트.
        T001L,        ">저장위치.
        ZTINSRSP,     ">보험부보내역.
        ZTIEPORT,     ">[수출입] 국가별 Port코드 관리.
        ZTIMIMG00,    ">[수입] IMG Setting.
        ZTIMIMG02,    ">[수입] 세관 코드.
        ZTIMIMG03,    ">[수입] 보세구역 코드.
        ZTIMIMG10.    ">[수입] 관세사 관리.

DATA: REPID LIKE SY-REPID,
      DYNNR LIKE SY-DYNNR.


DATA: answer.
  DATA: nodekey TYPE tv_nodekey,
          node TYPE snodetext.
  data: nodekeytab type treev_nks.
  data: itemname type tv_itmname.
  clear: node, nodekey, itemname.

* Define Table Type For Data Exchange.
TYPES: BEGIN OF MYTABLE_LINE,
       TDLINE LIKE ZTIMGTXT-TDLINE,
       END OF MYTABLE_LINE.

* Table To Exchange Text.
DATA SRC     TYPE TABLE OF MYTABLE_LINE.
DATA SRC_TMP TYPE TABLE OF ZTIMGTXT WITH HEADER LINE.
DATA: ctrl_node_tab TYPE STANDARD TABLE OF treev_node,
      ctrl_node_tab2 TYPE STANDARD TABLE OF treev_node,
      ctrl_item_tab TYPE STANDARD TABLE OF demo_item,
      ctrl_node TYPE treev_node,
      ctrl_item TYPE demo_item,
      header_tab TYPE STANDARD TABLE OF treev_hdr,
      column_tab TYPE STANDARD TABLE OF treev_col,
      header TYPE treev_hdr,
      hierarchy_header TYPE treev_hhdr,
      column TYPE treev_col.

DATA  fcode LIKE sy-ucomm.
DATA editor TYPE REF TO cl_gui_textedit.
DATA:  level TYPE i VALUE -1.

DATA: BEGIN OF tree OCCURS 100.
        INCLUDE STRUCTURE streenode.
DATA: END OF tree.
DATA: BEGIN OF nodetab OCCURS 100.
        INCLUDE STRUCTURE snodetext.
DATA: END OF nodetab.

DATA: customer_id(10) VALUE 'DEMO'.
DATA: BEGIN OF layout_id,
       'DEMO_LAYOUT_',
       user LIKE sy-uname,
      END OF layout_id.

DATA: BEGIN OF marktab OCCURS 10.
        INCLUDE STRUCTURE snodetext.
DATA: END OF marktab.

DATA: bool.
DATA: true  LIKE bool VALUE 'T'.
DATA: false LIKE bool VALUE 'F'.

DATA  value_request  LIKE bool.
DATA  save_necessary LIKE bool.

DATA: ok_code(4).

DATA: BEGIN OF layout OCCURS 1.
        INCLUDE STRUCTURE seutexpand.
DATA: END OF layout.

DATA: cucol TYPE i,
      culin TYPE i,
      licol TYPE i,
      lilin TYPE i.
DATA: first_node LIKE streenode-id.

DATA: tlength(2) TYPE n VALUE '40',
      col_begin  TYPE i,
      col_end    TYPE i.

DATA: BEGIN OF deltab OCCURS 10,
        name    LIKE aptree-name,
      END OF deltab.

DATA: BEGIN OF add_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF add_nodes.
DATA: BEGIN OF mod_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF mod_nodes.
DATA: BEGIN OF del_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF del_nodes.

DATA: act_edit(2) VALUE '02',
      act_show(2) VALUE '03'.
DATA  repname LIKE sy-repid.
DATA  BEGIN OF texttab OCCURS 50.
        INCLUDE STRUCTURE textpool.
DATA  END   OF texttab.
DATA  demo_id(10) VALUE 'DEMO'.
DATA  systemtype(10).

DATA: TEXT_ID(3)  TYPE N,
      FIELDNM(8)  TYPE C,
      W_STATUS    TYPE C.

FIELD-SYMBOLS <F>.

DATA TREEV TYPE REF TO CL_GUI_COLUMN_TREE.

NODES  : ZVPOHD_ITM,
         EKAB,
         EKBE,
         ZVEKBE,
         ZVCIVHD_IT,
         ZVCIVHD_IT1,
         ZTINS,
         ZVBLIT,
         ZTBLINR,
         ZTBLOUR,
         ZVIVHD_IT,
         ZVBWIT,
         ZVIVHSTIT,
         ZVIDRST,
         ZTIDS,
         ZVCGIT.

*DATA: node_handler type ref to c_event_handler,
*      item_handler type ref to c_event_handler.
*DATA: H_SPLITTER TYPE CNTL_HANDLE,
*      H_TREE TYPE CNTL_HANDLE.
*DATA: PARENT LIKE NODETAB-PARENT.
*DATA: ROOTID LIKE NODETAB-ID.
*DATA EVENTS TYPE CNTL_SIMPLE_EVENTS.
*Data evt type cntl_simple_event.
*data pic type ref to cl_gui_picture.
*FIELD-SYMBOLS: <FS> LIKE NODETAB, <GS> LIKE NODETAB.


DATA: activexactive.
DATA PIC_DATA LIKE W3MIME OCCURS 0.
DATA PIC_SIZE TYPE I.
  data url(255) type c.                " URL-field in screen 200
  data url2(255) type c.               " URL-field in screen 200
*DATA: node_rclick_handler TYPE REF TO c_rmouse_click,
*      item_rclick_handler TYPE REF TO c_rmouse_click,
*      node_select_handler TYPE REF TO c_rmouse_click,
*      item_select_handler TYPE REF TO c_rmouse_click.
DATA: p_menu TYPE REF TO cl_ctmenu.
DATA: gnode TYPE snodetext.
DATA: grelship TYPE stree_relation_type.
DATA: behaviour_left type ref to cl_dragdrop,
      behaviour_right type ref to cl_dragdrop.
DATA: handle_tree type i.
*DATA: dragdrop TYPE REF TO lcl_dragdrop_receiver.

DATA: G_ID      LIKE  NODETAB-ID,
      G_MATNR   LIKE  EKPO-MATNR,
      G_TXZ01   LIKE  EKPO-TXZ01,
      G_EBELN   LIKE  EKPO-EBELN,
      G_EBELP   LIKE  EKPO-EBELP,
      G_EBELN1  LIKE  EKPO-EBELN,
      G_EBELP1  LIKE  EKPO-EBELP,
      G_VGABE   LIKE  EKBE-VGABE,
      G_ZFREQNO LIKE  ZTREQIT-ZFREQNO,
      G_ZFITMNO LIKE  ZTREQIT-ZFITMNO,
      G_TABIX   LIKE  SY-TABIX,
      G_TEXT35(35),
      G_TEXT20(20).


DATA: BEGIN OF XZVPOHD_ITM OCCURS 0.
      INCLUDE STRUCTURE ZVPOHD_ITM.
      DATA:   ID    LIKE NODETAB-ID.
DATA: END   OF XZVPOHD_ITM.

DATA: BEGIN OF XZTINS OCCURS 0.
      INCLUDE STRUCTURE ZTINS.
      DATA:   ID    LIKE NODETAB-ID.
DATA: END   OF XZTINS.

DATA: BEGIN OF XEKBE OCCURS 5,
      VGABE   LIKE EKBE-VGABE,
      MENGE   LIKE EKBE-MENGE,
      END   OF XEKBE.
*DATA: BEGIN OF XZTINS OCCURS 0.
*      INCLUDE STRUCTURE ZTINS.
*      DATA:   ID    LIKE NODETAB-ID.
*DATA: END   OF XZTINS.



INITIALIZATION.
  PERFORM  P2000_SET_DATA_INITIAL.

*&---------------------------------------------------------------------*
*>
*&---------------------------------------------------------------------*
START-OF-SELECTION.
   SET TITLEBAR 'TIT1'.

*> 구매문서/수입의뢰.
   GET ZVPOHD_ITM.
   IF G_MATNR NE ZVPOHD_ITM-MATNR OR
      G_TXZ01 NE ZVPOHD_ITM-TXZ01.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      SELECT SINGLE * FROM T023T
             WHERE    SPRAS  EQ  SY-LANGU
             AND      MATKL  EQ  ZVPOHD_ITM-MATKL.

      WRITE : /(20) '자재내역',
               ZVPOHD_ITM-MATNR,   ZVPOHD_ITM-TXZ01,
           (25)T023T-WGBEZ.
      G_MATNR = ZVPOHD_ITM-MATNR.
      G_TXZ01 = ZVPOHD_ITM-TXZ01.
   ENDIF.

   IF G_EBELN NE ZVPOHD_ITM-EBELN OR
      G_EBELP NE ZVPOHD_ITM-EBELP.
      FORMAT COLOR COL_GROUP  INTENSIFIED OFF.
      SELECT SINGLE * FROM LFA1
             WHERE    LIFNR EQ ZVPOHD_ITM-LIFNR.
      IF ZVPOHD_ITM-BSTYP EQ 'K'.
         WRITE : /(20) '.일괄계약'.
      ELSEIF ZVPOHD_ITM-BSTYP EQ 'L'.
         WRITE : /(20) '.납품계약'.
      ELSE.
         WRITE : /(20) '.구매문서'.
      ENDIF.
      WRITE : "/(20) '.구매문서',
               ZVPOHD_ITM-EBELN,   ZVPOHD_ITM-EBELP,
               ZVPOHD_ITM-MENGEPO UNIT ZVPOHD_ITM-MEINSPO,
                                   ZVPOHD_ITM-MEINSPO,
                                   LFA1-NAME1,
                                   ZVPOHD_ITM-BEDAT.
      G_EBELN = ZVPOHD_ITM-EBELN.
      G_EBELP = ZVPOHD_ITM-EBELP.
   ENDIF.

   IF G_ZFREQNO NE ZVPOHD_ITM-ZFREQNO OR
      G_ZFITMNO NE ZVPOHD_ITM-ZFITMNO.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      SELECT * FROM  ZTREQIT_TMP
               WHERE ZFREQNO  EQ ZVPOHD_ITM-ZFREQNO
               AND   ZFITMNO  EQ ZVPOHD_ITM-ZFITMNO
               AND   EBELN    EQ ZVPOHD_ITM-EBELN
               AND   EBELP    EQ ZVPOHD_ITM-EBELP
               ORDER BY ZFAMDNO.

         SELECT SINGLE * FROM ZTREQST
                WHERE    ZFREQNO  EQ ZTREQIT_TMP-ZFREQNO
                AND      ZFAMDNO  EQ ZTREQIT_TMP-ZFAMDNO.
         IF ZTREQST-ZFAMDNO IS INITIAL.
            WRITE : /(20) '..수입의뢰 No',
                     ZTREQIT_TMP-ZFREQNO,   ZTREQIT_TMP-ZFITMNO,
                     ZTREQIT_TMP-MENGE UNIT ZTREQIT_TMP-MEINS,
                                            ZTREQIT_TMP-MEINS,
                                            ZTREQST-ZFOPNNO.

         ELSE.
            WRITE : /(25) '..Amend Seq',
                     ZTREQIT_TMP-ZFAMDNO,   ZTREQIT_TMP-ZFITMNO,
                     ZTREQIT_TMP-MENGE UNIT ZTREQIT_TMP-MEINS,
                                            ZTREQIT_TMP-MEINS,
                                            ZTREQST-ZFOPNNO.
         ENDIF.
         IF NOT ZTREQST-ZFOPNDT IS INITIAL.
            WRITE  ZTREQST-ZFOPNDT.
         ELSE.
            IF ZTREQST-ZFAPPDT IS INITIAL.
               WRITE ZTREQST-CDAT.
            ELSE.
               WRITE ZTREQST-ZFAPPDT.
            ENDIF.
         ENDIF.
      ENDSELECT.

      SELECT SINGLE * FROM ZTREQST
             WHERE    ZFREQNO EQ ZVPOHD_ITM-ZFREQNO
             AND      ZFAMDNO EQ
                    ( SELECT MAX( ZFAMDNO )
                             FROM ZTREQST
                             WHERE  ZFREQNO EQ ZVPOHD_ITM-ZFREQNO ).
      IF ZTREQST-ZFAMDNO IS INITIAL.
         WRITE : /(20) '..수입의뢰 No',
               ZVPOHD_ITM-ZFREQNO, ZVPOHD_ITM-ZFITMNO,
               ZVPOHD_ITM-MENGE UNIT ZVPOHD_ITM-MEINS,
                                     ZVPOHD_ITM-MEINS,
                                     ZTREQST-ZFOPNNO.
      ELSE.
         WRITE : /(25) '..Amend Seq',
               ZTREQST-ZFAMDNO,    ZVPOHD_ITM-ZFITMNO,
               ZVPOHD_ITM-MENGE UNIT ZVPOHD_ITM-MEINS,
                                     ZVPOHD_ITM-MEINS,
                                     ZTREQST-ZFOPNNO.
      ENDIF.
      IF NOT ZTREQST-ZFOPNDT IS INITIAL.
         WRITE  ZTREQST-ZFOPNDT.
      ELSE.
         IF ZTREQST-ZFAPPDT IS INITIAL.
            WRITE ZTREQST-CDAT.
         ELSE.
            WRITE ZTREQST-ZFAPPDT.
         ENDIF.
      ENDIF.
      G_ZFREQNO = ZVPOHD_ITM-ZFREQNO.
      G_ZFITMNO = ZVPOHD_ITM-ZFITMNO.
   ENDIF.

*-------------------------------------------------------------------
*> 일괄계약일 경우.. ---> LOCAL 입고 이력 SELECT.
*-------------------------------------------------------------------
   GET EKAB.
   IF ZVPOHD_ITM-BSTYP EQ 'K'.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE : /(20) '...릴리즈 구매문서',
                    EKAB-EBELN,
                    EKAB-EBELP,
                    EKAB-MENGE UNIT EKAB-MEINS,
                    EKAB-MEINS,
               (35) '',
                    EKAB-BEDAT.
      REFRESH : XEKBE.
      CLEAR : XEKBE, G_VGABE.
   ENDIF.

   GET EKBE.
   IF ZVPOHD_ITM-BSTYP EQ 'K'.
*      IF G_EBELN1 NE EKAB-EBELN OR
*         G_EBELP1 NE EKAB-EBELP.
*         REFRESH : XEKBE.
*         G_EBELN1 = EKAB-EBELN.
*         G_EBELP1 = EKAB-EBELP.
*      ENDIF.
*
*      IF G_VGABE NE EKBE-VGABE AND
*         NOT G_VGABE IS INITIAL.
*         READ TABLE XEKBE WITH KEY G_VGABE.
*         IF SY-SUBRC EQ 0.
*            G_TABIX  =  SY-TABIX.
*            FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
*            PERFORM   GET_DD07T_SELECT(SAPMZIM01)
*                              USING      'VGABE'   G_VGABE
*                              CHANGING   G_TEXT20.
*            CONCATENATE '....' G_TEXT20   INTO G_TEXT20.
*            WRITE : /(20) G_TEXT20,
*                     (10) '  합  계  ',
*                     (05) SPACE,
*                          XEKBE-MENGE UNIT EKAB-MEINS,
*                          EKAB-MEINS,
*                     (35) '',
*                     (10) ''.
*            DELETE TABLE XEKBE INDEX G_TABIX.
*         ENDIF.
*      ENDIF.

      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                              USING      'VGABE'   EKBE-VGABE
                              CHANGING   G_TEXT20.
      CONCATENATE '....' G_TEXT20   INTO G_TEXT20.
      WRITE : /(20) G_TEXT20,
               (10) EKBE-BELNR,
               (05) EKBE-GJAHR,
                    EKBE-MENGE UNIT EKAB-MEINS,
                    EKAB-MEINS,
               (35) '',
                    EKBE-BUDAT.
      MOVE-CORRESPONDING EKBE TO XEKBE.
      COLLECT XEKBE.
      G_VGABE = EKBE-VGABE.
   ENDIF.

   GET ZVEKBE.
   IF ZVPOHD_ITM-BSTYP NE 'K' AND
      ZTIMIMG00-LOGRIV EQ 'X'.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                              USING      'VGABE'   EKBE-VGABE
                              CHANGING   G_TEXT20.
      CONCATENATE '....' G_TEXT20   INTO G_TEXT20.
      WRITE : /(20) G_TEXT20,
               (10) EKBE-BELNR,
               (05) EKBE-GJAHR,
                    EKBE-MENGE UNIT EKAB-MEINS,
                    EKAB-MEINS,
               (35) '',
                    EKBE-BUDAT.
      MOVE-CORRESPONDING EKBE TO XEKBE.
      COLLECT XEKBE.
      G_VGABE = EKBE-VGABE.
   ENDIF.


*----------------------------------------------------------------------
*> 적하보험.
*----------------------------------------------------------------------
   GET ZTINS.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   IF ZTINS-ZFAMDNO IS INITIAL.
      WRITE : /(25) '..적하보험',
               ZTINS-ZFINSEQ, ZTINS-ZFAMDNO,
               ZVPOHD_ITM-MENGE UNIT ZVPOHD_ITM-MEINS,
                                     ZVPOHD_ITM-MEINS,
                                     ZTINS-ZFINNO.
   ELSE.
      WRITE : /(25) '..적하보험배서',
               ZTINS-ZFINSEQ, ZTINS-ZFAMDNO,
               ZVPOHD_ITM-MENGE UNIT ZVPOHD_ITM-MEINS,
                                     ZVPOHD_ITM-MEINS,
                                     ZTINS-ZFINNO.
   ENDIF.
   SELECT SINGLE * FROM ZTINSRSP
          WHERE  ZFREQNO  EQ  ZTINS-ZFREQNO
          AND    ZFINSEQ  EQ  ZTINS-ZFINSEQ
          AND    ZFAMDNO  EQ  ZTINS-ZFAMDNO.

   IF ZTINSRSP-ZFISDT NE '00000000' AND
      ZTINSRSP-ZFISDT NE ''.
      WRITE : ZTINSRSP-ZFISDT.
   ELSE.
      IF ZTINS-ZFINSDT IS INITIAL.
         WRITE : ZTINS-CDAT.
      ELSE.
         WRITE : ZTINS-ZFINSDT.
      ENDIF.
   ENDIF.

*----------------------------------------------------------------------
*> 선수금.
*----------------------------------------------------------------------
   GET ZVCIVHD_IT.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE : /(20) '...선수금',
               ZVCIVHD_IT-ZFCIVRN, ZVCIVHD_IT-ZFCIVSQ,
               ZVCIVHD_IT-ZFPRQN UNIT ZVCIVHD_IT-MEINS,
                              ZVCIVHD_IT-MEINS,
                              ZVCIVHD_IT-ZFCIDT.

*----------------------------------------------------------------------
*> B/L.
*----------------------------------------------------------------------
   GET ZVBLIT.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   WRITE : /(20) '....B/L',
               ZVBLIT-ZFBLNO, ZVBLIT-ZFBLIT,
               ZVBLIT-BLMENGE UNIT ZVBLIT-MEINS,
                              ZVBLIT-MEINS,
                          (35)ZVBLIT-ZFHBLNO.
   IF ZVBLIT-ZFRETA IS INITIAL.
      WRITE ZVBLIT-ZFETA.
   ELSE.
      WRITE ZVBLIT-ZFRETA.
   ENDIF.
*----------------------------------------------------------------------
*> 상업송장..
*----------------------------------------------------------------------
   GET ZVCIVHD_IT1.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE : /(20) '.....상업송장',
               ZVCIVHD_IT1-ZFCIVRN, ZVCIVHD_IT1-ZFCIVSQ,
               ZVCIVHD_IT1-ZFPRQN UNIT ZVCIVHD_IT1-MEINS,
                              ZVCIVHD_IT1-MEINS,
                              ZVCIVHD_IT1-ZFCIVNO,
                              ZVCIVHD_IT1-ZFCIDT.

*----------------------------------------------------------------------
*> 하역수량...
*----------------------------------------------------------------------
   GET ZVCGIT.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   SELECT SINGLE * FROM T001
          WHERE    BUKRS EQ ZVCGIT-BUKRS.

   SELECT SINGLE * FROM ZTIEPORT
          WHERE    LAND1   EQ   T001-LAND1
          AND      PORT    EQ   ZVCGIT-ZFCGPT.

   SELECT SINGLE * FROM ZTIMIMG03
          WHERE    ZFBNARCD EQ   ZVCGIT-ZFBNARCD.

   WRITE : /(20) '.....하역',
               ZVCGIT-ZFCGNO, ZVCGIT-ZFCGIT,
               ZVCGIT-CGMENGE UNIT ZVCGIT-MEINS,
                              ZVCGIT-MEINS,
*                              ZTIEPORT-PORTT,
                          (35)ZTIMIMG03-ZFBNARM,
                              ZVCGIT-ZFARVLDT.

*----------------------------------------------------------------------
*> 반입....
*----------------------------------------------------------------------
   GET ZTBLINR.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*   SELECT SINGLE ZFBNARM INTO G_TEXT35 FROM ZTIMIMG03
*          WHERE ZFBNAR EQ ZTBLINR-ZFABNAR.
*   MOVE ZTBLINR-ZFINRNO TO  G_TEXT35.
   CONCATENATE ZTBLINR-ZFINRNO(3)   ZTBLINR-ZFINRNO+3(5)
               ZTBLINR-ZFINRNO+8(2) ZTBLINR-ZFINRNO+10(6)
               INTO G_TEXT35
               SEPARATED BY '-'.
   WRITE : /(20) '.....반입',
               ZTBLINR-ZFBLNO, ZTBLINR-ZFBTSEQ,
               ZVBLIT-BLMENGE UNIT ZVBLIT-MEINS,
                              ZVBLIT-MEINS,
                              G_TEXT35,
                              ZTBLINR-ZFGIRDT.

*----------------------------------------------------------------------
*> 반출....
*----------------------------------------------------------------------
   GET ZTBLOUR.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*   MOVE ZTBLOUR-ZFINRNO TO  G_TEXT35.
   CONCATENATE ZTBLOUR-ZFOURNO(3)   ZTBLOUR-ZFOURNO+3(5)
               ZTBLOUR-ZFOURNO+8(2) ZTBLOUR-ZFOURNO+10(6)
               INTO G_TEXT35
               SEPARATED BY '-'.
   WRITE : /(20) '.....반출',
               ZTBLOUR-ZFBLNO, ZTBLOUR-ZFBTSEQ,
               ZVBLIT-BLMENGE UNIT ZVBLIT-MEINS,
                              ZVBLIT-MEINS,
                              G_TEXT35,
                              ZTBLOUR-ZFOTDT.

*----------------------------------------------------------------------
*> 통관요청.....
*----------------------------------------------------------------------
   GET ZVIVHD_IT.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                              USING      'ZDCLCD'  ZVIVHD_IT-ZFCLCD
                              CHANGING   G_TEXT35.
   WRITE : /(20) '......통관요청',
               ZVIVHD_IT-ZFIVNO, ZVIVHD_IT-ZFIVDNO,
               ZVIVHD_IT-GRMENGE UNIT ZVIVHD_IT-MEINS,
               ZVIVHD_IT-MEINS,
               G_TEXT35,
               ZVIVHD_IT-ZFCCDT.

*----------------------------------------------------------------------
*> 수입신고....
*----------------------------------------------------------------------
   GET ZVIDRST.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   SELECT SINGLE * FROM LFA1
          WHERE    LIFNR EQ ( SELECT ZFVEN FROM ZTIMIMG10
                              WHERE  ZFCUT EQ ZVIDRST-ZFCUT ).

   WRITE : /(20) '......수입신고',
               ZVIDRST-ZFBLNO, ZVIDRST-ZFCLSEQ,
               ZVIVHD_IT-CCMENGE UNIT ZVIVHD_IT-MEINS,
               ZVIVHD_IT-MEINS,
               LFA1-NAME1,
               ZVIDRST-ZFIDWDT.

*----------------------------------------------------------------------
*> 수입면허.....
*----------------------------------------------------------------------
   GET ZTIDS.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

   PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                              USING      'ZDCOTM'  ZTIDS-ZFINRC
                              CHANGING   G_TEXT35.

   CONCATENATE ZTIDS-ZFIDRNO '(' G_TEXT35 ')' INTO G_TEXT35.

   WRITE : /(20) '......수입면허',
               ZTIDS-ZFBLNO, ZTIDS-ZFCLSEQ,
               ZVIVHD_IT-CCMENGE UNIT ZVIVHD_IT-MEINS,
               ZVIVHD_IT-MEINS,
               G_TEXT35,
               ZTIDS-ZFIDSDT.

*----------------------------------------------------------------------
*> 보세창고출고....
*----------------------------------------------------------------------
   GET ZVBWIT.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE : /(20) '......보창출고',
               ZVBWIT-ZFGISEQ.

*----------------------------------------------------------------------
*> 입고.....
*----------------------------------------------------------------------
   GET ZVIVHSTIT.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   SELECT SINGLE NAME1 INTO G_TEXT35
          FROM   T001W
          WHERE  WERKS EQ ZVIVHSTIT-WERKS.

   IF NOT ZVIVHSTIT-LGORT IS INITIAL.
      SELECT SINGLE * FROM T001L
             WHERE    WERKS EQ ZVIVHSTIT-WERKS
             AND      LGORT EQ ZVIVHSTIT-LGORT.
      CONCATENATE G_TEXT35 '-' T001L-LGOBE INTO G_TEXT35
                  SEPARATED BY SPACE.
   ENDIF.
   WRITE : /(20) '......입고',
               ZVIVHSTIT-MBLNR, (5) ZVIVHSTIT-MJAHR,
               ZVIVHSTIT-GRMENGE UNIT ZVIVHSTIT-MEINS,
               ZVIVHSTIT-MEINS,
               G_TEXT35,
               ZVIVHSTIT-BUDAT.


*&---------------------------------------------------------------------*
*> TREE MAKE
*&---------------------------------------------------------------------*
END-OF-SELECTION.







*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DATA_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_DATA_INITIAL.

  SET TITLEBAR 'TIT1'.

  REFRESH : XZVPOHD_ITM, NODETAB.

  CLEAR : G_ID, NODETAB,
          G_VGABE,
          G_MATNR,   G_TXZ01,
          G_EBELN,   G_EBELP,
          G_EBELN1,  G_EBELP1,
          G_ZFREQNO, G_ZFITMNO.

ENDFORM.                    " P2000_SET_DATA_INITIAL
