************************************************************************
* Program Name      : ZRQM06R_Q_SCORE_VENDOR_V2
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.23.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Quality Score report by Vendor
*
* Modification Logs
* Date        Developer    RequestNo    Description
* 11/29/2006  Manju        UD1K923253   Add Vendor / Fiscal year to *
*                                       selection screen and
**                                      Performance fine tuning
* 03/13/2007  Manju        UD1K940823   Responsive score program *
*                                       corrections
*
************************************************************************

REPORT  zrqm06r_q_score_vendor_v2  NO STANDARD PAGE HEADING   .
TABLES: icon.

*&&& Data Declaration.  &&&*
*TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <icon>.

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : ztqm_q_score, "/Quality Score Table - AQM05
         mara,         "/Material Master
         lfa1,         "/Vendor Master
         mseg,         "/Document Segment: Material
         qmel,         "/Quality Notification
         mkpf.         "/Header: Material Document

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : zsqm_q_s_vend. "/Quality Score by Vendor Str.
TABLES : zsqm_delivery,
         zsqm_delivery1.

*//InfoType;().
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9000  TYPE TABLEVIEW USING SCREEN 9000.

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF ty_fcode,
        fcode LIKE rsmpe-func,
      END OF ty_fcode.

DATA: it_ex_func TYPE STANDARD TABLE OF ty_fcode WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      wa_ex_func TYPE ty_fcode.


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : c_mark   VALUE 'X'.

*-- Screen Control Mode
CONSTANTS : c_vendor(8)     TYPE c VALUE 'VENDOR',
            c_material(8)   TYPE c VALUE 'MATERIAL',
            c_delivery(8)   TYPE c VALUE 'DELIVERY'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : ok_code LIKE sy-ucomm.
DATA : wa_mode(7) TYPE c,
       wa_status(8) TYPE c.

*--
DATA : wa_return     LIKE	bapireturn1.   "Return Values

*-- User Confirm for pop-up Message
DATA : wa_answer TYPE c.
DATA : wa_repid LIKE sy-repid.

*-- Work area Variables in Program.(WA_xxxx)
DATA : wa_first_date LIKE sy-datum,
       wa_last_date  LIKE sy-datum.

DATA : wa_qmart  TYPE qmart VALUE 'Q2'. "/Noti Type for Defect data

*-- Data Level control
DATA : wa_level(8) TYPE c   VALUE c_vendor.
DATA :  wa_renewal_flg.

DATA : wa_extwg TYPE extwg.  "/to get from other prog.
DATA : wa_ewbez	TYPE ewbez.

DATA : wa_int_date TYPE dats. "/Date for interface

DATA : BEGIN OF wa_sel_vendor,  "/Selected vendor data for detail screen
         lifnr  TYPE lifnr,
         name1  TYPE name1_gp,
       END OF wa_sel_vendor.

  DATA: BEGIN OF it_CNT OCCURS 0,
        lifnum LIKE qmel-lifnum,
        RESPONSIVE like ZSQM_Q_S_VEND-RESPONSIVE,
        wa_num TYPE i,
        END OF it_CNT.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Screnn field cursor control
DATA : wa_fldtxt    LIKE feld-name,  "Field Name Variable
       wa_cur_line  LIKE feld-line.  "Field Line Variable

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
*- Quality Score by Vendor for Material Document data :Collective Data
DATA : it_zsqm_q_s_vend LIKE zsqm_q_s_vend OCCURS 0 WITH HEADER LINE.

*- Quality Score  ALV display : Basic and detail list internal tables
DATA : it_zsqm_q_vend_b LIKE zsqm_q_s_vend OCCURS 0 WITH HEADER LINE.
DATA : it_zsqm_q_vend_d LIKE zsqm_q_s_vend OCCURS 0 WITH HEADER LINE.

*- Quality Score by Vendor for Defect data
DATA : it_zsqm_q_s_def LIKE zsqm_q_s_vend OCCURS 0 WITH HEADER LINE,
       l_responsive like zsqm_q_s_vend-RESPONSIVE,
       l_cnt type i.
*- Internal table for collect data of GR/GI
*DATA : IT_ZSQM_Q_GRGI  LIKE ZSQM_Q_S_VEND OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_zsqm_q_grgi OCCURS 0,
      ebeln LIKE mseg-ebeln,
      ebelp LIKE mseg-ebelp,
      zbudat LIKE mseg-zbudat.
        INCLUDE STRUCTURE zsqm_q_s_vend.
DATA: END OF it_zsqm_q_grgi.

data : begin of it_zsqm_q_s_def_temp occurs 0.
        INCLUDE STRUCTURE zsqm_q_s_vend.
data:    objnr like qmel-objnr,
       end of it_zsqm_q_s_def_temp.


data: wa_zsqm_q_grgi like it_zsqm_q_grgi occurs 0
      with header line.

DATA: BEGIN OF it_zsqm_q_grgi_all OCCURS 0,
      ebeln LIKE mseg-ebeln,
      ebelp LIKE mseg-ebelp,
      zbudat LIKE mseg-zbudat,
      menge LIKE eket-menge,
      wemng LIKE eket-wemng,
      eindt LIKE eket-eindt.
        INCLUDE STRUCTURE zsqm_q_s_vend.
DATA: END OF it_zsqm_q_grgi_all.

DATA: it_grgi_all LIKE it_zsqm_q_grgi_all OCCURS 0 WITH HEADER LINE.

*- Internal table for collect data of quality score from  ZTQM_Q_SCORE
DATA : it_ztqm_q_score LIKE ztqm_q_score  OCCURS 0 WITH HEADER LINE.

*//Ranges; (R_)
*RANGES :

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
*FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
*"                              Table_control Object(CXTAB)

*//Field Group;

* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: wa_custom_control    TYPE   scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS : lcl_event_receiver DEFINITION DEFERRED.

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Global variables for attributes or etc of ALV GRID
DATA : wa_is_layout TYPE lvc_s_layo. "/The Layout Structure
DATA : it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
       it_sort     TYPE lvc_t_sort WITH HEADER LINE.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      wa_variant TYPE disvariant.      "for parameter IS_VARIANT


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME  TITLE text-t01.
*- Period
SELECT-OPTIONS : s_period  FOR ztqm_q_score-issuedat OBLIGATORY
                            DEFAULT wa_first_date TO wa_last_date
                                      NO-EXTENSION .
*- External Material Group
SELECT-OPTIONS :
*                s_extwg  FOR mara-extwg,
                 s_lifnr  for lfa1-lifnr.

PARAMETERS: p_werks LIKE marc-werks,
            p_mjahr like mseg-mjahr default sy-datum(4).
*                                      NO-EXTENSION NO INTERVALS.
*                                      default WA_EXTWG.
SELECTION-SCREEN END OF BLOCK blk .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON BLOCK blk .
  CHECK sy-ucomm = 'ONLI'.
*-- get quality score data from DB.
  PERFORM get_data_from_db.

  IF it_zsqm_q_grgi[] IS INITIAL.
    MESSAGE e000(zmqm) WITH 'No entries!'(e01).
    EXIT.
  ENDIF.

*-- Get text of ext. material group from table TWEWT
*  PERFORM get_ext_mat_group_text  USING s_extwg-low
*                                        wa_ewbez.


START-OF-SELECTION.
  CHECK NOT it_zsqm_q_grgi[]  IS INITIAL OR
        NOT it_zsqm_q_s_def[]   IS INITIAL OR
        NOT it_ztqm_q_score[] IS INITIAL.
*-- Collect data by vendor and matnerial.
  PERFORM collect_data_by.

*-- Collect data by vendor for Basic List display
  PERFORM collect_dat_by_vendor_basic.

*-- Calculate Quality score
  PERFORM calculate_qual_score  TABLES it_zsqm_q_vend_b.

**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT it_zsqm_q_grgi[] IS INITIAL.
*-- Fill Text.
  PERFORM fill_text.

  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.

**-- Get Selection data for Selection from Memory ID using IMPORT
* IMPORT s_extwg-low  FROM MEMORY ID 'BY_VENDOR_EXT'.
*
* MOVE :  s_extwg-low   TO wa_extwg.

  IMPORT wa_int_date  FROM MEMORY ID 'BY_VENDOR'.

  IF sy-subrc = 0.  "/Import data exist.
    FREE MEMORY ID 'BY_VENDOR'. FREE MEMORY ID 'BY_VENDOR_EXT'.

    wa_first_date = wa_int_date.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = wa_first_date
         IMPORTING
              last_day_of_month = wa_last_date
         EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    wa_first_date = sy-datum.
    wa_first_date+6(2) = '01'.
    wa_last_date = sy-datum.
  ENDIF.

INITIALIZATION.


***//Macro Definitions
*-- macro : macro_name &1 &2
*--           &1 -
*--           &2 -
*  DEFINE macro_name.
*  END-OF-DEFINITION.


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle events Double Click,
* Toolbar, User command
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row e_column,

    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

  PRIVATE SECTION.

ENDCLASS.

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.

*-- / Double Click
  METHOD handle_double_click.

* The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.
* : E_ROW-INDEX.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD handle_toolbar.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: ls_toolbar  TYPE stb_button.

    CASE wa_level.
      WHEN c_vendor.
*         append a separator('3') to normal toolbar
        CLEAR ls_toolbar.
        MOVE 3 TO ls_toolbar-butn_type.
        APPEND ls_toolbar TO e_object->mt_toolbar.
*         append an icon to show detail List of selected item.
        CLEAR ls_toolbar.
        MOVE 0 TO ls_toolbar-butn_type. "/ Button Type
        MOVE 'DETAIL'           TO ls_toolbar-function.
        MOVE icon_detail        TO ls_toolbar-icon.
        MOVE 'Show detail'(t12) TO ls_toolbar-quickinfo.
        MOVE 'Detail'(t13)      TO ls_toolbar-text.
        MOVE ' '                TO ls_toolbar-disabled.

        APPEND ls_toolbar TO e_object->mt_toolbar.
      WHEN c_material.
*         append a separator('3') to normal toolbar
        CLEAR ls_toolbar.
        MOVE 3 TO ls_toolbar-butn_type.
        APPEND ls_toolbar TO e_object->mt_toolbar.
*         append an icon to show detail List of selected item.
        CLEAR ls_toolbar.
        MOVE 0 TO ls_toolbar-butn_type. "/ Button Type
        MOVE 'DELIVERY'           TO ls_toolbar-function.
        MOVE icon_transport        TO ls_toolbar-icon.
        MOVE 'Show delivery'(t16) TO ls_toolbar-quickinfo.
        MOVE 'Delivery'(t15)      TO ls_toolbar-text.
        MOVE ' '                TO ls_toolbar-disabled.

        APPEND ls_toolbar TO e_object->mt_toolbar.

    ENDCASE.
  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD handle_user_command.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : lt_rows   TYPE lvc_t_row,
           lw_line_row LIKE LINE OF lt_rows.
    DATA : lw_lines TYPE i.

    CASE e_ucomm.
      WHEN 'DETAIL'.

        CALL METHOD alv_grid->get_selected_rows
                 IMPORTING et_index_rows = lt_rows.

        CALL METHOD cl_gui_cfw=>flush.

        IF sy-subrc NE 0.
          wa_repid = sy-repid.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    titel = wa_repid
                    txt2  = sy-subrc
                    txt1  = text-e01.
        ELSE.
          DESCRIBE TABLE lt_rows LINES lw_lines.
          CHECK lw_lines = 1.     "/Check single line Selected

          READ TABLE lt_rows  INDEX 1 INTO lw_line_row.

          CHECK NOT lw_line_row-index IS INITIAL.

          PERFORM retriev_detail_data USING lw_line_row-index.

          CHECK  wa_level = c_material AND
                NOT it_zsqm_q_vend_d IS INITIAL.

          wa_renewal_flg = c_mark.

        ENDIF.

      WHEN 'DELIVERY'.
        CALL METHOD alv_grid->get_selected_rows
                   IMPORTING et_index_rows = lt_rows.

        CALL METHOD cl_gui_cfw=>flush.

        IF sy-subrc NE 0.
          wa_repid = sy-repid.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    titel = wa_repid
                    txt2  = sy-subrc
                    txt1  = text-e01.
        ELSE.
          DESCRIBE TABLE lt_rows LINES lw_lines.
          CHECK lw_lines = 1.     "/Check single line Selected

          READ TABLE lt_rows  INDEX 1 INTO lw_line_row.

          CHECK NOT lw_line_row-index IS INITIAL.

          PERFORM retriev_delivery_data USING lw_line_row-index.
          wa_level = c_delivery.
          CHECK  wa_level = c_delivery AND
                NOT it_zsqm_q_grgi_all IS INITIAL.
*          CHECK  WA_LEVEL = C_MATERIAL AND
*                NOT IT_ZSQM_Q_GRGI_ALL IS INITIAL.


          wa_renewal_flg = c_mark.

        ENDIF.


    ENDCASE.

  ENDMETHOD.                           "handle_user_command

ENDCLASS.
*
* lcl_event_receiver (Implementation)
*===================================================================

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&-----------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&-----------------------------------------------------------------*
FORM get_data_from_db.
  DATA : lt_zsqm_q_grgi  LIKE it_zsqm_q_grgi OCCURS 0
                                WITH HEADER LINE.

  DATA:  BEGIN OF lt_zsqm_q_grgi_1 OCCURS 0.
          INCLUDE STRUCTURE it_zsqm_q_grgi.
  DATA:     znum TYPE i VALUE 1.
  DATA: END OF lt_zsqm_q_grgi_1.


  REFRESH : it_zsqm_q_s_vend, it_zsqm_q_grgi, it_ztqm_q_score.
  DATA: zmenge LIKE eket-menge,
        zwemng LIKE eket-wemng,
        zeindt LIKE eket-eindt,
        zmark(1),
        zmark1(1).

  DATA: BEGIN OF wa_it OCCURS 0,
        lifnum LIKE qmel-lifnum,
        matnr LIKE mara-matnr,
        wa_num TYPE i,
        END OF wa_it.




  DATA: BEGIN OF wa_matnr OCCURS 0,
        matnr LIKE mara-matnr,
        werks LIKE marc-werks,
        maktx LIKE makt-maktx,
       END OF wa_matnr.


  DATA: wa_it1 LIKE wa_it OCCURS 0 WITH HEADER LINE.

*-- Get GR/GI data from DB
*-- There is no material supplied by multi vendor. "/Notice
*// Modified by sllee : Change SQL Logic by Mr Kim 02/13/2004-Start
*-   Using Purchasing source List(table : 'EORD')

*  SELECT   B~LIFNR G~NAME1 B~BWART B~MEINS   B~MATNR F~MAKTX
*          SUM( B~MENGE ) AS MENGE_GR
*       INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_Q_GRGI
*         FROM ( ( ( ( MKPF AS A        INNER JOIN MSEG AS B
*            ON   A~MBLNR = B~MBLNR
*             AND A~MJAHR = B~MJAHR ) INNER JOIN MARA AS D
*            ON   B~MATNR = D~MATNR ) INNER JOIN MARC AS E
*            ON   D~MATNR = E~MATNR
*             AND B~WERKS = E~WERKS ) INNER JOIN MAKT AS F
*            ON B~MATNR = F~MATNR   ) LEFT OUTER JOIN LFA1 AS G
*            ON B~LIFNR = G~LIFNR
*           WHERE A~BUDAT IN S_PERIOD
*             AND D~EXTWG IN S_EXTWG
*             AND E~QMATV = C_MARK
*             AND B~BWART IN ('101', '102',            "/GR
*                              '261', '901', '903',    "/GI
*                              '262', '902', '904')
*             AND F~SPRAS = SY-LANGU
*            GROUP BY B~LIFNR G~NAME1 B~BWART B~MEINS B~MATNR F~MAKTX.
** changed by Furong on 11/27/2006
*  SELECT   G~LIFNR H~NAME1 B~BWART B~MEINS   B~MATNR F~MAKTX
*         B~mblnr B~EBELN B~EBELP B~ZBUDAT
*          SUM( B~MENGE ) AS MENGE_GR
*       INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_Q_GRGI
*         FROM ( ( ( ( ( MKPF AS A        INNER JOIN MSEG AS B
*            ON   A~MBLNR = B~MBLNR
*             AND A~MJAHR = B~MJAHR ) INNER JOIN MARA AS D
*            ON   B~MATNR = D~MATNR ) INNER JOIN MARC AS E
*            ON   D~MATNR = E~MATNR
**             AND B~WERKS = E~WERKS
*                                   ) INNER JOIN MAKT AS F
*            ON   B~MATNR = F~MATNR ) INNER JOIN EORD AS G
*            ON   E~MATNR = G~MATNR
*             AND E~WERKS = G~WERKS ) INNER JOIN LFA1 AS H
*            ON   G~LIFNR = H~LIFNR
*           WHERE
*           B~ZBUDAT IN S_PERIOD  and "/A~BUDAT IN S_PERIOD
*              D~EXTWG IN S_EXTWG
*             AND E~QMATV = C_MARK
*             AND B~BWART IN ('101', '102',            "/GR
*                              '261', '901', '903',    "/GI
*                              '262', '902', '904')
*             AND F~SPRAS = SY-LANGU
*             and E~WERKS = p_werks
**             AND G~ZEORD = '00001'
*            GROUP BY G~LIFNR H~NAME1 B~BWART B~MEINS B~MATNR F~MAKTX
*            B~mblnr B~EBELN B~EBELP B~ZBUDAT.
*
*
*  SELECT   G~LIFNR H~NAME1 B~BWART B~MEINS   B~MATNR F~MAKTX
*         B~mblnr B~EBELN B~EBELP B~ZBUDAT
*          SUM( B~MENGE ) AS MENGE_GR
*       INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_Q_GRGI
*         FROM ( ( (  MSEG AS B
*            INNER JOIN MARA AS D
*            ON   B~MATNR = D~MATNR ) INNER JOIN MARC AS E
*            ON   D~MATNR = E~MATNR
**             AND B~WERKS = E~WERKS
*                                   ) INNER JOIN MAKT AS F
*            ON   B~MATNR = F~MATNR )
*            INNER JOIN EORD AS G
*            ON   E~MATNR = G~MATNR
*             AND E~WERKS = G~WERKS )
*            INNER JOIN LFA1 AS H
*            ON   b~LIFNR = H~LIFNR
*           WHERE e~WERKS = p_werks
*           AND B~BWART IN ('101', '102',            "/GR
*                              '261', '901', '903',    "/GI
*                              '262', '902', '904')
*            and B~ZBUDAT IN S_PERIOD  and "/A~BUDAT IN S_PERIOD
*              D~EXTWG IN S_EXTWG
*             AND E~QMATV = C_MARK
*             AND F~SPRAS = SY-LANGU
**             AND G~ZEORD = '00001'
*            GROUP BY G~LIFNR H~NAME1 B~BWART B~MEINS B~MATNR F~MAKTX
*            B~mblnr B~EBELN B~EBELP B~ZBUDAT.
*
****
* Begin of changes - UD1K923253
* Remove MARA, EORD from select statement.

*  SELECT DISTINCT d~matnr e~werks f~maktx
*   INTO TABLE wa_matnr
*   FROM mara AS d INNER JOIN marc AS e
*   ON d~matnr = e~matnr
*   INNER JOIN makt AS f
*   ON  d~matnr = f~matnr
*   WHERE e~werks = p_werks
*      AND e~qmatv = c_mark
*      AND d~extwg IN s_extwg
*      AND f~spras = sy-langu.
*
*  SORT wa_matnr BY matnr.
*  IF wa_matnr[] IS INITIAL.
*  ELSE.
*    SELECT   G~lifnr h~name1 b~bwart b~meins   b~matnr
*             b~mblnr b~ebeln b~ebelp b~zbudat
*               b~menge  AS menge_gr
*           INTO CORRESPONDING FIELDS OF TABLE wa_zsqm_q_grgi
*            FROM mseg AS b
*           INNER JOIN EORD AS G
*            ON   B~MATNR = G~MATNR
*             AND B~WERKS = G~WERKS
*            INNER JOIN lfa1 AS h
*             ON  G~lifnr = h~lifnr
*            FOR ALL ENTRIES IN wa_matnr
*            WHERE b~werks = wa_matnr-werks
*              AND b~bwart IN ('101', '102',            "/GR
*                                  '261', '901', '903',    "/GI
*                                  '262', '902', '904')
*               AND b~zbudat IN s_period  "/A~BUDAT IN S_PERIOD
*               and b~matnr = wa_matnr-matnr.

  SELECT   g~lifnr h~name1 b~bwart b~meins   b~matnr
           b~ebeln b~ebelp b~zbudat f~maktx
           sum( b~menge )  as menge_gr
         INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_Q_GRGI
          FROM mseg AS b
         INNER JOIN EORD AS G
          ON   B~MATNR = G~MATNR
           AND B~WERKS = G~WERKS
            INNER JOIN lfa1 AS h
           ON  g~lifnr = h~lifnr
*           inner join marc as m
*             ON  B~MATNR = m~MATNR
*             AND B~WERKS = m~WERKS
        INNER JOIN MAKT AS F
            on b~matnr = f~matnr
          WHERE  b~mjahr eq p_mjahr
            and  b~werks = p_werks
            AND  b~bwart IN ('101', '102',           "/GR
                            '261', '901', '903',    "/GI
                            '262', '902', '904')
             AND b~zbudat IN s_period  "/A~BUDAT IN S_PERIOD
*              aND m~QMATV = C_MARK
*              and m~extwg IN s_extwg
             AND F~SPRAS = SY-LANGU
             and g~lifnr in s_lifnr
             and g~EBELN eq space
         GROUP BY g~LIFNR H~NAME1 B~BWART B~MEINS b~MATNR
         B~EBELN B~EBELP B~ZBUDAT f~maktx.

*    LOOP AT wa_zsqm_q_grgi.
*      it_zsqm_q_grgi = wa_zsqm_q_grgi.
*      read table wa_matnr with key matnr = wa_zsqm_q_grgi-matnr.
*      it_zsqm_q_grgi-MAKTX = wa_matnr-MAKTX.
*      collect it_zsqm_q_grgi.
*      clear: it_zsqm_q_grgi, wa_zsqm_q_grgi,wa_matnr.
*    ENDLOOP.
*  ENDIF.
* End of changes - UD1K923253

********add code for delivery information***

*if not IT_ZSQM_Q_GRGI[] is initial.
*
*
*select   * into corresponding fields of table
*IT_ZSQM_Q_GRGI_all from eket
*for all entries in IT_ZSQM_Q_GRGI
*where ebeln = IT_ZSQM_Q_GRGI-ebeln and
*ebelp = IT_ZSQM_Q_GRGI-ebelp.
*
*
*endif.
*
*loop at IT_ZSQM_Q_GRGI_all.
*
*
*read table IT_ZSQM_Q_GRGI with key ebeln = IT_ZSQM_Q_GRGI_all-ebeln
* ebelp = IT_ZSQM_Q_GRGI_all-ebelp.
*
*move  IT_ZSQM_Q_GRGI-zbudat to IT_ZSQM_Q_GRGI_all-zbudat.
*move  IT_ZSQM_Q_GRGI-matnr to IT_ZSQM_Q_GRGI_all-matnr.
*
*modify IT_ZSQM_Q_GRGI_all.
*
*clear IT_ZSQM_Q_GRGI.
*
*endloop.
*
*sort it_zsqm_q_grgi by matnr.
*loop at IT_ZSQM_Q_GRGI.
*IT_ZSQM_Q_GRGI-delivered = 100.
*
*select single  menge wemng eindt into (zmenge,zwemng,zeindt) from eket
*where ebeln = IT_ZSQM_Q_GRGI-ebeln and
*ebelp = IT_ZSQM_Q_GRGI-ebelp.
*
*if zwemng >= zmenge.
*zmark = 'X'.
*endif.
*if zeindt <= IT_ZSQM_Q_GRGI-zbudat.
*zmark1 = 'X'.
*endif.
*If zmark = 'X' and zmark1 = 'X'.
*elseIf zmark = 'X' and zmark1 <> 'X'.
*subtract 5 from IT_ZSQM_Q_GRGI-delivered .
*elseIf zmark <> 'X' and zmark1 = 'X'.
*subtract 5 from IT_ZSQM_Q_GRGI-delivered .
*endif.
*
*clear: IT_ZSQM_Q_GRGI-ebeln, IT_ZSQM_Q_GRGI-ebelp,
*IT_ZSQM_Q_GRGI-zbudat.
*********end addition*******************
*modify IT_ZSQM_Q_GRGI.
*clear: it_zsqm_q_grgi.
*clear: zmenge, zwemng, zeindt, zmark, zmark1.
*endloop.


*// Modified by sllee : Change SQL Logic by Mr Kim 02/13/2004-End
  CHECK     sy-subrc = 0 AND
        NOT it_zsqm_q_grgi[] IS INITIAL.
*  LT_ZSQM_Q_GRGI[] = IT_ZSQM_Q_GRGI[].
*
*  REFRESH IT_ZSQM_Q_GRGI.
  lt_zsqm_q_grgi[] = it_zsqm_q_grgi[].
  REFRESH it_zsqm_q_grgi.
  LOOP AT lt_zsqm_q_grgi.
    MOVE-CORRESPONDING lt_zsqm_q_grgi TO lt_zsqm_q_grgi_1.
    lt_zsqm_q_grgi_1-znum = 1.
*   '101', '102',            "/GR
*   '261', '901', '903',    "/GI
*   '262', '902', '904'
    CASE lt_zsqm_q_grgi-bwart.
      WHEN '101'.
*       N/A
      WHEN '102'.
        it_zsqm_q_grgi-menge_gr = it_zsqm_q_grgi-menge_gr * -1.
      WHEN '261' OR '901' OR '903'.
*        IT_ZSQM_Q_GRGI-MENGE_GI = IT_ZSQM_Q_GRGI-MENGE_GR.
*        CLEAR IT_ZSQM_Q_GRGI-MENGE_GR.
        lt_zsqm_q_grgi_1-menge_gi = lt_zsqm_q_grgi_1-menge_gr.
        CLEAR lt_zsqm_q_grgi_1-menge_gr.
      WHEN '262' OR '902' OR '904'.
*        IT_ZSQM_Q_GRGI-MENGE_GI = IT_ZSQM_Q_GRGI-MENGE_GR.
*        IT_ZSQM_Q_GRGI-MENGE_GI = IT_ZSQM_Q_GRGI-MENGE_GI * -1.
*        CLEAR IT_ZSQM_Q_GRGI-MENGE_GR.

* modification by 100565 GI qty incorrect
*     lT_ZSQM_Q_GRGI_1-MENGE_GI = IT_ZSQM_Q_GRGI-MENGE_GR.
        lt_zsqm_q_grgi_1-menge_gi = lt_zsqm_q_grgi_1-menge_gr.
        lt_zsqm_q_grgi_1-menge_gi = lt_zsqm_q_grgi_1-menge_gi * -1.
        CLEAR lt_zsqm_q_grgi_1-menge_gr.

    ENDCASE.

    CLEAR   it_zsqm_q_grgi-bwart.

    COLLECT lt_zsqm_q_grgi_1.
  ENDLOOP.

*  loop at lt_zsqm_q_grgi_1.
*  lt_zsqm_q_grgi_1-delivered = lt_zsqm_q_grgi_1-delivered /
*lt_zsqm_q_grgi_1-znum.
*modify lt_zsqm_q_grgi_1.
*endloop.

  it_zsqm_q_grgi[] = lt_zsqm_q_grgi_1[].
*-  Get Defect data from QMEL

  SELECT DISTINCT a~lifnum AS lifnr e~name1 b~meins a~matnr d~maktx
       SUM( a~rkmng ) AS rkmng
      SUM(  a~lndwntime ) AS lndwntime                      " rl 07_19
       SUM( a~responsive ) AS responsive                    " rl 07_19.
       a~OBJNR
*     INTO CORRESPONDING FIELDS OF TABLE it_zsqm_q_s_def
      INTO CORRESPONDING FIELDS OF TABLE it_zsqm_q_s_def_temp
       FROM ( ( (  qmel AS a INNER JOIN mara AS b
          ON   a~matnr = b~matnr   ) INNER JOIN marc AS c
          ON   b~matnr = c~matnr   ) INNER JOIN makt AS d
          ON   a~matnr = d~matnr   ) INNER JOIN lfa1 AS e
          ON   a~lifnum = e~lifnr
         WHERE
*           c~qmatv = c_mark AND "removed condition 100565
           a~erdat  IN s_period
           AND a~qmart  = wa_qmart    "/'Q2' - Fixed
           and a~lifnum in s_lifnr
*          AND b~extwg  IN s_extwg
           AND d~spras  = sy-langu
           AND c~werks = p_werks
** added code to unselect deleted Notifications
*and a~objnr not in ( select objnr from jest as f
*                  where     f~objnr eq a~objnr and
*                             f~stat = 'I0076' and Inact = ' '   )

        GROUP by a~lifnum e~name1 b~meins a~matnr d~maktx a~lndwntime
           a~responsive a~OBJNR
       HAVING   lndwntime >= 0                              " rl 07_19
       AND    responsive >= 0.                              " rl 07_19.




* Only Consider  Notifications which are completed.
  data : line like BSVX-STTXT,
         l_index type i..
  loop at it_zsqm_q_s_def_temp.
    l_index = sy-tabix.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        CLIENT                  = SY-MANDT
        OBJNR                   = it_zsqm_q_s_def_temp-objnr
        ONLY_ACTIVE             = 'X'
        SPRAS                   = sy-langu
       BYPASS_BUFFER           = ' '
     IMPORTING
*   ANW_STAT_EXISTING       =
*   E_STSMA                 =
        LINE                    = line
*   USER_LINE               =
*   STONR                   =
     EXCEPTIONS
       OBJECT_NOT_FOUND        = 1
       OTHERS                  = 2 .

*    IF line ne 'NOCO NOTE' or
*       line ne 'NOCO NOTE NOPT' OR
*       line ne 'NOCO NOTI' OR
*       line ne 'NOCO NOTI NOPT'.
*      delete it_zsqm_q_s_def_temp index l_index.
*
*    ENDIF.
   IF  line ne 'NOCO NOTE' and
       line ne 'NOCO NOTE NOPT' and
       line ne 'NOCO NOTI' and
       line ne 'NOCO NOTI NOPT'.
      delete it_zsqm_q_s_def_temp index l_index.

    ENDIF.



  endloop.





  it_zsqm_q_s_def[] = it_zsqm_q_s_def_temp[].

****Number of Notif with same matnr
  if not it_zsqm_q_s_def[] is initial.
    SELECT * FROM qmel INTO CORRESPONDING FIELDS OF TABLE wa_it
    FOR ALL ENTRIES IN it_zsqm_q_s_def
    WHERE matnr = it_zsqm_q_s_def-matnr
    AND lifnum = it_zsqm_q_s_def-lifnr
    AND erdat IN s_period.
  endif.



*loop at wa_it.
*add 1 to wa_it-wa_num.
*modify wa_it.
*endloop.
*
*loop at wa_it.
*collect wa_it into wa_it1.
*endloop.

  LOOP AT wa_it.
    ADD 1 TO wa_it-wa_num.
    COLLECT wa_it INTO wa_it1.
  ENDLOOP.


  LOOP AT it_zsqm_q_s_def.
*** to default responsive
   l_responsive  = l_responsive + it_zsqm_q_s_def-responsive.
   l_cnt = l_cnt + 1.
    IF it_zsqm_q_s_def-lndwntime IS INITIAL.
      it_zsqm_q_s_def-lndwntime = 100.
    ENDIF.
    IF it_zsqm_q_s_def-lndwntime <> 100.
      it_zsqm_q_s_def-lndwntime = 100 - it_zsqm_q_s_def-lndwntime.
    ENDIF.
    IF it_zsqm_q_s_def-responsive IS INITIAL.
*      it_zsqm_q_s_def-responsive = 100.
       it_zsqm_q_s_def-responsive =  0.  " manju
    ENDIF.
*** to default responsive
    READ TABLE wa_it1 WITH KEY  matnr = it_zsqm_q_s_def-matnr
                                lifnum = it_zsqm_q_s_def-lifnr.
*    READ TABLE wa_it1 WITH KEY lifnum = it_zsqm_q_s_def-lifnr.

      at end of LIFNR.
       it_cnt-lifnum  =  it_zsqm_q_s_def-lifnr.
       it_cnt-RESPONSIVE = l_responsive .
       it_cnt-wa_num = l_cnt.
       append it_cnt.
       clear : l_responsive, l_cnt.
      endat.
     it_zsqm_q_s_def-responsive = it_zsqm_q_s_def-responsive /
                                  wa_it1-wa_num.
*     it_zsqm_q_s_def-responsive = l_responsive /  wa_it1-wa_num.
     MODIFY it_zsqm_q_s_def.
  ENDLOOP.


***End select

*-- Get Quality Score data from CBO Table

  SELECT a~lifnr g~name1 a~meinh  a~meins  a~matnr f~maktx
*         SUM( A~LINESTOP ) AS LINESTOP
*         SUM( A~QNT_CAMP ) AS QNT_CAMP
*         SUM( A~QNT_SALV ) AS QNT_SALV
*         SUM( A~QNT_REPR ) AS QNT_REPR
     INTO CORRESPONDING FIELDS OF TABLE it_ztqm_q_score
       FROM ( ( ( ztqm_q_score AS a INNER JOIN mara AS b
          ON  a~matnr = b~matnr ) INNER JOIN marc AS e
          ON  b~matnr = e~matnr ) INNER JOIN makt AS f
          ON  a~matnr = f~matnr ) INNER JOIN lfa1 AS g
          ON  a~lifnr = g~lifnr
         WHERE a~lifnr in s_lifnr and
              a~issuedat IN s_period
*          AND b~extwg    IN s_extwg
           AND f~spras   = sy-langu
          GROUP by a~lifnr  g~name1 a~meinh  a~meins a~matnr f~maktx.


ENDFORM.                    " GET_DATA_FROM_DB
*&------------------------------------------------------------------*
*&      Form  COLLECT_DATA_BY
*&------------------------------------------------------------------*
FORM collect_data_by.
  DATA:  zmatnr LIKE it_zsqm_q_grgi-matnr,
         wa_num(3) TYPE c.

  REFRESH : it_zsqm_q_s_vend.

  LOOP AT it_zsqm_q_grgi.
    CLEAR it_zsqm_q_s_vend.
    MOVE-CORRESPONDING it_zsqm_q_grgi TO it_zsqm_q_s_vend.
    MOVE : 'MIN' TO it_zsqm_q_s_vend-meinh.
    CLEAR it_zsqm_q_s_vend-bwart.
    COLLECT it_zsqm_q_s_vend.
  ENDLOOP.

  LOOP AT it_zsqm_q_s_def.
    CLEAR it_zsqm_q_s_vend.
    CLEAR it_zsqm_q_s_def-bwart.
    MOVE-CORRESPONDING it_zsqm_q_s_def TO it_zsqm_q_s_vend.
    MOVE : 'MIN' TO it_zsqm_q_s_vend-meinh.
    COLLECT it_zsqm_q_s_vend.
  ENDLOOP.

  LOOP AT it_ztqm_q_score.
    CLEAR it_zsqm_q_s_vend.
    MOVE-CORRESPONDING it_ztqm_q_score TO it_zsqm_q_s_vend.
    COLLECT it_zsqm_q_s_vend.
  ENDLOOP.

ENDFORM.                    " COLLECT_DATA_BY
*&------------------------------------------------------------------*
*&      Form  CALCULATE_QUAL_SCORE
*&------------------------------------------------------------------*
FORM calculate_qual_score   TABLES pt_qs_v STRUCTURE it_zsqm_q_s_vend.

  LOOP AT pt_qs_v.

    IF NOT pt_qs_v-menge_gi IS INITIAL.

*-    PPM = ( Defect / GI quantity ) * 1,000,000
      pt_qs_v-q_ppm =
            ( pt_qs_v-rkmng / pt_qs_v-menge_gi )
           * 1000000.
    ENDIF.

*-     Quality Score
*    PT_QS_V-Q_SCORE = PT_QS_V-Q_PPM.
*          +  ( PT_QS_V-LINESTOP + PT_QS_V-QNT_CAMP
*            + PT_QS_V-QNT_SALV + PT_QS_V-QNT_REPR ).


    pt_qs_v-q_score = ( pt_qs_v-responsive + pt_qs_v-lndwntime ).
*PT_QS_V-DELIVERED ) .

    MODIFY pt_qs_v.
  ENDLOOP.

*-- Sort by Quality Score : Descending sort
*-- and fill rank(SRNO) field using loop index

  SORT pt_qs_v BY q_score DESCENDING.

  LOOP AT pt_qs_v.
    MOVE : sy-tabix TO pt_qs_v-srno. "/No by Q.Score Descending
    MODIFY pt_qs_v.
  ENDLOOP.

ENDFORM.                    " CALCULATE_QUAL_SCORE
*&-----------------------------------------------------------------*
*&      Form  FILL_TEXT
*&-----------------------------------------------------------------*
FORM fill_text.
  DATA : lw_score_index LIKE sy-tabix.

  LOOP AT it_zsqm_q_s_vend.
    lw_score_index = sy-tabix.

    SELECT SINGLE name1 INTO it_zsqm_q_s_vend-name1
       FROM lfa1
         WHERE lifnr = it_zsqm_q_s_vend-lifnr.

    MODIFY it_zsqm_q_s_vend INDEX lw_score_index.
  ENDLOOP.

ENDFORM.                    " FILL_TEXT
*&------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE modify_screen_9000 OUTPUT.
* When Vendor detail screen is selected display vendor information
  CASE wa_level.
    WHEN c_vendor.
      LOOP AT SCREEN.
        IF screen-group1 = 'MAT'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Container for ALV GRID
    CLEAR wa_renewal_flg.
*- Create Container('GRID_CONTAINER') with Custom Contro on screen
    CREATE OBJECT grid_container
           EXPORTING container_name = wa_custom_control
           EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      wa_repid = sy-repid.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = wa_repid
                txt2  = sy-subrc
                txt1  = 'The control can not be created'(e02).
    ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
    CREATE OBJECT alv_grid
           EXPORTING i_parent = grid_container
                     i_appl_events = 'X'.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM mask_columns_of_alv_grid TABLES it_fieldcat.

**-- adjust field sort and subtotal to display total of column
    PERFORM set_sort_total_field.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM set_table_for_display.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_double_click  FOR alv_grid.
*-   toolbar control event
    SET HANDLER event_receiver->handle_user_command  FOR alv_grid.
    SET HANDLER event_receiver->handle_toolbar       FOR alv_grid.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD alv_grid->set_toolbar_interactive.



    CALL METHOD cl_gui_control=>set_focus
                        EXPORTING control = alv_grid.

  ENDIF.

  IF NOT grid_container IS INITIAL AND "/Created Container for ALV GRID
     NOT wa_renewal_flg IS INITIAL.
    CLEAR wa_renewal_flg.
*-- PREPARE SETTING ATTRIBUTES AND ETC OF ALV OBJECT
    PERFORM set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM mask_columns_of_alv_grid TABLES it_fieldcat.

**-- adjust field sort and subtotal to display total of column
*    PERFORM SET_SORT_TOTAL_FIELD.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM set_table_for_display.


    PERFORM refresh_alv_grid_data_disp.

    CALL METHOD cl_gui_control=>set_focus
                        EXPORTING control = alv_grid.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.
*//-- Set Layout Structure


  wa_is_layout-language = sy-langu.      "/Language Key
  wa_is_layout-cwidth_opt = c_mark.     "/Optimize column width
*  WA_IS_LAYOUT-DETAILTITL = ''.        "/Title bar of detail screen
*  WA_IS_LAYOUT-GRID_TITLE = ''.        "/ Title bar text
*  WA_IS_LAYOUT-KEYHOT      = C_MARK.    "/ Key columns as hotspot
*  WA_IS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*  WA_IS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
*  WA_IS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
  wa_is_layout-numc_total  = c_mark. "/Allow totals for NUMC
*  WA_IS_LAYOUT-S_DRAGDROP  = LW_S_DRAGDROP. "/Drag & Drop control

  wa_is_layout-sel_mode  = 'A'. "/mode for select col and row
*  WA_IS_LAYOUT-SGL_CLK_HD = C_MARK. "/sorts the list whe column clicked

*//-- Set Variant Structure
  wa_variant-report = sy-repid.
  wa_variant-username = sy-uname.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM mask_columns_of_alv_grid TABLES   pt_fieldcat TYPE lvc_t_fcat.

  REFRESH pt_fieldcat. CLEAR pt_fieldcat.

  IF wa_level <> c_delivery.

* Build the fieldcat according to DDIC structure :
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
         EXPORTING
              i_structure_name = 'ZSQM_Q_S_VEND'
         CHANGING
              ct_fieldcat      = pt_fieldcat[].
  ELSE.

* Build the fieldcat according to DDIC structure :
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
         EXPORTING
              i_structure_name = 'ZSQM_DELIVERY'
         CHANGING
              ct_fieldcat      = pt_fieldcat[].

  ENDIF.

* Set field attribute
  LOOP AT pt_fieldcat.
    CASE wa_level.
      WHEN c_vendor.

        IF pt_fieldcat-fieldname = 'SRNO'.
          pt_fieldcat-key_sel = c_mark.
          pt_fieldcat-key     = c_mark.
          pt_fieldcat-coltext = 'No'(t60).
        ELSEIF pt_fieldcat-fieldname = 'NAME1'.
          pt_fieldcat-key_sel = c_mark.
          pt_fieldcat-key     = c_mark.
          pt_fieldcat-coltext = 'Vendor Name'(t50).
        ELSEIF  pt_fieldcat-fieldname = 'LIFNR'.
          pt_fieldcat-key_sel = c_mark.
          pt_fieldcat-key     = c_mark.
        ELSEIF pt_fieldcat-fieldname = 'MATNR' OR
               pt_fieldcat-fieldname = 'MAKTX' OR
               pt_fieldcat-fieldname = 'BWART'.
          pt_fieldcat-no_out = c_mark.
        ENDIF.

      WHEN c_material.

        IF pt_fieldcat-fieldname = 'SRNO'.
          pt_fieldcat-key_sel = c_mark.
          pt_fieldcat-key     = c_mark.
          pt_fieldcat-coltext = 'No'(t60).
        ELSEIF pt_fieldcat-fieldname = 'NAME1' OR
               pt_fieldcat-fieldname = 'LIFNR' OR
               pt_fieldcat-fieldname = 'BWART'.
          pt_fieldcat-no_out = c_mark.
        ELSEIF pt_fieldcat-fieldname = 'MATNR' OR
               pt_fieldcat-fieldname = 'MAKTX'.
          pt_fieldcat-key_sel = c_mark.
          pt_fieldcat-key     = c_mark.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF pt_fieldcat-fieldname = 'MENGE_GR'.
      pt_fieldcat-coltext = 'GR quantity'(t51).
    ELSEIF pt_fieldcat-fieldname = 'MENGE_GI'.
      pt_fieldcat-coltext = 'GI quantity'(t52).
    ELSEIF pt_fieldcat-fieldname = 'RKMNG'.
      pt_fieldcat-coltext = 'Defect'(t53).
    ELSEIF pt_fieldcat-fieldname = 'Q_PPM'.
      pt_fieldcat-emphasize = 'C711'.
    ELSEIF pt_fieldcat-fieldname = 'LINESTOP'.
      pt_fieldcat-coltext = 'Line stop'(t54).
    ELSEIF pt_fieldcat-fieldname = 'QNT_CAMP'.
      pt_fieldcat-coltext = 'Campaign'(t55).
    ELSEIF pt_fieldcat-fieldname = 'QNT_SALV'.
      pt_fieldcat-coltext = 'Salvage'(t56).
    ELSEIF pt_fieldcat-fieldname = 'QNT_REPR'.
      pt_fieldcat-coltext = 'Repair'(t57).
    ELSEIF pt_fieldcat-fieldname = 'Q_SCORE'.
      pt_fieldcat-emphasize = 'C310'.
    ENDIF.

    IF     ( pt_fieldcat-datatype = 'QUAN'    OR
             pt_fieldcat-datatype = 'INT4'      ) AND
       NOT ( pt_fieldcat-fieldname = 'Q_PPM'   OR
             pt_fieldcat-fieldname = 'Q_SCORE' OR
             pt_fieldcat-fieldname = 'SRNO'     ).

      pt_fieldcat-do_sum = c_mark.
    ENDIF.

    MODIFY pt_fieldcat.
  ENDLOOP.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------*
MODULE set_cursor_field OUTPUT.
  SET CURSOR FIELD wa_fldtxt LINE wa_cur_line.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&----------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&----------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  CLEAR: wa_fldtxt, wa_cur_line.
  GET CURSOR FIELD wa_fldtxt LINE wa_cur_line.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
MODULE exit_9000 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT'.
      IF wa_level = c_vendor.
        PERFORM free_alv_grid.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM set_list_level_control.

    WHEN 'RW'.
      IF wa_level = c_vendor.
        PERFORM free_alv_grid.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM set_list_level_control.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  FREE_ALV_GRID
*&------------------------------------------------------------------*
FORM free_alv_grid.
* On return Back to selection screen the ALV grid should be cleared
  CHECK NOT alv_grid IS INITIAL.
  CALL METHOD alv_grid->free.
  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    wa_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = wa_repid
              txt2  = sy-subrc
              txt1  = text-e01.
  ENDIF.

ENDFORM.                    " FREE_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
* On return Back to selection screen the ALV grid should be cleared
    WHEN 'BACK'.
      IF wa_level = c_vendor.
        PERFORM free_alv_grid.
        LEAVE TO SCREEN 0.

*      elseif  WA_LEVEL = C_delivery.
*        PERFORM FREE_ALV_GRID.
*        LEAVE SCREEN .

      ENDIF.
* Detail Level screen should come back to Vendor screen
      PERFORM set_list_level_control.

    WHEN 'REFRESH'.  "/02/16/2004 - SLLEE REQUESTED BY Mr. Moon

      REFRESH it_zsqm_q_grgi.
*-- get quality score data from DB.
      PERFORM get_data_from_db.

      IF NOT it_zsqm_q_grgi[]  IS INITIAL OR
         NOT it_zsqm_q_s_def[]   IS INITIAL OR
         NOT it_ztqm_q_score[] IS INITIAL.

*-- Collect data by vendor and matnerial.
        PERFORM collect_data_by.

*-- Collect data by vendor for Basic List display
        PERFORM collect_dat_by_vendor_basic.

*-- Calculate Quality score
        PERFORM calculate_qual_score  TABLES it_zsqm_q_vend_b.

        CHECK NOT it_zsqm_q_grgi[] IS INITIAL.
*-- Fill Text.
        PERFORM fill_text.

      ENDIF.

      CASE wa_level.
        WHEN c_vendor.

        WHEN c_material.
          REFRESH it_zsqm_q_vend_d.
*-- Collect data .
          PERFORM collect_data_by_detail USING wa_sel_vendor-lifnr.
*-- Calculate Quality score
          PERFORM calculate_qual_score  TABLES it_zsqm_q_vend_d.
        WHEN c_delivery.
      ENDCASE.

      wa_renewal_flg = c_mark.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Form  GET_EXT_MAT_GROUP_TEXT
*&-------------------------------------------------------------------*
FORM get_ext_mat_group_text USING    p_extwg
                                     p_ewbez.
* Get Ext Material Group text
  CHECK NOT p_extwg IS INITIAL.

  SELECT SINGLE ewbez INTO p_ewbez
    FROM twewt
      WHERE extwg = p_extwg
        AND spras = sy-langu.

ENDFORM.                    " GET_EXT_MAT_GROUP_TEXT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SORT  text
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_sort_total_field.
*----- 01/21/2004 Append by BSBAE
  CLEAR: it_sort.
  MOVE: 1 TO it_sort-spos,
        'Q_SCORE'  TO it_sort-fieldname,
        'X'        TO it_sort-down.
  APPEND it_sort.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
FORM retriev_detail_data USING    p_index.

* Retrieve Detail level data
  DATA : lw_sel_index   LIKE sy-tabix.
  DATA : lw_table_lines LIKE sy-tabix.

**- Quality Score by Vendor for Material Document data :Collective Data
*DATA : IT_ZSQM_Q_S_VEND LIKE ZSQM_Q_S_VEND OCCURS 0 WITH HEADER LINE.
*
**- Quality Score  ALV display : Basic and detail list internal tables
*DATA : IT_ZSQM_Q_VEND_B LIKE ZSQM_Q_S_VEND OCCURS 0 WITH HEADER LINE.
*DATA : IT_ZSQM_Q_VEND_D LIKE ZSQM_Q_S_VEND OCCURS 0 WITH HEADER LINE.

  lw_sel_index = p_index.

  REFRESH it_zsqm_q_vend_d.

  CASE wa_level.
    WHEN c_vendor.
      CLEAR it_zsqm_q_vend_b.
      READ TABLE it_zsqm_q_vend_b INDEX lw_sel_index.
      CHECK sy-subrc  = 0.

      MOVE-CORRESPONDING it_zsqm_q_vend_b TO wa_sel_vendor.

      wa_level = c_material.

*-- Collect data .
      PERFORM collect_data_by_detail USING wa_sel_vendor-lifnr.

*-- Calculate Quality score
      PERFORM calculate_qual_score  TABLES it_zsqm_q_vend_d.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
*&      Form  COLLECT_DAT_BY_VENDOR_BASIC
*&------------------------------------------------------------------*
FORM collect_dat_by_vendor_basic.

  DATA: BEGIN OF it_vend_tmp OCCURS 0.
          INCLUDE STRUCTURE zsqm_q_s_vend.
  DATA: wa_num TYPE i.
  DATA: END OF it_vend_tmp.

  REFRESH: it_zsqm_q_vend_b, it_vend_tmp.

*  LOOP AT IT_ZSQM_Q_S_VEND.
*    CLEAR IT_ZSQM_Q_VEND_B.
*         MOVE-CORRESPONDING IT_ZSQM_Q_S_VEND TO IT_ZSQM_Q_VEND_B.
*    CLEAR : IT_ZSQM_Q_VEND_B-MATNR,
*            IT_ZSQM_Q_VEND_B-MAKTX,
*            IT_ZSQM_Q_VEND_B-BWART.
** Avg for Vendor level
*add 1 to IT_ZSQM_Q_VEND_B-BWART.
** end add
*    COLLECT IT_ZSQM_Q_VEND_B.
*  ENDLOOP.
  LOOP AT it_zsqm_q_s_vend.
    CLEAR it_vend_tmp.
    MOVE-CORRESPONDING it_zsqm_q_s_vend TO it_vend_tmp.
    CLEAR : it_vend_tmp-matnr,
            it_vend_tmp-maktx,
            it_vend_tmp-bwart.

* Avg for Vendor level
*    if it_vend_tmp-RKMNG > 0 and
*       it_vend_tmp-responsive > 0.

    ADD 1 TO it_vend_tmp-wa_num.

*    endif.
* end add
* default responsive and linedwntime
    IF it_vend_tmp-responsive IS INITIAL.
      it_vend_tmp-responsive = 100.  " manju
*      it_vend_tmp-responsive = 0.
    ENDIF.
    IF it_vend_tmp-lndwntime IS INITIAL.
      it_vend_tmp-lndwntime = 100.
    ENDIF.
*end default

    COLLECT it_vend_tmp.
  ENDLOOP.
  LOOP AT it_vend_tmp.
   it_vend_tmp-responsive = it_vend_tmp-responsive / it_vend_tmp-wa_num.
    it_vend_tmp-lndwntime = it_vend_tmp-lndwntime / it_vend_tmp-wa_num.
    MODIFY it_vend_tmp.
    at end of lifnr.
      read table it_cnt with key lifnum = it_vend_tmp-lifnr.
       if sy-subrc eq 0.
          it_vend_tmp-responsive = it_cnt-responsive / it_cnt-wa_num.
          modify it_vend_tmp transporting responsive
                                   where lifnr = it_vend_tmp-lifnr.
       endif.
    endat.
  ENDLOOP.

  it_zsqm_q_vend_b[] = it_vend_tmp[].

ENDFORM.                    " COLLECT_DAT_BY_VENDOR_BASIC
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_FOR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_for_display.

  CASE wa_level.
    WHEN c_vendor.
      CALL METHOD alv_grid->set_table_for_first_display
           EXPORTING i_structure_name = 'ZSQM_Q_S_VEND'
                     is_layout        = wa_is_layout
                     i_save           = wa_save
                     is_variant       = wa_variant
                     i_default        = space "C_MARK
           CHANGING  it_fieldcatalog  = it_fieldcat[]
                     it_sort          = it_sort[]
                     it_outtab        = it_zsqm_q_vend_b[].
* The difference is in the int table IT_ZSQM_Q_VEND_B, IT_ZSQM_Q_VEND_D
    WHEN c_material.
      CALL METHOD alv_grid->set_table_for_first_display
           EXPORTING i_structure_name = 'ZSQM_Q_S_VEND'
                     is_layout        = wa_is_layout
                     i_save           = wa_save
                     is_variant       = wa_variant
                     i_default        = space " C_MARK
           CHANGING  it_fieldcatalog  = it_fieldcat[]
                     it_sort          = it_sort[]
                     it_outtab        = it_zsqm_q_vend_d[].

* Add third int table for delivery details

    WHEN c_delivery.
      CALL METHOD alv_grid->set_table_for_first_display
           EXPORTING i_structure_name = 'ZSQM_DELIVERY'
                     is_layout        = wa_is_layout
                     i_save           = wa_save
                     is_variant       = wa_variant
                     i_default        = space " C_MARK
           CHANGING  it_fieldcatalog  = it_fieldcat[]
                     it_sort          = it_sort[]
                     it_outtab        = it_grgi_all[].

  ENDCASE.
ENDFORM.                    " SET_TABLE_FOR_DISPLAY
*&------------------------------------------------------------------*
*&      Form  COLLECT_DATA_BY_DETAIL
*&------------------------------------------------------------------*
FORM collect_data_by_detail USING    p_lifnr.
* select detail information by vendor number
  REFRESH it_zsqm_q_vend_d.

  LOOP AT it_zsqm_q_s_vend WHERE lifnr = p_lifnr.
    CLEAR it_zsqm_q_vend_d.
    MOVE-CORRESPONDING it_zsqm_q_s_vend TO it_zsqm_q_vend_d.
    CLEAR : it_zsqm_q_vend_d-lifnr,
            it_zsqm_q_vend_d-name1,
            it_zsqm_q_vend_d-bwart.

    COLLECT it_zsqm_q_vend_d.
  ENDLOOP.
**default responsive and lndwntime
  LOOP AT it_zsqm_q_vend_d.
    IF it_zsqm_q_vend_d-responsive IS INITIAL.
      it_zsqm_q_vend_d-responsive = 100.
    ENDIF.
    IF it_zsqm_q_vend_d-lndwntime IS INITIAL.
      it_zsqm_q_vend_d-lndwntime = 100.
    ENDIF.
    MODIFY it_zsqm_q_vend_d.
  ENDLOOP.
ENDFORM.                    " COLLECT_DATA_BY_DETAIL
*&------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&------------------------------------------------------------------*
FORM set_list_level_control.
  CASE wa_level.
    WHEN c_vendor.
    WHEN c_material.
      wa_level = c_vendor.
      wa_renewal_flg = c_mark.
    WHEN c_delivery.
      wa_level = c_material.
      wa_renewal_flg = c_mark.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
FORM refresh_alv_grid_data_disp.
  CALL METHOD alv_grid->refresh_table_display
*         EXPORTING
*           IS_STABLE      =
*           I_SOFT_REFRESH =
*         EXCEPTIONS
*           FINISHED       = 1
*           others         = 2
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " REFRESH_ALV_GRID_DATA_DISP
*&---------------------------------------------------------------------*
*&      Form  RETRIEV_DELIVERY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_LINE_ROW_INDEX  text
*----------------------------------------------------------------------*
FORM retriev_delivery_data USING    p_lw_line_row_index.

  DATA: z_it LIKE it_zsqm_q_grgi_all OCCURS 0 WITH HEADER LINE.

*loop at IT_ZSQM_Q_grgi_all .
*collect IT_ZSQM_Q_grgi_all  into z_it.
*clear IT_ZSQM_Q_grgi_all .
*endloop.



  READ TABLE it_zsqm_q_vend_d  INDEX   p_lw_line_row_index.

  IF sy-subrc = 0.
    REFRESH it_grgi_all.
    LOOP AT it_zsqm_q_grgi_all WHERE matnr = it_zsqm_q_vend_d-matnr .
*read table IT_ZSQM_Q_grgi_all  with key matnr = IT_ZSQM_Q_VEND_D-matnr.



      MOVE-CORRESPONDING it_zsqm_q_grgi_all TO it_grgi_all.
      APPEND it_grgi_all.
      CLEAR: it_grgi_all.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " RETRIEV_DELIVERY_DATA
