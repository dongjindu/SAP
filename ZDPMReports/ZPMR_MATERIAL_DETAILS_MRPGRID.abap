REPORT zpmr_material_details NO STANDARD PAGE HEADING MESSAGE-ID zim
                     LINE-SIZE 150.
*-------------------------------------------------------------------*
* Date        Developer        Request         Description
* 05/07/2007  Manju            UD1K940486      Change Werks parameter
*                                              table from MARD to MARC
*--------------------------------------------------------------------*
*&&& Data Declaration.  &&&*
TYPE-POOLS vrm.     "//Value Request Manager: Types & Constants
INCLUDE <icon>.
*-------------------------------------------------*
* TABLE Declare                                   *
*-------------------------------------------------*

TABLES: mara,
        mard,
        makt,
        mbew,
        stxl,
        s031,
        m_mat1t,
        rmmg1,
        lfa1,
        ekpo.
*-------------------------------------------------*
* G/R DATA READ INTERNAL TABLE Declare            *
*-------------------------------------------------*

DATA: BEGIN OF it_material OCCURS 0,
*      matnr(70) type c,
      matnr LIKE mara-matnr,
      maktx LIKE makt-maktx,
      mtart LIKE m_mat1t-mtart,
      lgort LIKE rmmg1-lgort,
      mfrpn LIKE mara-mfrpn,
      mfrnr LIKE mara-mfrnr,
      name1 LIKE lfa1-name1,
      lgpbe LIKE mard-lgpbe,
      labst LIKE mard-labst,
      stprs LIKE mbew-stprs,
      verpr LIKE mbew-verpr,
      dismm LIKE marc-dismm,
      disls LIKE marc-disls,
      eisbe LIKE marc-eisbe,
*-<   added on 03.12.2014 by Victor
*      mabst LIKE marc-mabst,
      lminb LIKE mard-lminb,
      lbstf LIKE mard-lbstf,
      maabc LIKE marc-maabc,
*->
      bstfe LIKE marc-bstfe,
      salk3 LIKE mbew-salk3,
      spmon LIKE s031-spmon,
      ambwg LIKE s031-ambwg,
      werks LIKE mard-werks,
      magbb LIKE s031-magbb,
      tline LIKE tline-tdline.
DATA: END OF it_material.

DATA: it_lfa1 LIKE lfa1 OCCURS 0 WITH HEADER LINE.
DATA: it_tline LIKE tline OCCURS 0 WITH HEADER LINE.
DATA: ztline LIKE tline OCCURS 0 WITH HEADER LINE.
DATA: zthead LIKE thead OCCURS 0 ."WITH HEADER LINE.

*****Start addition for ALV Grid*********
*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF ty_fcode,
        fcode LIKE rsmpe-func,
      END OF ty_fcode.

DATA: it_ex_func TYPE STANDARD TABLE OF ty_fcode WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      wa_ex_func TYPE ty_fcode.

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : c_mark   VALUE 'X'.

*-- For list box
DATA: wa_fld_name  TYPE vrm_id,
      it_list_box  TYPE vrm_values,
      wa_value LIKE LINE OF it_list_box.
**//-- Global : used Variable just in this Program
*-- Function Control
DATA : ok_code LIKE sy-ucomm.
DATA : wa_mode(7) TYPE c,
       wa_status(8) TYPE c.

DATA : wa_level(15) TYPE c.
DATA : wa_renewal_flg.
*--
DATA : wa_return     LIKE	bapireturn1.   "Return Values

*-- User Confirm for pop-up Message
DATA : wa_answer TYPE c.
DATA : wa_repid LIKE sy-repid.

*// Class Definition
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA : event_receiver TYPE REF TO lcl_event_receiver.

*// Declare reference variables, the container and internal table
DATA: custom_control    TYPE   scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.


* Global variables for attributes or etc of ALV GRID
DATA : wa_is_layout TYPE lvc_s_layo. "/The Layout Structure
DATA : it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

*****End addition for ALV Grid*********

*--------------------------------------------------
* Search Condition SELECTION WINDOW.
*--------------------------------------------------
*Selection Criteria:
*Material Type
*MRP Type
*Lot Size
*Company Code
*Plant
*Material
*Storage Location
*Valuation Class
*Key Date
*Period To Analyze From Date
*Period To Analyze To Date

SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE text-p01.
SELECT-OPTIONS: s_matnr   FOR  mara-matnr MEMORY ID mat, " Material NO
*                maktx for makt-maktx ,
                maktg FOR makt-maktg,
               mtart FOR m_mat1t-mtart DEFAULT 'ERSA' MEMORY ID mta,
               verpr FOR mbew-verpr,
            lgort FOR rmmg1-lgort MEMORY ID lag,
            lgpbe FOR mard-lgpbe,
            mfrpn FOR mara-mfrpn,
            mfrnr FOR mara-mfrnr MATCHCODE OBJECT kred,
            name1 FOR lfa1-name1,
            spwoc FOR s031-spwoc.

** Changed by Furong on 02/08/10
*PARAMETERS:
*            werks like marc-werks obligatory default 'P001' memory id
*WRK.   "UD1K940486
SELECT-OPTIONS: s_werks FOR mard-werks DEFAULT 'P001' MEMORY ID wrk.
PARAMETERS: p_zero RADIOBUTTON GROUP grp1,
            p_greate RADIOBUTTON  GROUP grp1 DEFAULT 'X',
            p_both RADIOBUTTON GROUP grp1.

** End of change

*            werks like marD-werks obligatory default 'P001' memory id
*WRK.   "UD1K940486

*PARAMETERS:
*            MTART like M_MAT1T-MTART obligatory default 'ERSA',
*            LGORT LIKE RMMG1-LGORT obligatory default 'P600'.

SELECTION-SCREEN END OF BLOCK file.

* CALL FUNCTION 'VRM_SET_VALUES'
*         EXPORTING
*              ID     = WA_FLD_NAME
*              VALUES = IT_LIST_BOX.
*
*----------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------
START-OF-SELECTION.

  PERFORM read_data.

END-OF-SELECTION.

  CHECK NOT it_material[] IS INITIAL.
  CALL SCREEN 9000.

*---------------------------------------------------
* INITIALIZATION.
*---------------------------------------------------
***add for ALV
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle events
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

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.

*-- / Double Click
  METHOD handle_double_click.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD handle_toolbar.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method
*    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
  ENDMETHOD.                    "HANDLE_TOOLBAR
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD handle_user_command.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.
  ENDMETHOD.                    "HANDLE_USER_COMMAND
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*
* lcl_event_receiver (Implementation)
*===================================================================


***End for Alv
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
*  PERFORM TITLE_WRITE.



*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  DATA : lw_thead LIKE thead.
  DATA : zmatnr(70) TYPE c.
  DATA : potext(999) TYPE c.
  DATA: BEGIN OF it_temp OCCURS 0,
*      matnr(70) type c,
        matnr LIKE mara-matnr,
        ambwg LIKE s031-ambwg,
        END OF it_temp.
  DATA: it_temp1 LIKE it_temp OCCURS 0 WITH HEADER LINE.

** changed by Fuorng on 02/08/10

*SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
*FROM   ( mara AS H INNER JOIN mard AS I
*ON     H~matnr     EQ  I~matnr             )
*inner join mbew as k
*on i~matnr = k~matnr
*and i~werks = k~bwkey
*inner join MARC as M
*on i~matnr = M~matnr
*and i~werks = M~WERKS
*
*inner join makt as J
*on H~matnr eq J~matnr
*WHERE  H~matnr     IN  S_matnr
*and    H~mtart     in mtart
**and    J~maktx     in maktx
*and H~mfrpn in mfrpn
*and H~mfrnr in mfrnr
*and k~verpr in verpr
*and i~LGPBE in LGPBE " addition for storage bin
*and    J~maktg     in maktg
*and    I~LGORT     in LGORT
*and    i~werks      = werks.

  CASE 'X'.
    WHEN p_zero.
*  IF P_ZERO = 'X'.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
      FROM   ( mara AS h INNER JOIN mard AS i
                    ON     h~matnr     EQ  i~matnr             )
                 INNER JOIN mbew AS k
                     ON i~matnr = k~matnr
                     AND i~werks = k~bwkey
                 INNER JOIN marc AS m
                     ON i~matnr = m~matnr
                     AND i~werks = m~werks
                 INNER JOIN makt AS j
                   ON h~matnr EQ j~matnr
      WHERE  h~matnr     IN  s_matnr
        AND  h~mtart     IN mtart
*and    J~maktx     in maktx
         AND h~mfrpn IN mfrpn
         AND h~mfrnr IN mfrnr
         AND k~verpr IN verpr
         AND i~lgpbe IN lgpbe " addition for storage bin
         AND    j~maktg     IN maktg
         AND    i~lgort     IN lgort
         AND    i~werks IN s_werks
         AND labst = 0.
*  ELSE.

    WHEN p_greate.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
      FROM   ( mara AS h INNER JOIN mard AS i
                 ON     h~matnr     EQ  i~matnr             )
               INNER JOIN mbew AS k
                 ON i~matnr = k~matnr
                 AND i~werks = k~bwkey
               INNER JOIN marc AS m
                 ON i~matnr = m~matnr
                 AND i~werks = m~werks

               INNER JOIN makt AS j
                 ON h~matnr EQ j~matnr
      WHERE  h~matnr     IN  s_matnr
         AND    h~mtart     IN mtart
*   and    J~maktx     in maktx
         AND h~mfrpn IN mfrpn
         AND h~mfrnr IN mfrnr
         AND k~verpr IN verpr
         AND i~lgpbe IN lgpbe " addition for storage bin
         AND    j~maktg     IN maktg
         AND    i~lgort     IN lgort
         AND    i~werks IN s_werks
         AND labst > 0.

    WHEN p_both.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
     FROM   ( mara AS h INNER JOIN mard AS i
                 ON     h~matnr     EQ  i~matnr             )
               INNER JOIN mbew AS k
                 ON i~matnr = k~matnr
                 AND i~werks = k~bwkey
               INNER JOIN marc AS m
                 ON i~matnr = m~matnr
                 AND i~werks = m~werks

               INNER JOIN makt AS j
                 ON h~matnr EQ j~matnr
     WHERE  h~matnr     IN  s_matnr
        AND    h~mtart     IN mtart
*   and    J~maktx     in maktx
        AND h~mfrpn IN mfrpn
        AND h~mfrnr IN mfrnr
        AND k~verpr IN verpr
        AND i~lgpbe IN lgpbe " addition for storage bin
        AND    j~maktg     IN maktg
        AND    i~lgort     IN lgort
        AND    i~werks IN s_werks
        AND labst >= 0.
  ENDCASE.

*  ENDIF.
** End of change

** add the manufacturer name

** changed by Fuorng on 02/08/10
*  LOOP AT IT_MATERIAL.
*    SELECT SINGLE  NAME1 FROM LFA1 INTO IT_MATERIAL-NAME1
*    WHERE LIFNR = IT_MATERIAL-MFRNR.
**if sy-subrc = 0.
**move it_lfa1-name1 to it_material-vendor.
*    MODIFY IT_MATERIAL.
**endif.
*  ENDLOOP.

  IF name1[] IS INITIAL.
    LOOP AT it_material.
      SELECT SINGLE  name1 FROM lfa1 INTO it_material-name1
      WHERE lifnr = it_material-mfrnr.
*if sy-subrc = 0.
*move it_lfa1-name1 to it_material-vendor.
      MODIFY it_material.
*endif.
    ENDLOOP.
  ELSE.
    LOOP AT it_material.
      SELECT SINGLE  name1 FROM lfa1 INTO it_material-name1
      WHERE lifnr = it_material-mfrnr.
      IF it_material-name1 IN name1.
*if sy-subrc = 0.
*move it_lfa1-name1 to it_material-vendor.
        MODIFY it_material.
      ELSE.
        DELETE it_material.
      ENDIF.
*endif.
    ENDLOOP.

  ENDIF.
** End of change
***********************************
*new addition

  IF NOT it_material[] IS INITIAL.
    SELECT matnr
    ambwg
    FROM s031 INTO TABLE it_temp
    FOR ALL ENTRIES IN it_material
    WHERE spwoc IN spwoc
    AND werks = it_material-werks
    AND matnr = it_material-matnr
    AND lgort = it_material-lgort
    AND  ambwg > 0 .
  ENDIF.

  LOOP AT it_temp.
    COLLECT it_temp INTO it_temp1.
  ENDLOOP.


*loop at it_material.
*
*read table it_temp1 with key  matnr = it_material-matnr.
*it_material-ambwg = it_temp1-ambwg.
*modify  it_material.
*endloop.

  LOOP AT it_temp1.

    READ TABLE it_material WITH KEY  matnr = it_temp1-matnr.
    it_material-ambwg = it_temp1-ambwg.
    MODIFY  it_material TRANSPORTING ambwg WHERE matnr = it_temp1-matnr.
  ENDLOOP.


**********end addition***********

**loop at it_material.
**refresh it_tline.
**   clear it_tline.
**   clear lw_thead.
**PERFORM READ_LONGTEXT  TABLES  IT_TLINE
**                            USING   LW_THEAD.
**
**
**endloop.
  LOOP AT it_material.
    CLEAR: ztline[],ztline.
    CLEAR it_material-tline.
    zmatnr = it_material-matnr.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
       client                        = sy-mandt
        id                            = 'BEST'
        language                      = sy-langu
        name                          = zmatnr
        object                        = 'MATERIAL'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        = ZTHEAD
      TABLES
        lines                         = ztline
     EXCEPTIONS
       id                            = 1
       language                      = 2
       name                          = 3
       not_found                     = 4
       object                        = 5
       reference_check               = 6
       wrong_access_to_archive       = 7
       OTHERS                        = 8
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    DATA: total_value LIKE mbew-salk3.
    CLEAR: total_value.
    IF it_material-labst <> 0.
      IF it_material-stprs <> 0.
        total_value = it_material-stprs * it_material-labst.
      ELSEIF it_material-verpr <> 0.
        total_value = it_material-verpr * it_material-labst.
      ENDIF.
    ENDIF.

*WRITE:/1 IT_MATERIAL-MATNR,41 it_material-maktx,
*82 IT_MATERIAL-MFRNR.
*
*
*write:/1 IT_MATERIAL-LGORT, 15 IT_MATERIAL-MTART,30 total_value,65
*IT_MATERIAL-MFRPN, 85 IT_MATERIAL-DISMM, 95 IT_MATERIAL-DISLS.
*
*write:/1 IT_MATERIAL-LGPBE,25 it_material-verpr,
*45 IT_MATERIAL-LABST ,65 it_material-stprs, 85 IT_MATERIAL-EISBE, 100
*IT_MATERIAL-MABST.
*LOOP AT ZTLINE.
*WRITE:/ ZTLINE-TDLINE.
*
*ENDLOOP.
*Write:/ sy-uline.

*Add PO Text to the ALV GRID.
    LOOP AT ztline.

      CONCATENATE potext ztline-tdline INTO potext.
    ENDLOOP.
    it_material-tline = potext.
    MODIFY  it_material.
    potext = ''.
*   clear potext.
    CLEAR zmatnr.
    CLEAR it_material-tline.
  ENDLOOP.


ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  READ_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_LW_THEAD  text
*----------------------------------------------------------------------*
FORM read_longtext TABLES   p_tline STRUCTURE tline
                              "Insert correct name for <...>
                   USING     p_thead    STRUCTURE thead.

*CALL FUNCTION 'READ_TEXT'
*       EXPORTING
*            CLIENT                  = SY-MANDT
*            ID                      = P_THEAD-TDID
*            LANGUAGE                = SY-LANGU
*            NAME                    = P_THEAD-TDNAME
*            OBJECT                  = P_THEAD-TDOBJECT
*            ARCHIVE_HANDLE          = 0
*            LOCAL_CAT               = ' '
**       IMPORTING
**            HEADER                  =
*       TABLES
*            LINES                   = P_TLINE
*       EXCEPTIONS
*            ID                      = 1
*            LANGUAGE                = 2
*            NAME                    = 3
*            NOT_FOUND               = 4
*            OBJECT                  = 5
*            REFERENCE_CHECK         = 6
*            WRONG_ACCESS_TO_ARCHIVE = 7
*            OTHERS                  = 8.
*
*  IF SY-SUBRC <> 0.
**    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " READ_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM title_write.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 'Material Details'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'Date: ' ,
            sy-datum .
  SKIP 2.
*WRITE: /1 'Material', 31 'Mat Type', 40 'Material Description',
*  81 'Storage Loc',
*  105 'Manufacturer Part No', 126 'Manufacturer'.
**137 'Storage Bin'.
*Write:/1 'Standard Price', 20 'Moving avg Price',44 'Total Value',
* 60 'Storage Bin', 90 'Stock Quantity'.
*Write: /1 'Material P O Text'.
*
  WRITE:/1 'Material',41 'Material Description',
  82 'Manufacturer'.


  WRITE:/1 'Storage Loc', 15 'Material Type',30 'Total Value',65
  'Manufacturer Part No', 90 'MRP Type', 100 'Lot size'.

  WRITE:/1 'Storage Bin',25 'Moving avg Price',
 45 'Stock Quantity' ,65 'Standard Price', 85 'Safety Stock', 100 'Max'.

  WRITE: /1 'Material P O Text'.

  WRITE:/ sy-uline.
*  WRITE: / '   '  COLOR COL_HEADING INTENSIFIED OFF,
*           ': Material ' ,
*           '   '  COLOR COL_NORMAL  INTENSIFIED OFF,
*           ': Short Text ' ,
*           '   '  COLOR COL_NORMAL  INTENSIFIED ON,
*           ': Storage Loc  ',
*           '   '  COLOR COL_TOTAL   INTENSIFIED OFF,
*           ': Material Type  ',
*           '   '  COLOR COL_POSITIVE INTENSIFIED OFF.


ENDFORM.                    " TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000' .
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Container for ALV GRID

*- Create Container('GRID_CONTAINER') with Custom Contro on screen
    CREATE OBJECT grid_container
      EXPORTING
        container_name              = custom_control
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      wa_repid = sy-repid.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = wa_repid
          txt2  = sy-subrc
          txt1  = 'The control could not be created'(e02).
    ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
    CREATE OBJECT alv_grid
      EXPORTING
        i_parent      = grid_container
        i_appl_events = 'X'.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of IT_FIELDCAT
    PERFORM mask_columns_of_alv_grid TABLES it_fieldcat.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    CALL METHOD alv_grid->set_table_for_first_display
         EXPORTING i_structure_name = 'ZPMMATERIAL'
                   is_layout        = wa_is_layout
                   i_save           = wa_save
                   is_variant       = wa_variant
                   i_default        = c_mark
*                   SET_LAYOUT_PROPERTIES = c_mark
*            IT_TOOLBAR_EXCLUDING  = <internal table of type
*                                                          UI_FUNCTIONS>
*            IT_HYPERLINK          = <internal table of type LVC_T_HYPE>
*            IT_ALV_GRAPHICS       = <internal table of type DTC_T_TC>
         CHANGING  it_fieldcatalog  = it_fieldcat[]
                   it_outtab        = it_material[].
*            IT_SORT               = <internal table of type LVC_T_SORT>
*            IT_FILTER             = <internal table of type LVC_T_FILT>


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
      EXPORTING
        control = alv_grid.


  ENDIF.

  IF NOT grid_container IS INITIAL AND
         wa_renewal_flg = c_mark.

    CLEAR wa_renewal_flg.
*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of sflight
    PERFORM mask_columns_of_alv_grid TABLES it_fieldcat.

*-- Display data on ALV GRID Control using method

    PERFORM set_new_table_data.
    PERFORM refresh_alv_grid_data_disp.

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = alv_grid.

  ENDIF.


ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      REFRESH it_material.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout.
*//-- Set Layout Structure
  wa_is_layout-language = sy-langu.      "/Language Key
  wa_is_layout-cwidth_opt = c_mark.  "/Optimize column width
  wa_is_layout-numc_total  = c_mark. "/Allow totals for NUMC
  wa_is_layout-sel_mode  = 'A'. "/mode for select col and row
*//-- Set Variant Structure
* This allows changes to be done to layout and save it.
  wa_variant-report = sy-repid.
  wa_variant-username = sy-uname.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns_of_alv_grid TABLES    pt_fieldcat TYPE lvc_t_fcat.
  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  REFRESH pt_fieldcat. CLEAR pt_fieldcat.


* Build the fieldcat according to DDIC structure ZSQM_EQ_SUMM:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZPMMATERIAL'
    CHANGING
      ct_fieldcat      = pt_fieldcat[].

*
* Set field attribute
*  IF P_EQTYP = 'Z'.
  LOOP AT pt_fieldcat.

    CASE pt_fieldcat-fieldname.
      WHEN 'EQUNR'  .
        pt_fieldcat-key_sel = c_mark.
        pt_fieldcat-key      = c_mark.
      WHEN 'EQKTX'.
        pt_fieldcat-emphasize = c_mark.
      WHEN 'EQTYP' OR 'ATINN' OR 'ADZHL' OR 'CLINT' OR 'IMERK' OR
           'KLART' OR 'STATU' OR 'SPRAS' OR 'ILOAN' OR
           'AUTYP' OR 'STSMA' OR 'STAT' OR 'PARVW' OR 'RMZHL' OR
           'STAT_AUF' OR 'MABST'.
        pt_fieldcat-no_out = c_mark.

      WHEN 'LMINB'.
        pt_fieldcat-reptext = 'Min.Stock Level'.

      WHEN 'LBSTF'.
        pt_fieldcat-reptext = 'Max.Stock Level'.

      WHEN 'MAABC'.
        pt_fieldcat-reptext = 'ABC'.
      WHEN OTHERS.

    ENDCASE.

    pt_fieldcat-scrtext_l = pt_fieldcat-scrtext_s
        = pt_fieldcat-scrtext_m = pt_fieldcat-seltext =
          pt_fieldcat-tooltip =   pt_fieldcat-reptext.

    MODIFY pt_fieldcat.

  ENDLOOP.

*  ELSEIF P_EQTYP = 'Y'.
*    LOOP AT PT_FIELDCAT.
*
*      CASE PT_FIELDCAT-FIELDNAME.
*        WHEN 'EQUNR'  .
*          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-KEY      = C_MARK.
*        WHEN 'EQKTX'.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
*        WHEN 'EQTYP' OR 'ATINN' OR 'ADZHL' OR 'CLINT' OR 'IMERK' OR
*             'KLART' OR 'STATU' OR 'SPRAS' OR 'ILOAN' OR
*             'AUTYP' OR 'STSMA' OR 'STAT' OR 'PARVW' OR 'RMZHL' OR
*             'STAT_AUF' OR
*             'ANLNR' OR 'TXT04' OR 'CLASS' OR 'ATNAM' OR 'ATWRT' OR
*             'PARNR' OR 'ORGTX' OR 'WARPL' OR 'ZYKL1' OR 'ZEIEH' OR
*             'BRGEW' OR 'GEWEI' OR 'GROES' OR 'INBDT' OR 'ANSWT' OR
*             'WAERS' OR 'HERST' OR 'TYPBZ' .
*          PT_FIELDCAT-NO_OUT = C_MARK.
*
*        WHEN OTHERS.
*
*      ENDCASE.
*
*      MODIFY PT_FIELDCAT.
*    ENDLOOP.
*
*  ENDIF.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_new_table_data.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv_grid_data_disp.

ENDFORM.                    " REFRESH_ALV_GRID_DATA_DISP
*&---------------------------------------------------------------------*
*&      Form  FREE_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_alv_grid.
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
