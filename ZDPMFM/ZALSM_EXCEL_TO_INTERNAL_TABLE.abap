FUNCTION ZALSM_EXCEL_TO_INTERNAL_TABLE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_BEGIN_COL) TYPE  I
*"     VALUE(I_BEGIN_ROW) TYPE  I
*"     VALUE(I_END_COL) TYPE  I
*"     VALUE(I_END_ROW) TYPE  I
*"  TABLES
*"      INTERN STRUCTURE  ZKCDE_CELLS
*"  EXCEPTIONS
*"      INCONSISTENT_PARAMETERS
*"      UPLOAD_OLE
*"----------------------------------------------------------------------

  DATA: excel_tab     TYPE  ty_t_sender.
  DATA: ld_separator  TYPE  c.
  FIELD-SYMBOLS: <field>.
  DATA: application   TYPE  ole2_object,
        workbook      TYPE  ole2_object,
        range         TYPE  ole2_object,
        worksheet     TYPE  ole2_object.
  DATA: h_cell        TYPE  ole2_object,
        h_cell1       TYPE  ole2_object.

* Makro f? Fehlerbehandlung der Methods
  DEFINE m_message.
    case sy-subrc.
      when 0.
      when 1.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      when others. raise upload_ole.
    endcase.
  END-OF-DEFINITION.


* check parameters
  IF i_begin_row > i_end_row. RAISE inconsistent_parameters. ENDIF.
  IF i_begin_col > i_end_col. RAISE inconsistent_parameters. ENDIF.

* set seperator. Direct move is not sufficient (cause of hex type)
  ASSIGN ld_separator TO <field> TYPE 'X'.
  <field> = gc_hex_tab.

* open file in Excel
  IF application-header = space OR application-handle = -1.
    CREATE OBJECT application 'Excel.Application'.
    m_message.
  ENDIF.
  CALL METHOD  OF application    'Workbooks' = workbook.
  m_message.
  CALL METHOD  OF workbook 'Open'    EXPORTING #1 = filename.
  m_message.
*  set property of application 'Visible' = 1.
*  m_message.
  GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
  m_message.

* mark whole spread sheet
  CALL METHOD OF worksheet 'Cells' = h_cell
      EXPORTING #1 = i_begin_row #2 = i_begin_col.
  m_message.
  CALL METHOD OF worksheet 'Cells' = h_cell1
      EXPORTING #1 = i_end_row #2 = i_end_col.
  m_message.

  CALL METHOD  OF worksheet 'RANGE' = range
                 EXPORTING #1 = h_cell #2 = h_cell1.
  m_message.
  CALL METHOD OF range 'SELECT'.
  m_message.

* copy marked area (whole spread sheet) into Clippboard
  CALL METHOD OF range 'COPY'.
  m_message.

* Without control flush, CLPB_IMPORT does not find any data
  CALL FUNCTION 'CONTROL_FLUSH'
       EXCEPTIONS    OTHERS = 3.

* read clipboard into ABAP
  CALL FUNCTION 'CLPB_IMPORT'
       TABLES         data_tab   = excel_tab
       EXCEPTIONS     clpb_error = 1
                      OTHERS     = 2.
  IF sy-subrc <> 0.
     MESSAGE A037(ALSMEX).
  ENDIF.

  PERFORM separated_to_intern_convert TABLES excel_tab intern
                                      USING  ld_separator.

* clear clipboard
  REFRESH excel_tab.
  CALL FUNCTION 'CLPB_EXPORT'
       TABLES          data_tab   = excel_tab
       EXCEPTIONS      clpb_error = 1
                       OTHERS     = 2.

* quit Excel and free ABAP Object - unfortunately, this does not kill
* the Excel process
  CALL METHOD OF application 'QUIT'.
  m_message.

* >>>>> Begin of change note 575877
* to kill the Excel process it's necessary to free all used objects
  FREE OBJECT h_cell.       m_message.
  FREE OBJECT h_cell1.      m_message.
  FREE OBJECT range.        m_message.
  FREE OBJECT worksheet.    m_message.
  FREE OBJECT workbook.     m_message.
  FREE OBJECT application.  m_message.
* <<<<< End of change note 575877
ENDFUNCTION.
