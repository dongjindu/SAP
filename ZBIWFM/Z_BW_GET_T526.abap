FUNCTION Z_BW_GET_T526.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_REQUNR) TYPE  SRSC_S_IF_SIMPLE-REQUNR OPTIONAL
*"     REFERENCE(I_DSOURCE) TYPE  SRSC_S_IF_SIMPLE-DSOURCE OPTIONAL
*"     REFERENCE(I_MAXSIZE) TYPE  SRSC_S_IF_SIMPLE-MAXSIZE DEFAULT
*"       '500'
*"     REFERENCE(I_INITFLAG) TYPE  SRSC_S_IF_SIMPLE-INITFLAG OPTIONAL
*"     REFERENCE(I_READ_ONLY) TYPE  SRSC_S_IF_SIMPLE-READONLY OPTIONAL
*"  TABLES
*"      E_T_DATA STRUCTURE  ZBWT526 OPTIONAL
*"      I_T_SELECT TYPE  SRSC_S_IF_SIMPLE-T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SRSC_S_IF_SIMPLE-T_FIELDS OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

*  CLEAR E_T_DATA. REFRESH E_T_DATA.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE E_T_DATA
*           FROM T157E
*           WHERE SPRAS EQ 'EN'.
*  IF SY-SUBRC = 0.
*    RAISE NO_MORE_DATA.
*  ENDIF.
*
*ENDFUNCTION.


* Maximum number of lines for DB table
  STATICS: S_S_IF TYPE SRSC_S_IF_SIMPLE,

* counter
          S_COUNTER_DATAPAKID LIKE SY-TABIX,

* cursor
          S_CURSOR TYPE CURSOR.

* Initialization mode (first call by SAPI) or data transfer mode
* (following calls) ?
  IF I_INITFLAG = SBIWA_C_FLAG_ON.

***********************************************************************
*
* Initialization: check input parameters
*                 buffer input parameters
*                 prepare data selection
***********************************************************************
*

* Fill parameter buffer for data extraction calls
    S_S_IF-REQUNR    = I_REQUNR.
    S_S_IF-DSOURCE = I_DSOURCE.
    S_S_IF-MAXSIZE   = I_MAXSIZE.

* Fill field list table for an optimized select statement
* (in case that there is no 1:1 relation between InfoSource fields
* and database table fields this may be far from beeing trivial)
    APPEND LINES OF I_T_FIELDS TO S_S_IF-T_FIELDS.

  ELSE.                 "Initialization mode or data extraction ?

***********************************************************************
*
* Data transfer: First Call      OPEN CURSOR + FETCH
*                Following Calls FETCH only
***********************************************************************
*

* First data package -> OPEN CURSOR
    IF S_COUNTER_DATAPAKID = 0.

* Determine number of database records to be read per FETCH statement
* from input parameter I_MAXSIZE. If there is a one to one relation
* between DataSource table lines and database entries, this is trivial.
* In other cases, it may be impossible and some estimated value has to
* be determined.
      OPEN CURSOR WITH HOLD S_CURSOR FOR
      SELECT * FROM T526.
    ENDIF.                             "First data package ?

* Fetch records into interface table.
*   named E_T_'Name of extract structure'.
    FETCH NEXT CURSOR S_CURSOR
               APPENDING CORRESPONDING FIELDS
               OF TABLE E_T_DATA
               PACKAGE SIZE S_S_IF-MAXSIZE.

*    SELECT * INTO CORRESPONDING FIELDS OF TABLE E_T_DATA
*             FROM T157E
*             WHERE SPRAS EQ 'EN'.

    IF SY-SUBRC <> 0.
      CLOSE CURSOR S_CURSOR.
      RAISE NO_MORE_DATA.
    ENDIF.

    S_COUNTER_DATAPAKID = S_COUNTER_DATAPAKID + 1.

  ENDIF.              "Initialization mode or data extraction ?

ENDFUNCTION.
