REPORT  ZC01FIC_CHART_OF_ACCOUNTS    line-size 180     MESSAGE-ID 0.

*&---------------------------------------------------------------------*
*& Program Name :Z_CHART_OF_ACCOUNTS_UPLOAD                            *
*& Description : Upload Chart of Accounts from a tab delimited file
*
*& Module : FI                                                         *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*& Date        Author           Description/Reason for Change          *
*& ----------  --------------   ------------------------------------   *
*& 01/23/2003  Chris Meen       Initial Development                    *
*&---------------------------------------------------------------------*


************************************************************************
*  Tables
************************************************************************
tables: skb1,  "GL accounts
        t100.  "Messages

************************************************************************
*  Data Declarations
************************************************************************

* Input file format
data: begin of t_rec  occurs 0,
        SAKNR    LIKE SKA1-SAKNR,  " Account number
        BUKRS    LIKE RF02H-BUKRS, " Company code
        KTOKS    LIKE SKA1-KTOKS,  " Account group
        xplacct  like GLACCOUNT_SCREEN_COA-xplacct, " PL Type
        xbilk    like GLACCOUNT_SCREEN_COA-xbilk,  "BS
        TXT20    LIKE SKAT-TXT20,  " Short_text
        TXT50    LIKE SKAT-TXT50,  " Long_text
        WAERS    LIKE SKB1-WAERS,  " Currency
        XSALH    LIKE SKB1-XSALH,  " Only balances in local crcy
        MWSKZ    LIKE SKB1-MWSKZ,  " Tax category
        XMWNO    LIKE SKB1-XMWNO,  " Posting without tax allowed
        MITKZ    LIKE SKB1-MITKZ,  " Reconciliation account
        XOPVW    LIKE SKB1-XOPVW,  " Open item management
        XKRES    LIKE SKB1-XKRES,  " Line Item Management
        ZUAWA    LIKE SKB1-ZUAWA,  " Sort key
        FSTAG    LIKE SKB1-FSTAG,  " Field status group
        XINTB    LIKE SKB1-XINTB,  " Post automatically only
        XNKON    LIKE SKB1-XNKON,  " Supplement auto. posting
        FDLEV    LIKE SKB1-FDLEV,  " Planning level
        XGKON    LIKE SKB1-XGKON,  " Relevant to cash flow
        FIPOS    LIKE SKB1-FIPOS,  " Commitment Item
        HBKID    LIKE SKB1-HBKID,        " House bank
        HKTID    LIKE SKB1-HKTID,        " Bank account
      end of t_rec.

* BDC data
DATA : BEGIN OF t_bdc_tab OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF t_bdc_tab.

* Call transaction message table
DATA : BEGIN OF t_MESSTAB OCCURS 5.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF t_MESSTAB.


************************************************************************
*  Selection screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_FILE LIKE RLGRAP-FILENAME obligatory.
* Call transaction mode
parameter: p_mode(1) TYPE c obligatory default 'N'.
SELECTION-SCREEN END OF BLOCK 1.

************************************************************************
*  At selection screen
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* Allow F4 on filename
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.*,*.*.'
            MODE             = 'O'
            TITLE            = TEXT-T01
       IMPORTING
            FILENAME         = P_FILE
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.

************************************************************************
*  Start of selection
************************************************************************

START-OF-SELECTION.

* Upload tab delimited data file
  CALL FUNCTION 'WS_UPLOAD'
   EXPORTING
     FILENAME                      = p_FILE
     FILETYPE                      = 'DAT'
    TABLES
      DATA_TAB                      = t_rec.

* Loop at each data record and build bdc data
  LOOP AT t_REC.

* Check this is not a blank data record
    CHECK t_rec-SAKNR NE SPACE.

* Re-format GL account number
    SHIFT t_rec-SAKNR RIGHT DELETING TRAILING SPACE.
    WHILE SY-SUBRC = 0.
      REPLACE ' ' WITH '0' INTO t_rec-SAKNR.

    ENDWHILE.

* Check if account already exists for this company code
    select single saknr from skb1
      into skb1-saknr
         where saknr = t_rec-saknr and
               bukrs = t_rec-bukrs.

    if sy-subrc = 0.
      write:/ 'Account', t_rec-saknr, 'already exists in company code',
t_rec-bukrs.
    else.

* Build BDC data for transaction FS00
      PERFORM GENERATE_BDC_DATA_COA.

* Call transaction FS00
      CALL TRANSACTION 'FS00'
           USING   t_bdc_tab
           MODE   p_MODE
           UPDATE 'S'
           MESSAGES INTO t_MESSTAB.

      IF SY-SUBRC = 0.
        write:/ 'Account', t_rec-saknr, 'created in company code',
t_rec-bukrs.
      ELSE.
* If call transaction failed, find and display error message
        write:/ 'Errors creating account', t_rec-saknr.
        LOOP AT t_MESSTAB WHERE MSGTYP = 'E'.
          WRITE:/ 'Error Message', t_messtab-msgid, t_messtab-msgnr,
    t_messtab-msgv1.
          select single text from t100 into t100-text
             where sprsl = sy-langu and
                   arbgb = t_messtab-msgid and
                   msgnr = t_messtab-msgnr.

          if sy-subrc = 0.
            write:/ t100-text.
          endif.
        ENDLOOP.

      endif.
      REFRESH: t_MESSTAB.
      CLEAR: t_MESSTAB.

    endif.


  ENDLOOP.

*-----------------------------------------------------------------------
END-OF-SELECTION.
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_COA
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_COA.
  REFRESH t_bdc_tab.
  clear t_bdc_tab.

  PERFORM BDC_DYNPRO USING 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_KEY-SAKNR' t_rec-SAKNR.
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_KEY-BUKRS' t_rec-BUKRS.

* Switch to create mode
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'ACC_CRE'.
  PERFORM BDC_DYNPRO USING 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

* First tab
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_COA-KTOKS' t_rec-KTOKS.
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_COA-XPLACCT' t_rec-XPLACCT.
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_COA-XBILK' t_rec-XBILK.
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_COA-TXT20_ML' t_rec-TXT20.
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_COA-TXT50_ML' t_rec-TXT50.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'TAB02'.
  PERFORM BDC_DYNPRO USING 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

* Second tab
  PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-WAERS' t_rec-WAERS.
  IF t_rec-XSALH NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XSALH' t_rec-XSALH.
  ENDIF.
  IF t_rec-MWSKZ NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-MWSKZ' t_rec-MWSKZ.
  ENDIF.
  IF t_rec-XMWNO NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XMWNO' t_rec-XMWNO.
  ENDIF.
  IF t_rec-MITKZ NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-MITKZ' t_rec-MITKZ.
  ENDIF.
  IF t_rec-XOPVW NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XOPVW' t_rec-XOPVW.
  ENDIF.
  IF t_rec-XKRES NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XKRES' t_rec-XKRES.
  ENDIF.
  IF t_rec-ZUAWA NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-ZUAWA' t_rec-ZUAWA.
  ENDIF.

* Third tab
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  'TAB03'.
  PERFORM BDC_DYNPRO USING 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

  IF t_rec-FSTAG NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-FSTAG' t_rec-FSTAG.
  ENDIF.
  IF t_rec-XINTB NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XINTB' t_rec-XINTB.
  ENDIF.
  IF t_rec-XNKON NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XNKON' t_rec-XNKON.
  ENDIF.
  IF t_rec-FDLEV NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-FDLEV' t_rec-FDLEV.
  ENDIF.
  IF t_rec-XGKON NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-XGKON' t_rec-XGKON.
  ENDIF.
  IF t_rec-FIPOS NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-FIPOS' t_rec-FIPOS.
  ENDIF.
  IF t_rec-HBKID NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-HBKID' t_rec-HBKID.
  ENDIF.
  IF t_rec-HKTID NE SPACE.
    PERFORM BDC_FIELD  USING 'GLACCOUNT_SCREEN_CCODE-HKTID' t_rec-HKTID.
  ENDIF.

* Save GL account
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/11'.
  PERFORM BDC_DYNPRO USING 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

ENDFORM.                               " GENERATE_BDC_DATA_COA

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING    P_PROGRAM P_DYNPRO.
  CLEAR t_bdc_tab.
  t_bdc_tab-PROGRAM = P_PROGRAM.
  t_bdc_tab-DYNPRO  = P_DYNPRO.
  t_bdc_tab-DYNBEGIN = 'X'.
  APPEND t_bdc_tab.
  CLEAR t_bdc_tab.
ENDFORM.                               " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM  P_FVAL.
  CLEAR t_bdc_tab.
  t_bdc_tab-FNAM = P_FNAM.
  t_bdc_tab-FVAL = P_FVAL.
  APPEND t_bdc_tab.
  CLEAR t_bdc_tab.
ENDFORM.                               " BDC_FIELD
