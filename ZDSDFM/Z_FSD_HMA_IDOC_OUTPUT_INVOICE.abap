FUNCTION Z_FSD_HMA_IDOC_OUTPUT_INVOICE.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(OBJECT) LIKE  NAST STRUCTURE  NAST
*"     VALUE(CONTROL_RECORD_IN) LIKE  EDIDC STRUCTURE  EDIDC
*"  EXPORTING
*"     VALUE(OBJECT_TYPE) LIKE  WFAS1-ASGTP
*"     VALUE(CONTROL_RECORD_OUT) LIKE  EDIDC STRUCTURE  EDIDC
*"  TABLES
*"      INT_EDIDD STRUCTURE  EDIDD
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED
*"      NO_UNIT_ORDER_FOUND
*"----------------------------------------------------------------------
  DATA : XVBELN   TYPE VBELN,
         XMDCD    LIKE ZTSD_UM-MODEL_CODE,
         XBDNO    LIKE ZTSD_UM-BODY_NO,
         XOBJEK   LIKE MARA-MATNR,
         XWOSER   LIKE AUSP-ATWRT ,
         XSDATE   LIKE AUSP-ATWRT .
  DATA : XHEAD   LIKE TABLE OF ZINVSEG1 WITH HEADER LINE,
         XITEM   LIKE TABLE OF ZINVSEG2 WITH HEADER LINE.
  DATA : XAUSP    LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
         XVBRK    LIKE TABLE OF VBRK WITH HEADER LINE,
         XVBPA    LIKE TABLE OF VBPA WITH HEADER LINE.
  DATA : BEGIN OF XVBRP OCCURS 0 .
          INCLUDE STRUCTURE VBRP.
  DATA :  BDNO  LIKE ZTSD_UM-BODY_NO ,
          MDCD LIKE ZTSD_UM-MODEL_CODE,
         END OF XVBRP.

  DATA : LV_MODEL_YEAR LIKE ZTPP_VM-MODEL_YEAR.
  DATA : LV_MODEL(3).

  DATA : XSDUM LIKE TABLE OF ZTSD_UM WITH HEADER LINE.
  DATA : WTAB(72) OCCURS 100 WITH HEADER LINE.
  RANGES : R_ERDAT FOR SY-DATUM.

  DATA : XEDIDD LIKE INT_EDIDD.

*-------MODEL FUNCTION: IDOC_OUTPUT_DELVRY
* Key document
  XVBELN = OBJECT-OBJKY.  "Billing#

* prepare control_record_out
  DATA: H_LOGSYS LIKE T000-LOGSYS.

  CHECK OBJECT-NACHA = '6' OR                           "EDI
        OBJECT-NACHA = 'A'.                             "ALE
  CLEAR CONTROL_RECORD_OUT.
  MOVE CONTROL_RECORD_IN TO CONTROL_RECORD_OUT.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      OWN_LOGICAL_SYSTEM             = H_LOGSYS
    EXCEPTIONS
      OWN_LOGICAL_SYSTEM_NOT_DEFINED = 1
      OTHERS                         = 2.
  IF ( SY-SUBRC IS INITIAL ).
    CONTROL_RECORD_OUT-SNDPRT = 'LS'.
    CONTROL_RECORD_OUT-SNDPRN = H_LOGSYS.
  ELSE.
    MESSAGE ID 'B2' TYPE 'E' NUMBER '001'
            RAISING ERROR_MESSAGE_RECEIVED.                 "#EC *
  ENDIF.

  CONTROL_RECORD_OUT-SERIAL = SY-DATUM.
  CONTROL_RECORD_OUT-SERIAL+8 = SY-UZEIT.


* fill data in idoc table
  CLEAR : XHEAD, XITEM, XITEM[], XOBJEK.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE XVBRK
   FROM VBRK WHERE VBELN EQ XVBELN. "(WTAB) .

  CHECK NOT XVBRK[] IS INITIAL.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE XVBPA
         FROM VBPA
         FOR ALL ENTRIES IN XVBRK
         WHERE VBELN EQ XVBRK-VBELN
           AND PARVW EQ 'WE'.

*# make HEAD
  SORT :  XVBRK BY VBELN,
          XVBPA BY VBELN.

  LOOP AT XVBRK.
    READ TABLE XVBPA WITH KEY VBELN = XVBRK-VBELN.
    IF SY-SUBRC <> 0 .
      CLEAR XVBPA.
    ENDIF.
    XHEAD-IVNB = XVBRK-VBELN.
    XHEAD-IVDT = XVBRK-FKDAT.
    XHEAD-DIST = XVBPA-KUNNR.
    XHEAD-HKMC = 'HMG'.

    CASE XVBRK-VBTYP.
      WHEN 'M'.
        XHEAD-IVTP = 'A'.
      WHEN 'N'.
        XHEAD-IVTP = 'B'.

      WHEN 'P'.
        XHEAD-IVTP = 'C'.

      WHEN 'O'.
        XHEAD-IVTP = 'D'.
    ENDCASE.

    APPEND XHEAD.
*****
* For IDOC
*****
    XEDIDD-SEGNAM = 'ZINVSEG1'. "segment head
    XEDIDD-SDATA  = XHEAD    .

    APPEND XEDIDD TO INT_EDIDD .
  ENDLOOP.

*# make Item

  SELECT * INTO CORRESPONDING FIELDS OF XVBRP
   FROM VBRP
   FOR ALL ENTRIES IN XVBRK
   WHERE VBELN EQ XVBRK-VBELN .

    XVBRP-MDCD  = XVBRP-VGBEL+0(3).
    XVBRP-BDNO  = XVBRP-VGBEL+3(6).
    APPEND XVBRP.
  ENDSELECT.

  CHECK NOT XVBRP[] IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE XSDUM
    FROM ZTSD_UM
    FOR ALL ENTRIES IN XVBRP
    WHERE BODY_NO = XVBRP-BDNO
      AND MODEL_CODE = XVBRP-MDCD
      AND STATUS     <> 'S'
      AND STATUS     <> 'D'.

  SORT : XVBRK BY VBELN ,
         XVBRP BY VBELN VGBEL BDNO MDCD,
         XHEAD BY IVNB  ,
         XSDUM BY BODY_NO MODEL_CODE .

  CLEAR : XEDIDD.

  LOOP AT XVBRP.
** Furong on 08/07/12
    CLEAR: XITEM.
** End on 08/07/12
    READ TABLE XVBRK WITH KEY VBELN = XVBRP-VBELN.
    IF SY-SUBRC <> 0 . CLEAR XVBRK. ENDIF.

    READ TABLE XHEAD WITH KEY IVNB = XVBRP-VBELN.
    IF SY-SUBRC <> 0 . CLEAR XHEAD . ENDIF.

    READ TABLE XSDUM WITH KEY BODY_NO    = XVBRP-BDNO
                              MODEL_CODE = XVBRP-MDCD .
    IF SY-SUBRC <> 0 . CLEAR XSDUM . ENDIF.

    XITEM-HKMC          = 'HMG'.
    XITEM-INVTYP        = XHEAD-IVTP.
    XITEM-IVNB          = XVBRP-VBELN.
    XITEM-ZFSC          = XVBRP-MATNR.
    XITEM-IVDT      = XVBRK-FKDAT.
    XITEM-XINVB	    = XVBRK-XBLNR.
    XITEM-UORD      = XSDUM-INTNO.
    XITEM-ZVIN      = XSDUM-ZVIN .
*    XITEM-CURR      = XVBRK-STWAE.
    XITEM-CURR      = XVBRK-WAERK. "use CAD(not USD) 05.29.2014 Victor
    XITEM-FOBA      = XVBRP-NETWR.
    XITEM-ZCIF      = XVBRP-NETWR.
    XITEM-KGSS      = XVBRP-NTGEW.
    XITEM-CRDT      = XVBRP-ERDAT.


    PERFORM GETSINGLE_ATWRT USING :

    XVBRP-VGBEL XITEM-VINN          'P_VIN',
    XVBRP-VGBEL XITEM-MIMI          'P_MI',
    XVBRP-VGBEL XITEM-OCCN          'P_OCN',
    XVBRP-VGBEL XWOSER              'P_WORK_ORDER',
    XVBRP-VGBEL XITEM-EXCL          'P_EXT_COLOR',
    XVBRP-VGBEL XITEM-INCL          'P_INT_COLOR',
    XVBRP-VGBEL XITEM-PRDT          'P_RP18_SHOP_DATE'.

** Furong on 08/06/12
*    xvbrp-vgbel xitem-msdate       'P_RP23_SHOP_DATE',
*    xvbrp-vgbel xsdate         'P_RP23_ACTUAL_DATE'.

    XITEM-WKNO = XWOSER+0(9).
    XITEM-DIST = XWOSER+9(5).

    IF  XITEM-DIST+0(3)  <> 'B28'.
      CLEAR: XSDATE.
      PERFORM GETSINGLE_ATWRT USING :
      XVBRP-VGBEL XITEM-MSDATE    'P_RP27_SHOP_DATE',
      XVBRP-VGBEL XSDATE          'P_RP27_ACTUAL_DATE'.
      IF XITEM-MSDATE IS INITIAL OR XSDATE IS INITIAL.
        PERFORM GETSINGLE_ATWRT USING :
        XVBRP-VGBEL XITEM-MSDATE    'P_RP25_SHOP_DATE',
        XVBRP-VGBEL XSDATE          'P_RP25_ACTUAL_DATE'.
      ENDIF.
    ELSE.
      PERFORM GETSINGLE_ATWRT USING :
      XVBRP-VGBEL XITEM-MSDATE       'P_RP23_SHOP_DATE',
      XVBRP-VGBEL XSDATE         'P_RP23_ACTUAL_DATE'.
    ENDIF.

** End on 08/06/12

    XITEM-MDATE = XSDATE+0(8).
    XITEM-MZEIT = XSDATE+8(6).

    XITEM-SNDT = SY-DATUM.
*-<      Color conversion  2 -> 3 digit Victor 02.17.2012
    IF  XITEM-DIST+0(3)  <> 'B28'.
      LV_MODEL  =  XVBRP-MDCD+0(2).

      SELECT SINGLE MODEL_YEAR INTO LV_MODEL_YEAR
      FROM ZTPP_VM
      WHERE MODEL_CODE   = XVBRP-MDCD
        AND BODY_NO      = XVBRP-BDNO.

      CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
        EXPORTING
          I_MODEL = LV_MODEL
          I_YEAR  = LV_MODEL_YEAR
          I_GUBN  = ''            "HMMA -> HAC/HMM
          I_EXTC  = XITEM-EXCL
          I_INTC  = XITEM-INCL
        IMPORTING
          E_EXTC  = XITEM-EXCL
          E_INTC  = XITEM-INCL.
    ENDIF.
*->

    APPEND XITEM.
*****
* For IDOC
*****
    XEDIDD-SEGNAM = 'ZINVSEG2'. "segment lineitem
    XEDIDD-SDATA  = XITEM   .

    APPEND XEDIDD TO INT_EDIDD .

  ENDLOOP.

*--Victor 02.08.2012
  PERFORM MODIFY_CONTROL_RECORD USING   XITEM-DIST
                                CHANGING CONTROL_RECORD_OUT.

* Object type
* PERFORM OBJECT_TYPE_DETERMINE_DELIVERY.

*{ALE Begin} generation http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments. PA8 20050511
  CALL FUNCTION 'MGV_ALE_ADD_EXTERNAL_MATNR'
    TABLES
      IDOC_DATA   = INT_EDIDD
    CHANGING
      IDOC_HEADER = CONTROL_RECORD_OUT.
*{ALE End} generation
ENDFUNCTION.
