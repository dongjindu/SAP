*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM24I_6017F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LT_ZTMM_6017_01 LIKE IT_ZTMM_6017_01.

*/Begin of Added by Hakchin(Purpose of All Material Transfer)(20040304)
  IF P_ALL = 'X'.
    SELECT MATNR     AS MATNR
*         MAX( udate ) AS udate
*         MAX( utime ) AS utime
      INTO CORRESPONDING FIELDS OF TABLE LT_ZTMM_6017_01
      FROM MARA
      WHERE LVORM = SPACE.   "Flag Material for Deletion at Client Level

    LOOP AT LT_ZTMM_6017_01 ASSIGNING <FS_ZTMM_6017_01>.
      <FS_ZTMM_6017_01>-UDATE = SY-DATUM.
      <FS_ZTMM_6017_01>-UTIME = SY-UZEIT.
    ENDLOOP.
*/End of Added by Hakchin(Purpose of All Material Transfer)(20040304)
  ELSE.
*/ Get data for Material data
*  related to last changed(including creation)(Material Master)
    SELECT OBJECTID     AS MATNR
           MAX( UDATE ) AS UDATE
           MAX( UTIME ) AS UTIME
      INTO CORRESPONDING FIELDS OF TABLE LT_ZTMM_6017_01
      FROM CDHDR
    WHERE OBJECTCLAS = 'MATERIAL' AND
          UDATE IN S_UDATE
    GROUP BY OBJECTID.

*/ Begin of Added by Hakchin(20040529)
*  related to last changed(including creation)(Info Record)
    SELECT EINA~MATNR   AS MATNR
           MAX( UDATE ) AS UDATE
           MAX( UTIME ) AS UTIME
      APPENDING CORRESPONDING FIELDS OF TABLE LT_ZTMM_6017_01
      FROM CDHDR
        INNER JOIN EINA
        ON EINA~INFNR = CDHDR~OBJECTID
    WHERE CDHDR~OBJECTCLAS = 'INFOSATZ' AND
          CDHDR~UDATE IN S_UDATE
    GROUP BY EINA~MATNR.
*  Delete Duplicate data
    SORT LT_ZTMM_6017_01 BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_ZTMM_6017_01
                               COMPARING MATNR.
*/ End of Added by Hakchin(20040529)
  ENDIF.


*/ Get data for Material Master Interface(Outbound)
  LOOP AT LT_ZTMM_6017_01 ASSIGNING <FS_ZTMM_6017_01>.
    CLEAR: WA_ZTMM_6017_01.
    SELECT SINGLE
           MAKT~MAKTX    "Material description
           MARA~MEINS    "Base UoM
           MARA~NTGEW    "Net weight
           MARA~GEWEI    "Weight Unit
           MARA~PROFL    "MIP/LP/KD
           MARA~MTART    "Material type
      INTO CORRESPONDING FIELDS OF WA_ZTMM_6017_01
      FROM MARA
        INNER JOIN MAKT
        ON MAKT~MATNR = MARA~MATNR AND
           MAKT~SPRAS = SY-LANGU
    WHERE MARA~MATNR = <FS_ZTMM_6017_01>-MATNR AND
          MARA~LVORM = SPACE                   AND
          "Flag Material for Deletion at Client Level
*/Begin of Commented by Hakchin(20040129)
*          mara~profl      ='K'. "                AND "KD material
*/End of Commented by Hakchin(20040129)
*          ( mara~ersda = <fs_ztmm_6017_01>-udate OR   "Created on
*          mara~laeda = <fs_ztmm_6017_01>-udate ).     "Last change
*/Begin of Added by Hakchin(20040129)
          (
            ( MARA~MTART = 'HALB'    AND   "Semifinished products
              MARA~MATKL IN ('FSC','BIP','BIW','A/S') AND   "UD1K919940
              ( MARA~PROFL = 'M' OR  "MIP
                MARA~PROFL = 'K'     "KD
              )
            )                             OR
            MARA~MTART = 'FERT' OR      "Finished products
*            mara~mtart = 'ROH'  OR      "Production Materials
           ( MARA~MTART = 'ROH' AND MARA~MSTAE = '12' ) OR
           ( MARA~MTART = 'ROH' AND MARA~MSTAE = '13' ) OR
*            mara~mtart = 'ROH1'         "Raw / Sub Material
           ( MARA~MTART = 'ROH1' AND MARA~MSTAE = '12' ) OR
           ( MARA~MTART = 'ROH1' AND MARA~MSTAE = '13' )
          ).
*/End of Added by Hakchin(20040129)
    IF SY-SUBRC = 0.
      <FS_ZTMM_6017_01>-MAKTX = WA_ZTMM_6017_01-MAKTX.
      <FS_ZTMM_6017_01>-MEINS = WA_ZTMM_6017_01-MEINS.
      <FS_ZTMM_6017_01>-NTGEW = WA_ZTMM_6017_01-NTGEW."Net weight
      <FS_ZTMM_6017_01>-GEWEI = WA_ZTMM_6017_01-GEWEI.
      <FS_ZTMM_6017_01>-MTART = WA_ZTMM_6017_01-MTART. "Material type
      <FS_ZTMM_6017_01>-PROFL = WA_ZTMM_6017_01-PROFL.
      CONCATENATE <FS_ZTMM_6017_01>-UDATE
                  'T'
                  <FS_ZTMM_6017_01>-UTIME
                  INTO <FS_ZTMM_6017_01>-DATE_TIME.

*/Begin of Added by Hakchin(20040129) (Need of KBLEE)
* Production Type Code
      IF <FS_ZTMM_6017_01>-MTART = 'FERT' OR
         <FS_ZTMM_6017_01>-MTART = 'HALB'.
* Begin of changes -  UD1K919940
*        <fs_ztmm_6017_01>-ptc = 'IM'.  "Inventoried manufactured part
        <FS_ZTMM_6017_01>-PTC = 'NM'.  "Inventoried manufactured part
* End  of changes -  UD1K919940
      ENDIF.
      IF <FS_ZTMM_6017_01>-PROFL = 'M'.
* Begin of changes -  UD1K919940
*        <fs_ztmm_6017_01>-ptc = 'IM'.  "Inventoried manufactured part
        <FS_ZTMM_6017_01>-PTC = 'NM'.  "Inventoried manufactured part
* End  of changes -  UD1K919940
      ENDIF.
      IF <FS_ZTMM_6017_01>-PROFL = 'K' OR
         <FS_ZTMM_6017_01>-PROFL = 'V'.
        <FS_ZTMM_6017_01>-PTC = 'PC'.  "Purchase component
      ENDIF.


*/ Begin of Added by Hakchin(20040420)
*Get Valid Vendor Code from Soruce List
      PERFORM GET_LIFNR_FR_SOURCELIST
                        USING    <FS_ZTMM_6017_01>-MATNR
                                 <FS_ZTMM_6017_01>-UDATE
                        CHANGING <FS_ZTMM_6017_01>-LIFNR.
*/ End of Added by Hakchin(20040420)

* Get Vendor,Country key, Net Price and Currency from Info Record
      IF NOT <FS_ZTMM_6017_01>-LIFNR IS INITIAL.
        PERFORM GET_DATA_FR_INFORECORD
          USING    <FS_ZTMM_6017_01>-MATNR  "Material
                   <FS_ZTMM_6017_01>-LIFNR  "Vendor
          CHANGING
                   <FS_ZTMM_6017_01>-INFNR  "InfoRecord
                   <FS_ZTMM_6017_01>-LAND1  "Country Key
                   <FS_ZTMM_6017_01>-NETPR
                     "Net price in pur. info record
                   <FS_ZTMM_6017_01>-WAERS  "Currency Key
                   <FS_ZTMM_6017_01>-BPUMZ
                   <FS_ZTMM_6017_01>-BPUMN
                   <FS_ZTMM_6017_01>-PEINH. "Price Unit
      ELSE.
        PERFORM GET_NETPR_WAERS
                   USING    <FS_ZTMM_6017_01>-MATNR
                   CHANGING <FS_ZTMM_6017_01>-LIFNR  "Vendor code
                            <FS_ZTMM_6017_01>-INFNR  "InfoRecord
                            <FS_ZTMM_6017_01>-NETPR
                            <FS_ZTMM_6017_01>-WAERS
                            <FS_ZTMM_6017_01>-BPUMZ
                            <FS_ZTMM_6017_01>-BPUMN
                            <FS_ZTMM_6017_01>-PEINH. "Price Unit
      ENDIF.
*/End of Added by Hakchin(20040129)

*/Begin of Added by Hakchin(20040407)
*Adjusted Price
      IF NOT <FS_ZTMM_6017_01>-BPUMN IS INITIAL.
        <FS_ZTMM_6017_01>-NETPRUOM =
           ( <FS_ZTMM_6017_01>-NETPR * <FS_ZTMM_6017_01>-BPUMZ ) /
           <FS_ZTMM_6017_01>-BPUMN.
        IF NOT <FS_ZTMM_6017_01>-PEINH IS INITIAL.
          <FS_ZTMM_6017_01>-NETPRUOM = <FS_ZTMM_6017_01>-NETPRUOM /
                                       <FS_ZTMM_6017_01>-PEINH.
        ENDIF.
      ENDIF.


*/End of Added by Hakchin(20040407)

    ENDIF.
  ENDLOOP.

*/ Delete useless data.
  IF NOT LT_ZTMM_6017_01 IS INITIAL.
*1.
    DELETE LT_ZTMM_6017_01 WHERE DATE_TIME = SPACE. "i.e. Garbage Data
**2. Exclude material that is marc-dispo = 'M02'.
*    DATA: lv_dispo LIKE marc-dispo.  "MRP Controller
*    CLEAR: it_ztmm_6017_01.
*    LOOP AT lt_ztmm_6017_01 ASSIGNING <fs_ztmm_6017_01>.
*      SELECT SINGLE dispo
*        INTO lv_dispo
*        FROM marc
*        WHERE matnr = <fs_ztmm_6017_01>-matnr AND
*              dispo = 'M02'.
*      IF sy-subrc <> 0.
*        APPEND <fs_ztmm_6017_01> TO it_ztmm_6017_01.
*      ENDIF.
*    ENDLOOP.

    IT_ZTMM_6017_01 = LT_ZTMM_6017_01.  "Need by KBLEE 20040414

*3.
*/Begin of Added by Hakchin(20040412)
*Delete Color
    PERFORM DELETE_COLOR
           USING IT_ZTMM_6017_01.

    SORT IT_ZTMM_6017_01 BY MATNR.
    DELETE ADJACENT DUPLICATES FROM IT_ZTMM_6017_01
                               COMPARING MATNR.
*/End of Added by Hakchin(20040412)

  ENDIF.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6017_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM NUMBER_GET_NEXT
           USING    VALUE(P_NRO_INTERVAL) LIKE INRI-NRRANGENR
                    VALUE(P_NRO_OBJECT)   LIKE INRI-OBJECT
           CHANGING VALUE(P_NRO_NEXT).
  CLEAR: P_NRO_NEXT.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = P_NRO_INTERVAL
            OBJECT                  = P_NRO_OBJECT
       IMPORTING
            NUMBER                  = P_NRO_NEXT
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            OTHERS                  = 7.
  IF SY-SUBRC <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: LV_LOGNO_H TYPE NUM10.
  DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
  DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
  CONSTANTS : C_DEST(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6017_OUT_MAT'
    DESTINATION              C_DEST
    TABLES
      EXT_ZTMM_6017_01      = IT_ZTMM_6017_01
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE LV_MESSAGE
      SYSTEM_FAILURE        = 2 MESSAGE LV_MESSAGE.
  IF SY-SUBRC NE 0.
    LV_ZRESULT = 'E'.  "Result of the Processing
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ELSE.
    LV_ZRESULT = 'S'.  "Result of the Processing
    LV_MESSAGE = 'Outbound RFC FM Connected!'(002).
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ENDIF.

*/ Modify it_ZTMM_6017_01
* App. Doc. No.
  PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09
                                   'ZMMNRO0002'
                          CHANGING W_ZDOCNO.
  COMMIT WORK.
  LOOP AT IT_ZTMM_6017_01 ASSIGNING <FS_ZTMM_6017_01>.
    PERFORM NUMBER_GET_NEXT USING    '00'
                                     'ZMMNRO0002'
                            CHANGING LV_LOGNO_H.
    <FS_ZTMM_6017_01>-ZDOCNO  = W_ZDOCNO.  "App. Doc. No.
    <FS_ZTMM_6017_01>-LOGNO_H = LV_LOGNO_H."Logno Header

    <FS_ZTMM_6017_01>-ZUSER   = SY-UNAME.  "User name
*    <fs_ZTMM_6017_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6017_01>-zstim   = .  "Send file Created Time
    <FS_ZTMM_6017_01>-ZEDAT   = SY-DATUM.  "SAP Interface Date
    <FS_ZTMM_6017_01>-ZETIM   = SY-UZEIT.  "SAP Interface Time
    <FS_ZTMM_6017_01>-ZMODE   = 'C'.       "Data Characteristic Flag
    <FS_ZTMM_6017_01>-ZRESULT = LV_ZRESULT."Result of the Processing
    <FS_ZTMM_6017_01>-ZMSG    = LV_MESSAGE."Message text
*    <fs_ZTMM_6017_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6017_01.
  INSERT ZTMM_6017_01 FROM TABLE IT_ZTMM_6017_01.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_FCA_EAI_INTERFACE_LOG.
*/ Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change

  DATA: LV_TOTAL TYPE I.
  DESCRIBE TABLE IT_ZTMM_6017_01 LINES LV_TOTAL.

  CHECK NOT LV_TOTAL IS INITIAL.
  CLEAR: WA_ZTCA_IF_LOG.
  LOOP AT IT_ZTMM_6017_01 ASSIGNING <FS_ZTMM_6017_01>.
    IF <FS_ZTMM_6017_01>-ZZRET = 'S'.
      WA_ZTCA_IF_LOG-ZSUCC = WA_ZTCA_IF_LOG-ZSUCC + 1.
    ELSEIF <FS_ZTMM_6017_01>-ZZRET = 'E'.
      WA_ZTCA_IF_LOG-ERROR = WA_ZTCA_IF_LOG-ERROR + 1.
    ENDIF.
  ENDLOOP.

  WA_ZTCA_IF_LOG-TCODE = 'ZMMI71'. "Present Transaction Code
  WA_ZTCA_IF_LOG-TOTAL = LV_TOTAL. "Total Execution number
  WA_ZTCA_IF_LOG-ERDAT = SY-DATUM. "Created on.
  WA_ZTCA_IF_LOG-ERZET = SY-UNAME. "Created time.
  WA_ZTCA_IF_LOG-ERNAM = SY-UNAME. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      I_ZTCA_IF_LOG     = WA_ZTCA_IF_LOG
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     UPDATE_FAILED              = 1
     NUMBER_RANGE_ERROR         = 2
     TCODE_DOES_NOT_EXIST       = 3
     OTHERS                     = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LOG.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS TABLES   P_IT_FIELDCAT STRUCTURE IT_FIELDCAT.
* Build the fieldcat according to DDIC structure ZTMM_6017_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZTMM_6017_01'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZDOCNO'.
      P_IT_FIELDCAT-COLTEXT = 'App.
                                   DOCNO.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LOGNO_H'.
      P_IT_FIELDCAT-COLTEXT = 'Log No.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSDAT'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSTIM'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PTC'. "Product Type Code
      P_IT_FIELDCAT-COLTEXT = 'Product Type Code'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NETPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'NetPr UOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EFFPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'EffPr UOM'.
*    ELSEIF p_IT_fieldcat-fieldname = 'MATNR'.
*      p_IT_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY P_IT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  get_data_fr_inforecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6017_01>_MATNR  text
*      <--P_<FS_ZTMM_6017_01>_INFNR  text
*      <--P_<FS_ZTMM_6017_01>_LIFNR  text
*      <--P_<FS_ZTMM_6017_01>_LAND1  text
*      <--P_<FS_ZTMM_6017_01>_NETPR  text
*      <--P_<FS_ZTMM_6017_01>_WAERS  text
*----------------------------------------------------------------------*
FORM GET_DATA_FR_INFORECORD
              USING    VALUE(IM_MATNR)
                       VALUE(IM_LIFNR)
              CHANGING VALUE(EX_INFNR) "InfoRecord
                       VALUE(EX_LAND1)
                       VALUE(EX_NETPR) "Net price in pur. info record
                       VALUE(EX_WAERS)
                       VALUE(EX_BPUMZ)
      "Numerator for conversion of order price unit into order unit
                       VALUE(EX_BPUMN)
  "Denominator for conv. of order price unit into order unit
                       VALUE(EX_PEINH).

  CLEAR: EX_INFNR, EX_LAND1, EX_NETPR, EX_WAERS,
         EX_BPUMZ, EX_BPUMN, EX_PEINH.

** Changed by Furong pn 02/08/08
* SELECT SINGLE
*      EINA~INFNR
*      LFA1~LAND1
*      EINE~NETPR
*      EINE~WAERS
*      EINE~BPUMZ
*      EINE~BPUMN
*      EINE~PEINH
*    INTO (EX_INFNR, EX_LAND1, EX_NETPR, EX_WAERS,
*          EX_BPUMZ, EX_BPUMN, EX_PEINH)
*    FROM EINA
*      INNER JOIN LFA1
*      ON LFA1~LIFNR = EINA~LIFNR "Vendor
*      INNER JOIN EINE
*      ON EINE~INFNR = EINA~INFNR   "Info Record Number
*  WHERE EINA~MATNR = IM_MATNR AND
*        EINA~LIFNR = IM_LIFNR.

 DATA: L_MEINS LIKE EINA-MEINS,
        L_LMEIN LIKE EINA-LMEIN,
        L_UMREZ LIKE EINA-UMREZ,
        L_UMREN LIKE EINA-UMREN.

 SELECT SINGLE
      EINA~INFNR
      LFA1~LAND1
      EINE~NETPR
      EINE~WAERS
      EINE~BPUMZ
      EINE~BPUMN
      EINE~PEINH
      EINA~MEINS EINA~LMEIN
      EINA~UMREZ EINA~UMREN
    INTO (EX_INFNR, EX_LAND1, EX_NETPR, EX_WAERS,
          EX_BPUMZ, EX_BPUMN, EX_PEINH,
          L_MEINS, L_LMEIN, L_UMREZ, L_UMREN)
    FROM EINA
      INNER JOIN LFA1
      ON LFA1~LIFNR = EINA~LIFNR "Vendor
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Info Record Number
  WHERE EINA~MATNR = IM_MATNR AND
        EINA~LIFNR = IM_LIFNR.

  IF  L_MEINS NE L_LMEIN.
    EX_NETPR = ( EX_NETPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
  ENDIF.

ENDFORM.                    "get_vendor_land1_fr_inforecord
*&---------------------------------------------------------------------*
*&      Form  delete_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6017_01  text
*----------------------------------------------------------------------*
FORM DELETE_COLOR
     USING IET_ITABLE LIKE IT_ZTMM_6017_01.
  FIELD-SYMBOLS: <LF_ITABLE> LIKE LINE OF IET_ITABLE.
  DATA: LV_STRLEN TYPE I.
  DATA: W_NCHAR TYPE I.

  LOOP AT IET_ITABLE ASSIGNING <LF_ITABLE>.
    CLEAR: LV_STRLEN.
    LV_STRLEN = STRLEN( <LF_ITABLE>-MATNR ).
*    IF ( LV_STRLEN = 12 OR LV_STRLEN = 13 ) AND
*      <lf_itable>-mtart = 'ROH'             AND
*      <lf_itable>-matnr(1) <> 'B'.          and "Blank Mat
*
***Get Color
**      DATA: lv_rearcharacters_no TYPE i.
**      lv_rearcharacters_no = lv_strlen - 10.
**      PERFORM get_rearcharacters USING    <lf_itable>-matnr
**                                          lv_rearcharacters_no
**                                 CHANGING <lf_itable>-color.
**Delete Color
*      <lf_itable>-matnr = <lf_itable>-matnr(10).
** Changed by Furong on 11/15/07
    IF <LF_ITABLE>-MATNR(1) NE 'Z'.
      IF <LF_ITABLE>-MATNR+0(2) = 'CR' OR <LF_ITABLE>-MATNR+0(2) = 'EM'.
        IF ( LV_STRLEN GT 13 AND <LF_ITABLE>-MTART = 'ROH' AND
                              <LF_ITABLE>-MATNR(1) <> 'B' ).
          W_NCHAR = LV_STRLEN - 12.

          <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR(12).
        ELSE.
          IF ( LV_STRLEN GT 11 AND <LF_ITABLE>-MTART = 'ROH' AND
                               <LF_ITABLE>-MATNR(1) <> 'B' ).
            IF <LF_ITABLE>-MATNR+5(2) <> 'M1'.
              W_NCHAR = LV_STRLEN - 10.

              <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR(10).
            ELSE.
              <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR.
            ENDIF.
          ELSE.
            <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR.
          ENDIF.
        ENDIF.
      ELSE.
        IF ( LV_STRLEN GT 11 AND <LF_ITABLE>-MTART = 'ROH' AND
                                 <LF_ITABLE>-MATNR(1) <> 'B' ).
          W_NCHAR = LV_STRLEN - 10.

          <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR(10).
        ELSE.
          <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR.
        ENDIF.
      ENDIF.
    ELSE.
      IF ( LV_STRLEN GT 12 AND <LF_ITABLE>-MTART = 'ROH' AND
                            <LF_ITABLE>-MATNR(2) <> 'B' ).
        W_NCHAR = LV_STRLEN - 11.

        <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR(11).
      ELSE.
        <LF_ITABLE>-MATNR = <LF_ITABLE>-MATNR.
      ENDIF.
    ENDIF.
*    ENDIF.
** END OF CHANGE
  ENDLOOP.
ENDFORM.                    " delete_color
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_fr_sourcelist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6017_01>_MATNR  text
*      -->P_<FS_ZTMM_6017_01>_UDATE  text
*      <--P_<FS_ZTMM_6017_01>_LIFNR  text
*----------------------------------------------------------------------*
FORM GET_LIFNR_FR_SOURCELIST
               USING    VALUE(IM_MATNR)
                        VALUE(IM_DATE)
               CHANGING VALUE(EX_LIFNR).
  CLEAR: EX_LIFNR.
  SELECT SINGLE LIFNR
    INTO EX_LIFNR
    FROM EORD
    WHERE MATNR = IM_MATNR AND
          VDATU =< IM_DATE AND
             "Source list record valid from
          BDATU => IM_DATE.
  "Source list record valid to
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_netpr_effpr_waers
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6017_01>_MATNR  text
*      <--P_<FS_ZTMM_6017_01>_LIFNR  text
*      <--P_<FS_ZTMM_6017_01>_INFNR  text
*      <--P_<FS_ZTMM_6017_01>_NETPR  text
*      <--P_<FS_ZTMM_6017_01>_WAERS  text
*      <--P_<FS_ZTMM_6017_01>_BPUMZ  text
*      <--P_<FS_ZTMM_6017_01>_BPUMN  text
*----------------------------------------------------------------------*
FORM GET_NETPR_WAERS USING    VALUE(IM_MATNR)
                     CHANGING VALUE(EX_LIFNR)
                              VALUE(EX_INFNR)  " Info Record
                              VALUE(EX_NETPR)
                              VALUE(EX_WAERS)
                              VALUE(EX_BPUMZ)
                              VALUE(EX_BPUMN)
                              VALUE(EX_PEINH).  " Price Unit
  CLEAR: EX_LIFNR, EX_INFNR, EX_NETPR, EX_WAERS,
         EX_BPUMZ, EX_BPUMN, EX_PEINH.
** Changd by Furong on 02/08/08
*SELECT SINGLE
*      EINA~LIFNR
*      EINA~INFNR
*      EINE~NETPR
*      EINE~WAERS
*      EINE~BPUMZ
*      EINE~BPUMN
*      EINE~PEINH    "Price Unit
*    INTO (EX_INFNR, EX_INFNR, EX_NETPR, EX_WAERS,
*          EX_BPUMZ, EX_BPUMN, EX_PEINH)
*    FROM EINA
*      INNER JOIN LFA1
*      ON LFA1~LIFNR = EINA~LIFNR "Vendor
*      INNER JOIN EINE
*      ON EINE~INFNR = EINA~INFNR   "Info Record Number
*  WHERE EINA~MATNR = IM_MATNR.

DATA: L_MEINS LIKE EINA-MEINS,
        L_LMEIN LIKE EINA-LMEIN,
        L_UMREZ LIKE EINA-UMREZ,
        L_UMREN LIKE EINA-UMREN.

SELECT SINGLE
      EINA~LIFNR
      EINA~INFNR
      EINE~NETPR
      EINE~WAERS
      EINE~BPUMZ
      EINE~BPUMN
      EINE~PEINH    "Price Unit
      EINA~MEINS EINA~LMEIN
      EINA~UMREZ EINA~UMREN
    INTO (EX_INFNR, EX_INFNR, EX_NETPR, EX_WAERS,
          EX_BPUMZ, EX_BPUMN, EX_PEINH,
          L_MEINS, L_LMEIN, L_UMREZ, L_UMREN)
    FROM EINA
      INNER JOIN LFA1
      ON LFA1~LIFNR = EINA~LIFNR "Vendor
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Info Record Number
  WHERE EINA~MATNR = IM_MATNR.

  IF  L_MEINS NE L_LMEIN.
    EX_NETPR = ( EX_NETPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
  ENDIF.
ENDFORM.
