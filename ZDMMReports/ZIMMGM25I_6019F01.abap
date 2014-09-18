*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM25I_6019F01                                          *
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
  DATA: LT_ZTMM_6019_01 LIKE IT_ZTMM_6019_01.

*/Begin of Added by Hakchin(Purpose of All Material_HS
* Transfer)(20040304)
  IF P_ALL = 'X'.
    SELECT MATNR     AS MATNR
*         MAX( udate ) AS udate
*         MAX( utime ) AS utime
      INTO CORRESPONDING FIELDS OF TABLE LT_ZTMM_6019_01
      FROM MARA
      WHERE LVORM = SPACE.   "Flag Material for Deletion at Client Level

    LOOP AT LT_ZTMM_6019_01 ASSIGNING <FS_ZTMM_6019_01>.
      <FS_ZTMM_6019_01>-UDATE = SY-DATUM.
      <FS_ZTMM_6019_01>-UTIME = SY-UZEIT.
    ENDLOOP.
*/End of Added by Hakchin(Purpose of All Material_HS Transfer)(20040304)
  ELSE.
*/ Get data for Material data
*  related to last changed(including creation)
    SELECT OBJECTID     AS MATNR
           MAX( UDATE ) AS UDATE
           MAX( UTIME ) AS UTIME
      INTO CORRESPONDING FIELDS OF TABLE LT_ZTMM_6019_01
      FROM CDHDR
    WHERE OBJECTCLAS = 'MATERIAL' AND
          UDATE IN S_UDATE
    GROUP BY OBJECTID.

*/ Begin of Added by Hakchin(20040529)
*  related to last changed(including creation)(Info Record)
    SELECT EINA~MATNR   AS MATNR
           MAX( UDATE ) AS UDATE
           MAX( UTIME ) AS UTIME
      APPENDING CORRESPONDING FIELDS OF TABLE LT_ZTMM_6019_01
      FROM CDHDR
        INNER JOIN EINA
        ON EINA~INFNR = CDHDR~OBJECTID
    WHERE CDHDR~OBJECTCLAS = 'INFOSATZ' AND
          CDHDR~UDATE IN S_UDATE
    GROUP BY EINA~MATNR.
*  Delete Duplicate data
    SORT LT_ZTMM_6019_01 BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_ZTMM_6019_01
                               COMPARING MATNR.
*/ End of Added by Hakchin(20040529)
  ENDIF.

*/ Get data for Material Master Interface(Outbound)
  LOOP AT LT_ZTMM_6019_01 ASSIGNING <FS_ZTMM_6019_01>.
    CLEAR: WA_ZTMM_6019_01.
    SELECT SINGLE
           MAKT~MAKTX    "Material description
           MARA~PROFL    "MIP/LP/KD
           MARA~MTART    "Material type
      INTO CORRESPONDING FIELDS OF WA_ZTMM_6019_01
      FROM MARA
        INNER JOIN MAKT
        ON MAKT~MATNR = MARA~MATNR AND
           MAKT~SPRAS = SY-LANGU
      WHERE MARA~MATNR = <FS_ZTMM_6019_01>-MATNR  AND
            MARA~LVORM = SPACE                    AND
            "Flag Material for Deletion at Client Level
*/Begin of Commented by Hakchin(20040129)
*            mara~profl      ='K'. "KD material
*/End of Commented by Hakchin(20040129)
*/Begin of Added by Hakchin(20040202)
          (
            ( MARA~MTART = 'HALB'    AND   "Semifinished products
              MARA~MATKL IN ('FSC','BIP','BIW','A/S') AND
              ( MARA~PROFL = 'M' OR  "MIP
                MARA~PROFL = 'K'     "KD
              )
            )                            OR
            MARA~MTART = 'FERT'          OR   "Finished products
*            mara~mtart = 'ROH'           OR   "Production Materials
           ( MARA~MTART = 'ROH' AND MARA~MSTAE = '12' ) OR
           ( MARA~MTART = 'ROH' AND MARA~MSTAE = '13' ) OR
*            mara~mtart = 'ROH1'
           ( MARA~MTART = 'ROH1' AND MARA~MSTAE = '12' ) OR
           ( MARA~MTART = 'ROH1' AND MARA~MSTAE = '13' )
          ).       "Raw / Sub Material
*/End of Added by Hakchin(20040202)

    IF SY-SUBRC = 0.
      <FS_ZTMM_6019_01>-MAKTX = WA_ZTMM_6019_01-MAKTX.
      <FS_ZTMM_6019_01>-PROFL = WA_ZTMM_6019_01-PROFL.
      <FS_ZTMM_6019_01>-MTART = WA_ZTMM_6019_01-MTART. "Material type
      CONCATENATE <FS_ZTMM_6019_01>-UDATE
                  'T'
                  <FS_ZTMM_6019_01>-UTIME
                  INTO <FS_ZTMM_6019_01>-DATE_TIME.

*/ Begin of Added by Hakchin(20040420)
*Get Valid Vendor Code from Soruce List
      PERFORM GET_LIFNR_FR_SOURCELIST
                        USING    <FS_ZTMM_6019_01>-MATNR
                                 <FS_ZTMM_6019_01>-UDATE
                        CHANGING <FS_ZTMM_6019_01>-LIFNR.
*/ End of Added by Hakchin(20040420)

*/ Get Net Price, Effective Price and Currency from Info Record
      IF NOT <FS_ZTMM_6019_01>-LIFNR IS INITIAL.
        PERFORM GET_INFORECORD_DATA
                   USING    <FS_ZTMM_6019_01>-MATNR "Material
                            <FS_ZTMM_6019_01>-LIFNR "Vendor code
                   CHANGING <FS_ZTMM_6019_01>-NETPR
                            <FS_ZTMM_6019_01>-EFFPR
                            <FS_ZTMM_6019_01>-WAERS
                            <FS_ZTMM_6019_01>-BPUMZ
                            <FS_ZTMM_6019_01>-BPUMN
                            <FS_ZTMM_6019_01>-PEINH.
      ELSE.
        PERFORM GET_NETPR_EFFPR_WAERS
                   USING    <FS_ZTMM_6019_01>-MATNR
                   CHANGING <FS_ZTMM_6019_01>-LIFNR "Vendor code
                            <FS_ZTMM_6019_01>-NETPR
                            <FS_ZTMM_6019_01>-EFFPR
                            <FS_ZTMM_6019_01>-WAERS
                            <FS_ZTMM_6019_01>-BPUMZ
                            <FS_ZTMM_6019_01>-BPUMN
                            <FS_ZTMM_6019_01>-PEINH."Price Unit
      ENDIF.
*/Begin of Added by Hakchin(20040407)
*Adjusted Price
      IF NOT <FS_ZTMM_6019_01>-BPUMN IS INITIAL.
        <FS_ZTMM_6019_01>-NETPRUOM =
        ( <FS_ZTMM_6019_01>-NETPR * <FS_ZTMM_6019_01>-BPUMZ ) /
         <FS_ZTMM_6019_01>-BPUMN.

        <FS_ZTMM_6019_01>-EFFPRUOM =
        ( <FS_ZTMM_6019_01>-EFFPR * <FS_ZTMM_6019_01>-BPUMZ ) /
        <FS_ZTMM_6019_01>-BPUMN.

        IF NOT <FS_ZTMM_6019_01>-PEINH IS INITIAL.
          <FS_ZTMM_6019_01>-NETPRUOM = <FS_ZTMM_6019_01>-NETPRUOM /
                                       <FS_ZTMM_6019_01>-PEINH.

          <FS_ZTMM_6019_01>-EFFPRUOM = <FS_ZTMM_6019_01>-EFFPRUOM /
                                       <FS_ZTMM_6019_01>-PEINH.
        ENDIF.
      ENDIF.
*/End of Added by Hakchin(20040407)


*/When MTART = FERT OR HALB, There is no Net Price.
* So we get standard price instead of net price.
      IF <FS_ZTMM_6019_01>-MTART = 'FERT' OR
         ( <FS_ZTMM_6019_01>-MTART = 'HALB' AND
           <FS_ZTMM_6019_01>-PROFL <> 'K'
         ).

        PERFORM GET_STANDARDPRICE
                  USING    <FS_ZTMM_6019_01>-MATNR
                           SPACE
                  CHANGING <FS_ZTMM_6019_01>-NETPR
                           <FS_ZTMM_6019_01>-WAERS.
        MOVE: <FS_ZTMM_6019_01>-NETPR TO <FS_ZTMM_6019_01>-NETPRUOM.
      ENDIF.


*/ Get HS code
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
      PERFORM GET_HSCODE
                USING    <FS_ZTMM_6019_01>-MATNR
                CHANGING <FS_ZTMM_6019_01>-STAWN. "HS code

*/Begin of Added by Hakchin(20040306) (Need of KBLEE)
* Production Type Code
      IF <FS_ZTMM_6019_01>-MTART = 'FERT' OR
         <FS_ZTMM_6019_01>-MTART = 'HALB'.
** Chnaged by Furong on 02/08/08 UD1K942844
*        <FS_ZTMM_6019_01>-PTC = 'IM'.  "Inventoried manufactured part
        <FS_ZTMM_6019_01>-PTC = 'NM'.  "Inventoried manufactured part
** End of change
      ENDIF.

      IF <FS_ZTMM_6019_01>-PROFL = 'K' OR
         <FS_ZTMM_6019_01>-PROFL = 'V'.
        <FS_ZTMM_6019_01>-PTC = 'PC'.  "Purchase component
      ELSEIF <FS_ZTMM_6019_01>-PROFL = 'M'.
** Chnaged by Furong on 02/08/08 UD1K942844
*        <FS_ZTMM_6019_01>-PTC = 'IM'.  "Inventoried manufactured part
        <FS_ZTMM_6019_01>-PTC = 'NM'.  "Inventoried manufactured part
** End of change
      ENDIF.
*/End of Added by Hakchin(20040129) (Need of KBLEE)

*/Get Vendor Info
      IF NOT <FS_ZTMM_6019_01>-LIFNR IS INITIAL.
        PERFORM GET_VENDORINFO
                        USING    <FS_ZTMM_6019_01>-LIFNR
                        CHANGING <FS_ZTMM_6019_01>-NAME1  "Vendor name
                                 <FS_ZTMM_6019_01>-LAND1  "Country Key
                                 <FS_ZTMM_6019_01>-STREET "Street
                                 <FS_ZTMM_6019_01>-ORT01  "City
                                 <FS_ZTMM_6019_01>-REGIO  "Region
                                 <FS_ZTMM_6019_01>-PSTLZ. "Postal Code
      ENDIF.

*/ Country Refine
** Chnaged by Furong on 02/08/08 UD1K942844
*      IF <FS_ZTMM_6019_01>-PTC = 'IM'.   "MIP Mat
      IF <FS_ZTMM_6019_01>-PTC = 'NM'.   "MIP Mat
** End of change
        <FS_ZTMM_6019_01>-LAND1 = 'US'. "Country Key
      ENDIF.

*/StatusCode
      PERFORM GETSTATUSCODE.

    ENDIF.
  ENDLOOP.
*/ Delete useless data.
  IF NOT LT_ZTMM_6019_01 IS INITIAL.
*1.
    DELETE LT_ZTMM_6019_01 WHERE DATE_TIME = SPACE. "i.e. Garbage Data
**2. Exclude material that is marc-dispo = 'M02'.
*    DATA: lv_dispo LIKE marc-dispo.  "MRP Controller
*    CLEAR: it_ztmm_6019_01.
*    LOOP AT lt_ztmm_6019_01 ASSIGNING <fs_ztmm_6019_01>.
*      SELECT SINGLE dispo
*        INTO lv_dispo
*        FROM marc
*        WHERE matnr = <fs_ztmm_6019_01>-matnr AND
*              dispo = 'M02'.
*      IF sy-subrc <> 0.
*        APPEND <fs_ztmm_6019_01> TO it_ztmm_6019_01.
*      ENDIF.
*    ENDLOOP.
    IT_ZTMM_6019_01 = LT_ZTMM_6019_01.  "Need by KBLEE 20040414



*3.
*/Begin of Added by Hakchin(20040412)
*Delete Color
    PERFORM DELETE_COLOR
           USING IT_ZTMM_6019_01.

    SORT IT_ZTMM_6019_01 BY MATNR.
    DELETE ADJACENT DUPLICATES FROM IT_ZTMM_6019_01
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
*      <--P_WA_ZTMM_6019_01_LOGNO_H  text
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
  CALL FUNCTION 'Z_FMM_6019_OUT_MATHS'
    DESTINATION              C_DEST
    TABLES
      EXT_ZTMM_6019_01      = IT_ZTMM_6019_01
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

*/ Modify it_ZTMM_6019_01
* App. Doc. No.
  PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09
                                   'ZMMNRO0002'
                          CHANGING W_ZDOCNO.
  COMMIT WORK.

  LOOP AT IT_ZTMM_6019_01 ASSIGNING <FS_ZTMM_6019_01>.
    PERFORM NUMBER_GET_NEXT USING    '00'
                                     'ZMMNRO0002'
                            CHANGING LV_LOGNO_H.
    <FS_ZTMM_6019_01>-ZDOCNO  = W_ZDOCNO.  "App. Doc. No.
    <FS_ZTMM_6019_01>-LOGNO_H = LV_LOGNO_H."Logno Header

    <FS_ZTMM_6019_01>-ZUSER   = SY-UNAME.  "User name
*    <fs_ZTMM_6019_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6019_01>-zstim   = .  "Send file Created Time
    <FS_ZTMM_6019_01>-ZEDAT   = SY-DATUM.  "SAP Interface Date
    <FS_ZTMM_6019_01>-ZETIM   = SY-UZEIT.  "SAP Interface Time
    <FS_ZTMM_6019_01>-ZMODE   = 'C'.       "Data Characteristic Flag
    <FS_ZTMM_6019_01>-ZRESULT = LV_ZRESULT."Result of the Processing
    <FS_ZTMM_6019_01>-ZMSG    = LV_MESSAGE."Message text
*    <fs_ZTMM_6019_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6019_01.
  INSERT ZTMM_6019_01 FROM TABLE IT_ZTMM_6019_01.
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
  DESCRIBE TABLE IT_ZTMM_6019_01 LINES LV_TOTAL.

  CHECK NOT LV_TOTAL IS INITIAL.
  CLEAR: WA_ZTCA_IF_LOG.
  LOOP AT IT_ZTMM_6019_01 ASSIGNING <FS_ZTMM_6019_01>.
    IF <FS_ZTMM_6019_01>-ZZRET = 'S'.
      WA_ZTCA_IF_LOG-ZSUCC = WA_ZTCA_IF_LOG-ZSUCC + 1.
    ELSEIF <FS_ZTMM_6019_01>-ZZRET = 'E'.
      WA_ZTCA_IF_LOG-ERROR = WA_ZTCA_IF_LOG-ERROR + 1.
    ENDIF.
  ENDLOOP.

  WA_ZTCA_IF_LOG-TCODE = 'ZMMI72'. "Present Transaction Code
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
* Build the fieldcat according to DDIC structure ZTMM_6019_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZTMM_6019_01'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZDOCNO'.
      P_IT_FIELDCAT-COLTEXT = 'App.DocNo.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LOGNO_H'.
      P_IT_FIELDCAT-COLTEXT = 'Log No.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSDAT'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSTIM'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MATNR'.
      P_IT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'STATUSCODE'.
      P_IT_FIELDCAT-COLTEXT = 'StatusCode'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NETPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'NetPr UOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EFFPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'EffPr UOM'.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY P_IT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  get_netpr_waers
*&---------------------------------------------------------------------*
FORM GET_NETPR_EFFPR_WAERS USING    VALUE(IM_MATNR)
                     CHANGING VALUE(EX_LIFNR)
                              VALUE(EX_NETPR)
                              VALUE(EX_EFFPR)
                              VALUE(EX_WAERS)
                              VALUE(EX_BPUMZ)
                              VALUE(EX_BPUMN)
                              VALUE(EX_PEINH). "Price Unit
  CLEAR: EX_LIFNR, EX_NETPR, EX_EFFPR, EX_WAERS,
         EX_BPUMZ, EX_BPUMN, EX_PEINH.

** Changed by Furong pn 02/08/08
  DATA: L_MEINS LIKE EINA-MEINS,
        L_LMEIN LIKE EINA-LMEIN,
        L_UMREZ LIKE EINA-UMREZ,
        L_UMREN LIKE EINA-UMREN.


*  SELECT SINGLE EINA~LIFNR EINE~NETPR EINE~EFFPR EINE~WAERS
*                EINE~BPUMZ EINE~BPUMN EINE~PEINH
*    INTO (EX_LIFNR, EX_NETPR, EX_EFFPR, EX_WAERS,
*          EX_BPUMZ, EX_BPUMN, EX_PEINH)
*    FROM EINA
*      INNER JOIN EINE
*      ON EINE~INFNR = EINA~INFNR   "Info Record Number
*  WHERE EINA~MATNR = IM_MATNR.

  SELECT SINGLE EINA~LIFNR EINE~NETPR EINE~EFFPR EINE~WAERS
                EINE~BPUMZ EINE~BPUMN EINE~PEINH
                 EINA~MEINS EINA~LMEIN EINA~UMREZ
                EINA~UMREN
    INTO (EX_LIFNR, EX_NETPR, EX_EFFPR, EX_WAERS,
          EX_BPUMZ, EX_BPUMN, EX_PEINH,
           L_MEINS, L_LMEIN, L_UMREZ, L_UMREN)
    FROM EINA
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Info Record Number
  WHERE EINA~MATNR = IM_MATNR.

  IF  L_MEINS NE L_LMEIN.
    EX_NETPR = ( EX_NETPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
  ENDIF.





ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_hscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      <--P_<FS_ZTMM_6019_01>_STAWN  text
*----------------------------------------------------------------------*
FORM GET_HSCODE
        USING    VALUE(IM_MATNR)
        CHANGING VALUE(EX_STAWN) LIKE MARC-STAWN.
  CLEAR: EX_STAWN.
  SELECT SINGLE STAWN
    INTO EX_STAWN
    FROM MARC
    WHERE MATNR = IM_MATNR AND
          WERKS = 'P001'.
  IF SY-SUBRC <> 0.
    SELECT SINGLE STAWN
      INTO EX_STAWN
      FROM MARC
      WHERE MATNR = IM_MATNR.
  ENDIF.
  IF EX_STAWN IS INITIAL. EX_STAWN = '0000.00.0000'. ENDIF.
ENDFORM.                    " get_hscode
*&---------------------------------------------------------------------*
*&      Form  get_vendorinfo
*&---------------------------------------------------------------------*
FORM GET_VENDORINFO USING    VALUE(IM_LIFNR)
                    CHANGING EX_NAME1
                             EX_LAND1
                             EX_STREET
                             EX_ORT01
                             EX_REGIO
                             EX_PSTLZ.
  DATA: W_NAME_CO    LIKE ADRC-NAME_CO,
        W_STR_SUPPL1 LIKE ADRC-STR_SUPPL1,
        W_STR_SUPPL2 LIKE ADRC-STR_SUPPL2.

  SELECT SINGLE
         LFA1~NAME1                                         "Name 1
         LFA1~LAND1    "Country
         ADRC~NAME_CO   "Street
         ADRC~STR_SUPPL1   "City
         ADRC~STR_SUPPL2   "Region (State, Province, County)
    INTO (EX_NAME1, EX_LAND1, W_NAME_CO,
          W_STR_SUPPL1, W_STR_SUPPL2)
    FROM LFA1
      INNER JOIN ADRC
      ON ADRC~ADDRNUMBER = LFA1~ADRNR
    WHERE LFA1~LIFNR      = IM_LIFNR.

  EX_STREET = W_NAME_CO.
  EX_ORT01  = W_STR_SUPPL1.
  EX_REGIO  = W_STR_SUPPL2(2).
  EX_PSTLZ  = W_STR_SUPPL2+3(5).

  CLEAR: W_NAME_CO, W_STR_SUPPL1, W_STR_SUPPL2.

ENDFORM.                " get_vendorinfo
*&---------------------------------------------------------------------*
*&      Form  getstatuscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETSTATUSCODE.
*/ Get HS code Rate  (You can view the data from /nMEK3)
*1.
  DATA: LV_KNUMH LIKE KONP-KNUMH.
  SELECT SINGLE KNUMH INTO LV_KNUMH
    FROM A902
    WHERE KAPPL = 'M'    AND
          KSCHL = 'ZOA1' AND
          STAWN = <FS_ZTMM_6019_01>-STAWN.
*2.
  SELECT SINGLE KBETR KONWA
    INTO (<FS_ZTMM_6019_01>-KBETR, <FS_ZTMM_6019_01>-KONWA)
    FROM KONP
    WHERE KNUMH = LV_KNUMH.
  <FS_ZTMM_6019_01>-KBETR = <FS_ZTMM_6019_01>-KBETR / 10.

*/ StatusCode
  IF <FS_ZTMM_6019_01>-PROFL = 'K'.  "KD Material
** changed by Furong on 09/24/09 ' 79LF381293   UD1K941653
*    IF <fs_ztmm_6019_01>-kbetr =< '2.5'.
    IF <FS_ZTMM_6019_01>-KBETR < '2.5'.
** end of change
      <FS_ZTMM_6019_01>-STATUSCODE = 'P'.
    ELSE.
      <FS_ZTMM_6019_01>-STATUSCODE = 'N'.
    ENDIF.
  ELSEIF <FS_ZTMM_6019_01>-PROFL = 'V'.  "Local Part
    <FS_ZTMM_6019_01>-STATUSCODE = 'D'.
  ELSEIF <FS_ZTMM_6019_01>-PTC = 'IM'.   "
    <FS_ZTMM_6019_01>-STATUSCODE = 'F'.
  ENDIF.
*/ StatusCode Refine*/
  IF <FS_ZTMM_6019_01>-MTART EQ 'ROH' OR
                                <FS_ZTMM_6019_01>-MTART EQ 'ROH1'.
    IF <FS_ZTMM_6019_01>-LAND1 = 'US' OR  "USA
       <FS_ZTMM_6019_01>-LAND1 = 'CA' OR  "CANADA
       <FS_ZTMM_6019_01>-LAND1 = 'MX'.    "MEXICO
      <FS_ZTMM_6019_01>-STATUSCODE = 'D'. "Domestic
    ENDIF.
  ENDIF.
ENDFORM.                    " getstatuscode
*&---------------------------------------------------------------------*
*&      Form  get_standardprice
*&---------------------------------------------------------------------*
* Material Master(MM03) -> Accounting1 -> Standard Price
*----------------------------------------------------------------------*
FORM GET_STANDARDPRICE
          USING    VALUE(IM_MATNR) LIKE MBEW-MATNR  "Material
                   VALUE(IM_BWKEY) LIKE MBEW-BWKEY  "Valuation area
          CHANGING VALUE(EX_STPRS) LIKE MBEW-STPRS  "Standard Price
                   VALUE(EX_WAERS) LIKE T001-WAERS. "Currency Key
  CLEAR: EX_STPRS, EX_WAERS.
  IF IM_BWKEY IS INITIAL.
    SELECT SINGLE WERKS
      INTO IM_BWKEY
      FROM MARC
      WHERE MATNR = IM_MATNR.
  ENDIF.

  SELECT SINGLE STPRS  "Standard Price
    INTO EX_STPRS
    FROM MBEW    "Material Valuation
    WHERE MATNR = IM_MATNR AND  "Material
          BWKEY = IM_BWKEY.     "Valuation area

  SELECT SINGLE WAERS  "Currency Key
    INTO EX_WAERS
    FROM T001    "Company Codes
    WHERE BUKRS = 'H201'.    "Company Code
ENDFORM.                    "get_standardprice
*&---------------------------------------------------------------------*
*&      Form  delete_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6019_01  text
*----------------------------------------------------------------------*
FORM DELETE_COLOR
     USING IET_ITABLE LIKE IT_ZTMM_6019_01.
  FIELD-SYMBOLS: <LF_ITABLE> LIKE LINE OF IET_ITABLE.
  DATA: LV_STRLEN TYPE I.
  DATA: W_NCHAR TYPE I.
  LOOP AT IET_ITABLE ASSIGNING <LF_ITABLE>.
    CLEAR: LV_STRLEN.
    LV_STRLEN = STRLEN( <LF_ITABLE>-MATNR ).

** Changed by Furong on 11/15/07
*    IF ( lv_strlen = 12 OR lv_strlen = 13 ) AND
*      <lf_itable>-mtart = 'ROH'             AND
*      <lf_itable>-matnr(1) <> 'B'. "Blank Mat
***Get Color
**      DATA: lv_rearcharacters_no TYPE i.
**      lv_rearcharacters_no = lv_strlen - 10.
**      PERFORM get_rearcharacters USING    <lf_itable>-matnr
**                                          lv_rearcharacters_no
**                                 CHANGING <lf_itable>-color.
**Delete Color
*      <lf_itable>-matnr = <lf_itable>-matnr(10).
*    ENDIF.


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
*&      Form  get_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_UDATE  text
*      <--P_<FS_ZTMM_6019_01>_LIFNR  text
*----------------------------------------------------------------------*
FORM GET_LIFNR_FR_SOURCELIST
               USING    VALUE(IM_MATNR)
                        VALUE(IM_DATE)
               CHANGING VALUE(EX_LIFNR).
  " If Material type = 'ROH' or 'ROH1',
  " Source List exists.

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
*&      Form  get_inforecord_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_LIFNR  text
*      <--P_<FS_ZTMM_6019_01>_NETPR  text
*      <--P_<FS_ZTMM_6019_01>_EFFPR  text
*      <--P_<FS_ZTMM_6019_01>_WAERS  text
*      <--P_<FS_ZTMM_6019_01>_BPUMZ  text
*      <--P_<FS_ZTMM_6019_01>_BPUMN  text
*----------------------------------------------------------------------*
FORM GET_INFORECORD_DATA
                     USING    VALUE(IM_MATNR)
                              VALUE(IM_LIFNR)
                     CHANGING VALUE(EX_NETPR)
                              VALUE(EX_EFFPR)
                              VALUE(EX_WAERS)
                              VALUE(EX_BPUMZ)
                              VALUE(EX_BPUMN)
                              VALUE(EX_PEINH). "Price Unit
  CLEAR: EX_NETPR, EX_EFFPR, EX_WAERS,
         EX_BPUMZ, EX_BPUMN, EX_PEINH.
** Changed by Furong pn 02/08/08
  DATA: L_MEINS LIKE EINA-MEINS,
        L_LMEIN LIKE EINA-LMEIN,
        L_UMREZ LIKE EINA-UMREZ,
        L_UMREN LIKE EINA-UMREN.

  DATA: L_KNUMH LIKE KONP-KNUMH.

  DATA: BEGIN OF LT_DATAB OCCURS 0,
        DATAB LIKE A018-DATAB,
        KNUMH LIKE KONP-KNUMH,
        END OF LT_DATAB.

*  SELECT SINGLE EINE~NETPR EINE~EFFPR EINE~WAERS
*                EINE~BPUMZ EINE~BPUMN EINE~PEINH
*    INTO (EX_NETPR, EX_EFFPR, EX_WAERS,
*          EX_BPUMZ, EX_BPUMN, EX_PEINH)
*    FROM EINA
*      INNER JOIN EINE
*      ON EINE~INFNR = EINA~INFNR   "Info Record Number
*  WHERE EINA~MATNR = IM_MATNR AND
*        EINA~LIFNR = IM_LIFNR.

  SELECT SINGLE EINE~NETPR EINE~EFFPR EINE~WAERS
                EINE~BPUMZ EINE~BPUMN EINE~PEINH
                EINA~MEINS EINA~LMEIN EINA~UMREZ
                EINA~UMREN
    INTO (EX_NETPR, EX_EFFPR, EX_WAERS,
          EX_BPUMZ, EX_BPUMN, EX_PEINH,
          L_MEINS, L_LMEIN, L_UMREZ, L_UMREN)
    FROM EINA
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Info Record Number
  WHERE EINA~MATNR = IM_MATNR AND
        EINA~LIFNR = IM_LIFNR.

  IF  L_MEINS NE L_LMEIN.
    EX_NETPR = ( EX_NETPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
    EX_EFFPR = ( EX_EFFPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
  ENDIF.

** CHanged by Furong on 10/12/09 Requested by Prasad
  SELECT DATAB KNUMH INTO TABLE LT_DATAB
    FROM A018
    WHERE KSCHL = 'PB00'
      AND MATNR = IM_MATNR
      AND LIFNR = IM_LIFNR.

  SORT LT_DATAB BY DATAB DESCENDING.
  READ TABLE LT_DATAB INDEX 1.
  L_KNUMH = LT_DATAB-KNUMH.

  SELECT SINGLE KBETR KPEIN MEINS KMEIN KUMZA KUMNE
    INTO (EX_NETPR, EX_PEINH,  L_MEINS, L_LMEIN, L_UMREZ, L_UMREN)
    FROM KONP
    WHERE KNUMH = L_KNUMH
     AND KSCHL = 'PB00'.
  IF  L_MEINS NE L_LMEIN.
    EX_NETPR = ( EX_NETPR / EX_PEINH ) / ( L_UMREZ / L_UMREN ).
  ENDIF.
** End of change
ENDFORM.
