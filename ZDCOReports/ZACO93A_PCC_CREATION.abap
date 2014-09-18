*&--------------------------------------------------------------------
*& REPORT                 : ZACO93A_PCC_CREATION
*& Author                 : WSKIM
*& Creation Date          : 01/24/2005
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : PCC CREATION
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
 REPORT  zaco93a_pcc_creation MESSAGE-ID  zmco.

*Tabls definition
 TABLES : mara,mkal.
*Internal table definition
 DATA : BEGIN OF it_fsc OCCURS 0,
          matnr LIKE mara-matnr,
          verid LIKE mkal-verid,
        END OF it_fsc.
 DATA : it_msg LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE.

*Data definition
 DATA : p_check,
        w_int TYPE i,
        l_flag TYPE c ,
        l_text LIKE makt-maktx,
        wa_subrc LIKE sy-subrc .

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
 SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* General Info.
 PARAMETERS : p_werks LIKE mkal-werks DEFAULT 'P001'.
 SELECT-OPTIONS : s_matnr FOR mara-matnr.
 SELECTION-SCREEN END OF BLOCK bl1.
*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
 INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
 START-OF-SELECTION.
   PERFORM get_fsc USING p_check.
   CHECK p_check EQ 'X'.
   PERFORM check_pcc_routine.
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
 END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GET_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM get_fsc USING p_check.
   REFRESH it_fsc.
   CLEAR p_check.
   SELECT a~matnr b~verid INTO (it_fsc-matnr,it_fsc-verid)
         FROM mara AS a INNER JOIN mkal AS b
          ON a~matnr = b~matnr
           WHERE b~werks EQ p_werks
             AND (   a~mtart EQ 'FERT' OR
                   ( a~mtart EQ 'HALB' AND
                     a~matkl IN ('BIP','BIW') ) )
             AND a~matnr IN s_matnr
             AND b~verid <> '0'.
     APPEND it_fsc.
   ENDSELECT.

   DESCRIBE TABLE it_fsc LINES w_int.
   IF w_int <> 0.
     p_check = 'X'.
   ENDIF.
 ENDFORM.                    " GET_FSC
*&---------------------------------------------------------------------*
*&      Form  check_pcc_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM check_pcc_routine.
   DATA : f_verid LIKE it_fsc-verid.
   CLEAR f_verid.
   LOOP AT it_fsc.
*    " Function Call for the PCC check...
     CLEAR: l_flag, f_verid.
     PERFORM check_pcc_function   USING
                  it_fsc-matnr  it_fsc-verid l_flag.
     CHECK l_flag = space OR l_flag = 'X' .
     CLEAR: l_text .
     CONCATENATE it_fsc-matnr it_fsc-verid    INTO l_text .
     f_verid = it_fsc-verid.
     PERFORM check_pcc_version USING it_fsc-matnr l_flag.
     PERFORM create_pcc USING it_fsc-matnr  p_werks
                              l_text   f_verid l_flag .

   ENDLOOP.

 ENDFORM.                    " check_pcc_routine
*&---------------------------------------------------------------------*
*&      Form  check_pcc_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_MATNR  text
*      -->P_IT_FSC_VERID  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
 FORM check_pcc_function USING    pa_matnr  pa_verid  pa_flag .
   DATA: lp_procnr        LIKE aufk-procnr,
         lp_verid         LIKE afpo-verid ,
         lp_stlan         LIKE mkal-stlan ,
         lp_stlal         LIKE mkal-stlal ,
         lp_plnty         LIKE plko-plnty ,
         lp_plnnr         LIKE plko-plnnr ,
         lp_plnal         LIKE plko-plnal ,
         lp_aufnr         LIKE aufk-aufnr ,
         lw_keko          LIKE keko       ,
         lt_afko          LIKE TABLE OF afko     WITH HEADER LINE,
        lt_vkks0         LIKE TABLE OF vkks0     WITH HEADER LINE,
        lt_pkosa         LIKE TABLE OF pkosa     WITH HEADER LINE.

   pa_flag = 'X'.

   CALL FUNCTION 'KK_F_PKOSA_FIND'
        EXPORTING
             i_matnr               = pa_matnr
             i_werks               = p_werks
             i_pwerk               = p_werks
             i_verid               = pa_verid
        IMPORTING
             e_procnr              = lp_procnr
             e_verid               = lp_verid
             e_stlan               = lp_stlan
             e_stlal               = lp_stlal
             e_plnty               = lp_plnty
             e_plnnr               = lp_plnnr
             e_plnal               = lp_plnal
             e_aufnr               = lp_aufnr
        TABLES
             e_vkks0               = lt_vkks0
             e_pkosa               = lt_pkosa
        EXCEPTIONS
             none_found            = 1
             wrong_input           = 2
             none_picked           = 3
             wrong_rule            = 4
             rsh_not_valid         = 5
             wrong_characteristics = 6
             no_rule               = 7
             version_not_valid     = 8
             OTHERS                = 9.

   CASE sy-subrc  .
     WHEN 0.
       pa_flag = 'S' .     " Can not call the PCC Function..
     WHEN 1.
       " Check the Standard Costing Estimate REsult...
       " If the Data is make without error... Continue...
       " Else Error...
       SELECT SINGLE * INTO lw_keko
         FROM keko
        WHERE matnr = pa_matnr
          AND tvers = '01'
          AND werks = p_werks
          AND kokrs = 'H201'
          AND feh_sta = 'FR'
          AND klvar = 'PPC1'
          AND kadat <= sy-datum
          AND bidat >= sy-datum .

       IF sy-subrc NE 0.
         " Display the Message...
         WRITE: / pa_matnr, '(', pa_verid, ')',  text-003.
         pa_flag = 'S'.  EXIT.
       ELSE. "Exist result
         pa_flag = ' '.
       ENDIF.
     WHEN 8.
       pa_flag = 'E' .     " Error - Un-Respected Scenario.
     WHEN OTHERS.
       pa_flag = 'E' .     " Error - Un-Respected Scenario.
   ENDCASE.

 ENDFORM.                    " check_pcc_function
*&---------------------------------------------------------------------*
*&      Form  create_pcc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_MATNR  text
*      -->P_P_WERKS  text
*      -->P_L_TEXT  text
*      -->P_IT_FSC_VERID  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
 FORM create_pcc USING pa_matnr  pa_werks  pa_text  pa_verid  pa_flag .
   DATA: l_matnr  LIKE bdcdata-fval,
         l_werks  LIKE bdcdata-fval,
         l_ktext  LIKE bdcdata-fval,
         l_verid  LIKE bdcdata-fval,
         l_flag   LIKE bdcdata-fval.



   l_matnr = pa_matnr.   l_werks = pa_werks.
   l_ktext = pa_text .   l_verid = pa_verid.

   CLEAR: wa_subrc, it_msg, it_msg[].
   CALL FUNCTION 'Z_FCO_PCC_ORDER_CRE_WITH_PDV'
        EXPORTING
             matnr_001 = l_matnr
             werks_002 = l_werks
             ktext_004 = l_ktext
             verid_007 = l_verid
             p_first   = pa_flag
        IMPORTING
             subrc     = wa_subrc
        TABLES
             messtab   = it_msg.

   READ TABLE it_msg WITH KEY msgtyp = 'E' .
   IF sy-subrc = 0.
     LOOP AT it_msg WHERE  msgspra EQ 'E'.
       MESSAGE i001 WITH text-002 '#18 PCC UPDATE ERROR'.
       WRITE: / it_msg                .
       EXIT.
     ENDLOOP.
   ELSE.
     WRITE: / 'Success: ', l_ktext .
   ENDIF.

 ENDFORM.                    " create_pcc
*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_MATNR  text
*----------------------------------------------------------------------*
 FORM check_pcc_version USING    p_fsc_matnr check_flag.
   DATA: lp_procnr        LIKE aufk-procnr,
         lp_verid         LIKE afpo-verid ,
         lp_stlan         LIKE mkal-stlan ,
         lp_stlal         LIKE mkal-stlal ,
         lp_plnty         LIKE plko-plnty ,
         lp_plnnr         LIKE plko-plnnr ,
         lp_plnal         LIKE plko-plnal ,
         lp_aufnr         LIKE aufk-aufnr ,
         lw_keko          LIKE keko       ,
         lt_afko          LIKE TABLE OF afko     WITH HEADER LINE,
        lt_vkks0         LIKE TABLE OF vkks0     WITH HEADER LINE,
        lt_pkosa         LIKE TABLE OF pkosa     WITH HEADER LINE.

   check_flag = 'X'.

   LOOP AT it_fsc WHERE matnr EQ p_fsc_matnr.

     CALL FUNCTION 'KK_F_PKOSA_FIND'
          EXPORTING
               i_matnr               = it_fsc-matnr
               i_werks               = p_werks
               i_pwerk               = p_werks
               i_verid               = it_fsc-verid
          IMPORTING
               e_procnr              = lp_procnr
               e_verid               = lp_verid
               e_stlan               = lp_stlan
               e_stlal               = lp_stlal
               e_plnty               = lp_plnty
               e_plnnr               = lp_plnnr
               e_plnal               = lp_plnal
               e_aufnr               = lp_aufnr
          TABLES
               e_vkks0               = lt_vkks0
               e_pkosa               = lt_pkosa
          EXCEPTIONS
               none_found            = 1
               wrong_input           = 2
               none_picked           = 3
               wrong_rule            = 4
               rsh_not_valid         = 5
               wrong_characteristics = 6
               no_rule               = 7
               version_not_valid     = 8
               OTHERS                = 9.

     IF sy-subrc = 0.
       check_flag = ' '.
       EXIT.
     ENDIF.
   ENDLOOP.

 ENDFORM.                    " CHECK_PCC_VERSION
