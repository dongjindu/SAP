*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_C01                                        *
*----------------------------------------------------------------------*
INCLUDE zrpp_func_alv.
*&---------------------------------------------------------------------*
*&      Form  1000_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM 1000_display_data .

*---------------------------------------------------------------


*---------------------------------------------------------------

*---------------------------------------------------------------

  CLEAR g_variant_s.
  g_variant_s-report = gv_repid = sy-cprog.

*---------------------------------------------------------------
  PERFORM set_sort
          USING: 'STATUS' 'GT_ITEM' 'X' 'X'  'X' ,
                 'HKMC' 'GT_ITEM' 'X' 'X'  'X' ,
                 'ZVIN'  'GT_ITEM' 'X' 'X'  'X' ,
                 'DISTCO'  'GT_ITEM' 'X' 'X'  'X',
                 'MODEL_CODE'   'GT_ITEM' 'X' 'X'  'X'.
*                 'BODY'   'GT_ITEM' 'X' 'X'  'X',
*                 'DESTN'   'GT_ITEM' 'X' 'X' 'X',
*                 'ZVIN'   'GT_ITEM' 'X' 'X'  'X'.


*---------------------------------------------------------------

  PERFORM set_layout
          USING 'X'
                ''
                ' '
                ''.
  g_layout_s-lights_tabname    = 'GT_ITEM'.
  g_layout_s-box_tabname       = 'GT_ITEM'.
*  G_LAYOUT_S-DEFAULT_ITEM      = 'X'.
  g_layout_s-expand_all        = space.
*  G_LAYOUT_S-EXPAND_FIELDNAME  = 'EXPAND'.
*  G_LAYOUT_S-DETAIL_POPUP      = 'X'.


*---------------------------------------------------------------

  PERFORM get_filedcatalog_alv.

*---------------------------------------------------------------

  PERFORM build_event
          USING : c_user_command,
                  c_status_set,
                  c_context_menu.
*                  C_TOP_OF_PAGE.

*---------------------------------------------------------------

  PERFORM local_usercommand
          USING sy-ucomm
                g_selfield.


*---------------------------------------------------------------

  PERFORM local_topofpage
            USING 'X' .

  PERFORM local_before_line
            USING gs_lineinfo
                  space.

  PERFORM local_after_line
          USING gs_lineinfo
                space.
  PERFORM local_pfstatus
          USING  g_extab[].


*---------------------------------------------------------------

  PERFORM display_grid_alv
  TABLES gt_item[].

*---------------------------------------------------------------


ENDFORM. " 1000_DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCATALOG_ALV
*&---------------------------------------------------------------------*

FORM get_filedcatalog_alv .


*_Çì´õ FIELDCAT ±¸¼º

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  PERFORM get_filedcatalog TABLES g_fieldcat_t
                           USING  'GT_ITEM'.

*



  LOOP AT g_fieldcat_t INTO ls_fieldcat.


    CASE ls_fieldcat-fieldname.
      WHEN 'STATUS'.
        ls_fieldcat-col_pos = 0.
        ls_fieldcat-icon = 'X'.
        ls_fieldcat-key = 'X'.
        ls_fieldcat-seltext_s = 'S'.
        ls_fieldcat-no_out = ''.
      WHEN 'HKMC'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Company' .
        ls_fieldcat-ddictxt  = 'S'.
        ls_fieldcat-key = 'X'.
      WHEN 'ZVIN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'KMA Internal VIN   ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-key = 'X'.
      WHEN 'UORD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Unit order number  ' .
        ls_fieldcat-ddictxt               = 'S'.
        ls_fieldcat-key = 'X'.
      WHEN 'DISTCO'  .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Destination Code   ' .
        ls_fieldcat-ddictxt  = 'S'.
      WHEN 'DLER'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Dealer Code' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'PACK'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Order pack ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'MODEL_CODE'  .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Vehicle Model Code ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'GRADE'   .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'GRADE  ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'OCCN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Option Combination Number  ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'GRUP'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Gruop  ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'REFE'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Reference number   ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'IVNB'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Invoice number ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'WKNO'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Work Order Number  ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C111'.
      WHEN 'WO_EXTC' .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Work Order External Color  ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C111'.
      WHEN 'WO_INTC' .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Work Order Internal Color  ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C111'.
      WHEN 'PORT'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Port ID' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'VESL'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Vessel code' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'VIN' .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'VIN' .  ls_fieldcat-ddictxt = 'S'.
      WHEN 'ENG_NO'  .
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Engine Ass"Y ID' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'KEYN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Key number ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'CARN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Car number ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'REGN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Region Code' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'FLET'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status - Order ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ST01'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Order input on system ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST06'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Planning(Scheduled)   ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST07'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Sequence  ' .
        ls_fieldcat-ddictxt        = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST08'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Inline (Body) ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST09'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Inline (Paint)' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST10'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Inline (Trim) ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST11'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Sign-off  ' .
        ls_fieldcat-ddictxt   = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST12'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – PDI (VPC IN)  ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST13'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – (VPC OUT)' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST14'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – Factory wharfage  ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST15'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status – At sea (Shipping out) ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST16'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Port Stock  ' .
        ls_fieldcat-ddictxt  = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST17'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Inland Trans.(1)' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST18'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Compound' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST19'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Inland Trans.(2)' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST20'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Dealer Stock' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'ST21'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Status-Retail Sales' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C300'.
      WHEN 'DT01_A'.
        ls_fieldcat-seltext_s = 'DT01'.
        ls_fieldcat-reptext_ddic = 'Date-Dealer Allocation ' .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.

      WHEN 'DT06_A'..
        ls_fieldcat-seltext_s = 'DT06'.
        ls_fieldcat-reptext_ddic = 'Estimated Body-in Date'  .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT07_A'..
        ls_fieldcat-seltext_s = 'DT07'.
        ls_fieldcat-reptext_ddic = 'Estimated Body-in Date'  .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT08_A'..
        ls_fieldcat-seltext_s = 'DT08'.
        ls_fieldcat-reptext_ddic = 'Date of Body Line input '.
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT09_A'..
        ls_fieldcat-seltext_s = 'DT09'.
        ls_fieldcat-reptext_ddic = 'Date of Paint Line input'.
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT10_A'..
        ls_fieldcat-seltext_s = 'DT10'.
        ls_fieldcat-reptext_ddic = 'Date of Trim Line input '.
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT11_A'.
        ls_fieldcat-seltext_s = 'DT11'.
        ls_fieldcat-reptext_ddic = 'Date of Sign-off'.
        ls_fieldcat-ddictxt =  'S'.
        ls_fieldcat-emphasize    = 'C711'.

      WHEN 'DT12_A'.
        ls_fieldcat-seltext_s = 'DT12'.
        ls_fieldcat-reptext_ddic = 'Date of PDI input (VPC IN)'  .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.

      WHEN 'DT13_A'.
        ls_fieldcat-seltext_s = 'DT13'.
        ls_fieldcat-reptext_ddic = 'Date (VPCOUT)'.
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.


      WHEN 'DT14_A'.
        ls_fieldcat-seltext_s = 'DT14'.
        ls_fieldcat-reptext_ddic = 'Date of Factory wharfage input'  .
        ls_fieldcat-ddictxt = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT15_A'.
        ls_fieldcat-seltext_s = 'DT15'.
        ls_fieldcat-reptext_ddic = 'Date of Shipment  '  .
        ls_fieldcat-ddictxt  = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'DT16_A'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Date-Port Stock   '  .
        ls_fieldcat-ddictxt  = 'S'.
        ls_fieldcat-emphasize    = 'C711'.
      WHEN 'ES08'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – Body Line input  ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES09'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – Paint Line input ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES10'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – Trim Line input  ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES11'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimation constant – Sign-off ' .
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES12'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – PDI input (VPC IN)'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES13'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – F/Storage Yard input(VPC OUT)'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES14'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Estimation constant – F/Wharfage input  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES15'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimation constant – Shipment  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES16'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimated-Port Stock'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES17'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimated-Inland Trans.(1)  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES18'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimated-Compound  '.
        ls_fieldcat-ddictxt    = 'S'.
      WHEN 'ES19'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimated-Inland Trans. (2)  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ES20'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Estimated-Dealer Stock  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'RUSH'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Rush Order  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'RORQ'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Rush Order Request  '.
        ls_fieldcat-ddictxt  = 'S'.
      WHEN 'RORS'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Rush Order Response '.
        ls_fieldcat-ddictxt                  = 'S'.
      WHEN 'RRRC'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Rush Reject Reason Code '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ALLO'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Allocated Order '.
        ls_fieldcat-ddictxt =               'S'.
      WHEN 'AORQ'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Allocated Order Request '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ALLD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Allocating Dealer Code  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'AORS'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Allocated Order Response'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ARRC'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Allocate Reject Reason Code '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'SNDT'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Host(Prod.) SENDING DATE'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'CRDT_A'.
        ls_fieldcat-seltext_s = 'CRDT'.
        ls_fieldcat-reptext_ddic = 'Date of data creation   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'KSEQ'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Interim Sequence'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'CRTM'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Time of data creation   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'GUBN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Deletion constant – Unit order number   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'MNFS'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Generation constant – Manifest  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'SCRP'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Scrapping   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'BBAK'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Buy Back'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'RRCP'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Return goods receipt'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'VESN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Vessel Name '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'DIOR'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Agency Order No.'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'HCHK'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'HCHK'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'HCHD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'HCHD'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'SCHK'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'SCHK'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'SCHD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'SCHD'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'PLOC'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Plant location  '.
        ls_fieldcat-ddictxt =  'S'.
      WHEN 'GUMI'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Model Index under Old BOM   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'GUOC'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'OCN under Old BOM   '.
        ls_fieldcat-ddictxt  = 'S'.
      WHEN 'IPDD_A'.
        ls_fieldcat-seltext_s = 'IPDD'.
        ls_fieldcat-reptext_ddic =
        'Initial estimated date of Sign-off  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'LPDD_A'.
        ls_fieldcat-seltext_s = 'LPDD'.
        ls_fieldcat-reptext_ddic = 'Final estimated date of Sign-off'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'ISDD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic =
        'Initial estimated date of Shipment  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'LSDD'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Final estimated date of Shipment'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'IADD_A'.
        ls_fieldcat-seltext_s = 'IADD'.
        ls_fieldcat-reptext_ddic =
        'Initial estimated date of Arrival   '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'LADD_A'.
        ls_fieldcat-seltext_s = 'LADD'.
        ls_fieldcat-reptext_ddic = 'Final estimated date of Arrival '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'FOMN'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'FORECAST MONTH  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'SLTP'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Sales type(Normal,Fleet)'.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'DIS2'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Sales country   '.
        ls_fieldcat-ddictxt =   'S'.
      WHEN 'CNCL'.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
        ls_fieldcat-reptext_ddic = 'Cancel  '.
        ls_fieldcat-ddictxt = 'S'.
      WHEN 'CHDT_A'.
        ls_fieldcat-seltext_s = 'CHDT'.
        ls_fieldcat-reptext_ddic = 'Change Date '.
        ls_fieldcat-ddictxt = 'S'.
    ENDCASE.
    IF ls_fieldcat-fieldname EQ  'DT01' OR
      ls_fieldcat-fieldname EQ  'DT06' OR
      ls_fieldcat-fieldname EQ  'DT07' OR
      ls_fieldcat-fieldname EQ  'DT08' OR
      ls_fieldcat-fieldname EQ  'DT09' OR
      ls_fieldcat-fieldname EQ  'DT10' OR
      ls_fieldcat-fieldname EQ  'DT11' OR
      ls_fieldcat-fieldname EQ  'DT12' OR
      ls_fieldcat-fieldname EQ  'DT13' OR
      ls_fieldcat-fieldname EQ  'DT14' OR
      ls_fieldcat-fieldname EQ  'DT15' OR
      ls_fieldcat-fieldname EQ  'CRDT' OR

      ls_fieldcat-fieldname EQ  'IPDD' OR
      ls_fieldcat-fieldname EQ  'LPDD' OR
      ls_fieldcat-fieldname EQ  'CHDT' .
      ls_fieldcat-no_out = 'X'.
    ENDIF.

    IF ls_fieldcat-datatype EQ 'DATE'.
      ls_fieldcat-no_zero = 'X'.
    ENDIF.
    ls_fieldcat-ddictxt      = 'S'.
    MODIFY g_fieldcat_t FROM ls_fieldcat.
  ENDLOOP.


*  PERFORM GET_FILEDCATALOG TABLES LT_FIELDCAT
*                           USING  'GT_ITEM'.

*  LS_FIELDCAT-KEY = SPACE.
*  MODIFY LT_FIELDCAT FROM LS_FIELDCAT
*         TRANSPORTING KEY
*         WHERE KEY NE ''.
*  LOOP AT LT_FIELDCAT INTO LS_FIELDCAT.
*
*    CASE LS_FIELDCAT-FIELDNAME.
*     ENDCASE.
*
*    MODIFY LT_FIELDCAT FROM LS_FIELDCAT.
*
*  ENDLOOP.
*
*  CLEAR : LS_FIELDCAT.
*
*  IF NOT LT_FIELDCAT[] IS INITIAL.
*
*  APPEND LINES OF LT_FIELDCAT TO G_FIELDCAT_T.
*  ENDIF.
ENDFORM. " GET_FILEDCATALOG_ALV


*&---------------------------------------------------------------------*
*&      Form  LOCAL_USERCOMMAND
*&---------------------------------------------------------------------*

FORM local_usercommand
     USING l_ucomm LIKE sy-ucomm
           ls_selfield TYPE slis_selfield.

  DATA : lv_ans.
*
  CASE l_ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE SCREEN .
  ENDCASE.

  ls_selfield-refresh = 'X'.
  CASE l_ucomm.

    WHEN 'PROC'.
      IF gv_docnum IS INITIAL.
        PERFORM popup_to_confirm USING  'S'
                                            'Confirm'
                                            text-m01
                                            text-m02
                                            text-m03
                                   CHANGING lv_ans.

        ls_selfield-refresh = 'X'.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text   = 'IDOC Generating....'
          EXCEPTIONS
            OTHERS = 1.

        PERFORM send_data.
      ELSE.
        MESSAGE i001 WITH
        'Already sended.'.
      ENDIF.

    WHEN 'LOG'.
      READ TABLE gt_item INDEX 1.
      CHECK NOT gt_item-status IS INITIAL.
      PERFORM call_log_prg .
  ENDCASE.

ENDFORM. " LOCAL_USERCOMMAND

*&---------------------------------------------------------------------*
*&      Form  LOCAL_TOPOFPAGE
*&---------------------------------------------------------------------*

FORM local_topofpage USING p_check.



  CHECK p_check EQ 'X'.

*  WRITE: / SY-TITLE.

*_
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = G_LIST_TOP_OF_PAGE_T.
*

ENDFORM. " LOCAL_TOPOFPAGE


*&---------------------------------------------------------------------*
*&      Form  LOCAL_PFSTATUS
*&---------------------------------------------------------------------*

FORM local_pfstatus USING extab TYPE slis_t_extab.

  DATA : lv_cnt TYPE i.
  DESCRIBE TABLE gt_item LINES lv_cnt.

  SET TITLEBAR  'T100' WITH lv_cnt .
  SET PF-STATUS 'S100' EXCLUDING extab.

ENDFORM. " LOCAL_PFSTATUS

*&---------------------------------------------------------------------*
*&      Form  LOCAL_BEFORE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINEINFO  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*

FORM local_before_line
     USING    ps_lineinfo TYPE kkblo_lineinfo
              p_value.
  CHECK NOT p_value IS INITIAL.

ENDFORM. " LOCAL_BEFORE_LINE

*&---------------------------------------------------------------------*
*&      Form  LOCAL_AFTER_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINEINFO  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*

FORM local_after_line
     USING    ps_lineinfo TYPE kkblo_lineinfo
              p_value.


  CHECK NOT p_value IS INITIAL.



ENDFORM. " LOCAL_AFTER_LINE
