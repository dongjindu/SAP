*&---------------------------------------------------------------------*
*& Report  ZRKORREKBZ                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zrkorrekbz                 .
TABLES:  ekko,
         konv,
         ekbe,
         ekbz.

data: xekko like ekko occurs 10 with header line.

DATA: l_ekko like ekko.
data: l_ekpo like ekpo.
DATA: l_konv like konv,
      knumv  like konv-knumv,
      kposn  like konv-kposn.

DATA: lt_ekbe like ekbe occurs 0,
      chk_ekbe like ekbe occurs 0 with header line,
      l_ekbe like ekbe.
DATA: lt_ekbz like ekbz occurs 0,
      chg_ekbz like ekbz occurs 0 with header line,
      l_ekbz like ekbz.
data: pkg_ekbz like ekbz occurs 0 with header line.
data: last like sy-tabix.

DATA: doc_count LIKE sy-tabix.

*************** Begin of selektion screens *****************************
*----- Einschränkung der Belegdaten

selection-screen begin of block quelle with frame title text-011.

select-options: dd_ekorg for ekko-ekorg.

select-options: dd_ebeln for ekko-ebeln.

select-options: dd_bedat for ekko-bedat.

selection-screen end of block quelle.

* ----- Steuerung des Programmlaufs
selection-screen begin of block lauf with frame title text-012.
PARAMETERS: test RADIOBUTTON GROUP updt DEFAULT 'X',
            fix  RADIOBUTTON GROUP updt.

selection-screen end of block lauf.
*************** End of selektion screens *******************************

START-OF-SELECTION.

  IF NOT test IS INITIAL.
    WRITE: text-007, text-005.
  ELSE.
    WRITE: text-007, text-006.
  ENDIF.

  perform select_data.

  IF chg_ekbz[] IS INITIAL.
    WRITE:/ text-004.
    exit.
  ENDIF.

  perform correct_ekbz.

* neues Feld füllen und Datenbank fortschreiben.
  IF test IS INITIAL.

    perform update_ekbz.

* Ergebnis des Testlaufs
  ELSE.
    if not chg_ekbz[] is initial.
      WRITE:/ doc_count, text-003.
    else.
      write:/ text-004.
    endif.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

* Belege selektieren
  select * from ekko into table xekko
                     where
                     ebeln in dd_ebeln and
                     ekorg in dd_ekorg and
                     bedat in dd_bedat
                     order by primary key.

  loop at xekko.

    clear: lt_ekbe, lt_ekbe[], lt_ekbz, lt_ekbz[].

*   Nur Anfragen, Bestellungen, Kontrakte, Lieferpläne lesen
    check xekko-bstyp ca 'AFKL'.

* Materialbelege selektieren
    SELECT * FROM ekbz INTO table lt_ekbz
           WHERE ebeln eq xekko-ebeln and
                 vgabe EQ '1'
           ORDER BY PRIMARY KEY.

    check sy-subrc eq 0.
    append lines of lt_ekbz TO chg_ekbz.

    SELECT * FROM ekbe INTO table lt_ekbe
           WHERE ebeln eq xekko-ebeln and
                 vgabe EQ '1'
           ORDER BY PRIMARY KEY.

    append lines of lt_ekbe TO chk_ekbe.

  endloop.

ENDFORM.                               " select_data
*&---------------------------------------------------------------------*
*&      Form  correct_ekbz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM correct_ekbz.

  LOOP AT chg_ekbz.

    if chg_ekbz-bewtp eq 'F'.

      if chg_ekbz-shkko eq chg_ekbz-shkzg.
        delete chg_ekbz.
        continue.
      endif.
      chg_ekbz-shkko = chg_ekbz-shkzg.
      modify chg_ekbz.
      doc_count = doc_count + 1.
      continue.
    endif.

    clear chk_ekbe.
    read table chk_ekbe
       With key
       mandt = chg_ekbz-mandt
       ebeln = chg_ekbz-ebeln
       ebelp = chg_ekbz-ebelp
       zekkn = '00'
       vgabe = chg_ekbz-vgabe
       gjahr = chg_ekbz-gjahr
       belnr = chg_ekbz-belnr
       buzei = chg_ekbz-buzei
       binary search.

    IF SY-SUBRC <> 0.
      delete chg_ekbz.
      CONTINUE.
    ENDIF.


    IF NOT chk_ekbe-knumv IS INITIAL.
      knumv = chk_ekbe-knumv.
      move chk_ekbe-buzei to kposn.
    ELSE.
      clear xekko.
      read table xekko
           with key
           ebeln = chg_ekbz-ebeln
           binary search.
      if sy-subrc ne 0.
        delete chg_ekbz.
        CONTINUE.
      endif.
      knumv = xekko-knumv.
      move chk_ekbe-ebelp to kposn.
    ENDIF.


    SELECT SINGLE * FROM konv INTO l_konv
       WHERE
       knumv = knumv AND
       kposn = kposn AND
       stunr = chg_ekbz-stunr AND
       zaehk = chg_ekbz-zaehk.

    IF NOT sy-subrc EQ 0.
      delete chg_ekbz.
      CONTINUE.
    ENDIF.


    IF l_konv-kwert LT 0.
      if chg_ekbz-shkzg = 'S'.
        if chg_ekbz-shkko eq 'H'.
          delete chg_ekbz.
          continue.
        endif.

        chg_ekbz-shkko = 'H'.
      else.
        if chg_ekbz-shkko eq 'S'.
          delete chg_ekbz.
          continue.
        endif.
        chg_ekbz-shkko = 'S'.
      endif.
    else.
      if chg_ekbz-shkko eq chg_ekbz-shkzg.
        delete chg_ekbz.
        continue.
      endif.
      chg_ekbz-shkko = chg_ekbz-shkzg.
    endif.
    modify chg_ekbz.

    doc_count = doc_count + 1.

  ENDLOOP.

ENDFORM.                               " correct_ekbz
*&---------------------------------------------------------------------*
*&      Form  update_ekbz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ekbz.

  clear chg_ekbz.
  describe table chg_ekbz lines last.

  if last gt 0.

    read table chg_ekbz index 1.

    pkg_ekbz = chg_ekbz.
    loop at chg_ekbz.

      if pkg_ekbz-ebeln ne chg_ekbz-ebeln.
        if sy-tabix eq last.
          pkg_ekbz = chg_ekbz.
          append pkg_ekbz.
          UPDATE ekbz FROM table pkg_ekbz.
          COMMIT WORK.
        else.
          UPDATE ekbz FROM table pkg_ekbz.
          COMMIT WORK.
          pkg_ekbz = chg_ekbz.
          clear pkg_ekbz[].
          append pkg_ekbz.
        endif.

      else.
        pkg_ekbz = chg_ekbz.
        append pkg_ekbz.
        if sy-tabix eq last.
          UPDATE ekbz FROM table pkg_ekbz.
          COMMIT WORK.
        endif.
      endif.
    endloop.

    WRITE:/ doc_count, text-008.
  else.
    write:/ text-004.
  endif.

ENDFORM.                               " update_ekbz
