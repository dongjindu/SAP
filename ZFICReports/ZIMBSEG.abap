*
* Use carefully
* provided by Andy Choi.
*

REPORT ZIMBSEG
   no standard page heading line-size 255.

TABLES : BKPF, BSEG, BSIS, bsak, bsik.

PARAMETERS : P_BUKRS LIKE Bkpf-BUKRS memory id BUK,
             p_belnr like bkpf-belnr,
             P_gjahr like bkpf-gjahr memory id gjr,
             p_buzei like bseg-buzei.

parameters: p_run as checkbox.
parameters: p_frc as checkbox.


parameters: p_io like bseg-aufnr.


start-of-selection.


  SELECT single * FROM bseg
           WHERE bukrs = p_bukrs
             AND gjahr = p_gjahr
             AND belnr = p_belnr
             AND buzei = p_buzei.

  CHECK bseg-UMSKS = 'A'.
  if bseg-aufnr = space and p_frc = space.
    exit.
  endif.


  SELECT single * FROM bsis
           WHERE bukrs = p_bukrs
             AND gjahr = p_gjahr
             AND belnr = p_belnr
             AND buzei = p_buzei.

  if p_run = 'X'.
    bseg-aufnr = p_io.
    update bseg.
    bsis-aufnr = p_io.
    update bsis.


    SELECT single * FROM bsik
             WHERE bukrs = p_bukrs
               AND gjahr = p_gjahr
               AND belnr = p_belnr
               AND buzei = p_buzei.
    if sy-subrc = 0.
      bsik-aufnr = p_io.
      update bsik.
    else.
      SELECT single * FROM bsak
               WHERE bukrs = p_bukrs
                 AND gjahr = p_gjahr
                 AND belnr = p_belnr
                 AND buzei = p_buzei.

      if sy-subrc = 0.
        bsak-aufnr = p_io.
        update bsak.
      endif.
    endif.





    write:/ 'Downpayment Information is updated with '.
    write: bseg-aufnr.
    write:/ 'You have to run OKBG transaction'.
    write:/ 'Also you need to run IO settlement to AuC (KO88)'.
  endif.
