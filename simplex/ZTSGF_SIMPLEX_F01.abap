*&---------------------------------------------------------------------*
*&  Include           ZTSGF_SIMPLEX_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SIMPLEX_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_simplex_calculation.

  data: l_div_result1 type zdegf_number,
        l_div_result2 type zdegf_number,
        lw_function type y_line,
        lw_restriction1 type y_line,
        lw_restriction2 type y_line.

  lw_function = wa_function.
  lw_restriction1 = wa_restriction1.
  lw_restriction2 = wa_restriction2.

  perform zf_invert_function_signal changing lw_function.

  lw_function-z = 1.
  lw_restriction1-x4 = 1.
  lw_restriction2-x5 = 1.

  while lw_function-x1 lt 0
     or lw_function-x2 lt 0
     or lw_function-x3 lt 0.

    if lw_function-x1 lt lw_function-x2
   and lw_function-x1 lt lw_function-x3.

      if lw_restriction1-x1 is not initial.
        l_div_result1 = lw_restriction1-b / lw_restriction1-x1.
      endif.
      if lw_restriction2-x1 is not initial.
        l_div_result2 = lw_restriction2-b / lw_restriction2-x1.
      endif.

      if ( l_div_result2 lt l_div_result1
      or l_div_result2 eq l_div_result1 )
     and l_div_result2 gt 0.

        perform zf_calculate_new_line_by_x1 changing lw_restriction2.
        perform zf_calculate_n_line_by_x1 using lw_restriction2
                                       changing lw_restriction1.
        perform zf_calculate_n_line_by_x1 using lw_restriction2
                                       changing lw_function.

      elseif l_div_result1 lt l_div_result2
         and l_div_result1 gt 0.

        perform zf_calculate_new_line_by_x1 changing lw_restriction1.
        perform zf_calculate_n_line_by_x1 using lw_restriction1
                                       changing lw_restriction2.
        perform zf_calculate_n_line_by_x1 using lw_restriction1
                                       changing lw_function.
      else.
        return.
      endif.

    elseif lw_function-x2 lt lw_function-x1
       and lw_function-x2 lt lw_function-x3.

      if lw_restriction1-x2 is not initial.
        l_div_result1 = lw_restriction1-b / lw_restriction1-x2.
      endif.
      if lw_restriction2-x2 is not initial.
        l_div_result2 = lw_restriction2-b / lw_restriction2-x2.
      endif.

      if ( l_div_result2 lt l_div_result1
      or l_div_result2 eq l_div_result1 )
     and l_div_result2 gt 0.

        perform zf_calculate_new_line_by_x2 changing lw_restriction2.
        perform zf_calculate_n_line_by_x2 using lw_restriction2
                                       changing lw_restriction1.
        perform zf_calculate_n_line_by_x2 using lw_restriction2
                                       changing lw_function.

      elseif l_div_result1 lt l_div_result2
         and l_div_result1 gt 0.

        perform zf_calculate_new_line_by_x2 changing lw_restriction1.
        perform zf_calculate_n_line_by_x2 using lw_restriction1
                                       changing lw_restriction2.
        perform zf_calculate_n_line_by_x2 using lw_restriction1
                                       changing lw_function.

      else.
        return.
      endif.

    elseif lw_function-x3 lt lw_function-x1
       and lw_function-x3 lt lw_function-x2.

      if lw_restriction1-x3 is not initial.
        l_div_result1 = lw_restriction1-b / lw_restriction1-x3.
      endif.
      if lw_restriction2-x3 is not initial.
        l_div_result2 = lw_restriction2-b / lw_restriction2-x3.
      endif.

      if ( l_div_result2 lt l_div_result1
      or l_div_result2 eq l_div_result1 )
     and l_div_result2 gt 0.

        perform zf_calculate_new_line_by_x3 changing lw_restriction2.
        perform zf_calculate_n_line_by_x3 using lw_restriction2
                                       changing lw_restriction1.
        perform zf_calculate_n_line_by_x3 using lw_restriction2
                                       changing lw_function.

      elseif l_div_result1 lt l_div_result2
         and l_div_result1 gt 0.

        perform zf_calculate_new_line_by_x3 changing lw_restriction1.
        perform zf_calculate_n_line_by_x3 using lw_restriction1
                                       changing lw_restriction2.
        perform zf_calculate_n_line_by_x3 using lw_restriction1
                                       changing lw_function.

      else.
        return.
      endif.

    elseif lw_function-x1 eq lw_function-x2
        or lw_function-x1 eq lw_function-x3.

      if lw_restriction1-x1 is not initial.
        l_div_result1 = lw_restriction1-b / lw_restriction1-x1.
      endif.
      if lw_restriction2-x1 is not initial.
        l_div_result2 = lw_restriction2-b / lw_restriction2-x1.
      endif.

      if ( l_div_result2 lt l_div_result1
      or l_div_result2 eq l_div_result1 )
     and l_div_result2 gt 0.

        perform zf_calculate_new_line_by_x1 changing lw_restriction2.
        perform zf_calculate_n_line_by_x1 using lw_restriction2
                                       changing lw_restriction1.
        perform zf_calculate_n_line_by_x1 using lw_restriction2
                                       changing lw_function.

      elseif l_div_result1 lt l_div_result2
         and l_div_result1 gt 0.

        perform zf_calculate_new_line_by_x1 changing lw_restriction1.
        perform zf_calculate_n_line_by_x1 using lw_restriction1
                                       changing lw_restriction2.
        perform zf_calculate_n_line_by_x1 using lw_restriction1
                                       changing lw_function.

      else.
        return.
      endif.

    elseif lw_function-x2 eq lw_function-x1
        or lw_function-x2 eq lw_function-x3.

      if lw_restriction1-x2 is not initial.
        l_div_result1 = lw_restriction1-b / lw_restriction1-x2.
      endif.
      if lw_restriction2-x2 is not initial.
        l_div_result2 = lw_restriction2-b / lw_restriction2-x2.
      endif.

      if ( l_div_result2 lt l_div_result1
      or l_div_result2 eq l_div_result1 )
     and l_div_result2 gt 0.

        perform zf_calculate_new_line_by_x2 changing lw_restriction2.
        perform zf_calculate_n_line_by_x2 using lw_restriction2
                                       changing lw_restriction1.
        perform zf_calculate_n_line_by_x2 using lw_restriction2
                                       changing lw_function.

      elseif l_div_result1 lt l_div_result2
         and l_div_result1 gt 0.

        perform zf_calculate_new_line_by_x2 changing lw_restriction1.
        perform zf_calculate_n_line_by_x2 using lw_restriction1
                                       changing lw_restriction2.
        perform zf_calculate_n_line_by_x2 using lw_restriction1
                                       changing lw_function.

      else.
        return.
      endif.

    endif.

  endwhile.

  perform zf_calculate_answer using lw_function
                                    lw_restriction1
                                    lw_restriction2.

endform.                    " ZF_SIMPLEX_CALCULATION
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_NEW_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_calculate_new_line_by_x1 changing pw_restriction type y_line.

  pw_restriction-z = pw_restriction-z / pw_restriction-x1.
  pw_restriction-x2 = pw_restriction-x2 / pw_restriction-x1.
  pw_restriction-x3 = pw_restriction-x3 / pw_restriction-x1.
  pw_restriction-x4 = pw_restriction-x4 / pw_restriction-x1.
  pw_restriction-x5 = pw_restriction-x5 / pw_restriction-x1.
  pw_restriction-b = pw_restriction-b / pw_restriction-x1.
  pw_restriction-x1 = pw_restriction-x1 / pw_restriction-x1.

endform.                    " ZF_CALCULATE_NEW_LINE
*&---------------------------------------------------------------------*
*&      Form  ZF_INVERT_FUNCTION_SIGNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_FUNCTION  text
*----------------------------------------------------------------------*
form zf_invert_function_signal changing pw_function type y_line.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_function-x1
    importing
      e_result = pw_function-x1.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_function-x2
    importing
      e_result = pw_function-x2.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_function-x3
    importing
      e_result = pw_function-x3.

endform.                    " ZF_INVERT_FUNCTION_SIGNAL
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_NEW_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_RESTRICTION1  text
*----------------------------------------------------------------------*
form zf_calculate_n_line_by_x1 using pw_restriction2 type y_line
                            changing pw_restriction1 type y_line.
  data: new_line1 type y_line.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-z
    importing
      e_result = new_line1-z.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x1
    importing
      e_result = new_line1-x1.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x2
    importing
      e_result = new_line1-x2.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x3
    importing
      e_result = new_line1-x3.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x4
    importing
      e_result = new_line1-x4.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x5
    importing
      e_result = new_line1-x5.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-b
    importing
      e_result = new_line1-b.

  new_line1-x1 = new_line1-x1 * pw_restriction1-x1.
  new_line1-z = new_line1-z * pw_restriction1-x1.
  new_line1-x2 = new_line1-x2 * pw_restriction1-x1.
  new_line1-x3 = new_line1-x3 * pw_restriction1-x1.
  new_line1-x4 = new_line1-x4 * pw_restriction1-x1.
  new_line1-x5 = new_line1-x5 * pw_restriction1-x1.
  new_line1-b = new_line1-b * pw_restriction1-x1.

  pw_restriction1-z = pw_restriction1-z + new_line1-z.
  pw_restriction1-x1 = pw_restriction1-x1 + new_line1-x1.
  pw_restriction1-x2 = pw_restriction1-x2 + new_line1-x2.
  pw_restriction1-x3 = pw_restriction1-x3 + new_line1-x3.
  pw_restriction1-x4 = pw_restriction1-x4 + new_line1-x4.
  pw_restriction1-x5 = pw_restriction1-x5 + new_line1-x5.
  pw_restriction1-b = pw_restriction1-b + new_line1-b.

endform.                    " ZF_CALCULATE_NEW_LINE
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_ANSWER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_calculate_answer using pw_function type y_line
                               pw_restriction1 type y_line
                               pw_restriction2 type y_line.

  if pw_function-z eq 1
 and pw_restriction1-z is initial
 and pw_restriction2-z is initial.

    wa_answer-z = pw_function-b.

  elseif pw_restriction1-z eq 1
     and pw_function-z is initial
     and pw_restriction2-z is initial.

    wa_answer-z = pw_restriction1-b.

  elseif pw_restriction2-z eq 1
     and pw_function-z is initial
     and pw_restriction1-z is initial.

    wa_answer-z = pw_restriction2-b.

  endif.

  if pw_function-x1 eq 1
 and pw_restriction1-x1 is initial
 and pw_restriction2-x1 is initial.

    wa_answer-x1 = pw_function-b.

  elseif pw_restriction1-x1 eq 1
     and pw_function-x1 is initial
     and pw_restriction2-x1 is initial.

    wa_answer-x1 = pw_restriction1-b.

  elseif pw_restriction2-x1 eq 1
     and pw_function-x1 is initial
     and pw_restriction1-x1 is initial.

    wa_answer-x1 = pw_restriction2-b.

  endif.

  if pw_function-x2 eq 1
 and pw_restriction1-x2 is initial
 and pw_restriction2-x2 is initial.

    wa_answer-x2 = pw_function-b.

  elseif pw_restriction1-x2 eq 1
     and pw_function-x2 is initial
     and pw_restriction2-x2 is initial.

    wa_answer-x2 = pw_restriction1-b.

  elseif pw_restriction2-x2 eq 1
     and pw_function-x2 is initial
     and pw_restriction1-x2 is initial.

    wa_answer-x2 = pw_restriction2-b.

  endif.

  if pw_function-x3 eq 1
 and pw_restriction1-x3 is initial
 and pw_restriction2-x3 is initial.

    wa_answer-x3 = pw_function-b.

  elseif pw_restriction1-x3 eq 1
     and pw_function-x3 is initial
     and pw_restriction2-x3 is initial.

    wa_answer-x3 = pw_restriction1-b.

  elseif pw_restriction2-x3 eq 1
     and pw_function-x3 is initial
     and pw_restriction1-x3 is initial.

    wa_answer-x3 = pw_restriction2-b.

  endif.

  if pw_function-x4 eq 1
 and pw_restriction1-x4 is initial
 and pw_restriction2-x4 is initial.

    wa_answer-x4 = pw_function-b.

  elseif pw_restriction1-x4 eq 1
     and pw_function-x4 is initial
     and pw_restriction2-x4 is initial.

    wa_answer-x4 = pw_restriction1-b.

  elseif pw_restriction2-x4 eq 1
     and pw_function-x4 is initial
     and pw_restriction1-x4 is initial.

    wa_answer-x4 = pw_restriction2-b.

  endif.

  if pw_function-x5 eq 1
 and pw_restriction1-x5 is initial
 and pw_restriction2-x5 is initial.

    wa_answer-x5 = pw_function-b.

  elseif pw_restriction1-x5 eq 1
     and pw_function-x5 is initial
     and pw_restriction2-x5 is initial.

    wa_answer-x5 = pw_restriction1-b.

  elseif pw_restriction2-x5 eq 1
     and pw_function-x5 is initial
     and pw_restriction1-x5 is initial.

    wa_answer-x5 = pw_restriction2-b.

  endif.

endform.                    " ZF_CALCULATE_ANSWER
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_NEW_LINE_BY_X2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_RESTRICTION2  text
*----------------------------------------------------------------------*
form zf_calculate_new_line_by_x2 changing pw_restriction type y_line.

  pw_restriction-z = pw_restriction-z / pw_restriction-x2.
  pw_restriction-x1 = pw_restriction-x1 / pw_restriction-x2.
  pw_restriction-x3 = pw_restriction-x3 / pw_restriction-x2.
  pw_restriction-x4 = pw_restriction-x4 / pw_restriction-x2.
  pw_restriction-x5 = pw_restriction-x5 / pw_restriction-x2.
  pw_restriction-b = pw_restriction-b / pw_restriction-x2.
  pw_restriction-x2 = pw_restriction-x2 / pw_restriction-x2.

endform.                    " ZF_CALCULATE_NEW_LINE_BY_X2
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_NEW_LINE_BY_X3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_RESTRICTION2  text
*----------------------------------------------------------------------*
form zf_calculate_new_line_by_x3 changing pw_restriction type y_line.

  pw_restriction-z = pw_restriction-z / pw_restriction-x3.
  pw_restriction-x2 = pw_restriction-x2 / pw_restriction-x3.
  pw_restriction-x1 = pw_restriction-x1 / pw_restriction-x3.
  pw_restriction-x4 = pw_restriction-x4 / pw_restriction-x3.
  pw_restriction-x5 = pw_restriction-x5 / pw_restriction-x3.
  pw_restriction-b = pw_restriction-b / pw_restriction-x3.
  pw_restriction-x3 = pw_restriction-x3 / pw_restriction-x3.

endform.                    " ZF_CALCULATE_NEW_LINE_BY_X3
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_N_LINE_BY_X2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RESTRICTION2  text
*      <--P_WA_RESTRICTION1  text
*----------------------------------------------------------------------*
form zf_calculate_n_line_by_x2 using pw_restriction2 type y_line
                            changing pw_restriction1 type y_line.

  data: new_line1 type y_line.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-z
    importing
      e_result = new_line1-z.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x1
    importing
      e_result = new_line1-x1.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x2
    importing
      e_result = new_line1-x2.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x3
    importing
      e_result = new_line1-x3.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x4
    importing
      e_result = new_line1-x4.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x5
    importing
      e_result = new_line1-x5.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-b
    importing
      e_result = new_line1-b.

  new_line1-z = new_line1-z * pw_restriction1-x2.
  new_line1-x1 = new_line1-x1 * pw_restriction1-x2.
  new_line1-x2 = new_line1-x2 * pw_restriction1-x2.
  new_line1-x3 = new_line1-x3 * pw_restriction1-x2.
  new_line1-x4 = new_line1-x4 * pw_restriction1-x2.
  new_line1-x5 = new_line1-x5 * pw_restriction1-x2.
  new_line1-b = new_line1-b * pw_restriction1-x2.

  pw_restriction1-z = pw_restriction1-z + new_line1-z.
  pw_restriction1-x1 = pw_restriction1-x1 + new_line1-x1.
  pw_restriction1-x2 = pw_restriction1-x2 + new_line1-x2.
  pw_restriction1-x3 = pw_restriction1-x3 + new_line1-x3.
  pw_restriction1-x4 = pw_restriction1-x4 + new_line1-x4.
  pw_restriction1-x5 = pw_restriction1-x5 + new_line1-x5.
  pw_restriction1-b = pw_restriction1-b + new_line1-b.

endform.                    " ZF_CALCULATE_N_LINE_BY_X2
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULATE_N_LINE_BY_X3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RESTRICTION2  text
*      <--P_WA_FUNCTION  text
*----------------------------------------------------------------------*
form zf_calculate_n_line_by_x3 using pw_restriction2 type y_line
                            changing pw_restriction1 type y_line.

  data: new_line1 type y_line.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-z
    importing
      e_result = new_line1-z.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x1
    importing
      e_result = new_line1-x1.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x2
    importing
      e_result = new_line1-x2.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x3
    importing
      e_result = new_line1-x3.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x4
    importing
      e_result = new_line1-x4.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-x5
    importing
      e_result = new_line1-x5.

  call function 'ZTSGF_SIGNAL_INVERSION'
    exporting
      i_number = pw_restriction2-b
    importing
      e_result = new_line1-b.

  new_line1-z = new_line1-z * pw_restriction1-x3.
  new_line1-x1 = new_line1-x1 * pw_restriction1-x3.
  new_line1-x2 = new_line1-x2 * pw_restriction1-x3.
  new_line1-x3 = new_line1-x3 * pw_restriction1-x3.
  new_line1-x4 = new_line1-x4 * pw_restriction1-x3.
  new_line1-x5 = new_line1-x5 * pw_restriction1-x3.
  new_line1-b = new_line1-b * pw_restriction1-x3.

  pw_restriction1-z = pw_restriction1-z + new_line1-z.
  pw_restriction1-x1 = pw_restriction1-x1 + new_line1-x1.
  pw_restriction1-x2 = pw_restriction1-x2 + new_line1-x2.
  pw_restriction1-x3 = pw_restriction1-x3 + new_line1-x3.
  pw_restriction1-x4 = pw_restriction1-x4 + new_line1-x4.
  pw_restriction1-x5 = pw_restriction1-x5 + new_line1-x5.
  pw_restriction1-b = pw_restriction1-b + new_line1-b.

endform.                    " ZF_CALCULATE_N_LINE_BY_X3
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARE_EQUALITY_SYMBOLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zf_prepare_equality_symbols.

  zstgf_equality_symbol-line1 = '<='.
  zstgf_equality_symbol-line2 = '<='.
  zstgf_equality_symbol-line3 = '<='.
  zstgf_equality_symbol-line4 = '<='.
  zstgf_equality_symbol-line5 = '<='.

endform.                    " ZF_PREPARE_EQUALITY_SYMBOLS
