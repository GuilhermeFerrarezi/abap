*&---------------------------------------------------------------------*
*&  Include           ZTSGF_SIMPLEX_TOP
*&---------------------------------------------------------------------*
program  ztsgf_simplex.

*TYPE-POOLS: abap.

tables: zstgf_equality_symbol.

types:
begin of y_line,
  z type zdegf_number,
  x1 type zdegf_number,
  x2 type zdegf_number,
  x3 type zdegf_number,
  x4 type zdegf_number,
  x5 type zdegf_number,
  b type zdegf_number,
end of y_line.

data: v_restrictions type n,
      v_variables type n,
      wa_function type y_line,
      wa_restriction1 type y_line,
      wa_restriction2 type y_line,
      wa_answer type y_line.
