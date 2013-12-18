data Defunc = D_SUCCESSOR_0_I | D_SUCCESSOR_H_0_TI | D_IDENTITY_0_I | D_C0_0_TI | D_C1_0_TI | D_C3_0_TI | D_C6_0_TI | D_C12_0_TI | D_C24_0_TI | D_C48_0_TI | D_C96_0_TI | D_C192_0_TI | D_C385_0_TI | D_C771_0_TI | D_C1543_0_TI | D_C3087_0_TI | D_C7_0_TI | D_C3_H_0_TTI | D_C6174_0_TI | D_C343_0_TI 

main :: IO ()
main = putStrLn (show result)

identity identity_i = identity_i;
successor successor_i = successor_i + 1;
successor_h successor_h_f successor_h_x = (d_Apply_i_i successor_h_f successor_h_x) + 1;
church church_i church_f church_x = if (church_i == 0) then church_x else (church (church_i - 1) church_f (d_Apply_i_i church_f church_x));
church_h church_h_i church_h_f church_h_x church_h_z = if (church_h_i == 0) then (d_Apply_i_i church_h_x church_h_z) else (church_h (church_h_i - 1) church_h_f church_h_x (d_Apply_ti_i church_h_f church_h_x church_h_z));
unchurch unchurch_n = d_Apply_ti_i unchurch_n d_successor_0_i 0;
unchurch_h unchurch_h_n = d_Apply_tti_i unchurch_h_n d_successor_h_0_ti d_identity_0_i 0;
c_succ c_succ_n c_succ_f c_succ_x = d_Apply_ti_i c_succ_n c_succ_f (d_Apply_i_i c_succ_f c_succ_x);
c_plus c_plus_n c_plus_m c_plus_f c_plus_x = d_Apply_ti_i c_plus_n c_plus_f (d_Apply_ti_i c_plus_m c_plus_f c_plus_x);
c_nonzero c_nonzero_n c_nonzero_a c_nonzero_b c_nonzero_f c_nonzero_x = if ((unchurch c_nonzero_n) == 0) then (d_Apply_ti_i c_nonzero_b c_nonzero_f c_nonzero_x) else (d_Apply_ti_i c_nonzero_a c_nonzero_f c_nonzero_x);
c_exp c_exp_n c_exp_m c_exp_f c_exp_x = d_Apply_tti_i c_exp_m c_exp_n c_exp_f c_exp_x;
c_2x0 c_2x0_n c_2x0_f c_2x0_x = c_plus c_2x0_n c_2x0_n c_2x0_f c_2x0_x;
c_2x1 c_2x1_n c_2x1_f c_2x1_x = d_Apply_ti_i c_2x1_n c_2x1_f (d_Apply_ti_i c_2x1_n c_2x1_f (d_Apply_i_i c_2x1_f c_2x1_x));
c0 c0_f c0_x = church 0 c0_f c0_x;
c1 c1_f c1_x = c_2x1 d_c0_0_ti c1_f c1_x;
c3 c3_f c3_x = c_2x1 d_c1_0_ti c3_f c3_x;
c6 c6_f c6_x = c_2x0 d_c3_0_ti c6_f c6_x;
c12 c12_f c12_x = c_2x0 d_c6_0_ti c12_f c12_x;
c24 c24_f c24_x = c_2x0 d_c12_0_ti c24_f c24_x;
c48 c48_f c48_x = c_2x0 d_c24_0_ti c48_f c48_x;
c96 c96_f c96_x = c_2x0 d_c48_0_ti c96_f c96_x;
c192 c192_f c192_x = c_2x0 d_c96_0_ti c192_f c192_x;
c385 c385_f c385_x = c_2x1 d_c192_0_ti c385_f c385_x;
c771 c771_f c771_x = c_2x1 d_c385_0_ti c771_f c771_x;
c1543 c1543_f c1543_x = c_2x1 d_c771_0_ti c1543_f c1543_x;
c3087 c3087_f c3087_x = c_2x1 d_c1543_0_ti c3087_f c3087_x;
c6174 c6174_f c6174_x = c_2x0 d_c3087_0_ti c6174_f c6174_x;
c7 c7_f c7_x = c_succ d_c6_0_ti c7_f c7_x;
c3_h c3_h_f c3_h_x c3_h_z = church_h 3 c3_h_f c3_h_x c3_h_z;
c343 c343_f c343_x = c_exp d_c7_0_ti d_c3_h_0_tti c343_f c343_x;
c c_f c_x = c_nonzero d_c3_0_ti d_c6174_0_ti d_c343_0_ti c_f c_x;
rep rep_n rep_x = if (rep_n <= 0) then rep_x else ((1 + (rep (rep_n - 1) rep_x)) + (rep (rep_n - 2) rep_x));
result = rep 30 (unchurch d_c771_0_ti);
d_Apply_tti_i cl_tti_i d_Apply_tti_i_0 d_Apply_tti_i_1 d_Apply_tti_i_2 = case cl_tti_i of {
 D_C3_H_0_TTI -> c3_h d_Apply_tti_i_0 d_Apply_tti_i_1 d_Apply_tti_i_2
};
d_Apply_ti_i cl_ti_i d_Apply_ti_i_0 d_Apply_ti_i_1 = case cl_ti_i of {
 D_SUCCESSOR_H_0_TI -> successor_h d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C0_0_TI -> c0 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C1_0_TI -> c1 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C3_0_TI -> c3 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C6_0_TI -> c6 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C12_0_TI -> c12 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C24_0_TI -> c24 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C48_0_TI -> c48 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C96_0_TI -> c96 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C192_0_TI -> c192 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C385_0_TI -> c385 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C771_0_TI -> c771 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C1543_0_TI -> c1543 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C3087_0_TI -> c3087 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C7_0_TI -> c7 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C6174_0_TI -> c6174 d_Apply_ti_i_0 d_Apply_ti_i_1;
 D_C343_0_TI -> c343 d_Apply_ti_i_0 d_Apply_ti_i_1
};
d_Apply_i_i cl_i_i d_Apply_i_i_0 = case cl_i_i of {
 D_SUCCESSOR_0_I -> successor d_Apply_i_i_0;
 D_IDENTITY_0_I -> identity d_Apply_i_i_0
};
d_successor_0_i = D_SUCCESSOR_0_I ;
d_successor_h_0_ti = D_SUCCESSOR_H_0_TI ;
d_identity_0_i = D_IDENTITY_0_I ;
d_c0_0_ti = D_C0_0_TI ;
d_c1_0_ti = D_C1_0_TI ;
d_c3_0_ti = D_C3_0_TI ;
d_c6_0_ti = D_C6_0_TI ;
d_c12_0_ti = D_C12_0_TI ;
d_c24_0_ti = D_C24_0_TI ;
d_c48_0_ti = D_C48_0_TI ;
d_c96_0_ti = D_C96_0_TI ;
d_c192_0_ti = D_C192_0_TI ;
d_c385_0_ti = D_C385_0_TI ;
d_c771_0_ti = D_C771_0_TI ;
d_c1543_0_ti = D_C1543_0_TI ;
d_c3087_0_ti = D_C3087_0_TI ;
d_c7_0_ti = D_C7_0_TI ;
d_c3_h_0_tti = D_C3_H_0_TTI ;
d_c6174_0_ti = D_C6174_0_TI ;
d_c343_0_ti = D_C343_0_TI ;

