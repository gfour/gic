data Defunc1 = D_INC_0_I | D_F_1_I Defunc1 | D_ADD_1_I Int
result = f d_inc_0_i 7;
sq sq_c = sq_c * sq_c;
inc inc_t = inc_t + 1;
add add_a add_b = add_a + add_b;
f f_s f_x = if (f_x <= 0) then (d_Apply_i_i f_s f_x) else (f (d_f_1_i (d_add_1_i (d_Apply_i_i f_s f_x))) (f_x - 1));
d_Apply_i_i cl_i_i d_Apply_i_i_0 = case cl_i_i of {
 D_INC_0_I  -> inc d_Apply_i_i_0;
 D_F_1_I d_f_1_i_0 -> f d_f_1_i_0 d_Apply_i_i_0;
 D_ADD_1_I d_add_1_i_0 -> add d_add_1_i_0 d_Apply_i_i_0
};

d_inc_0_i = D_INC_0_I ;
d_f_1_i d_f_1_i_0 = D_F_1_I d_f_1_i_0;
d_add_1_i d_add_1_i_0 = D_ADD_1_I d_add_1_i_0;
