  (declare-datatypes () ((MyBool (MyTrue) (MyFalse))))
  (declare-datatypes () ((Nat (Zero) (Succ (proj_Succ_0 Nat)))))
  (declare-datatypes () ((List (Single (proj_Single_0 Nat)) (Cons (proj_Cons_0 Nat) (proj_Cons_1 List)))))
  (declare-datatypes () ((CList (Elt (proj_Elt_0 Nat)) (Cat (proj_Cat_0 CList) (proj_Cat_1 CList)))))
  (declare-fun lq (Nat Nat) MyBool)
  (declare-fun ite2 (MyBool Nat Nat) Nat)
  (declare-fun max (Nat Nat) Nat)
  (declare-datatypes () ((Tuple3 (MakeTuple3 (proj_MakeTuple3_0 Nat) (proj_MakeTuple3_1 Nat) (proj_MakeTuple3_2 MyBool)))))
  (declare-fun snd3 (Tuple3) Nat)
  (declare-fun gq (Nat Nat) MyBool)
  (declare-fun tf1 (List) Tuple3)
  (declare-fun tf0 (List) Tuple3)
  (declare-fun third3 (Tuple3) MyBool)
  (declare-fun spec (List) MyBool)
  (declare-fun tf3 (List List) List)
  (declare-fun tf2 (List List) List)
  (declare-fun catlist (List List) List)
  (declare-fun tf5 (CList) List)
  (declare-fun tf4 (CList) List)
  (declare-fun repr (CList) List)
  (declare-fun main (CList) MyBool)
  (declare-datatypes () ((Tuple4 (MakeTuple4 (proj_MakeTuple4_0 MyBool) (proj_MakeTuple4_1 Nat)))))
  (declare-fun myand (MyBool MyBool) MyBool)
  (declare-fun fst4 (Tuple4) MyBool)
  (declare-fun snd4 (Tuple4) Nat)
  (declare-fun tf7 (CList) Tuple4)
  (declare-fun tf6 (CList) Tuple4)
  (declare-fun reprNew (CList) Tuple4)
  (declare-fun mainNew (CList) MyBool)
  (assert (= (lq Zero Zero) MyFalse))
  (assert (forall ((x Nat)) (= (lq Zero (Succ x)) MyTrue)))
  (assert (forall ((x Nat)) (= (lq (Succ x) Zero) MyFalse)))
  (assert (forall ((y Nat) (x Nat)) (= (lq (Succ x) (Succ y)) (lq x y))))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyTrue x y) x)))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyFalse x y) y)))
  (assert (forall ((tv1 Nat) (tv0 Nat)) (= (max tv0 tv1) (ite2 (lq tv0 tv1) tv1 tv0))))
  (assert (forall ((x2 MyBool) (x1 Nat) (x0 Nat)) (= (snd3 (MakeTuple3 x0 x1 x2)) x1)))
  (assert (forall ((x Nat)) (= (gq Zero x) MyFalse)))
  (assert (forall ((x Nat)) (= (gq (Succ x) Zero) MyTrue)))
  (assert (forall ((y Nat) (x Nat)) (= (gq (Succ x) (Succ y)) (gq x y))))
  (assert (forall ((tv5 Nat)) (= (tf1 (Single tv5)) (MakeTuple3 tv5 tv5 MyTrue))))
  (assert (forall ((tv7 List) (tv6 Nat)) (= (tf1 (Cons tv6 tv7)) (MakeTuple3 tv6 (max (snd3 (tf0 tv7)) tv6) (gq tv6 (snd3 (tf0 tv7)))))))
  (assert (forall ((tv3 List)) (= (tf0 tv3) (tf1 tv3))))
  (assert (forall ((x2 MyBool) (x1 Nat) (x0 Nat)) (= (third3 (MakeTuple3 x0 x1 x2)) x2)))
  (assert (forall ((tv2 List)) (= (spec tv2) (third3 (tf0 tv2)))))
  (assert (forall ((tv14 Nat) (tv13 List)) (= (tf3 tv13 (Single tv14)) (Cons tv14 tv13))))
  (assert (forall ((tv16 List) (tv15 Nat) (tv13 List)) (= (tf3 tv13 (Cons tv15 tv16)) (Cons tv15 (tf2 tv16 tv13)))))
  (assert (forall ((tv11 List) (tv10 List)) (= (tf2 tv10 tv11) (tf3 tv11 tv10))))
  (assert (forall ((tv9 List) (tv8 List)) (= (catlist tv8 tv9) (tf2 tv8 tv9))))
  (assert (forall ((tv20 Nat)) (= (tf5 (Elt tv20)) (Single tv20))))
  (assert (forall ((tv22 CList) (tv21 CList)) (= (tf5 (Cat tv21 tv22)) (catlist (tf4 tv21) (tf4 tv22)))))
  (assert (forall ((tv18 CList)) (= (tf4 tv18) (tf5 tv18))))
  (assert (forall ((tv17 CList)) (= (repr tv17) (tf4 tv17))))
  (assert (forall ((tv23 CList)) (= (main tv23) (spec (repr tv23)))))
  (assert (forall ((x MyBool)) (= (myand MyFalse x) MyFalse)))
  (assert (forall ((true MyBool)) (= (myand true MyFalse) MyFalse)))
  (assert (= (myand MyTrue MyTrue) MyTrue))
  (assert (forall ((x1 Nat) (x0 MyBool)) (= (fst4 (MakeTuple4 x0 x1)) x0)))
  (assert (forall ((x1 Nat) (x0 MyBool)) (= (snd4 (MakeTuple4 x0 x1)) x1)))
  (assert (forall ((tv27 Nat)) (= (tf7 (Elt tv27)) (MakeTuple4 MyTrue tv27))))
  (assert (forall ((tv29 CList) (tv28 CList)) (= (tf7 (Cat tv28 tv29)) (MakeTuple4 (myand (fst4 (tf6 tv28)) (lq (snd4 (tf6 tv29)) (snd4 (tf6 tv28)))) (max (snd4 (tf6 tv29)) (snd4 (tf6 tv28)))))))
  (assert (forall ((tv25 CList)) (= (tf6 tv25) (tf7 tv25))))
  (assert (forall ((tv24 CList)) (= (reprNew tv24) (tf6 tv24))))
  (assert (forall ((tv30 CList)) (= (mainNew tv30) (fst4 (reprNew tv30)))))
  (assert (not (forall ((inp0 CList)) (= (main inp0) (mainNew inp0)))))
  (check-sat)
