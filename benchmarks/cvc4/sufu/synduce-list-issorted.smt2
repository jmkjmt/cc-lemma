  (declare-datatypes () ((MyBool (MyTrue) (MyFalse))))
  (declare-datatypes () ((Nat (Zero) (Succ (proj_Succ_0 Nat)))))
  (declare-datatypes () ((SList (Elt (proj_Elt_0 Nat)) (Cons (proj_Cons_0 Nat) (proj_Cons_1 SList)))))
  (declare-datatypes () ((CList (Single (proj_Single_0 Nat)) (Concat (proj_Concat_0 CList) (proj_Concat_1 CList)))))
  (declare-fun tf1 (SList SList) SList)
  (declare-fun tf0 (SList SList) SList)
  (declare-fun catlist (SList SList) SList)
  (declare-fun tf3 (CList) SList)
  (declare-fun tf2 (CList) SList)
  (declare-fun repr (CList) SList)
  (declare-datatypes () ((Tuple2 (MakeTuple2 (proj_MakeTuple2_0 Nat) (proj_MakeTuple2_1 Nat) (proj_MakeTuple2_2 MyBool)))))
  (declare-fun snd2 (Tuple2) Nat)
  (declare-fun myand (MyBool MyBool) MyBool)
  (declare-fun third2 (Tuple2) MyBool)
  (declare-fun lq (Nat Nat) MyBool)
  (declare-fun fst2 (Tuple2) Nat)
  (declare-fun tf5 (SList) Tuple2)
  (declare-fun tf4 (SList) Tuple2)
  (declare-fun spec (SList) MyBool)
  (declare-fun main (CList) MyBool)
  (declare-datatypes () ((Tuple3 (MakeTuple3 (proj_MakeTuple3_0 MyBool) (proj_MakeTuple3_1 Nat) (proj_MakeTuple3_2 Nat)))))
  (declare-fun fst3 (Tuple3) MyBool)
  (declare-fun third3 (Tuple3) Nat)
  (declare-fun snd3 (Tuple3) Nat)
  (declare-fun tf7 (CList) Tuple3)
  (declare-fun tf6 (CList) Tuple3)
  (declare-fun reprNew (CList) Tuple3)
  (declare-fun mainNew (CList) MyBool)
  (assert (forall ((tv6 Nat) (tv5 SList)) (= (tf1 tv5 (Elt tv6)) (Cons tv6 tv5))))
  (assert (forall ((tv8 SList) (tv7 Nat) (tv5 SList)) (= (tf1 tv5 (Cons tv7 tv8)) (Cons tv7 (tf0 tv8 tv5)))))
  (assert (forall ((tv3 SList) (tv2 SList)) (= (tf0 tv2 tv3) (tf1 tv3 tv2))))
  (assert (forall ((tv1 SList) (tv0 SList)) (= (catlist tv0 tv1) (tf0 tv0 tv1))))
  (assert (forall ((tv12 Nat)) (= (tf3 (Single tv12)) (Elt tv12))))
  (assert (forall ((tv14 CList) (tv13 CList)) (= (tf3 (Concat tv13 tv14)) (catlist (tf2 tv13) (tf2 tv14)))))
  (assert (forall ((tv10 CList)) (= (tf2 tv10) (tf3 tv10))))
  (assert (forall ((tv9 CList)) (= (repr tv9) (tf2 tv9))))
  (assert (forall ((x2 MyBool) (x1 Nat) (x0 Nat)) (= (snd2 (MakeTuple2 x0 x1 x2)) x1)))
  (assert (forall ((x MyBool)) (= (myand MyFalse x) MyFalse)))
  (assert (forall ((true MyBool)) (= (myand true MyFalse) MyFalse)))
  (assert (= (myand MyTrue MyTrue) MyTrue))
  (assert (forall ((x2 MyBool) (x1 Nat) (x0 Nat)) (= (third2 (MakeTuple2 x0 x1 x2)) x2)))
  (assert (= (lq Zero Zero) MyFalse))
  (assert (forall ((x Nat)) (= (lq Zero (Succ x)) MyTrue)))
  (assert (forall ((x Nat)) (= (lq (Succ x) Zero) MyFalse)))
  (assert (forall ((y Nat) (x Nat)) (= (lq (Succ x) (Succ y)) (lq x y))))
  (assert (forall ((x2 MyBool) (x1 Nat) (x0 Nat)) (= (fst2 (MakeTuple2 x0 x1 x2)) x0)))
  (assert (forall ((tv18 Nat)) (= (tf5 (Elt tv18)) (MakeTuple2 tv18 tv18 MyTrue))))
  (assert (forall ((tv20 SList) (tv19 Nat)) (= (tf5 (Cons tv19 tv20)) (MakeTuple2 tv19 (snd2 (tf4 tv20)) (myand (third2 (tf4 tv20)) (lq tv19 (fst2 (tf4 tv20))))))))
  (assert (forall ((tv16 SList)) (= (tf4 tv16) (tf5 tv16))))
  (assert (forall ((tv15 SList)) (= (spec tv15) (third2 (tf4 tv15)))))
  (assert (forall ((tv21 CList)) (= (main tv21) (spec (repr tv21)))))
  (assert (forall ((x2 Nat) (x1 Nat) (x0 MyBool)) (= (fst3 (MakeTuple3 x0 x1 x2)) x0)))
  (assert (forall ((x2 Nat) (x1 Nat) (x0 MyBool)) (= (third3 (MakeTuple3 x0 x1 x2)) x2)))
  (assert (forall ((x2 Nat) (x1 Nat) (x0 MyBool)) (= (snd3 (MakeTuple3 x0 x1 x2)) x1)))
  (assert (forall ((tv25 Nat)) (= (tf7 (Single tv25)) (MakeTuple3 MyTrue tv25 tv25))))
  (assert (forall ((tv27 CList) (tv26 CList)) (= (tf7 (Concat tv26 tv27)) (MakeTuple3 (myand (myand (fst3 (tf6 tv26)) (fst3 (tf6 tv27))) (lq (third3 (tf6 tv26)) (snd3 (tf6 tv27)))) (snd3 (tf6 tv26)) (third3 (tf6 tv27))))))
  (assert (forall ((tv23 CList)) (= (tf6 tv23) (tf7 tv23))))
  (assert (forall ((tv22 CList)) (= (reprNew tv22) (tf6 tv22))))
  (assert (forall ((tv28 CList)) (= (mainNew tv28) (fst3 (reprNew tv28)))))
  (assert (not (forall ((inp0 CList)) (= (main inp0) (mainNew inp0)))))
  (check-sat)