  (declare-datatypes () ((MyBool (MyTrue) (MyFalse))))
  (declare-datatypes () ((Unit (Null))))
  (declare-datatypes () ((Nat (Zero) (Succ (proj_Succ_0 Nat)))))
  (declare-datatypes () ((CList (Cnil (proj_Cnil_0 Unit)) (Single (proj_Single_0 Nat)) (Concat (proj_Concat_0 CList) (proj_Concat_1 CList)))))
  (declare-datatypes () ((List (Nil (proj_Nil_0 Unit)) (Cons (proj_Cons_0 Nat) (proj_Cons_1 List)))))
  (declare-fun gq (Nat Nat) MyBool)
  (declare-fun myand (MyBool MyBool) MyBool)
  (declare-fun tf1 (CList) MyBool)
  (declare-fun tf0 (CList) MyBool)
  (declare-fun allpos (CList) MyBool)
  (declare-fun tf3 (List List) List)
  (declare-fun tf2 (List List) List)
  (declare-fun cat (List List) List)
  (declare-fun tf5 (CList) List)
  (declare-fun tf4 (CList) List)
  (declare-fun repr (CList) List)
  (declare-fun lq (Nat Nat) MyBool)
  (declare-fun ite2 (MyBool Nat Nat) Nat)
  (declare-fun min (Nat Nat) Nat)
  (declare-fun max (Nat Nat) Nat)
  (declare-datatypes () ((Tuple3 (MakeTuple3 (proj_MakeTuple3_0 Nat) (proj_MakeTuple3_1 Nat)))))
  (declare-fun fst3 (Tuple3) Nat)
  (declare-fun snd3 (Tuple3) Nat)
  (declare-fun tf7 (List) Tuple3)
  (declare-fun tf6 (List) Tuple3)
  (declare-fun spec (List) Nat)
  (declare-fun tf9 (CList) CList)
  (declare-fun tf8 (CList) CList)
  (declare-fun target (CList) CList)
  (declare-fun main (CList) Nat)
  (declare-fun mainNew (CList) Nat)
  (assert (forall ((x Nat)) (= (gq Zero x) MyFalse)))
  (assert (forall ((x Nat)) (= (gq (Succ x) Zero) MyTrue)))
  (assert (forall ((y Nat) (x Nat)) (= (gq (Succ x) (Succ y)) (gq x y))))
  (assert (forall ((x MyBool)) (= (myand MyFalse x) MyFalse)))
  (assert (forall ((true MyBool)) (= (myand true MyFalse) MyFalse)))
  (assert (= (myand MyTrue MyTrue) MyTrue))
  (assert (forall ((tv3 Unit)) (= (tf1 (Cnil tv3)) MyTrue)))
  (assert (forall ((tv4 Nat)) (= (tf1 (Single tv4)) (gq tv4 Zero))))
  (assert (forall ((tv6 CList) (tv5 CList)) (= (tf1 (Concat tv5 tv6)) (myand (tf0 tv5) (tf0 tv6)))))
  (assert (forall ((tv1 CList)) (= (tf0 tv1) (tf1 tv1))))
  (assert (forall ((tv0 CList)) (= (allpos tv0) (tf0 tv0))))
  (assert (forall ((tv13 Nat) (tv14 List) (tv12 List)) (= (tf3 tv12 (Cons tv13 tv14)) (Cons tv13 (tf2 tv14 tv12)))))
  (assert (forall ((tv15 Unit) (tv12 List)) (= (tf3 tv12 (Nil tv15)) tv12)))
  (assert (forall ((tv10 List) (tv9 List)) (= (tf2 tv9 tv10) (tf3 tv10 tv9))))
  (assert (forall ((tv8 List) (tv7 List)) (= (cat tv7 tv8) (tf2 tv7 tv8))))
  (assert (forall ((tv19 Unit)) (= (tf5 (Cnil tv19)) (Nil Null))))
  (assert (forall ((tv20 Nat)) (= (tf5 (Single tv20)) (Cons tv20 (Nil Null)))))
  (assert (forall ((tv22 CList) (tv21 CList)) (= (tf5 (Concat tv21 tv22)) (cat (tf4 tv21) (tf4 tv22)))))
  (assert (forall ((tv17 CList)) (= (tf4 tv17) (tf5 tv17))))
  (assert (forall ((tv16 CList)) (= (repr tv16) (tf4 tv16))))
  (assert (= (lq Zero Zero) MyFalse))
  (assert (forall ((x Nat)) (= (lq Zero (Succ x)) MyTrue)))
  (assert (forall ((x Nat)) (= (lq (Succ x) Zero) MyFalse)))
  (assert (forall ((y Nat) (x Nat)) (= (lq (Succ x) (Succ y)) (lq x y))))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyTrue x y) x)))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyFalse x y) y)))
  (assert (forall ((tv24 Nat) (tv23 Nat)) (= (min tv23 tv24) (ite2 (lq tv23 tv24) tv23 tv24))))
  (assert (forall ((tv26 Nat) (tv25 Nat)) (= (max tv25 tv26) (ite2 (lq tv25 tv26) tv26 tv25))))
  (assert (forall ((x1 Nat) (x0 Nat)) (= (fst3 (MakeTuple3 x0 x1)) x0)))
  (assert (forall ((x1 Nat) (x0 Nat)) (= (snd3 (MakeTuple3 x0 x1)) x1)))
  (assert (forall ((tv30 Unit)) (= (tf7 (Nil tv30)) (MakeTuple3 Zero Zero))))
  (assert (forall ((tv32 List) (tv31 Nat)) (= (tf7 (Cons tv31 tv32)) (MakeTuple3 (min tv31 (fst3 (tf6 tv32))) (min (snd3 (tf6 tv32)) (max (fst3 (tf6 tv32)) tv31))))))
  (assert (forall ((tv28 List)) (= (tf6 tv28) (tf7 tv28))))
  (assert (forall ((tv27 List)) (= (spec tv27) (snd3 (tf6 tv27)))))
  (assert (forall ((tv36 Unit)) (= (tf9 (Cnil tv36)) (Cnil Null))))
  (assert (forall ((tv37 Nat)) (= (tf9 (Single tv37)) (Single tv37))))
  (assert (forall ((tv39 CList) (tv38 CList)) (= (tf9 (Concat tv38 tv39)) (Concat (tf8 tv38) (tf8 tv39)))))
  (assert (forall ((tv34 CList)) (= (tf8 tv34) (tf9 tv34))))
  (assert (forall ((tv33 CList)) (= (target tv33) (tf8 tv33))))
  (assert (forall ((tv40 CList)) (= (main tv40) (ite2 (allpos tv40) (spec (repr (target tv40))) Zero))))
  (assert (forall ((tv41 CList)) (= (mainNew tv41) (ite2 (allpos tv41) Zero Zero))))
  (assert (not (forall ((inp0 CList)) (= (main inp0) (mainNew inp0)))))
  (check-sat)