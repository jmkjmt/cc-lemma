  (declare-datatypes () ((MyBool (MyTrue) (MyFalse))))
  (declare-datatypes () ((Nat (Zero) (Succ (proj_Succ_0 Nat)))))
  (declare-datatypes () ((List (Elt (proj_Elt_0 Nat)) (Cons (proj_Cons_0 Nat) (proj_Cons_1 List)))))
  (declare-datatypes () ((CList (Single (proj_Single_0 Nat)) (Concat (proj_Concat_0 CList) (proj_Concat_1 CList)))))
  (declare-fun tf1 (List List) List)
  (declare-fun tf0 (List List) List)
  (declare-fun cat (List List) List)
  (declare-fun tf3 (CList) List)
  (declare-fun tf2 (CList) List)
  (declare-fun repr (CList) List)
  (declare-fun lq (Nat Nat) MyBool)
  (declare-fun ite2 (MyBool Nat Nat) Nat)
  (declare-fun max (Nat Nat) Nat)
  (declare-fun tf5 (CList) Nat)
  (declare-fun tf4 (CList) Nat)
  (declare-fun lmax (CList) Nat)
  (declare-fun min (Nat Nat) Nat)
  (declare-fun tf7 (CList) Nat)
  (declare-fun tf6 (CList) Nat)
  (declare-fun lmin (CList) Nat)
  (declare-fun myand (MyBool MyBool) MyBool)
  (declare-fun tf9 (CList) MyBool)
  (declare-fun tf8 (CList) MyBool)
  (declare-fun ispart (CList) MyBool)
  (declare-fun tf11 (List) Nat)
  (declare-fun tf10 (List) Nat)
  (declare-fun spec (List) Nat)
  (declare-fun tf13 (CList CList) CList)
  (declare-fun tf12 (CList) CList)
  (declare-fun target (CList) CList)
  (declare-fun main (CList) Nat)
  (declare-fun tf15 (CList) Nat)
  (declare-fun tf14 (CList) Nat)
  (declare-fun targetNew (CList) Nat)
  (declare-fun mainNew (CList) Nat)
  (assert (forall ((tv6 Nat) (tv5 List)) (= (tf1 tv5 (Elt tv6)) (Cons tv6 tv5))))
  (assert (forall ((tv8 List) (tv7 Nat) (tv5 List)) (= (tf1 tv5 (Cons tv7 tv8)) (Cons tv7 (tf0 tv8 tv5)))))
  (assert (forall ((tv3 List) (tv2 List)) (= (tf0 tv2 tv3) (tf1 tv3 tv2))))
  (assert (forall ((tv1 List) (tv0 List)) (= (cat tv0 tv1) (tf0 tv0 tv1))))
  (assert (forall ((tv12 Nat)) (= (tf3 (Single tv12)) (Elt tv12))))
  (assert (forall ((tv14 CList) (tv13 CList)) (= (tf3 (Concat tv13 tv14)) (cat (tf2 tv13) (tf2 tv14)))))
  (assert (forall ((tv10 CList)) (= (tf2 tv10) (tf3 tv10))))
  (assert (forall ((tv9 CList)) (= (repr tv9) (tf2 tv9))))
  (assert (= (lq Zero Zero) MyFalse))
  (assert (forall ((x Nat)) (= (lq Zero (Succ x)) MyTrue)))
  (assert (forall ((x Nat)) (= (lq (Succ x) Zero) MyFalse)))
  (assert (forall ((y Nat) (x Nat)) (= (lq (Succ x) (Succ y)) (lq x y))))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyTrue x y) x)))
  (assert (forall ((y Nat) (x Nat)) (= (ite2 MyFalse x y) y)))
  (assert (forall ((tv16 Nat) (tv15 Nat)) (= (max tv15 tv16) (ite2 (lq tv15 tv16) tv16 tv15))))
  (assert (forall ((tv20 Nat)) (= (tf5 (Single tv20)) tv20)))
  (assert (forall ((tv22 CList) (tv21 CList)) (= (tf5 (Concat tv21 tv22)) (max (tf4 tv21) (tf4 tv22)))))
  (assert (forall ((tv18 CList)) (= (tf4 tv18) (tf5 tv18))))
  (assert (forall ((tv17 CList)) (= (lmax tv17) (tf4 tv17))))
  (assert (forall ((tv24 Nat) (tv23 Nat)) (= (min tv23 tv24) (ite2 (lq tv23 tv24) tv23 tv24))))
  (assert (forall ((tv28 Nat)) (= (tf7 (Single tv28)) tv28)))
  (assert (forall ((tv30 CList) (tv29 CList)) (= (tf7 (Concat tv29 tv30)) (min (tf6 tv29) (tf6 tv30)))))
  (assert (forall ((tv26 CList)) (= (tf6 tv26) (tf7 tv26))))
  (assert (forall ((tv25 CList)) (= (lmin tv25) (tf6 tv25))))
  (assert (forall ((x MyBool)) (= (myand MyFalse x) MyFalse)))
  (assert (forall ((true MyBool)) (= (myand true MyFalse) MyFalse)))
  (assert (= (myand MyTrue MyTrue) MyTrue))
  (assert (forall ((tv34 Nat)) (= (tf9 (Single tv34)) MyTrue)))
  (assert (forall ((tv36 CList) (tv35 CList)) (= (tf9 (Concat tv35 tv36)) (myand (lq (lmax tv35) (lmin tv36)) (myand (tf8 tv35) (tf8 tv36))))))
  (assert (forall ((tv32 CList)) (= (tf8 tv32) (tf9 tv32))))
  (assert (forall ((tv31 CList)) (= (ispart tv31) (tf8 tv31))))
  (assert (forall ((tv40 Nat)) (= (tf11 (Elt tv40)) tv40)))
  (assert (forall ((tv42 List) (tv41 Nat)) (= (tf11 (Cons tv41 tv42)) (min tv41 (tf10 tv42)))))
  (assert (forall ((tv38 List)) (= (tf10 tv38) (tf11 tv38))))
  (assert (forall ((tv37 List)) (= (spec tv37) (tf10 tv37))))
  (assert (forall ((tv47 Nat) (tv46 CList)) (= (tf13 tv46 (Single tv47)) tv46)))
  (assert (forall ((tv49 CList) (tv48 CList) (tv46 CList)) (= (tf13 tv46 (Concat tv48 tv49)) (Concat (tf12 tv48) tv49))))
  (assert (forall ((tv44 CList)) (= (tf12 tv44) (tf13 tv44 tv44))))
  (assert (forall ((tv43 CList)) (= (target tv43) (tf12 tv43))))
  (assert (forall ((tv50 CList)) (= (main tv50) (ite2 (ispart tv50) (spec (repr (target tv50))) Zero))))
  (assert (forall ((tv54 Nat)) (= (tf15 (Single tv54)) tv54)))
  (assert (forall ((tv56 CList) (tv55 CList)) (= (tf15 (Concat tv55 tv56)) (tf14 tv55))))
  (assert (forall ((tv52 CList)) (= (tf14 tv52) (tf15 tv52))))
  (assert (forall ((tv51 CList)) (= (targetNew tv51) (tf14 tv51))))
  (assert (forall ((tv57 CList)) (= (mainNew tv57) (ite2 (ispart tv57) (targetNew tv57) Zero))))
  (assert (not (forall ((inp0 CList)) (= (main inp0) (mainNew inp0)))))
  (check-sat)
