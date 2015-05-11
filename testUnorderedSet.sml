open TextIO
open UnorderedSet

fun fail exp got =
    output (stdErr, "fail expected [" ^ exp ^ "], got [" ^ got ^ "]");

fun boolExpect n res1 res2 =
    (output (stdErr, "test " ^ Int.toString n ^ ": ");
     if res1 = res2 then output (stdErr, "pass")
     else fail (Bool.toString res1) (Bool.toString res2);
     output (stdErr, "\n"))


fun setExpect n s1 s2 =
    (output (stdErr, "test " ^ Int.toString n ^ ": ");
     if equal (s1, s2) then output (stdErr, "pass")
     else fail (toString Int.toString s1) (toString Int.toString s2);
     output (stdErr, "\n"))


fun testEquality () =
    let
        val empty1 = empty ()
        val empty2 = empty ()
        val s1 = addList (empty (), [3, 2, 1])
        val s2 = addList (empty (), [1, 2])
    in
        boolExpect 1 true (equal (empty1, empty2));
        boolExpect 2 false (equal (s1, s2));
        boolExpect 3 false (equal (s2, empty1));
        boolExpect 4 true (isSubset (s2, s1));
        boolExpect 5 true (isSubset (empty1, s1));
        boolExpect 6 false (isSubset (s1, empty1));
        boolExpect 7 true (equal (s1, add (s2, 3)));
        boolExpect 8 true (isEmpty empty1);
        boolExpect 9 false (isEmpty s1)
    end


fun testUnion () =
    let
        val s1 = addList (empty (), [1, 2, 3])
        val s2 = addList (empty (), [4, 5, 6])
        val s3 = addList (empty (), [1, 2, 3, 4, 5, 6])
    in
        setExpect 1 s3 (union (s1, s2));
        setExpect 2 s1 (union (empty (), s1))
    end


fun testIntersection () =
    let
        val s1 = addList (empty (), [1, 2, 3])
        val s2 = addList (empty (), [4, 5, 6])
        val s3 = singleton 3
        val s4 = addList (empty (), [3, 4, 5])
    in
        setExpect 1 (empty ()) (intersection (s1, s2));
        setExpect 2 s3 (intersection (s3, s1));
        setExpect 3 s3 (intersection (s1, s4))
    end


fun testDifference () =
    let
        val s1 = addList (empty (), [1, 2, 3])
        val s2 = addList (empty (), [4, 5, 6])
        val s3 = addList (empty (), [1, 2, 3, 4, 5, 6])
    in
        setExpect 1 s1 (difference (s1, s2));
        setExpect 2 (empty ()) (difference (s1, s1));
        setExpect 3 s1 (difference (s3, s2))
    end


fun testUnorderedSet () =
    (print "Testing equality...\n";
     testEquality ();
     print "\nTesting union...\n";
     testUnion ();
     print "\nTesting intersection...\n";
     testIntersection ();
     print "\nTesting difference...\n";
     testDifference ())

val _ = testUnorderedSet ()
