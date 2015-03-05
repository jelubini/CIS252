--
--  Starter file for HW #5: rbac.hs
--
--  Contains basic type definitions for RBAC components, plus some examples
--

-- import the module Data.List, so that you can use the following two
-- functions:
--
--  (i) nub: takes a list and removes excessive copies of elements in list
--       for example:  nub [1,3,1,5,6,3] returns [1,3,5,6]
--  
--  (ii) intersect: takes two lists and returns those elements that
--            appear in both lists
--       for example:  intersect [1,3,5,4,2] [2,4,6,8,1] returns [1,4,2]
--

import Data.List

--
-- Users, permissions, and roles are all represented by Strings.  
--
--     If you wanted more interesting structures, you could define them
--     here (but we won't).
--
type User = String
type Perm = String
type Role = String


--
-- A user assignment is a collection of user-role pairs.
-- A permission assignment is a collection of permission-role pairs.
--
type UA = [(User,Role)]
type PA = [(Perm,Role)]

-- A separation-of-duty relation is a collection of pairs of form (rs,n):
--    each rs is itself a collection of roles
--    each n is a "threshhold limit" of potential conflicts within 
--           a set of roles

type SSD = [([Role],Int)]


------------------------------------------------------------------------
--
-- Some samples
--
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Example #1
--
--     Here we have several individuals who belong to roles that are
--     related to University life: professors, students, staff, tas.
--
--     We also associate certain permissions (think: actions or verbs)
--     with the various roles.  These permissions include things like
--     griping (about early morning lectures), grading (exams,
--     homeworks, etc), sleeping in lecture, assisting people, and
--     submitting grades to the registrar 


-- Note that Bob belongs to both the staff and student roles, and Eve
-- belongs to both the student and ta roles.
ua1 :: UA
ua1 = [("Ava","professor"), ("Bob","student"), ("Cal","professor"), 
       ("Bob","staff"), ("Dana","staff"), ("Eve","student"), ("Eve","ta")]

--
--  Professors can do the following: gripe, grade, submit grades
--  Students can: gripe, sleep (lucky students!)
--  Staff can: assist
--  TAs can: grade
--
pa1 :: PA
pa1 = [("gripe","professor"), ("gripe","student"),
       ("grade","professor"), ("grade","ta"), ("sleep","student"), 
       ("assist","staff"), ("submit grades","professor")]

--
--   This static separation-of-duty relation indicates that:
--       (i) no one can be both a student and a TA
--           [In reality, you'd want to make this specific to a single
--           course: no student in CIS 252 can be a TA for CIS 252 at
--           the same time.  However, this is a toy example, so let's
--           keep it simple.]
--

ssd1 :: SSD
ssd1 = [(["student","ta"],2)]


------------------------------------------------------------------------
-- Example #2
--
--     Here we have several individuals who belong to roles related to
--     political parties, wealth, and ethics (honesty).
--
--     We also associate certain permissions (think: actions or verbs)
--     with the various roles.  These permissions include things like
--     raising/lowering taxes, bickering, pandering, buying votes, and
--     whistleblowing. 

ua2 :: UA
ua2 = [("Rob","republican"), ("Ima","independent"), ("Del","democrat"), 
       ("Pandora","republican"), ("Pandora","democrat"),
       ("Rob","rich"), ("Rob","honest"), 
       ("Ima","politician"), ("Ima","rich"), ("Ima","honest"),
       ("Del","politician"), ("Del","honest")
      ]

pa2 :: PA
pa2 = [("lower taxes","republican"), ("raise taxes","democrat"),
       ("raise taxes","independent"), ("lower taxes","independent"),
       ("bicker","republican"), ("bicker","democrat"), 
       ("buy votes","rich"), ("whistleblow","honest"), ("pander","politician")
 ]

--
--   This static separation-of-duty relation indicates that:
--       (i) no one can be in 2 or more of the following roles:
--            republican, democrat, independent   (i.e., everyone is
--            associated with at most one political party)
--
--      (ii) no one can be in 3 or more of the following roles:
--           rich, honest, politician
--        That is, there is no one who is rich AND honest AND a
--        politician.   (At least according to this SSD relation: I am
--        sure there are good people in the world....)
--
ssd2 :: SSD
ssd2 = [(["republican","democrat","independent"],2), 
	(["rich","honest","politician"],3)
       ]


------------------------------------------------------------------------
-- roles user ua
--      returns the list of roles that user assignment ua associates
--      with user 
------------------------------------------------------------------------

roles :: User -> UA -> [Role]
roles user ua = [ r | (u,r) <- ua, u == user ]

------------------------------------------------------------------------
--  Your code goes here
------------------------------------------------------------------------

------------------------------------------------------------------------
--
--  Problem #1:
--
--    authUsers role ua
--        returns the list of users that ua authorizes for role
------------------------------------------------------------------------

authUsers :: Role -> UA -> [User]
authUsers r ua = [ u | (u, ro) <- ua, r == ro]



------------------------------------------------------------------------
--
--  Problem #2:
--
--    authPerms role pa
--       returns the list of permissions that pa associates with role
--
------------------------------------------------------------------------

authPerms :: Role -> PA -> [Perm]
authPerms role pa = [ p | (p, r) <- pa, role == r]


------------------------------------------------------------------------
--
--  Problem #3:
--
--    userPerms user ua  pa
--       returns the list of permissions that ua and pa together
--       associate with user
--
--      user is granted all permissions that pa associates with at
--      least one of the roles authorized for user by ua
--
------------------------------------------------------------------------

userPerms :: User -> UA  -> PA -> [Perm]
userPerms u ua pa = nub [ p | (p, r) <- pa, elem (u, r) ua]


------------------------------------------------------------------------
--
--  Problem #4:
--
--    refMonitor (ua,pa) user perm
--       determines whether the policy (ua,pa) grants user the
--       permission perm 
--
------------------------------------------------------------------------

refMonitor :: (UA, PA) -> User -> Perm -> Bool
refMonitor (ua, pa) u p = elem (u, p) (nub [ (u, perm) | (perm, role) <- pa, elem (u, role) ua])



------------------------------------------------------------------------
--
--  Problem #5:
--
--    violation rset (rs,n)
--       determines whether the collection of roles rset violates the
--       separation-of-duty constraint (rs,n)
--
------------------------------------------------------------------------

violation :: [Role] -> ([Role],Int) -> Bool
violation rs1 (rs, n) = (length (intersect rs rs1)) >= n



------------------------------------------------------------------------
--
--  Problem #6:
--
--    userConflicts user ua ssd 
--       determines whether ua authorizes user for a collection of roles
--       that violates the ssd relation
--
------------------------------------------------------------------------

userConflicts :: User -> UA -> SSD -> Bool
userConflicts u ua ssd = not (null [ 1 | (role, n) <- ssd, (length (intersect role ([ r | (usr, r) <- ua, usr == u]))) >= n])


------------------------------------------------------------------------
--
--  Problem #7:
--
--    conflicts ua ssd 
--       returns the list of users whom the ua authorizes for a
--       collection of roles that violates the ssd relation
--
------------------------------------------------------------------------

-- conflicts :: UA -> SSD -> [User]
-- conflicts ua ssd [ usr | (usr, role) <- ua, ]

