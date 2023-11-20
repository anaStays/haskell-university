--Алгоритм Линдермайера (L-системы)
--аксиомы:
--a->ab
--b->a
--
--a ab aba abaab

next:: Char ->String
next 'a' = "ab"
next 'b' = "a"
