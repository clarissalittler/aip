(defun random-elt (lst)
  (nth (random (length lst)) lst))

(defun mappend (fun lst)
  (apply #'append (mapcar fun lst)))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> chicken house jellyfish werewolf)
    (Verb -> tosses licks snuggles)))

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  (car rule))

(defun rule-rhs (rule)
  (cdr (cdr rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((rewrites phrase)
	 (generate (random-elt (rewrites phrase))))
	(t (list phrase))))
