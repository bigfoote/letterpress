(defparameter *file* "./words.txt" "Location of the dictionary file")

(defun valid-words (letters)
  "For a set of letters, returns all words in dictionary that can be made from them."
  (let ((results '()))
    (with-open-file (in *file* :direction :input)
      (do ((word (read-line in nil 'done) (read-line in nil 'done)))
	  ((equal word 'done))
	(if (can-be-made-p word letters)
	    (push word results))))
    results))

(defun can-be-made-p (word letters)
  (cond ((equal word "") t)
	(t (let ((letter (char word 0)))
	     (and (find letter letters)
		  (can-be-made-p (subseq word 1) (remove letter letters :count 1)))))))

(defun score (word opp-letters free-letters)
  (cond ((or (equal word "")
	     (and (equal opp-letters "")
		  (equal free-letters ""))) 0)
	((find (char word 0) opp-letters) (+ 2 (score (subseq word 1)
						      (remove (char word 0) opp-letters :count 1)
						      free-letters)))
	((find (char word 0) free-letters) (+ 1 (score (subseq word 1)
						       opp-letters
						       (remove (char word 0) free-letters :count 1))))
	(t (score (subseq word 1) opp-letters free-letters))))

(defun suggest-words (letters opp-letters free-letters)
  (subseq (sort (valid-words letters) #'> :key #'(lambda (word)
                                                   (score word opp-letters free-letters)))
          0
          10))
