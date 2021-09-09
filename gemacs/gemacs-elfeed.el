;; Elfeed - move to separate file
(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '(("http://export.arxiv.org/rss/cs.CL" computation language)
	("http://export.arxiv.org/rss/cs.CR" cryptography security)
	("http://export.arxiv.org/rss/cs.SY" systems)
	("http://export.arxiv.org/rss/cs.stat" statistics)
	("http://export.arxiv.org/rss/cs.econ" economics)
	("http://export.arxiv.org/rss/cs.CL" technology)
	("http://export.arxiv.org/rss/math.NT" numbertheory)
	("http://export.arxiv.org/rss/math.DS" systems)
	))

