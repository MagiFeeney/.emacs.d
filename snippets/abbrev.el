(define-abbrev-table 'global-abbrev-table
	'(;; Arrows
	  ("ra" "→")
	  ("la" "←")
	  ("ua" "↑")
	  ("da" "↓")

	  ;; Emojis for context markers
	  ("todo"  "👷 TODO:")
	  ("fixme" "🔥 FIXME:")
	  ("note"  "📎 NOTE:")
	  ("hack"  "👾 HACK:")
	  ("pinch"  "🤌")
	  ("smile"  "😄")
	  ("party" "🎉")
	  ("up"  "☝️")
	  ("applause" "👏")
	  ("manyapplauses" "👏👏👏👏👏👏👏👏")
	  ("heart" "❤️")

	  ;; NerdFonts
	  ("nerdfolder" " ")
	  ("nerdgit" "")
	  ("nerdemacs" "")

	  ;; HTML entities
	  ("nb" "&nbsp;")
	  ("lt" "&lt;")
	  ("gt" "&gt;")

	  ;; Markdown
	  ("cb" "```@\n\n```"
	   (lambda () (search-backward "@") (delete-char 1)))

	  ;; ORG
	  ("ocb" "#+BEGIN_SRC @\n\n#+END_SRC"
	   (lambda () (search-backward "@") (delete-char 1)))
	  ("oheader" "#+TITLE: ###1###\n#+AUTHOR: ###2###\n#+EMAIL: ###3###\n#+OPTIONS: toc:nil\n"
	   emacs-solo/abbrev--replace-placeholders)

	  ;; JS/TS snippets
	  ("imp" "import { ###1### } from '###2###';"
	   emacs-solo/abbrev--replace-placeholders)
	  ("fn" "function ###1### () {\n ###@### ;\n};"
	   emacs-solo/abbrev--replace-placeholders)
	  ("clog" "console.log(\">>> LOG:\", { ###@### })"
	   emacs-solo/abbrev--replace-placeholders)
	  ("cwarn" "console.warn(\">>> WARN:\", { ###@### })"
	   emacs-solo/abbrev--replace-placeholders)
	  ("cerr" "console.error(\">>> ERR:\", { ###@### })"
	   emacs-solo/abbrev--replace-placeholders)
	  ("afn" "async function() {\n  \n}"
	   (lambda () (search-backward "}") (forward-line -1) (end-of-line)))
	  ("ife" "(function() {\n  \n})();"
	   (lambda () (search-backward ")();") (forward-line -1) (end-of-line)))
	  ("esdeps" "// eslint-disable-next-line react-hooks/exhaustive-deps"
	   (lambda () (search-backward ")();") (forward-line -1) (end-of-line)))
	  ("eshooks" "// eslint-disable-next-line react-hooks/rules-of-hooks"
	   (lambda () (search-backward ")();") (forward-line -1) (end-of-line)))

	  ;; React/JSX
	  ("rfc" "const ###1### = () => {\n  return (\n    <div>###2###</div>\n  );\n};"
	   emacs-solo/abbrev--replace-placeholders)))
