;; garbage collection
(setq gc-cons-percentage 0.2)
(setq gc-cons-threshold (* 200 1000 1000))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 20 1000 1000))))

;; Improve LSP/Process performance
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
