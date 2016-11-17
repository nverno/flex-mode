(require 'flex-mode)
(require 'ert)

(defun flex--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "flex--test")
    (message "cant run without ert.")))

(provide 'flex-tests)
