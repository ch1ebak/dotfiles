;;; dired-clipboard-tests.el --- Tests for dired-clipboard -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired-clipboard)

(ert-deftest dired-clipboard-copy-backends-skip-unavailable ()
  "File clipboard copy uses the first available successful backend."
  (let* ((calls nil)
         (dired-clipboard-file-clipboard-backends
          '(missing disabled failing ok later))
         (dired-clipboard-file-clipboard-backend-alist
          `((disabled
             :available ,(lambda (_operation _payload) nil)
             :copy ,(lambda (_payload)
                      (push 'disabled calls)
                      t))
            (failing
             :copy ,(lambda (_payload)
                      (push 'failing calls)
                      nil))
            (ok
             :copy ,(lambda (_payload)
                      (push 'ok calls)
                      t))
            (later
             :copy ,(lambda (_payload)
                      (push 'later calls)
                      t)))))
    (should (eq (dired-clipboard--copy-with-file-backends
                 '(:local-files ("/tmp/example")))
                'ok))
    (should (equal (nreverse calls) '(failing ok)))))

(ert-deftest dired-clipboard-files-from-clipboard-falls-back-after-missing-files ()
  "Paste falls through when a backend returns paths Emacs cannot see."
  (let* ((existing (make-temp-file "dired-clipboard-test"))
         (missing (concat existing "-missing"))
         (kill-ring (list existing))
         (kill-ring-yank-pointer kill-ring)
         (dired-clipboard-file-clipboard-backends '(backend))
         (dired-clipboard-file-clipboard-backend-alist
          (list (list 'backend :paste (lambda () (list missing))))))
    (unwind-protect
        (should (equal (dired-clipboard--files-from-clipboard)
                       (list existing)))
      (delete-file existing))))

(ert-deftest dired-clipboard-paste-backends-skip-missing-files ()
  "File clipboard paste tries the next backend when paths are not visible."
  (let* ((existing (make-temp-file "dired-clipboard-test"))
         (missing (concat existing "-missing"))
         (dired-clipboard-file-clipboard-backends '(first second)))
    (let ((dired-clipboard-file-clipboard-backend-alist
           (list (list 'first :paste (lambda () (list missing)))
                 (list 'second :paste (lambda () (list existing))))))
      (unwind-protect
          (should (equal (dired-clipboard--files-from-file-backends)
                         (list existing)))
        (delete-file existing)))))

(ert-deftest dired-clipboard-cut-payload-records-operation ()
  "Cut payloads advertise cut semantics where supported."
  (let ((file (make-temp-file "dired-clipboard-test")))
    (unwind-protect
        (let* ((uri (dired-clipboard--file-to-uri file))
               (payload (dired-clipboard--file-clipboard-payload
                         (list file) 'cut))
               (text (plist-get payload :text))
               (copied-files-list (plist-get payload :copied-files-list)))
          (should (eq (plist-get payload :operation) 'cut))
          (should (eq (get-text-property
                       0 'dired-clipboard-operation text)
                      'cut))
          (should (equal copied-files-list
                         (concat "cut\n" uri)))
          (should (equal (dired-clipboard--parse-copied-files-content
                          copied-files-list)
                         (list :operation 'cut :files (list file)))))
      (delete-file file))))

(ert-deftest dired-clipboard-content-from-kill-ring-preserves-cut ()
  "Dired-to-Dired cut falls back through the kill ring."
  (let* ((file (make-temp-file "dired-clipboard-test"))
         (text (copy-sequence file))
         (kill-ring-yank-pointer nil))
    (unwind-protect
        (progn
          (add-text-properties
           0 (length text)
           '(dired-clipboard-operation cut)
           text)
          (let ((kill-ring (list text))
                (dired-clipboard-file-clipboard-backends nil))
            (should (equal (dired-clipboard--content-from-clipboard)
                           (list :operation 'cut :files (list file))))))
      (delete-file file))))

(ert-deftest dired-clipboard-backend-content-preserves-internal-cut ()
  "Native backend files still use cut when the kill ring matches them."
  (let* ((file (make-temp-file "dired-clipboard-test"))
         (text (copy-sequence file))
         (kill-ring-yank-pointer nil)
         (dired-clipboard-file-clipboard-backends '(backend))
         (dired-clipboard-file-clipboard-backend-alist
          (list (list 'backend :paste (lambda () (list file))))))
    (unwind-protect
        (progn
          (add-text-properties
           0 (length text)
           '(dired-clipboard-operation cut)
           text)
          (let ((kill-ring (list text)))
            (should (equal (dired-clipboard--content-from-clipboard)
                           (list :operation 'cut :files (list file))))))
      (delete-file file))))

(ert-deftest dired-clipboard-windows-drop-effect-round-trips-cut ()
  "Windows Preferred DropEffect maps move to cut."
  (let* ((file (make-temp-file "dired-clipboard-test"))
         (output (concat "__DROPEFFECT:2\n"
                         (dired-clipboard--base64-encode-utf8 file)
                         "\n")))
    (unwind-protect
        (progn
          (should (= (dired-clipboard--windows-drop-effect 'copy) 1))
          (should (= (dired-clipboard--windows-drop-effect 'cut) 2))
          (should (eq (dired-clipboard--windows-operation-from-drop-effect 1)
                      'copy))
          (should (eq (dired-clipboard--windows-operation-from-drop-effect 2)
                      'cut))
          (should (equal (dired-clipboard--decode-base64-lines output)
                         (list file)))
          (should (equal (dired-clipboard--clipboard-content
                          (dired-clipboard--decode-base64-lines output)
                          (dired-clipboard--windows-operation-from-drop-effect
                           (dired-clipboard--windows-drop-effect-from-output
                            output)))
                         (list :operation 'cut :files (list file)))))
      (delete-file file))))

(ert-deftest dired-clipboard-windows-set-clipboard-uses-cut-effect ()
  "Windows clipboard setup receives DROPEFFECT_MOVE for cut payloads."
  (let* ((file (make-temp-file "dired-clipboard-test"))
         (system-type 'windows-nt)
         (dired-clipboard-use-native-file-clipboard t)
         (captured-input nil))
    (unwind-protect
        (cl-letf (((symbol-function #'dired-clipboard--powershell-program)
                   (lambda () "powershell"))
                  ((symbol-function #'dired-clipboard--call-powershell)
                   (lambda (_script input)
                     (setq captured-input input)
                     "ok")))
          (should (dired-clipboard--set-windows-file-clipboard
                   (list file) 'cut))
          (should (string-prefix-p "effect=2\n" captured-input)))
      (delete-file file))))

(ert-deftest dired-clipboard-paste-destination-renames-existing-file ()
  "Paste destination uses a copy-style name when a file exists."
  (let* ((directory (make-temp-file "dired-clipboard-test" t))
         (source (expand-file-name "example.txt" directory))
         (copy (expand-file-name "example copy.txt" directory))
         (second-copy (expand-file-name "example copy 2.txt" directory))
         (dired-clipboard-existing-file-policy 'rename))
    (unwind-protect
        (progn
          (write-region "" nil source)
          (should (equal (dired-clipboard--paste-destination source directory)
                         copy))
          (write-region "" nil copy)
          (should (equal (dired-clipboard--paste-destination source directory)
                         second-copy)))
      (delete-directory directory t))))

(ert-deftest dired-clipboard-paste-destination-renames-existing-directory ()
  "Paste destination uses a copy-style name for directories."
  (let* ((directory (make-temp-file "dired-clipboard-test" t))
         (source (expand-file-name "example.dir" directory))
         (copy (expand-file-name "example.dir copy" directory))
         (dired-clipboard-existing-file-policy 'rename))
    (unwind-protect
        (progn
          (make-directory source)
          (should (equal (dired-clipboard--paste-destination source directory)
                         copy)))
      (delete-directory directory t))))

(ert-deftest dired-clipboard-paste-destination-error-policy-keeps-name ()
  "The error policy leaves the destination name unchanged."
  (let* ((directory (make-temp-file "dired-clipboard-test" t))
         (source (expand-file-name "example.txt" directory))
         (dired-clipboard-existing-file-policy 'error))
    (unwind-protect
        (progn
          (write-region "" nil source)
          (should (equal (dired-clipboard--paste-destination source directory)
                         source)))
      (delete-directory directory t))))

(ert-deftest dired-clipboard-wayland-target-detects-mate ()
  "MATE desktops use Caja's copied-files MIME target."
  (let ((dired-clipboard-wayland-target 'auto)
        (process-environment
         (cons "XDG_CURRENT_DESKTOP=MATE" process-environment)))
    (should (eq (dired-clipboard--wayland-target) 'mate))
    (should (equal (dired-clipboard--wayland-mime-and-data
                    "copy\nfile:///tmp/example" "file:///tmp/example\r\n")
                   '("x-special/mate-copied-files" . "copy\nfile:///tmp/example")))))

(ert-deftest dired-clipboard-copied-files-supports-mate-target ()
  "MATE/Caja copied-files payloads are parsed and advertised."
  (let ((file (make-temp-file "dired-clipboard-test")))
    (unwind-protect
        (let* ((uri (dired-clipboard--file-to-uri file))
               (payload (concat "copy\n" uri)))
          (should (equal (dired-clipboard--parse-copied-files payload)
                         (list file)))
          (should (equal (dired-clipboard--parse-copied-files
                          (concat "x-special/mate-copied-files\n" payload))
                         (list file)))
          (should (equal (get-text-property
                          0 'x-special/mate-copied-files
                          (plist-get
                           (dired-clipboard--file-clipboard-payload
                            (list file))
                           :selection))
                         payload)))
      (delete-file file))))

(provide 'dired-clipboard-tests)

;;; dired-clipboard-tests.el ends here
