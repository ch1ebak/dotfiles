;;; dired-clipboard.el --- Copy and paste Dired files via clipboard -*- lexical-binding: t; -*-

;; Author: kn66
;; Version: 0.1.0
;; Keywords: files, convenience
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; This package adds Dired file clipboard commands:
;;
;; - M-w copies marked files/directories to the clipboard, or the file at
;;   point when no files are marked.
;; - C-w cuts marked files/directories to the clipboard, or the file at
;;   point when no files are marked.
;; - C-y pastes files/directories from the clipboard into the current Dired directory.
;; - M-w/C-w fall back to the default bindings while the region is active.
;; - M-w/C-w/C-y fall back to the default bindings while editing file names in WDired.

;;; Code:

(require 'dired)
(require 'dired-aux)
(require 'select)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defvar wdired-mode-map)
(declare-function xselect--encode-string "select"
                  (type str &optional can-modify prefer-string-to-c-string))

(defgroup dired-clipboard nil
  "Copy and paste Dired files via the clipboard."
  :group 'dired
  :prefix "dired-clipboard-")

(defcustom dired-clipboard-recursive-copies 'always
  "How `dired-clipboard-paste' handles directory copies.
The value is bound to `dired-recursive-copies' while pasting."
  :type '(choice (const :tag "Ask" top)
                 (const :tag "Always" always)
                 (const :tag "Never" nil))
  :group 'dired-clipboard)

(defcustom dired-clipboard-keep-marker dired-keep-marker-copy
  "Marker used for files created by `dired-clipboard-paste'."
  :type '(choice (const :tag "Do not mark pasted files" nil)
                 (const :tag "Use current Dired marker" t)
                 (character :tag "Marker character"))
  :group 'dired-clipboard)

(defcustom dired-clipboard-existing-file-policy 'rename
  "How `dired-clipboard-paste' handles existing destination files.
The value `rename' pastes using the first available name like
\"file copy.txt\" or \"file copy 2.txt\".

The value `error' keeps Dired's default behavior, which signals an
error when the destination already exists."
  :type '(choice (const :tag "Paste with a unique name" rename)
                 (const :tag "Signal an error" error))
  :group 'dired-clipboard)

(defcustom dired-clipboard-use-wl-copy t
  "Whether to use wl-copy for file clipboard data under PGTK/Wayland.
The PGTK clipboard backend advertises plain text targets for
`gui-set-selection', but GNOME Files needs file clipboard MIME
types such as `x-special/gnome-copied-files'.  When this option is
non-nil and wl-copy is available, `dired-clipboard-copy' starts a
wl-copy process that owns the clipboard with file-copy MIME data.
The MIME type is controlled by `dired-clipboard-wayland-target'.
The plain path list is still kept in the Emacs kill ring for
Dired-to-Dired paste."
  :type 'boolean
  :group 'dired-clipboard)

(defcustom dired-clipboard-wayland-target 'auto
  "File clipboard MIME target used by wl-copy under PGTK/Wayland.
The value `auto' uses the GNOME/Nautilus format on GNOME-like
desktops, the MATE/Caja format on MATE, and `text/uri-list' on
KDE/LXQt and unknown desktops."
  :type '(choice (const :tag "Auto-detect desktop" auto)
                 (const :tag "GNOME/Nautilus format" gnome)
                 (const :tag "MATE/Caja format" mate)
                 (const :tag "text/uri-list" uri-list))
  :group 'dired-clipboard)

(defcustom dired-clipboard-use-native-file-clipboard t
  "Whether to use native file clipboard formats on Windows and macOS.
On Windows this uses the Explorer FileDrop clipboard format through
PowerShell/.NET.  On macOS this uses NSPasteboard file URLs through
osascript and AppleScriptObjC.

The enabled backend order is controlled by
`dired-clipboard-file-clipboard-backends'."
  :type 'boolean
  :group 'dired-clipboard)

(defcustom dired-clipboard-file-clipboard-backends '(windows macos wayland)
  "OS/environment file clipboard backends tried in order.
Each symbol names an entry in
`dired-clipboard-file-clipboard-backend-alist'.  Unknown or
unavailable backends are skipped.  The generic Emacs selection and
kill-ring fallback is always used after these backends fail."
  :type '(repeat
          (choice
           (const :tag "Windows Explorer FileDrop" windows)
           (const :tag "macOS Finder NSPasteboard" macos)
           (const :tag "Wayland wl-copy" wayland)
           (symbol :tag "Custom backend")))
  :group 'dired-clipboard)

(defcustom dired-clipboard-powershell-program nil
  "PowerShell executable used for Windows file clipboard integration.
When nil, use the first available executable among powershell.exe,
powershell, pwsh.exe and pwsh."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Program"))
  :group 'dired-clipboard)

(defcustom dired-clipboard-osascript-program "osascript"
  "osascript executable used for macOS file clipboard integration."
  :type 'string
  :group 'dired-clipboard)

(defvar dired-clipboard--saved-text-uri-list-converter nil
  "Original `text/uri-list' converter before `dired-clipboard' wraps it.")

(defvar dired-clipboard--wl-copy-process nil
  "Current wl-copy process used to own the Wayland clipboard.")

(defconst dired-clipboard--copied-files-targets
  '(x-special/gnome-copied-files
    x-special/mate-copied-files)
  "File-manager clipboard targets using copy/cut followed by file URIs.")

(defvar dired-clipboard-file-clipboard-backend-alist
  '((windows
     :label "Windows Explorer FileDrop"
     :available dired-clipboard--windows-backend-available-p
     :copy dired-clipboard--copy-windows-file-clipboard
     :paste dired-clipboard--windows-files-from-clipboard
     :paste-content dired-clipboard--windows-content-from-clipboard)
    (macos
     :label "macOS Finder NSPasteboard"
     :available dired-clipboard--macos-backend-available-p
     :copy dired-clipboard--copy-macos-file-clipboard
     :paste dired-clipboard--macos-files-from-clipboard
     :paste-content dired-clipboard--macos-content-from-clipboard)
    (wayland
     :label "Wayland wl-copy"
     :available dired-clipboard--wayland-backend-available-p
     :copy dired-clipboard--copy-wayland-file-clipboard))
  "Alist of OS/environment file clipboard backend definitions.
Each entry is (NAME . PLIST).  PLIST may contain these keys:

:label is a human-readable backend name.
:available is an optional predicate called with OPERATION and
  PAYLOAD.  OPERATION is `copy', `paste' or `paste-content'.  PAYLOAD
  is the file clipboard payload plist for copy operations and nil for
  paste operations.
:copy is a function called with the file clipboard payload plist.
:paste is a function called with no arguments and returning file
  names represented by the current OS clipboard.
:paste-content is a function called with no arguments and returning a
  plist with :operation and :files represented by the current OS clipboard.

Users can add entries here and include their names in
`dired-clipboard-file-clipboard-backends'.")

(defconst dired-clipboard--file-uri-path-chars
  (append url-unreserved-chars '(?/ ?:))
  "Characters left unescaped in file URI paths.")

(defconst dired-clipboard--windows-set-file-drop-script
  "$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
Add-Type -AssemblyName System.Windows.Forms
$files = New-Object System.Collections.Specialized.StringCollection
foreach ($line in ([Console]::In.ReadToEnd() -split \"`n\")) {
  $line = $line.Trim()
  if ($line.Length -gt 0) {
    if ($line -match '^effect=([0-9]+)$') {
      $effectValue = [UInt32]$Matches[1]
    } else {
      $path = [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($line))
      [void] $files.Add($path)
    }
  }
}
if ($files.Count -eq 0) { exit 1 }
if ($null -eq $effectValue) { $effectValue = [UInt32]1 }
$data = New-Object System.Windows.Forms.DataObject
$data.SetFileDropList($files)
$effect = New-Object System.IO.MemoryStream(,[BitConverter]::GetBytes($effectValue))
$data.SetData('Preferred DropEffect', $effect)
[System.Windows.Forms.Clipboard]::SetDataObject($data, $true)"
  "PowerShell script that sets the Windows FileDrop clipboard.")

(defconst dired-clipboard--windows-get-file-drop-script
  "$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
Add-Type -AssemblyName System.Windows.Forms
$data = [System.Windows.Forms.Clipboard]::GetDataObject()
if ($null -ne $data -and $data.GetDataPresent('Preferred DropEffect')) {
  $effect = $data.GetData('Preferred DropEffect')
  if ($effect -is [System.IO.MemoryStream]) {
    $bytes = $effect.ToArray()
  } elseif ($effect -is [byte[]]) {
    $bytes = $effect
  } else {
    $bytes = $null
  }
  if ($null -ne $bytes -and $bytes.Length -ge 4) {
    [Console]::Out.WriteLine('__DROPEFFECT:' + [BitConverter]::ToUInt32($bytes, 0))
  }
}
if ([System.Windows.Forms.Clipboard]::ContainsFileDropList()) {
  foreach ($path in [System.Windows.Forms.Clipboard]::GetFileDropList()) {
    [Console]::Out.WriteLine(
      [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($path)))
  }
}"
  "PowerShell script that prints the Windows FileDrop clipboard.")

(defconst dired-clipboard--macos-set-file-urls-script
  "use framework \"AppKit\"
use framework \"Foundation\"
use scripting additions

on run argv
  set op to item 1 of argv
  set fileArgs to rest of argv
  set pbItems to current application's NSMutableArray's array()
  set paths to current application's NSMutableArray's array()
  set firstItem to missing value
  repeat with filePath in fileArgs
    set fileURL to current application's NSURL's fileURLWithPath:(contents of filePath)
    set pbItem to current application's NSPasteboardItem's alloc()'s init()
    (pbItem's setString:(fileURL's absoluteString()) forType:(current application's NSPasteboardTypeFileURL))
    if firstItem is missing value then set firstItem to pbItem
    (pbItems's addObject:pbItem)
    (paths's addObject:(contents of filePath))
  end repeat

  -- macOS has no native copy/cut distinction on the pasteboard, so record it
  -- in a private type (the analog of Windows' Preferred DropEffect).  Stored
  -- on the first item; read back by dired-clipboard--macos-content-from-clipboard.
  if firstItem is not missing value then
    (firstItem's setString:op forType:\"org.gnu.emacs.dired-clipboard.operation\")
  end if

  set pasteboard to current application's NSPasteboard's generalPasteboard()
  pasteboard's clearContents()
  if not (pasteboard's writeObjects:pbItems) then error \"NSPasteboard writeObjects failed\"
  pasteboard's setPropertyList:paths forType:(current application's NSFilenamesPboardType)
  -- Force pasteboardd to materialize every item before this short-lived
  -- process exits.  Without this read-back, writeObjects: registers each
  -- NSURL as a promised provider tied to the osascript process and drops a
  -- random subset of items when the process exits, so copying several files
  -- pastes only one or two of them.
  (pasteboard's pasteboardItems()'s |count|())
end run"
  "AppleScriptObjC script that sets macOS file URLs on NSPasteboard.
The first argument is the operation name (\"copy\" or \"cut\"); the remaining
arguments are the file paths.  Writes one public.file-url item per file (for
Finder and modern apps), a legacy NSFilenamesPboardType property list, and a
private operation marker, then forces synchronization so all items survive
after osascript exits.")

(defconst dired-clipboard--macos-get-file-urls-script
  "use framework \"AppKit\"
use framework \"Foundation\"
use scripting additions

set pasteboard to current application's NSPasteboard's generalPasteboard()
set urlClass to current application's NSURL's class
set urlClasses to current application's NSArray's arrayWithObject:urlClass
set fileOnly to current application's NSNumber's numberWithBool:true
set options to current application's NSDictionary's dictionaryWithObject:fileOnly forKey:(current application's NSPasteboardURLReadingFileURLsOnlyKey)
set urls to pasteboard's readObjectsForClasses:urlClasses options:options
if urls is missing value then return \"\"
if (urls's |count|()) is 0 then return \"\"
-- Extract paths with NSArray methods only.  Coercing a bridged NSArray with
-- `as list' or walking it with `repeat with x in urls' treats it as a single
-- opaque object and silently returns just the first element, so multi-file
-- pastes lose all but one file.
return ((urls's valueForKey:\"path\")'s componentsJoinedByString:linefeed) as text"
  "AppleScriptObjC script that prints macOS file URLs from NSPasteboard.")

(defconst dired-clipboard--macos-get-content-script
  "use framework \"AppKit\"
use framework \"Foundation\"
use scripting additions

set pasteboard to current application's NSPasteboard's generalPasteboard()
set op to pasteboard's stringForType:\"org.gnu.emacs.dired-clipboard.operation\"
if op is missing value then set op to \"\"
set header to \"operation:\" & op
set urlClass to current application's NSURL's class
set urlClasses to current application's NSArray's arrayWithObject:urlClass
set fileOnly to current application's NSNumber's numberWithBool:true
set options to current application's NSDictionary's dictionaryWithObject:fileOnly forKey:(current application's NSPasteboardURLReadingFileURLsOnlyKey)
set urls to pasteboard's readObjectsForClasses:urlClasses options:options
if urls is missing value then return header
if (urls's |count|()) is 0 then return header
set pathText to ((urls's valueForKey:\"path\")'s componentsJoinedByString:linefeed) as text
return header & linefeed & pathText"
  "AppleScriptObjC script that prints the macOS clipboard operation and files.
The first output line is \"operation:copy\" or \"operation:cut\" (empty when no
operation marker is present, e.g. files copied in Finder); the remaining lines
are the file paths.")

(defun dired-clipboard--call-process (program input &rest args)
  "Run PROGRAM with ARGS and optional INPUT, returning stdout on success."
  (when-let ((executable (executable-find program)))
    (with-temp-buffer
      (let* ((output (current-buffer))
             (status
              (if input
                  (with-temp-buffer
                    (insert input)
                    (apply #'call-process-region
                           (point-min) (point-max)
                           executable nil output nil args))
                (apply #'call-process executable nil output nil args))))
        (when (and (integerp status) (zerop status))
          (buffer-string))))))

(defun dired-clipboard--base64-encode-utf8 (text)
  "Return TEXT encoded as one unwrapped base64 UTF-8 string."
  (base64-encode-string (encode-coding-string text 'utf-8) t))

(defun dired-clipboard--base64-decode-utf8 (text)
  "Return base64 TEXT decoded as UTF-8."
  (decode-coding-string (base64-decode-string text) 'utf-8))

(defun dired-clipboard--base64-lines (strings)
  "Return STRINGS encoded as newline-separated base64 UTF-8."
  (concat (mapconcat #'dired-clipboard--base64-encode-utf8 strings "\n")
          "\n"))

(defun dired-clipboard--decode-base64-lines (text)
  "Return newline-separated base64 UTF-8 TEXT as strings."
  (let (strings)
    (dolist (line (dired-clipboard--lines text) (nreverse strings))
      (when (and (zerop (% (length line) 4))
                 (string-match-p "\\`[[:alnum:]+/=]+\\'" line))
        (when-let ((string (ignore-errors
                             (dired-clipboard--base64-decode-utf8 line))))
          (push string strings))))))

(defun dired-clipboard--powershell-program ()
  "Return the PowerShell executable used for Windows clipboard integration."
  (or dired-clipboard-powershell-program
      (executable-find "powershell.exe")
      (executable-find "powershell")
      (executable-find "pwsh.exe")
      (executable-find "pwsh")))

(defun dired-clipboard--powershell-encoded-command (script)
  "Return SCRIPT encoded for PowerShell -EncodedCommand."
  (base64-encode-string (encode-coding-string script 'utf-16le) t))

(defun dired-clipboard--call-powershell (script &optional input)
  "Run PowerShell SCRIPT with optional INPUT, returning stdout on success."
  (when-let ((program (dired-clipboard--powershell-program)))
    (dired-clipboard--call-process
     program input
     "-NoLogo" "-NoProfile" "-NonInteractive" "-STA"
     "-OutputFormat" "Text" "-EncodedCommand"
     (dired-clipboard--powershell-encoded-command script))))

(defun dired-clipboard--call-osascript (script &rest args)
  "Run AppleScript SCRIPT with ARGS, returning stdout on success."
  (apply #'dired-clipboard--call-process
         dired-clipboard-osascript-program nil
         "-e" script args))

(defun dired-clipboard--selection-target-value (target value)
  "Return TARGET text property from selection VALUE."
  (and (stringp value)
       (get-text-property 0 target value)))

(defun dired-clipboard--clipboard-target-available-p (selection type value)
  "Return non-nil if VALUE provides TYPE for the CLIPBOARD SELECTION."
  (and (eq selection 'CLIPBOARD)
       (dired-clipboard--selection-target-value type value)))

(defun dired-clipboard--local-files (files)
  "Return local expanded file names from FILES."
  (let (local-files)
    (dolist (file files (nreverse local-files))
      (unless (file-remote-p file)
        (push (expand-file-name file) local-files)))))

(defun dired-clipboard--uris-for-files (files)
  "Return file URIs for local FILES."
  (let (uris)
    (dolist (file files (nreverse uris))
      (push (dired-clipboard--file-to-uri file) uris))))

(defun dired-clipboard--convert-clipboard-target (_selection type value)
  "Convert VALUE to clipboard target TYPE."
  (when-let ((text (dired-clipboard--selection-target-value type value)))
    (cons type (cdr (xselect--encode-string 'TEXT text t)))))

(defun dired-clipboard--call-saved-text-uri-list-converter
    (selection type value predicate)
  "Call the saved `text/uri-list' converter for SELECTION, TYPE and VALUE.
When PREDICATE is non-nil, call the saved predicate instead."
  (let ((converter dired-clipboard--saved-text-uri-list-converter))
    (cond
     ((and predicate (consp converter) (functionp (car converter)))
      (funcall (car converter) selection type value))
     ((and (not predicate) (consp converter) (functionp (cdr converter)))
      (funcall (cdr converter) selection type value))
     ((and (not predicate) (functionp converter))
      (funcall converter selection type value)))))

(defun dired-clipboard--text-uri-list-available-p (selection type value)
  "Return non-nil when `text/uri-list' is available for SELECTION."
  (or (dired-clipboard--clipboard-target-available-p selection type value)
      (dired-clipboard--call-saved-text-uri-list-converter
       selection type value t)))

(defun dired-clipboard--convert-text-uri-list (selection type value)
  "Convert VALUE to `text/uri-list' for SELECTION."
  (or (dired-clipboard--convert-clipboard-target selection type value)
      (dired-clipboard--call-saved-text-uri-list-converter
       selection type value nil)))

(defun dired-clipboard--install-selection-converters ()
  "Install clipboard converters used by external file managers."
  (unless dired-clipboard--saved-text-uri-list-converter
    (setq dired-clipboard--saved-text-uri-list-converter
          (cdr (assq 'text/uri-list selection-converter-alist))))
  (dolist (target dired-clipboard--copied-files-targets)
    (setq selection-converter-alist
          (assq-delete-all target selection-converter-alist))
    (push `(,target
            . (dired-clipboard--clipboard-target-available-p
               . dired-clipboard--convert-clipboard-target))
          selection-converter-alist))
  (let ((cell (assq 'text/uri-list selection-converter-alist)))
    (if cell
        (setcdr cell
                '(dired-clipboard--text-uri-list-available-p
                  . dired-clipboard--convert-text-uri-list))
      (push '(text/uri-list
              . (dired-clipboard--text-uri-list-available-p
                 . dired-clipboard--convert-text-uri-list))
            selection-converter-alist))))

(defun dired-clipboard--wl-copy-available-p ()
  "Return non-nil if wl-copy should be used for the current display."
  (and dired-clipboard-use-wl-copy
       (featurep 'pgtk)
       (getenv "WAYLAND_DISPLAY")
       (executable-find "wl-copy")))

(defun dired-clipboard--wayland-backend-available-p (operation _payload)
  "Return non-nil if the Wayland backend can handle OPERATION."
  (and (eq operation 'copy)
       (dired-clipboard--wl-copy-available-p)))

(defun dired-clipboard--desktop-match-p (&rest names)
  "Return non-nil if current desktop name matches any of NAMES."
  (let ((desktop (or (getenv "XDG_CURRENT_DESKTOP")
                     (getenv "DESKTOP_SESSION")
                     "")))
    (catch 'match
      (dolist (name names nil)
        (when (string-match-p (regexp-quote name) desktop)
          (throw 'match t))))))

(defun dired-clipboard--wayland-target ()
  "Return the wl-copy MIME target kind for the current desktop."
  (pcase dired-clipboard-wayland-target
    ('gnome 'gnome)
    ('mate 'mate)
    ('uri-list 'uri-list)
    (_ (cond
        ((dired-clipboard--desktop-match-p "MATE")
         'mate)
        ((dired-clipboard--desktop-match-p "GNOME" "Cinnamon" "XFCE"
                                           "Budgie" "Unity" "Pantheon")
         'gnome)
        ((dired-clipboard--desktop-match-p "KDE" "Plasma" "LXQt")
         'uri-list)
        (t 'uri-list)))))

(defun dired-clipboard--wayland-mime-and-data (copied-files-list uri-list)
  "Return a cons of MIME type and data.
COPIED-FILES-LIST is the copy/cut plus file URI payload used by
GNOME/Nautilus-style targets, including Caja's MATE target."
  (pcase (dired-clipboard--wayland-target)
    ('gnome (cons "x-special/gnome-copied-files" copied-files-list))
    ('mate (cons "x-special/mate-copied-files" copied-files-list))
    (_ (cons "text/uri-list" uri-list))))

(defun dired-clipboard--stop-wl-copy ()
  "Stop the wl-copy process owned by `dired-clipboard'."
  (when (process-live-p dired-clipboard--wl-copy-process)
    (delete-process dired-clipboard--wl-copy-process))
  (setq dired-clipboard--wl-copy-process nil))

(defun dired-clipboard--set-wayland-file-clipboard
    (copied-files-list uri-list)
  "Set Wayland file clipboard data using wl-copy."
  (when (dired-clipboard--wl-copy-available-p)
    (dired-clipboard--stop-wl-copy)
    (pcase-let* ((`(,mime-type . ,data)
                  (dired-clipboard--wayland-mime-and-data
                   copied-files-list uri-list))
                 (process
                  (make-process
                   :name "dired-clipboard-wl-copy"
                   :command (list "wl-copy" "--foreground" "--type" mime-type)
                   :connection-type 'pipe
                   :noquery t)))
      (setq dired-clipboard--wl-copy-process process)
      (process-send-string process data)
      (process-send-eof process)
      process)))

(defun dired-clipboard--copy-wayland-file-clipboard (payload)
  "Copy PAYLOAD to the Wayland file clipboard."
  (when-let* ((copied-files-list
               (plist-get payload :copied-files-list))
              (uri-list (plist-get payload :uri-list)))
    (dired-clipboard--set-wayland-file-clipboard
     copied-files-list uri-list)))

(defun dired-clipboard--lines (text)
  "Return non-empty lines from TEXT."
  (when (stringp text)
    (let (lines)
      (dolist (line (split-string text "\n" t) (nreverse lines))
        (when (and (> (length line) 0)
                   (= (aref line (1- (length line))) ?\r))
          (setq line (substring line 0 -1)))
        (unless (string-empty-p line)
          (push line lines))))))

(defun dired-clipboard--file-to-uri (file)
  "Return a file URI for local FILE."
  (let ((path (expand-file-name file)))
    (if (and (eq system-type 'windows-nt)
             (string-match "\\`//\\([^/]+\\)\\(/.*\\)" path))
        (concat "file://"
                (match-string 1 path)
                (url-hexify-string
                 (match-string 2 path)
                 dired-clipboard--file-uri-path-chars))
      (concat "file://"
              (unless (string-prefix-p "/" path) "/")
              (url-hexify-string path dired-clipboard--file-uri-path-chars)))))

(defun dired-clipboard--uri-to-file (uri)
  "Return a local file name from file URI."
  (when (string-match-p "\\`file:" uri)
    (let* ((url (url-generic-parse-url uri))
           (host (url-host url))
           (path (url-filename url)))
      (when (and path
                 (or (eq system-type 'windows-nt)
                     (null host)
                     (string-empty-p host)
                     (string= host "localhost")))
        (setq path (url-unhex-string path 'utf-8))
        (when (eq system-type 'windows-nt)
          (cond
           ((and host
                 (not (string-empty-p host))
                 (not (string= host "localhost")))
            (setq path (concat "//" host path)))
           ((string-match-p "\\`/[[:alpha:]]:" path)
            (setq path (substring path 1)))))
        path))))

(defun dired-clipboard--parse-uri-list (text)
  "Return local file names from a text/uri-list TEXT."
  (let (files)
    (dolist (line (dired-clipboard--lines text) (nreverse files))
      (unless (or (string-empty-p line)
                  (eq (aref line 0) ?#))
        (when-let ((file (dired-clipboard--uri-to-file line)))
          (push file files))))))

(defun dired-clipboard--parse-copied-files-content (text)
  "Return a plist for copy/cut plus file URI clipboard TEXT."
  (let ((lines (dired-clipboard--lines text)))
    (when (member (car lines)
                  '("x-special/nautilus-clipboard"
                    "x-special/mate-copied-files"))
      (setq lines (cdr lines)))
    (when (member (car lines) '("copy" "cut"))
      (list :operation (intern (car lines))
            :files (dired-clipboard--parse-uri-list
                    (mapconcat #'identity (cdr lines) "\n"))))))

(defun dired-clipboard--parse-copied-files (text)
  "Return local file names from copy/cut plus file URI clipboard TEXT."
  (plist-get (dired-clipboard--parse-copied-files-content text) :files))

(defun dired-clipboard--parse-path-list (text)
  "Return absolute file names from newline-separated TEXT."
  (let (files)
    (dolist (line (dired-clipboard--lines text) (nreverse files))
      (when (file-name-absolute-p line)
        (push (expand-file-name line) files)))))

(defun dired-clipboard--selection (data-type)
  "Return clipboard selection converted to DATA-TYPE, or nil."
  (when (fboundp 'gui-get-selection)
    (ignore-errors
      (gui-get-selection 'CLIPBOARD data-type))))

(defun dired-clipboard--current-kill ()
  "Return the latest kill-ring entry, or nil."
  (ignore-errors
    (current-kill 0 t)))

(defun dired-clipboard--existing-files (files)
  "Return existing FILES without duplicates."
  (let (seen existing)
    (dolist (file files (nreverse existing))
      (when (and (file-exists-p file)
                 (not (member file seen)))
        (push file seen)
        (push file existing)))))

(defun dired-clipboard--first-existing-files (&rest candidates)
  "Return the first non-empty existing file list from CANDIDATES."
  (catch 'files
    (dolist (files candidates)
      (when-let ((existing (dired-clipboard--existing-files files)))
        (throw 'files existing)))))

(defun dired-clipboard--existing-clipboard-content (content)
  "Return CONTENT with only existing files, or nil when none exist."
  (when-let ((files (dired-clipboard--existing-files
                     (plist-get content :files))))
    (plist-put (copy-sequence content) :files files)))

(defun dired-clipboard--first-existing-clipboard-content (&rest candidates)
  "Return the first clipboard content with existing files."
  (catch 'content
    (dolist (content candidates)
      (when-let ((existing
                  (dired-clipboard--existing-clipboard-content content)))
        (throw 'content existing)))))

(defun dired-clipboard--clipboard-content (files &optional operation)
  "Return a clipboard content plist for FILES and OPERATION."
  (list :operation (or operation 'copy)
        :files files))

(defun dired-clipboard--operation-from-text (text)
  "Return clipboard operation recorded on TEXT, or `copy'."
  (or (and (stringp text)
           (> (length text) 0)
           (get-text-property 0 'dired-clipboard-operation text))
      'copy))

(defun dired-clipboard--content-with-text-operation (content text-files text)
  "Return CONTENT, using TEXT's operation when it describes TEXT-FILES."
  (if (and (eq (dired-clipboard--operation-from-text text) 'cut)
           (equal (plist-get content :files) text-files))
      (plist-put (copy-sequence content) :operation 'cut)
    content))

(defun dired-clipboard--file-clipboard-payload (files &optional operation)
  "Return a file clipboard payload plist for FILES."
  (let* ((operation (or operation 'copy))
         (operation-name (symbol-name operation))
         (text (mapconcat #'identity files "\n"))
         (local-files (dired-clipboard--local-files files))
         (local-uris (dired-clipboard--uris-for-files local-files))
         (uri-list (when local-uris
                     (concat (mapconcat #'identity local-uris "\r\n") "\r\n")))
         (copied-files-list
          (when local-uris
            ;; Nautilus rejects empty lines in this MIME payload.
            (concat operation-name "\n"
                    (mapconcat #'identity local-uris "\n"))))
         (selection (copy-sequence text)))
    (add-text-properties
     0 (length text)
     `(dired-clipboard-operation ,operation)
     text)
    (when local-uris
      (dired-clipboard--install-selection-converters)
      (add-text-properties
       0 (length selection)
       `(text/uri-list ,uri-list
                       x-special/gnome-copied-files ,copied-files-list
                       x-special/mate-copied-files ,copied-files-list)
       selection))
    (list :operation operation
          :files files
          :text text
          :local-files local-files
          :local-uris local-uris
          :uri-list uri-list
          :copied-files-list copied-files-list
          :gnome-list copied-files-list
          :selection selection)))

(defun dired-clipboard--file-clipboard-backend-definition (name)
  "Return the file clipboard backend definition for NAME."
  (assq name dired-clipboard-file-clipboard-backend-alist))

(defun dired-clipboard--file-clipboard-backend-handler (definition operation)
  "Return DEFINITION's handler for OPERATION."
  (let ((handler (plist-get (cdr definition)
                            (pcase operation
                              ('copy :copy)
                              ('paste-content :paste-content)
                              ('paste :paste)))))
    (and (functionp handler) handler)))

(defun dired-clipboard--file-clipboard-backend-available-p
    (definition operation payload)
  "Return non-nil if DEFINITION can handle OPERATION with PAYLOAD."
  (let ((predicate (plist-get (cdr definition) :available)))
    (if predicate
        (and (functionp predicate)
             (funcall predicate operation payload))
      t)))

(defun dired-clipboard--call-file-clipboard-backend
    (name operation &optional payload)
  "Call file clipboard backend NAME for OPERATION with optional PAYLOAD."
  (when-let* ((definition
               (dired-clipboard--file-clipboard-backend-definition name))
              (handler
               (dired-clipboard--file-clipboard-backend-handler
                definition operation)))
    (ignore-errors
      (when (dired-clipboard--file-clipboard-backend-available-p
             definition operation payload)
        (pcase operation
          ('copy (funcall handler payload))
          ('paste-content (funcall handler))
          ('paste (funcall handler)))))))

(defun dired-clipboard--copy-with-file-backends (payload)
  "Copy PAYLOAD through the first successful file clipboard backend.
Return the backend name that claimed the clipboard."
  (catch 'backend
    (dolist (name dired-clipboard-file-clipboard-backends)
      (when (dired-clipboard--call-file-clipboard-backend name 'copy payload)
        (throw 'backend name)))))

(defun dired-clipboard--content-from-file-backends ()
  "Return clipboard content from the first backend with existing paths."
  (catch 'content
    (dolist (name dired-clipboard-file-clipboard-backends)
      (when-let ((content
                  (dired-clipboard--existing-clipboard-content
                   (dired-clipboard--call-file-clipboard-backend
                    name 'paste-content))))
        (throw 'content content))
      (when-let ((files
                  (dired-clipboard--existing-files
                   (dired-clipboard--call-file-clipboard-backend
                    name 'paste))))
        (throw 'content (dired-clipboard--clipboard-content files))))))

(defun dired-clipboard--files-from-file-backends ()
  "Return files from the first backend with existing paths."
  (plist-get (dired-clipboard--content-from-file-backends) :files))

(defun dired-clipboard--windows-backend-available-p (operation _payload)
  "Return non-nil if the Windows backend can handle OPERATION."
  (and (memq operation '(copy paste paste-content))
       dired-clipboard-use-native-file-clipboard
       (eq system-type 'windows-nt)
       (dired-clipboard--powershell-program)))

(defun dired-clipboard--windows-drop-effect (operation)
  "Return the Windows DROPEFFECT value for OPERATION."
  (if (eq operation 'cut) 2 1))

(defun dired-clipboard--windows-operation-from-drop-effect (effect)
  "Return the clipboard operation represented by Windows DROPEFFECT."
  (if (and effect
           (not (zerop (logand effect 2))))
      'cut
    'copy))

(defun dired-clipboard--windows-drop-effect-from-output (output)
  "Return the Windows DROPEFFECT marker from OUTPUT, or nil."
  (when (stringp output)
    (catch 'effect
      (dolist (line (dired-clipboard--lines output))
        (when (string-match "\\`__DROPEFFECT:\\([0-9]+\\)\\'" line)
          (throw 'effect (string-to-number (match-string 1 line))))))))

(defun dired-clipboard--set-windows-file-clipboard (files &optional operation)
  "Set the Windows Explorer file clipboard to FILES.
OPERATION is either `copy' or `cut'."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'windows-nt)
             files)
    (dired-clipboard--call-powershell
     dired-clipboard--windows-set-file-drop-script
     (concat "effect="
             (number-to-string
              (dired-clipboard--windows-drop-effect operation))
             "\n"
             (dired-clipboard--base64-lines files)))))

(defun dired-clipboard--copy-windows-file-clipboard (payload)
  "Copy PAYLOAD to the Windows Explorer file clipboard."
  (when-let ((files (plist-get payload :local-files)))
    (dired-clipboard--set-windows-file-clipboard
     files
     (plist-get payload :operation))))

(defun dired-clipboard--windows-files-from-clipboard ()
  "Return file names from the Windows Explorer file clipboard."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'windows-nt))
    (when-let ((output (dired-clipboard--call-powershell
                        dired-clipboard--windows-get-file-drop-script)))
      (dired-clipboard--decode-base64-lines output))))

(defun dired-clipboard--windows-content-from-clipboard ()
  "Return clipboard content from the Windows Explorer file clipboard."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'windows-nt))
    (when-let ((output (dired-clipboard--call-powershell
                        dired-clipboard--windows-get-file-drop-script)))
      (dired-clipboard--clipboard-content
       (dired-clipboard--decode-base64-lines output)
       (dired-clipboard--windows-operation-from-drop-effect
        (dired-clipboard--windows-drop-effect-from-output output))))))

(defun dired-clipboard--macos-backend-available-p (operation _payload)
  "Return non-nil if the macOS backend can handle OPERATION."
  (and (memq operation '(copy paste paste-content))
       dired-clipboard-use-native-file-clipboard
       (eq system-type 'darwin)
       (executable-find dired-clipboard-osascript-program)))

(defun dired-clipboard--set-macos-file-clipboard (files &optional operation)
  "Set the macOS Finder file clipboard to FILES.
OPERATION is either `copy' or `cut' and is recorded in a private
pasteboard marker so that `dired-clipboard-paste' can move instead of
copy.  macOS has no native cut concept, so external apps such as Finder
ignore the marker and always paste a copy."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'darwin)
             files
             (executable-find dired-clipboard-osascript-program))
    (apply #'dired-clipboard--call-osascript
           dired-clipboard--macos-set-file-urls-script
           (symbol-name (or operation 'copy))
           files)))

(defun dired-clipboard--copy-macos-file-clipboard (payload)
  "Copy PAYLOAD to the macOS Finder file clipboard."
  (when-let ((files (plist-get payload :local-files)))
    (dired-clipboard--set-macos-file-clipboard
     files (plist-get payload :operation))))

(defun dired-clipboard--macos-files-from-clipboard ()
  "Return file names from the macOS Finder file clipboard."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'darwin)
             (executable-find dired-clipboard-osascript-program))
    (when-let ((output (dired-clipboard--call-osascript
                        dired-clipboard--macos-get-file-urls-script)))
      (dired-clipboard--lines output))))

(defun dired-clipboard--macos-content-from-clipboard ()
  "Return clipboard content from the macOS Finder file clipboard.
The returned plist carries the recorded `copy'/`cut' operation; files
placed by an external app without the marker default to `copy'."
  (when (and dired-clipboard-use-native-file-clipboard
             (eq system-type 'darwin)
             (executable-find dired-clipboard-osascript-program))
    (when-let ((output (dired-clipboard--call-osascript
                        dired-clipboard--macos-get-content-script)))
      (let* ((lines (dired-clipboard--lines output))
             (header (car lines))
             (files (cdr lines))
             (operation
              (if (and (stringp header)
                       (string= header "operation:cut"))
                  'cut
                'copy)))
        (when files
          (dired-clipboard--clipboard-content files operation))))))

(defun dired-clipboard--files-from-clipboard ()
  "Return existing files/directories represented by the clipboard."
  (plist-get (dired-clipboard--content-from-clipboard) :files))

(defun dired-clipboard--content-from-clipboard ()
  "Return clipboard content represented by the clipboard."
  (let* ((backend-content (dired-clipboard--content-from-file-backends))
         (gnome (dired-clipboard--selection 'x-special/gnome-copied-files))
         (mate (dired-clipboard--selection 'x-special/mate-copied-files))
         (uri-list (dired-clipboard--selection 'text/uri-list))
         (text (or (dired-clipboard--selection 'UTF8_STRING)
                   (dired-clipboard--selection 'STRING)
                   (dired-clipboard--current-kill)))
         (text-files (dired-clipboard--parse-path-list text)))
    (dired-clipboard--first-existing-clipboard-content
     (dired-clipboard--content-with-text-operation
      backend-content text-files text)
     (dired-clipboard--parse-copied-files-content gnome)
     (dired-clipboard--parse-copied-files-content mate)
     (dired-clipboard--parse-copied-files-content text)
     (dired-clipboard--clipboard-content
      (dired-clipboard--parse-uri-list uri-list))
     (dired-clipboard--clipboard-content
      text-files
      (dired-clipboard--operation-from-text text)))))

(defun dired-clipboard--copy-files (files &optional operation)
  "Copy FILES to the kill ring and system clipboard.
OPERATION is either `copy' or `cut'."
  (let* ((payload (dired-clipboard--file-clipboard-payload files operation))
         (text (plist-get payload :text))
         (selection (plist-get payload :selection))
         (clipboard-owned
          (and (plist-get payload :local-uris)
               (dired-clipboard--copy-with-file-backends payload))))
    (if clipboard-owned
        (let ((interprogram-cut-function nil))
          (kill-new text))
      (dired-clipboard--stop-wl-copy)
      (kill-new text)
      (when (fboundp 'gui-set-selection)
        (ignore-errors
          (gui-set-selection 'CLIPBOARD selection))))))

(add-hook 'kill-emacs-hook #'dired-clipboard--stop-wl-copy)

(defun dired-clipboard--ensure-not-wdired ()
  "Signal an error when called from WDired."
  (when (derived-mode-p 'wdired-mode)
    (user-error "M-w/C-w/C-y are disabled while editing Dired entries")))

(defun dired-clipboard--region-active-p ()
  "Return non-nil when a non-empty region is active."
  (and (bound-and-true-p mark-active)
       (mark t)
       (> (region-end) (region-beginning))))

(defun dired-clipboard--copy-name (file index)
  "Return a copy-style name for FILE at numeric INDEX."
  (let* ((directory-p (file-directory-p file))
         (name (file-name-nondirectory (directory-file-name file)))
         (root (if directory-p name (file-name-sans-extension name)))
         (extension (if directory-p "" (file-name-extension name t)))
         (suffix (if (= index 1) " copy" (format " copy %d" index))))
    (concat root suffix extension)))

(defun dired-clipboard--unique-destination (file directory)
  "Return a destination for FILE in DIRECTORY that does not exist."
  (let* ((target-directory (file-name-as-directory directory))
         (name (file-name-nondirectory (directory-file-name file)))
         (destination (expand-file-name name target-directory))
         (index 1))
    (while (file-exists-p destination)
      (setq destination
            (expand-file-name
             (dired-clipboard--copy-name file index)
             target-directory))
      (setq index (1+ index)))
    destination))

(defun dired-clipboard--paste-destination (file directory)
  "Return the paste destination for FILE in DIRECTORY."
  (let ((destination
         (expand-file-name
          (file-name-nondirectory (directory-file-name file))
          directory)))
    (pcase dired-clipboard-existing-file-policy
      ('rename (dired-clipboard--unique-destination file directory))
      (_ destination))))

;;;###autoload
(defun dired-clipboard-copy ()
  "Copy marked Dired files/directories, or current file, to the clipboard."
  (interactive nil dired-mode)
  (dired-clipboard--ensure-not-wdired)
  (when (dired-clipboard--region-active-p)
    (user-error "Dired file copy is disabled while the region is active"))
  (let ((files (dired-get-marked-files
                nil nil nil nil
                "No file at point")))
    (dired-clipboard--copy-files files)
    (message "%d item%s copied to clipboard"
             (length files)
             (if (= (length files) 1) "" "s"))))

;;;###autoload
(defun dired-clipboard-paste ()
  "Paste files/directories represented by the clipboard into Dired."
  (interactive nil dired-mode)
  (dired-clipboard--ensure-not-wdired)
  (let* ((content (dired-clipboard--content-from-clipboard))
         (files (plist-get content :files))
         (operation (plist-get content :operation))
         (move-p (eq operation 'cut))
         (create-function (if move-p #'dired-rename-file #'dired-copy-file))
         (description (if move-p "Move" "Paste"))
         (target (file-name-as-directory (dired-current-directory)))
         (dired-recursive-copies dired-clipboard-recursive-copies))
    (unless files
      (user-error "Clipboard does not contain pasteable files or directories"))
    (dired-create-files
     create-function
     description
     files
     (lambda (from)
       (dired-clipboard--paste-destination from target))
     dired-clipboard-keep-marker)))

;;;###autoload
(defun dired-clipboard-cut ()
  "Cut marked Dired files/directories, or current file, to the clipboard."
  (interactive nil dired-mode)
  (dired-clipboard--ensure-not-wdired)
  (when (dired-clipboard--region-active-p)
    (user-error "Dired file cut is disabled while the region is active"))
  (let ((files (dired-get-marked-files
                nil nil nil nil
                "No file at point")))
    (dired-clipboard--copy-files files 'cut)
    (message "%d item%s cut to clipboard"
             (length files)
             (if (= (length files) 1) "" "s"))))

(defun dired-clipboard--disabled-in-wdired ()
  "Disable file clipboard commands while editing Dired entries."
  (interactive)
  (user-error "M-w/C-w/C-y are disabled while editing Dired entries"))

(defun dired-clipboard--copy-key-binding (&optional _)
  "Return the M-w binding for the current Dired state."
  (if (or (derived-mode-p 'wdired-mode)
          (dired-clipboard--region-active-p))
      nil
    #'dired-clipboard-copy))

(defun dired-clipboard--cut-key-binding (&optional _)
  "Return the C-w binding for the current Dired state."
  (if (or (derived-mode-p 'wdired-mode)
          (dired-clipboard--region-active-p))
      nil
    #'dired-clipboard-cut))

(defun dired-clipboard--paste-key-binding (&optional _)
  "Return the C-y binding for the current Dired state."
  (if (derived-mode-p 'wdired-mode)
      nil
    #'dired-clipboard-paste))

(defvar dired-clipboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-w")
                `(menu-item "" nil :filter ,#'dired-clipboard--copy-key-binding))
    (define-key map (kbd "C-w")
                `(menu-item "" nil :filter ,#'dired-clipboard--cut-key-binding))
    (define-key map (kbd "C-y")
                `(menu-item "" nil :filter ,#'dired-clipboard--paste-key-binding))
    map)
  "Keymap for `dired-clipboard-mode'.")

;;;###autoload
(define-minor-mode dired-clipboard-mode
  "Use M-w/C-w/C-y to copy, cut and paste files in Dired."
  :lighter nil
  :keymap dired-clipboard-mode-map)

(with-eval-after-load 'wdired
  (when (eq (lookup-key wdired-mode-map (kbd "M-w"))
            #'dired-clipboard--disabled-in-wdired)
    (define-key wdired-mode-map (kbd "M-w") nil))
  (when (eq (lookup-key wdired-mode-map (kbd "C-w"))
            #'dired-clipboard--disabled-in-wdired)
    (define-key wdired-mode-map (kbd "C-w") nil))
  (when (eq (lookup-key wdired-mode-map (kbd "C-y"))
            #'dired-clipboard--disabled-in-wdired)
    (define-key wdired-mode-map (kbd "C-y") nil)))

(provide 'dired-clipboard)

;;; dired-clipboard.el ends here
