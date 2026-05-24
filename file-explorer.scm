;; File Explorer — component-based sidebar panel for Helix
;; space+e to open/focus/close  •  q to close when focused
;; j/k or arrows to navigate  •  o/Enter to open file
;; tab to toggle dir  •  F fold-all  •  E expand-all
;; a create file  •  A create dir  •  r refresh

(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

;; ===== Config =====

(define *fe-content-width* 35)       ; mutable — changed by +/-
(define *fe-min-content-width* 16)
(define *fe-max-content-width* 60)

(define (fe-total-width) (+ *fe-content-width* 2))  ; +2 for border chars

(define *fe-help-lines*
  '("j/k  ↑↓     navigate"
    "h           parent dir"
    "o/↵         open file"
    "tab         toggle dir"
    "F/E         fold/expand all"
    "a           new file or dir"
    "r           rename"
    "d           delete"
    "R           refresh"
    "+/-         panel width"
    "esc         unfocus"
    "q           close"
    "?           this help"))

;; ===== Module-level state =====

(define *fe-active*         #f)  ; bg render component is on the stack
(define *fe-focused*        #f)  ; fg navigator component is on the stack
(define *fe-tree*           '())    ; list of (path . display-string)
(define *fe-cursor*         0)
(define *fe-window-start*   0)
(define *fe-visible-height* 30)     ; updated every frame by bg render
(define *fe-directories*    (hash)) ; path -> #t folded / #f open
(define *fe-ignore-set*
  (hashset ".git" "target" ".direnv" "node_modules" "__pycache__" ".hg"))

;; ===== Sort (merge-sort, no list-head/list-tail) =====

(define (fe-even-items lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (cadr lst) (fe-even-items (cddr lst)))))

(define (fe-odd-items lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          (list (car lst))
          (cons (car lst) (fe-odd-items (cddr lst))))))

(define (fe-merge-sorted l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [(string<? (car l1) (car l2))
         (cons (car l1) (fe-merge-sorted (cdr l1) l2))]
        [else (cons (car l2) (fe-merge-sorted (cdr l2) l1))]))

(define (fe-sort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (fe-merge-sorted (fe-sort (fe-odd-items lst))
                       (fe-sort (fe-even-items lst)))))

(define (fe-sort-dirs-first lst)
  (define dirs  (filter is-dir? lst))
  (define files (filter (lambda (p) (not (is-dir? p))) lst))
  (append (fe-sort dirs) (fe-sort files)))

;; ===== List utilities =====

(define (fe-drop lst n)
  (if (or (null? lst) (<= n 0)) lst (fe-drop (cdr lst) (- n 1))))

(define (fe-take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (fe-take (cdr lst) (- n 1)))))

(define (fe-repeat-str s n)
  (if (<= n 0) "" (string-append s (fe-repeat-str s (- n 1)))))

(define (fe-truncate s max-w)
  (if (<= (string-length s) max-w)
      s
      (string-append (substring s 0 (- max-w 1)) "…")))

;; ===== Icons =====

(define (fe-file-icon path) "")

(define (fe-dir-indicator path)
  (if (hash-contains? *fe-directories* path)
      (if (hash-try-get *fe-directories* path) "▶ " "▼ ")
      "▶ "))

;; ===== Tree builder =====

(define (fe-build-tree!)
  (define result '())
  (define (walk path depth)
    (define name (file-name path))
    (unless (hashset-contains? *fe-ignore-set* name)
      (define padding (fe-repeat-str "  " depth))
      (define icon    (if (is-dir? path) (fe-dir-indicator path) (fe-file-icon path)))
      (set! result (cons (cons path (string-append padding icon name)) result))
      (when (is-dir? path)
        (unless (hash-contains? *fe-directories* path)
          (set! *fe-directories* (hash-insert *fe-directories* path (> depth 0))))
        (unless (hash-try-get *fe-directories* path)
          (for-each (lambda (child) (walk child (+ depth 1)))
                    (fe-sort-dirs-first (read-dir path)))))))
  (walk (helix-find-workspace) 0)
  (set! *fe-tree* (reverse result)))

;; ===== Reveal current file =====

(define (fe-parent-path path)
  (trim-end-matches path (string-append "/" (file-name path))))

(define (fe-half-floor n)
  (let loop ([n n] [h 0])
    (if (< n 2) h (loop (- n 2) (+ h 1)))))

;; Mark every ancestor dir between ws (inclusive) and the file open in *fe-directories*.
;; Call this BEFORE fe-build-tree! so the walk expands those dirs.
(define (fe-open-ancestors-for-file! path)
  (define ws (helix-find-workspace))
  (define ws-prefix (string-append ws "/"))
  (when (and (string? path)
             (>= (string-length path) (string-length ws-prefix))
             (equal? (substring path 0 (string-length ws-prefix)) ws-prefix))
    (define (open-up! p)
      (define parent (fe-parent-path p))
      (set! *fe-directories* (hash-insert *fe-directories* parent #f))
      (unless (equal? parent ws)
        (open-up! parent)))
    (open-up! path)))

;; After fe-build-tree!, move the cursor to path and scroll it into the centre.
(define (fe-seek-file! path)
  (when (string? path)
    (define idx
      (let loop ([items *fe-tree*] [i 0])
        (cond [(null? items) #f]
              [(equal? (car (car items)) path) i]
              [else (loop (cdr items) (+ i 1))])))
    (when idx
      (set! *fe-cursor* idx)
      (set! *fe-window-start*
            (max 0 (- idx (fe-half-floor *fe-visible-height*)))))))

;; Convenience: reveal whatever file the editor currently has focused.
(define (fe-reveal-current-file!)
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (fe-open-ancestors-for-file! path)
  (fe-build-tree!)
  (fe-seek-file! path))

(define (fe-goto-parent!)
  (define entry (fe-current-entry))
  (when entry
    (define path   (car entry))
    (define ws     (helix-find-workspace))
    (define parent (fe-parent-path path))
    (unless (equal? parent ws)
      (fe-seek-file! parent))))

;; ===== Cursor movement =====

(define (fe-cursor-down!)
  (define n (length *fe-tree*))
  (when (< *fe-cursor* (- n 1))
    (set! *fe-cursor* (+ *fe-cursor* 1))
    (when (> *fe-cursor* (+ *fe-window-start* (- *fe-visible-height* 1)))
      (set! *fe-window-start* (+ *fe-window-start* 1)))))

(define (fe-cursor-up!)
  (when (> *fe-cursor* 0)
    (set! *fe-cursor* (- *fe-cursor* 1))
    (when (< *fe-cursor* *fe-window-start*)
      (set! *fe-window-start* (- *fe-window-start* 1)))))

;; ===== Actions =====

(define (fe-current-entry)
  (and (not (null? *fe-tree*))
       (list-ref *fe-tree* *fe-cursor*)))

(define (fe-toggle-dir! path)
  (if (hash-try-get *fe-directories* path)
      (set! *fe-directories* (hash-insert *fe-directories* path #f))
      (set! *fe-directories* (hash-insert *fe-directories* path #t)))
  (define old *fe-cursor*)
  (fe-build-tree!)
  (set! *fe-cursor* (min old (max 0 (- (length *fe-tree*) 1)))))

(define (fe-activate-current!)
  (define entry (fe-current-entry))
  (cond
    [(not entry) event-result/consume]
    [(is-file? (car entry))
     (define path (car entry))
     (set! *fe-focused* #f)
     (enqueue-thread-local-callback (lambda () (helix.open path)))
     event-result/close]  ; pops fg only; bg stays, editor regains focus
    [(is-dir? (car entry))
     (fe-toggle-dir! (car entry))
     event-result/consume]))

(define (fe-fold-all!)
  (set! *fe-directories*
        (transduce *fe-directories*
                   (mapping (lambda (p) (list (list-ref p 0) #t)))
                   (into-hashmap)))
  (fe-build-tree!)
  (set! *fe-cursor* 0)
  (set! *fe-window-start* 0))

(define (fe-unfold-all!)
  (set! *fe-directories*
        (transduce *fe-directories*
                   (mapping (lambda (p) (list (list-ref p 0) #f)))
                   (into-hashmap)))
  (fe-build-tree!))

(define (fe-prompt-create!)
  (define entry (fe-current-entry))
  (when entry
    (define path (car entry))
    (define base (if (is-dir? path)
                     (string-append path "/")
                     (trim-end-matches path (file-name path))))
    (enqueue-thread-local-callback
     (lambda ()
       (push-component!
        (prompt (string-append "New (end with / for dir): " base)
                (lambda (name)
                  (define full (string-append base name))
                  (if (equal? (substring name (- (string-length name) 1) (string-length name)) "/")
                      (hx.create-directory full)
                      (begin
                        (helix.vsplit-new)
                        (helix.open full)
                        (helix.write full)
                        (helix.quit)))
                  (enqueue-thread-local-callback fe-build-tree!))))))))

(define (fe-prompt-delete!)
  (define entry (fe-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define kind (if (is-dir? path) "directory" "file"))
    (enqueue-thread-local-callback
     (lambda ()
       (fe-show-confirm!
        (string-append "Delete " kind " '" name "'? (y/N) ")
        (lambda (confirmed?)
          (when confirmed?
            (if (is-dir? path)
                (delete-directory! path)
                (delete-file! path))
            (enqueue-thread-local-callback fe-build-tree!))))))))

;; ===== Inline input component (supports pre-filled default value) =====

(define *fe-input-prompt*   "")
(define *fe-input-buffer*   "")
(define *fe-input-callback* #f)

(struct FeInputState ())

(define (fe-input-render state rect frame)
  (define w (area-width rect))
  (define h (area-height rect))
  (define y (- h 1))
  (define text (string-append *fe-input-prompt* *fe-input-buffer*))
  (define st (theme-scope-ref "ui.text"))
  (frame-set-string! frame 0 y (make-string w #\space) st)
  (frame-set-string! frame 0 y (fe-truncate text (- w 1)) st))

(define (fe-input-cursor-fn state area)
  (define h (area-height area))
  (position (- h 1)
            (string-length (string-append *fe-input-prompt* *fe-input-buffer*))))

(define (fe-input-handle-event state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-enter? event)
     (define result *fe-input-buffer*)
     (define cb *fe-input-callback*)
     (set! *fe-input-callback* #f)
     (when cb
       (enqueue-thread-local-callback (lambda () (cb result))))
     event-result/close]
    [(key-event-escape? event)
     (set! *fe-input-callback* #f)
     event-result/close]
    [(key-event-backspace? event)
     (define len (string-length *fe-input-buffer*))
     (when (> len 0)
       (set! *fe-input-buffer* (substring *fe-input-buffer* 0 (- len 1))))
     event-result/consume]
    [(char? ch)
     (set! *fe-input-buffer* (string-append *fe-input-buffer* (string ch)))
     event-result/consume]
    [else event-result/consume]))

(define (fe-show-input! prompt-text initial-value callback)
  (set! *fe-input-prompt*   prompt-text)
  (set! *fe-input-buffer*   initial-value)
  (set! *fe-input-callback* callback)
  (push-component!
   (new-component! "fe-input"
                   (FeInputState)
                   fe-input-render
                   (hash "handle_event" fe-input-handle-event
                         "cursor"       fe-input-cursor-fn))))

;; ===== Single-keypress confirmation component =====

(define *fe-confirm-prompt*   "")
(define *fe-confirm-callback* #f)

(struct FeConfirmState ())

(define (fe-confirm-render state rect frame)
  (define w  (area-width rect))
  (define y  (- (area-height rect) 1))
  (define st (theme-scope-ref "ui.text"))
  (frame-set-string! frame 0 y (make-string w #\space) st)
  (frame-set-string! frame 0 y (fe-truncate *fe-confirm-prompt* (- w 1)) st))

(define (fe-confirm-handle-event state event)
  (define ch  (key-event-char event))
  (define cb  *fe-confirm-callback*)
  (set! *fe-confirm-callback* #f)
  (when cb
    (enqueue-thread-local-callback (lambda () (cb (and (char? ch) (equal? ch #\y))))))
  event-result/close)

(define (fe-show-confirm! prompt-text callback)
  (set! *fe-confirm-prompt*   prompt-text)
  (set! *fe-confirm-callback* callback)
  (push-component!
   (new-component! "fe-confirm"
                   (FeConfirmState)
                   fe-confirm-render
                   (hash "handle_event" fe-confirm-handle-event))))

;; ===== Help overlay =====

(struct FeHelpState ())

(define (fe-help-render state rect frame)
  (define w   (fe-total-width))
  (define h   (area-height rect))
  (define bg  (theme-scope-ref "ui.background"))
  (define brd (theme-scope-ref "ui.window"))
  (define txt (theme-scope-ref "ui.text"))
  (define ttl (theme-scope-ref "ui.statusline.normal"))
  (define n   (length *fe-help-lines*))
  (define box-h (+ n 2))
  (define y0  (max 1 (- h box-h 1)))
  (define panel-area (area 0 y0 w box-h))
  (buffer/clear-with frame panel-area bg)
  (block/render frame panel-area (make-block bg brd "all" "rounded"))
  (frame-set-string! frame 2 y0 "  Keys  " ttl)
  (let loop ([lines *fe-help-lines*] [row (+ y0 1)])
    (unless (null? lines)
      (frame-set-string! frame 2 row (fe-truncate (car lines) (- w 4)) txt)
      (loop (cdr lines) (+ row 1)))))

(define (fe-help-handle-event state event)
  event-result/close)

(define (fe-show-help!)
  (enqueue-thread-local-callback
   (lambda ()
     (push-component!
      (new-component! "fe-help"
                      (FeHelpState)
                      fe-help-render
                      (hash "handle_event" fe-help-handle-event))))))

;; ===== Rename =====

(define (fe-prompt-rename!)
  (define entry (fe-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define dir  (trim-end-matches path (string-append "/" name)))
    (enqueue-thread-local-callback
     (lambda ()
       (fe-show-input!
        "Rename: "
        name
        (lambda (new-name)
          (when (and (not (equal? new-name ""))
                     (not (equal? new-name name)))
            (rename-file-or-directory! path (string-append dir "/" new-name))
            (enqueue-thread-local-callback fe-build-tree!))))))))


;; Close both components and reset clip
(define (fe-wider!)
  (set! *fe-content-width*
        (min *fe-max-content-width* (+ *fe-content-width* 2)))
  (helix.redraw '()))

(define (fe-narrower!)
  (set! *fe-content-width*
        (max *fe-min-content-width* (- *fe-content-width* 2)))
  (helix.redraw '()))

(define (fe-close-all!)
  (set! *fe-active*  #f)
  (set! *fe-focused* #f)
  (pop-last-component-by-name! "file-explorer-bg")
  (enqueue-thread-local-callback (lambda () (set-editor-clip-left! 0))))

(define (fe-unfocus!)
  (set! *fe-focused* #f))

;; ===== Background component — renders the panel, passes all events to editor =====

(struct ExplorerBgState ())

(define (fe-render-bg state rect frame)
  (define h (area-height rect))
  (define w (fe-total-width))

  (set! *fe-visible-height* (max 1 (- h 2)))
  (set-editor-clip-left! w)

  (define bg-style     (theme-scope-ref "ui.background"))
  (define border-style (if *fe-focused*
                           (theme-scope-ref "ui.window")
                           (theme-scope-ref "ui.text")))
  (define normal-style (theme-scope-ref "ui.text"))
  (define hl-style     (theme-scope-ref "ui.menu.selected"))
  (define title-style  (theme-scope-ref "ui.statusline.normal"))
  (define dir-style    (theme-scope-ref "ui.text.info"))
  (define help-style   (theme-scope-ref "ui.text"))

  (define panel-area (area 0 0 w h))
  (buffer/clear-with frame panel-area bg-style)
  (block/render frame panel-area (make-block bg-style border-style "all" "rounded"))

  (define ws-name (file-name (helix-find-workspace)))
  (frame-set-string! frame 2 0
                     (fe-truncate (string-append "  " ws-name "  ") (- w 4))
                     title-style)

  ;; File tree entries
  (define max-text-w (- w 3))
  (define visible (fe-take (fe-drop *fe-tree* *fe-window-start*) *fe-visible-height*))

  (let loop ([items visible] [row 0])
    (unless (or (null? items) (>= row *fe-visible-height*))
      (define entry   (car items))
      (define abs-idx (+ *fe-window-start* row))
      (define path    (car entry))
      (define text    (fe-truncate (cdr entry) max-text-w))
      (define y       (+ 1 row))
      (define hl?     (= abs-idx *fe-cursor*))

      (when hl?
        (frame-set-string! frame 1 y
                           (make-string (- w 2) #\space)
                           hl-style))

      (frame-set-string! frame 1 y text
                         (cond [hl?             hl-style]
                               [(is-dir? path)  dir-style]
                               [else            normal-style]))

      (loop (cdr items) (+ row 1))))

  ;; Hint footer
  (define hint-style (theme-scope-ref "ui.text"))
  (frame-set-string! frame 1 (- h 1)
                     (fe-truncate "? for help" (- w 2))
                     hint-style))

(define (fe-handle-event-bg state event)
  ;; No cursor fn on bg: editor's native cursor is active.
  ;; event-result/ignore lets the editor receive these events.
  event-result/ignore)

;; ===== Foreground component — captures input while explorer is focused =====

(struct ExplorerFgState ())

(define (fe-render-fg state rect frame)
  void)  ; bg handles all drawing

(define (fe-cursor-fn-fg state area)
  (position (+ 1 (- *fe-cursor* *fe-window-start*)) 1))

(define (fe-handle-event-fg state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-down? event)  (fe-cursor-down!) event-result/consume]
    [(key-event-up? event)    (fe-cursor-up!)   event-result/consume]
    [(key-event-enter? event) (fe-activate-current!)]
    [(key-event-tab? event)
     (define entry (fe-current-entry))
     (when (and entry (is-dir? (car entry)))
       (fe-toggle-dir! (car entry)))
     event-result/consume]

    [(key-event-escape? event)
     (fe-unfocus!)
     event-result/close]  ; pops fg only; bg stays visible

    [(and (char? ch) (equal? ch #\q))
     (fe-close-all!)
     event-result/close]  ; pops fg; fe-close-all! already popped bg

    [(and (char? ch) (equal? ch #\j)) (fe-cursor-down!)   event-result/consume]
    [(and (char? ch) (equal? ch #\k)) (fe-cursor-up!)     event-result/consume]
    [(and (char? ch) (equal? ch #\h)) (fe-goto-parent!)   event-result/consume]
    [(and (char? ch) (equal? ch #\o)) (fe-activate-current!)]
    [(and (char? ch) (equal? ch #\F)) (fe-fold-all!)    event-result/consume]
    [(and (char? ch) (equal? ch #\E)) (fe-unfold-all!)  event-result/consume]
    [(and (char? ch) (equal? ch #\r)) (fe-prompt-rename!)       event-result/consume]
    [(and (char? ch) (equal? ch #\R)) (fe-build-tree!)          event-result/consume]
    [(and (char? ch) (equal? ch #\a)) (fe-prompt-create!)       event-result/consume]
    [(and (char? ch) (equal? ch #\d)) (fe-prompt-delete!)       event-result/consume]
    [(and (char? ch) (equal? ch #\+)) (fe-wider!)              event-result/consume]
    [(and (char? ch) (equal? ch #\-)) (fe-narrower!)           event-result/consume]
    [(and (char? ch) (equal? ch #\?)) (fe-show-help!)          event-result/consume]

    [else event-result/consume]))  ; block unknown keys from editor while focused

;; ===== Component factories =====

(define (fe-make-bg-component)
  (new-component! "file-explorer-bg"
                  (ExplorerBgState)
                  fe-render-bg
                  (hash "handle_event" fe-handle-event-bg)))

(define (fe-make-fg-component)
  (new-component! "file-explorer-fg"
                  (ExplorerFgState)
                  fe-render-fg
                  (hash "handle_event" fe-handle-event-fg
                        "cursor"       fe-cursor-fn-fg)))

;; ===== Public command =====

(provide open-file-explorer)
;;@doc
;; Toggle/focus the file explorer panel. Bound to space+e.
;; - Inactive      → open panel and focus navigator
;; - Active+focused → close panel entirely
;; - Active+unfocused → push navigator to regain focus
(define (open-file-explorer)
  (cond
    [(not *fe-active*)
     (set! *fe-active*  #t)
     (set! *fe-focused* #t)
     (set! *fe-cursor* 0)
     (set! *fe-window-start* 0)
     (fe-reveal-current-file!)
     (push-component! (fe-make-bg-component))
     (push-component! (fe-make-fg-component))]

    [*fe-focused*
     (set! *fe-active*  #f)
     (set! *fe-focused* #f)
     (pop-last-component-by-name! "file-explorer-fg")
     (pop-last-component-by-name! "file-explorer-bg")
     (set-editor-clip-left! 0)]

    [else
     (set! *fe-focused* #t)
     (fe-reveal-current-file!)
     (push-component! (fe-make-fg-component))]))
