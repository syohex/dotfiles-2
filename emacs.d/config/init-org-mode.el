;;;; Settings


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;;; Org TAGS stuff
;; Tags with fast selection keys
(setq org-tag-alist '(
                      ;; where
                      (:startgroup)
                      ("@home" . ?h)
                      ("@lab" . ?l)
                      ("@shop" . ?s)
                      ("@out" . ?o)
                      (:endgroup)

                      ;; when
                      (:startgroup)
                      ("@night" . ?n)
                      ("@wend" . ?w)    ; weekend
                      ("@day"  . ?d)
                      (:endgroup)

                      ;; type
                      ("mail" . ?E)
                      ("read" . ?r)
                      ("write" . ?R)
                      ("work" . ?W)
                      ("phone" . ?P)
                      ("home" . ?H)))


;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;(setq org-log-done t
(setq org-log-done 'time
      org-log-done 'note
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

;;;; Org-babel
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (ruby . t)
   (emacs-lisp . t)
   (java . t)
   (R . t)
   (python . t)
   (gnuplot . t)
   (clojure . t)
   (ledger . t)
   (org . t)
   (latex . t)
   (js . t)
   (C . t)))

(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))


(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook (lambda ()
                                        (condition-case nil
                                        (org-display-inline-images)
                                        (error nil)))
                                        'append)

;;;; Org-abbrev
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; 1
(define-skeleton skel-org-block-elisp
  "Insert an emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")
(define-abbrev org-mode-abbrev-table "elsrc" "" 'skel-org-block-elisp)

;; 2
(define-skeleton skel-org-block-js
  "Insert a JavaScript block"
  ""
  "#+begin_src js\n"
  _ - \n
  "#+end_src\n")
(define-abbrev org-mode-abbrev-table "jssrc" "" 'skel-org-block-js)

;; 3
(define-skeleton skel-header-block
  "Creates my default header"
  ""
  "#+TITLE: " str "\n"
  "#+AUTHOR: Aaron Bedra\n"
  "#+EMAIL: aaron@aaronbedra.com\n"
  "#+OPTIONS: toc:3 num:nil\n"
  "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />\n")
(define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

;; 4
(define-skeleton skel-org-html-file-name
  "Insert an HTML snippet to reference the file by name"
  ""
  "#+HTML: <strong><i>"str"</i></strong>")
(define-abbrev org-mode-abbrev-table "fname" "" 'skel-org-html-file-name)

(define-skeleton skel-ngx-config
  "Template for NGINX module config file"
  ""
  "ngx_addon_name=ngx_http_" str  "_module\n"
  "HTTP_MODULES=\"$HTTP_MODULES ngx_http_" str "_module\"\n"
  "NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_" str "_module.c\"")
(define-abbrev fundamental-mode-abbrev-table "ngxcnf" "" 'skel-ngx-config)

;; 5
(define-skeleton skel-ngx-module
  "Template for NGINX modules"
  ""
  "#include <nginx.h>\n"
  "#include <ngx_config.h>\n"
  "#include <ngx_core.h>\n"
  "#include <ngx_http.h>\n\n"

  "ngx_module_t ngx_http_" str "_module;\n\n"

  "static ngx_int_t\n"
  "ngx_http_" str "_handler(ngx_http_request_t *r)\n"
  "{\n"
  >"if (r->main->internal) {\n"
  >"return NGX_DECLINED;\n"
  "}" > \n
  \n
  >"ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, \"My new module\");\n\n"
  > _ \n
  >"return NGX_OK;\n"
  "}" > "\n\n"

  "static ngx_int_t\n"
  "ngx_http_"str"_init(ngx_conf_t *cf)\n"
  "{\n"
  >"ngx_http_handler_pt *h;\n"
  >"ngx_http_core_main_conf_t *cmcf;\n\n"

  >"cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);\n"
  >"h = ngx_array_push(&cmcf->phases[NGX_HTTP_ACCESS_PHASE].handlers);\n\n"

  >"if (h == NULL) {\n"
  >"return NGX_ERROR;\n"
  "}" > \n
  \n
  >"*h = ngx_http_"str"_handler;\n\n"

  >"return NGX_OK;\n"
  "}" > \n
  \n
  "static ngx_http_module_t ngx_http_"str"_module_ctx = {\n"
  >"NULL,                 /* preconfiguration */\n"
  >"ngx_http_"str"_init,  /* postconfiguration */\n"
  >"NULL,                 /* create main configuration */\n"
  >"NULL,                 /* init main configuration */\n"
  >"NULL,                 /* create server configuration */\n"
  >"NULL,                 /* merge server configuration */\n"
  >"NULL,                 /* create location configuration */\n"
  >"NULL                  /* merge location configuration */\n"
  "};" > \n
  \n

  "ngx_module_t ngx_http_"str"_module = {\n"
  >"NGX_MODULE_V1,\n"
  >"&ngx_http_"str"_module_ctx,  /* module context */\n"
  >"NULL,                        /* module directives */\n"
  >"NGX_HTTP_MODULE,             /* module type */\n"
  >"NULL,                        /* init master */\n"
  >"NULL,                        /* init module */\n"
  >"NULL,                        /* init process */\n"
  >"NULL,                        /* init thread */\n"
  >"NULL,                        /* exit thread */\n"
  >"NULL,                        /* exit process */\n"
  >"NULL,                        /* exit master */\n"
  >"NGX_MODULE_V1_PADDING\n"
  "};" >)
(require 'cc-mode)
(define-abbrev c-mode-abbrev-table "ngxmod" "" 'skel-ngx-module)

;; 6
(define-skeleton skel-ngx-append-header
  "Template for header appending function for NGINX modules"
  ""
  "static void append_header(ngx_http_request_t *r)\n"
  "{\n"
  > "ngx_table_elt_t *h;\n"
  > "h = ngx_list_push(&r->headers_out.headers);\n"
  > "h->hash = 1;\n"
  > "ngx_str_set(&h->key, \"X-NGINX-Hello\");\n"
  > "ngx_str_set(&h->value, \"Hello NGINX!\");\n"
  "}\n")
(define-abbrev c-mode-abbrev-table "ngxhdr" "" 'skel-ngx-append-header)

(provide 'init-org-mode)