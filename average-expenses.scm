;; -*-scheme-*-

;; This is a minimum report definition in GnuCash.
;; It illustrates the the minimum definitions needed to create
;; a new GnuCash report.
;; It will create an empty page with heading 'Prototype'.
;; To be used as template.

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------

(define-module (gnucash report standard-reports average-expenses))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(debug-enable 'backtrace)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0) ;for gnc-build-url

;; ------------------------------------------------------------------
;; Define the Options for this report
;; ------------------------------------------------------------------


;; define all option's names so that they are properly defined
;; in *one* place.
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))


(define (options-generator)    
  (let* ((options (gnc:new-options))         

    ;; This is just a helper function for making options.
    ;; See libgnucash/app-utils/options.scm for details.
    (add-option 
     (lambda (new-option)
       (gnc:register-option options new-option))))

    (add-option
     (gnc:make-color-option
      (N_ "Page Style") (N_ "Background Color")
      "f" (N_ "This is a color option.")
      (list #xf6 #xff #xdb #xff)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Page Style") (N_ "Text Color")
      "f" (N_ "This is a color option.")
      (list #x00 #x00 #x00 #xff)
      255
      #f))
 
    (add-option
     (gnc:make-account-list-option
      (N_ "Accounts List") (N_ "An account list option")
      "g" (N_ "This is an account list option.")
      ;; FIXME : this used to be gnc:get-current-accounts, but 
      ;; that doesn't exist any more.
      (lambda () '())
      #f #t))
 
    ;; This is a date/time option. The user can pick a date and,
    ;; possibly, a time. Times are stored as an integer specifying
    ;; number of seconds measured from Jan 1, 1970, i.e.,
    ;; Unix time. The last option is false, so the user can only
    ;; select a date, not a time. The default value is the current
    ;; time.
    
;;    (add-option
;;     (gnc:make-date-option
;;      (N_ "Date Selection") (N_ "Start Period")
;;      "d" (N_ "This is a date option.")
;;      (lambda () (cons 'absolute (current-time)))
;;      #f 'absolute #f ))

;;    (add-option
;;     (gnc:make-date-option
;;      (N_ "Date Selection") (N_ "End Period")
;;      "d" (N_ "This is a date option.")
;;      (lambda () (cons 'absolute (current-time)))
;;      #f 'absolute #f ))
	
	(gnc:options-add-date-interval! 
	  options (N_ "Date Selection") (N_ "Start Period") (N_ "End Period") "c")
	

    options))

;; ------------------------------------------------------------------
;; Render the HTML document
;; ------------------------------------------------------------------

(define (document-renderer report-obj)

  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (define (calculate-amount-spent acct start-period end-period)
	(- (xaccAccountGetBalanceAsOfDate acct end-period)
	   (xaccAccountGetBalanceAsOfDate acct start-period)))

	(define (calculate-average-monthly-amount acct start-period end-period)
	  (let ((start-amount (xaccAccountGetBalanceAsOfDate acct start-period))
			(end-amount (xaccAccountGetBalanceAsOfDate acct end-period)))
		(/ (- end-amount start-amount) (- (gnc:date-to-month-fraction end-period) 
										  (gnc:date-to-month-fraction start-period)))
		))


  (let ((bg-color-op  (get-op   "Page Style" "Background Color"))
        (accounts     (op-value "Accounts List"   "An account list option"))
        (txt-color-op (get-op   "Page Style" "Text Color"))
		(reportdate (gnc:get-today))
		(start-period (gnc:date-option-absolute-time
						(op-value "Date Selection" "Start Period")))
		(end-period (gnc:date-option-absolute-time
					  (op-value "Date Selection" "End Period")))
		(document (gnc:make-html-document)))


	(gnc:html-document-set-style!
	  document "body"
	  'attribute (list "bgcolor" (gnc:color-option->html bg-color-op))
	  'font-color (gnc:color-option->html txt-color-op))

    (gnc:html-document-set-title! document (_ "My Average Expenses"))

	(if (not (null? accounts))
		(let ((table (gnc:make-html-table) ))

		(gnc:html-table-append-column! table
			 (cons 
				(gnc:make-html-text
				   (gnc:html-markup-b "Current Balance"))
			   (map (lambda (acct)
				   (gnc:make-html-text 
					   (gnc:html-markup-p 
						 (append (xaccAccountGetBalanceAsOfDate acct reportdate)))
					 ))
				 accounts)))

		(gnc:html-table-append-column! table
			 (cons 
				(gnc:make-html-text
				   (gnc:html-markup-b "Total Spent"))
		    (map (lambda (acct)
				   (gnc:make-html-text 
					   (gnc:html-markup-p 
						 (append (calculate-amount-spent acct start-period end-period)))
					 ))
				 accounts)))

		(gnc:html-table-append-column! table
			 (cons 
				(gnc:make-html-text
				   (gnc:html-markup-b "Monthly Average"))
		    (map (lambda (acct)
				   (gnc:make-html-text 
					   (gnc:html-markup-p 
						 (append (calculate-average-monthly-amount  
								   acct start-period end-period )))
					 ))
				 accounts)))

		(gnc:html-table-append-column! table
			 (cons 
				(gnc:make-html-text
				   (gnc:html-markup-b "Account Name"))
			(map (lambda (acct)
				   (gnc:make-html-text
				     (gnc:html-markup-anchor
					   (gnc-build-url URL-TYPE-REGISTER 
						  (string-append "account=" (gnc-account-get-full-name acct)) "")
				   (xaccAccountGetName acct))
					 ))
				 accounts)))

		(gnc:html-table-append-row! table
				(list 
				  (gnc:make-html-text
						 (gnc:html-markup-b 
						   (apply + 
							 (map (lambda (acct) (xaccAccountGetBalanceAsOfDate acct reportdate))
								  accounts)
							 )))
				  (gnc:make-html-text
						 (gnc:html-markup-b 
						   (apply + 
							 (map (lambda (acct) (calculate-amount-spent acct start-period end-period))
								  accounts)
							 )))
				  (gnc:make-html-text
						 (gnc:html-markup-b 
						   (apply + 
							 (map (lambda (acct) (calculate-average-monthly-amount  
								   acct start-period end-period ))
								  accounts)
							 )))
				  (gnc:make-html-text
						 (gnc:html-markup-b "Total"))
						 ))


		(gnc:html-document-add-object! document table))

		(gnc:html-document-add-object! 
		  document
		  (gnc:make-html-text
			(gnc:html-markup-p "Select some accounts to average"))))

      document))

;; ------------------------------------------------------------------
;; Define the actual report
;; ------------------------------------------------------------------

(gnc:define-report
 'version 1
 'name (N_ "My Average Expenses")
 'report-guid "2340e0d34f344e50a625298cbf2c3808"
 'menu-tip (N_ "Unstable. Used for Testing.")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator options-generator
 'renderer document-renderer)
