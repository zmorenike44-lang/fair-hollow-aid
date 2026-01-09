;; FairHollow - Decentralized Microfinance Platform
;; Core smart contract for community-validated lending

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-loan-active (err u104))
(define-constant err-insufficient-attestations (err u105))
(define-constant err-already-attested (err u106))
(define-constant err-loan-not-active (err u107))

;; Data Variables
(define-data-var loan-counter uint u0)
(define-data-var min-attestations uint u3)

;; Data Maps
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    amount: uint,
    interest-rate: uint, ;; basis points (e.g., 500 = 5%)
    repaid: uint,
    status: (string-ascii 20),
    impact-score: uint,
    attestation-count: uint,
    milestone-current: uint,
    milestone-total: uint,
    created-at: uint
  }
)

(define-map attestations
  { loan-id: uint, attester: principal }
  {
    stake-amount: uint,
    attested-at: uint,
    rating: uint ;; 1-100
  }
)

(define-map user-reputation
  { user: principal }
  {
    total-attestations: uint,
    successful-loans: uint,
    reputation-score: uint
  }
)

(define-map loan-milestones
  { loan-id: uint, milestone-id: uint }
  {
    description: (string-ascii 100),
    amount: uint,
    released: bool,
    verified: bool
  }
)

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-attestation (loan-id uint) (attester principal))
  (map-get? attestations { loan-id: loan-id, attester: attester })
)

(define-read-only (get-reputation (user principal))
  (default-to 
    { total-attestations: u0, successful-loans: u0, reputation-score: u50 }
    (map-get? user-reputation { user: user })
  )
)

(define-read-only (get-milestone (loan-id uint) (milestone-id uint))
  (map-get? loan-milestones { loan-id: loan-id, milestone-id: milestone-id })
)

(define-read-only (get-loan-counter)
  (ok (var-get loan-counter))
)

;; Public functions

;; Create a new loan request
(define-public (create-loan (amount uint) (interest-rate uint) (milestone-count uint))
  (let
    (
      (loan-id (+ (var-get loan-counter) u1))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= interest-rate u10000) err-invalid-amount) ;; Max 100% interest
    
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        amount: amount,
        interest-rate: interest-rate,
        repaid: u0,
        status: "pending",
        impact-score: u50,
        attestation-count: u0,
        milestone-current: u0,
        milestone-total: milestone-count,
        created-at: block-height
      }
    )
    
    (var-set loan-counter loan-id)
    (ok loan-id)
  )
)

;; Community members attest to loan viability
(define-public (attest-loan (loan-id uint) (stake-amount uint) (rating uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (existing-attestation (get-attestation loan-id tx-sender))
    )
    (asserts! (is-none existing-attestation) err-already-attested)
    (asserts! (is-eq (get status loan) "pending") err-loan-active)
    (asserts! (and (>= rating u1) (<= rating u100)) err-invalid-amount)
    (asserts! (> stake-amount u0) err-invalid-amount)
    
    ;; Record attestation
    (map-set attestations
      { loan-id: loan-id, attester: tx-sender }
      {
        stake-amount: stake-amount,
        attested-at: block-height,
        rating: rating
      }
    )
    
    ;; Update loan attestation count
    (map-set loans
      { loan-id: loan-id }
      (merge loan { attestation-count: (+ (get attestation-count loan) u1) })
    )
    
    ;; Update attester reputation
    (let
      (
        (rep (get-reputation tx-sender))
      )
      (map-set user-reputation
        { user: tx-sender }
        (merge rep { total-attestations: (+ (get total-attestations rep) u1) })
      )
    )
    
    (ok true)
  )
)

;; Activate loan once minimum attestations reached
(define-public (activate-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status loan) "pending") err-loan-active)
    (asserts! (>= (get attestation-count loan) (var-get min-attestations)) err-insufficient-attestations)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "active" })
    )
    
    (ok true)
  )
)

;; Record milestone completion
(define-public (complete-milestone (loan-id uint) (milestone-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (milestone (unwrap! (get-milestone loan-id milestone-id) err-not-found))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status loan) "active") err-loan-not-active)
    
    (map-set loan-milestones
      { loan-id: loan-id, milestone-id: milestone-id }
      (merge milestone { verified: true })
    )
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { milestone-current: (+ (get milestone-current loan) u1) })
    )
    
    (ok true)
  )
)

;; Make repayment
(define-public (make-repayment (loan-id uint) (amount uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status loan) "active") err-loan-not-active)
    (asserts! (> amount u0) err-invalid-amount)
    
    (let
      (
        (new-repaid (+ (get repaid loan) amount))
        (total-due (+ (get amount loan) (/ (* (get amount loan) (get interest-rate loan)) u10000)))
      )
      (map-set loans
        { loan-id: loan-id }
        (merge loan { 
          repaid: new-repaid,
          status: (if (>= new-repaid total-due) "completed" "active")
        })
      )
      
      ;; Update borrower reputation on completion
      (if (>= new-repaid total-due)
        (let
          (
            (rep (get-reputation tx-sender))
            (new-rep-score (+ (get reputation-score rep) u10))
          )
          (map-set user-reputation
            { user: tx-sender }
            (merge rep { 
              successful-loans: (+ (get successful-loans rep) u1),
              reputation-score: (if (> new-rep-score u100) u100 new-rep-score)
            })
          )
          (ok true)
        )
        (ok true)
      )
    )
  )
)

;; Update impact score (called by oracle or governance)
(define-public (update-impact-score (loan-id uint) (new-score uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-score u100) err-invalid-amount)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { impact-score: new-score })
    )
    
    (ok true)
  )
)

;; Admin function to update minimum attestations
(define-public (set-min-attestations (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-attestations new-min)
    (ok true)
  )
)