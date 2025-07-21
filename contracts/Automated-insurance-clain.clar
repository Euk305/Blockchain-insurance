;; Automated-insurance-clain
;; A robust automated insurance smart contract for Stacks blockchain.
;; This contract allows users to purchase insurance, submit claims, and automates claim approval based on external data (oracle).
;; Admin can fund the contract and manage insurance parameters.

;; constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_POLICY_NOT_FOUND (err u101))
(define-constant ERR_POLICY_ACTIVE (err u102))
(define-constant ERR_POLICY_INACTIVE (err u103))
(define-constant ERR_CLAIM_NOT_FOUND (err u104))
(define-constant ERR_CLAIM_ALREADY_SUBMITTED (err u105))
(define-constant ERR_INSUFFICIENT_FUNDS (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_ALREADY_INSURED (err u108))
(define-constant ERR_NOT_INSURED (err u109))
(define-constant ERR_CLAIM_NOT_ELIGIBLE (err u110))

(define-constant POLICY_DURATION_BLOCKS u52560) ;; ~1 month at 10s/block
(define-constant INSURANCE_PREMIUM u1000000) ;; 1 STX (microstacks)
(define-constant INSURANCE_PAYOUT u5000000) ;; 5 STX (microstacks)

;; data maps and vars
(define-data-var admin principal tx-sender)                  ;; The contract admin (deployer by default)
(define-data-var contract-balance uint u0)                   ;; Tracks total STX held by contract
(define-data-var total-policies uint u0)                     ;; Total number of policies ever issued
(define-data-var total-claims uint u0)                       ;; Total number of claims ever submitted
(define-data-var total-approved-claims uint u0)              ;; Total number of claims approved
(define-data-var total-rejected-claims uint u0)              ;; Total number of claims rejected
(define-data-var last-policy-block uint u0)                  ;; Block height of last policy purchase
(define-data-var last-claim-block uint u0)                   ;; Block height of last claim submission

;; policy: {owner: principal, start-block: uint, active: bool}
(define-map policies principal
  {
    start-block: uint,
    active: bool
  }
)

;; claim: {owner: principal, block: uint, status: (pending|approved|rejected)}
(define-map claims principal
  {
    block: uint,
    status: (string-ascii 10)
  }
)

;; private functions

(define-private (is-admin (sender principal))
  (is-eq sender (var-get admin))
)

(define-private (policy-active? (owner principal))
  (let ((policy (map-get? policies owner)))
    (if (is-some policy)
      (let (
        (p (unwrap-panic policy))
        (start (get start-block p))
        (active (get active p))
        (now block-height)
      )
        (and active (<= start now) (< now (+ start POLICY_DURATION_BLOCKS)))
      )
      false
    )
  )
)

;; public functions

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (fund-contract (amount uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

(define-public (buy-policy (amount uint))
  (begin
    (asserts! (not (policy-active? tx-sender)) ERR_ALREADY_INSURED)
    (asserts! (is-eq amount INSURANCE_PREMIUM) ERR_INVALID_AMOUNT)
    (map-set policies tx-sender {start-block: block-height, active: true})
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

(define-public (cancel-policy)
  (begin
    (let ((policy (map-get? policies tx-sender)))
      (asserts! (is-some policy) ERR_POLICY_NOT_FOUND)
      (let ((p (unwrap! policy ERR_POLICY_NOT_FOUND)))
        (asserts! (get active p) ERR_POLICY_INACTIVE)
        (map-set policies tx-sender {start-block: (get start-block p), active: false})
        (ok true)
      )
    )
  )
)

(define-public (submit-claim)
  (begin
    (asserts! (policy-active? tx-sender) ERR_NOT_INSURED)
    (asserts! (is-none (map-get? claims tx-sender)) ERR_CLAIM_ALREADY_SUBMITTED)
    (map-set claims tx-sender {block: block-height, status: "pending"})
    (ok true)
  )
)

;; Oracle or admin calls this to approve/reject claims
(define-public (process-claim (user principal) (approve bool))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (let ((claim (map-get? claims user)))
      (asserts! (is-some claim) ERR_CLAIM_NOT_FOUND)
      (let ((c (unwrap! claim ERR_CLAIM_NOT_FOUND)))
        (asserts! (is-eq (get status c) "pending") ERR_CLAIM_NOT_ELIGIBLE)
        (if approve
          (begin
            (asserts! (>= (var-get contract-balance) INSURANCE_PAYOUT) ERR_INSUFFICIENT_FUNDS)
            (try! (stx-transfer? INSURANCE_PAYOUT (var-get admin) user))
            (var-set contract-balance (- (var-get contract-balance) INSURANCE_PAYOUT))
            (map-set claims user {block: (get block c), status: "approved"})
            (ok "approved")
          )
          (begin
            (map-set claims user {block: (get block c), status: "rejected"})
            (ok "rejected")
          )
        )
      )
    )
  )
)

(define-public (get-policy (user principal))
  (ok (map-get? policies user))
)

(define-public (get-claim (user principal))
  (ok (map-get? claims user))
)
