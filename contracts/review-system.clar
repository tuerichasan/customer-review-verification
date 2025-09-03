
;; title: Customer Review Verification System
;; version: 1.0.0
;; summary: A comprehensive product review system with purchase verification and merchant response management
;; description: This contract manages product reviews, ensures authenticity through purchase verification, detects fake reviews, and enables merchant responses

;; constants
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_INVALID_REVIEW (err u402))
(define-constant ERR_ALREADY_REVIEWED (err u403))
(define-constant ERR_PRODUCT_NOT_FOUND (err u404))
(define-constant ERR_REVIEW_NOT_FOUND (err u405))
(define-constant ERR_INSUFFICIENT_PURCHASE (err u406))
(define-constant ERR_MERCHANT_NOT_FOUND (err u407))
(define-constant ERR_INVALID_RATING (err u408))

(define-constant MAX_RATING u5)
(define-constant MIN_RATING u1)
(define-constant REVIEW_REWARD u100)
(define-constant FAKE_REVIEW_PENALTY u200)

;; data vars
(define-data-var contract-owner principal tx-sender)
(define-data-var review-counter uint u0)
(define-data-var product-counter uint u0)

;; data maps
(define-map products
  { product-id: uint }
  {
    merchant: principal,
    name: (string-ascii 128),
    price: uint,
    total-reviews: uint,
    avg-rating: uint,
    verified-purchases: uint
  }
)

(define-map purchases
  { buyer: principal, product-id: uint }
  {
    purchase-amount: uint,
    purchase-block: uint,
    verified: bool
  }
)

(define-map reviews
  { review-id: uint }
  {
    product-id: uint,
    reviewer: principal,
    rating: uint,
    review-text: (string-ascii 500),
    purchase-verified: bool,
    flagged-fake: bool,
    review-block: uint,
    helpfulness-score: uint
  }
)

(define-map merchant-responses
  { review-id: uint }
  {
    merchant: principal,
    response-text: (string-ascii 300),
    response-block: uint
  }
)

(define-map reviewer-reputation
  { reviewer: principal }
  {
    total-reviews: uint,
    verified-reviews: uint,
    flagged-reviews: uint,
    reputation-score: uint
  }
)

(define-map product-review-tracker
  { reviewer: principal, product-id: uint }
  { reviewed: bool }
)

;; public functions

;; Register a new product
(define-public (register-product (name (string-ascii 128)) (price uint))
  (let (
    (new-product-id (+ (var-get product-counter) u1))
  )
    (asserts! (> (len name) u0) ERR_INVALID_REVIEW)
    (asserts! (> price u0) ERR_INVALID_REVIEW)
    
    (map-set products
      { product-id: new-product-id }
      {
        merchant: tx-sender,
        name: name,
        price: price,
        total-reviews: u0,
        avg-rating: u0,
        verified-purchases: u0
      }
    )
    
    (var-set product-counter new-product-id)
    (ok new-product-id)
  )
)

;; Record a purchase
(define-public (record-purchase (product-id uint) (purchase-amount uint))
  (let (
    (product-data (unwrap! (map-get? products { product-id: product-id }) ERR_PRODUCT_NOT_FOUND))
  )
    (asserts! (>= purchase-amount (get price product-data)) ERR_INSUFFICIENT_PURCHASE)
    
    (map-set purchases
      { buyer: tx-sender, product-id: product-id }
      {
        purchase-amount: purchase-amount,
        purchase-block: burn-block-height,
        verified: true
      }
    )
    
    (map-set products
      { product-id: product-id }
      (merge product-data { verified-purchases: (+ (get verified-purchases product-data) u1) })
    )
    
    (ok true)
  )
)

;; Submit a review
(define-public (submit-review (product-id uint) (rating uint) (review-text (string-ascii 500)))
  (let (
    (new-review-id (+ (var-get review-counter) u1))
    (product-data (unwrap! (map-get? products { product-id: product-id }) ERR_PRODUCT_NOT_FOUND))
    (purchase-data (map-get? purchases { buyer: tx-sender, product-id: product-id }))
    (already-reviewed (default-to { reviewed: false } (map-get? product-review-tracker { reviewer: tx-sender, product-id: product-id })))
    (is-purchase-verified (is-some purchase-data))
    (reputation-data (default-to { total-reviews: u0, verified-reviews: u0, flagged-reviews: u0, reputation-score: u0 }
                                 (map-get? reviewer-reputation { reviewer: tx-sender })))
  )
    (asserts! (and (>= rating MIN_RATING) (<= rating MAX_RATING)) ERR_INVALID_RATING)
    (asserts! (> (len review-text) u0) ERR_INVALID_REVIEW)
    (asserts! (not (get reviewed already-reviewed)) ERR_ALREADY_REVIEWED)
    
    ;; Create review
    (map-set reviews
      { review-id: new-review-id }
      {
        product-id: product-id,
        reviewer: tx-sender,
        rating: rating,
        review-text: review-text,
        purchase-verified: is-purchase-verified,
        flagged-fake: false,
        review-block: burn-block-height,
        helpfulness-score: u0
      }
    )
    
    ;; Mark as reviewed
    (map-set product-review-tracker
      { reviewer: tx-sender, product-id: product-id }
      { reviewed: true }
    )
    
    ;; Update product statistics
    (let (
      (new-total-reviews (+ (get total-reviews product-data) u1))
      (new-avg-rating (/ (+ (* (get avg-rating product-data) (get total-reviews product-data)) rating) new-total-reviews))
    )
      (map-set products
        { product-id: product-id }
        (merge product-data {
          total-reviews: new-total-reviews,
          avg-rating: new-avg-rating
        })
      )
    )
    
    ;; Update reviewer reputation
    (map-set reviewer-reputation
      { reviewer: tx-sender }
      {
        total-reviews: (+ (get total-reviews reputation-data) u1),
        verified-reviews: (+ (get verified-reviews reputation-data) (if is-purchase-verified u1 u0)),
        flagged-reviews: (get flagged-reviews reputation-data),
        reputation-score: (calculate-reputation-score 
                          (+ (get total-reviews reputation-data) u1)
                          (+ (get verified-reviews reputation-data) (if is-purchase-verified u1 u0))
                          (get flagged-reviews reputation-data))
      }
    )
    
    (var-set review-counter new-review-id)
    (ok new-review-id)
  )
)

;; Merchant response to review
(define-public (respond-to-review (review-id uint) (response-text (string-ascii 300)))
  (let (
    (review-data (unwrap! (map-get? reviews { review-id: review-id }) ERR_REVIEW_NOT_FOUND))
    (product-data (unwrap! (map-get? products { product-id: (get product-id review-data) }) ERR_PRODUCT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get merchant product-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> (len response-text) u0) ERR_INVALID_REVIEW)
    
    (map-set merchant-responses
      { review-id: review-id }
      {
        merchant: tx-sender,
        response-text: response-text,
        response-block: burn-block-height
      }
    )
    
    (ok true)
  )
)

;; Flag review as fake (admin function)
(define-public (flag-fake-review (review-id uint))
  (let (
    (review-data (unwrap! (map-get? reviews { review-id: review-id }) ERR_REVIEW_NOT_FOUND))
    (reviewer (get reviewer review-data))
    (reputation-data (default-to { total-reviews: u0, verified-reviews: u0, flagged-reviews: u0, reputation-score: u0 }
                                 (map-get? reviewer-reputation { reviewer: reviewer })))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    
    ;; Flag the review
    (map-set reviews
      { review-id: review-id }
      (merge review-data { flagged-fake: true })
    )
    
    ;; Update reviewer reputation with penalty
    (map-set reviewer-reputation
      { reviewer: reviewer }
      {
        total-reviews: (get total-reviews reputation-data),
        verified-reviews: (get verified-reviews reputation-data),
        flagged-reviews: (+ (get flagged-reviews reputation-data) u1),
        reputation-score: (calculate-reputation-score 
                          (get total-reviews reputation-data)
                          (get verified-reviews reputation-data)
                          (+ (get flagged-reviews reputation-data) u1))
      }
    )
    
    (ok true)
  )
)

;; Vote on review helpfulness
(define-public (vote-helpful (review-id uint))
  (let (
    (review-data (unwrap! (map-get? reviews { review-id: review-id }) ERR_REVIEW_NOT_FOUND))
  )
    (map-set reviews
      { review-id: review-id }
      (merge review-data { helpfulness-score: (+ (get helpfulness-score review-data) u1) })
    )
    
    (ok true)
  )
)

;; read only functions

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

;; Get merchant response
(define-read-only (get-merchant-response (review-id uint))
  (map-get? merchant-responses { review-id: review-id })
)

;; Get reviewer reputation
(define-read-only (get-reviewer-reputation (reviewer principal))
  (map-get? reviewer-reputation { reviewer: reviewer })
)

;; Check if purchase is verified
(define-read-only (is-purchase-verified (buyer principal) (product-id uint))
  (is-some (map-get? purchases { buyer: buyer, product-id: product-id }))
)

;; Get purchase details
(define-read-only (get-purchase (buyer principal) (product-id uint))
  (map-get? purchases { buyer: buyer, product-id: product-id })
)

;; Check if user has already reviewed product
(define-read-only (has-reviewed-product (reviewer principal) (product-id uint))
  (default-to false (get reviewed (map-get? product-review-tracker { reviewer: reviewer, product-id: product-id })))
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-products: (var-get product-counter),
    total-reviews: (var-get review-counter)
  }
)

;; private functions

;; Calculate reputation score based on review history
(define-private (calculate-reputation-score (total-reviews uint) (verified-reviews uint) (flagged-reviews uint))
  (let (
    (verification-rate (if (> total-reviews u0) (/ (* verified-reviews u100) total-reviews) u0))
    (flag-penalty (* flagged-reviews u20))
    (base-score (+ (* total-reviews u10) verification-rate))
  )
    (if (>= base-score flag-penalty)
      (- base-score flag-penalty)
      u0
    )
  )
)
