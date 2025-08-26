;; VendorManagement System
;; Supplier onboarding and performance tracking with automated contract compliance
;; A decentralized system for managing vendor relationships and performance metrics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-vendor-not-found (err u101))
(define-constant err-vendor-already-exists (err u102))
(define-constant err-invalid-rating (err u103))
(define-constant err-contract-not-compliant (err u104))
(define-constant err-unauthorized (err u105))

;; Data structures
(define-map vendors 
  principal 
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    registration-date: uint,
    compliance-score: uint,
    performance-rating: uint,
    total-contracts: uint,
    active-status: bool
  })

(define-map contract-compliance
  {vendor: principal, contract-id: uint}
  {
    contract-value: uint,
    delivery-date: uint,
    actual-delivery: uint,
    quality-score: uint,
    is-compliant: bool
  })

;; Global counters
(define-data-var total-vendors uint u0)
(define-data-var next-contract-id uint u1)

;; Function 1: Vendor Onboarding
;; Registers a new vendor with initial compliance and performance metrics
(define-public (onboard-vendor 
  (vendor-address principal)
  (vendor-name (string-ascii 50))
  (category (string-ascii 30))
  (initial-compliance-score uint))
  (begin
    ;; Only contract owner can onboard vendors
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check if vendor doesn't already exist
    (asserts! (is-none (map-get? vendors vendor-address)) err-vendor-already-exists)
    
    ;; Validate compliance score (0-100)
    (asserts! (<= initial-compliance-score u100) err-invalid-rating)
    
    ;; Create vendor record
    (map-set vendors vendor-address
      {
        name: vendor-name,
        category: category,
        registration-date: stacks-block-height,
        compliance-score: initial-compliance-score,
        performance-rating: u0,
        total-contracts: u0,
        active-status: true
      })
    
    ;; Update total vendor count
    (var-set total-vendors (+ (var-get total-vendors) u1))
    
    ;; Emit event for vendor onboarding
    (print {
      event: "vendor-onboarded",
      vendor: vendor-address,
      name: vendor-name,
      category: category,
      compliance-score: initial-compliance-score,
      registration-block: stacks-block-height
    })
    
    (ok true)))

;; Function 2: Performance Tracking and Contract Compliance
;; Updates vendor performance based on contract delivery and automatically checks compliance
(define-public (update-performance-and-compliance
  (vendor-address principal)
  (contract-value uint)
  (expected-delivery uint)
  (actual-delivery uint)
  (quality-score uint))
  (begin
    ;; Only contract owner can update performance
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check if vendor exists
    (asserts! (is-some (map-get? vendors vendor-address)) err-vendor-not-found)
    
    ;; Validate quality score (0-100)
    (asserts! (<= quality-score u100) err-invalid-rating)
    
    (let (
      (current-contract-id (var-get next-contract-id))
      (vendor-data (unwrap! (map-get? vendors vendor-address) err-vendor-not-found))
      
      ;; Calculate compliance metrics
      (delivery-compliance (if (<= actual-delivery expected-delivery) u100 u50))
      (overall-compliance (/ (+ delivery-compliance quality-score) u2))
      (is-compliant (>= overall-compliance u70))
      
      ;; Calculate new performance rating (weighted average)
      (current-contracts (get total-contracts vendor-data))
      (current-rating (get performance-rating vendor-data))
      (new-rating (if (is-eq current-contracts u0)
                    quality-score
                    (/ (+ (* current-rating current-contracts) quality-score)
                       (+ current-contracts u1))))
    )
      
      ;; Record contract compliance data
      (map-set contract-compliance
        {vendor: vendor-address, contract-id: current-contract-id}
        {
          contract-value: contract-value,
          delivery-date: expected-delivery,
          actual-delivery: actual-delivery,
          quality-score: quality-score,
          is-compliant: is-compliant
        })
      
      ;; Update vendor record with new performance metrics
      (map-set vendors vendor-address
        (merge vendor-data
          {
            compliance-score: overall-compliance,
            performance-rating: new-rating,
            total-contracts: (+ current-contracts u1),
            active-status: is-compliant ;; Deactivate if not compliant
          }))
      
      ;; Increment contract ID counter
      (var-set next-contract-id (+ current-contract-id u1))
      
      ;; Emit performance update event
      (print {
        event: "performance-updated",
        vendor: vendor-address,
        contract-id: current-contract-id,
        compliance-score: overall-compliance,
        performance-rating: new-rating,
        is-compliant: is-compliant,
        total-contracts: (+ current-contracts u1)
      })
      
      ;; Return compliance status
      (if is-compliant
        (ok {compliant: true, score: overall-compliance})
        (ok {compliant: false, score: overall-compliance})))))

;; Read-only functions for data retrieval

;; Get vendor information
(define-read-only (get-vendor-info (vendor-address principal))
  (ok (map-get? vendors vendor-address)))

;; Get contract compliance details
(define-read-only (get-contract-compliance (vendor-address principal) (contract-id uint))
  (ok (map-get? contract-compliance {vendor: vendor-address, contract-id: contract-id})))

;; Get total number of registered vendors
(define-read-only (get-total-vendors)
  (ok (var-get total-vendors)))

;; Check if vendor is compliant (active status)
(define-read-only (is-vendor-compliant (vendor-address principal))
  (match (map-get? vendors vendor-address)
    vendor-data (ok (get active-status vendor-data))
    (ok false)))
