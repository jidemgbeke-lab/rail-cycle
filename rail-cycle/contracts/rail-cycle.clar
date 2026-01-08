;; CreatorRail - Digital Collectibles Platform with Continuous Royalties
;; A blockchain platform for sustainable creator monetization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-amount (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% in basis points
(define-data-var next-collectible-id uint u0)
(define-data-var next-stake-id uint u0)

;; Data Maps
(define-map collectibles
  { collectible-id: uint }
  {
    creator: principal,
    metadata-uri: (string-ascii 256),
    royalty-percentage: uint,
    total-interactions: uint,
    creation-block: uint,
    active: bool
  }
)

(define-map collectible-owners
  { collectible-id: uint, owner: principal }
  { owned: bool }
)

(define-map interaction-royalties
  { collectible-id: uint }
  { accumulated: uint }
)

(define-map creator-stakes
  { stake-id: uint }
  {
    staker: principal,
    creator: principal,
    amount: uint,
    block-height: uint,
    active: bool
  }
)

(define-map creator-total-staked
  { creator: principal }
  { total: uint }
)

;; Private Functions
(define-private (calculate-fee (amount uint) (percentage uint))
  (/ (* amount percentage) u10000)
)

;; Public Functions - Collectible Management

(define-public (mint-collectible (metadata-uri (string-ascii 256)) (royalty-percentage uint))
  (let
    (
      (collectible-id (var-get next-collectible-id))
    )
    (asserts! (<= royalty-percentage u1000) err-invalid-amount) ;; Max 10% royalty
    (map-set collectibles
      { collectible-id: collectible-id }
      {
        creator: tx-sender,
        metadata-uri: metadata-uri,
        royalty-percentage: royalty-percentage,
        total-interactions: u0,
        creation-block: block-height,
        active: true
      }
    )
    (map-set collectible-owners
      { collectible-id: collectible-id, owner: tx-sender }
      { owned: true }
    )
    (map-set interaction-royalties
      { collectible-id: collectible-id }
      { accumulated: u0 }
    )
    (var-set next-collectible-id (+ collectible-id u1))
    (ok collectible-id)
  )
)

(define-public (record-interaction (collectible-id uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { collectible-id: collectible-id }) err-not-found))
      (current-interactions (get total-interactions collectible))
    )
    (asserts! (get active collectible) err-unauthorized)
    (map-set collectibles
      { collectible-id: collectible-id }
      (merge collectible { total-interactions: (+ current-interactions u1) })
    )
    (ok true)
  )
)

(define-public (distribute-royalty (collectible-id uint) (amount uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { collectible-id: collectible-id }) err-not-found))
      (creator (get creator collectible))
      (royalty-pct (get royalty-percentage collectible))
      (platform-fee-pct (var-get platform-fee-percentage))
      (royalty-amount (calculate-fee amount royalty-pct))
      (platform-fee (calculate-fee amount platform-fee-pct))
      (net-amount (- royalty-amount platform-fee))
      (current-royalties (unwrap! (map-get? interaction-royalties { collectible-id: collectible-id }) err-not-found))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active collectible) err-unauthorized)
    
    ;; Transfer royalty to creator
    (try! (stx-transfer? net-amount tx-sender creator))
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; Update accumulated royalties
    (map-set interaction-royalties
      { collectible-id: collectible-id }
      { accumulated: (+ (get accumulated current-royalties) net-amount) }
    )
    
    ;; Record interaction
    (try! (record-interaction collectible-id))
    
    (ok net-amount)
  )
)

;; Staking Functions

(define-public (stake-creator (creator principal) (amount uint))
  (let
    (
      (stake-id (var-get next-stake-id))
      (current-total (default-to { total: u0 } (map-get? creator-total-staked { creator: creator })))
    )
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer stake amount to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Record stake
    (map-set creator-stakes
      { stake-id: stake-id }
      {
        staker: tx-sender,
        creator: creator,
        amount: amount,
        block-height: block-height,
        active: true
      }
    )
    
    ;; Update creator total staked
    (map-set creator-total-staked
      { creator: creator }
      { total: (+ (get total current-total) amount) }
    )
    
    (var-set next-stake-id (+ stake-id u1))
    (ok stake-id)
  )
)

(define-public (unstake (stake-id uint))
  (let
    (
      (stake (unwrap! (map-get? creator-stakes { stake-id: stake-id }) err-not-found))
      (staker (get staker stake))
      (creator (get creator stake))
      (amount (get amount stake))
      (current-total (unwrap! (map-get? creator-total-staked { creator: creator }) err-not-found))
    )
    (asserts! (is-eq tx-sender staker) err-unauthorized)
    (asserts! (get active stake) err-unauthorized)
    
    ;; Return stake to staker
    (try! (as-contract (stx-transfer? amount tx-sender staker)))
    
    ;; Deactivate stake
    (map-set creator-stakes
      { stake-id: stake-id }
      (merge stake { active: false })
    )
    
    ;; Update creator total
    (map-set creator-total-staked
      { creator: creator }
      { total: (- (get total current-total) amount) }
    )
    
    (ok true)
  )
)

;; Read-Only Functions

(define-read-only (get-collectible (collectible-id uint))
  (map-get? collectibles { collectible-id: collectible-id })
)

(define-read-only (get-collectible-royalties (collectible-id uint))
  (map-get? interaction-royalties { collectible-id: collectible-id })
)

(define-read-only (get-stake (stake-id uint))
  (map-get? creator-stakes { stake-id: stake-id })
)

(define-read-only (get-creator-total-staked (creator principal))
  (default-to { total: u0 } (map-get? creator-total-staked { creator: creator }))
)

(define-read-only (is-collectible-owner (collectible-id uint) (owner principal))
  (default-to { owned: false } (map-get? collectible-owners { collectible-id: collectible-id, owner: owner }))
)

;; Admin Functions

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

(define-public (toggle-collectible (collectible-id uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { collectible-id: collectible-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator collectible)) err-unauthorized)
    (map-set collectibles
      { collectible-id: collectible-id }
      (merge collectible { active: (not (get active collectible)) })
    )
    (ok true)
  )
)