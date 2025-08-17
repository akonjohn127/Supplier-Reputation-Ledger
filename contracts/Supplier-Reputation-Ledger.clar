(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SUPPLIER_NOT_FOUND (err u101))
(define-constant ERR_INVALID_RATING (err u102))
(define-constant ERR_ALREADY_RATED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_SUPPLIER_EXISTS (err u105))
(define-constant ERR_INVALID_PARAMETERS (err u106))

(define-data-var next-supplier-id uint u1)
(define-data-var platform-fee uint u50)
(define-data-var min-stake uint u1000000)

(define-map suppliers
    { supplier-id: uint }
    {
        owner: principal,
        name: (string-ascii 64),
        category: (string-ascii 32),
        total-rating: uint,
        rating-count: uint,
        average-rating: uint,
        stake-amount: uint,
        created-at: uint,
        is-active: bool,
    }
)

(define-map supplier-by-principal
    { owner: principal }
    { supplier-id: uint }
)

(define-map ratings
    {
        supplier-id: uint,
        reviewer: principal,
    }
    {
        rating: uint,
        comment: (string-ascii 256),
        created-at: uint,
    }
)

(define-map transactions
    { transaction-id: uint }
    {
        supplier-id: uint,
        buyer: principal,
        amount: uint,
        status: (string-ascii 16),
        created-at: uint,
        completed-at: (optional uint),
    }
)

(define-data-var next-transaction-id uint u1)

(define-map escrow-funds
    { transaction-id: uint }
    { amount: uint }
)

(define-read-only (get-supplier (supplier-id uint))
    (map-get? suppliers { supplier-id: supplier-id })
)

(define-read-only (get-supplier-by-owner (owner principal))
    (match (map-get? supplier-by-principal { owner: owner })
        supplier-data (get-supplier (get supplier-id supplier-data))
        none
    )
)

(define-read-only (get-rating
        (supplier-id uint)
        (reviewer principal)
    )
    (map-get? ratings {
        supplier-id: supplier-id,
        reviewer: reviewer,
    })
)

(define-read-only (get-transaction (transaction-id uint))
    (map-get? transactions { transaction-id: transaction-id })
)

(define-read-only (get-next-supplier-id)
    (var-get next-supplier-id)
)

(define-read-only (get-platform-fee)
    (var-get platform-fee)
)

(define-read-only (get-min-stake)
    (var-get min-stake)
)

(define-read-only (calculate-reputation-score
        (total-rating uint)
        (rating-count uint)
    )
    (if (is-eq rating-count u0)
        u0
        (/ (* total-rating u100) rating-count)
    )
)

(define-public (register-supplier
        (name (string-ascii 64))
        (category (string-ascii 32))
    )
    (let (
            (supplier-id (var-get next-supplier-id))
            (stake (var-get min-stake))
        )
        (asserts! (>= (stx-get-balance tx-sender) stake) ERR_INSUFFICIENT_FUNDS)
        (asserts! (is-none (map-get? supplier-by-principal { owner: tx-sender }))
            ERR_SUPPLIER_EXISTS
        )
        (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
        (asserts! (> (len category) u0) ERR_INVALID_PARAMETERS)

        (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))

        (map-set suppliers { supplier-id: supplier-id } {
            owner: tx-sender,
            name: name,
            category: category,
            total-rating: u0,
            rating-count: u0,
            average-rating: u0,
            stake-amount: stake,
            created-at: stacks-block-height,
            is-active: true,
        })

        (map-set supplier-by-principal { owner: tx-sender } { supplier-id: supplier-id })

        (var-set next-supplier-id (+ supplier-id u1))
        (ok supplier-id)
    )
)

(define-public (rate-supplier
        (supplier-id uint)
        (rating uint)
        (comment (string-ascii 256))
    )
    (let (
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
            (existing-rating (map-get? ratings {
                supplier-id: supplier-id,
                reviewer: tx-sender,
            }))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
        (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)
        (asserts! (not (is-eq tx-sender (get owner supplier-data)))
            ERR_UNAUTHORIZED
        )

        (let (
                (new-total-rating (+ (get total-rating supplier-data) rating))
                (new-rating-count (+ (get rating-count supplier-data) u1))
                (new-average (calculate-reputation-score new-total-rating new-rating-count))
            )
            (map-set ratings {
                supplier-id: supplier-id,
                reviewer: tx-sender,
            } {
                rating: rating,
                comment: comment,
                created-at: stacks-block-height,
            })

            (map-set suppliers { supplier-id: supplier-id }
                (merge supplier-data {
                    total-rating: new-total-rating,
                    rating-count: new-rating-count,
                    average-rating: new-average,
                })
            )

            (ok true)
        )
    )
)

(define-public (create-transaction
        (supplier-id uint)
        (amount uint)
    )
    (let (
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
            (transaction-id (var-get next-transaction-id))
            (fee (/ (* amount (var-get platform-fee)) u10000))
            (total-amount (+ amount fee))
        )
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)
        (asserts! (> amount u0) ERR_INVALID_PARAMETERS)
        (asserts! (>= (stx-get-balance tx-sender) total-amount)
            ERR_INSUFFICIENT_FUNDS
        )

        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))

        (map-set transactions { transaction-id: transaction-id } {
            supplier-id: supplier-id,
            buyer: tx-sender,
            amount: amount,
            status: "pending",
            created-at: stacks-block-height,
            completed-at: none,
        })

        (map-set escrow-funds { transaction-id: transaction-id } { amount: amount })

        (var-set next-transaction-id (+ transaction-id u1))
        (ok transaction-id)
    )
)

(define-public (complete-transaction (transaction-id uint))
    (let (
            (transaction-data (unwrap! (get-transaction transaction-id) ERR_SUPPLIER_NOT_FOUND))
            (supplier-data (unwrap! (get-supplier (get supplier-id transaction-data))
                ERR_SUPPLIER_NOT_FOUND
            ))
            (escrow-data (unwrap! (map-get? escrow-funds { transaction-id: transaction-id })
                ERR_SUPPLIER_NOT_FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get buyer transaction-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transaction-data) "pending")
            ERR_INVALID_PARAMETERS
        )

        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender
            (get owner supplier-data)
        )))

        (map-set transactions { transaction-id: transaction-id }
            (merge transaction-data {
                status: "completed",
                completed-at: (some stacks-block-height),
            })
        )

        (map-delete escrow-funds { transaction-id: transaction-id })
        (ok true)
    )
)

(define-public (dispute-transaction (transaction-id uint))
    (let (
            (transaction-data (unwrap! (get-transaction transaction-id) ERR_SUPPLIER_NOT_FOUND))
            (escrow-data (unwrap! (map-get? escrow-funds { transaction-id: transaction-id })
                ERR_SUPPLIER_NOT_FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get buyer transaction-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transaction-data) "pending")
            ERR_INVALID_PARAMETERS
        )

        (map-set transactions { transaction-id: transaction-id }
            (merge transaction-data { status: "disputed" })
        )

        (ok true)
    )
)

(define-public (resolve-dispute
        (transaction-id uint)
        (release-to-supplier bool)
    )
    (let (
            (transaction-data (unwrap! (get-transaction transaction-id) ERR_SUPPLIER_NOT_FOUND))
            (supplier-data (unwrap! (get-supplier (get supplier-id transaction-data))
                ERR_SUPPLIER_NOT_FOUND
            ))
            (escrow-data (unwrap! (map-get? escrow-funds { transaction-id: transaction-id })
                ERR_SUPPLIER_NOT_FOUND
            ))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transaction-data) "disputed")
            ERR_INVALID_PARAMETERS
        )

        (if release-to-supplier
            (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender
                (get owner supplier-data)
            )))
            (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender
                (get buyer transaction-data)
            )))
        )

        (map-set transactions { transaction-id: transaction-id }
            (merge transaction-data {
                status: (if release-to-supplier
                    "completed"
                    "refunded"
                ),
                completed-at: (some stacks-block-height),
            })
        )

        (map-delete escrow-funds { transaction-id: transaction-id })
        (ok true)
    )
)

(define-public (update-supplier-info
        (name (string-ascii 64))
        (category (string-ascii 32))
    )
    (let (
            (supplier-lookup (unwrap! (map-get? supplier-by-principal { owner: tx-sender })
                ERR_SUPPLIER_NOT_FOUND
            ))
            (supplier-id (get supplier-id supplier-lookup))
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
        )
        (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
        (asserts! (> (len category) u0) ERR_INVALID_PARAMETERS)
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)

        (map-set suppliers { supplier-id: supplier-id }
            (merge supplier-data {
                name: name,
                category: category,
            })
        )

        (ok true)
    )
)

(define-public (deactivate-supplier)
    (let (
            (supplier-lookup (unwrap! (map-get? supplier-by-principal { owner: tx-sender })
                ERR_SUPPLIER_NOT_FOUND
            ))
            (supplier-id (get supplier-id supplier-lookup))
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
        )
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)

        (try! (as-contract (stx-transfer? (get stake-amount supplier-data) tx-sender tx-sender)))

        (map-set suppliers { supplier-id: supplier-id }
            (merge supplier-data { is-active: false })
        )

        (ok true)
    )
)

(define-public (reactivate-supplier)
    (let (
            (supplier-lookup (unwrap! (map-get? supplier-by-principal { owner: tx-sender })
                ERR_SUPPLIER_NOT_FOUND
            ))
            (supplier-id (get supplier-id supplier-lookup))
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
            (stake (var-get min-stake))
        )
        (asserts! (not (get is-active supplier-data)) ERR_INVALID_PARAMETERS)
        (asserts! (>= (stx-get-balance tx-sender) stake) ERR_INSUFFICIENT_FUNDS)

        (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))

        (map-set suppliers { supplier-id: supplier-id }
            (merge supplier-data {
                is-active: true,
                stake-amount: stake,
            })
        )

        (ok true)
    )
)

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-fee u1000) ERR_INVALID_PARAMETERS)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

(define-public (set-min-stake (new-stake uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-stake u0) ERR_INVALID_PARAMETERS)
        (var-set min-stake new-stake)
        (ok true)
    )
)

(define-public (withdraw-fees (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_PARAMETERS)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (ok true)
    )
)

(define-read-only (get-supplier-reputation (supplier-id uint))
    (match (get-supplier supplier-id)
        supplier-data (some {
            supplier-id: supplier-id,
            name: (get name supplier-data),
            category: (get category supplier-data),
            average-rating: (get average-rating supplier-data),
            rating-count: (get rating-count supplier-data),
            is-active: (get is-active supplier-data),
        })
        none
    )
)

(define-read-only (get-suppliers-by-category (category (string-ascii 32)))
    (ok true)
)

(define-read-only (is-supplier-active (supplier-id uint))
    (match (get-supplier supplier-id)
        supplier-data (get is-active supplier-data)
        false
    )
)

(define-read-only (get-contract-balance)
    (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-escrow-amount (transaction-id uint))
    (match (map-get? escrow-funds { transaction-id: transaction-id })
        escrow-data (some (get amount escrow-data))
        none
    )
)

(define-public (emergency-pause-supplier (supplier-id uint))
    (let ((supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)

        (map-set suppliers { supplier-id: supplier-id }
            (merge supplier-data { is-active: false })
        )

        (ok true)
    )
)

(define-public (cancel-transaction (transaction-id uint))
    (let (
            (transaction-data (unwrap! (get-transaction transaction-id) ERR_SUPPLIER_NOT_FOUND))
            (escrow-data (unwrap! (map-get? escrow-funds { transaction-id: transaction-id })
                ERR_SUPPLIER_NOT_FOUND
            ))
            (time-elapsed (- stacks-block-height (get created-at transaction-data)))
        )
        (asserts! (is-eq tx-sender (get buyer transaction-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status transaction-data) "pending")
            ERR_INVALID_PARAMETERS
        )
        (asserts! (>= time-elapsed u144) ERR_UNAUTHORIZED)

        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender
            (get buyer transaction-data)
        )))

        (map-set transactions { transaction-id: transaction-id }
            (merge transaction-data {
                status: "cancelled",
                completed-at: (some stacks-block-height),
            })
        )

        (map-delete escrow-funds { transaction-id: transaction-id })
        (ok true)
    )
)

(define-public (increase-stake (additional-amount uint))
    (let (
            (supplier-lookup (unwrap! (map-get? supplier-by-principal { owner: tx-sender })
                ERR_SUPPLIER_NOT_FOUND
            ))
            (supplier-id (get supplier-id supplier-lookup))
            (supplier-data (unwrap! (get-supplier supplier-id) ERR_SUPPLIER_NOT_FOUND))
        )
        (asserts! (get is-active supplier-data) ERR_SUPPLIER_NOT_FOUND)
        (asserts! (> additional-amount u0) ERR_INVALID_PARAMETERS)
        (asserts! (>= (stx-get-balance tx-sender) additional-amount)
            ERR_INSUFFICIENT_FUNDS
        )

        (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))

        (map-set suppliers { supplier-id: supplier-id }
            (merge supplier-data { stake-amount: (+ (get stake-amount supplier-data) additional-amount) })
        )

        (ok true)
    )
)

(define-read-only (get-transaction-history (buyer principal))
    (ok true)
)

(define-read-only (get-supplier-transactions (supplier-id uint))
    (ok true)
)
