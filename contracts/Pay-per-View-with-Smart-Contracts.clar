(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-CONTENT-EXISTS (err u102))
(define-constant ERR-CONTENT-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-PURCHASED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee uint u50)
(define-data-var total-content-count uint u0)

(define-map contents
    { content-id: uint }
    {
        creator: principal,
        price: uint,
        title: (string-ascii 64),
        views: uint,
        revenue: uint,
        active: bool,
    }
)

(define-map user-purchases
    {
        user: principal,
        content-id: uint,
    }
    {
        purchased: bool,
        timestamp: uint,
    }
)

(define-map creator-stats
    { creator: principal }
    {
        total-content: uint,
        total-revenue: uint,
        total-views: uint,
    }
)

(define-read-only (get-content (content-id uint))
    (map-get? contents { content-id: content-id })
)

(define-read-only (get-purchase-status
        (user principal)
        (content-id uint)
    )
    (map-get? user-purchases {
        user: user,
        content-id: content-id,
    })
)

(define-read-only (get-creator-stats (creator principal))
    (map-get? creator-stats { creator: creator })
)

(define-public (add-content
        (title (string-ascii 64))
        (price uint)
    )
    (let (
            (content-id (var-get total-content-count))
            (creator-data (default-to {
                total-content: u0,
                total-revenue: u0,
                total-views: u0,
            }
                (map-get? creator-stats { creator: tx-sender })
            ))
        )
        (asserts! (> price u0) ERR-INVALID-AMOUNT)
        (map-set contents { content-id: content-id } {
            creator: tx-sender,
            price: price,
            title: title,
            views: u0,
            revenue: u0,
            active: true,
        })
        (map-set creator-stats { creator: tx-sender } {
            total-content: (+ (get total-content creator-data) u1),
            total-revenue: (get total-revenue creator-data),
            total-views: (get total-views creator-data),
        })
        (var-set total-content-count (+ content-id u1))
        (ok content-id)
    )
)

(define-public (purchase-content (content-id uint))
    (let (
            (content (unwrap! (map-get? contents { content-id: content-id })
                ERR-CONTENT-NOT-FOUND
            ))
            (purchase-exists (map-get? user-purchases {
                user: tx-sender,
                content-id: content-id,
            }))
            (creator-data (default-to {
                total-content: u0,
                total-revenue: u0,
                total-views: u0,
            }
                (map-get? creator-stats { creator: (get creator content) })
            ))
        )
        (asserts! (get active content) ERR-CONTENT-NOT-FOUND)
        (asserts! (is-none purchase-exists) ERR-ALREADY-PURCHASED)
        (try! (stx-transfer? (get price content) tx-sender (get creator content)))
        (map-set user-purchases {
            user: tx-sender,
            content-id: content-id,
        } {
            purchased: true,
            timestamp: stacks-block-height,
        })
        (map-set contents { content-id: content-id }
            (merge content {
                revenue: (+ (get revenue content) (get price content)),
                views: (+ (get views content) u1),
            })
        )
        (map-set creator-stats { creator: (get creator content) } {
            total-content: (get total-content creator-data),
            total-revenue: (+ (get total-revenue creator-data) (get price content)),
            total-views: (+ (get total-views creator-data) u1),
        })
        (ok true)
    )
)
(define-public (update-content-price
        (content-id uint)
        (new-price uint)
    )
    (let ((content (unwrap! (map-get? contents { content-id: content-id })
            ERR-CONTENT-NOT-FOUND
        )))
        (asserts! (is-eq tx-sender (get creator content)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-price u0) ERR-INVALID-AMOUNT)
        (map-set contents { content-id: content-id }
            (merge content { price: new-price })
        )
        (ok true)
    )
)

(define-public (deactivate-content (content-id uint))
    (let ((content (unwrap! (map-get? contents { content-id: content-id })
            ERR-CONTENT-NOT-FOUND
        )))
        (asserts! (is-eq tx-sender (get creator content)) ERR-NOT-AUTHORIZED)
        (map-set contents { content-id: content-id }
            (merge content { active: false })
        )
        (ok true)
    )
)
