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
        (unwrap-panic (record-content-activity content-id "purchase"))
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

(define-constant ERR-SUBSCRIPTION-EXPIRED (err u106))
(define-constant ERR-INVALID-DURATION (err u107))

(define-map creator-subscriptions
    { creator: principal }
    {
        monthly-price: uint,
        active: bool,
        subscriber-count: uint,
    }
)

(define-map user-subscriptions
    {
        user: principal,
        creator: principal,
    }
    {
        start-block: uint,
        end-block: uint,
        price-paid: uint,
    }
)

(define-read-only (get-subscription-plan (creator principal))
    (map-get? creator-subscriptions { creator: creator })
)

(define-read-only (get-user-subscription
        (user principal)
        (creator principal)
    )
    (map-get? user-subscriptions {
        user: user,
        creator: creator,
    })
)

(define-read-only (is-subscription-active
        (user principal)
        (creator principal)
    )
    (match (map-get? user-subscriptions {
        user: user,
        creator: creator,
    })
        subscription (> (get end-block subscription) stacks-block-height)
        false
    )
)

(define-public (create-subscription-plan (monthly-price uint))
    (begin
        (asserts! (> monthly-price u0) ERR-INVALID-AMOUNT)
        (map-set creator-subscriptions { creator: tx-sender } {
            monthly-price: monthly-price,
            active: true,
            subscriber-count: u0,
        })
        (ok true)
    )
)

(define-public (subscribe-to-creator
        (creator principal)
        (duration-blocks uint)
    )
    (let (
            (subscription-plan (unwrap! (map-get? creator-subscriptions { creator: creator })
                ERR-CONTENT-NOT-FOUND
            ))
            (existing-subscription (map-get? user-subscriptions {
                user: tx-sender,
                creator: creator,
            }))
            (start-block (match existing-subscription
                sub (if (> (get end-block sub) stacks-block-height)
                    (get end-block sub)
                    stacks-block-height
                )
                stacks-block-height
            ))
        )
        (asserts! (get active subscription-plan) ERR-CONTENT-NOT-FOUND)
        (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
        (try! (stx-transfer? (get monthly-price subscription-plan) tx-sender creator))
        (map-set user-subscriptions {
            user: tx-sender,
            creator: creator,
        } {
            start-block: start-block,
            end-block: (+ start-block duration-blocks),
            price-paid: (get monthly-price subscription-plan),
        })
        (map-set creator-subscriptions { creator: creator }
            (merge subscription-plan { subscriber-count: (+ (get subscriber-count subscription-plan) u1) })
        )
        (ok true)
    )
)

(define-public (access-content-with-subscription (content-id uint))
    (let (
            (content (unwrap! (map-get? contents { content-id: content-id })
                ERR-CONTENT-NOT-FOUND
            ))
            (creator-data (default-to {
                total-content: u0,
                total-revenue: u0,
                total-views: u0,
            }
                (map-get? creator-stats { creator: (get creator content) })
            ))
        )
        (asserts! (get active content) ERR-CONTENT-NOT-FOUND)
        (asserts! (is-subscription-active tx-sender (get creator content))
            ERR-SUBSCRIPTION-EXPIRED
        )
        (map-set contents { content-id: content-id }
            (merge content { views: (+ (get views content) u1) })
        )
        (map-set creator-stats { creator: (get creator content) } {
            total-content: (get total-content creator-data),
            total-revenue: (get total-revenue creator-data),
            total-views: (+ (get total-views creator-data) u1),
        })
        (unwrap-panic (record-content-activity content-id "view"))
        (ok true)
    )
)

(define-constant ERR-INVALID-RATING (err u108))
(define-constant ERR-NOT-PURCHASED (err u109))
(define-constant ERR-ALREADY-REVIEWED (err u110))

(define-map content-ratings
    { content-id: uint }
    {
        total-rating: uint,
        review-count: uint,
        average-rating: uint,
    }
)

(define-map user-reviews
    {
        user: principal,
        content-id: uint,
    }
    {
        rating: uint,
        review-text: (string-ascii 256),
        timestamp: uint,
    }
)

(define-map creator-reputation
    { creator: principal }
    {
        total-rating: uint,
        total-reviews: uint,
        average-rating: uint,
    }
)

(define-read-only (get-content-rating (content-id uint))
    (map-get? content-ratings { content-id: content-id })
)

(define-read-only (get-user-review
        (user principal)
        (content-id uint)
    )
    (map-get? user-reviews {
        user: user,
        content-id: content-id,
    })
)

(define-read-only (get-creator-reputation (creator principal))
    (map-get? creator-reputation { creator: creator })
)

(define-public (rate-content
        (content-id uint)
        (rating uint)
        (review-text (string-ascii 256))
    )
    (let (
            (content (unwrap! (map-get? contents { content-id: content-id })
                ERR-CONTENT-NOT-FOUND
            ))
            (purchase-status (unwrap!
                (map-get? user-purchases {
                    user: tx-sender,
                    content-id: content-id,
                })
                ERR-NOT-PURCHASED
            ))
            (existing-review (map-get? user-reviews {
                user: tx-sender,
                content-id: content-id,
            }))
            (current-rating (default-to {
                total-rating: u0,
                review-count: u0,
                average-rating: u0,
            }
                (map-get? content-ratings { content-id: content-id })
            ))
            (creator-rep (default-to {
                total-rating: u0,
                total-reviews: u0,
                average-rating: u0,
            }
                (map-get? creator-reputation { creator: (get creator content) })
            ))
        )
        (asserts! (get purchased purchase-status) ERR-NOT-PURCHASED)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
        (let (
                (new-review-count (+ (get review-count current-rating) u1))
                (new-total-rating (+ (get total-rating current-rating) rating))
                (new-average (/ new-total-rating new-review-count))
                (new-creator-reviews (+ (get total-reviews creator-rep) u1))
                (new-creator-total (+ (get total-rating creator-rep) rating))
                (new-creator-average (/ new-creator-total new-creator-reviews))
            )
            (map-set user-reviews {
                user: tx-sender,
                content-id: content-id,
            } {
                rating: rating,
                review-text: review-text,
                timestamp: stacks-block-height,
            })
            (map-set content-ratings { content-id: content-id } {
                total-rating: new-total-rating,
                review-count: new-review-count,
                average-rating: new-average,
            })
            (map-set creator-reputation { creator: (get creator content) } {
                total-rating: new-creator-total,
                total-reviews: new-creator-reviews,
                average-rating: new-creator-average,
            })
            (ok true)
        )
    )
)

(define-read-only (get-top-rated-content-by-creator (creator principal))
    (get-creator-reputation creator)
)

(define-constant ERR-INVALID-TIMEFRAME (err u111))

(define-map trending-metrics
    { content-id: uint }
    {
        views-last-100-blocks: uint,
        purchases-last-100-blocks: uint,
        last-activity-block: uint,
        trending-score: uint,
    }
)

(define-map global-trending
    { rank: uint }
    {
        content-id: uint,
        trending-score: uint,
        last-updated: uint,
    }
)

(define-data-var max-trending-rank uint u20)

(define-read-only (get-trending-metrics (content-id uint))
    (map-get? trending-metrics { content-id: content-id })
)

(define-read-only (get-trending-content (rank uint))
    (map-get? global-trending { rank: rank })
)

(define-read-only (get-trending-list
        (start-rank uint)
        (end-rank uint)
    )
    (let (
            (max-rank (var-get max-trending-rank))
            (valid-start (if (<= start-rank max-rank)
                start-rank
                u1
            ))
            (valid-end (if (<= end-rank max-rank)
                end-rank
                max-rank
            ))
        )
        (asserts! (<= valid-start valid-end) ERR-INVALID-TIMEFRAME)
        (ok {
            start: valid-start,
            end: valid-end,
            max-rank: max-rank,
        })
    )
)

(define-private (calculate-trending-score
        (views-recent uint)
        (purchases-recent uint)
        (blocks-since-activity uint)
    )
    (let (
            (view-weight u3)
            (purchase-weight u10)
            (decay-factor (if (> blocks-since-activity u100)
                (/ u100 blocks-since-activity)
                u100
            ))
            (raw-score (+ (* views-recent view-weight) (* purchases-recent purchase-weight)))
        )
        (/ (* raw-score decay-factor) u100)
    )
)

(define-private (update-trending-score (content-id uint))
    (let (
            (current-block stacks-block-height)
            (current-metrics (default-to {
                views-last-100-blocks: u0,
                purchases-last-100-blocks: u0,
                last-activity-block: u0,
                trending-score: u0,
            }
                (map-get? trending-metrics { content-id: content-id })
            ))
            (blocks-since-activity (if (> current-block (get last-activity-block current-metrics))
                (- current-block (get last-activity-block current-metrics))
                u0
            ))
            (new-score (calculate-trending-score (get views-last-100-blocks current-metrics)
                (get purchases-last-100-blocks current-metrics)
                blocks-since-activity
            ))
        )
        (map-set trending-metrics { content-id: content-id }
            (merge current-metrics { trending-score: new-score })
        )
        new-score
    )
)

(define-private (record-content-activity
        (content-id uint)
        (activity-type (string-ascii 10))
    )
    (let (
            (current-block stacks-block-height)
            (current-metrics (default-to {
                views-last-100-blocks: u0,
                purchases-last-100-blocks: u0,
                last-activity-block: u0,
                trending-score: u0,
            }
                (map-get? trending-metrics { content-id: content-id })
            ))
            (blocks-passed (if (> current-block (get last-activity-block current-metrics))
                (- current-block (get last-activity-block current-metrics))
                u0
            ))
            (decay-views (if (> blocks-passed u100)
                u0
                (get views-last-100-blocks current-metrics)
            ))
            (decay-purchases (if (> blocks-passed u100)
                u0
                (get purchases-last-100-blocks current-metrics)
            ))
            (new-views (if (is-eq activity-type "view")
                (+ decay-views u1)
                decay-views
            ))
            (new-purchases (if (is-eq activity-type "purchase")
                (+ decay-purchases u1)
                decay-purchases
            ))
        )
        (map-set trending-metrics { content-id: content-id } {
            views-last-100-blocks: new-views,
            purchases-last-100-blocks: new-purchases,
            last-activity-block: current-block,
            trending-score: (calculate-trending-score new-views new-purchases u0),
        })
        (ok true)
    )
)

(define-public (update-trending-content (content-id uint))
    (let (
            (current-metrics (default-to {
                views-last-100-blocks: u0,
                purchases-last-100-blocks: u0,
                last-activity-block: u0,
                trending-score: u0,
            }
                (map-get? trending-metrics { content-id: content-id })
            ))
            (current-block stacks-block-height)
            (blocks-since-activity (if (> current-block (get last-activity-block current-metrics))
                (- current-block (get last-activity-block current-metrics))
                u0
            ))
            (new-score (calculate-trending-score (get views-last-100-blocks current-metrics)
                (get purchases-last-100-blocks current-metrics)
                blocks-since-activity
            ))
        )
        (map-set trending-metrics { content-id: content-id }
            (merge current-metrics { trending-score: new-score })
        )
        (ok new-score)
    )
)

(define-public (set-content-trending-rank
        (content-id uint)
        (rank uint)
    )
    (let (
            (metrics (unwrap! (map-get? trending-metrics { content-id: content-id })
                ERR-CONTENT-NOT-FOUND
            ))
            (max-rank (var-get max-trending-rank))
        )
        (asserts! (<= rank max-rank) ERR-INVALID-TIMEFRAME)
        (map-set global-trending { rank: rank } {
            content-id: content-id,
            trending-score: (get trending-score metrics),
            last-updated: stacks-block-height,
        })
        (ok true)
    )
)
