;; NanoQuest - Micro-Gaming Ecosystem Smart Contract
;; A simple implementation for tournament management and rewards

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-tournament-full (err u103))
(define-constant err-tournament-ended (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-invalid-game-type (err u106))

;; Data Variables
(define-data-var next-tournament-id uint u1)
(define-data-var tournament-fee uint u1000) ;; 1000 microSTX
(define-data-var contract-balance uint u0)

;; Game Types
(define-constant PIXEL-PERFECT u1)
(define-constant REFLEX-ARENA u2)
(define-constant LOGIC-SPRINT u3)

;; Data Maps
(define-map tournaments 
  uint 
  {
    game-type: uint,
    creator: principal,
    max-players: uint,
    current-players: uint,
    entry-fee: uint,
    prize-pool: uint,
    start-block: uint,
    end-block: uint,
    winner: (optional principal),
    status: (string-ascii 20) ;; "open", "active", "finished"
  }
)

(define-map tournament-participants 
  {tournament-id: uint, player: principal}
  {
    score: uint,
    timestamp: uint,
    rank: (optional uint)
  }
)

(define-map player-stats
  principal
  {
    tournaments-played: uint,
    tournaments-won: uint,
    total-earnings: uint,
    current-streak: uint,
    nano-tokens: uint
  }
)

(define-map player-tournament-history
  {player: principal, tournament-id: uint}
  {
    final-score: uint,
    final-rank: uint,
    earnings: uint
  }
)

;; Read-only functions
(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments tournament-id)
)

(define-read-only (get-player-stats (player principal))
  (default-to 
    {tournaments-played: u0, tournaments-won: u0, total-earnings: u0, current-streak: u0, nano-tokens: u0}
    (map-get? player-stats player)
  )
)

(define-read-only (get-tournament-participant (tournament-id uint) (player principal))
  (map-get? tournament-participants {tournament-id: tournament-id, player: player})
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-tournament-fee)
  (var-get tournament-fee)
)

(define-read-only (is-valid-game-type (game-type uint))
  (or (is-eq game-type PIXEL-PERFECT)
      (or (is-eq game-type REFLEX-ARENA)
          (is-eq game-type LOGIC-SPRINT)))
)

;; Private functions
(define-private (update-player-stats (player principal) (won bool) (earnings uint))
  (let ((current-stats (get-player-stats player)))
    (map-set player-stats player
      {
        tournaments-played: (+ (get tournaments-played current-stats) u1),
        tournaments-won: (+ (get tournaments-won current-stats) (if won u1 u0)),
        total-earnings: (+ (get total-earnings current-stats) earnings),
        current-streak: (if won (+ (get current-streak current-stats) u1) u0),
        nano-tokens: (+ (get nano-tokens current-stats) earnings)
      }
    )
  )
)

;; Public functions
(define-public (create-tournament (game-type uint) (max-players uint) (duration-blocks uint))
  (let ((tournament-id (var-get next-tournament-id))
        (entry-fee (var-get tournament-fee))
        (current-block block-height))
    (asserts! (is-valid-game-type game-type) err-invalid-game-type)
    (asserts! (> max-players u0) (err u107))
    (asserts! (> duration-blocks u0) (err u108))
    
    (map-set tournaments tournament-id
      {
        game-type: game-type,
        creator: tx-sender,
        max-players: max-players,
        current-players: u0,
        entry-fee: entry-fee,
        prize-pool: u0,
        start-block: current-block,
        end-block: (+ current-block duration-blocks),
        winner: none,
        status: "open"
      }
    )
    (var-set next-tournament-id (+ tournament-id u1))
    (ok tournament-id)
  )
)

(define-public (join-tournament (tournament-id uint))
  (let ((tournament (unwrap! (get-tournament tournament-id) err-not-found))
        (entry-fee (get entry-fee tournament))
        (current-players (get current-players tournament)))
    
    (asserts! (is-eq (get status tournament) "open") err-tournament-ended)
    (asserts! (< current-players (get max-players tournament)) err-tournament-full)
    (asserts! (is-none (get-tournament-participant tournament-id tx-sender)) err-already-exists)
    
    ;; Transfer entry fee (simplified - in real implementation would use STX transfer)
    (map-set tournament-participants 
      {tournament-id: tournament-id, player: tx-sender}
      {score: u0, timestamp: block-height, rank: none}
    )
    
    ;; Update tournament
    (map-set tournaments tournament-id
      (merge tournament {
        current-players: (+ current-players u1),
        prize-pool: (+ (get prize-pool tournament) entry-fee)
      })
    )
    
    (var-set contract-balance (+ (var-get contract-balance) entry-fee))
    (ok true)
  )
)

(define-public (submit-score (tournament-id uint) (score uint))
  (let ((tournament (unwrap! (get-tournament tournament-id) err-not-found))
        (participant (unwrap! (get-tournament-participant tournament-id tx-sender) err-not-found)))
    
    (asserts! (< block-height (get end-block tournament)) err-tournament-ended)
    
    ;; Update participant score
    (map-set tournament-participants 
      {tournament-id: tournament-id, player: tx-sender}
      (merge participant {score: score, timestamp: block-height})
    )
    
    (ok true)
  )
)

(define-public (end-tournament (tournament-id uint) (winner principal))
  (let ((tournament (unwrap! (get-tournament tournament-id) err-not-found))
        (prize-pool (get prize-pool tournament)))
    
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-eq tx-sender (get creator tournament))) err-owner-only)
    (asserts! (>= block-height (get end-block tournament)) (err u109))
    (asserts! (is-eq (get status tournament) "open") err-tournament-ended)
    
    ;; Calculate rewards (simplified)
    (let ((winner-reward (/ (* prize-pool u70) u100)) ;; 70% to winner
          (second-reward (/ (* prize-pool u20) u100))  ;; 20% to second
          (third-reward (/ (* prize-pool u10) u100)))  ;; 10% to third
      
      ;; Update tournament status
      (map-set tournaments tournament-id
        (merge tournament {
          winner: (some winner),
          status: "finished"
        })
      )
      
      ;; Update winner stats
      (update-player-stats winner true winner-reward)
      
      ;; Record tournament history
      (map-set player-tournament-history
        {player: winner, tournament-id: tournament-id}
        {final-score: u0, final-rank: u1, earnings: winner-reward}
      )
      
      (var-set contract-balance (- (var-get contract-balance) winner-reward))
      (ok winner-reward)
    )
  )
)

(define-public (withdraw-nano-tokens (amount uint))
  (let ((player-stats-data (get-player-stats tx-sender))
        (current-tokens (get nano-tokens player-stats-data)))
    
    (asserts! (>= current-tokens amount) err-insufficient-balance)
    
    ;; Update player tokens
    (map-set player-stats tx-sender
      (merge player-stats-data {
        nano-tokens: (- current-tokens amount)
      })
    )
    
    ;; In real implementation, would transfer tokens to player
    (ok amount)
  )
)

;; Admin functions
(define-public (set-tournament-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set tournament-fee new-fee)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get contract-balance)) err-insufficient-balance)
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok amount)
  )
)

;; Initialize contract
(begin
  (var-set contract-balance u0)
  (print "NanoQuest contract deployed successfully")
)