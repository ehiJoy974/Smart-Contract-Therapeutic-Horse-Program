;; ===================================================================
;; THERAPEUTIC HORSE PROGRAM - MAIN CONTRACT
;; ===================================================================
;; A comprehensive system for managing equine-assisted therapy programs
;; including horse care, session scheduling, and therapeutic outcomes

;; ===================================================================
;; CONSTANTS & ERROR CODES
;; ===================================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-PARAMETERS (err u104))
(define-constant ERR-INSUFFICIENT-PERMISSIONS (err u105))
(define-constant ERR-HORSE-NOT-AVAILABLE (err u106))
(define-constant ERR-FACILITY-UNAVAILABLE (err u107))
(define-constant ERR-INVALID-DATE (err u108))
(define-constant ERR-SESSION-CONFLICT (err u109))

;; ===================================================================
;; DATA STRUCTURES
;; ===================================================================

;; Horse Management
(define-map horses
  { horse-id: uint }
  {
    name: (string-ascii 50),
    breed: (string-ascii 30),
    age: uint,
    health-status: (string-ascii 20),
    therapy-certified: bool,
    availability-status: (string-ascii 20),
    last-health-check: uint,
    care-coordinator: principal,
    welfare-score: uint,
    created-at: uint,
    updated-at: uint
  }
)

;; Participant Management
(define-map participants
  { participant-id: uint }
  {
    name: (string-ascii 50),
    age: uint,
    condition: (string-ascii 100),
    therapy-goals: (string-ascii 200),
    emergency-contact: (string-ascii 100),
    therapist: principal,
    start-date: uint,
    status: (string-ascii 20),
    progress-level: uint,
    created-at: uint
  }
)

;; Session Management
(define-map therapy-sessions
  { session-id: uint }
  {
    participant-id: uint,
    horse-id: uint,
    therapist: principal,
    volunteer: (optional principal),
    scheduled-date: uint,
    duration: uint,
    facility-area: (string-ascii 30),
    session-type: (string-ascii 30),
    status: (string-ascii 20),
    notes: (string-ascii 500),
    outcome-score: uint,
    created-at: uint
  }
)

;; Volunteer Management
(define-map volunteers
  { volunteer-id: uint }
  {
    name: (string-ascii 50),
    skills: (string-ascii 200),
    availability: (string-ascii 100),
    certifications: (string-ascii 200),
    hours-completed: uint,
    rating: uint,
    contact-info: (string-ascii 100),
    status: (string-ascii 20),
    joined-at: uint
  }
)

;; Facility Resources
(define-map facilities
  { facility-id: uint }
  {
    name: (string-ascii 50),
    area-type: (string-ascii 30),
    capacity: uint,
    equipment: (string-ascii 200),
    maintenance-status: (string-ascii 20),
    safety-certified: bool,
    last-inspection: uint,
    availability: (string-ascii 20)
  }
)

;; Progress Tracking
(define-map progress-records
  { record-id: uint }
  {
    participant-id: uint,
    session-id: uint,
    assessment-date: uint,
    therapist: principal,
    motor-skills: uint,
    emotional-state: uint,
    social-interaction: uint,
    confidence-level: uint,
    overall-progress: uint,
    notes: (string-ascii 300),
    next-goals: (string-ascii 200)
  }
)

;; Staff and Permissions
(define-map staff-permissions
  { staff-member: principal }
  {
    role: (string-ascii 30),
    permissions: (list 10 (string-ascii 20)),
    active: bool,
    hire-date: uint,
    supervisor: (optional principal)
  }
)

;; ===================================================================
;; COUNTERS
;; ===================================================================

(define-data-var next-horse-id uint u1)
(define-data-var next-participant-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-volunteer-id uint u1)
(define-data-var next-facility-id uint u1)
(define-data-var next-record-id uint u1)

;; ===================================================================
;; AUTHORIZATION FUNCTIONS
;; ===================================================================

(define-private (is-authorized (action (string-ascii 20)))
  (or
    (is-eq tx-sender CONTRACT-OWNER)
    (match (map-get? staff-permissions { staff-member: tx-sender })
      staff-data (is-eq (get active staff-data) true)
      false
    )
  )
)

(define-private (has-permission (action (string-ascii 20)))
  (match (map-get? staff-permissions { staff-member: tx-sender })
    staff-data
    (and
      (get active staff-data)
      (is-some (index-of? (get permissions staff-data) action))
    )
    false
  )
)

;; ===================================================================
;; HORSE MANAGEMENT FUNCTIONS
;; ===================================================================

(define-public (register-horse
  (name (string-ascii 50))
  (breed (string-ascii 30))
  (age uint)
  (care-coordinator principal)
)
  (let
    (
      (horse-id (var-get next-horse-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-horses") ERR-UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
    (asserts! (> age u0) ERR-INVALID-PARAMETERS)

    (map-set horses
      { horse-id: horse-id }
      {
        name: name,
        breed: breed,
        age: age,
        health-status: "healthy",
        therapy-certified: false,
        availability-status: "available",
        last-health-check: current-block,
        care-coordinator: care-coordinator,
        welfare-score: u100,
        created-at: current-block,
        updated-at: current-block
      }
    )

    (var-set next-horse-id (+ horse-id u1))
    (ok horse-id)
  )
)

(define-public (update-horse-health
  (horse-id uint)
  (health-status (string-ascii 20))
  (welfare-score uint)
)
  (let
    (
      (horse-data (unwrap! (map-get? horses { horse-id: horse-id }) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-horses") ERR-UNAUTHORIZED)
    (asserts! (<= welfare-score u100) ERR-INVALID-PARAMETERS)

    (map-set horses
      { horse-id: horse-id }
      (merge horse-data {
        health-status: health-status,
        welfare-score: welfare-score,
        last-health-check: current-block,
        updated-at: current-block
      })
    )
    (ok true)
  )
)

(define-public (certify-horse-for-therapy (horse-id uint))
  (let
    (
      (horse-data (unwrap! (map-get? horses { horse-id: horse-id }) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-horses") ERR-UNAUTHORIZED)
    (asserts! (is-eq (get health-status horse-data) "healthy") ERR-INVALID-STATUS)
    (asserts! (>= (get welfare-score horse-data) u80) ERR-INVALID-STATUS)

    (map-set horses
      { horse-id: horse-id }
      (merge horse-data {
        therapy-certified: true,
        updated-at: current-block
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; PARTICIPANT MANAGEMENT FUNCTIONS
;; ===================================================================

(define-public (register-participant
  (name (string-ascii 50))
  (age uint)
  (condition (string-ascii 100))
  (therapy-goals (string-ascii 200))
  (emergency-contact (string-ascii 100))
  (therapist principal)
)
  (let
    (
      (participant-id (var-get next-participant-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-participants") ERR-UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)

    (map-set participants
      { participant-id: participant-id }
      {
        name: name,
        age: age,
        condition: condition,
        therapy-goals: therapy-goals,
        emergency-contact: emergency-contact,
        therapist: therapist,
        start-date: current-block,
        status: "active",
        progress-level: u1,
        created-at: current-block
      }
    )

    (var-set next-participant-id (+ participant-id u1))
    (ok participant-id)
  )
)

(define-public (update-participant-progress
  (participant-id uint)
  (progress-level uint)
)
  (let
    (
      (participant-data (unwrap! (map-get? participants { participant-id: participant-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized "manage-participants") ERR-UNAUTHORIZED)
    (asserts! (<= progress-level u10) ERR-INVALID-PARAMETERS)

    (map-set participants
      { participant-id: participant-id }
      (merge participant-data {
        progress-level: progress-level
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; SESSION SCHEDULING FUNCTIONS
;; ===================================================================

(define-public (schedule-therapy-session
  (participant-id uint)
  (horse-id uint)
  (therapist principal)
  (scheduled-date uint)
  (duration uint)
  (facility-area (string-ascii 30))
  (session-type (string-ascii 30))
)
  (let
    (
      (session-id (var-get next-session-id))
      (current-block stacks-block-height)
      (participant-data (unwrap! (map-get? participants { participant-id: participant-id }) ERR-NOT-FOUND))
      (horse-data (unwrap! (map-get? horses { horse-id: horse-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized "schedule-sessions") ERR-UNAUTHORIZED)
    (asserts! (>= scheduled-date current-block) ERR-INVALID-DATE)
    (asserts! (is-eq (get status participant-data) "active") ERR-INVALID-STATUS)
    (asserts! (is-eq (get availability-status horse-data) "available") ERR-HORSE-NOT-AVAILABLE)
    (asserts! (get therapy-certified horse-data) ERR-HORSE-NOT-AVAILABLE)

    (map-set therapy-sessions
      { session-id: session-id }
      {
        participant-id: participant-id,
        horse-id: horse-id,
        therapist: therapist,
        volunteer: none,
        scheduled-date: scheduled-date,
        duration: duration,
        facility-area: facility-area,
        session-type: session-type,
        status: "scheduled",
        notes: "",
        outcome-score: u0,
        created-at: current-block
      }
    )

    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (assign-volunteer-to-session
  (session-id uint)
  (volunteer principal)
)
  (let
    (
      (session-data (unwrap! (map-get? therapy-sessions { session-id: session-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized "manage-volunteers") ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status session-data) "scheduled") ERR-INVALID-STATUS)

    (map-set therapy-sessions
      { session-id: session-id }
      (merge session-data {
        volunteer: (some volunteer)
      })
    )
    (ok true)
  )
)

(define-public (complete-session
  (session-id uint)
  (notes (string-ascii 500))
  (outcome-score uint)
)
  (let
    (
      (session-data (unwrap! (map-get? therapy-sessions { session-id: session-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized "manage-sessions") ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status session-data) "scheduled") ERR-INVALID-STATUS)
    (asserts! (<= outcome-score u10) ERR-INVALID-PARAMETERS)

    (map-set therapy-sessions
      { session-id: session-id }
      (merge session-data {
        status: "completed",
        notes: notes,
        outcome-score: outcome-score
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; VOLUNTEER MANAGEMENT FUNCTIONS
;; ===================================================================

(define-public (register-volunteer
  (name (string-ascii 50))
  (skills (string-ascii 200))
  (availability (string-ascii 100))
  (certifications (string-ascii 200))
  (contact-info (string-ascii 100))
)
  (let
    (
      (volunteer-id (var-get next-volunteer-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-volunteers") ERR-UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)

    (map-set volunteers
      { volunteer-id: volunteer-id }
      {
        name: name,
        skills: skills,
        availability: availability,
        certifications: certifications,
        hours-completed: u0,
        rating: u5,
        contact-info: contact-info,
        status: "active",
        joined-at: current-block
      }
    )

    (var-set next-volunteer-id (+ volunteer-id u1))
    (ok volunteer-id)
  )
)

(define-public (update-volunteer-hours
  (volunteer-id uint)
  (additional-hours uint)
)
  (let
    (
      (volunteer-data (unwrap! (map-get? volunteers { volunteer-id: volunteer-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-authorized "manage-volunteers") ERR-UNAUTHORIZED)

    (map-set volunteers
      { volunteer-id: volunteer-id }
      (merge volunteer-data {
        hours-completed: (+ (get hours-completed volunteer-data) additional-hours)
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; FACILITY MANAGEMENT FUNCTIONS
;; ===================================================================

(define-public (register-facility
  (name (string-ascii 50))
  (area-type (string-ascii 30))
  (capacity uint)
  (equipment (string-ascii 200))
)
  (let
    (
      (facility-id (var-get next-facility-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-facilities") ERR-UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
    (asserts! (> capacity u0) ERR-INVALID-PARAMETERS)

    (map-set facilities
      { facility-id: facility-id }
      {
        name: name,
        area-type: area-type,
        capacity: capacity,
        equipment: equipment,
        maintenance-status: "good",
        safety-certified: false,
        last-inspection: current-block,
        availability: "available"
      }
    )

    (var-set next-facility-id (+ facility-id u1))
    (ok facility-id)
  )
)

(define-public (update-facility-maintenance
  (facility-id uint)
  (maintenance-status (string-ascii 20))
)
  (let
    (
      (facility-data (unwrap! (map-get? facilities { facility-id: facility-id }) ERR-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "manage-facilities") ERR-UNAUTHORIZED)

    (map-set facilities
      { facility-id: facility-id }
      (merge facility-data {
        maintenance-status: maintenance-status,
        last-inspection: current-block
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; PROGRESS TRACKING FUNCTIONS
;; ===================================================================

(define-public (record-progress
  (participant-id uint)
  (session-id uint)
  (motor-skills uint)
  (emotional-state uint)
  (social-interaction uint)
  (confidence-level uint)
  (overall-progress uint)
  (notes (string-ascii 300))
  (next-goals (string-ascii 200))
)
  (let
    (
      (record-id (var-get next-record-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-authorized "record-progress") ERR-UNAUTHORIZED)
    (asserts! (<= motor-skills u10) ERR-INVALID-PARAMETERS)
    (asserts! (<= emotional-state u10) ERR-INVALID-PARAMETERS)
    (asserts! (<= social-interaction u10) ERR-INVALID-PARAMETERS)
    (asserts! (<= confidence-level u10) ERR-INVALID-PARAMETERS)
    (asserts! (<= overall-progress u10) ERR-INVALID-PARAMETERS)

    (map-set progress-records
      { record-id: record-id }
      {
        participant-id: participant-id,
        session-id: session-id,
        assessment-date: current-block,
        therapist: tx-sender,
        motor-skills: motor-skills,
        emotional-state: emotional-state,
        social-interaction: social-interaction,
        confidence-level: confidence-level,
        overall-progress: overall-progress,
        notes: notes,
        next-goals: next-goals
      }
    )

    (var-set next-record-id (+ record-id u1))
    (ok record-id)
  )
)

;; ===================================================================
;; STAFF MANAGEMENT FUNCTIONS
;; ===================================================================

(define-public (add-staff-member
  (staff-member principal)
  (role (string-ascii 30))
  (permissions (list 10 (string-ascii 20)))
)
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)

    (map-set staff-permissions
      { staff-member: staff-member }
      {
        role: role,
        permissions: permissions,
        active: true,
        hire-date: current-block,
        supervisor: (some tx-sender)
      }
    )
    (ok true)
  )
)

(define-public (deactivate-staff-member (staff-member principal))
  (let
    (
      (staff-data (unwrap! (map-get? staff-permissions { staff-member: staff-member }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)

    (map-set staff-permissions
      { staff-member: staff-member }
      (merge staff-data {
        active: false
      })
    )
    (ok true)
  )
)

;; ===================================================================
;; READ-ONLY FUNCTIONS
;; ===================================================================

(define-read-only (get-horse-info (horse-id uint))
  (map-get? horses { horse-id: horse-id })
)

(define-read-only (get-participant-info (participant-id uint))
  (map-get? participants { participant-id: participant-id })
)

(define-read-only (get-session-info (session-id uint))
  (map-get? therapy-sessions { session-id: session-id })
)

(define-read-only (get-volunteer-info (volunteer-id uint))
  (map-get? volunteers { volunteer-id: volunteer-id })
)

(define-read-only (get-facility-info (facility-id uint))
  (map-get? facilities { facility-id: facility-id })
)

(define-read-only (get-progress-record (record-id uint))
  (map-get? progress-records { record-id: record-id })
)

(define-read-only (get-staff-permissions (staff-member principal))
  (map-get? staff-permissions { staff-member: staff-member })
)

(define-read-only (get-current-counters)
  {
    next-horse-id: (var-get next-horse-id),
    next-participant-id: (var-get next-participant-id),
    next-session-id: (var-get next-session-id),
    next-volunteer-id: (var-get next-volunteer-id),
    next-facility-id: (var-get next-facility-id),
    next-record-id: (var-get next-record-id)
  }
)

(define-read-only (get-horse-availability (horse-id uint))
  (match (map-get? horses { horse-id: horse-id })
    horse-data
    (and
      (get therapy-certified horse-data)
      (is-eq (get availability-status horse-data) "available")
      (is-eq (get health-status horse-data) "healthy")
      (>= (get welfare-score horse-data) u80)
    )
    false
  )
)

;; FIXED: Wrapped the tuple in 'some' to match return types
(define-read-only (get-participant-progress-summary (participant-id uint))
  (match (map-get? participants { participant-id: participant-id })
    participant-data
    (some {
      participant-id: participant-id,
      name: (get name participant-data),
      progress-level: (get progress-level participant-data),
      status: (get status participant-data),
      therapist: (get therapist participant-data)
    })
    none
  )
)
