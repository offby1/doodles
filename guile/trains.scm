;; a segment of track consists of ---

;; two ENDS

;; elevation gain from one end to the other

;; suggested speed limit

;; maybe an ID


;; an END is either a dead-end or a switch, and in any case, has a
;; geographic location

;; a switch is a reference to three segments -- known as the "base",
;; "left", and "right" segments.  Exactly one of "left" and "right" is
;; selected at all times; this can be changed.

;; segments, along their length, can cross interesting items ...
;; 
;; they can cross another segment -- thus you can't have two trains at
;; the intersection at once (or else there's an accident).  This
;; intersection occurs just at one point along the segment (as opposed
;; to lasting a significant length)

;; they can cross a road (I think this is called a "level crossing").
;; Like crossing another segment, these might just occur at one point.

;; they can be in a tunnel or on a bridge.  These have a length.

;; they can be adjacent to a station platform.  These have a length
;; too.

;;;;;;;;;;;;;;;;;;;;

;; a route is a list of segments that you can traverse from one end to
;; the other.

