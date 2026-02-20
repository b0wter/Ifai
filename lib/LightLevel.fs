namespace Ifai.Lib

type LightState =
    /// Glare, harder to make out details
    | Blinding
    /// Well-lit easy to see
    | Bright
    /// Baseline
    | Moderate
    /// Mild perception penalties
    | Low
    /// Very hard to make out details
    | VeryLow
    /// Almost blind, barely able to move
    | Minimal
    /// No visibility at all
    | PitchBlack

    
type VisibilityEffects =
    { AutoDetectSmallObjects : bool
      CanRead : bool
      PerceptionModifier : int
      MovementRisk : bool
      IdentificationPossible : bool }


module LightLevel =
    let visibilityEffects level =
        match level with
        | PitchBlack ->
            { AutoDetectSmallObjects = false
              CanRead = false
              PerceptionModifier = -10
              MovementRisk = true
              IdentificationPossible = false }

        | Minimal ->
            { AutoDetectSmallObjects = false
              CanRead = false
              PerceptionModifier = -6
              MovementRisk = true
              IdentificationPossible = false }

        | VeryLow ->
            { AutoDetectSmallObjects = false
              CanRead = false
              PerceptionModifier = -3
              MovementRisk = false
              IdentificationPossible = true }

        | Low ->
            { AutoDetectSmallObjects = true
              CanRead = false
              PerceptionModifier = -1
              MovementRisk = false
              IdentificationPossible = true }

        | Moderate ->
            { AutoDetectSmallObjects = true
              CanRead = true
              PerceptionModifier = 0
              MovementRisk = false
              IdentificationPossible = true }

        | Bright ->
            { AutoDetectSmallObjects = true
              CanRead = true
              PerceptionModifier = 0
              MovementRisk = false
              IdentificationPossible = true }

        | Blinding ->
            { AutoDetectSmallObjects = true
              CanRead = false       // cannot look at glare
              PerceptionModifier = -4
              MovementRisk = false  // optional: could be true if intense
              IdentificationPossible = false }    