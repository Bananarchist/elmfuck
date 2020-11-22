module Queue exposing (Queue, dequeue, dequeued, enqueue, enqueued)


type alias Queue a =
    List a


enqueue : Queue a -> a -> Queue a
enqueue q element =
    List.append q [ element ]


dequeue : Queue a -> Maybe a
dequeue q =
    case q of
        x :: _ ->
            Just x

        [] ->
            Nothing


dequeued : Queue a -> Queue a
dequeued q =
    case q of
        _ :: xs ->
            xs

        [] ->
            []


enqueued : Queue a -> Bool
enqueued q =
    case q of
        x :: _ ->
            True

        [] ->
            False
