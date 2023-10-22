(ns strucon.protocols)

(defprotocol Cancellable
  (cancel [_]))

(defprotocol Droppable
  (drop! [_]))
