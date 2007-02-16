;; USART configuration block

(in-package :picflow)

;; FIXME: there should probably *actually* be options here.
(defclass usart-config (node)
  ())

(defmethod init-code-emit ((node usart-config))
  ;; FIXME: These settings should be customizable, especially the baud rate
  (emit "OpenUSART(USART_TX_INT_OFF  &
            USART_RX_INT_OFF  &
            USART_ASYNCH_MODE &
            USART_EIGHT_BIT   &
            USART_CONT_RX     &
            USART_BRGH_HIGH, 25);"))

(defparameter *usart-config* (make-instance 'usart-config))