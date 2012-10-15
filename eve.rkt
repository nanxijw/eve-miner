#lang racket
(require ffi/unsafe)
(require ffi/com)
(require ffi/com-registry)
(define dm-obj (com-create-instance (coclass->clsid "dm.dmsoft")))

(define-syntax-rule (dm function-name ...)
  (com-invoke dm-obj function-name ...))

(dm "SetPath" "D:/Work/eve")
(dm "SetDict" 0 "eve采矿.txt")
;; 设定屏幕宽和高
(define screen-w 1024)
(define screen-h 768)
(define-struct posn (x y))

(define-syntax-rule (mouse-move-to x y)
  (= 1 (dm "MoveTo" x y)))

(define-syntax-rule (get-mouse-pos)
  (let ([x (box 0)]
        [y (box 0)])
  (if (= 1 (dm "GetCursorPos" x y))
  (make-posn (unbox x) (unbox y))
  #f)))

(define-syntax-rule (mouse-move x y)
  (= 1 (dm "MoveR" x y)))

(define-syntax-rule (mouse-left-click)
  (= 1 (dm "LeftClick")))

(define-syntax-rule (mouse-right-click)
  (= 1 (dm "RightClick")))

(define-syntax-rule (mouse-left-down)
  (= 1 (dm "LeftDown")))

(define-syntax-rule (mouse-left-up)
  (= 1 (dm "LeftUp")))

(define-syntax-rule (mouse-right-down)
  (= 1 (dm "RightDown")))

(define-syntax-rule (mouse-right-up)
  (= 1 (dm "RightUp")))

(define-syntax-rule (mouse-move-right x)
    (mouse-move x 0))

(define-syntax-rule (mouse-move-left x)
    (mouse-move (- x) 0))

(define-syntax-rule (mouse-move-up y)
    (mouse-move 0 (- y)))

(define-syntax-rule (mouse-move-down y)
    (mouse-move 0 y))
;; 发送快捷键的组合
(define (key-combine-3 key1 key2 key3)
  (dm "KeyDownChar" key1)
    (dm "KeyDownChar" key2)
     (dm "KeyPressChar" key3)
     (dm "KeyUpChar" key2)
    (dm "KeyUpChar" key1))

(define ocr-find-x (box 0))
(define ocr-find-y (box 0))

(define (find-string content)
  (let ([found-x (box 0)]
        [found-y (box 0)])
    (if (= 0
  (dm "FindStr"
      0 0 screen-w screen-h 
      content 
      "207.4.80-207.4.10" 
      1.0
      found-x 
      found-y))
        (make-posn (unbox found-x) (unbox found-y))
        #f)))

(define (mouse-move-to-string content)
  (let ([mouse-pos (find-string content)])
    (if mouse-pos
        (mouse-move-to (+ 3 (posn-x mouse-pos)) (+ 3 (posn-y mouse-pos)))
        #f)))
;; 离开空间站函数
(define (exit-space-station)
  ;; 除非发现总览条目，否则代表出站失败，继续出站 
  (unless
   (find-string "选中的物体")
     (key-combine-3 "ctrl" "alt" "e")
    (sleep 5);; 等待5秒
    (exit-space-station)
    ))