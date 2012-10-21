#lang racket/gui
(require ffi/unsafe)
(require ffi/com)
(require ffi/com-registry)
(define dm-obj (com-create-instance (coclass->clsid "dm.dmsoft")))

(define-syntax-rule (dm function-name ...)
  (com-invoke dm-obj function-name ...))

(dm "SetPath" "D:/Work/eve")
(dm "SetDict" 0 "采矿.txt")
(dm "SetMouseDelay" "dx" 800)

(define-struct posn (x y))

;; 设定屏幕宽和高
(define screen-w 1024)
(define screen-h 768)
(define mouse-delay 1)

(define ocr-found-x (box 0))
(define ocr-found-y (box 0))
(define mouse-pos (make-posn 0 0))

(define-syntax-rule (mouse-dest-x)
  (unbox ocr-found-x))
(define-syntax-rule (mouse-dest-y)
  (unbox ocr-found-y))


(define pos-total-list (make-posn 0 0))
(define pos-selected-object (make-posn 0 0))
(define pos-menu (make-posn 50 40))


(define-syntax-rule (mouse-move-to x y)
  (= 1 (dm "MoveTo" x y)))

(define-syntax-rule (mouse-get-pos)
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
(define (key-combine-2 key1 key2)
  (dm "KeyDownChar" key1)
     (dm "KeyPressChar" key2)
    (dm "KeyUpChar" key1))

(define (find-string content)
    (if (zero? (dm "FindStrFast"
                   0 0 screen-w screen-h
                   content
                   "0.0.75-0.0.6"
                   1.0
                   ocr-found-x ocr-found-y))
        (make-posn (unbox ocr-found-x) (unbox ocr-found-y))
        #f))

(define (mouse-move-to-string content)
  (let ([mouse-pos (find-string content)])
    (if mouse-pos
        (mouse-move-to (+ 3 (posn-x mouse-pos)) (+ 3 (posn-y mouse-pos)))
        #f)))
;; 离开空间站函数
(define (exit-space-station)
  (key-combine-3 "ctrl" "alt" "e");; 先发送离站命令
  (sleep 5);; 等待5秒
  ;; 除非发现总览，否则代表出站失败，继续出站
  (if
   (or (find-string "选中的物体") (find-string "总览"))
   #t
   (exit-space-station)
   ))
;; 初始化
(define (do-init)
  (define hwnd-eve (dm "FindWindow" "triuiScreen" "EVE - IGameless"))
  (dm "BindWindow" hwnd-eve "dx2" "windows" "dx" 0)
  (set! pos-total-list (find-string "总览"))
  (set! pos-selected-object (find-string "选中的物体"))
  (if (and pos-selected-object pos-total-list)
      #t
      #f)
  )

;; 退出时运行
(define (do-quit)
  (dm "UnBindWindow"))

(define (menu-select-item content)
  (mouse-move-to (posn-x pos-menu) (posn-y pos-menu))
  (sleep mouse-delay)
  (mouse-left-click)
  (sleep 3)
  (if(zero? (dm "FindStrFast"
             (posn-x pos-menu) (posn-y pos-menu) (+ (posn-x pos-menu) 175) (+ (posn-y pos-menu) 175)
             content
             "0.0.75-0.0.6"
             1.0
             ocr-found-x ocr-found-y))
     (mouse-move-to (+ (mouse-dest-x) 7) (+ (mouse-dest-y) 5))
     (menu-select-item content)))



(define (menu-deep-in)
  ;; 进入下一级菜单
  (set! mouse-pos (mouse-get-pos))
  (if (zero? (dm "FindPic"
                 (posn-x mouse-pos) (- (posn-y mouse-pos) 10) 1024 (+ (posn-y mouse-pos) 15)
                 "pic/right.bmp"
                 "000000"
                 1.0 0
                 ocr-found-x ocr-found-y))
      (mouse-move-to (+  (mouse-dest-x) 100) (+ (mouse-dest-y) 3))
      #f))

(define-syntax-rule (menu-move-index n)
  (mouse-move 0 (* n 15)))

(define (goto-space-station n)
  ;; 到第n个空间站
  (key-combine-3 "ctrl" "alt" "s") ;; 先停止舰船
  (sleep 1)
  (menu-select-item "空间站")
  (sleep mouse-delay)
  (menu-deep-in)
  (sleep mouse-delay)
  (menu-move-index (- n 1))
  (sleep mouse-delay)
  (menu-deep-in)
  (sleep mouse-delay)
  (zero? (dm "FindStrFast"
             (mouse-dest-x) (mouse-dest-y) (+ (mouse-dest-x) 175) (+ (mouse-dest-y) 175)
             "停靠"
             "0.0.75-0.0.6"
             1.0
             ocr-found-x ocr-found-y))
  (mouse-move-to (+ (mouse-dest-x) 3) ( + (mouse-dest-y) 3))
  (sleep mouse-delay)
  (mouse-left-click)
  (sleep 5)
  (define (wait-for-enter-station)
    (sleep 3)
    (if (not (find-string "空间站服务"))
        (wait-for-enter-station)
        #t))
  (wait-for-enter-station)
  )

(define (goto-asteroid-belt n)
  ;; 到第n个小行星带
  (key-combine-3 "ctrl" "alt" "s") ;; 先停止舰船
  (sleep 1)
  (menu-select-item "小行星带")
  (sleep mouse-delay)
  (menu-deep-in)
  (sleep mouse-delay)
  (menu-move-index (- n 1))
  (sleep mouse-delay)
  (menu-deep-in)
  (sleep mouse-delay)
  (mouse-left-click)
  (sleep 5) ;; 等待越前引擎启动
  (define (wait-for-jumping)
    (sleep 3)
    (if (in-jumping?)
        (wait-for-jumping)
        #t))
  (wait-for-jumping)
  )

(define (mineral-select-index n)
  ;; 锁定第n个矿物
  (define move-step 19)
  (define base-x (+ (posn-x pos-total-list) 80))
  (define base-y (- (+ (posn-y pos-total-list) 66) move-step))
  (mouse-move-to base-x (+ base-y (* n move-step)))
  (sleep mouse-delay)
  (mouse-left-click)
  (key-combine-3 "ctrl" "alt" "l")
  )

(define (store-unload)
  ;; 卸载货物
  ;; 查找物品机柜位置
  (cond [(zero? (dm "FindStrFast"
                 30 40 328 612
                 "物品机库"
                 "0.0.59-0.0.0"
                 1.0
                 ocr-found-x ocr-found-y))
         (mouse-move-to 500 200)
         (mouse-left-click)
         (sleep mouse-delay)
         (key-combine-2 "ctrl" "a")
         (let ([store-dest-x (+ 10 (mouse-dest-x))]
               [store-dest-y (+ 5 (mouse-dest-y))])
           (if (find-string "货物名字")
                (mouse-move-to (+ 20 (mouse-dest-x))
                               (+ 20 (mouse-dest-y)))
                (mouse-move-to 475 90))
           (sleep mouse-delay)
           (mouse-left-down)
           (sleep mouse-delay)
           (mouse-move-to store-dest-x store-dest-y)
           (sleep mouse-delay)
           (mouse-left-up))]
        [else (key-combine-2 "alt" "c");; 不存在则打开货柜
              (sleep 2) ;; 等待0.5秒，等待后继续判断货柜位置
              (store-unload)]))

(define (in-jumping?)
  ;; 判断是否正在跃迁
  (zero? (dm "FindStrFast"
             460 700 560 750
             "正在跃迁"
             "0.0.0-0.0.0"
             1.0
             (box 0) (box 0))))