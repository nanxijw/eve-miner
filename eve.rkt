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
(define MOUSEDELAY 1)
(define KEYDELAY 0.1)

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
(define pos-weapon-left (make-posn 450 675))
(define pos-weapon-right (make-posn 560 675))

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
  (sleep KEYDELAY)
  (dm "KeyDownChar" key2)
  (sleep KEYDELAY)
  (dm "KeyPressChar" key3)
  (sleep KEYDELAY)
  (dm "KeyUpChar" key2)
  (sleep KEYDELAY)
  (dm "KeyUpChar" key1))

(define (key-combine-2 key1 key2)
  (dm "KeyDownChar" key1)
  (sleep KEYDELAY)
  (dm "KeyPressChar" key2)
  (sleep KEYDELAY)
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

(define (exit-space-station)
   ;; 离开空间站函数
  (key-combine-3 "ctrl" "alt" "e");; 先发送离站命令
  (sleep 5);; 等待5秒
  ;; 除非发现总览，否则代表出站失败，继续出站
  (if
   (or (find-string "选中的物体") (find-string "总览"))
   #t
   (exit-space-station)
   ))

(define (do-init)
  (define hwnd-eve (dm "FindWindow" "triuiScreen" "EVE - IGameless"))
  (dm "BindWindow" hwnd-eve "dx2" "windows" "dx" 0)
  (set! pos-total-list (find-string "总览"))
  (set! pos-selected-object (find-string "选中的物体"))
  (when (zero? (dm "FindStrFast"
      420 640 602 700
      "<<"
      "0.0.91-0.0.40"
      1.0
      ocr-found-x ocr-found-y))
  (set! pos-weapon-right (make-posn (mouse-dest-x) (mouse-dest-y))))
  (when (zero? (dm "FindStrFast"
      420 640 602 700
      ">>"
      "0.0.91-0.0.40"
      1.0
      ocr-found-x ocr-found-y))
  (set! pos-weapon-left (make-posn (mouse-dest-x) (mouse-dest-y))))
  (if (and pos-selected-object pos-total-list)
      #t
      #f))

(define (do-quit)
  (dm "UnBindWindow"))

(define (menu-select-item content)
  (mouse-move-to (posn-x pos-menu) (posn-y pos-menu))
  (sleep MOUSEDELAY)
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
  (sleep MOUSEDELAY)
  (menu-deep-in)
  (sleep MOUSEDELAY)
  (menu-move-index (- n 1))
  (sleep MOUSEDELAY)
  (menu-deep-in)
  (sleep MOUSEDELAY)
  (zero? (dm "FindStrFast"
             (mouse-dest-x) (mouse-dest-y) (+ (mouse-dest-x) 175) (+ (mouse-dest-y) 175)
             "停靠"
             "0.0.75-0.0.6"
             1.0
             ocr-found-x ocr-found-y))
  (mouse-move-to (+ (mouse-dest-x) 3) ( + (mouse-dest-y) 3))
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (sleep 5)
  (define (wait-for-enter-station)
    (sleep 3)
    (if (not (find-string "空间站服务"))
        (wait-for-enter-station)
        #t))
  (wait-for-enter-station))

(define (goto-asteroid-belt n)
  ;; 到第n个小行星带
  (key-combine-3 "ctrl" "alt" "s") ;; 先停止舰船
  (sleep 1)
  (menu-select-item "小行星带")
  (sleep MOUSEDELAY)
  (menu-deep-in)
  (sleep MOUSEDELAY)
  (menu-move-index (- n 1))
  (sleep MOUSEDELAY)
  (menu-deep-in)
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (sleep 5) ;; 等待越前引擎启动
  (define (wait-for-jumping)
    (sleep 3)
    (if (in-jumping?)
        (wait-for-jumping)
        #t))
  (wait-for-jumping))

(define (mineral-select-index n)
  ;; 锁定第n个矿物
  (define move-step 19)
  (define base-x (+ (posn-x pos-total-list) 80))
  (define base-y (- (+ (posn-y pos-total-list) 66) move-step))
  (mouse-move-to base-x (+ base-y (* n move-step)))
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (sleep 1)
  (key-combine-3 "ctrl" "alt" "l"))

(define (store-unload)
  ;; 卸载货物
  ;; 查找物品机柜位置
  (cond [(zero? (dm "FindStrFast"
                 30 40 328 612
                 "物品机库"
                 "0.0.59-0.0.25"
                 1.0
                 ocr-found-x ocr-found-y))
         (mouse-move-to 500 200)
         (mouse-left-click)
         (sleep MOUSEDELAY)
         (mouse-right-click)
         (sleep MOUSEDELAY)
         (mouse-move 10 5)
         (sleep MOUSEDELAY)
         (mouse-left-click)
         (let ([store-dest-x (+ 10 (mouse-dest-x))]
               [store-dest-y (+ 5 (mouse-dest-y))])
           (if (find-string "货物名字")
                (mouse-move-to (+ 20 (mouse-dest-x))
                               (+ 20 (mouse-dest-y)))
                (mouse-move-to 475 90))
           (sleep MOUSEDELAY)
           (mouse-left-down)
           (sleep MOUSEDELAY)
           (mouse-move-to store-dest-x store-dest-y)
           (sleep MOUSEDELAY)
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

(define (weapon-in-use?)
  (zero? (dm "FindStr" 662 648 664 650
             "白色点"
             "c5c5c5-333333"
             1.0
             (box 0) (box 0)
             )))

(define (wait-for-weapon n)
  (define (wait-for-weapon-ex j k)
    (cond [(<= j 0) #t]
          [else
           (sleep 1)
           (if (weapon-in-use?)
               (wait-for-weapon-ex k k)
             (wait-for-weapon-ex (- j 1) k))]))
  (wait-for-weapon-ex n n))

(define (do-when-store-is-full)
  ;; 货柜满的处理
  (goto-space-station 1)
  (sleep 3)
  (store-unload))

(define (in-cannon-shot?)
  ;; 是否进入射程
 (and (string=? "" (dm "FindStrFastEx"
      (+ 30 (posn-x pos-selected-object)) (+ 20 (posn-y pos-selected-object)) 
      (+ 124 (posn-x pos-selected-object)) (+ 51 (posn-y pos-selected-object))
      "km"
      "0.0.67-0.0.50"
      1.0))
      (string=? "" (dm "FindPicEx"
                       (+ 30 (posn-x pos-selected-object)) (+ 20 (posn-y pos-selected-object)) 
                       (+ 124 (posn-x pos-selected-object)) (+ 51 (posn-y pos-selected-object))
                       "pic/dot.bmp"
                       "000000"
                       1.0 0))))

(define (wait-for-approach n)
  ;; 接近目标等待，进入射程后返回
  (define (wait-for-approach-ex j k)
    (cond [(<= j 0) #t]
          [else
           (sleep 0.3)
           (if (not (in-cannon-shot?))
               (wait-for-approach-ex k k)
             (wait-for-approach-ex (- j 1) k))]))
  (wait-for-approach-ex n n))

(define (locked?)
  ;;判断是否锁定目标
  (not (string=? "" (dm "FindStrFastEx"
      (- (posn-x pos-total-list) 30) (+ 20 (posn-y pos-total-list)) screen-w screen-h
      "锁定矿"
      "200.1.92-200.1.10"
      1.0))))

(define (store-is-full?)
  (cond [(zero? (dm "FindStrFast"
                 0 0 1024 768
                 "过滤器"
                 "0.0.37-0.0.0"
                 1.0
                 ocr-found-x ocr-found-y))
      (not (string=? "" (dm "FindStrFastEx"
          (- (mouse-dest-x) 20) (+ (mouse-dest-y) 2) (mouse-dest-x) (+ (mouse-dest-y) 10)
          "仓库满"
          "194.100.40-0.0.0"
          1.0)))]
      [else (key-combine-2 "alt" "c") (sleep 2) (store-is-full?)]))

(define (create-main-window)
  ;; 构造窗体
  (define frame (new frame% [label "取个什么名字呢"]
                     [width 400]))
  (define pane1 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane2 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane3 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane4 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane5 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane6 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane7 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane8 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane9 (new horizontal-pane% [parent frame]
                    [min-width 400]))
  (define pane10 (new horizontal-pane% [parent frame]
                    [min-width 400]
                    [alignment '(center top)]))
                    
  (define label-char-name (new text-field%
                               [label "角色名:"]
                               [init-value "请输入用户名"]
                               [parent pane1]))
  (define button-do-bind (new button%
                              [label "查找并绑定窗口"]
                              [parent pane1]
                              [callback (lambda (button-do-bind event)
                                          (send button-do-bind enable #f);;绑定比较耗时，先将按钮锁定，防止重复绑定
                                          (cond [(zero? hwnd) ((lambda ()
                                                                (cond
                                                                 [(find-and-bind-char (send label-char-name get-value))
                                                                  (send label-char-name enable #f)
                                                                  (send button-do-bind set-label "解除窗口绑定")]
                                                                 [else (send label-char-name set-value "绑定失败，请检查角色名是否正确")])))]
                                                [else (unbind-char)
                                                      (send label-char-name enable #t)
                                                      (send button-do-bind set-label "查找并绑定窗口")])
                                          (send button-do-bind enable #t))]))
  
    (define button-start-stop (new button% [label "开始"]
                                 [parent pane10]))
  
  (send frame show #t)
  frame)

(define (find-and-bind-char char-name)
  (set! hwnd (dm "FindWindow" "triuiScreen" (string-append "EVE - " char-name)))
  (if (zero? hwnd)
      #f
      (= 1 (dm "BindWindow" hwnd "dx2" "windows" "dx" 0))))

(define (unbind-char)
  (dm "UnBindWindow")
  (set! hwnd 0))
