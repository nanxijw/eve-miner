#lang racket/gui
(require ffi/unsafe)
(require ffi/com)
(require ffi/com-registry)

(define dm-obj (com-create-instance (coclass->clsid "dm.dmsoft")))
(define hwnd 0)
(define t 0)

(define-syntax-rule (dm function-name ...)
  (com-invoke dm-obj function-name ...))

(define (do-when-load)
  ;;(dm "SetPath" "D:\\Work\\eve")
  (dm "SetPath" "data")
  (dm "SetDict" 0 "采矿.txt")
  (dm "SetDict" 1 "eve_num.txt")
  (dm "LoadPic" "pic/*.bmp")
  (void))
(do-when-load)

(define-struct posn (x y))
(define-struct weapon (x y hotkey))

;; 设定屏幕宽和高
(define screen-w 1024)
(define screen-h 768)

;; 设置键鼠延迟
(define MOUSEDELAY 1)
(define KEYDELAY 0.1)

(define ocr-found-x (box 0))
(define ocr-found-y (box 0))
(define mouse-pos (make-posn 0 0))

(define-syntax mouse-dest-x
    (syntax-id-rules ()
      [_ (unbox ocr-found-x)]))
(define-syntax mouse-dest-y
    (syntax-id-rules ()
      [_ (unbox ocr-found-y)]))

(define pos-total-list (make-posn 0 0))
(define pos-selected-object (make-posn 0 0))
(define pos-menu (make-posn 50 40))
(define pos-weapon-left (make-posn 450 675))
(define pos-weapon-right (make-posn 560 675))

(define (weaponn n)
  (make-weapon (+ 561 (* n 51)) 648 (string-append "F" (number->string n))))
(define (weaponsn n)
  (cond
   [(= n 0) empty]
   [else (cons (weaponn n) (weaponsn (- n 1)))]))
(define weapons (weaponsn 1))

(define conf-asteroid-belt 1)
(define conf-space-station 1)
(define conf-miniplanet 1)
(define conf-max-asteroid-belt 4)
(define current-asteroid-belt 0)
(define weapon-gap 65)

(define-syntax-rule (mouse-move-to x y)
  (= 1 (dm "MoveTo" x y)))

(define-syntax-rule (mouse-get-pos)
  (let ([x (box 0)]
        [y (box 0)])
    (if (= 1 (dm "GetCursorPos" x y))
        (make-posn (unbox x) (unbox y)) #f)))

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
(define-syntax key-press
  (syntax-rules ()
    [(_ key) (dm "KeyPressChar" key)]
    [(_ key1 key2) (begin
                     (dm "KeyDownChar" key1)
                     (key-press key2)
                     (dm "KeyUpChar" key1))]
    [(_ key1 key2 ...) (begin
                         (dm "KeyDownChar" key1)
                         (key-press key2 ...)
                         (dm "KeyUpChar" key1))]))

(define (find-string content)
  (if (zero? (dm "FindStrFast"
                 0 0 screen-w screen-h
                 content
                 "0.0.75-0.0.6" 1.0
                 ocr-found-x ocr-found-y))
      (make-posn (unbox ocr-found-x) (unbox ocr-found-y)) #f))

;; 离开空间站函数
(define (exit-space-station)
  (key-press "ctrl" "alt" "e");; 先发送离站命令
  (sleep 5);; 等待5秒
  ;; 除非发现总览,否则代表出站失败,继续出站
  (if (or (find-string "选中的物体") (find-string "总览")) #t (exit-space-station)))

(define (do-init)
  (cond [(zero? hwnd) #f]
        [else
         (dm "LockInput" 1)
         (set! pos-total-list (find-string "总览"))
         (set! pos-selected-object (find-string "选中的物体"))
         (when (zero? (dm "FindStrFast"
                          420 640 602 700
                          "<<"
                          "0.0.91-0.0.40" 1.0
                          ocr-found-x ocr-found-y))
           (set! pos-weapon-right (make-posn mouse-dest-x mouse-dest-y)))
         (when (zero? (dm "FindStrFast"
                          420 640 602 700
                          ">>"
                          "0.0.91-0.0.40" 1.0
                          ocr-found-x ocr-found-y))
           (set! pos-weapon-left (make-posn mouse-dest-x mouse-dest-y)))
         (if (and pos-selected-object pos-total-list) #t #f)]))

(define (menu-select-item content)
  (mouse-move-to (posn-x pos-menu) (posn-y pos-menu))
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (sleep 2)
  (if(zero? (dm "FindStrFast"
                (posn-x pos-menu) (posn-y pos-menu) (+ (posn-x pos-menu) 175) (+ (posn-y pos-menu) 175)
                content
                "0.0.75-0.0.6" 1.0
                ocr-found-x ocr-found-y))
     (mouse-move-to (+ mouse-dest-x 7) (+ mouse-dest-y 5))
     (menu-select-item content)))

;; 进入下一级菜单
(define (menu-deep-in)
  (set! mouse-pos (mouse-get-pos))
  (if (zero? (dm "FindPic"
                 (posn-x mouse-pos) (- (posn-y mouse-pos) 10) screen-w (+ (posn-y mouse-pos) 15)
                 "pic/right.bmp"
                 "000000" 1.0 0
                 ocr-found-x ocr-found-y))
      (mouse-move-to (+  mouse-dest-x 100) (+ mouse-dest-y 3)) #f))

(define-syntax-rule (menu-move-index n)
  (mouse-move 0 (* n 15)))

;; 到第n个空间站
(define (goto-space-station n)
  (key-press "ctrl" "alt" "s") ;; 先停止舰船
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
             mouse-dest-x mouse-dest-y (+ mouse-dest-x 175) (+ mouse-dest-y 175)
             "停靠"
             "0.0.75-0.0.6" 1.0
             ocr-found-x ocr-found-y))
  (mouse-move-to (+ mouse-dest-x 3) ( + mouse-dest-y 3))
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (set! current-asteroid-belt 0)
  (sleep 5)
  (define (wait-for-enter-station)
    (sleep 3)
    (if (not (find-string "空间站服务"))
        (wait-for-enter-station) #t))
  (wait-for-enter-station))

;; 到第n个小行星带
(define (goto-asteroid-belt n)
  ;; 当前小行星带不是目标小行星带时才执行跃迁
  (unless (= current-asteroid-belt n)
    (key-press "ctrl" "alt" "s") ;; 先停止舰船
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
          (wait-for-jumping) #t))
    (wait-for-jumping)
    ;; 跃迁成功，修改当前小行星带
    (set! current-asteroid-belt n)) #t)

;; 锁定第n个矿物
(define (mineral-select-index n)
  (define move-step 19)
  (define base-x (+ (posn-x pos-total-list) 80))
  (define base-y (- (+ (posn-y pos-total-list) 66) move-step))
  (when (miniplanet-exist?)
    (set! base-x (+ mouse-dest-x 19))
    (set! base-y (- (+ mouse-dest-y 6) move-step)))
  (mouse-move-to base-x (+ base-y (* n move-step)))
  (sleep MOUSEDELAY)
  (mouse-left-click)
  (sleep MOUSEDELAY))

;; 卸载货物
(define (store-unload)
  ;; 查找物品机柜位置
  (cond [(zero? (dm "FindStrFast"
                    30 40 328 612
                    "物品机库"
                    "0.0.59-0.0.25" 1.0
                    ocr-found-x ocr-found-y))
         (let ([store-dest-x (+ 10 mouse-dest-x)]
               [store-dest-y (+ 5 mouse-dest-y)])
           (mouse-move-to 430 490)
           (sleep MOUSEDELAY)
           (mouse-left-click)
           (sleep MOUSEDELAY)
           (mouse-right-click)
           (sleep MOUSEDELAY)
           (set! mouse-pos (mouse-get-pos))
           (if (zero? (dm "FindStrFast"
                          (posn-x mouse-pos) (posn-y mouse-pos)
                          (+ 130 (posn-x mouse-pos)) (+ 145 (posn-y mouse-pos))
                          "全选"
                          "0.0.75-0.0.0" 1.0
                          ocr-found-x ocr-found-y))
               (mouse-move-to (+ 5 mouse-dest-x) (+ 5 mouse-dest-y))
               (mouse-move 10 5))
           (sleep MOUSEDELAY)
           (mouse-left-click)
           (sleep MOUSEDELAY)
           (mouse-move-to 450 90)
           (sleep MOUSEDELAY)
           (mouse-left-down)
           (sleep MOUSEDELAY)
           (mouse-move-to store-dest-x store-dest-y)
           (mouse-left-up))]
        [else (key-press "alt" "c");; 不存在则打开货柜
              (sleep 2) ;; 等待0.5秒，等待后继续判断货柜位置
              (store-unload)]))

;; 判断是否正在跃迁
(define (in-jumping?)
  (zero? (dm "FindStrFast"
             460 700 560 750
             "正在跃迁"
             "0.0.0-0.0.0" 1.0
             (box 0) (box 0))))

(define (wait-for-weapon n)
  (define (wait-for-weapon-ex j k)
    (cond [(<= j 0) #t]
          [else
           (sleep 1)
           (if (weapon-in-use? (weaponn 1))
               (wait-for-weapon-ex k k)
               (wait-for-weapon-ex (- j 1) k))]))
  (wait-for-weapon-ex n n))

;; 货柜满的处理
(define (do-when-store-is-full)
  (goto-space-station conf-space-station)
  (sleep 3)
  (store-unload))

;; 与目标的距离
(define-syntax distance-to-target
  (syntax-id-rules ()
    [_ (let ([distance-string ""])
         (dm "UseDict" 1)
         (set! distance-string (dm "Ocr"
                                   (+ (posn-x pos-selected-object) 69)  (+ (posn-y pos-selected-object) 35)
                                   (+ (posn-x pos-selected-object) 103) (+ (posn-y pos-selected-object) 48)
                                   "0.0.67-0.0.50" 1.0))
         (dm "UseDict" 0)
         (string->number (string-replace distance-string "km" "000")))]))

;; 是否进入射程
(define (in-cannon-shot?)
  (< distance-to-target 15000))

;; 接近目标等待，进入射程后返回
(define (wait-for-approach n)
  (define (wait-for-approach-ex j k)
    (cond [(<= j 0) #t]
          [else
           (sleep 0.3)
           (if (not (in-cannon-shot?))
               (wait-for-approach-ex k k)
               (wait-for-approach-ex (- j 1) k))]))
  (wait-for-approach-ex n n))

;;判断是否锁有小行星存在
(define (miniplanet-exist?)
  (zero? (dm "FindStrFast"
             (- (posn-x pos-total-list) 30) (+ 20 (posn-y pos-total-list)) screen-w screen-h
             "小行星矿"
             "0.0.75-0.0.10" 1.0
             ocr-found-x ocr-found-y)))

;;确定不存在小行星？判断两次，因为有时总览列表更新，会一瞬间黑屏
(define (miniplanet-exist??)
  (cond
   [(miniplanet-exist?) #t]
   [else (sleep 1) (miniplanet-exist?)]))

;;判断是否锁定目标
(define (locked?)
  (zero? (dm "FindStrFast"
             (- (posn-x pos-total-list) 30) (+ 20 (posn-y pos-total-list)) screen-w screen-h
             "锁定矿"
             "200.1.92-200.1.10" 1.0
             (box 0) (box 0))))

;; 锁定目标
(define (lock-target)
  (cond
   [(locked?) #t] ;;已锁定，返回成功
   [else (key-press "ctrl")
         (sleep 4)
         ;; 再次判断是否锁定，未成功则继续锁定
         (lock-target)]))

(define (store-is-full?)
  (cond [(zero? (dm "FindStrFast"
                    0 0 screen-w screen-h
                    "过滤器"
                    "0.0.37-0.0.0" 1.0
                    ocr-found-x ocr-found-y))
         (zero? (dm "FindStrFast"
                    (- mouse-dest-x 20) (+ mouse-dest-y 2) mouse-dest-x (+ mouse-dest-y 10)
                    "仓库满"
                    "194.100.40-0.0.0" 1.0
                    (box 0) (box 0)))]
        [else (key-press "alt" "c") (sleep 2) (store-is-full?)]))

;; 构造窗体
(define (create-main-window)
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
                      [min-width 400]))

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
                                                                   (send frame set-label (send label-char-name get-value))
                                                                   (send button-do-bind set-label "解除窗口绑定")]
                                                                  [else (send label-char-name set-value "绑定失败，请检查角色名是否正确")
                                                                        (sleep 0.5)
                                                                        (send label-char-name set-value "请输入用户名")])))]
                                                [else (unbind-char)
                                                      (send label-char-name enable #t)
                                                      (send frame set-label "到底取什么名字好呢")
                                                      (send button-do-bind set-label "查找并绑定窗口")])
                                          (send button-do-bind enable #t))]))

  (define text-field-station-conf (new text-field%
                                       [label "默认空间站："]
                                       [init-value (number->string conf-space-station)]
                                       [parent pane2]))
  (define text-field-max-asteroid-conf (new text-field%
                                            [label "小行星带数目："]
                                            [init-value (number->string conf-max-asteroid-belt)]
                                            [parent pane2]))
  (define text-field-asteroid-conf (new text-field%
                                        [label "默认小行星带："]
                                        [init-value (number->string conf-asteroid-belt)]
                                        [parent pane2]))

  (define text-field-weapon-num (new text-field%
                                     [label "采矿器数量："]
                                     [init-value "2"]
                                     [parent pane3]))

  (define text-field-weapon-gap (new text-field%
                                     [label "采矿周期："]
                                     [init-value "65"]
                                     [parent pane3]))

  (define button-test-weapon (new button%
                                  [label "测试武器状态"]
                                  [parent pane3]
                                  [callback (lambda (button-test-weapon event)
                                              (cond [(weapon-in-use? (weaponn 1))
                                                     (send button-test-weapon set-label "武器使用中")
                                                     (sleep 0.5)
                                                     (send button-test-weapon set-label "测试武器状态")]
                                                    [else (send button-test-weapon set-label "武器空闲")
                                                          (sleep 0.5)
                                                          (send button-test-weapon set-label "测试武器状态")]))]))

  (define button-start-stop (new button%
                                 [label "开始"]
                                 [parent pane10]
                                 [callback (lambda (button-start-stop event)
                                             (cond
                                              [(string=? (send button-start-stop get-label) "开始") (do-when-button-start-clicked)]
                                              [else (do-when-button-stop-clicked)]))]))
  (define message-show-status (new message%
                                   [label "绑定窗口前，请确保窗口有一部分在屏幕外部"]
                                   [parent pane10]))

  (define (do-when-button-start-clicked)
    (cond
     [(do-init)
      (set! conf-asteroid-belt (string->number (send text-field-asteroid-conf get-value)))
      (set! conf-max-asteroid-belt (string->number (send text-field-max-asteroid-conf get-value)))
      (set! conf-space-station (string->number (send text-field-station-conf get-value)))
      (set! weapons (weaponsn (string->number (send text-field-weapon-num get-value))))
      (set! weapon-gap (string->number (send text-field-weapon-gap get-value)))
      (mouse-move-to 350 230)
      (sleep MOUSEDELAY)
      (mouse-left-click)
      (sleep MOUSEDELAY)
      (if (zero? weapon-gap)
          (set! t (thread start-mine))
          (set! t (thread start-mine-ex)))
      (send button-start-stop set-label "停止")]
     [else (send message-show-status set-label "请先绑定角色或先将你的矿船开到太空中")
           (sleep 0.5)
           (send message-show-status set-label "绑定窗口前，请确保窗口有一部分在屏幕外部")]))

  (define (do-when-button-stop-clicked)
    (kill-thread t)
    (dm "LockInput" 0)
    (unbind-char)
    (send button-do-bind set-label "查找并绑定窗口")
    (send button-start-stop set-label "开始"))
  frame)

(define (find-and-bind-char char-name)
  (set! hwnd (dm "FindWindow" "triuiScreen" (string-append "EVE - " char-name)))
  (if (zero? hwnd) #f (= 1 (dm "BindWindow" hwnd "dx2" "dx" "dx" 0))))

(define (unbind-char)
  (dm "UnBindWindow")
  (set! hwnd 0))

;; 进行采矿
(define (do-mine)
  (mineral-select-index conf-miniplanet);; 选择一个小行星
  ;; 若目标不是小行星，则重新选择
  (cond [(target-is-miniplant?)
         (key-press "q") ;; 接近小型星
         (lock-target)  ;; 进行锁定
         (wait-for-approach 3) ;; 等待进入射程
         (start-weapons weapons)
         (sleep 3)]
        [else (do-mine)]))

(define (shift-asteroid-belt)
  (if (>= conf-asteroid-belt conf-max-asteroid-belt)
      (set! conf-asteroid-belt 1)
      (set! conf-asteroid-belt (+ 1 conf-asteroid-belt))))

(define (shift-miniplant)
  (if (>= conf-miniplanet 30)
      (set! conf-miniplanet 1)
      (set! conf-miniplanet (+ 1 conf-miniplanet))))

(define (start-mine)
  (goto-asteroid-belt conf-asteroid-belt)
  ;; 有矿物才进行开采，没有则更换小行星带
  (cond [(miniplanet-exist?)
         ;; 进行采矿操作
         (do-mine)
         (wait-for-weapon 3)
         ;; 武器停了,需要检查原因
         (cond
          [(store-is-full?) ;; 仓库满则回空间站，卸载矿物后继续采矿
           (do-when-store-is-full)
           (sleep 2)
           (exit-space-station)
           (start-mine)]
          [(not (target-is-miniplant?)) ;;有颗小行星被采空，目标丢失，换个miniplant重新开始
           (stop-weapons weapons) ;; 停下正在使用的武器
           (start-mine)]
          [else ;; 其他情况暂未处理，继续开采
           (stop-weapons weapons) ;; 停下正在使用的武器
           (start-mine)])]
        [else (shift-asteroid-belt) (start-mine)]))

;; 判断目标是否小行星
(define (target-is-miniplant?)
  (zero? (dm "FindStrFast"
             (posn-x pos-selected-object)
             (posn-y pos-selected-object)
             (+ (posn-x pos-selected-object) 90)
             (+ (posn-y pos-selected-object) 50)
             "小行星"
             "0.0.75-0.0.0" 1.0
             (box 0) (box 0))))

;; 停下正在使用的武器
(define (stop-weapons weapons)
  (cond
   [(empty? weapons) #t]
   [else (when (weapon-in-use? (first weapons))
           (key-press (weapon-hotkey (first weapons))))
         (stop-weapons (rest weapons))]))

(define (start-weapons weapons)
  (cond
   [(empty? weapons) #t]
   [else (unless (weapon-in-use? (first weapons))
           (key-press (weapon-hotkey (first weapons))))
         (start-weapons (rest weapons))]))

;; 判断武器是否在使用中
(define (weapon-in-use? a-weapon)
  (= 1 (dm "FindColor"
           (weapon-x a-weapon) (weapon-y a-weapon)
           (+ 1 (weapon-x a-weapon)) (+ 1 (weapon-y a-weapon))
           "c5c5c5-333333" 1.0 0 (box 0) (box 0))))

(define (start-mine-ex)
  (goto-asteroid-belt conf-asteroid-belt)
  ;; 有矿物才进行开采，没有则更换小行星带
  (cond [(miniplanet-exist?)
         ;; 进行采矿操作
         (do-mine)
         (sleep weapon-gap)
         (stop-weapons weapons)
         (wait-for-weapon 3)
         ;; 武器停了,需要检查原因
         (cond
          [(store-is-full?) ;; 仓库满则回空间站，卸载矿物后继续采矿
           (do-when-store-is-full)
           (sleep 2)
           (exit-space-station)
           (start-mine-ex)]
          [(not (target-is-miniplant?)) ;;有颗小行星被采空，目标丢失，换个miniplant重新开始
           (start-mine-ex)]
          [else ;; 其他情况暂未处理，继续开采
           (start-mine-ex)])]
        [else (shift-asteroid-belt) (start-mine-ex)]))

(define main-window (create-main-window))
(send main-window show #t)
