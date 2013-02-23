(require ffi/unsafe)
(require ffi/com)
(require ffi/com-registry)

;; 创建一个大漠插件实例
(define dm-obj (com-create-instance (coclass->clsid "dm.dmsoft")))

;; 定义dm宏，封装大漠命令
(define-syntax-rule (dm function-name ...)
  (com-invoke dm-obj function-name ...))


;; 鼠标处理 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct posn (x y)) ;; 坐标点结构

;; 移动到指定坐标
(define-syntax mouse-move-to
  (syntax-rules ()
                [(_ pos) (= 1 (dm "MoveTo" (posn-x pos) (posn-y pos)))]
                [(_ x y) (= 1 (dm "MoveTo" x y))]))
;; 移动相对坐标
(define-syntax-rule (mouse-move x y)
  (= 1 (dm "MoveR" x y)))
;; 鼠标左击
(define-syntax-rule (mouse-left-click)
  (= 1 (dm "LeftClick")))
;; 鼠标右击
(define-syntax-rule (mouse-right-click)
  (= 1 (dm "RightClick")))
;; 鼠标双击
(define-syntax-rule (mouse-double-click)
  (= 1 (dm "LeftDoubleClick")))
;; 左键按下
(define-syntax-rule (mouse-left-down)
  (= 1 (dm "LeftDown")))
;; 左键松开
(define-syntax-rule (mouse-left-up)
  (= 1 (dm "LeftUp")))
;; 右键按下
(define-syntax-rule (mouse-right-down)
  (= 1 (dm "RightDown")))
;; 右键松开
(define-syntax-rule (mouse-right-up)
  (= 1 (dm "RightUp")))
;; 鼠标拖动
(define-syntax-rule (mouse-drag from to)
  (and (mouse-move-to from)
       (mouse-left-down)
       (mouse-move-to to)
       (mouse-left-up)))
;; 返回鼠标当前位置
(define-syntax-rule (mouse-get-pos)
  (let ([x (box 0)]
        [y (box 0)])
    (if (= 1 (dm "GetCursorPos" x y))
        (make-posn (unbox x) (unbox y)) #f)))

;; 键盘操作 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
