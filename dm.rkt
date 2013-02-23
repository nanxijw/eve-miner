(require ffi/unsafe)
(require ffi/com)
(require ffi/com-registry)

;; ����һ����Į���ʵ��
(define dm-obj (com-create-instance (coclass->clsid "dm.dmsoft")))

;; ����dm�꣬��װ��Į����
(define-syntax-rule (dm function-name ...)
  (com-invoke dm-obj function-name ...))


;; ��괦�� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct posn (x y)) ;; �����ṹ

;; �ƶ���ָ������
(define-syntax mouse-move-to
  (syntax-rules ()
                [(_ pos) (= 1 (dm "MoveTo" (posn-x pos) (posn-y pos)))]
                [(_ x y) (= 1 (dm "MoveTo" x y))]))
;; �ƶ��������
(define-syntax-rule (mouse-move x y)
  (= 1 (dm "MoveR" x y)))
;; ������
(define-syntax-rule (mouse-left-click)
  (= 1 (dm "LeftClick")))
;; ����һ�
(define-syntax-rule (mouse-right-click)
  (= 1 (dm "RightClick")))
;; ���˫��
(define-syntax-rule (mouse-double-click)
  (= 1 (dm "LeftDoubleClick")))
;; �������
(define-syntax-rule (mouse-left-down)
  (= 1 (dm "LeftDown")))
;; ����ɿ�
(define-syntax-rule (mouse-left-up)
  (= 1 (dm "LeftUp")))
;; �Ҽ�����
(define-syntax-rule (mouse-right-down)
  (= 1 (dm "RightDown")))
;; �Ҽ��ɿ�
(define-syntax-rule (mouse-right-up)
  (= 1 (dm "RightUp")))
;; ����϶�
(define-syntax-rule (mouse-drag from to)
  (and (mouse-move-to from)
       (mouse-left-down)
       (mouse-move-to to)
       (mouse-left-up)))
;; ������굱ǰλ��
(define-syntax-rule (mouse-get-pos)
  (let ([x (box 0)]
        [y (box 0)])
    (if (= 1 (dm "GetCursorPos" x y))
        (make-posn (unbox x) (unbox y)) #f)))

;; ���̲��� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
