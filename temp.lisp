(dm "SetPath" "D:/MyDoc/����/3.1233")
(dm "SetDict" 0 "eve�ɿ�.txt")
;; �����
(dm "UnBindWindow")



(define hwnd (dm "FindWindow" "AskTao" "��ͯһ��"))
 (dm "BindWindow" hwnd "dx" "dx2" "dx" 0)

(dm "Capture" 0 0 800 600 "screen.bmp")

(dm "Ocr" 0 0 800 600 "ffffff-111111" 1.0)
(dm "FindStr" 0 0 1024 768 "���н�Ǯ" "ffffff-111111" 1.0 SelectedX   SelectedY)








(define hwnd-eve (dm "FindWindow" "triuiScreen" "EVE - IGameless"))
(dm "BindWindow" hwnd-eve "dx2" "windows" "dx" 0)

(mouse-move-to 512 760)

(mouse-right-click)

(mouse-move-to-string "С���Ǵ�")

(mouse-move-right 220)

(mouse-move-to-string "ԾǨ��")

(mouse-left-click)

(dm "Capture" 0 0 1024 768 "screen.bmp")

(define SelectedY (box 0))
(define SelectedY (box 0))
(dm "FindStr" 0 0 1024 768 "����" "207.4.80-207.4.10" 1.0 ocr-find-x   ocr-find-y)
(unbox SelectedY)
(unbox SelectedX)
(dm "SendString" hwnd-eve "���Ĵ�����ȫû����")
(dm "MoveTo" (unbox SelectedX) (unbox SelectedY))

(dm "MoveTo" (+ 3 (unbox SelectedX)) (+ 3 (unbox SelectedY)))
(dm "GetCursorPos" SelectedX SelectedY)
