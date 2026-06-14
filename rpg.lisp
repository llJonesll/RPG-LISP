;; ====== 1. CLASSES E ATRIBUTOS ======

(defclass personagem ()
  ((nome      :initarg :nome      :accessor nome)
   (hp        :initarg :hp        :accessor hp)
   (hp-max    :initarg :hp-max    :accessor hp-max)
   (dano-base :initarg :dano-base :accessor dano-base)))

(defclass guerreiro (personagem) ())
(defclass mago      (personagem) ())
(defclass monstro (personagem) ())

;; ====== 2. FUNÇÕES BASE DE DANO ======

(declaim (optimize (debug 3)))

;; --- Dano ---
(defun calcular-dano-guerreiro (p) (dano-base p))
(defun calcular-dano-mago (p)      (* (dano-base p) 2))
(defun calcular-dano-boss (b)      (dano-base b))


;; ====== 3. ESTADO GLOBAL ======

(defvar *clima-atual* "Normal")
(defvar *descricao-clima* "Funcoes em estado padrao. Danos fixos e previstos.")
(defvar *log-combate* (list "O combate ainda nao comecou!"))
(defvar *max-linhas-log* 8)

;; Nossos personagens agora sao GLOBAIS para podermos hackear pelo REPL!
(defvar *guerreiro* nil)
(defvar *mago* nil)
(defvar *chefe* nil)

(defun adicionar-log (msg)
  (setf *log-combate* (append *log-combate* (list msg)))
  (when (> (length *log-combate*) *max-linhas-log*)
    (setf *log-combate* (cdr *log-combate*))))

    ;; O gancho que vai segurar o "controle remoto" da tela
(defvar *hook-atualizar-ui* nil)

;; A função que você vai chamar no terminal!
(defun atualizar-tela ()
  (if *hook-atualizar-ui*
      (progn
        (adicionar-log "[SISTEMA] O REPL interveio e atualizou a realidade!")
        (funcall *hook-atualizar-ui*)
        'interface-atualizada-com-sucesso)
      'erro-o-jogo-ainda-nao-comecou))
;; ====== 4. MÚLTIPLO DESPACHO ======

(defgeneric atacar (atacante alvo))

(defmethod atacar ((p guerreiro) (m monstro))
  (let ((dano (calcular-dano-guerreiro p))) ;; <-- Passando 'p' aqui!
    (setf (hp m) (max 0 (- (hp m) dano)))
    (adicionar-log (format nil "[GUERREIRO] ~A atacou o Boss por ~A de dano!" (nome p) dano))))

(defmethod atacar ((p mago) (m monstro))
  (let ((dano (calcular-dano-mago p)))      ;; <-- Aqui ja estava certo
    (setf (hp m) (max 0 (- (hp m) dano)))
    (adicionar-log (format nil "[MAGO] ~A conjurou e causou ~A de dano!" (nome p) dano))))

(defmethod atacar ((m monstro) (p personagem))
  (let ((dano (calcular-dano-boss m)))      ;; <-- Passando 'm' (o boss) aqui!
    (setf (hp p) (max 0 (- (hp p) dano)))
    (adicionar-log (format nil "[BOSS] ~A atacou ~A por ~A de dano!" (nome m) (nome p) dano))))

;; ====== 5. METAPROGRAMACAO DINAMICA (MUTACAO DO CLIMA) ======

(defun mudar-clima-do-mundo (tipo)
  (cond
    ;; --- CLIMA 1: Normal ---
    ((equal tipo 1)
     (setf *clima-atual* "Normal")
     (setf *descricao-clima* "Logica padrao. Guerreiro usa dano-base, Mago=Magia*2, Boss usa dano-base.")
     (setf (fdefinition 'calcular-dano-guerreiro)
           (lambda (p) (dano-base p))) ;; <-- Usa o atributo da classe!
     (setf (fdefinition 'calcular-dano-mago)
           (lambda (p) (* (dano-base p) 2)))
     (setf (fdefinition 'calcular-dano-boss)
           (lambda (b) (dano-base b)))) ;; <-- Usa o atributo da classe!

    ;; --- CLIMA 2: Infernal ---
    ((equal tipo 2)
     (setf *clima-atual* "Infernal (Fogo)")
     (setf *descricao-clima* "Fogo caotiza tudo! Guerreiro=dado(0/Base/50), Mago=Magia*4, Boss=Base+15!")
     (setf (fdefinition 'calcular-dano-guerreiro)
           (lambda (p)
             (let ((dado (+ 1 (random 3))))
               (cond ((equal dado 1) 50)
                     ((equal dado 2) (dano-base p)) ;; <-- Se tirar 2, bate o dano normal
                     (t              0)))))
     (setf (fdefinition 'calcular-dano-mago)
           (lambda (p) (* (dano-base p) 4)))
     (setf (fdefinition 'calcular-dano-boss)
           (lambda (b) (+ (dano-base b) 15)))) ;; <-- Boss ganha buff no dano base!

    ;; --- CLIMA 3: Antimagia ---
    ((equal tipo 3)
     (setf *clima-atual* "Antimagia Absoluta")
     (setf *descricao-clima* "Magia bloqueada! Mago causa 0 dano. Boss enfraquece.")
     (setf (fdefinition 'calcular-dano-guerreiro)
           (lambda (p) (dano-base p)))
     (setf (fdefinition 'calcular-dano-mago)
           (lambda (p) (declare (ignore p)) 0)) ;; Ignora e bate 0
     (setf (fdefinition 'calcular-dano-boss)
           (lambda (b) (declare (ignore b)) 10))) ;; Ignora a base e bate só 10

    ;; --- CLIMA 4: Inversao Total ---
    ((equal tipo 4)
     (setf *clima-atual* "Inversao Total")
     (setf *descricao-clima* "Realidade invertida! Mago vira tanque(5), Guerreiro usa magia(30), Boss confuso(5).")
     (setf (fdefinition 'calcular-dano-guerreiro)
           (lambda (p) (declare (ignore p)) 30))
     (setf (fdefinition 'calcular-dano-mago)
           (lambda (p) (declare (ignore p)) 5))
     (setf (fdefinition 'calcular-dano-boss)
           (lambda (b) (declare (ignore b)) 5)))))

;; ====== 6. INTERFACE GRAFICA ======

(ql:quickload :ltk)
(ql:quickload :bordeaux-threads)


(defun barra-hp (hp hp-max &optional (tamanho 10))
  (let* ((cheios (round (* tamanho (/ hp hp-max))))
         (vazios (- tamanho cheios)))
    (format nil "[~A~A] ~A/~A"
            (make-string cheios :initial-element #\#)
            (make-string vazios :initial-element #\.)
            hp hp-max)))

(defun iniciar-jogo ()
  ;; A funcao iniciar-jogo agora APENAS cria a thread e devolve o terminal pra voce na hora!
  (bt:make-thread
    (lambda ()
      ;; === TODO O SEU CÓDIGO ANTIGO VEM AQUI PRA DENTRO ===
      (setf *random-state* (make-random-state t))
      (setf *log-combate* (list "O combate ainda nao comecou!"))

      (setf *guerreiro* (make-instance 'guerreiro :nome "Jones" :hp 100 :hp-max 100 :dano-base 15))
      (setf *mago* (make-instance 'mago :nome "Lisbon" :hp 80  :hp-max 80  :dano-base 30))
      (setf *chefe* (make-instance 'monstro :nome "Prof. de Paradigmas" :hp 300 :hp-max 300 :dano-base 40))

      (let ((turno 0))
        (ltk:with-ltk ()
          (ltk:wm-title ltk:*tk* "LISP RPG")

      (let* (
             ;; === TITULO ===
             (lbl-titulo (make-instance 'ltk:label :text "=== LISP RPG ===" :font "Courier 14 bold"))

             ;; === CLIMA ===
             (lbl-clima (make-instance 'ltk:label :text (format nil "Clima: ~A" *clima-atual*) :font "Helvetica 11 bold" :foreground "darkgreen"))
             (lbl-desc-clima (make-instance 'ltk:label :text *descricao-clima* :font "Helvetica 9 italic" :foreground "gray30"))

             ;; === STATUS DOS PERSONAGENS ===
             (lbl-status-g (make-instance 'ltk:label
                             :text (format nil "Guerreiro ~A [Base: ~A]: ~A" (nome *guerreiro*) (dano-base *guerreiro*) (barra-hp (hp *guerreiro*) (hp-max *guerreiro*)))
                             :font "Courier 10" :foreground "navy"))
             (lbl-status-m (make-instance 'ltk:label
                             :text (format nil "Mago      ~A [Base: ~A]: ~A" (nome *mago*) (dano-base *mago*) (barra-hp (hp *mago*) (hp-max *mago*)))
                             :font "Courier 10" :foreground "purple"))
             (lbl-status-b (make-instance 'ltk:label
                             :text (format nil "Boss ~A [Base: ~A]: ~A" (nome *chefe*) (dano-base *chefe*) (barra-hp (hp *chefe*) (hp-max *chefe*)))
                             :font "Courier 10 bold" :foreground "darkred"))

             ;; === LOG DE COMBATE ===
             ;; (Mantenha o txt-log normal)
             (lbl-log-titulo (make-instance 'ltk:label :text "-- Log de Combate --" :font "Helvetica 10 bold"))
             (txt-log (make-instance 'ltk:text :width 65 :height 8))

             ;; === AST DAS FUNCOES ===
             ;; (Mantenha normal)
             (lbl-ast-titulo (make-instance 'ltk:label :text "-- AST das Funcoes na RAM (fdefinition ao vivo) --" :font "Courier 10 bold"))
             (lbl-ast-g (make-instance 'ltk:label :text "calcular-dano-guerreiro:" :font "Courier 9 bold" :foreground "navy"))
             (txt-ast-guerreiro (make-instance 'ltk:text :width 65 :height 3))
             (lbl-ast-m (make-instance 'ltk:label :text "calcular-dano-mago:" :font "Courier 9 bold" :foreground "purple"))
             (txt-ast-mago (make-instance 'ltk:text :width 65 :height 3))
             (lbl-ast-b (make-instance 'ltk:label :text "calcular-dano-boss:" :font "Courier 9 bold" :foreground "darkred"))
             (txt-ast-boss (make-instance 'ltk:text :width 65 :height 3))

             ;; === FUNCOES DE ATUALIZACAO DA UI ===
             (atualizar-status
               (lambda ()
                 (setf (ltk:text lbl-status-g) (format nil "Guerreiro ~A [Base: ~A]: ~A" (nome *guerreiro*) (dano-base *guerreiro*) (barra-hp (hp *guerreiro*) (hp-max *guerreiro*))))
                 (setf (ltk:text lbl-status-m) (format nil "Mago      ~A [Base: ~A]: ~A" (nome *mago*) (dano-base *mago*) (barra-hp (hp *mago*) (hp-max *mago*))))
                 (setf (ltk:text lbl-status-b) (format nil "Boss ~A [Base: ~A]: ~A" (nome *chefe*) (dano-base *chefe*) (barra-hp (hp *chefe*) (hp-max *chefe*))))))
             
             (atualizar-log
               (lambda ()
                 (ltk:format-wish "~A configure -state normal" (ltk::widget-path txt-log))
                 (ltk:format-wish "~A delete 1.0 end" (ltk::widget-path txt-log))
                 (dolist (linha *log-combate*)
                   (ltk:format-wish "~A insert end {~A~A}" (ltk::widget-path txt-log) linha (string #\newline)))
                 (ltk:format-wish "~A configure -state disabled" (ltk::widget-path txt-log))
                 (ltk:format-wish "~A see end" (ltk::widget-path txt-log))))

             (escrever-ast
               (lambda (widget expr-str)
                 (ltk:format-wish "~A configure -state normal" (ltk::widget-path widget))
                 (ltk:format-wish "~A delete 1.0 end" (ltk::widget-path widget))
                 (ltk:format-wish "~A insert end {~A}" (ltk::widget-path widget) expr-str)
                 (ltk:format-wish "~A configure -state disabled" (ltk::widget-path widget))))

             (atualizar-ast
               (lambda ()
                 (funcall escrever-ast txt-ast-guerreiro (format nil "~A" (function-lambda-expression #'calcular-dano-guerreiro)))
                 (funcall escrever-ast txt-ast-mago (format nil "~A" (function-lambda-expression #'calcular-dano-mago)))
                 (funcall escrever-ast txt-ast-boss (format nil "~A" (function-lambda-expression #'calcular-dano-boss)))))

             ;; === BOTAO PRINCIPAL ===
             (btn-turno
               (make-instance 'ltk:button
                 :text ">>> AVANCAR TURNO <<<"
                 :command (lambda ()
                            (cond
                              ((and (<= (hp *guerreiro*) 0) (<= (hp *mago*) 0))
                               (adicionar-log "[DERROTA] Os herois cairam... O Professor vence!")
                               (funcall atualizar-log))
                              ((<= (hp *chefe*) 0)
                               (adicionar-log "[VITORIA] O chefe foi derrotado! A prova foi entregue!")
                               (funcall atualizar-log))
                              (t
                               (cond
                                 ((= turno 0)
                                  (if (> (hp *guerreiro*) 0)
                                    (atacar *guerreiro* *chefe*)
                                    (adicionar-log "[SKIP] Guerreiro esta morto.")))
                                 ((= turno 1)
                                  (if (> (hp *mago*) 0)
                                    (atacar *mago* *chefe*)
                                    (adicionar-log "[SKIP] Mago esta morto.")))
                                 ((= turno 2)
                                  (if (> (hp *guerreiro*) 0)
                                    (atacar *chefe* *guerreiro*)
                                    (adicionar-log "[SKIP] Boss tenta atacar Guerreiro, mas ele ja caiu.")))
                                 ((= turno 3)
                                  (if (> (hp *mago*) 0)
                                    (atacar *chefe* *mago*)
                                    (adicionar-log "[SKIP] Boss tenta atacar Mago, mas ele ja caiu."))))
                               (setf turno (mod (+ turno 1) 4))
                               (funcall atualizar-status)
                               (funcall atualizar-log)
                               (funcall atualizar-ast))))))

             ;; === BOTOES DE CLIMA ===
             (btn-normal
               (make-instance 'ltk:button :text "[1] Clima Normal"
                 :command (lambda ()
                            (mudar-clima-do-mundo 1)
                            (setf (ltk:text lbl-clima) (format nil "Clima: ~A" *clima-atual*))
                            (setf (ltk:text lbl-desc-clima) *descricao-clima*)
                            (adicionar-log (format nil "[CLIMA] Mudou para: ~A" *clima-atual*))
                            (funcall atualizar-log)
                            (funcall atualizar-ast))))

             (btn-fogo
               (make-instance 'ltk:button :text "[2] Clima Infernal"
                 :command (lambda ()
                            (mudar-clima-do-mundo 2)
                            (setf (ltk:text lbl-clima) (format nil "Clima: ~A" *clima-atual*))
                            (setf (ltk:text lbl-desc-clima) *descricao-clima*)
                            (adicionar-log (format nil "[CLIMA] Mudou para: ~A" *clima-atual*))
                            (funcall atualizar-log)
                            (funcall atualizar-ast))))

             (btn-antimagia
               (make-instance 'ltk:button :text "[3] Zona Antimagia"
                 :command (lambda ()
                            (mudar-clima-do-mundo 3)
                            (setf (ltk:text lbl-clima) (format nil "Clima: ~A" *clima-atual*))
                            (setf (ltk:text lbl-desc-clima) *descricao-clima*)
                            (adicionar-log (format nil "[CLIMA] Mudou para: ~A" *clima-atual*))
                            (funcall atualizar-log)
                            (funcall atualizar-ast))))

             (btn-inversao
               (make-instance 'ltk:button :text "[4] Inversao Total"
                 :command (lambda ()
                            (mudar-clima-do-mundo 4)
                            (setf (ltk:text lbl-clima) (format nil "Clima: ~A" *clima-atual*))
                            (setf (ltk:text lbl-desc-clima) *descricao-clima*)
                            (adicionar-log (format nil "[CLIMA] Mudou para: ~A" *clima-atual*))
                            (funcall atualizar-log)
                            (funcall atualizar-ast)))))

        ;; === CONECTANDO O CONTROLE REMOTO PARA O REPL ===
             ;; 1. Capturamos a conexao da Thread do jogo numa variavel local
             (let ((conexao-tk ltk:*wish*)) 
               (setf *hook-atualizar-ui*
                     (lambda ()
                       ;; 2. Quando o REPL chamar essa funcao, ele usa a conexao emprestada!
                       (let ((ltk:*wish* conexao-tk))
                         (funcall atualizar-status)
                         (funcall atualizar-log)
                         (funcall atualizar-ast)))))
        ;; === LAYOUT ===
        (ltk:pack lbl-titulo :pady 10)
        (ltk:pack lbl-clima :pady 3)
        (ltk:pack lbl-desc-clima :pady 2)
        (ltk:pack lbl-status-g :anchor :w :padx 40 :pady 1)
        (ltk:pack lbl-status-m :anchor :w :padx 40 :pady 1)
        (ltk:pack lbl-status-b :anchor :w :padx 40 :pady 1)
        (ltk:pack btn-turno :fill :x :padx 40 :pady 8)
        (ltk:pack btn-normal :fill :x :padx 40 :pady 2)
        (ltk:pack btn-fogo :fill :x :padx 40 :pady 2)
        (ltk:pack btn-antimagia :fill :x :padx 40 :pady 2)
        (ltk:pack btn-inversao :fill :x :padx 40 :pady 2)
        (ltk:pack lbl-log-titulo :pady 6)
        (ltk:pack txt-log :padx 40 :pady 2)
        (ltk:pack lbl-ast-titulo :pady 8)
        (ltk:pack lbl-ast-g :anchor :w :padx 40)
        (ltk:pack txt-ast-guerreiro :padx 40 :pady 2)
        (ltk:pack lbl-ast-m :anchor :w :padx 40)
        (ltk:pack txt-ast-mago :padx 40 :pady 2)
        (ltk:pack lbl-ast-b :anchor :w :padx 40)
        (ltk:pack txt-ast-boss :padx 40 :pady 2)

        ;; Inicializa tudo
        (ltk:after 100
          (lambda ()
            (funcall atualizar-log)
            (funcall atualizar-ast)))))))))