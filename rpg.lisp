;; ====== 1. CLASSES E ATRIBUTOS ======

(defclass personagem ()
	((nome :initarg :nome :accessor nome)
	(hp :initarg :hp :accessor hp)
	(forca :initarg :forca :accessor forca)
	(magia :initarg :magia :accessor magia)))

(defclass guerreiro (personagem) ())
(defclass mago (personagem) ())

(defclass monstro ()
	((nome :initarg :nome :accessor nome)
	(hp :initarg :hp :accessor hp)))

;; ====== 2. ASSINATURAS E FUNÇÕES BASE ======

(declaim (optimize (debug 3)))

(defun calcular-dano-guerreiro () 15)
(defun calcular-dano-mago (mago-obj) (* (magia mago-obj) 2))

(defvar *clima-atual* "Normal")
(defvar *ultimo-log-combate* "O combate ainda nao comecou. Clique em 'Avancar Turno'!")

;; ====== 3. MÚLTIPLO DESPACHO (MULTIPLE DISPATCH) ======

(defgeneric atacar (atacante alvo))

(defmethod atacar ((p guerreiro) (m monstro))
	(let ((dano (calcular-dano-guerreiro)))
		(setf (hp m) (- (hp m) dano))
		(setf *ultimo-log-combate* (format nil "[TURNO] O Guerreiro ~A atacou e causou ~A de dano!" (nome p) dano))))

(defmethod atacar ((p mago) (m monstro))
	(let ((dano (calcular-dano-mago p)))
		(setf (hp m) (- (hp m) dano))
		(setf *ultimo-log-combate* (format nil "[TURNO] O Mago ~A conjurou feitico e causou ~A de dano!" (nome p) dano))))

;; ====== 4. METAPROGRAMAÇÃO DINÂMICA (MUTAÇÃO DO CLIMA) ======

(defun mudar-clima-do-mundo (tipo)
	(cond
		((equal tipo 1)
			(setf *clima-atual* "Normal")
			(setf (fdefinition 'calcular-dano-guerreiro) (lambda () 15))
			(setf (fdefinition 'calcular-dano-mago) (lambda (mago-obj) (* (magia mago-obj) 2))))

		((equal tipo 2)
			(setf *clima-atual* "Infernal (Fogo)")
			(setf (fdefinition 'calcular-dano-guerreiro)
				(lambda ()
					(let ((dado (+ 1 (random 3))))
						(cond
							((equal dado 1) 50)
							((equal dado 2) 15)
							((equal dado 3) 0)))))
			(setf (fdefinition 'calcular-dano-mago)
				(lambda (mago-obj) (* (magia mago-obj) 4))))

		((equal tipo 3)
			(setf *clima-atual* "Antimagia Absoluta")
			(setf (fdefinition 'calcular-dano-guerreiro) (lambda () 15))
			(setf (fdefinition 'calcular-dano-mago)
				(lambda (mago-obj) (declare (ignore mago-obj)) 0)))))

;; ====== 5. INTERFACE GRÁFICA CONTROLADA POR BOTÃO ======

(ql:quickload :ltk)

(defun iniciar-jogo ()
	(setf *random-state* (make-random-state t))

	(let ((guerreiro (make-instance 'guerreiro :nome "Jones" :hp 100 :forca 15 :magia 0))
		(mago (make-instance 'mago :nome "Lisbon" :hp 60 :forca 5 :magia 20))
		(chefe (make-instance 'monstro :nome "Professor de Paradigmas" :hp 300))
		(turno-guerreiro t))

		(ltk:with-ltk ()
			(ltk:wm-title ltk:*tk* "Candy Box: Lisp Mutante Edition")

			(let* ((lbl-titulo (make-instance 'ltk:label :text "=== CANDY BOX: LISP MUTANTE ===" :font "Courier 14 bold"))
				(lbl-clima (make-instance 'ltk:label :text (format nil "Clima Atual: ~A" *clima-atual*) :font "Helvetica 12 bold" :foreground "darkgreen"))
				(lbl-status (make-instance 'ltk:label :text (format nil "Inimigo: ~A | HP: ~A / 300" (nome chefe) (hp chefe)) :font "Helvetica 11 bold"))
				(lbl-log (make-instance 'ltk:label :text *ultimo-log-combate* :font "Helvetica 10 italic" :foreground "blue"))

				;; Caixas de texto para exibir as funções mutantes na RAM
				(lbl-ast-guerreiro (make-instance 'ltk:label :text "AST da Funcao do Guerreiro na RAM:" :font "Courier 10 bold"))
				(txt-ast-guerreiro (make-instance 'ltk:entry :width 70))
				(lbl-ast-mago (make-instance 'ltk:label :text "AST da Funcao do Mago na RAM:" :font "Courier 10 bold"))
				(txt-ast-mago (make-instance 'ltk:entry :width 70))

				;; Função auxiliar para atualizar as caixas de texto
				(atualizar-interface-ast
					(lambda ()
						(setf (ltk:text txt-ast-guerreiro) (format nil "~A" (function-lambda-expression #'calcular-dano-guerreiro)))
						(setf (ltk:text txt-ast-mago) (format nil "~A" (function-lambda-expression #'calcular-dano-mago)))))

				;; Botões sem propriedades de fontes problemáticas
				(btn-turno (make-instance 'ltk:button
							:text " >>> AVANÇAR TURNO (ATACAR) <<<"
							:command (lambda ()
										(if (<= (hp chefe) 0)
											(progn
												(setf (ltk:text lbl-status) "Inimigo: DEFEATED! | HP: 0 / 300")
												(setf (ltk:text lbl-log) "[VITÓRIA] O chefe evaporou! O trabalho está pronto."))
											(progn
												(if turno-guerreiro
													(atacar guerreiro chefe)
													(atacar mago chefe))
												(setf turno-guerreiro (not turno-guerreiro))
												(setf (ltk:text lbl-status) (format nil "Inimigo: ~A | HP: ~A / 300" (nome chefe) (hp chefe)))
												(setf (ltk:text lbl-log) *ultimo-log-combate*)
												(funcall atualizar-interface-ast))))))

				(btn-normal (make-instance 'ltk:button
							:text "Mudar para Clima Normal"
							:command (lambda ()
										(mudar-clima-do-mundo 1)
										(setf (ltk:text lbl-clima) (format nil "Clima Atual: ~A" *clima-atual*))
										(funcall atualizar-interface-ast))))

				(btn-fogo (make-instance 'ltk:button
							:text "Ativar Clima Infernal (Fogo)"
							:command (lambda ()
										(mudar-clima-do-mundo 2)
										(setf (ltk:text lbl-clima) (format nil "Clima Atual: ~A" *clima-atual*))
										(funcall atualizar-interface-ast))))

				(btn-antimagia (make-instance 'ltk:button
								:text "Ativar Zona Antimagia"
								:command (lambda ()
											(mudar-clima-do-mundo 3)
											(setf (ltk:text lbl-clima) (format nil "Clima Atual: ~A" *clima-atual*))
											(funcall atualizar-interface-ast)))))

				;; Posicionamento na Tela
				(ltk:pack lbl-titulo :pady 15)
				(ltk:pack lbl-clima :pady 5)
				(ltk:pack lbl-status :pady 5)
				(ltk:pack lbl-log :pady 10)

				(ltk:pack btn-turno :fill :x :padx 40 :pady 10)

				(ltk:pack btn-normal :fill :x :padx 40 :pady 2)
				(ltk:pack btn-fogo :fill :x :padx 40 :pady 2)
				(ltk:pack btn-antimagia :fill :x :padx 40 :pady 2)

				(ltk:pack lbl-ast-guerreiro :anchor :w :padx 40 :pady 10)
				(ltk:pack txt-ast-guerreiro :padx 40)
				(ltk:pack lbl-ast-mago :anchor :w :padx 40 :pady 10)
				(ltk:pack txt-ast-mago :padx 40 :pady 15)

				;; Usa o ltk:after para garantir estabilidade da janela ao abrir
				(ltk:after 100 atualizar-interface-ast)))))