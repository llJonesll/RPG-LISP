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

;; ====== 2. MÚLTIPLO DESPACHO (MULTIPLE DISPATCH) ======

(defgeneric atacar (atacante alvo))

;; CORREÇÃO: Agora o Guerreiro usa a função mutável 'calcular-dano-guerreiro' no cálculo!
(defmethod atacar ((p guerreiro) (m monstro))
	(let ((dano (calcular-dano-guerreiro)))
		(setf (hp m) (- (hp m) dano))
		(format t "~%[COMBATE] O Guerreiro ~A desferiu um golpe físico e causou ~A de dano!" (nome p) dano)))

;; Se um Mago atacar um Monstro: dano baseado na Magia
(defmethod atacar ((p mago) (m monstro))
	(let ((dano (* (magia p) 2)))
		(setf (hp m) (- (hp m) dano))
		(format t "~%[COMBATE] O Mago ~A lançou uma bola de fogo e causou ~A de dano mágico!" (nome p) dano)))

;; ====== 3. METAPROGRAMAÇÃO E ALTERAÇÃO DE REGRAS ======

;; GARANTIA: Dizer ao compilador para salvar a estrutura do código original na memória
(declaim (optimize (debug 3)))

;; Função que guarda a fórmula de dano padrão
(defun calcular-dano-guerreiro ()
	15)

(defvar *mutacao-ativa* nil)

(defun alternar-regra-caotica ()
	(if (not *mutacao-ativa*)
		(progn
			(setf *mutacao-ativa* t)
			;; Mudamos a função para retornar 45 dinamicamente
			(eval '(defun calcular-dano-guerreiro () 45)))
		(progn
			(setf *mutacao-ativa* nil)
			(eval '(defun calcular-dano-guerreiro () 15)))))

;; ====== 4. GAME LOOP (PARADIGMA IMPERATIVO) ======

(defun iniciar-jogo ()
	(let ((guerreiro (make-instance 'guerreiro :nome "Jones" :hp 100 :forca 15 :magia 0))
		(mago (make-instance 'mago :nome "Lisbon" :hp 60 :forca 5 :magia 20))
		(chefe (make-instance 'monstro :nome "Professor de Paradigmas" :hp 200)))
		
		(format t "~%=====================================")
		(format t "~%   BEM-VINDO AO LISP RPG MUTANTE   ")
		(format t "~%=====================================")
		
		(loop
			(format t "~%~%--- STATUS DO CHEFE ---")
			(format t "~%Inimigo: ~A | HP: ~A" (nome chefe) (hp chefe))
			(format t "~%Modo Caótico Ativo: ~A" *mutacao-ativa*)
			(format t "~%-----------------------")
			
			(if (<= (hp chefe) 0)
				(progn
					(format t "~%~%[VITÓRIA] Vocês derrotaram o chefe e ganharam nota 10!")
					(return))
				(progn
					(format t "~%Escolha uma ação:")
					(format t "~%1. Atacar com o Guerreiro")
					(format t "~%2. Atacar com o Mago")
					(format t "~%3. Metaprogramação: Alternar Regra do Mundo (Modificar Código)")
					(format t "~%4. Sair do Jogo")
					(format t "~%Opção: ")
					(finish-output)
					
					(let ((opcao (read)))
						(cond
							((equal opcao 1) (atacar guerreiro chefe))
							((equal opcao 2) (atacar mago chefe))
							((equal opcao 3) (alternar-regra-caotica))
							((equal opcao 4) (return))
							(t (format t "~%Opção inválida!")))))))))