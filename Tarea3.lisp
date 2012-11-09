;Programa para revisar si existen documentos tipo mp3

;Variables globales del programa;
(defvar *Directorio*)	;Variable global para el directorio
(defvar *Archivos*)		;Variable global para la carga de archivos
(defvar *Elemento*)		;Variable global que contendra el metadato
(defvar *Archtemp*)		;Variable global que contendra la información de un archio temporal
(defvar *Archbase*)		;Variable global que contendra la información de un archio de respaldo
(defvar *Contador*)		;contador para el registro de la base de datos
(defvar *Opcion*)		;Variable global de opción para consultar o se carga info
(defvar *Objetivo*)		;Variable por la que se va a buscar una cancion
(defparameter *Tablahash* (make-hash-table)) 									;Creación de la tabla hash
(defparameter *Vector-MT* (make-array 1 :fill-pointer 0 :adjustable t))			;Vector pque contendra todas las tablas hash
(defparameter *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))	;Vector pque contendra los resultados de busqueda

;;Asignaciones de variables globales
(setf *Contador* 1)		;asigna al contador 1
(setf *Archivos* nil)	;sele asigna nulo por defecto

;Funcion que presenta el programa
;Es como una introducción
(defun presentacion ()
	(format t "Bienvenido.~%")
	(format t "Esta es la Tarea Programada #3~%")
	(format t "Integrantes:~%")
	(format t "Estefany Quesada Montero~%")
	(format t "Esteban Aguilar Varverde~%")
	(format t "Esteban Mora Soto~%")
	(format t "------------------------------~%")
	(charge-database)
)

;Funcion que escribe en la base de datos
;no borra elementos solo genera un append
(defun write-database()
	(with-open-file (stream "basededatos.txt" :direction :output :if-exists :append)
		(format stream *Elemento*)
	)
)

;Funcion que lee de la base de datos
;carga datos en la tabla hash
;carga en grupos de 5 elementos
(defun charge-database()
	(setf *Contador* 1)
	(with-open-file (stream "basededatos.txt")
	(do ((line (read-line stream nil)
			   (read-line stream nil)))
		((null line))
	(setf *Elemento* line)	
	(cond 
		((eq *Contador* 1)
		(setf (gethash 'Nombre *Tablahash*) *Elemento*)
		(setf *Contador* (+ *Contador* 1)))
		((eq *Contador* 2)
		(setf (gethash 'Albun *Tablahash*) *Elemento*)
		(setf *Contador* (+ *Contador* 1)))
		((eq *Contador* 3)
		(setf (gethash 'Year *Tablahash*) *Elemento*)
		(setf *Contador* (+ *Contador* 1)))
		((eq *Contador* 4)
		(setf (gethash 'Genero *Tablahash*) *Elemento*)
		(setf *Contador* (+ *Contador* 1)))
		((eq *Contador* 5)
		(setf (gethash 'Artista *Tablahash*) *Elemento*)
		(setf *Contador* (+ *Contador* 1)))
		((eq *Contador* 6)
		(vector-push-extend *Tablahash* *Vector-MT*)
		(setf *Contador* 1))
	)
	);cierre del do
	);cierre del with-open-file
	(desicion)
);fin de defun

;Funcion que le permite decidir
;al usuario si cargar mp3 o consultar
;despues de cargada la base de datos
(defun desicion ()
	(format t "Desea hacer una consulta o desea cargar datos de un directorio~%")
	(format t "Presione 1 para consultar u otro número o letra para cargar~%")
	(format t "-----------------------------------~%")
	(setf *Opcion* (read-line))
	(cond
		((equal *Opcion* "1")
			(buscador)
		)
		(t
			(change-directory)
		)
	)
)

;Funcion que cambia de directorio
;el sistema pide un directorio
;si ya esta en uno se retorna al inicio
;si no existe el directorio cierra el programa
(defun change-directory ()
	(format t "Por favor escriba el nombre del directorio.~%")
	(format t "-------------------------------------------~%")
	(format t "Ejemplo: Mis Documentos/~%")
	(setf *Directorio* (concatenate 'string "~/" (read-line)))
	(format t "Cambiando al directorio:   ~a~%" *Directorio*)
	(format t "--------------------------------------------~%")
	(cd "/home/esteban/")
	(cd *Directorio*)
	(format t "El directorio ha sido cambiado correctamente~%")
	(format t "--------------------------------------------~%")
	(setf *Archios* (directory "~/*.mp3"))
	(cond 
		((not (null *Archivos*)) 
			(format t "Archivos mp3 cargado exitosamente.~%")
			(format t "----------------------------- ----~%")
			(sent)
		)
		(t 
			(format t "Los archivos mp3 no existen en este directorio~%")
			(format t "----------------------------- ---- -----------~%")
			(change-directory)
		)
	);fin del cond
);fin del defun

;Funcion que recorre la variable con los nombres de los archivos ".mp3"
;toma cada nombre y lo envia a la función que leea los metadatos
(defun sent ()
	(dotimes (i (list-length *Archivos*))
		(read-metadata (namestring (ELT *Archivos* i))))
)

;Funcion que carga los metadatos
;carga el metadato y lo coloca en un archivo temporal
;escribe el metadato del archivo temporal
;carga el archivo y carga el metadato y lo coloca en la tabla hash
(defun read-metadata (nombarch)
	(run-shell-command (concatenate 'string "mp3info -p ""%f"" " nombarch):output "temp.txt")
	(setf *Archtemp* (open "temp.txt"))
	(setf *Elemento* (read-line *Archtemp*))
	(setf (gethash 'Nombre *Tablahash*) *Elemento*)
	(write-database)
	(close *Archtemp*)
	;----------------------------------------------------------------------------------------;
	(run-shell-command (concatenate 'string "mp3info -p ""%l"" " nombarch):output "temp.txt")
	(setf *Archtemp* (open "temp.txt"))
	(setf *Elemento* (read-line *Archtemp*))
	(setf (gethash 'Albun *Tablahash*) *Elemento*)
	(write-database)
	(close *Archtemp*)
	;----------------------------------------------------------------------------------------;
	(run-shell-command (concatenate 'string "mp3info -p ""%y"" " nombarch):output "temp.txt")
	(setf *Archtemp* (open "temp.txt"))
	(setf *Elemento* (read-line *Archtemp*))
	(setf (gethash 'Year *Tablahash*) *Elemento*)
	(write-database)
	(close *Archtemp*)
	;----------------------------------------------------------------------------------------;
	(run-shell-command (concatenate 'string "mp3info -p ""%g"" " nombarch):output "temp.txt")
	(setf *Archtemp* (open "temp.txt"))
	(setf *Elemento* (read-line *Archtemp*))
	(setf (gethash 'Genero *Tablahash*) *Elemento*)
	(write-database)
	(close *Archtemp*)
	;----------------------------------------------------------------------------------------;
	(run-shell-command (concatenate 'string "mp3info -p ""%a"" " nombarch):output "temp.txt")
	(setf *Archtemp* (open "temp.txt"))
	(setf *Elemento* (read-line *Archtemp*))
	(setf (gethash 'Artista *Tablahash*) *Elemento*)
	(write-database)
	(close *Archtemp*)
	(vector-push-extend *Tablahash* *Vector-MT*)
	(format t "Tabla hash cargada correctamente~%")
	(desicion)
)

;Iniciador de buscador
;define por que parametro desea buscar el usuario
;al finalizar la busqueda lo deolvera a la función desicion
(defun buscador ()
	(format t "Por favor descida por cual parametro desea buscar:~%")
	(format t "1 Para Nombre de la canción~%")
	(format t "2 Para Albun de la canción~%")
	(format t "3 Para Year de la canción~%")
	(format t "4 Para Genero de la canción~%")
	(format t "5 Para Artista de la canción~%")
	(format t "6 Para todas las canciones~%")
	(format t "----------------------------------------------------~%")
	(setf *Opcion* (read-line))
	(cond
		((eq *Opcion* 1)
		(sek-nombre))
		((eq *Opcion* 2)
		(sek-albun))
		((eq *Opcion* 3)
		(sek-year))
		((eq *Opcion* 4)
		(sek-genero))
		((eq *Opcion* 5)
		(sek-artista))
		((eq *Opcion* 6)
		(format t "Aqui cambia a consulta por todo~%"))
	)
)

;----------------------------------------------------------------------;
;Las siguientes funciones hacen lo mismo
;piden el objetivo a buscar e inician la busqueda
(defun sek-nombre ()
	(format t "Por favor introdusca el nombre de la canción~%")
	(setf *Objetivo* (read-line))
	(busca-nombre)
)

(defun sek-artista ()
	(format t "Por favor introdusca el nombre del artista~%")
	(setf *Objetivo* (read-line))
	(busca-artista)
)

(defun sek-albun ()
	(format t "Por favor introdusca el nombre del albun~%")
	(setf *Objetivo* (read-line))
	(busca-albun)
)

(defun sek-year ()
	(format t "Por favor introdusca el año~%")
	(setf *Objetivo* (read-line))
	(busca-year)
)

(defun sek-genero ()
	(format t "Por favor introdusca el genero~%")
	(setf *Objetivo* (read-line))
	(busca-genero)
)
;---------------------------------------------------------------------;


;Busqueda por el nombre
(defun busca-nombre()
	;limpieza el vector de resultados para iniciar la busqueda
	(setf *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *Vector-MT*))
		(when (String-Equal (gethash 'Nombre (ELT *Vector-MT* i)) *Objetivo*)
		(vector-push-extend (ELT *Vector-MT* i) *Vector-resultado*))
	)
)

;Busqueda por el artista
(defun busca-artista()
	;limpieza el vector de resultados para iniciar la busqueda
	(setf *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *Vector-MT*))
		(when (String-Equal (gethash 'Artista (ELT *Vector-MT* i)) *Objetivo*)
		(vector-push-extend (ELT *Vector-MT* i) *Vector-resultado*))
	)
	(result)
)

;Busqueda por el año
(defun busca-year()
	;limpieza el vector de resultados para iniciar la busqueda
	(setf *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *Vector-MT*))
		(when (String-Equal (gethash 'Year (ELT *Vector-MT* i)) *Objetivo*)
		(vector-push-extend (ELT *Vector-MT* i) *Vector-resultado*))
	)
	(result)
)

;Busqueda por el albun
(defun busca-albun()
	;limpieza el vector de resultados para iniciar la busqueda
	(setf *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *Vector-MT*))
		(when (String-Equal (gethash 'Albun (ELT *Vector-MT* i)) *Objetivo*)
		(vector-push-extend (ELT *Vector-MT* i) *Vector-resultado*))
	)
	(result)
)

;Busqueda por el genero
(defun busca-genero()
	;limpieza el vector de resultados para iniciar la busqueda
	(setf *Vector-resultado* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *Vector-MT*))
		(when (String-Equal (gethash 'Genero (ELT *Vector-MT* i)) *Objetivo*)
		(vector-push-extend (ELT *Vector-MT* i) *Vector-resultado*))
	)
	(result)
)

;Funciónque presenta la información
;de resultado de la busqueda especifica
(defun result ()
	(dotimes (i (length *Vector-resultado*))
	(format t "~s - ~s - ~s - ~s - ~s" 
	(gethash 'Nombre (ELT *Vector-resultado* i))
	(gethash 'Artista (ELT *Vector-resultado* i))
	(gethash 'Albun (ELT *Vector-resultado* i))
	(gethash 'Genero (ELT *Vector-resultado* i))
	(gethash 'Year(ELT *Vector-resultado* i)))
	)
)

;Funciónque presenta la información
;de resultado de la busqueda especifica
(defun result ()
	(dotimes (i (length *Vector-resultado*))
	(format t "~s - ~s - ~s - ~s - ~s" 
	(gethash 'Nombre (ELT *Vector-resultado* i))
	(gethash 'Artista (ELT *Vector-resultado* i))
	(gethash 'Albun (ELT *Vector-resultado* i))
	(gethash 'Genero (ELT *Vector-resultado* i)))
	)
)

;Llamada a la presentacion para iniciar el programa
(presentacion)
