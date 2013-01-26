;;todo检查所有的底层函数，正常运行返回t，否则返回nil
;;给所有的操作加上反馈信息
;;; entry type
;;
;; (id   title   postid      categories src-file state)
;; (int  string  int(string) list       string   string)

(require 'metaweblog)

(defgroup cnblogs nil
  "博客园客户端分组"
  :group 'emacs)

(defun cnblogs-define-variables ()
  "定义及初始化各变量"
  (defcustom cnblogs-server-url nil
    "MetaWeblog访问地址"
    :group 'cnblogs
    :type 'string)
  (defcustom cnblogs-blog-id nil
    "博客ID"
    :group 'cnblogs
    :type 'string)
  (defcustom cnblogs-user-name nil
    "登录用户名"
    :group 'cnblogs
    :type 'string)
  (defcustom cnblogs-user-passwd nil
    "用户密码"
    :group 'cnblogs
    :type 'string)
  (defcustom cnblogs-media-object-suffix-list '("jpg" "jpeg" "png" "gif" "mp4")
    "希望处理的媒体文件类型"
    :group 'cnblogs
    :type 'list)
  (defcustom cnblogs-src-file-extension-list '("org" "html")
    "希望处理的媒体文件类型"
    :group 'cnblogs
    :type 'list)
  (defcustom cnblogs-template-head
    "#TITLE:    \n#KEYWORDS: \n#DATE:    \n"
    "博客头模板"
    :group 'cnblogs)
  :type 'list
  (defcustom cnblogs-file-root-path "~/.Cnblogs/"
    "数据文件的根目录"
    :group 'cnblogs
    :type 'string)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar cnblogs-posts-in-category nil
    "分类之后的博文，这是显示在Cnblogs-Manager缓冲区里的主要内容")
  (defvar cnblogs-entry-list-file
    (concat cnblogs-file-root-path "entry-list-file")
    "博文项列表文件")
  (defvar cnblogs-file-post-path
    (concat cnblogs-file-root-path "posts/")
    "博文内容文件根目录，其中的博文内容文件以博文ｉｄ命名")
  (defvar cnblogs-category-list nil
    "博文分类列表")
  (defvar cnblogs-category-list-file 
    (concat cnblogs-file-root-path "category-list-file")
    "博文分类列表")
  (defvar cnblogs-blog-info nil
    "博客信息")
  (defvar cnblogs-entry-list nil
    "本地博客列表")
  (defvar cnblogs-category-list nil
    "分类列表")
  (defvar cnblogs-post-list-window nil
    "博文列表窗口")
  (setq  test-post  `(("title" . "博文题目") 
		      ("dateCreated" :datetime (20423 52590))
		      ("categories"  "categories" "[随笔分类]Emacs" "[随笔分类]Linux应用")
		      ("description" . "博文正文。")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Menu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar cnblogs-mode-map
    (make-sparse-keymap "Cnblogs")
    "cnblogs博客客户端菜单")

  (define-key cnblogs-mode-map [tags-getUsersBlogs]
    '(menu-item "User information" cnblogs-get-users-blogs
		:help "获取用户的博客信息"))

  (define-key cnblogs-mode-map [tags-getRecentPosts]
    '(menu-item "Get recent posts" cnblogs-get-recent-posts
		:help "获取最近发布的N篇博客"))

  (define-key cnblogs-mode-map [tags-getCategories]
    '(menu-item "Get(Update) categories" cnblogs-get-categories
		:help "获取并更新本地博客分类"))

  (define-key cnblogs-mode-map [tags-getPost]
    '(menu-item "Get post" cnblogs-get-post
		:help "获取并更新本地指定的博客"))
  (define-key cnblogs-mode-map [separator-cnblogs-tags]
    '(menu-item "--"))

  (define-key cnblogs-mode-map [tags-editPost]
    '(menu-item "Update post" cnblogs-edit-post
		:help "更新已发布的博客"))

  (define-key cnblogs-mode-map [tags-deletePost]
    '(menu-item "Delete post" cnblogs-delete-post
		:help "将当前缓冲区对应的博客删除"))

  (define-key cnblogs-mode-map [tags-saveDraft]
    '(menu-item "Save draft" cnblogs-save-draft
		:help "将草稿保存到服务器，但状态为“未发布”"))

  (define-key cnblogs-mode-map [tags-newPost]
    '(menu-item "Publish post" cnblogs-new-post
		:help "发布当前缓冲区"))

  (define-key cnblogs-mode-map [C-S-mouse-1]
    cnblogs-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;KeyMap;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-key cnblogs-mode-map (kbd "\C-c c p") 'cnblogs-new-post)
  (define-key cnblogs-mode-map (kbd "\C-c c s") 'cnblogs-save-draft)
  (define-key cnblogs-mode-map (kbd "\C-c c d") 'cnblogs-delete-post)
  (define-key cnblogs-mode-map (kbd "\C-c c e") 'cnblogs-edit-post)
  (define-key cnblogs-mode-map (kbd "\C-c c g") 'cnblogs-get-post)
  (define-key cnblogs-mode-map (kbd "\C-c c c") 'cnblogs-get-categories)
  (define-key cnblogs-mode-map (kbd "\C-c c r") 'cnblogs-get-recent-posts)
  (define-key cnblogs-mode-map (kbd "\C-c c u") 'cnblogs-get-users-blogs)
  )

					;(cnblogs-define-variables)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LoadData;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnblogs-load-variables ()
  "加载各变量的值";
					;加载博文项列表
  (cnblogs-load-entry-list)
					;加载博文分类
  (cnblogs-load-category-list)
					;将博文项列表中的项加入到相应的分类中去
  (mapc (lambda (categorie)
	  (progn
					;先将该分类加入
	    (push (cons categorie nil)
		  cnblogs-posts-in-category)
	    )
	  
					;将属于该分类的项加入该分类
	  (mapc (lambda (entry)
		  (let* ((entry-categories (nth 3 entry))
			 (flag (member categorie entry-categories)))
		    (and flag
			 (push entry
			       (cdr (assoc categorie cnblogs-posts-in-category)))))
		  )
		
		cnblogs-entry-list))

	cnblogs-category-list)
  )

					;(cnblogs-load-entry-list) ;; todo: 放在初始化中

(defun cnblogs-load-entry-list ()
  (setq cnblogs-entry-list
	(condition-case ()
	    (with-temp-buffer
	      (insert-file-contents cnblogs-entry-list-file)
	      (car (read-from-string (buffer-string))))
	  (error nil))))

(defun cnblogs-save-entry-list () 
  "保存cnblogs-entry-list，成功返回t，否则返回nil"
  (condition-case ()
      (with-temp-file cnblogs-entry-list-file
	(print cnblogs-entry-list
	       (current-buffer)))
    (error nil)))


(defun cnblogs-load-category-list ()
  (setq cnblogs-category-list
	(condition-case ()
	    (with-temp-buffer
	      (insert-file-contents cnblogs-category-list-file)
	      (car (read-from-string (buffer-string))))
	  (error nil))))

(defun cnblogs-save-category-list ()
					;先将分类格式简化，只留取名字
  (setq cnblogs-category-list
	(mapcar (lambda (category)
		  (cdr (assoc "description" category))
		  )
		cnblogs-category-list))
  (with-temp-file cnblogs-category-list-file
    (print cnblogs-category-list
	   (current-buffer))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;底层函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnblogs-check-legal-for-publish (src-file)
  "检查文件是否可以发布"
  (and
   (if (member (file-name-extension src-file)
	       cnblogs-src-file-extension-list)
       t
     (progn
       (message "Failed: UNSUPPORTED file!")
       nil))

   (if (equal (cnblogs-check-src-file-state (buffer-file-name))
	      "PUBLISHED")
       (progn
	 (message "This post has been published, you can update it, using M-x cnblogs-edit-post")
	 nil)
     t)))

(defun cnblogs-check-legal-for-delete (src-file)
  "检查文件是否可以删除相应的博文"
  (and 
   (if (member (file-name-extension src-file)
	       cnblogs-src-file-extension-list)
       t
     (progn
       (message "Failed: UNSUPPORTED file!")
       nil))
   (if (equal (cnblogs-check-src-file-state (buffer-file-name))
	      "PUBLISHED")
       t
     (progn
       (message "This post has not been published, so you cann't delete it!")
       nil))))

(defun cnblogs-check-legal-for-edit (src-file)
  "检查文件是否可以更新"
  (and
   (if (member (file-name-extension src-file)
	       cnblogs-src-file-extension-list)
       t
     (progn
       (message "Failed: UNSUPPORTED file!")
       nil))

   (if (equal (cnblogs-check-src-file-state (buffer-file-name))
	      "PUBLISHED")
       t
     (progn
       (message "This post has not been published, you can't update it. You can publish it using M-x cnblogs-new-post")
       nil))))

(defun cnblogs-check-src-file-state (src-file)
  (let ((state nil))
    (mapc (lambda (entry)
	    (if (equal src-file (nth 4 entry))
		(setq state (nth 5 entry))))
	  cnblogs-entry-list)
    state))

(defun cnblogs-gen-id ()
  "给entry产生一个id，从１开始"
  (let ((id 0)
	(flag t))
    (while flag
      (progn
	(setq flag nil id (1+ id))

	(mapc (lambda (entry)
		(and (equal id 
			    (car entry))
		     (setq flag t)))
	      cnblogs-entry-list)))
    id))


(defun cnblogs-push-post-to-entry-list (post)
  "将博文保存到cnblogs-entry-list变量中。但并不立即保存到文件中"

  (let ((title (cdr (assoc "title" post)))
	(postid (cdr (assoc "postid" post)))
	(categories (cdr (assoc "categories" post)))
	(done nil)
	(index 0))
    (progn 
      (if (integerp postid)
	  (setq postid (int-to-string postid)))

      ;;保存博文
      (with-temp-file (concat cnblogs-file-post-path postid)
	(print post (current-buffer)))
      ;;如果有相同标题的博文项，则提示是否合并到同一项中去，如果有已经存在多个相同的项，对每个都询问，直到回答是或者完
      (mapc (lambda (entry)
	      (progn
		(or done
		    (not (equal title (nth 1 entry)))
		    (not (y-or-n-p (format "merge the post %s with entry %S" postid entry)))
					;下面是将该博文合并到该项中
		    (progn 
		      (setq done t)
		      (setcar (nthcdr index cnblogs-entry-list)
					;id
			      (list (nth 0 entry)
					;title
				    title
					;postid
				    postid
					;categories
				    categories
					;src-file
				    (nth 4 entry)
					;state
				    "PUBLISHED"))))
		(setq index (1+ index))))
	    cnblogs-entry-list)

					;还没有插入则新建项
      (or done
	  (push 
					;id
	   (list (cnblogs-gen-id)        
					;title
		 title
					;postid
		 postid
					;categories
		 categories
					;src-file
		 nil
					;state
		 "PUBLISHED")

	   cnblogs-entry-list))))) 

(defun cnblogs-push-src-file-to-entry-list (src-file)
  "将一个源文件加入到博文项中，但并不立即保存博文项到文件中。"
  (if (cnblogs-check-file-in-entry-list src-file)
      t
    (let ((title 
	   (with-temp-buffer
	     (insert-file-contents src-file)
	     (cnblogs-fetch-field "TITLE")))
	  (done nil)
	  (index 0))
      (progn (mapc (lambda (entry)
		     (progn
		       (or done
			   (not title)
			   (not (equal title (nth 1 entry)))
			   (not (y-or-n-p (format "merge the file %s with entry %S" src-file entry)))
					;下面是将该文件合并到该项中
			   (progn 
			     (setq done t)
			     (setcar (nthcdr 4 (nth index cnblogs-entry-list)) 
				     src-file)))
		       (setq index (1+ index))))
		   cnblogs-entry-list)

					;还没有插入则新建项
	     (or done
		 (push 
					;id
		  (list (cnblogs-gen-id)        
					;title
			title
					;postid
			nil
					;categories
			nil
					;src-file
			src-file
					;state
			"UNPUBLISHED")

		  cnblogs-entry-list))))))

(defun cnblogs-assign-post-to-file (post src-file)
  "将post合并到一个指定源文件的列表项中，成功返回t，不立即保存列表项"
  (condition-case()
      (progn
	(setq cnblogs-entry-list
	      (mapcar (lambda (entry)
			(if (equal src-file
				   (nth 4 entry))
					;id
			    (list (nth 0 entry)
					;title
				  (cdr (assoc "title" post))
					;postid
				  (cdr (assoc "postid" post))
					;categories
				  (cdr (assoc "categories" post))
					;src-file
				  src-file
				  "PUBLISHED")
			  entry))
		      cnblogs-entry-list))
	t)
    (error nil)))


(defun cnblogs-categories-string-to-list (categories-string)
  "将分类字符串按空白符分成字符串列表"
  (if (or (eq categories-string nil)
	  (eq categories-string ""))
      nil
    (let ((idx1
	   (string-match "[^　 \t]+"    ;圆角半角空格
			 categories-string)))
      (if (not idx1)
	  nil
	(setq categories-string         ;圆角半角空格
	      (substring categories-string idx1))
	(let ((idx2 
	       (string-match "[　 \t]+"
			     categories-string)))
	  (if idx2
	      (cons (concat "[随笔分类]"
			    (substring categories-string 
				       0
				       idx2))
		    (cnblogs-categories-string-to-list (substring categories-string
								  idx2)))
	    (cons (concat "[随笔分类]"
			  categories-string)
		  nil)))))))


(defun cnblogs-fetch-field (field)
  (let* ((regexp
	  (concat "^[ \t]*[#]+[\\+]?"
		  field
		  ":[^\n]*"))
	 (idx (string-match regexp 
			    (buffer-substring-no-properties (point-min)
							    (point-max)))))
    (if idx
	(let* ((field-val (match-string  0 
					 (buffer-substring-no-properties (point-min) 
									 (point-max))))
	       (val (substring field-val
			       (1+ (string-match  ":"  field-val))))
	       (idx2 (string-match "[^ \t]+"
				   val)))
	  (and idx2
	       (substring val
			  idx2)))
      nil)))

(defun cnblogs-make-media-object-file-data (media-path) ;todo: type可能要详细分类
  "根据给出的文件路径返回相应的FileData，文件不存在返回nil"
  (and (file-exists-p media-path)
       (list
	;;media-path name
	(cons "name" 
	      (file-name-nondirectory media-path))

	;; bits
	(cons "bits"
	      (base64-encode-string
	       (with-temp-buffer
		 (insert-file-contents-literally media-path)
		 (buffer-string))))
	(cons "type" "image/jpg"))))


(defun cnblogs-org-mode-buffer-to-post ()
  (delq nil(list
	    ;; title
	    (cons "title"
		  (or (cnblogs-fetch-field "TITLE")
		      "新随笔"))

	    ;; excerpt
	    (cons "mt_excerpt"
		  (or (cnblogs-fetch-field "DESCRIPTION")
		      ""))
	    
	    ;; categories
	    (cons "categories"
		  (let ((categories-list
			 (cnblogs-categories-string-to-list
			  (cnblogs-fetch-field "CATEGORIES"))))
		    (or
		     categories-list
		     '("[随笔分类]未分类"))))
	    ;; tags
	    (cons "mt_keywords"
		  (or 
		   (cnblogs-fetch-field "KEYWORDS")
		   ""))

	    ;; dateCreated
	    (cons "dateCreated"
		  (list 
		   :datetime
		   (condition-case ()
		       (date-to-time (cnblogs-fetch-field "DATE")) ;todo: 要转化
		     (error (progn
			      (message "时间格式不支持，使用默认时间:1989-05-17 00:00")
			      (date-to-time "1989-05-17 00:00"))))))

	    ;; description
	    (cons "description"
		  (with-current-buffer (org-export-as-html 3 nil nil "*Org HTML Export*")
		    (let ((buf-str 
			   (cnblogs-replace-media-object-location
			    (buffer-substring-no-properties 
			     (point-min)
			     (point-max)))))
		      (kill-buffer)
		      buf-str))))))


(defun cnblogs-other-mode-buffer-to-post () ;todo: post还不完全
  (delq nil
	(list
	 ;; title
	 (cons "title"
	       (or (cnblogs-fetch-field "TITLE")
		   "新随笔"))


	 ;; categories
	 (cons "categories"
	       (let ((categories-list
		      (cnblogs-categories-string-to-list
		       (cnblogs-fetch-field "CATEGORIES"))))
		 (or
		  categories-list
		  '("[随笔分类]未分类"))))

	 ;; tags
	 (cons "mt_keywords"
	       (or
		(cnblogs-fetch-field "KEYWORDS")
		""))

	 ;; dateCreated
	 (cons "dateCreated"
	       (list 
		:datetime
		(condition-case ()
		    (date-to-time (cnblogs-fetch-field "DATE")) ;todo: 要转化
		  (error (progn
			   (message "时间格式不支持，使用默认时间:1989-05-17 00:00")
			   (date-to-time "1989-05-17 00:00"))))))
	 ;; description
	 (cons "description"
	       (cnblogs-replace-media-object-location
		(buffer-substring-no-properties
		 (cnblogs-point-template-head-end)
		 (point-max)))))))


(defun cnblogs-insert-template-head ()
  "插入头模板"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert cnblogs-template-head)))


(defun cnblogs-delete-entry-from-entry-list (postid) 
  "通过postid删除博文项及posts目录下相应的文件，POSTID是string类型"
  (condition-case ()
      (progn
	(setq cnblogs-entry-list
	      (remove-if (lambda (entry)
			   (equal postid
				  (nth 2 entry)))
			 cnblogs-entry-list))
	(cnblogs-save-entry-list)
	(and (file-exists-p (concat cnblogs-file-post-path postid))
	     (delete-file (concat cnblogs-file-post-path postid)))
	t)
    (error nil)))


(defun cnblogs-get-postid-by-title (title)
  (and (stringp title)
       (let ((postid nil))
	 (mapc (lambda (entry)
		 (or postid
		     (and (equal title
				 (nth 4 cnblogs-entry-list))
			  (setq postid 
				(nth 2 cnblogs-entry-list)))))
	       cnblogs-entry-list)
	 (and postid
	      (integerp postid)
	      (int-to-string postid))
	 (or postid
	     (setq postid "0"))))
  postid)


(defun cnblogs-get-postid-by-src-file-name (filename)
  "在cnblogs-entry-list中查找src-file为filename的项的博文id，找不到返回\"0\""
  (let ((postid nil))
    (mapc (lambda (entry)
	    (if (equal filename (nth 4 entry))
		(setq postid (nth 2 entry))))
	  cnblogs-entry-list)
    (or postid
	(setq postid "0"))
    postid))


(defun cnblogs-replace-media-object-location (buf-str)
  "处理BUF-STR中的媒体文件，返回处理后的字符串"
  (mapc (lambda (suffix)
	  (let ((regexp 
		 (concat "[file]*[:]?[/\\]*[a-z]?[:]?[^:*\"?<>|#]+."
			 suffix))
		(current 0))
	    (while (string-match regexp
				 buf-str
				 current)
	      (let* ((media-path (match-string 0
					       buf-str))
		     (media-url
		      (save-match-data
			(and (file-exists-p media-path)
			     (cnblogs-metaweblog-new-media-object 
			      (cnblogs-make-media-object-file-data
			       media-path))))))

		(if media-url
		    (progn
		      (setq current
			    (+ (match-beginning 0)
			       (length media-url)))
		      (setq buf-str
			    (replace-match media-url
					   t
					   t
					   buf-str)))
		  (setq current
			(match-end 0)))))))
	cnblogs-media-object-suffix-list)
  buf-str)

(defun cnblogs-point-template-head-end ()
  (print  (save-excursion
	    (goto-char (point-min))
	    (forward-paragraph)
	    (point))))


(defun cnblogs-current-buffer-to-post ()
  (cond
   ((equal mode-name
	   "Org")
    (cnblogs-org-mode-buffer-to-post))

   (t
    (cnblogs-other-mode-buffer-to-post))))


(defun cnblogs-check-file-in-entry-list (src-file)
  "检查文件是否已经在列表项中"
  (let ((res nil))
    (mapc (lambda (entry)
	    (or res
		(setq res 
		      (equal src-file (nth 4 entry)))))
	  cnblogs-entry-list)
    res))



(defun cnblogs-delete-post-from-entry-list (postid) 
  "通过postid将相应的entry的postid设置为nil并删除posts目录下相应的文件，成功返回t.POSTID是string类型或者int类型"
  (if (integerp postid)
      (setq postid (int-to-string postid)))

  (condition-case ()
      (progn
	(setq cnblogs-entry-list
	      (mapcar (lambda (entry)
			(if (equal postid 
				   (if (integerp (nth 2 entry))
				       (int-to-string (nth 2 entry))
				     (nth 2 entry)))
			    (progn
			      (setcar (nthcdr 2 entry) nil)
			      (setcar (nthcdr 3 entry) nil)
			      (setcar (nthcdr 5 entry) "UNPUBLISHED")))
			entry)
		      cnblogs-entry-list))
	(cnblogs-save-entry-list)
	(and (file-exists-p (concat cnblogs-file-post-path postid))
	     (delete-file (concat cnblogs-file-post-path postid)))
	t)
    (error nil)))

(defun cnblogs-import-directory (directory)
  ;; 滤掉所有以.开头的文件，这样就没有了..和.以及所有的隐藏文件
  ;; 滤掉所有以~结尾的文件，这样就没有了自动备份

  (let ((files (directory-files directory t "^[^\.].*[^~]$" t)))
    (mapc (lambda (file)
					;目录
	    (cond ((file-directory-p file)
		   (cnblogs-import-directory file))
					;合法文件
		  ((member (file-name-extension file) cnblogs-src-file-extension-list)
		   (cnblogs-push-src-file-to-entry-list file))))
	  files)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;功能函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnblogs-import-current-file ()
  "将当前文件加入到库中（增加到博文项cnblogs-entry-list中）"
  (interactive)
  (let ((src-file (buffer-file-name)))
    (if (member (file-name-extension src-file)
		cnblogs-src-file-extension-list)
	(progn 
	  (cnblogs-push-src-file-to-entry-list src-file)
	  (cnblogs-save-entry-list)
	  (message "Succeed!"))
      (message "Failed: UNSUPPORTED file!"))))

(defun cnblogs-import-file ()
  "添加一个文件加入到库中（增加到博文项cnblogs-entry-list中）"
  (interactive)
  (let ((src-file (read-file-name "Import file: ")))
    (if (member (file-name-extension src-file)
		cnblogs-src-file-extension-list)
	(progn 
	  (cnblogs-push-src-file-to-entry-list src-file)
	  (cnblogs-save-entry-list)
	  (message "Succeed!"))
      (message "Failed: UNSUPPORTED file!"))))


(defun cnblogs-import-folder ()
  "递归添加一个目录中的所有合法文件到库中，这个是给用户用的，主要是调用cnblogs-import-directory"
  (interactive)
  (let ((directory (read-directory-name "Import folder: ")))
    (cnblogs-import-directory directory)
    (cnblogs-save-entry-list)))

(defun cnblogs-setup-blog ()
  (interactive)
  (setq cnblogs-blog-id
	(read-string "Your blog ID:" nil nil))
  (setq cnblogs-user-name
	(read-string "Your username:" nil nil))
  (setq cnblogs-user-passwd
	(read-passwd "Your password:" nil ))
  (setq cnblogs-server-url
	(concat "http://www.cnblogs.com/"
		cnblogs-blog-id
		"/services/metaweblog.aspx"))
  (setq cnblogs-category-list
	(cnblogs-metaweblog-get-categories))
  (setq cnblogs-blog-info
	(cnblogs-metaweblog-get-users-blogs))
  (if cnblogs-blog-info
      (progn
	(customize-save-variable 'cnblogs-blog-id  cnblogs-blog-id)
	(customize-save-variable 'cnblogs-user-name cnblogs-user-name)
	(customize-save-variable 'cnblogs-user-passwd cnblogs-user-passwd)
	(customize-save-variable 'cnblogs-server-url cnblogs-server-url)
	;; 如果没有根目录，则新建这个目录
	(or (file-directory-p cnblogs-file-root-path)
	    (make-directory cnblogs-file-root-path))
	;; 如果没有posts目录，则新建这个目录
	(or (file-directory-p cnblogs-file-post-path)
	    (make-directory cnblogs-file-post-path))
	(cnblogs-save-category-list)
	(and (yes-or-no-p "Should I pull all your posts now, it may talk a long time?")
	     (let ((posts (cnblogs-metaweblog-get-recent-posts 0)))
	       (mapc (lambda (post)
		       (cnblogs-push-post-to-entry-list post))
		     posts))
	     (cnblogs-save-entry-list))
	(message "设置成功"))
    (message "设置失败")))


(defun cnblogs-new-post ()
  (interactive)

  (if (cnblogs-check-legal-for-publish (buffer-file-name))
      ;; 下面发布处理
      (let* ((postid  ;得到博文ｉｄ
	      (cnblogs-metaweblog-new-post (cnblogs-current-buffer-to-post)
					   t))
					;得到博文内容
	     (post
	      (cnblogs-metaweblog-get-post postid)))
	
					;todo:这里要刷新列表
	;;保存博文项和博文内容
	(if (integerp postid)
	    (setq postid (int-to-string postid)))
	;;保存博文
	(with-temp-file (concat cnblogs-file-post-path postid)
	  (print post (current-buffer)))

	(if (cnblogs-check-file-in-entry-list (buffer-file-name))
	    (cnblogs-assign-post-to-file post (buffer-file-name))
	  (push 
					;id
	   (list (cnblogs-gen-id)
					;title
		 (cdr (assoc "title" post))
					;postid
		 postid
					;categories
		 (cdr (assoc "categories" post))
		 (buffer-file-name)
		 "PUBLISHED")
	   cnblogs-entry-list))
					;保存博文项列表
	(cnblogs-save-entry-list)
	(message "Post published！"))))



(defun cnblogs-save-draft ()
  (interactive)
  (let ((postid  
	 (cnblogs-metaweblog-new-post (cnblogs-current-buffer-to-post)
				      nil)))
    (setq cnblogs-entry-list
	  (cons
	   (cnblogs-metaweblog-get-post postid)
	   cnblogs-entry-list))
    (cnblogs-save-entry-list))
  (message "保存草稿成功！"))




(defun cnblogs-delete-post ()
  (interactive)
  (if (cnblogs-check-legal-for-delete (buffer-file-name))
      (let ((postid
	     (cnblogs-get-postid-by-src-file-name (buffer-file-name))))
	(if (and postid
		 (yes-or-no-p "Are you sure?")
		 (cnblogs-metaweblog-delete-post postid t)
		 (cnblogs-delete-post-from-entry-list postid)
		 (cnblogs-save-entry-list))
	    
	    (message "Succeed！")
	  (message "Failed！")))))


(defun cnblogs-edit-post ();;todo:更新本地
  (interactive)
  (if (cnblogs-check-legal-for-edit (buffer-file-name))
      (let ((postid
	     (cnblogs-get-postid-by-src-file-name
	      (buffer-file-name))))
	(if (and postid
		 (yes-or-no-p "Are you sure to update?")
		 (cnblogs-metaweblog-edit-post postid
					       (cnblogs-current-buffer-to-post)
					       t)
		 (cnblogs-assign-post-to-file (cnblogs-metaweblog-get-post postid)
					      (buffer-file-name))
		 (cnblogs-save-entry-list))
	    
	    (message "Succeed!")
	  (message "Failed!")))))

(defun cnblogs-get-post ()
  (interactive)
  (let* ((postid
	  (read-string "Post ID："))
	 (post
	  (condition-case ()
	      (cnblogs-metaweblog-get-post postid)
	    (error nil))))
    (if (and postid
	     (cnblogs-delete-post-from-entry-list postid)
	     post
	     (setq cnblogs-entry-list
		   (cons post cnblogs-entry-list)))
	(message "获取成功！")
      (message "获取失败"))))

;; 获取并保存分类
(defun cnblogs-get-categories ()
  (interactive)
  (setq cnblogs-category-list
	(condition-case ()
	    (cnblogs-metaweblog-get-categories)
	  (error nil)))
  (if cnblogs-category-list
      (progn
	(cnblogs-save-category-list)
	(message "获取分类成功！"))
    (message "获取分类失败")))



(defun cnblogs-get-recent-posts ()
  (interactive)
  (let* ((num (read-number "输入要获取的随笔篇数："
			   1))
	 (posts (condition-case ()
		    (cnblogs-metaweblog-get-recent-posts num)
		  (error nil))))
    (if (not posts)
	(message "获取失败！")
      (progn
	(mapc (lambda (post)
		(cnblogs-push-post-to-entry-list post))
	      posts)
	(cnblogs-save-entry-list)
	(message "获取成功！")))))



(defun cnblogs-get-users-blogs ()
  (interactive)
  (setq cnblogs-blog-info
	(condition-case ()
	    (prog1
		(cnblogs-metaweblog-get-users-blogs)
	      (message "获取用户博客信息成功！"))
	  (error cnblogs-blog-info))))

(defun cnblogs-add-props (str plist)
  "将faces属性plist赋给str，并返回这个str"
  (set-text-properties 0 (length str) plist str)
  str)
;;[c][b]
(defun cnblogs-category-selection-toggle (c)
  "根据字符c查找要触发的分类，然后触发这个分类"
  (let* ((begin (string-match (concat "[" c "]" [ ]*) 
			      (buffer-substring (string-match "随笔分类" (buffer-string)) (point-max)))

		(substring (buffer-substring (point-min) (point-max)) 23728 23731 )
		))))
(defun cnblogs-category-selection ()
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (split-window-vertically)
    (org-switch-to-buffer-other-window (get-buffer-create " *Cnblogs categories*"))
    (erase-buffer)
    
    ;; 列出当前已经选择的分类
    (insert "Current:    \n")
    
    ;; 列出随笔分类
    (insert "\n\n随笔分类:    \n")
    (let* ((idx ?0)
	   (ctgr-list (remove-if-not (lambda (ctgr)
				       (equal (substring ctgr 1 5) "随笔分类"))
				     cnblogs-category-list))
	   (maxlen (apply 'max (mapcar 'length ctgr-list))))
      
      (mapc (lambda (ctgr)
	      (insert "[" idx "]" (format "%s  " (substring ctgr 6)))
	      (setq idx (1+ idx)))
	    ctgr-list))

    ;; 列出网站类分
    (insert "\n\n网站分类:    \n")
    (let* ((idx ?A)
	   (ctgr-list (remove-if-not (lambda (ctgr)
				       (equal (substring ctgr 1 5) "网站分类"))
				     cnblogs-category-list))
	   (maxlen (apply 'max (mapcar 'length ctgr-list))))
      (mapc (lambda (ctgr)
	      (insert "[" idx "]" (format "%s  " (substring ctgr 6)))
	      (setq idx (1+ idx)
		    ))
	    ctgr-list))
    (insert "\n\n其他分类:    \n")
    ;; 列出其他分类
    (mapc (lambda (ctgr)
	    (if (equal (substring ctgr 1 3) "发布")
		(insert (format "%s   " ctgr)))
	    )
	  cnblogs-category-list)
    (message "[0..9..a-z..]:Toggle [SPC]:clear [RET]:accept")
    ;; 处理分类选择
    (catch 'exit
      (while t
	(let ((c (read-char-exclusive)))
	  (cond
	   ((= c ?\r) (throw exit t))
	   (t (do nothing)
	      )
	   ))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;mode设置;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cnblogs-define-variables)		;定义变量，可以不用定义成函数
;; 下面是关于minor mode的内容
(defun cnblogs-init ()
  "Cnblogs的所有初始化工作"
  (cnblogs-load-variables)
  )

;; 定义菜单
(define-key cnblogs-mode-map [menu-bar menu-bar-cnblogs-menu]
  (cons "Cnblogs" cnblogs-mode-map))

(define-minor-mode cnblogs-minor-mode
  "cnblogs-minor-mode"
  :init-value nil
  :lighter " Cnblogs"
  :keymap cnblogs-mode-map
  :group Cnblogs)

(add-hook 'cnblogs-minor-mode-hook 'cnblogs-init) ;打开cnblogs-minor-mode时再加载数据等初始化

(provide 'cnblogs)
