cnblogs
=======

管理博客园博客的Emacs扩展


本扩展提供一个用来管理博客的cnblogs-minor-mode副模式。可以用来发布org文件或者html文件。
  

Table of Contents
=================
1 使用方法
    1.1 配置
    1.2 使用


1 使用方法 
===========

1.1 配置 
---------
   下载之后将其中的el文件放到一个目录中（如~/.emacs.d/misc/），然后将这个目录加入到load-path中：


  (add-to-list 'load-path
               "~/.emacs.d/misc/")

  然后再：


  (require 'cnblogs)


1.2 使用 
---------
   先设置博客M-x cnblogs-setup-blog，其中blog ID就是博客地址中的那个博客名称（如我的就是Open_Source）。之后询问是否将网上的博客同步到本地，如果博客较多，可能需要很长时间。然后可以M-x cnblogs-import-file或者cnblogs-import-folder将本地的博客文件（即你的org源文件或者html源文件）导入，导入过程中遇到同名的博客会问是否将这个源文件与网络上的博客关联起来。

   启动cnblogs副模式可以M-x cnblogs-minor-mode，也可以自动启动：


  (add-hook 'org-mode-hook (lambda ()
                             (cnblogs-minor-mode)))

   
