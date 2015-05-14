<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: shell-pop.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=shell-pop.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: shell-pop.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=shell-pop.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for shell-pop.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=shell-pop.el" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-2101513-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97">サイトマップ</a> <a class="local" href="http://www.emacswiki.org/emacs/%e6%9b%b4%e6%96%b0%e5%b1%a5%e6%ad%b4">更新履歴</a> <a class="local" href="http://www.emacswiki.org/emacs/%e3%83%8b%e3%83%a5%e3%83%bc%e3%82%b9">ニュース</a> <a class="local" href="http://www.emacswiki.org/emacs/%ef%bc%a5%ef%bd%8c%ef%bd%89%ef%bd%93%ef%bd%90%e3%82%bb%e3%82%af%e3%82%b7%e3%83%a7%e3%83%b3">Ｅｌｉｓｐセクション</a> <a class="local" href="http://www.emacswiki.org/emacs/%e5%88%a9%e7%94%a8%e6%89%8b%e5%bc%95">利用手引</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22shell-pop.el%22">shell-pop.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="download/shell-pop.el">Download</a></p><pre class="code"><span class="linecomment">;;; shell-pop.el --- Helps you pop up and pop out shell buffer easily.</span>
<span class="linecomment">;;; $Id: shell-pop.el,v 1.10 2010/02/21 14:19:26 kyagi Exp kyagi $</span>

<span class="linecomment">;; Copyright (C) 2009 Free Software Foundation, Inc.</span>

<span class="linecomment">;; Author:        Kazuo YAGI &lt;kyagi@1liner.jp&gt;</span>
<span class="linecomment">;; Maintainer:    Kazuo YAGI &lt;kyagi@1liner.jp&gt;</span>
<span class="linecomment">;; Created:       2009-05-31</span>
<span class="linecomment">;; Last-Updated:  $Date: 2010/02/21 14:19:26 $</span>
<span class="linecomment">;; Revision:      $Revision: 1.10 $</span>
<span class="linecomment">;; Keywords:      shell, terminal, tools</span>
<span class="linecomment">;; Compatibility: GNU Emacs 23.x</span>

<span class="linecomment">;; This file is part of GNU Emacs.</span>

<span class="linecomment">;; GNU Emacs is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 2, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; GNU Emacs is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with GNU Emacs; see the file COPYING.  If not, write to the</span>
<span class="linecomment">;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,</span>
<span class="linecomment">;; Boston, MA 02110-1301, USA.</span>

<span class="linecomment">;;; Commentary;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This is a utility which helps you pop up and pop out shell buffer</span>
<span class="linecomment">;; window easily. Just do M-x shell-pop, and it is strongly recommmended</span>
<span class="linecomment">;; to assign one hot-key to this function.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Configuration;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; You can choose your favorite internal mode such as `shell', `terminal',</span>
<span class="linecomment">;; `ansi-term', and `eshell'. Also you can use any shell such as</span>
<span class="linecomment">;; `/bin/bash', `/bin/tcsh', `/bin/zsh' as you like.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; A configuration sample for your .emacs is as follows.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; (require 'shell-pop)</span>
<span class="linecomment">;; (shell-pop-set-internal-mode "ansi-term")</span>
<span class="linecomment">;; (shell-pop-set-internal-mode-shell "/bin/zsh")</span>
<span class="linecomment">;; (global-set-key [f8] 'shell-pop)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Besides, you can set the window height, the number for the percentage</span>
<span class="linecomment">;; for selected window.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; (shell-pop-set-window-height 60)</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Update Info;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; $Log: shell-pop.el,v $</span>
<span class="linecomment">;; Revision 1.10  2010/02/21 14:19:26  kyagi</span>
<span class="linecomment">;; bug fix</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Revision 1.9  2010/02/21 14:03:43  kyagi</span>
<span class="linecomment">;; bug fix</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Revision 1.8  2010/02/21 12:55:28  kyagi</span>
<span class="linecomment">;; bug fix</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Revision 1.7  2010/02/21 11:29:56  kyagi</span>
<span class="linecomment">;; add a function shell-pop-set-window-position</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Revision 1.6  2010/02/21 11:25:01  kyagi</span>
<span class="linecomment">;; add a option shell-pop-window-position</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Code:</span>
(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-window-height 30) <span class="linecomment">; percentage for shell-buffer window height</span>
(defvar shell-pop-window-position "<span class="quote">bottom</span>")

(defvar shell-pop-internal-mode "<span class="quote">shell</span>")
(defvar shell-pop-internal-mode-buffer "<span class="quote">*shell*</span>")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-internal-mode-shell "<span class="quote">/bin/bash</span>")

(defvar shell-pop-internal-mode-list
  (list
    <span class="linecomment">; mode, buffer, function</span>
    '("<span class="quote">shell</span>"     "<span class="quote">*shell*</span>"     '(lambda () (shell)))
    '("<span class="quote">terminal</span>"  "<span class="quote">*terminal*</span>"  '(lambda () (term shell-pop-internal-mode-shell)))
    '("<span class="quote">ansi-term</span>" "<span class="quote">*ansi-term*</span>" '(lambda () (ansi-term shell-pop-internal-mode-shell)))
    '("<span class="quote">eshell</span>"    "<span class="quote">*eshell*</span>"    '(lambda () (eshell)))))

(defun shell-pop-set-window-height (number)
  (interactive "<span class="quote">nInput the number for the percentage of \
selected window height (10-100): </span>")
  (setq shell-pop-window-height number))

(defun shell-pop-set-window-position (position)
  (interactive "<span class="quote">sInput the position for shell-pop (top|bottom): </span>")
  (setq shell-pop-window-position position))

(defun shell-pop-set-internal-mode (mode)
  (interactive "<span class="quote">sInput your favorite mode (shell|terminal|ansi-term|eshell): </span>")
  (if (catch 'found
        (dolist (l shell-pop-internal-mode-list)
          (if (string-match mode (car l))
              (progn
                (setq shell-pop-internal-mode-buffer (nth 1 l))
                (setq shell-pop-internal-mode-func (nth 2 l))
                (throw 'found t)))))
      t
    nil))

(defun shell-pop-set-internal-mode-shell (shell)
  (interactive (list (read-from-minibuffer "<span class="quote">Input your favorite shell:</span>"
                                           shell-pop-internal-mode-shell)))
  (setq shell-pop-internal-mode-shell shell))

(defun shell-pop ()
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-up ()
  (let ((w (get-buffer-window shell-pop-internal-mode-buffer)))
    (if w
        (select-window w)
      (progn
        <span class="linecomment">; save shell-pop-last-buffer and shell-pop-last-window to return</span>
          (setq shell-pop-last-buffer (buffer-name))
          (setq shell-pop-last-window (selected-window))
          (if (not (eq shell-pop-window-height 100))
              (progn
                (split-window (selected-window)
                              (if (string= shell-pop-window-position "<span class="quote">bottom</span>")
                                  (round (* (window-height)
                                            (/ (- 100 shell-pop-window-height) 100.0)))
                                (round (* (window-height) (/ shell-pop-window-height 100.0)))))
                (if (string= shell-pop-window-position "<span class="quote">bottom</span>")
                    (other-window 1))))
          (if (not (get-buffer shell-pop-internal-mode-buffer))
              (funcall (eval shell-pop-internal-mode-func))
            (switch-to-buffer shell-pop-internal-mode-buffer))))))

(defun shell-pop-out ()
  (if (not (eq shell-pop-window-height 100))
      (progn
        (delete-window)
        (if (string= shell-pop-window-position "<span class="quote">bottom</span>")
            (select-window shell-pop-last-window))))
  (switch-to-buffer shell-pop-last-buffer))

(provide 'shell-pop)

<span class="linecomment">;;; shell-pop.el ends here.</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97">サイトマップ</a> <a class="local" href="http://www.emacswiki.org/emacs/%e6%9b%b4%e6%96%b0%e5%b1%a5%e6%ad%b4">更新履歴</a> <a class="local" href="http://www.emacswiki.org/emacs/%e3%83%8b%e3%83%a5%e3%83%bc%e3%82%b9">ニュース</a> <a class="local" href="http://www.emacswiki.org/emacs/%ef%bc%a5%ef%bd%8c%ef%bd%89%ef%bd%93%ef%bd%90%e3%82%bb%e3%82%af%e3%82%b7%e3%83%a7%e3%83%b3">Ｅｌｉｓｐセクション</a> <a class="local" href="http://www.emacswiki.org/emacs/%e5%88%a9%e7%94%a8%e6%89%8b%e5%bc%95">利用手引</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=shell-pop.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=shell-pop.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=shell-pop.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=shell-pop.el">Administration</a></span><span class="time"><br /> Last edited 2010-02-21 14:20 UTC by <a class="author" title="from KHP059139230131.ppp-bb.dion.ne.jp" href="http://www.emacswiki.org/emacs/kyagi">kyagi</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=shell-pop.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
