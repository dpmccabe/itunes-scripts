FasdUAS 1.101.10   ��   ��    k             l      ��  ��   ��
"Quick Convert" for iTunes
written by Doug Adams
dougadams@mac.com

v3.4 nov 23 2013
- fixes error with initial nag sheet continuing to open after Donate button clicked in past launch

v3.3 nov 15 2013
- maintenance update
- compatibility fixes for OS X 10.9 (Mavericks)

v3.2 dec 6 2012
- fixes issue that could prevent deletion of original file (when set)

v3.1 jul 24 2012
- compatible with OS X 10.8

v3.0 aug 14 2011
- written as Cocoa-AppleScript applet for OS X 10.6 or better only
- consolidates all settings into a single window
- displays progress during processing

v2.9 oct 6 2008
- fixes minor problem converting POSIX files in "bookmarkable" routine

v2.8 aug 20 2008
- compatible with iTunes 7.7.1
- GUI updates
- accounts for DOS-formatted drives

v2.7 apr 3 2008
- saved as universal binary
- files converted to AAC with the bookmarkable option are re-added to "Audiobooks" Master Library correctly

v2.6.1 dec 31 2006
- fixed error thrown when conversion procedure is canceled

v2.6 march 24 2006
- fixed problem selecting CD playlist
- updated dialog and choose prompts to refer to import or convert depending on class of selected playlist tracks
- prevents selection of iPod or Radio playlist tracks; prevents attempt to convert URL tracks

v2.5 january 18 2006
- option to remove/delete original tracks/files
- option to convert to bookmarkable also renames AAC with "m4b" extension

v2.4 february 25 2005
- option to make tracks converted to AAC "bookmarkable"

v2.3 released may 22 '04
-- no longer works in OS 9
-- condensed code
-- more error checking

v2.2 released mar 4 '04
-- tidied up some routines
-- removed a superflous confirmation dialog


Get more free AppleScripts and info on writing your own
at Doug's AppleScripts for iTunes
dougscripts.com
     � 	 	� 
 " Q u i c k   C o n v e r t "   f o r   i T u n e s 
 w r i t t e n   b y   D o u g   A d a m s 
 d o u g a d a m s @ m a c . c o m 
 
 v 3 . 4   n o v   2 3   2 0 1 3 
 -   f i x e s   e r r o r   w i t h   i n i t i a l   n a g   s h e e t   c o n t i n u i n g   t o   o p e n   a f t e r   D o n a t e   b u t t o n   c l i c k e d   i n   p a s t   l a u n c h 
 
 v 3 . 3   n o v   1 5   2 0 1 3 
 -   m a i n t e n a n c e   u p d a t e 
 -   c o m p a t i b i l i t y   f i x e s   f o r   O S   X   1 0 . 9   ( M a v e r i c k s ) 
 
 v 3 . 2   d e c   6   2 0 1 2 
 -   f i x e s   i s s u e   t h a t   c o u l d   p r e v e n t   d e l e t i o n   o f   o r i g i n a l   f i l e   ( w h e n   s e t ) 
 
 v 3 . 1   j u l   2 4   2 0 1 2 
 -   c o m p a t i b l e   w i t h   O S   X   1 0 . 8 
 
 v 3 . 0   a u g   1 4   2 0 1 1 
 -   w r i t t e n   a s   C o c o a - A p p l e S c r i p t   a p p l e t   f o r   O S   X   1 0 . 6   o r   b e t t e r   o n l y 
 -   c o n s o l i d a t e s   a l l   s e t t i n g s   i n t o   a   s i n g l e   w i n d o w 
 -   d i s p l a y s   p r o g r e s s   d u r i n g   p r o c e s s i n g 
 
 v 2 . 9   o c t   6   2 0 0 8 
 -   f i x e s   m i n o r   p r o b l e m   c o n v e r t i n g   P O S I X   f i l e s   i n   " b o o k m a r k a b l e "   r o u t i n e 
 
 v 2 . 8   a u g   2 0   2 0 0 8 
 -   c o m p a t i b l e   w i t h   i T u n e s   7 . 7 . 1 
 -   G U I   u p d a t e s 
 -   a c c o u n t s   f o r   D O S - f o r m a t t e d   d r i v e s 
 
 v 2 . 7   a p r   3   2 0 0 8 
 -   s a v e d   a s   u n i v e r s a l   b i n a r y 
 -   f i l e s   c o n v e r t e d   t o   A A C   w i t h   t h e   b o o k m a r k a b l e   o p t i o n   a r e   r e - a d d e d   t o   " A u d i o b o o k s "   M a s t e r   L i b r a r y   c o r r e c t l y 
 
 v 2 . 6 . 1   d e c   3 1   2 0 0 6 
 -   f i x e d   e r r o r   t h r o w n   w h e n   c o n v e r s i o n   p r o c e d u r e   i s   c a n c e l e d 
 
 v 2 . 6   m a r c h   2 4   2 0 0 6 
 -   f i x e d   p r o b l e m   s e l e c t i n g   C D   p l a y l i s t 
 -   u p d a t e d   d i a l o g   a n d   c h o o s e   p r o m p t s   t o   r e f e r   t o   i m p o r t   o r   c o n v e r t   d e p e n d i n g   o n   c l a s s   o f   s e l e c t e d   p l a y l i s t   t r a c k s 
 -   p r e v e n t s   s e l e c t i o n   o f   i P o d   o r   R a d i o   p l a y l i s t   t r a c k s ;   p r e v e n t s   a t t e m p t   t o   c o n v e r t   U R L   t r a c k s 
 
 v 2 . 5   j a n u a r y   1 8   2 0 0 6 
 -   o p t i o n   t o   r e m o v e / d e l e t e   o r i g i n a l   t r a c k s / f i l e s 
 -   o p t i o n   t o   c o n v e r t   t o   b o o k m a r k a b l e   a l s o   r e n a m e s   A A C   w i t h   " m 4 b "   e x t e n s i o n 
 
 v 2 . 4   f e b r u a r y   2 5   2 0 0 5 
 -   o p t i o n   t o   m a k e   t r a c k s   c o n v e r t e d   t o   A A C   " b o o k m a r k a b l e " 
 
 v 2 . 3   r e l e a s e d   m a y   2 2   ' 0 4 
 - -   n o   l o n g e r   w o r k s   i n   O S   9 
 - -   c o n d e n s e d   c o d e 
 - -   m o r e   e r r o r   c h e c k i n g 
 
 v 2 . 2   r e l e a s e d   m a r   4   ' 0 4 
 - -   t i d i e d   u p   s o m e   r o u t i n e s 
 - -   r e m o v e d   a   s u p e r f l o u s   c o n f i r m a t i o n   d i a l o g 
 
 
 G e t   m o r e   f r e e   A p p l e S c r i p t s   a n d   i n f o   o n   w r i t i n g   y o u r   o w n 
 a t   D o u g ' s   A p p l e S c r i p t s   f o r   i T u n e s 
 d o u g s c r i p t s . c o m 
   
  
 l     ��������  ��  ��        l     ��  ��    6 0 Copyright 2012 Doug Adams. All rights reserved.     �   `   C o p y r i g h t   2 0 1 2   D o u g   A d a m s .   A l l   r i g h t s   r e s e r v e d .      l     ��������  ��  ��        j     �� �� 0 mytitle myTitle  m        �    Q u i c k   C o n v e r t      j    �� �� 0 workarea workArea  J    ����        l     ��������  ��  ��        i    
     I     ������
�� .aevtoappnull  �   � ****��  ��     k     \ ! !  " # " Z     + $ %���� $ l     &���� & =     ' ( ' I     �������� 0 
accesshook 
accessHook��  ��   ( m    ��
�� boovfals��  ��   % Q   
 ' ) * + ) O    , - , I   ������
�� .aevtquitnull��� ��� null��  ��   -  f     * R      �� .��
�� .ascrerr ****      � **** . o      ���� 0 m  ��   + k    ' / /  0 1 0 I   $�� 2��
�� .ascrcmnt****      � **** 2 o     ���� 0 m  ��   1  3�� 3 L   % '����  ��  ��  ��   #  4 5 4 l  , ,��������  ��  ��   5  6 7 6 O  , B 8 9 8 r   2 A : ; : n  2 ; < = < I   7 ;�������� 0 init  ��  ��   = I   2 7�������� 	0 alloc  ��  ��   ; o      ���� 0 workarea workArea 9 n  , / > ? > o   - /���� 0 dawindow DAWindow ? m   , -��
�� misccura 7  @ A @ r   C L B C B  f   C D C n      D E D o   I K���� 0 
mainscript 
mainScript E o   D I���� 0 workarea workArea A  F�� F O  M \ G H G I   U [�� I���� 0 launch_   I  J�� J  f   V W��  ��   H o   M R���� 0 workarea workArea��     K L K l     ��������  ��  ��   L  M N M i     O P O I      �������� 0 
accesshook 
accessHook��  ��   P k     � Q Q  R S R Z     $ T U���� T =     V W V n     X Y X I    �������� *0 checkitunesisactive checkItunesIsActive��  ��   Y  f      W m    ��
�� boovfals U k   
   Z Z  [ \ [ r   
  ] ^ ] l  
  _���� _ I  
 �� ` a
�� .sysodlogaskr        TEXT ` m   
  b b � c c , i T u n e s   i s   n o t   r u n n i n g . a �� d e
�� 
btns d J     f f  g�� g m     h h � i i  O K��   e �� j k
�� 
dflt j m    ����  k �� l m
�� 
appr l m     n n � o o " C a n n o t   p r o c e e d . . . m �� p q
�� 
disp p m    ����   q �� r��
�� 
givu r m    ���� ��  ��  ��   ^ o      ���� 0 opt   \  s�� s L      t t m    ��
�� boovfals��  ��  ��   S  u v u l  % %��������  ��  ��   v  w x w Z   % I y z���� y =  % , { | { n  % * } ~ } I   & *�������� ,0 itunesisnotaccesible itunesIsNotAccesible��  ��   ~  f   % & | m   * +��
�� boovtrue z k   / E    � � � r   / B � � � l  / @ ����� � I  / @�� � �
�� .sysodlogaskr        TEXT � m   / 0 � � � � � j C l o s e   a n y   u t i l i t y   w i n d o w s   t h a t   m a y   b e   o p e n   i n   i T u n e s . � �� � �
�� 
btns � J   1 4 � �  ��� � m   1 2 � � � � �  O K��   � �� � �
�� 
dflt � m   5 6����  � �� � �
�� 
appr � m   7 8 � � � � � " C a n n o t   p r o c e e d . . . � �� � �
�� 
disp � m   9 :����   � �� ���
�� 
givu � m   ; <���� ��  ��  ��   � o      ���� 0 opt   �  ��� � L   C E � � m   C D��
�� boovfals��  ��  ��   x  � � � l  J J��������  ��  ��   �  � � � Z   J � � ����� � n  J O � � � I   K O�������� 0 isfullscreen isFullScreen��  ��   �  f   J K � k   R � � �  � � � I  R Y�� ���
�� .sysodelanull��� ��� nmbr � m   R U � � ?�      ��   �  � � � r   Z | � � � l  Z z ����� � I  Z z�� � �
�� .sysodisAaleR        TEXT � m   Z ] � � � � � < i T u n e s   i s   i n   f u l l   s c r e e n   m o d e . � �� � �
�� 
mesS � m   ` c � � � � � T h i s   a p p l e t ' s   i n t e r f a c e   c a n n o t   b e   d i s p l a y e d   w i t h   i T u n e s   w h i l e   i n   f u l l   s c r e e n   m o d e . 
 	 	 
 Y o u   c a n   Q u i t   a n d   r e - l a u n c h   t h i s   a p p l e t   a f t e r   t a k i n g   i T u n e s   o u t   o f   f u l l   s c r e e n   m o d e . 
 
 O r   y o u   c a n   P r o c e e d   A n y w a y ,   b u t   i T u n e s   w i l l   n o t   b e   v i s i b l e   w h i l e   t h e   a p p l e t   i s   r u n n i n g . � �� � �
�� 
btns � J   d l � �  � � � m   d g � � � � �  Q u i t �  ��� � m   g j � � � � �  P r o c e e d   A n y w a y��   � �� � �
�� 
dflt � m   m n����  � �� � �
�� 
as A � m   q t�
� EAlTwarN � �~ ��}
�~ 
givu � m   u v�|�| �}  ��  ��   � o      �{�{ 0 opt   �  ��z � Z   } � � ��y�x � =  } � � � � n   } � � � � 1   ~ ��w
�w 
bhit � o   } ~�v�v 0 opt   � m   � � � � � � �  q u i t � k   � � � �  � � � O  � � � � � I  � ��u�t�s
�u .miscactvnull��� ��� null�t  �s   � m   � � � ��                                                                                  hook  alis    P  Mountain Lion              �5��H+    w
iTunes.app                                                      �p���2        ����  	                Applications    �5��      ��r      w  &Mountain Lion:Applications: iTunes.app   
 i T u n e s . a p p    M o u n t a i n   L i o n  Applications/iTunes.app   / ��   �  ��r � L   � � � � m   � ��q
�q boovfals�r  �y  �x  �z  ��  ��   �  � � � l  � ��p�o�n�p  �o  �n   �  ��m � L   � � � � m   � ��l
�l boovtrue�m   N  � � � l     �k�j�i�k  �j  �i   �  � � � i    � � � I      �h�g�f�h *0 checkitunesisactive checkItunesIsActive�g  �f   � O     � � � L     � � 1    �e
�e 
prun � 5     �d ��c
�d 
capp � m     � � � � �   c o m . a p p l e . i T u n e s
�c kfrmID   �  � � � l     �b�a�`�b  �a  �`   �  � � � i     � � � I      �_�^�]�_ ,0 itunesisnotaccesible itunesIsNotAccesible�^  �]   � k     # � �  � � � Q       � � � � t     � � � O    � � � e     � � n     � � � 1    �\
�\ 
pnam � 4    �[ �
�[ 
cLiP � m    �Z�Z  � 5    
�Y ��X
�Y 
capp � m     � � � � �   c o m . a p p l e . i T u n e s
�X kfrmID   � m    �W�W  � R      �V�U�T
�V .ascrerr ****      � ****�U  �T   � L      � � m    �S
�S boovtrue �  ��R � L   ! # � � m   ! "�Q
�Q boovfals�R   �  � � � l     �P�O�N�P  �O  �N   �    i     I      �M�L�K�M 0 isfullscreen isFullScreen�L  �K   Q     % O     O    	
	 L     l   �J�I e     n     1    �H
�H 
valL n     4    �G
�G 
attr m     �  A X F u l l S c r e e n 4    �F
�F 
cwin m    �E�E �J  �I  
 4    �D
�D 
prcs m   	 
 �  i T u n e s m    �                                                                                  sevs  alis    �  Mountain Lion              �5��H+    ZSystem Events.app                                               5��Ɖ        ����  	                CoreServices    �5��      ���      Z  T  S  >Mountain Lion:System: Library: CoreServices: System Events.app  $  S y s t e m   E v e n t s . a p p    M o u n t a i n   L i o n  -System/Library/CoreServices/System Events.app   / ��   R      �C�B�A
�C .ascrerr ****      � ****�B  �A   L   # % m   # $�@
�@ boovfals �? l     �>�=�<�>  �=  �<  �?       	�;  !"�;   �:�9�8�7�6�5�4�: 0 mytitle myTitle�9 0 workarea workArea
�8 .aevtoappnull  �   � ****�7 0 
accesshook 
accessHook�6 *0 checkitunesisactive checkItunesIsActive�5 ,0 itunesisnotaccesible itunesIsNotAccesible�4 0 isfullscreen isFullScreen �3�2�3  �2   �1  �0�/#$�.
�1 .aevtoappnull  �   � ****�0  �/  # �-�- 0 m  $ �,�+�*�)�(�'�&�%�$�#�"�, 0 
accesshook 
accessHook
�+ .aevtquitnull��� ��� null�* 0 m  �)  
�( .ascrcmnt****      � ****
�' misccura�& 0 dawindow DAWindow�% 	0 alloc  �$ 0 init  �# 0 
mainscript 
mainScript�" 0 launch_  �. ]*j+  f  " ) *j UW X  �j OhY hO��, *j+ j+ Ec  UO)b  �,FOb   *)k+ 
U �! P� �%&��! 0 
accesshook 
accessHook�   �  % �� 0 opt  & � b� h�� n������ � � �� �� �� � � ����� � ��� *0 checkitunesisactive checkItunesIsActive
� 
btns
� 
dflt
� 
appr
� 
disp
� 
givu� � 

� .sysodlogaskr        TEXT� ,0 itunesisnotaccesible itunesIsNotAccesible� 0 isfullscreen isFullScreen
� .sysodelanull��� ��� nmbr
� 
mesS
� 
as A
� EAlTwarN
� .sysodisAaleR        TEXT
� 
bhit
� .miscactvnull��� ��� null� �)j+  f  ���kv�k���j��� E�OfY hO)j+ e  ���kv�k���j��� E�OfY hO)j+  Oa j Oa a a �a a lv�ka a ��� E�O�a ,a   a  *j UOfY hY hOe  �
 ��	�'(��
 *0 checkitunesisactive checkItunesIsActive�	  �  '  ( � ���
� 
capp
� kfrmID  
� 
prun� )���0 *�,EU! � ���)*� � ,0 itunesisnotaccesible itunesIsNotAccesible�  �  )  * �� �����������
�� 
capp
�� kfrmID  
�� 
cLiP
�� 
pnam��  ��  �  $ kn)���0 	*�k/�,EUoW 	X  eOf" ������+,���� 0 isfullscreen isFullScreen��  ��  +  , 	������������
�� 
prcs
�� 
cwin
�� 
attr
�� 
valL��  ��  �� & � *��/ *�k/��/�,EUUW 	X  f ascr  ��ޭ