FasdUAS 1.101.10   ��   ��    k             l      ��  ��   ��"List MIAs" for iTuneswritten by Doug Adamsdougadams@mac.comv3.0  oct 1 2013- maintenance release
- adds notifications
 
v2.1 jul 22 2012
- compatible with OS X 10.8

v2.1 feb 17 12
- maintenance update
- minor UI fixes

v2.0 nov 6 11
- written as a Cocoa-AppleScript applet for OS X 10.6 or 10.7 only
- improves progress indicator
- no longer lists playlists

v1.2 sept 8 03
- make text file routine removes same-named text file

v1.1 aug 29 03
- now lists missing tracks followed by list of playlists they're supposed to be in

v1.0 dec 17 02
- initial release
Get more free AppleScripts and info on writing your ownat Doug's AppleScripts for iTunesdougscripts.com     � 	 	J  " L i s t   M I A s "   f o r   i T u n e s  w r i t t e n   b y   D o u g   A d a m s  d o u g a d a m s @ m a c . c o m   v 3 . 0     o c t   1   2 0 1 3  -   m a i n t e n a n c e   r e l e a s e 
 -   a d d s   n o t i f i c a t i o n s 
    
 v 2 . 1   j u l   2 2   2 0 1 2 
 -   c o m p a t i b l e   w i t h   O S   X   1 0 . 8 
 
 v 2 . 1   f e b   1 7   1 2 
 -   m a i n t e n a n c e   u p d a t e 
 -   m i n o r   U I   f i x e s 
 
 v 2 . 0   n o v   6   1 1 
 -   w r i t t e n   a s   a   C o c o a - A p p l e S c r i p t   a p p l e t   f o r   O S   X   1 0 . 6   o r   1 0 . 7   o n l y 
 -   i m p r o v e s   p r o g r e s s   i n d i c a t o r 
 -   n o   l o n g e r   l i s t s   p l a y l i s t s 
 
 v 1 . 2   s e p t   8   0 3 
 -   m a k e   t e x t   f i l e   r o u t i n e   r e m o v e s   s a m e - n a m e d   t e x t   f i l e 
 
 v 1 . 1   a u g   2 9   0 3 
 -   n o w   l i s t s   m i s s i n g   t r a c k s   f o l l o w e d   b y   l i s t   o f   p l a y l i s t s   t h e y ' r e   s u p p o s e d   t o   b e   i n 
 
 v 1 . 0   d e c   1 7   0 2 
 -   i n i t i a l   r e l e a s e 
  G e t   m o r e   f r e e   A p p l e S c r i p t s   a n d   i n f o   o n   w r i t i n g   y o u r   o w n  a t   D o u g ' s   A p p l e S c r i p t s   f o r   i T u n e s  d o u g s c r i p t s . c o m    
  
 l     ��������  ��  ��        l     ��������  ��  ��        l     ��������  ��  ��        j     �� �� 0 
mainwindow 
mainWindow  4     �� 
�� 
pcls  m       �    m a i n W i n d o w      j    
�� �� 0 workarea workArea  J    	����        j    �� �� 0 mytitle myTitle  m       �    L i s t   M I A s      l     ��������  ��  ��       !   i     " # " I      �������� 0 
accesshook 
accessHook��  ��   # k     � $ $  % & % Z     $ ' (���� ' =     ) * ) n     + , + I    �������� *0 checkitunesisactive checkItunesIsActive��  ��   ,  f      * m    ��
�� boovfals ( k   
   - -  . / . r   
  0 1 0 l  
  2���� 2 I  
 �� 3 4
�� .sysodlogaskr        TEXT 3 m   
  5 5 � 6 6 , i T u n e s   i s   n o t   r u n n i n g . 4 �� 7 8
�� 
btns 7 J     9 9  :�� : m     ; ; � < <  O K��   8 �� = >
�� 
dflt = m    ����  > �� ? @
�� 
appr ? m     A A � B B " C a n n o t   p r o c e e d . . . @ �� C D
�� 
disp C m    ����   D �� E��
�� 
givu E m    ���� ��  ��  ��   1 o      ���� 0 opt   /  F�� F L      G G m    ��
�� boovfals��  ��  ��   &  H I H l  % %��������  ��  ��   I  J K J Z   % I L M���� L =  % , N O N n  % * P Q P I   & *�������� ,0 itunesisnotaccesible itunesIsNotAccesible��  ��   Q  f   % & O m   * +��
�� boovtrue M k   / E R R  S T S r   / B U V U l  / @ W���� W I  / @�� X Y
�� .sysodlogaskr        TEXT X m   / 0 Z Z � [ [ j C l o s e   a n y   u t i l i t y   w i n d o w s   t h a t   m a y   b e   o p e n   i n   i T u n e s . Y �� \ ]
�� 
btns \ J   1 4 ^ ^  _�� _ m   1 2 ` ` � a a  O K��   ] �� b c
�� 
dflt b m   5 6����  c �� d e
�� 
appr d m   7 8 f f � g g " C a n n o t   p r o c e e d . . . e �� h i
�� 
disp h m   9 :����   i �� j��
�� 
givu j m   ; <���� ��  ��  ��   V o      ���� 0 opt   T  k�� k L   C E l l m   C D��
�� boovfals��  ��  ��   K  m n m l  J J��������  ��  ��   n  o p o Z   J � q r���� q n  J O s t s I   K O�������� 0 isfullscreen isFullScreen��  ��   t  f   J K r k   R � u u  v w v I  R Y�� x��
�� .sysodelanull��� ��� nmbr x m   R U y y ?�      ��   w  z { z r   Z | | } | l  Z z ~���� ~ I  Z z��  �
�� .sysodisAaleR        TEXT  m   Z ] � � � � � < i T u n e s   i s   i n   f u l l   s c r e e n   m o d e . � �� � �
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
as A � m   q t��
�� EAlTwarN � �� ���
�� 
givu � m   u v���� ��  ��  ��   } o      ���� 0 opt   {  ��� � Z   } � � ����� � =  } � � � � n   } � � � � 1   ~ ���
�� 
bhit � o   } ~���� 0 opt   � m   � � � � � � �  q u i t � k   � � � �  � � � O  � � � � � I  � �������
�� .miscactvnull��� ��� null��  ��   � m   � � � ��                                                                                  hook  alis    P  Mountain Lion              �5��H+    w
iTunes.app                                                      �p���2        ����  	                Applications    �5��      ��r      w  &Mountain Lion:Applications: iTunes.app   
 i T u n e s . a p p    M o u n t a i n   L i o n  Applications/iTunes.app   / ��   �  ��� � L   � � � � m   � ���
�� boovfals��  ��  ��  ��  ��  ��   p  � � � l  � ���������  ��  ��   �  ��� � L   � � � � m   � ���
�� boovtrue��   !  � � � l     ��������  ��  ��   �  � � � i    � � � I      �������� *0 checkitunesisactive checkItunesIsActive��  ��   � O     � � � L     � � l    ����� � I   �� ���
�� .coredoexbool       obj  � l    ����� � 6    � � � 3    ��
�� 
prcs � =    � � � 1   	 ��
�� 
pnam � m     � � � � �  i T u n e s��  ��  ��  ��  ��   � m      � ��                                                                                  sevs  alis    �  Mountain Lion              �5��H+    ZSystem Events.app                                               5��Ɖ        ����  	                CoreServices    �5��      ���      Z  T  S  >Mountain Lion:System: Library: CoreServices: System Events.app  $  S y s t e m   E v e n t s . a p p    M o u n t a i n   L i o n  -System/Library/CoreServices/System Events.app   / ��   �  � � � l     ��������  ��  ��   �  � � � i     � � � I      �������� ,0 itunesisnotaccesible itunesIsNotAccesible��  ��   � k     # � �  � � � Q       � � � � t     � � � O    � � � e     � � n     � � � 1    �
� 
pnam � 4    �~ �
�~ 
cLiP � m    �}�}  � 5    
�| ��{
�| 
capp � m     � � � � �   c o m . a p p l e . i T u n e s
�{ kfrmID   � m    �z�z  � R      �y�x�w
�y .ascrerr ****      � ****�x  �w   � L      � � m    �v
�v boovtrue �  ��u � L   ! # � � m   ! "�t
�t boovfals�u   �  � � � l     �s�r�q�s  �r  �q   �  � � � i     � � � I      �p�o�n�p 0 isfullscreen isFullScreen�o  �n   � Q     % � � � � O     � � � O     � � � L     � � l    ��m�l � e     � � n     � � � 1    �k
�k 
valL � n     � � � 4    �j �
�j 
attr � m     � � � � �  A X F u l l S c r e e n � 4    �i �
�i 
cwin � m    �h�h �m  �l   � 4    �g �
�g 
prcs � m   	 
 � � � � �  i T u n e s � m     � ��                                                                                  sevs  alis    �  Mountain Lion              �5��H+    ZSystem Events.app                                               5��Ɖ        ����  	                CoreServices    �5��      ���      Z  T  S  >Mountain Lion:System: Library: CoreServices: System Events.app  $  S y s t e m   E v e n t s . a p p    M o u n t a i n   L i o n  -System/Library/CoreServices/System Events.app   / ��   � R      �f�e�d
�f .ascrerr ****      � ****�e  �d   � L   # % � � m   # $�c
�c boovfals �  � � � l     �b�a�`�b  �a  �`   �  � � � i    ! � � � I     �_�^�]
�_ .aevtoappnull  �   � ****�^  �]   � k     ^ � �  � � � Z     + � ��\�[ � l     �Z�Y  =     I     �X�W�V�X 0 
accesshook 
accessHook�W  �V   m    �U
�U boovfals�Z  �Y   � Q   
 ' O    I   �T�S�R
�T .aevtquitnull��� ��� null�S  �R    f     R      �Q�P
�Q .ascrerr ****      � **** o      �O�O 0 m  �P   k    '		 

 I   $�N�M
�N .ascrcmnt****      � **** o     �L�L 0 m  �M   �K L   % '�J�J  �K  �\  �[   �  l  , ,�I�H�G�I  �H  �G    O  , D r   4 C n  4 = I   9 =�F�E�D�F 0 init  �E  �D   I   4 9�C�B�A�C 	0 alloc  �B  �A   o      �@�@ 0 workarea workArea o   , 1�?�? 0 
mainwindow 
mainWindow  r   E N  f   E F n      o   K M�>�> 0 
mainscript 
mainScript o   F K�=�= 0 workarea workArea �< O  O ^  I   W ]�;!�:�; 0 launch_  ! "�9"  f   X Y�9  �:    o   O T�8�8 0 workarea workArea�<   � #$# l     �7�6�5�7  �6  �5  $ %�4% l     �3�2�1�3  �2  �1  �4       
�0&'( )*+,-�0  & �/�.�-�,�+�*�)�(�/ 0 
mainwindow 
mainWindow�. 0 workarea workArea�- 0 mytitle myTitle�, 0 
accesshook 
accessHook�+ *0 checkitunesisactive checkItunesIsActive�* ,0 itunesisnotaccesible itunesIsNotAccesible�) 0 isfullscreen isFullScreen
�( .aevtoappnull  �   � ****' .. �'�&/
�' misccura
�& 
pcls/ �00  m a i n W i n d o w( �%�$�%  �$  ) �# #�"�!12� �# 0 
accesshook 
accessHook�"  �!  1 �� 0 opt  2 � 5� ;�� A������ Z ` f� y� �� � � ����� � ��� *0 checkitunesisactive checkItunesIsActive
� 
btns
� 
dflt
� 
appr
� 
disp
� 
givu� � 

� .sysodlogaskr        TEXT� ,0 itunesisnotaccesible itunesIsNotAccesible� 0 isfullscreen isFullScreen
� .sysodelanull��� ��� nmbr
� 
mesS
� 
as A
� EAlTwarN
� .sysodisAaleR        TEXT
� 
bhit
� .miscactvnull��� ��� null�  �)j+  f  ���kv�k���j��� E�OfY hO)j+ e  ���kv�k���j��� E�OfY hO)j+  Oa j Oa a a �a a lv�ka a ��� E�O�a ,a   a  *j UOfY hY hOe* � ���
34�	� *0 checkitunesisactive checkItunesIsActive�  �
  3  4  ��5� ��
� 
prcs5  
� 
pnam
� .coredoexbool       obj �	 � *�.�[�,\Z�81j U+ � ���67�� ,0 itunesisnotaccesible itunesIsNotAccesible�  �  6  7 � �� ��������
� 
capp
�  kfrmID  
�� 
cLiP
�� 
pnam��  ��  � $ kn)���0 	*�k/�,EUoW 	X  eOf, �� �����89���� 0 isfullscreen isFullScreen��  ��  8  9 	 ��� ����� �������
�� 
prcs
�� 
cwin
�� 
attr
�� 
valL��  ��  �� & � *��/ *�k/��/�,EUUW 	X  f- �� �����:;��
�� .aevtoappnull  �   � ****��  ��  : ���� 0 m  ; 	�������������������� 0 
accesshook 
accessHook
�� .aevtquitnull��� ��� null�� 0 m  ��  
�� .ascrcmnt****      � ****�� 	0 alloc  �� 0 init  �� 0 
mainscript 
mainScript�� 0 launch_  �� _*j+  f  " ) *j UW X  �j OhY hOb    *j+ j+ Ec  UO)b  �,FOb   *)k+ Uascr  ��ޭ