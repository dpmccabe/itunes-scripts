FasdUAS 1.101.10   ��   ��    k             l      ��  ��   
"New Play Count" for iTunes
written by Doug Adams
dougadams@mac.com

v2.0 mar 12 09
-- erases played count when play count is '0' - iTunes 8.1 or better

v1.1 aug 24 06
-- works with streams (URL tracks)

v1.0 oct 16 03
-- initial release

Get more free AppleScripts and info on writing your own
at Doug's AppleScripts for iTunes
http://www.dougscripts.com/itunes/

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

Get a copy of the GNU General Public License by writing to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

or visit http://www.gnu.org/copyleft/gpl.html

     � 	 	0 
 " N e w   P l a y   C o u n t "   f o r   i T u n e s 
 w r i t t e n   b y   D o u g   A d a m s 
 d o u g a d a m s @ m a c . c o m 
 
 v 2 . 0   m a r   1 2   0 9 
 - -   e r a s e s   p l a y e d   c o u n t   w h e n   p l a y   c o u n t   i s   ' 0 '   -   i T u n e s   8 . 1   o r   b e t t e r 
 
 v 1 . 1   a u g   2 4   0 6 
 - -   w o r k s   w i t h   s t r e a m s   ( U R L   t r a c k s ) 
 
 v 1 . 0   o c t   1 6   0 3 
 - -   i n i t i a l   r e l e a s e 
 
 G e t   m o r e   f r e e   A p p l e S c r i p t s   a n d   i n f o   o n   w r i t i n g   y o u r   o w n 
 a t   D o u g ' s   A p p l e S c r i p t s   f o r   i T u n e s 
 h t t p : / / w w w . d o u g s c r i p t s . c o m / i t u n e s / 
 
 T h i s   p r o g r a m   i s   f r e e   s o f t w a r e ;   y o u   c a n   r e d i s t r i b u t e   i t   a n d / o r   m o d i f y   i t   u n d e r   t h e   t e r m s   o f   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e   a s   p u b l i s h e d   b y   t h e   F r e e   S o f t w a r e   F o u n d a t i o n ;   e i t h e r   v e r s i o n   2   o f   t h e   L i c e n s e ,   o r   ( a t   y o u r   o p t i o n )   a n y   l a t e r   v e r s i o n . 
 
 T h i s   p r o g r a m   i s   d i s t r i b u t e d   i n   t h e   h o p e   t h a t   i t   w i l l   b e   u s e f u l ,   b u t   W I T H O U T   A N Y   W A R R A N T Y ;   w i t h o u t   e v e n   t h e   i m p l i e d   w a r r a n t y   o f   M E R C H A N T A B I L I T Y   o r   F I T N E S S   F O R   A   P A R T I C U L A R   P U R P O S E .     S e e   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e   f o r   m o r e   d e t a i l s . 
 
 G e t   a   c o p y   o f   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e   b y   w r i t i n g   t o   t h e   F r e e   S o f t w a r e   F o u n d a t i o n ,   I n c . ,   5 1   F r a n k l i n   S t r e e t ,   F i f t h   F l o o r ,   B o s t o n ,   M A     0 2 1 1 0 - 1 3 0 1 ,   U S A . 
 
 o r   v i s i t   h t t p : / / w w w . g n u . o r g / c o p y l e f t / g p l . h t m l 
 
   
  
 l     ��������  ��  ��        p         ������ 0 thismany  ��        l    � ����  O     �    Z    �  ��   >   
    1    ��
�� 
sele  J    	����    k    y       r        1    ��
�� 
sele  o      ���� 0 sel        O      !   I    �� "���� 0 tell_how_many   "  #�� # m     $ $ � % %  ��  ��   !  f       &�� & X    y '�� ( ' k   / t ) )  * + * r   / 4 , - , n   / 2 . / . 1   0 2��
�� 
pcnt / o   / 0���� 0 t   - o      ���� 0 t   +  0�� 0 Z   5 t 1 2���� 1 G   5 D 3 4 3 =  5 : 5 6 5 n   5 8 7 8 7 m   6 8��
�� 
pcls 8 o   5 6���� 0 t   6 m   8 9��
�� 
cFlT 4 =  = B 9 : 9 n   = @ ; < ; m   > @��
�� 
pcls < o   = >���� 0 t   : m   @ A��
�� 
cURT 2 k   G p = =  > ? > r   G N @ A @ l  G J B���� B c   G J C D C o   G H���� 0 thismany   D m   H I��
�� 
long��  ��   A n       E F E 1   K M��
�� 
pPlC F o   J K���� 0 t   ?  G�� G Z   O p H I���� H =  O T J K J l  O R L���� L c   O R M N M o   O P���� 0 thismany   N m   P Q��
�� 
long��  ��   K m   R S����   I Q   W l O P�� O r   Z c Q R Q m   Z ]��
�� 
msng R n       S T S 1   ^ b��
�� 
pPlD T o   ] ^���� 0 t   P R      ������
�� .ascrerr ****      � ****��  ��  ��  ��  ��  ��  ��  ��  ��  �� 0 t   ( o   " #���� 0 sel  ��  ��    l  | � U V W U O  | � X Y X I   � ��� Z���� 0 message_and_cancel   Z  [�� [ m   � � \ \ � ] ] & N o   t r a c k s   s e l e c t e d .��  ��   Y  f   | } V   no track selected    W � ^ ^ $   n o   t r a c k   s e l e c t e d  m      _ _�                                                                                  hook  alis    N  Macintosh HD               ��Z�H+  >ǐ
iTunes.app                                                     ����r�W        ����  	                Applications    �Ò�      �s-�    >ǐ  %Macintosh HD:Applications: iTunes.app    
 i T u n e s . a p p    M a c i n t o s h   H D  Applications/iTunes.app   / ��  ��  ��     ` a ` l     ��������  ��  ��   a  b c b i     d e d I      �� f���� 0 tell_how_many   f  g�� g o      ���� 0 addenda  ��  ��   e k     ; h h  i j i s      k l k n      m n m 1    ��
�� 
ttxt n l     o���� o I    �� p q
�� .sysodlogaskr        TEXT p l     r���� r b      s t s o     ���� 0 addenda   t m     u u � v v ^ S e t   t h e   " P l a y   C o u n t "   o f   t h e   s e l e c t e d   t r a c k s   t o :��  ��   q �� w x
�� 
dtxt w m     y y � z z   x �� {��
�� 
disp { m    ���� ��  ��  ��   l o      ���� 0 thismany   j  |�� | Q    ; } ~  } Z   $ � ����� � =    � � � n     � � � m    ��
�� 
pcls � l    ����� � c     � � � o    ���� 0 thismany   � m    ��
�� 
nmbr��  ��   � m    ��
�� 
long � L     ����  ��  ��   ~ R      ������
�� .ascrerr ****      � ****��  ��    O  , ; � � � I   0 :�� ����� 0 tell_how_many   �  ��� � b   1 6 � � � b   1 4 � � � m   1 2 � � � � � " E n t e r   a   n u m b e r . . . � o   2 3��
�� 
ret  � o   4 5��
�� 
ret ��  ��   �  f   , -��   c  � � � l     ��������  ��  ��   �  ��� � i    � � � I      �� ����� 0 message_and_cancel   �  ��� � o      ���� 0 ms  ��  ��   � I    �� � �
�� .sysodlogaskr        TEXT � o     ���� 0 ms   � �� � �
�� 
btns � J     � �  ��� � m     � � � � �  C a n c e l��   � �� � �
�� 
dflt � m    ����  � �� ���
�� 
disp � m    	����  ��  ��       �� � � � ���   � �������� 0 tell_how_many  �� 0 message_and_cancel  
�� .aevtoappnull  �   � **** � �� e���� � ����� 0 tell_how_many  �� �� ���  �  ���� 0 addenda  ��   � ���� 0 addenda   �  u�� y��������������~�}�| ��{�z
�� 
dtxt
�� 
disp�� 
�� .sysodlogaskr        TEXT
�� 
ttxt�� 0 thismany  
�� 
nmbr
� 
pcls
�~ 
long�}  �|  
�{ 
ret �z 0 tell_how_many  �� <��%���k� �,EQ�O ��&�,�  hY hW X  ) *��%�%k+ U � �y ��x�w � ��v�y 0 message_and_cancel  �x �u ��u  �  �t�t 0 ms  �w   � �s�s 0 ms   � �r ��q�p�o�n
�r 
btns
�q 
dflt
�p 
disp�o 
�n .sysodlogaskr        TEXT�v ���kv�k�j�  � �m ��l�k � ��j
�m .aevtoappnull  �   � **** � k     � � �  �i�i  �l  �k   � �h�h 0 t   �  _�g�f $�e�d�c�b�a�`�_�^�]�\�[�Z�Y�X�W�V \�U
�g 
sele�f 0 sel  �e 0 tell_how_many  
�d 
kocl
�c 
cobj
�b .corecnte****       ****
�a 
pcnt
�` 
pcls
�_ 
cFlT
�^ 
cURT
�] 
bool�\ 0 thismany  
�[ 
long
�Z 
pPlC
�Y 
msng
�X 
pPlD�W  �V  �U 0 message_and_cancel  �j �� �*�,jv q*�,E�O) *�k+ UO Y�[��l kh  ��,E�O��,� 
 	��,� �& .��&��,FO��&j   a �a ,FW X  hY hY h[OY��Y ) 
*a k+ UUascr  ��ޭ