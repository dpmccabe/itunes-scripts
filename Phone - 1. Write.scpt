JsOsaDAS1.001.00bplist00�Vscripto5 f u n c t i o n   w r i t e T e x t T o F i l e ( t e x t ,   f i l e ,   o v e r w r i t e E x i s t i n g C o n t e n t )   { 
     t r y   {   
         / /   C o n v e r t   t h e   f i l e   t o   a   s t r i n g 
         v a r   f i l e S t r i n g   =   f i l e . t o S t r i n g ( ) 
   
         / /   O p e n   t h e   f i l e   f o r   w r i t i n g 
         v a r   o p e n e d F i l e   =   a p p . o p e n F o r A c c e s s ( P a t h ( f i l e S t r i n g ) ,   {   w r i t e P e r m i s s i o n :   t r u e   } ) 
   
         / /   C l e a r   t h e   f i l e   i f   c o n t e n t   s h o u l d   b e   o v e r w r i t t e n 
         i f   ( o v e r w r i t e E x i s t i n g C o n t e n t )   { 
             a p p . s e t E o f ( o p e n e d F i l e ,   {   t o :   0   } ) 
         } 
   
         / /   W r i t e   t h e   n e w   c o n t e n t   t o   t h e   f i l e 
         a p p . w r i t e ( t e x t ,   {   t o :   o p e n e d F i l e ,   s t a r t i n g A t :   a p p . g e t E o f ( o p e n e d F i l e )   } ) 
   
         / /   C l o s e   t h e   f i l e 
         a p p . c l o s e A c c e s s ( o p e n e d F i l e ) 
   
         / /   R e t u r n   a   b o o l e a n   i n d i c a t i n g   t h a t   w r i t i n g   w a s   s u c c e s s f u l 
         r e t u r n ( t r u e ) 
     } 
     c a t c h ( e r r o r )   { 
         t r y   { 
             / /   C l o s e   t h e   f i l e 
             a p p . c l o s e A c c e s s ( f i l e ) 
         } 
         c a t c h ( e r r o r )   { 
             / /   R e p o r t   t h e   e r r o r   i s   c l o s i n g   f a i l e d 
             c o n s o l e . l o g ( ` C o u l d n ' t   c l o s e   f i l e :   $ { e r r o r } ` ) 
         } 
   
         / /   R e t u r n   a   b o o l e a n   i n d i c a t i n g   t h a t   w r i t i n g   w a s   s u c c e s s f u l 
         r e t u r n ( f a l s e ) 
     } 
 } 
 
 a p p   =   A p p l i c a t i o n . c u r r e n t A p p l i c a t i o n ( ) 
 a p p . i n c l u d e S t a n d a r d A d d i t i o n s   =   t r u e 
 
 i t u n e s   =   A p p l i c a t i o n ( ' i T u n e s ' ) 
 
 p l _ n a m e   =   "& + " 
 p l   =   i t u n e s . p l a y l i s t s [ p l _ n a m e ] 
 t r a c k s   =   p l . t r a c k s ( ) 
 
 t r a c k _ i n f o s   =   t r a c k s . m a p ( f u n c t i o n ( t r a c k )   { 
 	 t r a c k _ i n f o   =   [ 
 	 	 t r a c k . d a t a b a s e I D ( ) , 
 	 	 t r a c k . d u r a t i o n ( ) , 
 	 	 t r a c k . r a t i n g ( )   /   2 0 , 
 	 	 t r a c k . p l a y e d C o u n t ( ) , 
 	 	 ( a p p . c u r r e n t D a t e ( )   -   t r a c k . p l a y e d D a t e ( ) )   /   ( 2 4 * 3 6 0 0 * 1 0 0 0 ) , 
 	 	 t r a c k . g e n r e ( ) . r e p l a c e ( / , / g ,   " " ) , 
 	     t r a c k . g r o u p i n g ( ) . r e p l a c e ( / , / g ,   " " ) , 
 	 	 t r a c k . t r a c k N u m b e r ( ) 
 	 ] 
 	 r e t u r n ( t r a c k _ i n f o ) 
 } ) 
 
 t r a c k _ i n f o s _ s   =   t r a c k _ i n f o s . j o i n ( " \ n " ) . t o S t r i n g ( ) 
 w r i t e T e x t T o F i l e ( t r a c k _ i n f o s _ s ,   " / U s e r s / d e v i n / M u s i c / s t a r r e d . c s v " ,   t r u e ) 
                              �jscr  ��ޭ