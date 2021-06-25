*          DATA SET DEISSCREEN AT LEVEL 013 AS OF 09/27/00                      
*PHASE DEISSCRN                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'FIND SCREEN MEMBERS ON PAN'                                     
DEISSCRN CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,*DEISSC*,=V(REGSAVE)                                           
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         MVC   TITLE(12),=C'FIND SCREENS'                                       
         EJECT                                                                  
* MAIN LOGIC FLOW                                                               
*                                                                               
RE10     GOTO1 =V(CARDS),DMCB,P,=C'RE00'                                        
         CLC   P(2),=C'/*'         TEST FOR END OF FILE                         
         BE    EXIT                                                             
         MVC   BOOKNAME,P                                                       
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',BOOKNAME,CARD            
         TM    DMCB+8,X'10'        TEST PAN BOOK FOUND                          
         BZ    RE20                                                             
         MVC   P(23),=C'PAN-NAME NOT IN LIBRARY'                                
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RE20     CLC   =C'DC',BOOKNAME     IF BOOKNAME STARTS WITH DC                   
         BNE   RE30                                                             
         CLI   CARD,C'<'            AND HAS FORMATTING CHARACTER                
         BE    RE10                                                             
         CLC   =C'TITLE',CARD+9     OR  'TITLE' IS IN COLUMN 10                 
         BE    RE10                                                             
*                                                                               
RE30     CLC   =C'*PHASE',CARD                                                  
         BE    RE10                                                             
         CLC   =C'*CATALP',CARD                                                 
         BE    RE10                                                             
*                                                                               
         CLC   =C'SRCON',BOOKNAME                                               
         BE    RE10                                                             
         CLC   =C'SRNOP',BOOKNAME                                               
         BE    RE10                                                             
         CLC   =C'SRPWD',BOOKNAME                                               
         BE    RE10                                                             
         CLC   =C'SQL',BOOKNAME                                                 
         BE    RE10                                                             
         CLC   =C'CC',BOOKNAME                                                  
         BE    RE10                                                             
         CLC   =C'FANEW',BOOKNAME                                               
         BE    RE10                                                             
         CLC   =C'LAR',BOOKNAME                                                 
         BE    RE10                                                             
*                                                                               
         CLI   CARD,C'S'           SCREENS START WITH 'S'                       
         BNE   *+12                                                             
         CLI   CARD+4,C'T'         AND HAVE 'T' IN COLUMN 5                     
         BE    RE10                                                             
*                                                                               
         MVC   P(10),BOOKNAME                                                   
         MVC   P+12(80),CARD                                                    
         GOTO1 =V(PRINTER)                                                      
         B     RE10                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(DEISSCRN),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
BYTE     DS    X                                                                
BOOKNAME DC    CL10' '             NAME OF PANVALET BOOK                        
WORK     DS    XL132               WORK AREA                                    
*                                                                               
         DS    0D                                                               
         DC    C'**CARD**'                                                      
CARD     DS    CL80                INPUT SOURCE CARD                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013DEISSCREEN09/27/00'                                      
         END                                                                    
