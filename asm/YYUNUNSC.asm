*          DATA SET YYUNUNSC   AT LEVEL 114 AS OF 08/16/00                      
*PHASE YYUNUNSA                                                                 
*INCLUDE UNSCANA                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TESTBOOK--ORGANIZE THE INVOICE RECORDS OF A BOOKSTORE'          
*******************************************************************             
         EJECT                                                                  
TESTBOOK CSECT                                                                  
*        PRINT NOGEN                                                            
         NBASE 0,*TESTBOOK,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         MVI   DMCB+12,1                                                        
         MVC   DMCB+13,=C'$LT'                                                  
*                                                                               
         GOTO1 =V(UNSCAN),DMCB,(3,B1),(X'FF',FIELDH),0                          
         MVC   P(L'FIELD),FIELD                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
DMCB     DS    8F                                                               
FIELDH   DC    X'44',XL7'00'                                                    
FIELD    DS    CL80                                                             
B1       DC    C'A',C'01'                                                       
         DC    C'B',C'02'                                                       
         DC    C'C',C'03'                                                       
         DC    C'D',C'04'                                                       
         DC    C'E',C'05'                                                       
         DC    C'F',C'06'                                                       
         DC    C'G',C'07'                                                       
         DC    C'H',C'08'                                                       
         DC    C'I',C'09'                                                       
         DC    C'J',C'10'                                                       
B2       DC    C'A',C'1'                                                        
         DC    C'B',C'2'                                                        
         DC    C'C',C'3'                                                        
         DC    C'D',C'4'                                                        
         DC    C'E',C'5'                                                        
         DC    C'F',C'6'                                                        
         DC    C'G',C'7'                                                        
         DC    C'H',C'8'                                                        
         DC    C'I',C'9'                                                        
B3       DC    C'A',CL10' '                                                     
         DC    C'B',CL10' '                                                     
         DC    C'C',CL10' '                                                     
         DC    C'D',CL10' '                                                     
         DC    C'E',CL10' '                                                     
         DC    C'F',CL10' '                                                     
         DC    C'G',CL10' '                                                     
         DC    C'H',CL10' '                                                     
         DC    C'I',CL10' '                                                     
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114YYUNUNSC  08/16/00'                                      
         END                                                                    
                                                                                
