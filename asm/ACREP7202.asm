*          DATA SET ACREP7202  AT LEVEL 020 AS OF 08/16/00                      
         TITLE 'LIST OF ACCOUNT RULES'                                          
*PHASE AC7202A                                                                  
AC7202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7202                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC72D,RC                                                         
         SPACE 1                                                                
AC72     CLI   MODE,REQFRST                                                     
         BNE   AC72A                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         L     R3,ADCMPNAM                                                      
         L     R7,ADCOMP                                                        
         B     PRULES                                                           
         SPACE 1                                                                
AC72A    CLI   MODE,PROCLEVA                                                    
         BNE   AC72B                                                            
         L     R3,ADLVANAM                                                      
         L     R7,ADHEIRA                                                       
         B     PRULES                                                           
         SPACE 1                                                                
AC72B    CLI   MODE,PROCLEVB                                                    
         BNE   AC72C                                                            
         L     R3,ADLVBNAM                                                      
         L     R7,ADHEIRB                                                       
         B     PRULES                                                           
         SPACE 1                                                                
AC72C    CLI   MODE,PROCACC                                                     
         BNE   ACCXIT                                                           
         L     R3,ADACCNAM                                                      
         L     R7,ADACC                                                         
         B     PRULES                                                           
         SPACE 1                                                                
PRULES   ST    R7,AREC                                                          
         AH    R7,DATADISP                                                      
         SR    R6,R6                                                            
         MVI   FOUND,0             SET TO NOT-FOUND                             
         SPACE 1                                                                
ACCRU6   CLI   0(R7),X'42'         RULES ELEMENT                                
         BE    ACCRU10                                                          
         CLI   0(R7),0             END OF RECORD                                
         BE    ACCRU16                                                          
         SPACE 1                                                                
ACCRU8   IC    R6,1(R7)            BUMP TO NEXT ELEMENT                         
         AR    R7,R6                                                            
         B     ACCRU6              AND GO FOR MORE                              
         EJECT                                                                  
ACCRU10  TM    FOUND,X'01'                                                      
         BO    ACCRU11                                                          
         L     R2,AREC                                                          
         MVC   P+1(14),1(R2)                                                    
         USING ACNAMED,R3                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),ACNMNAME                                                 
         SPACE 1                                                                
*              FORMAT THIS RULES ELEMENT                                        
         SPACE 1                                                                
ACCRU11  OI    FOUND,X'01'         SET ON FOUND BIT                             
         USING ACRULED,R7                                                       
         MVC   P+56(1),ACRLMED                                                  
         CLI   ACRLMED,0                                                        
         BNE   *+10                                                             
         MVC   P+56(3),=C'ALL'                                                  
         MVC   P+64(2),ACRLWORK                                                 
         CLI   ACRLWORK,0                                                       
         BNE   *+10                                                             
         MVC   P+64(3),=C'ALL'                                                  
         CLI   ACRLCOMM,X'FF'                                                   
         BE    ACCRU12                                                          
         CP    ACRLCOMM,=P'0'                                                   
         BNE   *+14                                                             
         MVC   P+70(4),=C'ZERO'    IF NIL DO A PRETTY                           
         B     ACCRU12                                                          
         CLI   ACRLLEN,X'0B'                                                    
         BL    ACCRU112                                                         
         TM    ACRLSTAT,X'80'      4DP                                          
         BZ    ACCRU112                                                         
         EDIT  (P4,ACRLCOMM),(7,P+70),4,DROP=1                                  
         B     ACCRU12                                                          
ACCRU112 EDIT  (P4,ACRLCOMM),(6,P+69),2                                         
         SPACE 1                                                                
ACCRU12  CLI   ACRLTAX,X'FF'                                                    
         BE    ACCRU14                                                          
         MVC   P+78(4),=C'ZERO'                                                 
         CLI   ACRLTAX,C'Z'                                                     
         BE    ACCRU14                                                          
         MVC   P+78(6),=C'EXEMPT'                                               
         CLI   ACRLTAX,C'X'                                                     
         BE    ACCRU14                                                          
         MVC   P+78(8),=C'STANDARD'                                             
         SPACE 1                                                                
ACCRU14  GOTO1 ACREPORT                                                         
         B     ACCRU8                                                           
         EJECT                                                                  
*              END/EXIT                                                         
ACCRU16  CLI   FOUND,0             PRINT A BLANK LINE IF                        
         BE    ACCXIT              WE ACTUALLY PRINTED SOME RULES               
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
ACCXIT   XIT1                                                                   
         EJECT                                                                  
AC72D    DSECT                                                                  
FOUND    DS    CL1                                                              
AREC     DS    F                                                                
         SPACE 2                                                                
* ACGENBOTH                                                                     
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREP7202 08/16/00'                                      
         END                                                                    
