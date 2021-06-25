*          DATA SET ACPRO1E    AT LEVEL 025 AS OF 09/12/02                      
*PHASE T60B1EA,*                                                                
         TITLE 'T60B1E - WORK CODE LIST'                                        
T60B1E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B1E**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR WORK CODES                                        
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         CLI   KEY,X'0A'           ARE WE HANDLING WORK CODES YET               
         BE    LIST2                                                            
         LA    R2,PROWGRH          OPTIONAL WORK GROUP FILTER                   
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         MVI   OPTION,0                                                         
         GOTO1 VALWG                                                            
         SPACE 1                                                                
LIST1    MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),PROSTA                                                  
         B     LIST3                                                            
         SPACE 1                                                                
LIST2    ZIC   R1,KEY+5                                                         
         LA    R1,1(R1)            FORCE READ OF NEXT WORKCODE                  
         STC   R1,KEY+5                                                         
         SPACE 1                                                                
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(4),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR+1                                                      
         L     R4,AIO                                                           
         MVC   0(2,R3),4(R4)       SHOW WORK CODE                               
         LA    R3,LISTAR+8                                                      
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETELIO                                                       
         USING ACANALD,R6                                                       
         MVC   0(1,R3),ACANGRUP    WORK CODE GROUP                              
         CLI   PROWGR,X'41'        OPTIONAL GROUP FILTER                        
         BL    LIST8                                                            
         CLC   PROWGR(1),0(R3)                                                  
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST8    LA    R3,LISTAR+13        DESCRIPTION                                  
         MVC   0(15,R3),ACANDESC                                                
         LA    R3,LISTAR+29        COMMISSION                                   
         MVI   0(R3),C'Y'                                                       
         TM    ACANSTAT,X'02'                                                   
         BNO   *+8                                                              
         MVI   0(R3),C'N'                                                       
         LA    R3,LISTAR+35        TYPE                                         
         MVC   0(L'ACANTYPE,R3),ACANTYPE                                        
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         MVI   0(R3),C'O'                                                       
         LA    R3,LISTAR+42        MEDIA TRANSFER                               
         MVI   0(R3),C'N'                                                       
         TM    ACANSTAT,X'01'                                                   
         BZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         LA    R3,LISTAR+49        STATUS                                       
         MVI   0(R3),C'A'                                                       
         TM    ACANSTA2,X'80'                                                   
         BZ    *+8                                                              
         MVI   0(R3),C'I'                                                       
         LA    R3,LISTAR+57        ADJUSTMENT                                   
         MVI   0(R3),C'N'                                                       
         TM    ACANSTA2,X'40'                                                   
         BZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         LA    R3,LISTAR+65        INTERNAL                                     
         MVI   0(R3),C' '                                                       
         TM    ACANSTA2,X'20'                                                   
         BZ    *+8                                                              
         MVI   0(R3),C'I'                                                       
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROEED                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACPRO1E   09/12/02'                                      
         END                                                                    
