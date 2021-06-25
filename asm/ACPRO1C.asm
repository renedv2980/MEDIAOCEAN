*          DATA SET ACPRO1C    AT LEVEL 009 AS OF 09/12/02                      
*PHASE T60B1CA,*                                                                
         TITLE 'T60B1C - OFFICE LIST'                                           
T60B1C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B1C**,RA                                                    
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
*              HANDLE I/O FOR OFFICES                                           
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         CLI   ACOGRTYP,ACOGEQU                                                 
         BNE   *+12                                                             
         CLI   ACOGSREC,ACOGOFF    ARE WE HANDLING OFFICES YET?                 
         BE    LIST2                                                            
         LA    R2,PROOGRH          OPTIONAL OFFICE GROUP FILTER                 
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         MVI   OPTION,0                                                         
         GOTO1 VALOG                                                            
         SPACE 1                                                                
LIST1    XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         B     LIST3                                                            
         SPACE 1                                                                
LIST2    ZIC   R1,ACOGOFC+1                                                     
         LA    R1,1(R1)            INCREMENT OFFICE CODE                        
         STC   R1,ACOGOFC+1        TO READ PAST LAST ONE                        
         SPACE 1                                                                
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   ACOGKEY(ACOGOFC-ACOGKEY),KEYSAVE  CHECK C/B                      
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR+2                                                      
         L     R4,AIO                                                           
         MVC   0(2,R3),ACOGOFC     SHOW OFFICE CODE                             
         LA    R3,LISTAR+10                                                     
         MVI   ELCODE,ACGPELQ      GROUP CODE IF AROUND                         
         BAS   RE,GETELIO                                                       
         USING ACGPD,R6                                                         
         BNE   *+10                                                             
         MVC   0(1,R3),ACGPCODE                                                 
         CLI   PROOGR,X'41'        OPTIONAL GROUP FILTER                        
         BL    LIST8                                                            
         CLC   PROOGR(1),0(R3)                                                  
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST8    LA    R3,LISTAR+15        OFFICE NAME                                  
         GOTO1 SETNAME,DMCB,AIO,(R3)                                            
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
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROECD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACPRO1C   09/12/02'                                      
         END                                                                    
