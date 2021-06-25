*          DATA SET ACPRO1B    AT LEVEL 008 AS OF 12/13/89                      
*PHASE T60B1BA,*                                                                
         TITLE 'T60B1B - OFFICE GROUP LIST'                                     
T60B1B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B1B**,RA                                                    
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
*              HANDLE I/O FOR OFFICE GROUPS                                     
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         CLI   ACOGRTYP,ACOGEQU                                                 
         BNE   *+12                                                             
         CLI   ACOGSREC,ACOGOG     ARE WE HANDLING GROUPS YET                   
         BE    LIST2                                                            
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGCODE,PROSTA                                                  
         B     LIST3                                                            
         SPACE 1                                                                
LIST2    ZIC   R1,ACOGCODE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ACOGCODE                                                      
         SPACE 1                                                                
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   ACOGKEY(ACOGCODE-ACOGKEY),KEYSAVE  CHECK C/B                     
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR+2                                                      
         L     R4,AIO                                                           
         MVC   0(1,R3),ACOGCODE    SHOW GROUP CODE AND NAME                     
         LA    R3,LISTAR+7                                                      
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
       ++INCLUDE ACPROEBD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO1B   12/13/89'                                      
         END                                                                    
