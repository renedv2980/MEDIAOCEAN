*          DATA SET ACPRO19    AT LEVEL 012 AS OF 02/01/08                      
*PHASE T60B19A                                                                  
         TITLE 'T60B19 - COMMENT LIST'                                          
T60B19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B19**,RA                                                    
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
*              HANDLE I/O FOR COMMENT RECORDS                                   
         SPACE 3                                                                
         CLI   KEY,SCMKTYPQ        ARE WE HANDLING COMMENTS YET                 
         BE    LIST2                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,SCMKTYPQ                                                     
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(6),SPACES                                                  
         LA    R3,KEY+2+6          CODES ARE RIGHT ALIGNED                      
         LA    R2,PROSTAH                                                       
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    LIST2                                                            
         SR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PROSTA                                                   
         SPACE 1                                                                
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(2),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
*                                                                               
         USING SCMRECD,R4                                                       
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         L     R4,AIO                                                           
         MVC   0(6,R3),SCMKCODE    SHOW COMMENT CODE                            
*                                                                               
         TM    SCMRSTAT,SCMKSABO   ALLOWED FOR BO?                              
         BZ    *+10                YES                                          
         MVC   LISTAR+28(3),=C'YES'                                             
         LA    R3,LISTAR+7                                                      
         MVI   ELCODE,COIELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   LIST8                                                            
         USING COIELD,R6                                                        
         MVC   0(4,R3),COIFILT     COMMENT FILTER                               
         MVC   5(15,R3),COIDESC            AND DESCRIPTION                      
         SPACE 1                                                                
LIST8    CLI   PROFILTH+5,0        WAS A COMMENT FILTER REQUESTED               
         BE    LIST14                                                           
         LA    RE,PROFILT                                                       
         LR    RF,R3                                                            
         ZIC   R0,PROFILTH+5                                                    
         SPACE 1                                                                
LIST10   CLI   0(RE),C'*'          UNLESS COLUMN IS 'WILD'                      
         BE    LIST12                                                           
         CLI   0(RE),X'41'                                                      
         BL    LIST12                                                           
         CLC   0(1,RE),0(RF)       CHECK MATCH AGAINST RECORD FILTER            
         BNE   LIST4                                                            
         SPACE 1                                                                
LIST12   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LIST10                                                        
         SPACE 1                                                                
LIST14   LA    R3,LISTAR+32        START OF TEXT                                
         MVI   ELCODE,SCMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   LIST16                                                           
         USING SCMELD,R6                                                        
         ZIC   R1,SCMLN                                                         
         SH    R1,=H'5'                                                         
         CH    R1,=H'35'                                                        
         BL    *+8                                                              
         LA    R1,35                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SCMNARR                                                  
         SPACE 1                                                                
LIST16   GOTO1 LISTMON                                                          
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
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROE9D                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACPRO19   02/01/08'                                      
         END                                                                    
