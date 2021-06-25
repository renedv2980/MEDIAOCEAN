*          DATA SET ACPRO63    AT LEVEL 002 AS OF 06/02/99                      
*PHASE T60B63A,*                                                                
         TITLE 'T60B63 - JOB GROUP LIST'                                        
T60B63   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B63**,RA                                                    
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
*              HANDLE I/O FOR JOB GROUPS                                        
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING JGRRECD,R4                                                       
         CLI   JGRKTYP,JGRKTYPQ    ALREADY READING JOB GROUP RECS?              
         BNE   *+12                                                             
         CLI   JGRKSUB,JGRKSUBQ                                                 
         BE    LIST3                                                            
         XC    JGRKEY,JGRKEY                                                    
         MVI   JGRKTYP,JGRKTYPQ    X'2C'                                        
         MVI   JGRKSUB,JGRKSUBQ    X'12'                                        
         MVC   JGRKCPY,CUL         COMPANY CODE                                 
         MVC   JGRKUNT(2),CUL+1    UNIT/LEDGER                                  
*                                                                               
         OC    PROSTA,PROSTA       ANY START AT FILTER                          
         BZ    *+10                                                             
         MVC   JGRKCODE,PROSTA     START AT FILTER                              
*                                                                               
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   JGRKEY(JGRKCODE-JGRKEY),KEYSAVE                                  
         BNE   XIT                                                              
*                                                                               
         USING LINED,R3                                                         
         LA    R3,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         L     R4,AIO                                                           
         MVC   LINCODE,JGRKCODE    JOB GROUP CODE                               
         LA    R3,LINNAME                                                       
         GOTO1 SETNAME,DMCB,AIO,(R3)                                            
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
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
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROBAD                                                       
         SPACE 2                                                                
***********************************************************************         
* LIST LINE DSECT                                                               
***********************************************************************         
*                                                                               
LINED    DSECT                                                                  
LINCODE  DS    CL8                                                              
         DS    CL4                                                              
LINNAME  DS    CL36                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACPRO63   06/02/99'                                      
         END                                                                    
