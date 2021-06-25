*          DATA SET SPSFM30    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T21730A                                                                  
*                                                                               
         TITLE 'SPSFM30 REASON LIST'                                            
T21730   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21730*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN10                                                           
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
MAIN10   CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       DS    0H                                                               
         LA    R2,SREMEDH                                                       
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
         XC    MYCODE,MYCODE                                                    
         LA    R2,SRECODEH         CHECK FOR START AT CODE                      
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYCODE(0),8(R2)                                                  
*                                                                               
VK10     XC    MYOFF,MYOFF                                                      
         LA    R2,SREOFFH          OFFICE FILTER                                
         CLI   5(R2),0                                                          
         BE    VK20                                                             
*******   VALIDATE OFFICE                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYOFF(0),8(R2)                                                   
*                                                                               
         USING RSNRECD,R4                                                       
VK20     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   RSNKTYPE,=X'0D77'   RECORD TYPE                                  
         MVC   RSNKMED,QMED        MEDIA                                        
         MVC   RSNKAGY,AGENCY      AGENCY                                       
         MVC   RSNKCODE,MYCODE     CODE                                         
         MVC   MYSVKEY,KEY                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        LIST RECORDS ROUTINE                                                   
*                                                                               
         USING LISTD,R2                                                         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING RSNRECD,R4                                                       
         OC    KEY(L'RSNKEY),KEY   FIRST TIME THROUGH                           
         BNZ   *+10                NO, READ WHERE LEFT OFF                      
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR                
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(RSNKCODE-RSNRECD),MYSVKEY  COMP UP TO CODE                   
         BNE   LRX                                                              
         OC    MYOFF,MYOFF         IS THERE AN OFFICE FILTER                    
         BZ    LR30                                                             
         CLC   RSNKOFF,MYOFF                                                    
         BNE   LR10                                                             
*                                                                               
LR30     MVC   LCODE,RSNKCODE      CODE                                         
         MVC   LOFF,RSNKOFF                                                     
         OC    RSNKOFF,RSNKOFF     OFFICE                                       
         BNZ   *+10                                                             
         MVC   LOFF,=C'ALL'                                                     
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSNEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR10                                                             
         MVC   LTEXT,RSNTEXT       TEXT                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR40                                                             
         GOTO1 LISTMON                                                          
         B     LR50                                                             
*                                                                               
LR40     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR50     B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'REASON LIST'                                             
         SSPEC H2,34,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,1,C'CODE'                                                     
         SSPEC H4,9,C'OFFICE'                                                   
         SSPEC H4,16,C'REASON TEXT'                                             
         SPACE 1                                                                
         SSPEC H5,1,C'----'                                                     
         SSPEC H5,9,C'------'                                                   
         SSPEC H5,16,C'-----------'                                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA0D          DSECT FOR RECORD LISTING.                    
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
         ORG   SYSSPARE                                                         
MYCODE   DS    CL6                                                              
MYOFF    DS    CL2                                                              
MYSVKEY  DS    CL(L'KEY)                                                        
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
LCODE    DS    CL6                                                              
         DS    CL2                                                              
LOFF     DS    CL3                                                              
         DS    CL4                                                              
LTEXT    DS    CL50                                                             
         EJECT                                                                  
       ++INCLUDE SPGENREAS                                                      
         EJECT                                                                  
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPSFM30   05/01/02'                                      
         END                                                                    
