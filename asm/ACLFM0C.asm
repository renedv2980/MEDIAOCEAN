*          DATA SET ACLFM0C    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T6030CA,+0                                                               
         TITLE 'MODULE TO HANDLE RULES ELEMENTS'                                
T6030C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFMC**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         MVI   ERROR,X'FF'                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 3                                                                
         LA    R2,LOGCLIH                                                       
         CLI   MODE,BUILDKEY                                                    
         BNE   RL20                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 READ                                                             
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         USING ACCOMPD,R4                                                       
         MVC   SAVEPROD,ACMPJOB                                                 
         SPACE 2                                                                
RL2      MVC   KEY,SPACES          HANDLE CLIENT                                
         MVC   KEY(1),COMPANY                                                   
         TM    4(R2),X'20'                                                      
         BO    RL4                                                              
         FOUT  LOGCLINH,SPACES,36                                               
         FOUT  LOGPRDNH,SPACES,36                                               
         FOUT  LOGJOBNH,SPACES,36                                               
         MVI   ANYKEY,C'Y'                                                      
         NI    LOGPRDH+4,X'DF'                                                  
         NI    LOGJOBH+4,X'DF'                                                  
         SPACE 2                                                                
RL4      GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),=CL6'ALL'                                                
         BE    ALLOUT1                                                          
         BAS   RE,GETHEIR          MAKE SUR HEIRARCHY IS IN TWA                 
         LA    R4,SAVEHEIR                                                      
         USING ACHEIRD,R4                                                       
         MVC   KEY+1(2),SAVEPROD                                                
         MVC   KEY+3(12),WORK                                                   
         TM    4(R2),X'20'                                                      
         BO    RL6                                                              
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         SPACE 2                                                                
RL6      LA    R2,LOGPRDH          HANDLE PRODUCT                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                                                      
         BO    RL8                                                              
         FOUT  LOGPRDNH,SPACES,36                                               
         FOUT  LOGJOBNH,SPACES,36                                               
         NI    LOGJOBH+4,X'0F'                                                  
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
RL8      CLC   WORK(6),=CL6'ALL'                                                
         BE    ALLOUT2                                                          
         SR    R3,R3                                                            
         IC    R3,ACHRLEVA                                                      
         LA    R5,KEY+3(R3)                                                     
         MVC   0(6,R5),WORK                                                     
         TM    4(R2),X'20'                                                      
         BO    RL10                                                             
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         SPACE 2                                                                
RL10     LA    R2,LOGJOBH          HANDLE JOB                                   
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                                                      
         BO    RL12                                                             
         FOUT  LOGJOBNH,SPACES,36                                               
         MVI   ANYKEY,C'Y'                                                      
         NI    LOGMEDH+4,X'FF'-X'20'   VALIDATE MEDIA                           
         SPACE 2                                                                
RL12     CLC   WORK(6),=CL6'ALL'                                                
         BE    ALLOUT3                                                          
         SR    R3,R3                                                            
         IC    R3,ACHRLEVB                                                      
         LA    R5,KEY+3(R3)                                                     
         MVC   0(6,R5),WORK                                                     
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         SPACE 2                                                                
ALLOUT1  DS    0H                                                               
         FOUT  LOGPRDH,=CL6'ALL',6                                              
         SPACE 2                                                                
ALLOUT2  DS    0H                                                               
         FOUT  LOGJOBH,=CL6'ALL',6                                              
         SPACE 2                                                                
ALLOUT3  OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              BUILD AN ELEMENT FIRST                                           
         SPACE 3                                                                
RL20     LA    R4,ELEMENT                                                       
         USING ACRULED,R4                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACRLEL,X'42'                                                     
         MVI   ACRLLEN,X'0A'                                                    
         MVC   ACRLCOMM(5),=5X'FF'                                              
         LA    R2,LOGMEDH          MEDIA CODE OR ALL                            
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                                                      
         BO    RL24                                                             
         FOUT  LOGMEDNH,SPACES,15                                               
         CLC   WORK(6),=CL6'ALL'                                                
         BE    RL26                                                             
         CLC   LOGJOB(3),=C'ALL'   ARE WE SPECIFYING A UNIQUE JOB ?             
         BE    RL21                NO                                           
         CLC   LOGMED(1),LOGJOB    YES, IS IT SAME AS 1ST DIGIT OF JOB?         
         BE    RL21                YES, OK                                      
         MVI   ERROR,INVALID       NO, ERROR                                    
         B     XIT                                                              
         SPACE 2                                                                
RL21     MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),LOGMED                                                  
         GOTO1 READ                                                             
         LA    R5,IO                                                            
         AH    R5,DATADISP                                                      
         SR    R3,R3                                                            
         B     RL23                                                             
         SPACE 2                                                                
RL22     IC    R3,1(R5)                                                         
         AR    R5,R3                                                            
         SPACE 2                                                                
RL23     CLI   0(R5),0                                                          
         BE    RLMISS                                                           
         CLI   0(R5),X'11'                                                      
         BNE   RL22                                                             
         USING ACMEDIAD,R5                                                      
         CLC   ACMDCODE,LOGMED                                                  
         BNE   RL22                                                             
         MVC   LOGMEDN,ACMDDESC                                                 
         SPACE 2                                                                
RL24     CLC   WORK(6),=CL6'ALL'                                                
         BE    RL26                                                             
         MVC   ACRLMED,LOGMED                                                   
         SPACE 2                                                                
RL26     OI    4(R2),X'20'                                                      
         LA    R2,LOGWRKH          WORK CODE OR ALL                             
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                                                      
         BO    RL28                                                             
         FOUT  LOGWRKNH,SPACES,15                                               
         CLC   WORK(6),=CL6'ALL'                                                
         BE    RL30                                                             
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),SAVEPROD                                                
         MVC   KEY+4(2),WORK                                                    
         GOTO1 READ                                                             
         LA    R5,IO                                                            
         AH    R5,DATADISP                                                      
         USING ACANALD,R5                                                       
         MVC   LOGWRKN,ACANDESC                                                 
         SPACE 2                                                                
RL28     CLC   WORK(6),=CL6'ALL'                                                
         BE    RL30                                                             
         MVC   ACRLWORK,WORK                                                    
         SPACE 2                                                                
RL30     LA    R5,IO2              NOW LOOK FOR MATCH                           
         AH    R5,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
RL32     CLI   0(R5),0                                                          
         BE    RL36                                                             
         CLI   0(R5),X'42'                                                      
         BNE   RL34                                                             
         CLC   2(3,R5),2(R4)                                                    
         BE    RL38                                                             
         SPACE 2                                                                
RL34     IC    R3,1(R5)                                                         
         AR    R5,R3                                                            
         B     RL32                                                             
         SPACE 2                                                                
RL36     CLI   MODE,NEWELEM        NO MATCH FOUND                               
         BE    RL42                                                             
         MVI   ERROR,NOELEM        ONLY OK FOR NEW                              
         B     XIT                                                              
         SPACE 2                                                                
RL38     CLI   MODE,NEWELEM        MATCH FOUND                                  
         BNE   RL40                                                             
         MVI   ERROR,RECONFLE                                                   
         B     XIT                                                              
         SPACE 2                                                                
RL40     CLI   MODE,DSPLYREC                                                    
         BE    RL50                                                             
         MVI   0(R5),X'FF'         REMOVE FOR CHANGE                            
         GOTO1 REMANEL,DMCB,0                                                   
         SPACE 2                                                                
RL42     LA    R2,LOGCOMMH         AND ADD FOR NEW OR CHANGE                    
         CLI   LOGCOMMH+5,0                                                     
         BNE   RL44                                                             
*&&US                                                                           
         CLI   MODE,NEWELEM                                                     
         BNE   *+12                IF 'NEW' AND COMMISSION NOT ENTERED          
         MVI   ERROR,MISSING       PRINT AN ERROR                               
         B     XIT                                                              
*&&                                                                             
         SPACE                                                                  
         CLI   LOGTAXH+5,0         IF BOTH INPUTS ARE MISSING                   
         BE    XIT                 ELEMENT CAN BE DELETED                       
         B     RL46                                                             
         SPACE 2                                                                
RL44     SR    R3,R3               VALIDATE COMMISSION RATE                     
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,X'FF'                                                       
         BNE   RL45                                                             
*&&US                                                                           
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R3)   MIGHT HAVE 4 DECIMALS              
         CLI   DMCB,X'FF'                                                       
         BNE   RL444                                                            
*&&                                                                             
         MVI   ERROR,NOTNUMRC                                                   
         B     XIT                                                              
         SPACE 1                                                                
RL444    MVI   ACRLLEN,X'0B'     ELEMENT LENGTH IS 11 IF 4 DECIMALS             
         MVI   ACRLSTAT,0                                                       
         OI    ACRLSTAT,X'80'                                                   
         SPACE 2                                                                
RL45     L     R3,DMCB+4                                                        
         CVD   R3,DUB                                                           
*&&US                                                                           
         LR    R7,RA                                                            
         USING TWAD,R7                                                          
         CLI   TWAOFFC,C'*'        IS IT A DDS TERMINAL                         
         BE    RL45B               YES--OK TO HAVE PCT OVER 99(DDS0NLY)         
         TM    ACRLSTAT,X'80'      IS IT 4 DECIMAL PCT                          
         BZ    RL45A               NO IT MUST BE TWO                            
         CP    DUB,=P'990000'      IS IT OVER 99 PCT (4 DEC)                    
         BH    RLPCTERR            YES ERROR MESSAGE                            
         B     RL45B                                                            
RL45A    CP    DUB,=P'10000'       IS IT OVER 100PCT (TWO DEC)                  
         BH    RLPCTERR            YES ERROR MESSAGE                            
*&&                                                                             
         SPACE 1                                                                
RL45B    MVC   ACRLCOMM,DUB+4                                                   
         SPACE 2                                                                
RL46     LA    R2,LOGTAXH          VALIDATE TAX RULE                            
         CLC   COUNTRY,=C'UK'                                                   
         BNE   RL48                                                             
         CLI   LOGTAXH+5,0                                                      
         BE    RL48                                                             
         CLI   LOGTAX,C' '                                                      
         BE    RL48                                                             
         MVC   ACRLTAX,LOGTAX                                                   
         CLI   ACRLTAX,C'S'                                                     
         BE    RL48                                                             
         CLI   ACRLTAX,C'Z'                                                     
         BE    RL48                                                             
         CLI   ACRLTAX,C'X'                                                     
         BE    RL48                                                             
         MVI   ERROR,NOTVLCDE                                                   
         B     XIT                                                              
         SPACE 2                                                                
RL48     GOTO1 ADDANEL                                                          
         B     XIT                                                              
         SPACE 2                                                                
RL50     LR    R4,R5                                                            
         LA    R2,LOGCOMMH         DISPLAY COMMISSION                           
         FOUT  LOGCOMMH,SPACES,5                                                
         CLI   ACRLCOMM,X'FF'                                                   
         BE    RL52                                                             
         CLI   ACRLLEN,X'0B'                                                    
         BL    RL51                                                             
         TM    ACRLSTAT,X'80'                                                   
         BZ    RL51                BRANCH IF NOT 4 DECIMAL PLACES               
         EDIT  (P4,ACRLCOMM),(8,8(R2)),4,ALIGN=LEFT,DROP=1                      
         B     RL51A                                                            
RL51     EDIT  (P4,ACRLCOMM),(8,8(R2)),2,ALIGN=LEFT                             
RL51A    CLI   12(R2),C' '                                                      
         BNE   RL52                                                             
         FOUT  LOGCOMMH,LOGCOMM,4                                               
         CP    ACRLCOMM,=P'0'                                                   
         BNE   RL52                                                             
         MVC   LOGCOMM(4),=C'ZERO'                                              
         SPACE 2                                                                
RL52     FOUT  LOGTAXH,SPACES,1                                                 
         CLI   ACRLTAX,X'FF'                                                    
         BE    RL54                                                             
         CLC   COUNTRY,=C'UK'                                                   
         BNE   RL54                                                             
         MVC   LOGTAX(1),ACRLTAX                                                
         SPACE 2                                                                
RL54     LA    R2,LOGCOMMH                                                      
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
RLMISS   MVI   ERROR,NOELEM                                                     
         B     XIT                                                              
         SPACE 1                                                                
*&&US                                                                           
RLPCTERR LA    R2,LOGCOMMH         SET CURSOR TO COMMISH RATE                   
         MVC   LOGHEAD(25),=C'** ERROR - OVER 99 PCT **'                        
         MVI   ERROR,X'FE'         I SET ERROR MESSAGE                          
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              ROUTINE TO GET THE HEIRARCHY ELEMENT                             
         SPACE 3                                                                
GETHEIR  NTR1                                                                   
         MVI   LEVEL,3                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),SAVEPROD                                                
         GOTO1 READ                                                             
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
GH2      CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'16'                                                      
         BE    GH4                                                              
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     GH2                                                              
         SPACE 2                                                                
GH4      MVC   SAVEHEIR,0(R4)                                                   
         MVI   LEVEL,2                                                          
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF3D                                                       
SAVEPROD DS    CL2                                                              
SAVEHEIR DS    CL80                                                             
       ++INCLUDE ACLFMWORK                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACLFM0C   05/01/02'                                      
         END                                                                    
