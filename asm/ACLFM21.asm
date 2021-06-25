*          DATA SET ACLFM21    AT LEVEL 004 AS OF 08/10/00                      
*PHASE T60321A                                                                  
         TITLE 'STANDARD HOURS ELEMENTS'                                        
T60321   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM21*                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 2                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   HO20                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1R'                                                  
         CLI   MYANYKEY,C'Y'                                                    
         BE    XIT                                                              
         MVI   MYANYKEY,C'Y'       FIRST TIME SET FOR READ                      
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY ELEMENTS                                                 
         SPACE 2                                                                
HO20     CLI   MODE,DSPLYREC                                                    
         BNE   HO40                                                             
HO21     TWAXC LOGPERDH                                                         
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         USING ACHOD,R4                                                         
         LA    R2,LOGPERDH                                                      
         USING HOURD,R2                                                         
         SPACE 1                                                                
HO22     CLI   0(R4),0                                                          
         BE    HO28                                                             
         CLI   0(R4),X'17'         FIND HOURS ELEMENT                           
         BE    HO25                                                             
HO24     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HO22                                                             
         SPACE 2                                                                
HO25     MVC   WORK(2),ACHODATE    PUT OUT DATE                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,WORK+3)                                  
         MVC   HMNT,WORK+3                                                      
         OI    HMNTH+6,X'80'                                                    
         LA    R5,HPER1H                                                        
         LA    R3,5                                                             
         LA    R6,ACHOURS                                                       
         SPACE 1                                                                
HO26     EDIT  (P4,0(R6)),(7,8(R5)),2,ALIGN=LEFT,ZERO=BLANK                     
         OI    6(R5),X'80'                                                      
         ZIC   R1,0(R5)                                                         
         AR    R5,R1               NEXT FIELD HEADER                            
         LA    R6,4(R6)            NEXT PERIOD BUCKET                           
         BCT   R3,HO26                                                          
         LA    R2,HOURLEN(R2)                                                   
         CLI   0(R2),9             END OF SCREEN                                
         BNE   HO24                                                             
         SPACE 1                                                                
HO28     CLI   LOGACT,C'I'                                                      
         BNE   XIT                                                              
         LA    R2,LOGRECH                                                       
HO31     OI    6(R2),X'40'         INSERT CURSOR                                
         B     XIT                                                              
         EJECT                                                                  
*              BUILD ELEMENTS                                                   
         SPACE 2                                                                
HO40     GOTO1 REMANEL,DMCB,(X'17',0)                                           
         LA    R2,LOGPERDH                                                      
HO42     XC    ELEMENT,ELEMENT                                                  
         CLI   5(R2),0                                                          
         BE    HO21                                                             
         LA    R4,ELEMENT                                                       
         USING ACHOD,R4                                                         
         MVC   ACHOEL(2),=X'1718'                                               
         SPACE 2                                                                
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK      VALIDATE DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    BADATE                           INVALID DATE FORMAT             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACHODATE,WORK+6             DATE TO ELEMENT                      
         MVC   ACHOURS(20),=5PL4'0'                                             
         LA    R2,HPER1H                                                        
         LA    R3,5                                                             
         LA    R6,ACHOURS                                                       
HO45     CLI   5(R2),0                                                          
         BE    HO46                                                             
         GOTO1 VALICASH                                                         
         CP    DUB,=P'50000'                                                    
         BH    BADAMT                     HOURS INVALID                         
         ZAP   0(L'ACHOURS,R6),DUB        HOURS TO ELEMENT                      
HO46     LA    R6,L'ACHOURS(R6)                                                 
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,HO45                                                          
         CLC   ACHOURS(20),=5PL4'0'                                             
         BE    HO47                                                             
         GOTO1 ADDANEL                                                          
HO47     CLI   0(R2),9                                                          
         BNE   HO42                                                             
         B     HO21                                                             
         SPACE 1                                                                
BADATE   MVI   ERROR,13            INVALID DATE FORMAT                          
         B     HO31                                                             
BADAMT   MVI   ERROR,2             INVALID AMOUNT                               
         B     HO31                                                             
INVINP   MVI   ERROR,2                                                          
         B     HO31                                                             
         SPACE 3                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              DSECT FOR A LINE ON SCREEN                                       
HOURD    DSECT                                                                  
HMNTH    DS    CL8                                                              
HMNT     DS    CL6                                                              
HPER1H   DS    CL8                                                              
HPER1    DS    CL7                                                              
HPER2H   DS    CL8                                                              
HPER2    DS    CL7                                                              
HPER3H   DS    CL8                                                              
HPER3    DS    CL7                                                              
HPER4H   DS    CL8                                                              
HPER4    DS    CL7                                                              
HPER5H   DS    CL8                                                              
HPER5    DS    CL7                                                              
HOURLEN  EQU   *-HOURD                                                          
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME2D                                                       
         ORG   LOGHEADH                                                         
         DS    1000C                                                            
MYANYKEY DS    CL1                                                              
*        ACLFMWORK                                                              
*        ACGENBOTH                                                              
*        ACLFMEQU                                                               
*        DDLFDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACLFM21   08/10/00'                                      
         END                                                                    
