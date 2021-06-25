*          DATA SET ACREP6202  AT LEVEL 060 AS OF 07/23/13                      
*PHASE AC6202C                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'INVENTORY BALANCE REPORT'                                       
AC6202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC62**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC62D,RC                                                         
         ST    R5,PRELOC                                                        
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   WP10                                                             
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
WP10     CLI   MODE,REQFRST                                                     
         BNE   WP20                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   REQTOT,=P'0'                                                     
         ZAP   REQNET,=P'0'                                                     
         ZAP   REQORIG,=P'0'                                                    
         ZAP   REQCURR,=P'0'                                                    
         CLC   QEND,SPACES                                                      
         BE    XIT                                                              
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,END)   CONVERT TO PACKED                 
         B     XIT                                                              
         SPACE 3                                                                
WP20     CLI   MODE,LEVAFRST                                                    
         BNE   WP40                                                             
         ZAP   CLITOT,=P'0'                                                     
         ZAP   CLINET,=P'0'                                                     
         ZAP   CLIORIG,=P'0'                                                    
         ZAP   CLICURR,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         CLI   PROGPROF,C'Y'                                                    
         BE    WP30                                                             
         CLI   PROGPROF,C'U'                                                    
         BNE   XIT                                                              
         MVI   RCSUBPRG,2                                                       
         B     XIT                                                              
WP30     MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
         SPACE 3                                                                
WP40     CLI   MODE,LEVBFRST                                                    
         BNE   WP60                                                             
         ZAP   PRODNET,=P'0'                                                    
         ZAP   PRODORIG,=P'0'                                                   
         ZAP   PRODCURR,=P'0'                                                   
         ZAP   PRODTOT,=P'0'                                                    
         MVI   PRODPRT,C'N'                                                     
         B     XIT                                                              
         SPACE 3                                                                
WP60     CLI   MODE,PROCACC                                                     
         BNE   WP70                                                             
         XC    LAST,LAST                                                        
         XC    LACT,LACT                                                        
         MVI   LACT+2,X'15'                                                     
         CLI   PROGPROF,C'Y'           NET FORMAT                               
         BE    *+12                                                             
         CLI   PROGPROF,C'U'           UNCOMMITTED FORMAT                       
         BNE   XIT                                                              
         ZAP   ORIGEST,=P'0'                                                    
         ZAP   CURREST,=P'0'                                                    
         ZAP   JOBNET,=P'0'                                                     
         BAS   RE,LOOKUP                                                        
         B     XIT                                                              
         EJECT                                                                  
WP70     CLI   MODE,PROCHIST       DO WE HAVE A HISTORY RECORD ?                
         BE    WP72                YES, GO USE CORRECT DSECT                    
         CLI   MODE,PROCTRNS       NO, IS IT A TRANSACTION ?                    
         BNE   WP80                NO, KEEP CHECKING                            
         L     R2,ADTRANS          YES, GET RECORD ADDRESS                      
         USING TRANSD,R2                                                        
         CLI   0(R2),X'44'         DOUBLE CHECK THE ELEMENT                     
         BNE   XIT                 SHOULDN'T REACH THIS POINT                   
         LR    RF,R2                                                            
         SH    RF,DATADISP         LOOK AT WORK CODE IN KEY                     
         USING ACKEYD,RF                                                        
         CLC   ACKEYWRK,=C'99'                                                  
         BNE   XIT                                                              
         CLI   PROGPROF,C'Y'                                                    
         BE    WP73                                                             
         CLI   PROGPROF,C'U'                                                    
         BE    WP73                                                             
         CLC   LAST,TRNSDATE       LATEST BILLING DATE                          
         BH    XIT                                                              
         MVC   LAST,TRNSDATE                                                    
         B     XIT                                                              
         SPACE 1                                                                
WP72     L     R2,ADTRANS                                                       
         CLI   0(R2),X'45'                                                      
         BNE   XIT                                                              
         CLI   PROGPROF,C'Y'                                                    
         BE    XIT              HISTORY NOT NEEDED FOR NET                      
         CLI   PROGPROF,C'U'                                                    
         BE    XIT              NOR FOR UNCOMMITTED FORMAT                      
         USING TRHISTD,R2                                                       
         CLC   LACT,TRHSYEAR       LATEST ACTIVE MONTH                          
         BH    XIT                                                              
         MVC   LACT,TRHSYEAR                                                    
         B     XIT                                                              
         SPACE 1                                                                
WP73     EQU   *                                                                
         USING TRANSD,R2                                                        
         CLC   QEND,SPACES                                                      
         BE    WP74                                                             
         CLC   END,TRNSDATE                                                     
         BNH   XIT                                                              
WP74     AP    JOBNET,TRNSAMNT          NET BILLING AMOUNT                      
         CLI   PROGPROF,C'U'                                                    
         BNE   *+10                                                             
         AP    JOBNET,TRNSNARR+15(6)    ADD IN COMMISION                        
         B     XIT                                                              
         EJECT                                                                  
WP80     CLI   MODE,ACCLAST                                                     
         BNE   WP100                                                            
         CLI   PROGPROF,C'Y'                                                    
         BE    WP81                                                             
         CLI   PROGPROF,C'U'                                                    
         BE    WP81                                                             
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         ZAP   DOUBLE,ACBLDR                                                    
         SP    DOUBLE,ACBLCR                                                    
         CP    DOUBLE,=P'0'                                                     
         BNE   WP82                                                             
         CLI   QOPT1,C'Y'          ALL-JOBS OPTION                              
         BNE   XIT                                                              
         B     WP82                                                             
         SPACE 2                                                                
WP81     AP    PRODNET,JOBNET                                                   
         AP    PRODORIG,ORIGEST                                                 
         AP    PRODCURR,CURREST                                                 
         EDIT  (P6,JOBNET),(11,P+70),2                                          
         SPACE 2                                                                
WP82     L     R2,ADLDGHIR                                                      
         USING ACHEIRD,R2                                                       
         CLI   PRODPRT,C'N'        PRODUCT PRINT PENDING                        
         BNE   WP84                                                             
         MVI   PRODPRT,C'Y'                                                     
         CLI   PROGPROF,C'N'                                                    
         BE    WP83                                                             
         MVC   HOLD,P        SAVE THIS JOB TOTALS                               
         MVI   P,X'40'        WHILE PRINTING NEW PRODUCT CODE                   
         MVC   P+1(L'P-1),P                                                     
WP83     L     R3,ADHEIRB                                                       
         ZIC   R4,ACHRLEVA                                                      
         AR    R3,R4                                                            
         MVC   P+1(6),3(R3)        PRODUCT CODE                                 
         LA    R5,P+8                                                           
         L     R6,ADLVBNAM                                                      
         BAS   RE,NAMOUT           PRODUCT NAME                                 
         GOTO1 =V(SQUASHER),DMCB,P+1,50,RR=PRELOC                               
         BAS   RE,CHOP                                                          
         BAS   RE,REPORT                                                        
         CLI   PROGPROF,C'N'                                                    
         BE    WP84                                                             
         MVC   P,HOLD                                                           
         SPACE 2                                                                
WP84     ZIC   R4,ACHRLEVB                                                      
         L     R3,ADHEIRC                                                       
         AR    R3,R4                                                            
         LR    R5,R4                                                            
         IC    R4,ACHRLEVC                                                      
         SR    R4,R5                                                            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),3(R3)        JOB CODE                                     
         LA    R5,P+8                                                           
         L     R6,ADACCNAM                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 =V(SQUASHER),DMCB,P+1,60,RR=PRELOC                               
         BAS   RE,CHOP                                                          
         CLI   PROGPROF,C'Y'                                                    
         BE    *+12                                                             
         CLI   PROGPROF,C'U'                                                    
         BNE   WP84A                                                            
         EDIT  (P6,ORIGEST),(11,P+40),2                                         
         EDIT  (P6,CURREST),(11,P+55),2                                         
         B     WP88                                                             
         SPACE 1                                                                
WP84A    EDIT  (P8,DOUBLE),(11,P+82),2,MINUS=YES                                
         AP    PRODTOT,DOUBLE                                                   
         OC    LAST,LAST                                                        
         BZ    WP85                                                             
         GOTO1 DATCON,DMCB,(1,LAST),(8,P+100)                                   
WP85     OC    LACT,LACT                                                        
         BZ    WP86                                                             
         GOTO1 DATCON,DMCB,(1,LACT),(9,P+70)                                    
WP86     L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLC   QEND,SPACES                                                      
         BE    WP86A                                                            
         OC    LACT,LACT           IF NO ACTIVE MONTH                           
         BNZ   *+10                                                             
         MVC   LACT(2),ACSTLAST    USE LAST ACTIVE DATE                         
         CLC   LACT(2),END                                                      
         BNH   WP86A                                                            
         MVC   P,SPACES                                                         
         SP    PRODTOT,DOUBLE                                                   
         B     XIT                                                              
WP86A    MVC   SAVESTAT,ACSTSTAT                                                
         GOTO1 DATCON,DMCB,(1,ACSTBFDT),(8,P+39)                                
         GOTO1 DATCON,DMCB,(1,ACSTLAST),(8,P+59)                                
         L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
WP87     CLI   0(R2),0                                                          
         BE    WP88                                                             
         CLI   0(R2),X'26'                                                      
         BE    WP87A                                                            
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     WP87                                                             
WP87A    DS    0H                                                               
         USING ACJOBD,R2                                                        
         TM    SAVESTAT,X'40'                                                   
         BZ    WP88                NO CLOSED DATE IF STILL OPEN                 
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,P+47)                                
WP88     DS    0H                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
WP100    CLI   MODE,LEVBLAST                                                    
         BNE   WP110                                                            
         CLI   PRODPRT,C'N'                                                     
         BE    XIT                                                              
         CLI   PROGPROF,C'Y'                                                    
         BE    *+12                                                             
         CLI   PROGPROF,C'U'                                                    
         BNE   WP101                                                            
         BAS   RE,REPORT                                                        
         AP    CLINET,PRODNET                                                   
         AP    CLIORIG,PRODORIG                                                 
         AP    CLICURR,PRODCURR                                                 
         EDIT  (P6,PRODNET),(11,P+70),2                                         
         CLI   PROGPROF,C'U'                                                    
         BNE   WP100A                                                           
         EDIT  (P6,PRODORIG),(11,P+40),2                                        
         EDIT  (P6,PRODCURR),(11,P+55),2                                        
         MVC   P+10(14),=C'PRODUCT TOTALS'                                      
         B     WP102                                                            
WP100A   MVC   P+10(25),=C'TOTAL BILLING FOR PRODUCT'                           
         B     WP102                                                            
WP101    AP    CLITOT,PRODTOT                                                   
         EDIT  PRODTOT,(11,P+82),2,MINUS=YES                                    
         MVC   P+23(17),=C'TOTAL FOR PRODUCT'                                   
WP102    BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         MVI   PRODPRT,C'N'                                                     
         ZAP   PRODTOT,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
WP110    CLI   MODE,LEVALAST                                                    
         BNE   WP120                                                            
         CLI   PROGPROF,C'Y'                                                    
         BE    WP110A                                                           
         CLI   PROGPROF,C'U'                                                    
         BNE   WP111                                                            
         MVC   P+10(13),=C'CLIENT TOTALS'                                       
         B     *+10                                                             
WP110A   MVC   P+10(24),=C'TOTAL BILLING FOR CLIENT'                            
         EDIT  (P6,CLINET),(11,P+70),2                                          
         CLI   PROGPROF,C'U'                                                    
         BNE   WP112                                                            
         EDIT  (P6,CLIORIG),(11,P+40),2                                         
         EDIT  (P6,CLICURR),(11,P+55),2                                         
         B     WP112                                                            
WP111    CP    CLITOT,=P'0'                                                     
         BE    XIT                                                              
         EDIT  CLITOT,(11,P+82),2,MINUS=YES                                     
         MVC   P+24(16),=C'TOTAL FOR CLIENT'                                    
WP112    BAS   RE,REPORT                                                        
         AP    REQTOT,CLITOT                                                    
         AP    REQNET,CLINET                                                    
         AP    REQORIG,CLIORIG                                                  
         AP    REQCURR,CLICURR                                                  
         B     XIT                                                              
         EJECT                                                                  
WP120    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         BAS   RE,REPORT                                                        
         CLI   PROGPROF,C'Y'                                                    
         BE    WP122                                                            
         CLI   PROGPROF,C'U'                                                    
         BNE   WP124                                                            
         MVC   P+10(14),=C'REQUEST TOTALS'                                      
         B     *+10                                                             
WP122    MVC   P+10(25),=C'TOTAL BILLING FOR REQUEST'                           
         EDIT  (P6,REQNET),(11,P+70),2                                          
         CLI   PROGPROF,C'U'                                                    
         BNE   WP126                                                            
         EDIT  (P6,REQORIG),(11,P+40),2                                         
         EDIT  (P6,REQCURR),(11,P+55),2                                         
         B     WP126                                                            
WP124    CP    REQTOT,=P'0'                                                     
         BE    XIT                                                              
         EDIT  REQTOT,(11,P+82),2,MINUS=YES                                     
         MVC   P+24(17),=C'TOTAL FOR REQUEST'                                   
WP126    BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FILL HEADS AND PRINT                                             
         SPACE 2                                                                
REPORT   NTR1                                                                   
         L     R3,ADHEIRA                                                       
         MVC   HEAD4+8(6),3(R3)    CLIENT CODE                                  
         LA    R5,HEAD4+15                                                      
         L     R6,ADLVANAM                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 =V(SQUASHER),DMCB,HEAD4+8,50,RR=PRELOC                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 3                                                                
NAMOUT   MVC   0(36,R5),SPACES                                                  
         ZIC   RF,1(R6)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),2(R6)                                                    
         SPACE 2                                                                
CHOP     NTR1                      CHOP PROD OR JOB NAME INTO P                 
         L     RF,DMCB+4           LENGTH FROM SQUASHER                         
         MVC   WORK,P+1                                                         
         MVC   P+1(60),SPACES                                                   
         GOTO1 CHOPPER,DMCB,((RF),WORK),(36,P+1),(C'P',2)                       
         B     XIT                                                              
         SPACE 3                                                                
LOOKUP   NTR1  ,                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ACMACOL                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   LOOK02              NO                                           
         USING MJETABD,R3                                                       
         ZAP   ORIGEST,MJETVAL                                                  
         ZAP   CURREST,MJETVAL+6(6)                                             
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
LOOK02   ZAP   ORIGEST,JBCOLVAL                                                 
         ZAP   CURREST,JBCOLVAL+6(6)                                            
         B     XIT                                                              
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE'                                                         
         DROP  R3,R5                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
AC62D    DSECT                                                                  
PRELOC   DS    F                                                                
PRODPRT  DS    CL1                                                              
LAST     DS    CL3                                                              
PRODTOT  DS    PL6                                                              
CLITOT   DS    PL6                                                              
REQTOT   DS    PL6                                                              
ORIGEST  DS    PL6                                                              
CURREST  DS    PL6                                                              
REQNET   DS    PL6                                                              
REQORIG  DS    PL6                                                              
REQCURR  DS    PL6                                                              
CLINET   DS    PL6                                                              
CLIORIG  DS    PL6                                                              
CLICURR  DS    PL6                                                              
PRODNET  DS    PL6                                                              
PRODORIG DS    PL6                                                              
PRODCURR DS    PL6                                                              
JOBNET   DS    PL6                                                              
HOLD     DS    CL132                                                            
SAVESTAT DS    CL1                                                              
END      DS    CL3                                                              
LACT     DS    CL2                                                              
         DS    CL1                 BELONGS WITH LACT                            
         SPACE 2                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACREP6202 07/23/13'                                      
         END                                                                    
