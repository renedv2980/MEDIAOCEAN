*          DATA SET SPREPXF02  AT LEVEL 019 AS OF 05/01/02                      
*PHASE SPXF02A                                                                  
         TITLE 'SPXF02 - SPOTPAK FIND ESTIMATES BY DATE REPORT'                 
SPXF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPXF02,R8,RR=R7                                                
         LA    R8,2048(RB)                                                      
         LA    R8,2048(RC)                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA                                                       
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         ST    R7,RELO             SAVE RELOCATION                              
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF20                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RNF10                                                            
EXIT     XIT1                                                                   
         SPACE 3                                                                
* RUNFRST *                                                                     
         SPACE 1                                                                
RNF10    STM   R8,RB,HDGRTNR8      SAVE BASE/DSECT PTRS                         
         LA    R0,HDGRTN                                                        
         ST    R0,HEADHOOK                                                      
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
* REQFRST *                                                                     
         SPACE 1                                                                
RQF20    OPEN  (SPTP,INPUT)                                                     
         SPACE                                                                  
         LA    R3,QOPT1                                                         
         LA    R5,5                MAX OPTIONS                                  
         LA    R6,REC                                                           
         CLI   0(R3),C' '                                                       
         BNH   AGYERR                                                           
         GOTO1 CKAGY                                                            
         BCTR  R5,0                                                             
         SPACE                                                                  
GETNXT   GET   SPTP,REC-4                                                       
         SPACE                                                                  
DETREC   OC    REC(2),REC          TEST FILE HEADER                             
         BNZ   DETREC2                                                          
         MVC   HLDFILE,REC+76      SAVE FILENAME                                
         MVC   HLDDATE,REC+56      SAVE CREATION DATE                           
         B     GETNXT                                                           
DETREC2  LA    R4,P1                                                            
         USING PLINED,R4                                                        
         CLI   REC,0               TEST FOR HDR REC                             
         BNE   PRNTAGY                                                          
         CLI   REC+8,0             TEST FOR BILLREC                             
         BNE   GETNXT              YES - GET NEXT REC                           
         MVC   WORK(1),REC+1                                                    
         NI    WORK,X'F0'                                                       
DETREC2A CLC   SELAGY,WORK                                                      
         BH    GETNXT                                                           
         BE    DETREC3                                                          
         LA    R3,1(,R3)                                                        
         CLI   0(R3),C' '                                                       
         BNH   PRNTAGY                                                          
         BCT   R5,DETREC2B                                                      
         B     PRNTAGY             GO PRINT AGENCY LIST                         
DETREC2B GOTO1 CKAGY                                                            
         B     DETREC2A                                                         
DETREC3  CLI   REC+4,0             NO - TEST FOR CLTHDR                         
         BE    CLTSAVE             YES - SAVE CLIENT NAME                       
         CLI   REC+7,0             TEST FOR PRDHDR                              
         BE    PRDSAVE                                                          
         USING ESTHDRD,R6                                                       
         CLC   QSTART,EEND         REQ START DATE/EST HDR END DATE              
         BH    GETNXT              BYPASS                                       
         CLC   QEND,ESTART         REQ END DATE/EST HDR END DATE                
         BL    GETNXT              BYPASS                                       
         MVC   AGYSAVE,REC+1       GET AGY/MEDIA CODE                           
         NI    AGYSAVE,X'F0'                                                    
         CLC   PREVAGY,AGYSAVE     AGENCY = PREVIOUS                            
         BE    AGYEQ                                                            
         MVC   PREVAGY,AGYSAVE     NO - MOVE AGENCY TO PREVIOUS                 
         MVI   FORCEHED,C'Y'                                                    
         B     AGYEQ                                                            
         EJECT                                                                  
         USING CLTHDRD,R6                                                       
CLTSAVE  GOTO1 CLUNPK,DMCB,CKEYCLT,CLT                                          
         MVC   CLTNM(L'CNAME),CNAME                                             
         MVC   PCLT,CLT                                                         
         LA    R1,PCLT+2                                                        
         CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(L'CNAME,R1),CLTNM                                              
         MVC   HLDCLT,PCLT         SAVE CLT AND NAME FOR HDG                    
         MVC   MEDSAVE,CKEYAM                                                   
         NI    MEDSAVE,X'0F'                                                    
         MVI   PMEDIA,C'T'         MOVE 'T' TO PL                               
         CLI   MEDSAVE,1           MEDIA = 'T'                                  
         BE    *+8                 YES                                          
         MVI   PMEDIA,C'R'         NO - MOVE 'R' TO PL                          
         B     GETNXT                                                           
         SPACE                                                                  
         USING PRDHDRD,R6                                                       
PRDSAVE  MVC   PPRD(3),PKEYPRD                                                  
         MVI   PPRD+3,C'-'                                                      
         MVC   PPRD+4(L'PNAME),PNAME                                            
         MVC   HLDPRD,PPRD         SAVE PRD                                     
         B     GETNXT                                                           
         SPACE                                                                  
*              SET UP ESTIMATE PRINT LINE                                       
         SPACE                                                                  
         USING ESTHDRD,R6                                                       
AGYEQ    MVC   MEDSAVE,EKEYAM      GET MEDIA                                    
         NI    MEDSAVE,X'0F'                                                    
         ZIC   R1,EKEYEST         CONV EST NO. TO ZONED DEC                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST(3),DUB                                                      
         MVI   PEST+3,C'-'                                                      
         MVC   PEST+4(L'EDESC),EDESC                                            
         GOTO1 DATCON,DMCB,(0,ESTART),(5,PESTART)                               
         GOTO1 DATCON,DMCB,(0,EEND),(5,PESTEND)                                 
PRINTIT  MVI   RCSUBPRG,0                                                       
         GOTO1 REPORT                                                           
         B     GETNXT                                                           
         EJECT                                                                  
*        ROUTINE TO PRINT AGENCY NAMES AND NUMERIC CODES                        
*                                  HEADLINE SETUP ROUTINE                       
         SPACE                                                                  
         USING AGYHDRD,R6                                                       
PRNTAGY  MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         SPACE                                                                  
AGYTEST  CLI   REC,X'06'                                                        
         BH    EOFSPT                                                           
         BL    GTAGYREC                                                         
         XC    PRTLINE,PRTLINE                                                  
         MVC   PAGYNAME,AGYNAME                                                 
         MVC   PAGYADDR,AGYADDR                                                 
         MVC   PAGYAFCD(2),AGYKEY+1                                             
         CLI   AGYPROF+19,X'F0'                                                 
         BNL   AGYEDIT                                                          
         ZIC   R0,AGYPROF+19                                                    
         SH    R0,=H'183'                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAGYNMCD,DUB                                                     
         B     AGYPRINT                                                         
AGYEDIT  MVI   PAGYNMCD,X'F0'                                                   
         MVC   PAGYNMCD+1(1),AGYPROF+19                                         
         SPACE 2                                                                
AGYPRINT GOTO1 REPORT                                                           
         XC    P,P                                                              
         BASR  RE,RF                                                            
         SPACE                                                                  
GTAGYREC GET   SPTP,REC-4                                                       
         B     AGYTEST                                                          
         SPACE 3                                                                
EOFSPT   CLOSE (SPTP)                                                           
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
         EJECT                                                                  
CKAGY    NTR1                                                                   
         CLI   0(R3),C'A'                                                       
         BL    AGYERR                                                           
         CLI   0(R3),C'F'                                                       
         BH    CKAGY10                                                          
         ZIC   R0,0(R3)                                                         
         N     R0,=F'15'                                                        
         BCTR  R0,0                                                             
         AH    R0,=H'10'                                                        
         B     CKAGY20                                                          
CKAGY10  CLI   0(R3),C'0'                                                       
         BL    AGYERR                                                           
         CLI   0(R3),C'9'                                                       
         BH    AGYERR                                                           
         ZIC   R0,0(R3)                                                         
         N     R0,=F'15'                                                        
CKAGY20  SLL   R0,4                                                             
         STC   R0,SELAGY                                                        
         B     EXIT                                                             
         SPACE 3                                                                
AGYERR   MVC   P1(40),=CL40'QOPT1-5 MUST BE 0-9 OR A-F OR BLK'                  
ALLERR   MVI   SPACING,2                                                        
         XC    HEADHOOK,HEADHOOK   DROP HEADHOOK RTN                            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
         EJECT                                                                  
******************************                                                  
*                            *                                                  
*     HEADHOOK PROCESSING    *                                                  
*                            *                                                  
******************************                                                  
         SPACE 1                                                                
         DROP  RB,R8                                                            
         DS    0F                                                               
         USING *,RF                                                             
HDGRTN   NTR1                                                                   
         LM    R8,RB,HDGRTNR8                                                   
         B     HDHK0                                                            
HDGRTNR8 DC    4F'0'                                                            
         DROP  RF                                                               
         USING SPXF02,RB,R8                                                     
*                                                                               
HDHK0    DS    0H                                                               
         LA    R4,P1                                                            
         CLI   RCSUBPRG,0                                                       
         BNE   HDHK1                                                            
         MVC   H3+44(8),=CL8'AGENCY ='                                          
         ZIC   R0,AGYSAVE                                                       
         SRL   R0,4                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H3+53(2),DUB                                                     
         MVI   PMEDIA,C'T'         MOVE 'T' TO PL                               
         CLI   MEDSAVE,1           MEDIA = 'T'                                  
         BE    *+8                 YES                                          
         MVI   PMEDIA,C'R'         NO - MOVE 'R' TO PL                          
         MVC   PCLT,HLDCLT                                                      
         MVC   PPRD,HLDPRD                                                      
         B     EXIT                                                             
*                                                                               
HDHK1    CLI   RCSUBPRG,1                                                       
         BNE   HDHK2                                                            
         MVC   H1+64(11),HLDFILE                                                
         MVC   H1+78(8),HLDDATE                                                 
*                                                                               
HDHK2    B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
         SPACE 1                                                                
*                                                                               
SPCLSOUT DS    D                                                                
REC      DS    2008C                                                            
         SPACE 3                                                                
AGYSAVE  DS    CL1                                                              
MEDSAVE  DS    CL1                                                              
HLDFILE  DS    CL11                                                             
HLDDATE  DS    CL8                                                              
HLDCLT   DS    CL24                                                             
HLDPRD   DS    CL24                                                             
PREVAGY  DC    X'00'                                                            
SELAGY   DC    X'00'                                                            
SPTP     DCB   BLKSIZE=8200,                                           X        
               DDNAME=SPTFILE,                                         X        
               DSORG=PS,                                               X        
               EODAD=EOFSPT,                                           X        
               LRECL=2008,                                             X        
               MACRF=GM,                                               X        
               RECFM=VB                                                         
PLINED   DSECT                                                                  
PRTLINE  DS    0CL110                                                           
         DS    CL6                                                              
PMEDIA   DS    CL1                                                              
         DS    CL6                                                              
PCLT     DS    CL24                                                             
         DS    CL1                                                              
PPRD     DS    CL24                                                             
         DS    CL1                                                              
PEST     DS    CL24                                                             
         DS    CL3                                                              
PESTART  DS    CL8                                                              
         DS    CL3                                                              
PESTEND  DS    CL8                                                              
         DS    CL5                                                              
         ORG   PRTLINE                                                          
         DS    CL7                                                              
PAGYAFCD DS    CL2                                                              
         DS    CL16                                                             
PAGYNMCD DS    CL2                                                              
         DS    CL8                                                              
PAGYNAME DS    CL33                                                             
         DS    CL2                                                              
PAGYADDR DS    CL33                                                             
         DS    CL7                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
*        PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPREPXF02 05/01/02'                                      
         END                                                                    
