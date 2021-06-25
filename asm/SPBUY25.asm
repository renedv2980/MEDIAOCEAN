*          DATA SET SPBUY25    AT LEVEL 016 AS OF 04/02/13                      
*PHASE T21125C                                                                  
         TITLE 'T21125 - SPOTPAK BUY - REQUESTS'                                
T21125   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21125                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
REQ0     XC    ELEM(26),ELEM                                                    
         MVI   ELEM+14,106                                                      
         LA    R8,ELEM+26                                                       
         MVI   0(R8),C' '                                                       
         MVC   1(79,R8),0(R8)                                                   
*                                                                               
         CLI   SVCPROF+2,X'00'     SUPPRESS TURNAROUNDS                         
         BE    EXIT                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   *+12                NO                                           
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BE    REQ50                                                            
         OC    SVNETYM,SVNETYM     TEST NETPAK                                  
         BNZ   EXIT                                                             
         EJECT                                                                  
         MVC   0(2,R8),=C'61'      NON-POL GETS 61'S                            
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   REQ1                NO                                           
         CLI   SVCPROF,C'2'        TEST BRD POL 61'S                            
         BNE   REQ2                NO                                           
REQ1     CLI   SVCPROF+2,C'1'      TEST BRD POL 61'S BY STA                     
         BNE   REQ4                                                             
         MVC   18(5,R8),QSTA                                                    
         B     REQ4                                                             
*                                                                               
REQ2     MVC   0(2,R8),=C'81'      PROF+0 = 0 OR 1 SO GET U5 EVENTUALLY         
         CLI   SVCPROF+2,C'2'      TEST U3 BY MKT                               
         BNE   *+14                                                             
         MVC   0(2,R8),=C'U3'                                                   
         B     REQ4                                                             
*                                                                               
         CLI   SVCPROF+2,C'3'      TEST U3 BY STATION                           
         BNE   *+10                                                             
         MVC   0(2,R8),=C'U3'                                                   
         MVC   18(5,R8),QSTA        AND ALL 81'S ARE BY STATION                 
*                                                                               
REQ4     DS    0H                                                               
         CLC   0(2,R8),=C'U3'                                                   
         BE    REQ5                                                             
         PACK  DUB,0(2,R8)                                                      
         CVB   R0,DUB                                                           
         STC   R0,ELEM+10                                                       
*                                                                               
REQ5     MVC   2(2,R8),AGYALPHA                                                 
         MVC   4(1,R8),BUYMD                                                    
         MVC   5(3,R8),QCLT                                                     
         MVC   8(2,R8),=C'NN'      NO MKT/PRD GRPS                              
*                                                                               
         MVC   11(3,R8),QPRD                                                    
*                                                                               
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   REQ10               NO                                           
         CLI   SVCPROF,C'0'        TEST BRD POL                                 
         BE    REQ10               NO                                           
*                                                                               
         CLI   BDMASPRD,0                                                       
         BE    REQ10                                                            
*                                                                               
         LA    R1,SVQPRDS                                                       
         BAS   RE,FINDPRD                                                       
         MVC   11(3,R8),0(RF)                                                   
*                                                                               
REQ10    MVC   DUB(2),SVKEY+4      MARKET NUM                                   
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(4,R8),DUB                                                     
*                                                                               
         ZIC   R0,SVKEY+9          EST                                          
         CLI   SVEST1,0            TEST SERIES REQ                              
         BE    *+8                                                              
         IC    R0,SVEST1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R8),DUB                                                     
*                                                                               
         ICM   R0,1,SVEST2                                                      
         BZ    *+18                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  26(3,R8),DUB                                                     
*                                                                               
         MVC   37(12,R8),SVSTART   EST START/END DATES                          
*                                                                               
         MVI   59(R8),C'B'         T/A IND                                      
         CLI   ELEM+10,81                                                       
         BNE   REQ12                                                            
         MVI   59(R8),C'A'         SET TO GEN U4                                
         MVI   ELEM+10,0                                                        
         MVC   ELEM+26(2),=C'U5'                                                
*                                                                               
REQ12    CLI   ELEM+10,61                                                       
         BNE   REQ18                                                            
         MVC   61(5,R8),=C'30000'                                               
*                                                                               
         MVI   ELEM+10,0                                                        
         MVC   ELEM+26(2),=C'U3'                                                
         MVC   ELEM+26+49(19),SPACES  CLEAR ALL OPTIONS                         
*                                                                               
REQ18    MVC   68(12,R8),BUYBU                                                  
         OC    68(12,R8),SPACES                                                 
*                                                                               
         CLI   SVCPROF+0,C'0'      TEST TRUE POL                                
         BNE   *+12                                                             
         CLI   SVCPROF+12,C'Y'     TEST REQUEST BY PRD                          
         BE    REQ40                                                            
         BAS   RE,ADDREQ                                                        
*                                                                               
REQ20    CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    REQ24               NO                                           
* NEED PARTNER REQUEST                                                          
         LA    R6,BDELEM                                                        
REQ22    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),4                                                          
         BNE   REQ22                                                            
         MVC   11(3,R8),6(R6)      PASSIVE PRD                                  
*                                                                               
         ZIC   R0,3(R6)            PASSIVE EST                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R8),DUB                                                     
         GOTO1 VDATAMGR,DMCB                                                    
         B     EXIT                                                             
*                                                                               
* POL - CHECK FOR P/B MASPRD                                                    
*                                                                               
REQ24    CLI   SVQPRDS+1,0                                                      
         BE    EXIT                                                             
* ADD REQUEST FOR SECOND BRAND                                                  
         LA    R1,SVQPRDS+1                                                     
         BAS   RE,FINDPRD                                                       
         MVC   11(3,R8),0(RF)                                                   
         GOTO1 VDATAMGR,DMCB                                                    
         B     EXIT                                                             
         EJECT                                                                  
* ADD REQUESTS FOR EACH PRD IF CPROF+12 = C'Y'                                  
         SPACE 1                                                                
REQ40    MVC   WORK(32),SVQLIST                                                 
         NC    WORK(32),QLIST                                                   
         XC    QLIST,WORK          GET NEW BITS ONLY IN QLIST                   
         OC    QLIST,QLIST                                                      
         BZ    EXIT                                                             
         SPACE 1                                                                
* BUILD A LIST OF ALL PRD CODES TO BE REQUESTED *                               
         SPACE 1                                                                
         XC    SPLLWRK,SPLLWRK     CLEAR WORK AREA                              
         LA    R1,1                                                             
         LA    RE,SPLLWRK                                                       
         LA    RF,QLIST                                                         
*                                                                               
REQ42    LA    R4,8                                                             
         ZIC   R5,0(RF)                                                         
         SLL   R5,24               LEFT ALIGN                                   
*                                                                               
REQ44    LTR   R5,R5               TEST PRODUCT ACTIVE                          
         BNM   *+12                NO                                           
         STC   R1,0(RE)            ADD PRD NUM TO LIST                          
         LA    RE,1(RE)            BUMP LIST POINTER                            
*                                                                               
         LA    R1,1(R1)            NEXT PRD CODE                                
         SLL   R5,1                                                             
         BCT   R4,REQ44                                                         
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R0,QLIST+32                                                      
         CR    RF,R0                                                            
         BL    REQ42                                                            
         OC    SVQLIST,QLIST       ADD REQUESTED PRDS TO SAVED LIST             
         XC    QLIST,QLIST         AND CLEAR CURRENT LIST                       
         SPACE 1                                                                
* NOW ADD A REQUEST FOR EACH PRD *                                              
         SPACE 1                                                                
         LA    R4,SPLLWRK-1                                                     
REQ46    LA    R4,1(R4)            NEXT PRD                                     
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         LR    R1,R4                                                            
         BAS   RE,FINDPRD                                                       
         BAS   RE,ADDREQ                                                        
         B     EXIT                                                             
         EJECT                                                                  
* CANAD NTWK                                                                    
*                                                                               
REQ50    MVC   0(2,R8),=C'DN'                                                   
         MVC   2(2,R8),AGYALPHA                                                 
         MVC   4(1,R8),BUYMD                                                    
         MVC   5(3,R8),QCLT                                                     
         MVC   11(3,R8),QPRD                                                    
         MVC   18(4,R8),QSTA                                                    
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST NTWK BUY                                
         BNZ   REQ54               YES                                          
*                                                                               
         LA    R6,BDELEM           FOR EXPL BUY MUST FIND NTWK EL               
         SR    R0,R0                                                            
REQ52    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                WHICH MAY NOT BE THERE ON OLD BUYS           
         CLI   0(R6),X'68'                                                      
         BNE   REQ52                                                            
         MVC   18(4,R8),2(R6)      ELSE MOVE NTWK TO REQ                        
*                                                                               
REQ54    ZIC   R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R8),DUB                                                     
         MVC   37(12,R8),SVSTART                                                
         ZIC   R0,SVLIN                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  61(3,R8),DUB                                                     
         MVI   64(R8),C'B'         SET TURNAROUND IND                           
         B     REQ18                                                            
         EJECT                                                                  
* SUBROUTINE TO FIND PRD CODE IN SVCLIST                                        
* R1 POINTS TO PRD ON ENTRY, RF POINTS TO LIST ENTRY ON EXIT                    
* IF PRD NOT FOUND, REREAD CLIENT HEADER AND RESAVE PRD LIST                    
         SPACE 1                                                                
FINDPRD  SR    R0,R0                CLEAR FLAG                                  
*                                                                               
FINDPRD2 L     RF,ASVCLIST                                                      
*                                                                               
FINDPRD4 CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   FINDPRD4                                                         
         LTR   R0,R0               TEST READ CLTHDR ALREADY                     
         BNZ   REQERR              YES - PRD NOT VALID                          
         LR    R0,RE               SET FLAG AND SAVE RETURN REG                 
         BAS   RE,READCLT                                                       
         LR    RE,R0               RESTORE RETURN REG                           
         B     FINDPRD2            AND SEARCH PRDLIST AGAIN                     
         SPACE 2                                                                
* READ CLTHDR                                                                   
         SPACE 1                                                                
READCLT  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         GOTO1 READ                                                             
         L     R0,AREC             SAVE CURRENT AREC                            
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE AREC                                 
*                                                                               
         USING CLTHDRD,R6                                                       
         L     R1,ASVCLIST                                                      
         MVC   0(220,R1),CLIST+000                                              
         MVC   220(220,R1),CLIST+220                                            
         MVC   440(220,R1),CLIST+440                                            
         MVC   660(220,R1),CLIST+660                                            
         B     EXIT                                                             
*                                                                               
REQERR   MVC   BUYMSG(37),=C'** ERROR DURING REQUEST GENERATION **'             
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* SUBROUTINE TO ADD REQUESTS                                    *               
*===============================================================*               
         SPACE 1                                                                
ADDREQ   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                    
*                                                                               
         CLI   8(R1),0                                                          
         BNE   EXIT                EXIT WITH CC NEQ                             
*                                                                               
ADDREQ2  CLC   SVRFPGRP,SPACES                                                  
         BNH   EXIT                                                             
         CLC   =C'G7',AGYALPHA                                                  
         BNE   ADDREQ4                                                          
         TM    SVESTFL1,EF1REQ     TEST ESTIMATE REQUESTABLE                    
         BZ    EXIT                                                             
         MVC   ELEM+128(80),ELEM+26    SAVE ORIGINAL REQUEST                    
*                                                                               
ADDREQ4  LA    R1,ELEM                                                          
         USING REQHDRD,R1                                                       
         MVC   REQORIG,SVRFPID                                                  
         DROP  R1                                                               
*                                                                               
         MVC   ELEM+26(2),=C'RF'                                                
         MVC   ELEM+26+49(8),SVRFPGRP                                           
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                    
*                                                                               
         MVC   ELEM+26(80),ELEM+128 RESTORE ORIGINAL REQUEST                    
*                                                                               
         LA    R1,ELEM             CLEAR PARTS OF REQHDR JUST SET               
         USING REQHDRD,R1                                                       
         XC    REQORIG,REQORIG                                                  
         MVI   REQFLAG,0                                                        
         DROP  R1                                                               
*                                                                               
         CLI   DMCB+8,0             SET CC                                      
*                                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
         PRINT ON                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
REQHDRD  DSECT                                                                  
       ++INCLUDE DDREQHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPBUY25   04/02/13'                                      
         END                                                                    
