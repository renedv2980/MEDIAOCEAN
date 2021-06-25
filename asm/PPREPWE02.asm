*          DATA SET PPREPWE02  AT LEVEL 128 AS OF 05/01/02                      
*PHASE PPWE02A                                                                  
*INCLUDE MININAM                                                                
         TITLE 'PPWE02 - WEC/CAMBELL REP CONVERSION'                            
         PRINT NOGEN                                                            
*                                                                               
*         QOPT1-2    AGENCY                                                     
*         QOPT3      MEDIA                                                      
*         QOPT5      'N' TEST RUN DO NOT MARK FILE                              
*                    'Y' MEANS MARK FILE                                        
*         QOPT7      'P' = PDUMP REP CHANGES (BEFORE AND AFTER)                 
*                                                                               
PPWE02   CSECT                                                                  
         NMOD1 0,PPWE02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPWEWRKD,R8                                                      
         LA    R7,PPWE02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPWE02+4096,R7                                                   
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   BUYCNT,=P'0'                                                     
         ZAP   BUYCCNT,=P'0'                                                    
         ZAP   ESTCNT,=P'0'                                                     
         ZAP   ESTCCNT,=P'0'                                                    
         ZAP   JOBCNT,=P'0'                                                     
         ZAP   JOBCCNT,=P'0'                                                    
         ZAP   PUBCNT,=P'0'                                                     
         ZAP   PUBCCNT,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'Y'          MEANS MARK FILE                              
         BE    *+8                 LEAVE RCWRITE ALONE                          
         MVI   RCWRITE,C'N'                                                     
*                                                                               
BUYS     MVC   P+1(4),=C'BUYS'                                                  
         MVC   PSECOND+1(4),=C'----'                                            
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1                                                     
         MVI   KEY+3,X'20'         BUYS                                         
         CLC   QOPT1+3(3),=C'ALL'                                               
         BE    BUYS2                                                            
         MVC   KEY+4(3),QOPT1+3    START AT CLIENT                              
*                                                                               
BUYS2    GOTO1 HIGH                                                             
         B     BUYS4                                                            
*                                                                               
BUYS3    DS    0H                                                               
         GOTO1 SEQ                                                              
BUYS4    DS    0H                                                               
         CLI   KEY+3,X'20'                                                      
         BNE   ESTS                                                             
         CLC   KEY(3),QOPT1       ONLY MATCH AGENCY/MEDIA                       
         BNE   ESTS               DONE                                          
*                                                                               
*                                                                               
         CLI   KEY+3,X'20'        SEE IF BUY RECORD                             
         BE    BUYS5                                                            
         B     ESTS               DONE                                          
*                                                                               
*                                                                               
BUYS5    DS    0H                                                               
         OC    KEY+21(3),KEY+21      IGNORE PASSIVE POINTERS                    
         BNZ   BUYS3                                                            
*                                                                               
         MVI   BUYACT,C'N'           SET TO NO ACTIVITY                         
         AP    BUYCNT,=P'1'                                                     
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'80'           CHK FOR SPECIAL REP ELEM                  
         BAS   RE,NEXTEL                                                        
         BNE   BUYSPAY                 CHK FOR REPS IN PAY ELEMS                
         CLI   QOPT7,C'P'              SEE IF PDUMPING                          
         BNE   BUYS6                                                            
         MVC   P+1(6),=C'BEFORE'                                                
         MVC   P+9(4),2(R2)                                                     
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PBUYKEY                                           
         GOTO1 HEXOUT,DMCB,0(R2),P+80,8,=C'N'                                   
         GOTO1 HEXOUT,DMCB,KEY+27,P+110,4,=C'N'                                 
         BAS   RE,RPRT                                                          
*                                                                               
BUYS6    DS    0H                                                               
         LA    RE,2(R2)               ADDRESS OF REP                            
         ST    RE,FULL                                                          
         BAS   RE,CHGREP                                                        
         CLI   ERR,0                 SEE IF REP FOUND                           
         BE    BUYS8                 YES - WRITE BACK PRT RECORD                
         MVC   P+3(4),2(R2)                                                     
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+25,25,=C'N'                                
         MVC   PSECOND+25(25),PBUYKEY                                           
         MVC   PSECOND+3(18),=C'** NOT IN TABLE **'                             
         GOTO1 HEXOUT,DMCB,0(R2),P+80,8,=C'N'                                   
         GOTO1 HEXOUT,DMCB,KEY+27,P+110,4,=C'N'                                 
         MVC   P+125(4),=C'SREP'                                                
         BAS   RE,RPRT                                                          
         B     BUYSX                                                            
*                                                                               
BUYS8    MVC   2(4,R2),NEWREP                                                   
         MVI   BUYACT,C'Y'                                                      
         CLI   QOPT7,C'P'              SEE IF PDUMPING                          
         BNE   BUYS9                                                            
         MVC   P+1(5),=C'AFTER'                                                 
         MVC   P+9(4),NEWREP                                                    
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PBUYKEY                                           
         GOTO1 HEXOUT,DMCB,0(R2),P+80,8,=C'N'                                   
         GOTO1 HEXOUT,DMCB,KEY+27,P+110,4,=C'N'                                 
         MVC   P+125(4),=C'SREP'                                                
         BAS   RE,RPRT                                                          
*                                                                               
BUYS9    DS    0H                                                               
BUYSPAY  DS    0H                                                               
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'25'           CHK FOR PAY ELEMS WITH REPS               
BUYP5    BAS   RE,NEXTEL                                                        
         BNE   BUYPX                   CHK FOR REPS IN PAY ELEMS                
         USING PPAYELEM,R2                                                      
         OC    PPDDATE,PPDDATE         CHECK FOR DATE                           
         BZ    BUYP5                                                            
         OC    PPREP,PPREP             REP                                      
         BZ    BUYP5                                                            
         MVC   WORK(2),PPREP                                                    
         NI    WORK,X'3F'     SET OFF HIGH ORDER BITS X'80' ANDX'40'            
         OC    WORK(2),WORK            AGAIN CHK FOR REP                        
         BZ    BUYP5                                                            
         MVC   SVBITS,PPREP                                                     
         NI    SVBITS,X'C0'           SET OFF ALL  BUT X'80' ,X'40'             
         MVC   HALF,WORK                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORKREP,DUB+5(3)                                                 
         CLI   QOPT7,C'P'                                                       
         BNE   BUYP6                                                            
         MVC   P+1(6),=C'BEFORE'                                                
         MVC   P+9(4),WORKREP+1                                                 
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PBUYKEY                                           
         GOTO1 HEXOUT,DMCB,0(R2),P+75,22,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+123,4,=C'N'                                 
         BAS   RE,RPRT                                                          
BUYP6    LA    RE,WORKREP+1           ADDRESS OF REP                            
         ST    RE,FULL                                                          
         BAS   RE,CHGREP                                                        
         CLI   ERR,0                 SEE IF REP FOUND                           
         BE    BUYP8                 YES - WRITE BACK PRT RECORD                
         MVC   P+3(4),WORKREP+1                                                 
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+25,25,=C'N'                                
         MVC   PSECOND+25(25),PBUYKEY                                           
         MVC   PSECOND+3(18),=C'** NOT IN TABLE **'                             
         GOTO1 HEXOUT,DMCB,0(R2),P+75,22,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+123,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         B     BUYP5               STILL DO NEXT ELEM                           
*                                                                               
*                                                                               
BUYP8    DS    0H                                                               
         PACK  DUB,NEWREP(4)                                                    
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         OC    HALF(1),SVBITS                                                   
         MVC   PPREP,HALF                                                       
         CLI   QOPT7,C'P'                                                       
         BNE   BUYP9                                                            
         MVC   P+1(5),=C'AFTER'                                                 
         MVC   P+9(4),NEWREP                                                    
         GOTO1 HEXOUT,DMCB,PBUYKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PBUYKEY                                           
         GOTO1 HEXOUT,DMCB,0(R2),P+75,22,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+123,4,=C'N'                                 
         BAS   RE,RPRT                                                          
BUYP9    MVI   BUYACT,C'Y'                                                      
         B     BUYP5               GO DO NEXT ELEM                              
*                                                                               
BUYPX    DS    0H                                                               
BUYSX    DS    0H                                                               
         CLI   BUYACT,C'Y'                                                      
         BNE   BUYS3                GO DO NEXT BUY                              
         AP    BUYCCNT,=P'1'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   BUYS3                GO DO NEXT BUY                              
         GOTO1 PUTPRT                                                           
         B     BUYS3                GO DO NEXT BUY                              
*                                                                               
         DROP  R2                                                               
*                                                                               
ESTS     MVC   P+1(9),=C'ESTIMATES'                                             
         MVC   PSECOND+1(9),=C'---------'                                       
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1                                                     
         MVI   KEY+3,X'07'         ESTS                                         
         CLC   QOPT1+3(3),=C'ALL'                                               
         BE    ESTS2                                                            
         MVC   KEY+4(3),QOPT1+3    START AT CLIENT                              
*                                                                               
ESTS2    GOTO1 HIGH                                                             
         B     ESTS4                                                            
*                                                                               
ESTS3    DS    0H                                                               
         GOTO1 SEQ                                                              
ESTS4    DS    0H                                                               
         CLI   KEY+3,X'07'                                                      
         BNE   JOBS                                                             
         CLC   KEY(3),QOPT1       ONLY MATCH AGENCY/MEDIA                       
         BNE   JOBS               DONE                                          
*                                                                               
*                                                                               
         CLI   KEY+3,X'07'        SEE IF EST RECORD                             
         BE    ESTS6                                                            
         B     JOBS               DONE                                          
*                                                                               
*                                                                               
ESTS6    DS    0H                                                               
         AP    ESTCNT,=P'1'                                                     
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         OC    PESTREP,PESTREP       CHECK FOR REP                              
         BZ    ESTS3                                                            
         MVC   P+1(6),=C'BEFORE'                                                
         MVC   P+9(4),PESTREP                                                   
         GOTO1 HEXOUT,DMCB,PESTKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PESTKEY                                           
         GOTO1 HEXOUT,DMCB,KEY+27,P+75,4,=C'N'                                  
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         LA    R2,PESTREP                                                       
         ST    R2,FULL                                                          
         BAS   RE,CHGREP                                                        
         CLI   ERR,0                 SEE IF REP FOUND                           
         BE    ESTS8                 YES - WRITE BACK PRT RECORD                
         MVC   P+3(4),0(R2)                                                     
         GOTO1 HEXOUT,DMCB,PESTKEY,P+25,25,=C'N'                                
         MVC   PSECOND+25(25),PESTKEY                                           
         MVC   PSECOND+3(18),=C'** NOT IN TABLE **'                             
         GOTO1 HEXOUT,DMCB,KEY+27,P+70,4,=C'N'                                  
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     ESTSX                                                            
ESTS8    MVC   0(4,R2),NEWREP                                                   
         MVC   P+1(5),=C'AFTER'                                                 
         MVC   P+9(4),NEWREP                                                    
         GOTO1 HEXOUT,DMCB,PESTKEY,P+20,25,=C'N'                                
         MVC   PSECOND+20(25),PESTKEY                                           
         GOTO1 HEXOUT,DMCB,KEY+27,P+75,4,=C'N'                                  
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         AP    ESTCCNT,=P'1'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   ESTS3                GO DO NEXT EST                              
         GOTO1 PUTPRT                                                           
         B     ESTS3                                                            
*                                                                               
ESTSX    B     ESTS3                                                            
*                                                                               
JOBS     MVC   P+1(7),=C'JOBRECS'                                               
         MVC   PSECOND+1(7),=C'-------'                                         
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1                                                     
         MVI   KEY+3,X'15'         JOBS                                         
         CLC   QOPT1+3(3),=C'ALL'                                               
         BE    JOBS2                                                            
         MVC   KEY+4(3),QOPT1+3    START AT CLIENT                              
*                                                                               
JOBS2    GOTO1 HIGH                                                             
         B     JOBS4                                                            
*                                                                               
JOBS3    DS    0H                                                               
         GOTO1 SEQ                                                              
JOBS4    DS    0H                                                               
         CLI   KEY+3,X'15'                                                      
         BNE   PUBS                                                             
         CLC   KEY(3),QOPT1       ONLY MATCH AGENCY/MEDIA                       
         BNE   PUBS               DONE                                          
*                                                                               
*                                                                               
         CLI   KEY+3,X'15'        SEE IF JOB RECORD                             
         BE    JOBS6                                                            
         B     PUBS               DONE                                          
*                                                                               
*                                                                               
JOBS6    DS    0H                                                               
         OC    KEY+16(6),KEY+16     SEE IF INSTRUCTION RECORD                   
         BNZ   JOBS3                YES - SKIP                                  
         AP    JOBCNT,=P'1'                                                     
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         OC    PJOBPROD,PJOBPROD     CHK FOR REP                                
         BZ    JOBS3                                                            
         LA    R2,PJOBPROD           PRODUCTION HOUSE                           
         ST    R2,FULL                                                          
         BAS   RE,CHGREP                                                        
         CLI   ERR,0                 SEE IF REP FOUND                           
         BE    JOBS8                 YES - WRITE BACK PRT RECORD                
         MVC   P+3(4),0(R2)                                                     
         GOTO1 HEXOUT,DMCB,PJOBKEY,P+15,25,=C'N'                                
         MVC   PSECOND+15(25),PJOBKEY                                           
         GOTO1 HEXOUT,DMCB,KEY+27,P+70,4,=C'N'                                  
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     JOBSX                                                            
JOBS8    MVC   0(4,R2),NEWREP                                                   
         AP    JOBCCNT,=P'1'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   JOBS3                GO DO NEXT JOB                              
         GOTO1 PUTPRT                                                           
         B     JOBS3                                                            
*                                                                               
JOBSX    B     JOBS3                                                            
*                                                                               
PUBS     MVC   P+1(12),=C'PUBLICATIONS'                                         
         MVC   PSECOND+1(12),=C'------------'                                   
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         XC    LASTPUB,LASTPUB                                                  
         XC    PUBREC(30),PUBREC                                                
PUBS3    BAS   RE,NXTPUB                                                        
         CLI   PUBREC,X'FF'                                                     
         BE    PUBS9                                                            
         AP    PUBCNT,=P'1'                                                     
         MVI   PUBERR,0                                                         
         MVI   PUBACT,0                                                         
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
         CLI   0(R2),X'14'                                                      
         BE    PUBS6B                                                           
PUBS6    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PUBS8               NO OR LAST REP ELEM                          
PUBS6B   DS    0H                                                               
         USING PUBREPEL,R2                                                      
         MVC   SVCREP,PUBCNREP      MUST SAVE ORIG CON REP                      
         LA    R3,PUBCNREP          REVERSE ORDER                               
*                                  SO I DO CONTRACT BEFORE TRAFFIC              
         LA    R4,3                                                             
PUBS7    DS    0H                                                               
         OC    0(4,R3),0(R3)       CHK FOR REP                                  
         BZ    PUBS7D                                                           
         CLC   0(4,R3),=C'0000'                                                 
         BE    PUBS7D                                                           
         MVI   PUBACT,C'Y'                                                      
         CLI   QOPT7,C'P'             SEE IF PDUMPING 50 RECS                   
         BNE   PUBS7A                                                           
         MVC   P+1(6),=C'BEFORE'                                                
         MVC   P+9(5),0(R3)                                                     
         GOTO1 HEXOUT,DMCB,PUBREC,P+20,20,=C'N'                                 
         GOTO1 HEXOUT,DMCB,0(R2),P+65,20,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+110,4,=C'N'                                 
         BAS   RE,RPRT                                                          
*                                                                               
PUBS7A   CLI   QMEDIA,C'O'         SEE IF DOING OUTDOOR                         
         BNE   PUBS7C                                                           
         CH    R4,=H'2'            SEE IF DOING TRAFFIC REP                     
         BNE   PUBS7C              NO                                           
         CLC   0(3,R3),=C'000'     SEE IF DOING SUFFIX                          
         BNE   PUBS7C                                                           
         MVC   DOUBLE(4),SVCREP    USE SAVED CONTRACT BAS REP                   
         MVC   DOUBLE+4(1),3(R3)   SUFFIX                                       
         B     PUBS7C5                                                          
*                                                                               
PUBS7C   MVC   DOUBLE(4),0(R3)                                                  
         MVI   DOUBLE+4,0                                                       
PUBS7C5  MVI   DOUBLE+5,0                                                       
         LA    RE,DOUBLE                                                        
         ST    RE,FULL                                                          
         BAS   RE,CHGREP                                                        
*                                                                               
         CLI   ERR,0                                                            
         BE    PUBS7C6                                                          
         MVC   P+3(5),DOUBLE                                                    
         GOTO1 HEXOUT,DMCB,PUBREC,P+15,20,=C'N'                                 
         GOTO1 HEXOUT,DMCB,0(R2),P+60,20,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+105,4,=C'N'                                 
         MVC   PSECOND+3(17),=C'** NOT IN TABLE**'                              
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     PUBS7D                                                           
PUBS7C6  DS    0H                                                               
         CLI   QMEDIA,C'O'            SEE IF DOING OUTDOOR                      
         BNE   PUBS7C9                                                          
         CH    R4,=H'2'              SEE IF DOING TRAFFIC REP                   
         BNE   PUBS7C9                                                          
         CLI   NEWREP+4,C' '          SEE IF NEW HAS SUFFIX                     
         BE    PUBS7C0                NO                                        
         CLC   NEWREP(4),PUBCNREP     MUST MATCH                                
         BNE   PUBRERR                                                          
         MVC   PUBTRREP(3),=C'000'                                              
         MVC   PUBTRREP+3(1),NEWREP+4   SUFFIX                                  
         B     PUBS7CX                                                          
*                                                                               
PUBS7C9  CLI   NEWREP+4,C' '                                                    
         BE    PUBS7C0                                                          
PUBRERR  MVC   P+1(29),=C'INVALID - NEW REP WITH SUFFIX'                        
         MVC   P+35(5),DOUBLE                                                   
         MVC   P+45(5),NEWREP                                                   
         BAS   RE,RPRT                                                          
         MVI   PUBERR,2                                                         
         B     PUBS7D                                                           
*                                                                               
PUBS7C0  MVC   0(4,R3),NEWREP                                                   
PUBS7CX  DS    0H                                                               
         CLI   QOPT7,C'P'             SEE IF PDUMPING CHANGES                   
         BNE   PUBS7D                                                           
         MVC   P+1(5),=C'AFTER'                                                 
         MVC   P+9(5),NEWREP                                                    
         GOTO1 HEXOUT,DMCB,PUBREC,P+20,20,=C'N'                                 
         GOTO1 HEXOUT,DMCB,0(R2),P+65,20,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEY+27,P+110,4,=C'N'                                 
         BAS   RE,RPRT                                                          
*                                                                               
PUBS7D   DS    0H                                                               
         SH    R3,=H'4'               BACK-UP TO NEXT REP                       
         BCT   R4,PUBS7                                                         
         B     PUBS6                                                            
*                                                                               
PUBS8    CLI   PUBACT,0                                                         
         BE    PUBS3                  GO DO NEXT PUB                            
         CLI   PUBERR,0                                                         
         BNE   PUBS3                                                            
PUBS8X   AP    PUBCCNT,=P'1'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   PUBS3                                                            
         LA    R0,PUBKEY                                                        
         ST    R0,AREC                                                          
         GOTO1 PUTPUB                                                           
         B     PUBS3                                                            
*                                                                               
PUBS9    DS    0H                                                               
         B     EXIT                   DONE                                      
CHGREP   NTR1                                                                   
*                                FULL HAS ADDR OF REP                           
         MVI   ERR,0                                                            
         LA    R3,MREPTAB                                                       
         CLI   QMEDIA,C'M'       MAGAZINES                                      
         BE    CHGREP2                                                          
         LA    R3,NREPTAB                                                       
         CLI   QMEDIA,C'N'       NEWSPAPERS                                     
         BE    CHGREP2                                                          
         LA    R3,OREPTAB                                                       
         CLI   QMEDIA,C'O'       OUTDOOR                                        
         BE    CHGREP2                                                          
         LA    R3,SREPTAB        SUPPLEMENTS                                    
         CLI   QMEDIA,C'S'                                                      
         BE    CHGREP2                                                          
         LA    R3,TREPTAB        MUST BE TRADE                                  
CHGREP2  L     R4,FULL                                                          
         MVC   WORK(5),0(R4)                                                    
         CLI   WORK+4,C'0'                                                      
         BE    *+8                                                              
         MVI   WORK+4,C' '                                                      
*                                                                               
CHGREP3  CLC   0(5,R3),WORK                                                     
         BH    CHGRERR                                                          
         BE    CHGREP5                                                          
         LA    R3,10(R3)                                                        
         B     CHGREP3                                                          
*                                                                               
CHGRERR  MVI   ERR,1             REP NOT IN TABLE                               
         B     CHGRX                                                            
*                                                                               
CHGREP5  CLI   5(R3),C'='        MEANS SAME REP                                 
         BNE   CHGREP8                                                          
         MVC   NEWREP,0(R3)      NEW SAME AS OLD                                
         B     CHGRX                                                            
*                                                                               
CHGREP8  MVC   NEWREP,5(R3)                                                     
CHGRX    XIT1                                                                   
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
BUYCNT   DS    PL8                                                              
         DC    CL15'BUYRECS READ'                                               
BUYCCNT  DS    PL8                                                              
         DC    CL15'BUYRECS CHANGED'                                            
ESTCNT   DS    PL8                                                              
         DC    CL15'ESTIMATES READ'                                             
ESTCCNT  DS    PL8                                                              
         DC    CL15'ESTS CHANGED'                                               
JOBCNT   DS    PL8                                                              
         DC    CL15'JOBS READ'                                                  
JOBCCNT  DS    PL8                                                              
         DC    CL15'JOBS CHANGED'                                               
PUBCNT   DS    PL8                                                              
         DC    CL15'PUBS READ'                                                  
PUBCCNT  DS    PL8                                                              
         DC    CL15'PUBS CHANGED'                                               
         DC    X'FF'                                                            
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         ZIC   R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),PAGYKMED                                                  
         BNE   NPX                                                              
         TM    KEY+25,X'01'         IGNORE PASSIVE POINTERS                     
         BO    NP2                                                              
         CLC   KEY+7(2),PAGYKAGY                                                
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   NP2                                                              
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         B     NPX                                                              
***      L     RF,ALTLREC                                                       
***      XC    0(50,RF),0(RF)                                                   
***      GOTO1 SEQPUB                                                           
***      CLC   KEY(9),PUBKEY                                                    
***      BNE   NPX                                                              
***      GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         LA    R2,220                                                           
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
       ++INCLUDE WEREPCON                                                       
         EJECT                                                                  
PAYELD   DSECT                                                                  
       ++INCLUDE PPAYELEM                                                       
         SPACE 3                                                                
PPWEWRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
SVBITS   DS    XL1                                                              
WORKREP  DS    CL5                                                              
BUYACT   DS    CL1                                                              
*                                                                               
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
NEWREP   DS    CL5                                                              
ERR      DS    CL1                                                              
PUBERR   DS    CL1                                                              
PUBACT   DS    CL1                                                              
SVCREP   DS    CL4                                                              
*                                                                               
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128PPREPWE02 05/01/02'                                      
         END                                                                    
