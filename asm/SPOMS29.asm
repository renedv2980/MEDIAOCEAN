*          DATA SET SPOMS29    AT LEVEL 060 AS OF 03/24/09                      
*PHASE T23429A                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE PERVERT                                                                
                                                                                
T23429   TITLE 'SPOMS29 - PRINT DARE FAXES'                                     
T23429   CSECT                                                                  
                                                                                
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,**3429**,RR=R3,CLEAR=YES                         
*                                                                               
         LR    R2,RC                                                            
         USING MYWORKD,R2                                                       
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         ST    RB,SPOM29RB                                                      
                                                                                
Q        USING RQAREA,REQ                                                       
T        USING TSARD,TSARBLK                                                    
                                                                                
         BRAS  RE,INIT                                                          
*                                                                               
         BRAS  RE,SETOPTS          SET REPORT PROFILE OPTIONS                   
*                                                                               
         BRAS  RE,ESTF             DO ESTIMATE FIRST PROCESSING                 
*                                                                               
         BRAS  RE,GETDRORD         READ DARE ORDER TO IO3                       
         BE    GOREAD                                                           
         CLI   PQONLY,C'Y'                                                      
         BE    NOPQOERR                                                         
         DC    H'0'                                                             
*                                                                               
GOREAD   BRAS  RE,READBUYS                                                      
*                                                                               
         MVI   COPYTYPE,C'X'       SET THIS IS FAX COPY                         
         CLI   SDXTOPQ,C'Y'        TEST TO PUT COPY TO PRTQUE                   
         BNE   *+8                                                              
         MVI   COPYTYPE,C'Q'       SET THIS IS PRTQUE COPY                      
*                                                                               
REOPEN   BRAS  RE,PQOPEN                                                        
         B     REOP2     ********* CODE BELOW FOR TRACING *********             
         CLI   COPYTYPE,C'X'                                                    
         BNE   REOP2                                                            
         BRAS  RE,PRINTSAR         FOR DEBUGGING                                
*                                                                               
REOP2    XC    HEADHOOK,HEADHOOK                                                
         XC    MIDHOOK,MIDHOOK                                                  
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BRAS  RE,PRTCOVER                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         BRAS  RE,PR               PRINT REPORT                                 
*                                                                               
         BRAS  RE,PQCLOSE                                                       
         CLI   PQONLY,C'Y'         TEST SEND TO PRTQUE ONLY                     
         BE    EXIT                                                             
         CLI   COPYTYPE,C'X'       TEST THIS IS FAX COPY                        
         BE    EXIT                                                             
         MVI   COPYTYPE,C'X'       SET FOR FAX COPY                             
         MVI   PASS,1              RESET                                        
         B     REOPEN                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
NOPQOERR MVC   GERROR(2),=Y(274)                                                
         MVI   GETMSYS,23                                                       
         GOTO1 MYERR                                                            
         EJECT                                                                  
         LTORG                                                                  
*================================================================               
* ESTIMATE FIRST PROCESSING - BUILD DATE TABLES                                 
*================================================================               
                                                                                
ESTF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
*                                                                               
         MVI   SVPOL,C'N'                                                       
         XC    KEY,KEY             SEE IF THIS IS A POL ESTIMATE                
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),BEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+8                                                              
         MVI   SVPOL,C'Y'                                                       
*                                                                               
         MVI   BPRD2,0                                                          
         CLC   Q.RQPTNR,SPACES                                                  
         BNH   ESTF6                                                            
*                                                                               
         LA    R1,SVCLIST                                                       
*                                                                               
ESTF2    CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   Q.RQPTNR,0(R1)                                                   
         BE    ESTF4                                                            
         AHI   R1,4                                                             
         B     ESTF2                                                            
*                                                                               
ESTF4    MVC   BPRD2,3(R1)                                                      
*                                                                               
         XC    KEY,KEY             READ PRODUCT RECORD                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),Q.RQPTNR                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
         MVC   PRD2NM(L'PNAME),PNAME   EXTRACT PARTNER NAME                     
         DROP  R6                                                               
         EJECT                                                                  
*==================================================================             
* READ ESTIMATE HEADER FOR OOWR FLAG AND GET DEMO NAMES IF NEEDED               
*==================================================================             
                                                                                
ESTF6    XC    KEY,KEY             READ ESTIMATE HEADER                         
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),Q.RQPRD                                                 
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         MVC   SVOOWR,EOWSDAY                                                   
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1                                                          
         XC    0(256,R6),0(R6)                                                  
         USING DBLOCKD,R6                                                       
*                                                                               
         MVC   DBSELAGY,Q.RQAGY                                                 
         MVC   DBSELMED,Q.RQMED    SET MEDIA CODE IN DBLOCK                     
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         DROP  R6                                                               
* GET 6 CHARACTER DEMO NAMES                                                    
         MVC   SVDEMNMS,SPACES                                                  
*                                                                               
         SR    R0,R0                                                            
         CLI   Q.RQ2DEMO,C'N'      TEST NO DEMOS                                
         BE    *+12                                                             
         IC    R0,Q.RQ2DEMO                                                     
         N     R0,=X'0000000F'                                                  
         STH   R0,NUMDEMS                                                       
*                                                                               
         LA    R1,ESTDEMOS         COUNT ESTHDR DEMOS                           
         LHI   R0,4                                                             
*                                                                               
ESTF8A   CLI   1(R1),0             TEST FOR NO DEMO                             
         BE    ESTF8B                                                           
         AHI   R1,3                                                             
         BCT   R0,ESTF8A                                                        
*                                                                               
ESTF8B   LHI   R1,4                                                             
         SR    R1,R0               GIVES DEMOS PRESENT                          
         CH    R1,NUMDEMS                                                       
         BH    *+8                                                              
         STH   R1,NUMDEMS                                                       
         LR    R0,R1                                                            
*                                                                               
         LA    R7,ESTDEMOS+(EUSRNMS-EDEMOS)   POINT TO SAVED USRNMS             
         GOTO1 DEMOCON,DMCB,((R0),ESTDEMOS),(6,SVDEMNMS),              X        
               (C'S',(R6)),(R7)                                                 
                                                                                
*=================================================================              
* SET UP REPORT PERIODS                                                         
* BACK UP TO MONDAY START FOR WEEKLY ESTIMATE UNLESS OOWR                       
*=================================================================              
                                                                                
         CLC   =C'ES',Q.RQSTAUTO                                                
         BNE   *+10                                                             
         MVC   Q.RQSTART(12),ESTSTRT                                            
*                                                                               
         CLI   SVEDAILY,C'Y'       TEST DAILY ESTIMATE                          
         BE    ESTF10                                                           
         CLI   SVOOWR,0            TEST OOWR                                    
         BNE   ESTF10                                                           
         GOTO1 GETDAY,DMCB,ESTSTRT,DUB                                          
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AHI   R0,-1                                                            
         BZ    ESTF10                                                           
         LCR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,ESTSTRT,Q.RQSTART,(R0)                                
                                                                                
ESTF10   MVC   SVQSTART(12),Q.RQSTART SAVE ORIGINAL START/END DATES             
         GOTO1 DATCON,DMCB,SVQSTART,(3,SVQSTB)                                  
         GOTO1 (RF),(R1),,(2,SVQSTP)                                            
         GOTO1 (RF),(R1),SVQEND,(3,SVQENDB)                                     
         GOTO1 (RF),(R1),,(2,SVQENDP)                                           
*                                                                               
         XC    PASSTAB,PASSTAB       CLEAR TABLE OF PERIODS                     
* BUILD NEW TABLE                                                               
         LA    R4,PASSTAB                                                       
         MVC   PASSTAB(6),Q.RQSTART                                             
         LHI   R5,7                MAX IS 7 PERIODS                             
         CLI   SVEDAILY,C'Y'       TEST DAILY ESTIMATE                          
         BNE   ESTF20                                                           
                                                                                
**** DAILY                                                                      
                                                                                
ESTF12   GOTO1 ADDAY,DMCB,0(R4),6(R4),F'13'  GET END DATE                       
         CLC   6(6,R4),Q.RQEND               TEST PAST REQUEST END              
         BNL   ESTF14                                                           
         GOTO1 (RF),(R1),6(R4),12(R4),F'1'   GET NEXT START DATE                
         LA    R4,12(R4)                                                        
         BCT   R5,ESTF12                                                        
*                                                                               
ESTF14   MVC   6(6,R4),Q.RQEND     SET REQ END DATE IN LAST ENTRY               
                                                                                
* FILL IN WEEKTAB WITH DAYS                                                     
                                                                                
         MVC   WORK(6),Q.RQSTART                                                
         LA    R4,WEEKTAB                                                       
         LHI   R5,53                                                            
         B     ESTF17                                                           
*                                                                               
ESTF16   GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         CLC   WORK+6(6),Q.RQEND   TEST PAST REQUEST END                        
         BH    ESTF18                                                           
         MVC   WORK(6),WORK+6                                                   
*                                                                               
ESTF17   GOTO1 DATCON,DMCB,WORK,(2,(R4))                                        
         MVC   2(2,R4),0(R4)       SET END DAY TOO                              
         AHI   R4,4                NEXT  ENTRY                                  
         BCT   R5,ESTF16                                                        
*                                                                               
ESTF18   MVI   0(R4),X'FF'         SET EOT                                      
         B     ESTF37                                                           
                                                                                
*** WEEKLY                                                                      
                                                                                
ESTF20   CLI   SVOOWR,0            IS IT OOWR?                                  
         BE    ESTF25                NO, LETS USE MOBILE LATER                  
         MVC   WORK(6),Q.RQSTART     YES, MOBILE DOESN'T SUPPORT OOW            
         LA    R4,WEEKTAB                                                       
         LHI   R5,53                                                            
*                                                                               
ESTF21   GOTO1 ADDAY,DMCB,WORK,WORK+6,F'6'                                      
         CLC   WORK+6(6),Q.RQEND   TEST END DATE PAST REQUEST END               
         BNH   ESTF22                                                           
         CLC   WORK(6),Q.RQEND     TEST START DATE PAST REQUEST END?            
         BH    ESTF23              YES, FINISHED                                
         MVC   WORK+6(6),Q.RQEND   NO, SET REQUEST END AS END DATE              
         B     ESTF22                                                           
*                                                                               
ESTF22   GOTO1 DATCON,DMCB,WORK,(2,(R4))                                        
         GOTO1 DATCON,DMCB,WORK+6,(2,2(R4))                                     
         AHI   R4,4                NEXT  ENTRY                                  
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'1'  POINT TO START OF NEXT WEEK         
         BCT   R5,ESTF21                                                        
*                                                                               
ESTF23   MVI   0(R4),X'FF'         SET EOT                                      
*                                                                               
ESTF25   GOTO1 CALLOV,DMCB,0,X'D9000A1D'   GET GETBROAD ADDRESS                 
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
                                                                                
* BUILD TABLE OF 53 (MAX) BROADCAST WEEKS                                       
* FROM START DATE IF OOWR OR FROM MONDAY IF NOT                                 
                                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A72'   GET MOBILE ADDRESS                   
         L     RF,0(R1)                                                         
*                                                                               
         CLI   SVOOWR,0            IS IT OOWR?                                  
         BNE   ESTF27               - YES, SKIP THIS AS WE DID IT ABOVE         
         GOTO1 (RF),DMCB,(53,Q.RQSTART),(5,WEEKTAB),WORK,0                      
                                                                                
* NOW DO BROADCAST MONTHS                                                       
                                                                                
ESTF27   GOTO1 (RF),DMCB,(13,Q.RQSTART),(0,AIO1),WORK,0                         
                                                                                
* CALL PERVERT FOR THE NUMBER OF WEEKS IN REQUEST PERIOD                        
* IF LESS THAN 14, THE REPORT WILL HAVE ONLY ONE PERIOD                         
                                                                                
         L     RF,=V(PERVERT)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,Q.RQSTART,Q.RQEND                                      
         LH    R0,12(R1)           GET NUMBER OF WHOLE WEEKS                    
         OC    10(2,R1),10(R1)     TEST ANY REMAINDER                           
         BZ    *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,14               TEST MORE THAN 14 WEEKS                      
         BH    ESTF28                                                           
         MVC   PASSTAB(12),Q.RQSTART  SET START/END DATES                       
         B     ESTF37                                                           
*                                                                               
ESTF28   ICM   R0,1,SVOOWR         TEST IF OOWR?                                
         BZ    ESTF30                                                           
         BCTR  R0,0                SET TO LAST DAY OF OOWR                      
         L     R5,AIO1             YES, ADJUST THE MONTHS                       
ESTF29   CLI   4(R5),X'FF'                                                      
         BE    ESTF30                                                           
         GOTO1 DATCON,DMCB,(2,2(R5)),WORK                                       
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+6,(2,2(R5))                                     
         MVC   WORK(6),WORK+6                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,WORK+6,(2,4(R5))                                     
         LA    R5,4(R5)                                                         
         B     ESTF29                                                           
                                                                                
* NOW UP TO 3 BROADCAST MONTHS PER PERIOD                                       
                                                                                
ESTF30   LA    R4,PASSTAB                                                       
         L     R5,AIO1                                                          
*                                                                               
ESTF31   GOTO1 DATCON,DMCB,(2,0(R5)),0(R4)                                      
*                                                                               
ESTF33   CLI   4(R5),X'FF'         TEST MONTH 2 IS THERE                        
         BE    ESTF35              NO - USE CURRENT                             
         AHI   R5,4                POINT TO NEXT MONTH                          
         CLI   4(R5),X'FF'         TEST MONTH 3 IS THERE                        
         BE    ESTF35                                                           
         AHI   R5,4                                                             
*                                                                               
ESTF35   GOTO1 DATCON,DMCB,(2,2(R5)),6(R4)                                      
         AHI   R4,12                                                            
         AHI   R5,4                                                             
         CLI   0(R5),X'FF'                                                      
         BNE   ESTF31                                                           
*                                                                               
ESTF37   MVI   PASS,1              RESET PASS COUNTER                           
*                                                                               
         LA    R1,PASSTAB                                                       
         SR    R0,R0                                                            
ESTF40   OC    0(12,R1),0(R1)                                                   
         BZ    *+12                                                             
         AHI   R1,12                                                            
         BCT   R0,ESTF40                                                        
         LPR   R0,R0                                                            
         STC   R0,MAXPASS                                                       
         J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* READ BUY RECORDS AND BUILD TSAR BUFFER                                        
*=============================================================                  
                                                                                
READBUYS NTR1  BASE=*,LABEL=*                                                   
         XC    RPTTOTS,RPTTOTS                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING BUYKEY,R7                                                        
*                                                                               
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
         MVC   SVBUYKEY,KEY                                                     
*                                                                               
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BE    RB100                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     RB4                                                              
*                                                                               
RB2      GOTO1 SEQ                                                              
*                                                                               
RB4      CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    RB6                                                              
*                                                                               
         L     RF,=X'FFFFFFFF'     SET NORMAL MASK (0 BIT NTWK)                 
         CLI   Q.RQMED,C'R'        TEST RADIO                                   
         BE    RB6                                                              
         CLI   Q.RQSTA,C'0'        TEST CABLE REQUEST                           
         BL    RB6                 NO                                           
         L     RF,=X'FFFFFF80'     SET LOCAL CABLE MASK(7 BITS)                 
*                                                                               
RB6      SR    R0,R0                                                            
         ICM   R0,7,KEY+6                                                       
         NR    R0,RF                                                            
         SR    RE,RE                                                            
         ICM   RE,7,KEYSAVE+6                                                   
         NR    RE,RF                                                            
         CR    R0,RE                                                            
         BNE   RB100                                                            
*                                                                               
         CLC   KEY+9(1),KEYSAVE+9  TEST SAME ESTIMATE                           
         BNE   RB2                                                              
*                                                                               
         L     R7,AIO1                                                          
         ST    R7,AIO                                                           
         USING BUYRECD,R7                                                       
         GOTO1 GETREC              NOTE - READ FOR UPDATE                       
*                                                                               
         TM    15(R7),X'80'        TEST DELETED                                 
         BO    RB2                                                              
         CLC   KEY+4(2),4(R7)      TEST SAME MARKET                             
         BNE   RB2                 NO - IGNORE SPILL                            
*                                                                               
         CLI   DRCSHTRD,C'R'       TEST TRADE SIDE OF CASH/TRD                  
         BNE   RB8                                                              
         CLC   BDREP,DRTRDDTA      MATCH REP                                    
         BE    RB10                IF NO MATCH IT'S CASH SO EXIT                
         B     RB2                                                              
*                                                                               
RB8      CLI   DRCSHTRD,C'R'-X'40' TEST CASH SIDE OF CASH TRD                   
         BNE   RB10                                                             
         CLC   BDREP,DRTRDDTA      COMPARE TO SPECIAL REP                       
         BE    RB2                 IF MATCH IT'S TRADE, SO EXIT                 
*                                                                               
RB10     CLI   SDXPROF+1,C'Y'      TEST NEW PAGE BY NETWORK                     
         BNE   RB11                NO - CONTINUE                                
         CLC   KEY(10),KEYSAVE     TEST CHANGE OF NTWK                          
         BE    RB11                NO                                           
         MVC   KEYSAVE(13),KEY     THEN SAVE NEW NETWORK                        
         MVI   FORCEHED,C'Y'       AND SET FOR NEW PAGE                         
*                                                                               
RB11     LA    R0,TSARREC                                                       
         LHI   R1,TSARRECX-TSARREC                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,COVTAB                                                        
         LHI   R1,COVTABX-COVTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BNE   RB30                                                             
                                                                                
* POL BUY                                                                       
                                                                                
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
RB12     BRAS  RE,NEXTEL                                                        
         BNE   RB50                                                             
*                                                                               
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   RB12                                                             
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   RB12                                                             
         CLC   2(2,R6),SVQSTP      TEST PRIOR TO START                          
         BL    RB12                                                             
         CLC   2(2,R6),SVQENDP     OR AFTER END                                 
         BH    RB12                                                             
*                                                                               
         CLI   1(R6),14            TEST PIGGYBACK SPOT                          
         BH    RB14                YES                                          
*                                                                               
         CLI   BPRD2,0             TEST PIGGYBACK REQUEST                       
         BNE   RB12                YES -IGNORE                                  
         CLC   10(1,R6),BPRD       RIGHT PRODUCT                                
         BNE   RB12                                                             
         B     RB20                                                             
                                                                                
* THIS IS A PIGGYBACK SPOT                                                      
                                                                                
RB14     CLI   BPRD2,0             TEST PIGGYBACK REQUEST                       
         BE    RB12                NO - SO SKIP P/B SPOT                        
*                                                                               
         CLC   10(1,R6),BPRD       MATCH FIRST PRD                              
         BE    RB16                                                             
         CLC   14(1,R6),BPRD       MATCH TO SECOND PRD                          
         BNE   RB12                NO - THEN MATCHES NEITHER                    
*                                                                               
RB16     CLC   10(1,R6),BPRD2      NOW MATCH PRD2                               
         BE    RB20                                                             
         CLC   14(1,R6),BPRD2                                                   
         BNE   RB12                                                             
*                                                                               
RB20     BAS   RE,POSTIT                                                        
         B     RB12                                                             
         EJECT                                                                  
*==============================================================                 
* NON-POL BUY                                                                   
*==============================================================                 
                                                                                
RB30     CLI   BPRD2,0             TEST PIGGYBACK REQUEST                       
         BE    RB34                NO                                           
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   RB50                IF NO PBELEM, IGNORE BUY                     
*                                                                               
         USING PBELEM,R6                                                        
         CLC   BUYKEY+3(1),BPRD    TEST BPRD IS ACTIVE PRD                      
         BNE   RB32                NO                                           
         CLC   PBPROD,BPRD2        TEST P/B MATCHES PRD2                        
         BE    RB34                YES                                          
         B     RB50                NO - IGNORE BUY                              
*                                                                               
RB32     CLC   BUYKEY+3(1),BPRD2   TEST PRD2 IS ACTVE PRD                       
         BNE   RB50                NO - IGNORE BUY                              
                                                                                
RB34     LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'07'                                                     
*                                                                               
RB36     BRAS  RE,NEXTEL                                                        
         BNE   RB50                                                             
*                                                                               
         CLC   2(2,R6),SVQSTP      TEST PRIOR TO START                          
         BL    RB36                                                             
         CLC   2(2,R6),SVQENDP     OR AFTER END                                 
         BH    RB36                                                             
         BAS   RE,POSTIT                                                        
         B     RB36                                                             
                                                                                
*=============================================================                  
* END OF BUY ELEMENTS                                                           
*=============================================================                  
                                                                                
RB50     MVC   TSARSTA,BUYKEY+6       STATION TO TSAR KEY                       
         MVC   TSARLIN+1(1),BUYKEY+10   AND LINE NUMBER                         
*                                                                               
         OC    TSSPTS,TSSPTS       TEST ANY SPOTS POSTED                        
         BNZ   RB52                                                             
         OC    COVTAB,COVTAB       OR ANY COST OVRD SPOTS                       
         BZ    RB2                 NO SPOTS - SKIP TSAR                         
                                                                                
* FILL IN REMAINDER OF TSAR RECORD                                              
                                                                                
RB52     MVC   TSBDDAY,BDDAY                                                    
         MVC   TSBDTIME,BDTIMST                                                 
         MVC   TSBDCOST,BDCOST                                                  
         MVC   TSBDCIND,BDCIND                                                  
         MVC   TSBDCIN2,BDCIND2                                                 
         MVC   TSBDSEC,BDSEC                                                    
         MVC   TSBDPROG,BDPROGRM                                                
*                                                                               
         CLI   DRCSHTRD,C'R'       TEST TRADE SIDE OF CASH/TRADE                
         BNE   RB54                                                             
         CLI   SDARPROF+12,C'Y'    YES, ZERO COST TRADE?                        
         BNE   RB54                                                             
         XC    TSDOLS,TSDOLS                                                    
         XC    TSBDCOST,TSBDCOST                                                
         XC    TSTAX,TSTAX                                                      
*                                                                               
RB54     BAS   RE,GETDEMS          EXTRACT DEMO VALUES                          
*                                                                               
RB56     CLI   Q.RQCOST2,C'Y'      SEND THE COST2?                              
         BNE   RB60                NO                                           
         MVI   ELCDLO,X'71'                                                     
         MVI   ELCDHI,X'71'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   RB60                                                             
         ICM   RE,15,2(R6)                                                      
         STCM  RE,7,TSBDCOST                                                    
*                                                                               
RB60     MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
                                                                                
*===============================================================                
* ADD TSAR RECORDS FOR COST OVRDS, ORBITS AND COMMENTS AS NEEDED                
*===============================================================                
                                                                                
         LA    R4,COVTAB                                                        
         LHI   R5,(COVTABX-COVTAB)/L'COVTAB                                     
*                                                                               
RB62     OC    0(3,R4),0(R4)       TEST ENTRY PRESENT                           
         BZ    RB70                                                             
*                                                                               
         LA    R0,TSARREC+L'TSARKEY   CLEAR ALL BUT KEY                         
         LHI   R1,(TSARRECX-TSARREC)-L'TSARKEY                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,TSARSEQ                                                       
         AHI   R0,1                                                             
         STC   R0,TSARSEQ                                                       
*                                                                               
         ICM   R0,7,0(R4)          GET COST                                     
         N     R0,=X'007FFFFF'     DROP ZERO FLAG                               
         STCM  R0,7,TSBDCOST                                                    
         MVC   TSSPTS,3(R4)        MOVE SPOT TABLE                              
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         AHI   R4,L'COVTAB                                                      
         BCT   R5,RB62                                                          
                                                                                
RB70     MVI   ELCDLO,X'67'        SEARCH FOR ORBIT ELEMENTS                    
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   RB72                                                             
*                                                                               
         LA    R0,TSARREC+L'TSARKEY   CLEAR ALL BUT KEY                         
         LHI   R1,(TSARRECX-TSARREC)-L'TSARKEY                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-5               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSORBS(0),4(R6)                                                  
*                                                                               
         MVI   TSARSEQ,X'0F'       SET ORBIT RECORD CODE                        
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
RB72     BRAS  RE,DOBUYCOM                                                      
         B     RB2                                                              
         EJECT                                                                  
*=====================================================================          
* READ THROUGH HISTORY RECORDS AND ADD DELETED BUYS TO TSAR BUFFER              
* THIS CODE ALSO BUILDS TSAR RECORDS FOR RESEND                                 
*=====================================================================          
                                                                                
RB100    XC    SVXSPKEY,SVXSPKEY   CLEAR SAVE AREA                              
         XC    KEY,KEY                                                          
K        USING OHISRECD,KEY                                                     
         MVI   K.OHISTYP,OHISTYQ                                                
         MVI   K.OHISSTYP,OHISSTYQ                                              
         MVC   K.OHISORD,BINORDER                                               
         MVC   K.OHISBKAM,BAGYMD                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',KEYSAVE,KEY                   
         CLC   KEY(20),KEYSAVE           SAME TY/ORDER/A-M                      
         BNE   RB130                                                            
*                                                                               
RB102    MVI   T.TSACTN,TSARDH     SEE IF RECORD IN TSAR BUFFER                 
         XC    TSARKEY,TSARKEY                                                  
         MVC   TSARSTA,K.OHISBKST       SET STATION                             
         MVC   TSARLIN+1(1),K.OHISBKLN  AND LINE                                
         BRAS  RE,CALLTSAR                                                      
         BE    RB120               RECORD IS IN BUFFER                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',K.OHISDDA,AIO1,DMWORK         
*                                                                               
         L     R6,AIO1                                                          
         AHI   R6,42                                                            
         USING OBDELEM,R6                                                       
*                                                                               
         LA    R0,TSARREC          PREPARE A NEW TSAR RECORD                    
         LHI   R1,TSARRECX-TSARREC                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TSARSTA,K.OHISBKST        SET STATION                            
         MVC   TSARLIN+1(1),K.OHISBKLN   AND BUYLINE                            
*                                                                               
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BE    RB104               YES                                          
*                                                                               
         CLI   OBDFLAG,0           TEST BUY WAS DELETED PREVIOUSLY              
         BNE   RB120               YES - IGNORE                                 
*                                                                               
RB103    MVC   TSFLAG,Q.RQ2CVRSN   SET DEL VERSION NUMBER IN TSARREC            
                                                                                
* NEED SPOTS IN TSAR RECORD SO KNOW WHICH PERIODS TO PRINT                      
                                                                                
         XC    TSSPTS,TSSPTS                                                    
         MVI   ELCDLO,6                                                         
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BNE   *+8                 NO                                           
         MVI   ELCDLO,X'16'                                                     
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RB103A                                                           
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    TSSPTS(0),2(R6)                                                  
*                                                                               
RB103A   MVI   ELCDLO,8            NOW RATE OVERRIDE ELEMENTS                   
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BNE   *+8                 NO                                           
         MVI   ELCDLO,X'18'                                                     
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
RB103B   BRAS  RE,NEXTEL                                                        
         BNE   RB103X                                                           
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-6                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    TSSPTS(0),5(R6)     SET NON-ZERO IN ACTIVE WEEKS                 
         B     RB103B                                                           
*                                                                               
RB103X   MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         B     RB120                                                            
         DROP  K                                                                
                                                                                
* FOR RESEND. CREATE TSAR RECORD FROM ORDER HISTORY RECORD                      
                                                                                
RB104    CLI   OBDFLAG,0           TEST PREVIOUSLY DELETED                      
         BE    RB106               NO - RECREATE BUY DATA                       
*                                                                               
         CLC   OBDFLAG,Q.RQ2CVRSN  TEST DELETED IN THIS VERSION                 
         BNE   RB120               NO - IGNORE                                  
         BE    RB103               AND BUILD SPOTS FROM PRIOR ELEMS             
*                                                                               
RB106    MVC   TSBDDAY,OBDDAY                                                   
         MVC   TSBDTIME,OBDTIME                                                 
         MVC   TSBDSEC,OBDSEC                                                   
         MVC   TSBDCOST,OBDCOST                                                 
         MVC   TSBDCIND,OBDCIND                                                 
         MVC   TSBDCIN2,OBDCIND2                                                
         MVC   TSBDPROG,OBDPROG                                                 
         MVC   TSDEMS(32),OBDDEMS                                               
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO SPOTS ELEMENT                       
         CLI   0(R6),6                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSSPTS(0),2(R6)                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO DOLLARS ELEMENT                     
         CLI   0(R6),7                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSDOLS(0),2(R6)                                                  
*                                                                               
RB106A   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    RB106X                                                           
         CLI   0(R6),9             TEST TAX ELEMENT                             
         BNE   RB106A                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSTAX(0),2(R6)                                                   
*                                                                               
RB106X   MVI   T.TSACTN,TSAADD     ADD BUY DESC REC                             
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         LA    R0,TSARREC+L'TSARKEY   CLEAR ALL BUT KEY                         
         LHI   R1,(TSARRECX-TSARREC)-L'TSARKEY                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO1             RESET RECORD POINTER                         
         AHI   R6,42                                                            
*                                                                               
RB107    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    RB110                                                            
         CLI   0(R6),8             TEST COST OVRD ELEM                          
         BL    RB107                                                            
         BH    RB110                                                            
         USING OHCOVEL,R6                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,TSARSEQ                                                       
         AHI   R0,1                                                             
         STC   R0,TSARSEQ          THIS SHOULD ADD SEQNUMS 01-08                
*                                                                               
         ICM   R0,7,OHCCOST                                                     
         N     R0,=X'007FFFFF'     DROP X'80' FLAG                              
         STCM  R0,7,TSBDCOST       MOVE COST                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-3               LESS 3 FOR COST                              
         AHI   RE,-3               AND ELEM CODE/LEN/+1                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSSPTS(0),OHCSPOTS  AND SPOTS                                    
         DROP  R6                                                               
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         B     RB107                                                            
                                                                                
* NOW READ THE BUY RECORD FOR COMMENTS AND ORBIT DATA                           
                                                                                
RB110    L     RE,AIO1                                                          
         XC    0(13,RE),0(RE)      CLEAR IN CASE NOT FOUND                      
*                                                                               
         MVC   SVXSPKEY,KEY        SAVE ORDER HISTORY KEY                       
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVBUYKEY                                                 
         CLI   SVPOL,C'Y'                                                       
         BNE   *+8                                                              
         MVI   KEY+3,X'FF'         READ POL POINTER IF POSSIBLE                 
         MVC   KEY+11(1),TSARLIN+1                                              
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   RB112                                                            
         MVC   AIO,AIO1            READ OVER ORDER HISTORY REC                  
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCDLO,X'67'        FIND ORBIT DATA                              
         MVI   ELCDHI,X'67'                                                     
         L     R6,AIO                                                           
         AHI   R6,24                                                            
         BRAS  RE,NEXTEL                                                        
         BNE   RB112                                                            
*                                                                               
         LA    R0,TSARREC+L'TSARKEY   CLEAR ALL BUT KEY                         
         LHI   R1,(TSARRECX-TSARREC)-L'TSARKEY                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   TSARSEQ,X'0F'       SET ORBIT SEQNUM                             
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-5               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSORBS(0),4(R6)                                                  
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
RB112    CLI   TSFLAG,0            TEST DELETED BUY RECORD                      
         BNE   RB120                                                            
         BRAS  RE,DOBUYCOM         ADD TSAR COMMENT RECORDS                     
*                                                                               
RB120    OC    SVXSPKEY,SVXSPKEY   TEST BROKE READ SEQUENCE                     
         BZ    RB122                                                            
         MVC   KEY,SVXSPKEY                                                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',KEYSAVE,KEY                   
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SVXSPKEY,SVXSPKEY                                                
*                                                                               
RB122    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',KEYSAVE,KEY                   
         CLC   KEY(20),KEYSAVE     TEST SAME ORDER/AGYMD                        
         BE    RB102                                                            
*                                                                               
RB130    J     EXIT                                                             
         EJECT                                                                  
POSTIT   NTR1                                                                   
                                                                                
         CLI   Q.RQCOST2,C'Y'      SEND THE COST2?                              
         BNE   POSTIT1                                                          
         MVC   SPOTS,=C'COS2'      NOW GET COST2                                
*                                                                               
POSTIT1  GOTO1 GETRATE,DMCB,(BUYKEY+3,SPOTS),BUYKEY,(R6)                        
*                                                                               
         LA    R1,WEEKTAB                                                       
         LHI   R0,53                                                            
*                                                                               
POSTIT2  CLC   2(2,R6),0(R1)       TEST PRIOR TO WEEK START                     
         BL    POSTIT3                                                          
         CLC   2(2,R6),2(R1)       TEST PRIOR TO WEEK END                       
         BNH   POSTIT4                                                          
POSTIT3  AHI   R1,4                                                             
         BCT   R0,POSTIT2                                                       
         DC    H'0'                                                             
*                                                                               
POSTIT4  LA    R0,WEEKTAB                                                       
         SR    R1,R0                                                            
         LR    RE,R1               SAVE DSPL IN 4 BYTE TABLE                    
*                                                                               
         LA    R1,TSDOLS(R1)       POINT TO DOLLAR TABLE ENTRY                  
         L     R0,0(R1)                                                         
         A     R0,GROSS                                                         
         ST    R0,0(R1)                                                         
*                                                                               
         AHI   R1,L'TSDOLS         POINT TO TAX TABLE ENTRY                     
         L     R0,0(R1)                                                         
         A     R0,TAX                                                           
         ST    R0,0(R1)                                                         
*                                                                               
         SRL   RE,2                ADJUST FOR 1 BYTE SPOT ENTRIES               
         LA    R1,TSSPTS(RE)                                                    
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         A     R0,SPOTS                                                         
*                                                                               
         CLI   SDXCOSTS,C'Y'       TEST REPORTING COST                          
         BNE   POSTIT6             NO - IGNORE COST OVERRIDES                   
         TM    6(R6),X'20'         TEST COST OVERRIDE THIS SPOT                 
         BO    POSTIT10            YES - DON'T COUNT HERE                       
*                                                                               
POSTIT6  STC   R0,0(R1)                                                         
         B     POSTITX                                                          
                                                                                
*=================================================================              
* FIND THIS COST IN THE COVTAB OR ADD IT IF THERE'S ROOM                        
* NOTE THAT THE PRINTED COST OVERRIDE DOES NOT INCLUDE TAX !                    
*=================================================================              
                                                                                
         USING REGELEM,R6                                                       
POSTIT10 LA    R1,COVTAB                                                        
         LHI   R0,(COVTABX-COVTAB)/L'COVTAB                                     
         SR    RF,RF                                                            
         ICM   RF,7,RPCOST         GET GROSS COST THIS SPOT                     
         BNZ   *+8                                                              
         O     RF,=X'00800000'     SET SPECIAL VALUE FOR ZERO                   
         DROP  R6                                                               
*                                                                               
POSTIT12 OC    0(3,R1),0(R1)       TEST ENTRY PRESENT                           
         BZ    POSTIT14                                                         
         CLM   RF,7,0(R1)          MATCH COST                                   
         BE    POSTIT14                                                         
         AHI   R1,L'COVTAB                                                      
         BCT   R0,POSTIT12                                                      
         B     POSTITX             NO MORE ROOM IN TABLE !                      
*                                                                               
POSTIT14 STCM  RF,7,0(R1)          SET COST IN COVTAB ENTRY                     
         LA    R1,3(R1,RE)         POINT TO WEEK IN COVTAB ENTRY                
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         A     R0,SPOTS                                                         
         STC   R0,0(R1)                                                         
*                                                                               
POSTITX  J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*===============================================================                
* EXTRACT DEMO VALUES TO TSAR RECORD                                            
*===============================================================                
                                                                                
GETDEMS  NTR1  BASE=*,LABEL=*                                                   
         USING BUYRECD,R7                                                       
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ESTDEMOS         POINT TO CURRENT DEMO LIST                   
         LA    R4,TSDEMS                                                        
         SR    R5,R5                                                            
         ICM   R5,3,NUMDEMS                                                     
         JZ    EXIT                                                             
*                                                                               
GETD2    OC    0(3,R3),0(R3)       TEST THERE IS A DEMO THERE                   
         JZ    EXIT                NO                                           
*                                                                               
         BAS   RE,FINDDEM                                                       
         BNE   *+10                                                             
         MVC   0(8,R4),0(R1)                                                    
*                                                                               
         CLI   1(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'E'                                                       
         BNE   GETD20                                                           
*                                                                               
         CLI   S00PROF+9,C'Y'      TEST 2-DEC RATINGS                           
         BNE   GETD10              NO                                           
                                                                                
* AGENCY USES 2-DEC RATINGS                                                     
                                                                                
         TM    4(R4),X'40'         TEST 2-DEC VALUE                             
         BO    GETD20                                                           
         L     R1,4(R4)            CONVERT 1-DEC VALUE TO 2-DEC                 
         N     R1,=X'3FFFFFFF'     DROP FLAGS                                   
         M     R0,=F'10'                                                        
         O     R1,=X'40000000'                                                  
         TM    4(R4),X'80'                                                      
         BZ    *+8                                                              
         O     R1,=X'80000000'                                                  
         ST    R1,4(R4)                                                         
         B     GETD20                                                           
                                                                                
* AGENCY USES 1-DEC RATINGS                                                     
                                                                                
GETD10   TM    4(R4),X'40'         TEST 2-DEC VALUE                             
         BZ    GETD20                                                           
*                                                                               
         L     R1,4(R4)            CONVERT 2-DEC TO 1-DEC                       
         N     R1,=X'3FFFFFFF'     DROP FLAGS                                   
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         TM    4(R4),X'80'         TEST OVERRIDE                                
         BZ    *+8                                                              
         O     R1,=X'80000000'                                                  
         ST    R1,4(R4)                                                         
*                                                                               
GETD20   AHI   R3,3                                                             
         LA    R4,8(R4)                                                         
         BCT   R5,GETD2                                                         
         J     EXIT                                                             
*                                                                               
FINDDEM  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                COUNT OF DEMOS IN DEMEL                      
         LA    R1,24(R6)                                                        
*                                                                               
FINDDEM2 CLC   0(3,R3),0(R1)                                                    
         BER   RE                  RETURN WITH CC EQ                            
         AHI   R1,8                                                             
         BCT   R0,FINDDEM2                                                      
         LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         LTORG                                                                  
*&&DO                              NEED TO MULTIPLY BY SPOTS !                  
         LA    R1,TOTDEMS                                                       
         LA    R4,TSDEMS                                                        
         LHI   R5,4                                                             
*                                                                               
GETD30   L     RE,4(R4)                                                         
         N     RE,=X'3FFFFFFF'                                                  
         L     RF,0(R1)                                                         
         N     RF,=X'3FFFFFFF'                                                  
         AR    RF,RE                                                            
         TM    4(R4),X'40'         TEST 2-DEC VALUE                             
         BZ    *+8                                                              
         O     RF,=X'40000000'                                                  
         ST    RF,4(R4)                                                         
*                                                                               
         AHI   R1,4                                                             
         AHI   R4,8                                                             
         BCT   R5,GETD30                                                        
*&&                                                                             
         EJECT                                                                  
*=================================================================              
* FIND BUY COMMENTS AND ADD TSAR RECORDS FOR THEM                               
*=================================================================              
                                                                                
DOBUYCOM NTR1  BASE=*,LABEL=*                                                   
         MVI   TSARSEQ,X'10'       SET BASE SEQNUM FOR COMMENTS                 
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         L     R6,AIO                                                           
         CLI   0(R6),0             TEST BUY RECORD PRESENT                      
         JE    EXIT                NO - GET OUT                                 
         AHI   R6,24                                                            
*                                                                               
DOCOM2   BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
*                                                                               
         CLC   3(2,R6),=C'X-'      THESE COMMENTS NEVER PRINT                   
         BE    DOCOM2                                                           
*                                                                               
         CLI   PROGPROF+4,C'0'                                                  
         BE    DOCOM6                                                           
         CLI   PROGPROF+4,C'1'     TEST PRINT ACCTG COMMENTS ONLY               
         BNE   DOCOM4                                                           
         CLI   3(R6),C'$'          TEST COMMENT BEGINS WITH $                   
         BE    DOCOM6                                                           
         CLI   2(R6),4             TEST COMMENT NUMBER 4                        
         BE    DOCOM6                                                           
         CLI   2(R6),5             OR 5                                         
         BE    DOCOM6                                                           
         B     DOCOM2                                                           
*                                                                               
DOCOM4   CLC   =C'COMMENT-',3(R6)  PRINT COMMENT- COMMENTS                      
         BNE   DOCOM2                                                           
*                                                                               
DOCOM6   IC    R0,TSARSEQ          SET SEQUENCE NUMBER                          
         AHI   R0,1                                                             
         STC   R0,TSARSEQ                                                       
*                                                                               
         MVC   TSCOM,SPACES                                                     
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EXE                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSCOM(0),3(R6)         MOVE COMMENT                              
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         B     DOCOM2                                                           
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* PRINT THE FAX                                                                 
*================================================================               
                                                                                
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   T.TSACTN,TSAGET     GET FIRST TSAR RECORD FOR TSARSTA            
         LHI   R0,1                                                             
         STH   R0,T.TSRNUM                                                      
         BRAS  RE,CALLTSAR         AND IGNORE ERRORS                            
*                                                                               
         XC    RPTTOTS,RPTTOTS     CLEAR REPORT TOTALS                          
*                                                                               
         BRAS  RE,GETHDNG          GET HEADING ADDRESS                          
         ST    R1,SPECS                                                         
         BRAS  RE,GETHDHK                                                       
         ST    R1,HEADHOOK                                                      
         XC    MIDHOOK,MIDHOOK                                                  
                                                                                
*===================================================================            
* BUILD A BUFFER OF OCOMS THAT APPLY TO THIS MD/CLT/PRD/EST                     
* THESE COMMENTS PRINT AFTER THE REPORT HEADLINES BUT BEFORE SKED               
*===================================================================            
                                                                                
PR2      XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OMCOMD,R1                                                        
*                                                                               
         MVI   OMCACT,C'G'                                                      
         MVC   OMCLBUFF,=H'8000'   MAY CLOBBER EYECATCHER **I/O2**!             
         MVC   OMCABUFF,AIO1       OVERWRITE IO1                                
         MVC   OMCACOMF,ACOMFACS                                                
         MVC   OMCAGMD,BAGYMD                                                   
         MVC   OMCCLT,BCLT                                                      
         MVC   OMCPRD,BPRD                                                      
         MVC   OMCEST,BEST                                                      
         MVC   OMCFLT,BINFLTNM                                                  
*                                                                               
         GOTO1 SPOMCOM,(R1)                                                     
*                                                                               
         OC    OMCCOUNT,OMCCOUNT   TEST ANY COMMENTS                            
         BZ    PR10                                                             
*                                                                               
         L     R4,AIO1                                                          
         LH    R5,OMCCOUNT                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
PR4      MVC   P(72),8(R4)                                                      
         MVC   P+73(8),0(R4)       SHOW COMMENT SOURCE                          
         CLC   0(8,R4),80(R4)      TEST NEXT COM HAS SAME SOURCE                
         BE    *+8                                                              
         MVI   P2,0                ELSE FORCE BLANK LINE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         AHI   R4,80                                                            
         BCT   R5,PR4                                                           
*                                                                               
PR10     L     R6,AIO3             POINT TO ORDER RECORD                        
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
*                                                                               
         LA    R1,DOISCOM1         STANDARD COMMENT 1                           
         LA    R0,3                MAX 3 STANDARD COMMENTS                      
         MVI   ELCDLO,X'10'        STANDARD COMMENT TEXT ELEM                   
*                                                                               
PR20     CLI   0(R1),C' '                                                       
         BNH   PR22                                                             
*                                                                               
         BRAS  RE,DARSTD           READ STANDARD COMMENT                        
         BRAS  RE,DARPRT           PRINT STANDARD COMMENT                       
*                                                                               
PR22     LA    R1,8(R1)                                                         
         BCT   R0,PR20                                                          
                                                                                
*============================================================                   
* NOW PRINT ORDER COMMENTS                                                      
*============================================================                   
                                                                                
         L     R6,AIO3             POINT TO ORDER RECORD                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)       MOVE ORDER KEY                               
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKCMT,X'01'        SET FOR COMMENT RECORD                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND ANY COMMENTS                      
         BNE   PR30                                                             
*                                                                               
         MVI   ELCDLO,X'30'        ORDER COMMENT ELCODE                         
         BRAS  RE,DARPRT           DO GETREC AND PRINT                          
         EJECT                                                                  
*====================================================================           
* NOW PRINT THE ORDER                                                           
*====================================================================           
                                                                                
PR30     MVI   FORCEMID,C'Y'                                                    
*                                                                               
PR32     BRAS  RE,GETMIDHK                                                      
         ST    R1,MIDHOOK                                                       
*                                                                               
PR34     MVI   T.TSACTN,TSAGET                                                  
         LHI   R0,1                                                             
         STH   R0,T.TSRNUM                                                      
         BRAS  RE,CALLTSAR                                                      
         BE    PR35                                                             
*                                                                               
         MVI   FORCEMID,C'N'                                                    
         MVC   P(29),=C'** NO BUYS ON THIS STATION **'                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRX                                                              
*                                                                               
PR35     SR    R4,R4                                                            
         IC    R4,PASS                                                          
         BCTR  R4,0                                                             
         MHI   R4,12                                                            
         LA    R4,PASSTAB(R4)                                                   
         GOTO1 DATCON,DMCB,0(R4),(2,BPERST)  GET 2-BYTE PER START               
         GOTO1 (RF),(R1),6(R4),(2,BPEREND)     AND END DATES                    
*                                                                               
         XC    PASSTOTS,PASSTOTS   CLEAR PASS TOTALS                            
         MVI   PASSACT,C'N'        RESET PASS ACTIVE FLAG                       
                                                                                
* NOW GET DISPLACEMENT OF START WEEK IN WEEKTAB                                 
                                                                                
         LA    R4,WEEKTAB                                                       
         LHI   R5,53                                                            
PR36     CLC   BPERST,0(R4)        MATCH DATE                                   
         BE    PR38                                                             
         AHI   R4,4                                                             
         BCT   R5,PR36                                                          
         DC    H'0'                                                             
*                                                                               
PR38     LA    R0,WEEKTAB                                                       
         SR    R4,R0                                                            
         STH   R4,WEEKDSPL         SAVE START DSPL IN WEEKTAB                   
         STH   R4,DOLDSPL          SAME DSPL IN TSDOLS                          
*                                                                               
         LHI   R0,53                                                            
         SR    R0,R5                                                            
         STH   R0,SPTDSPL          SET DSPL OF START WEEK IN TSSPTS             
         XC    WEEKSPTS,WEEKSPTS                                                
         B     PR42                                                             
*                                                                               
PR40     MVI   ANYCHANG,C'N'                                                    
         MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   PR200                                                            
         CLI   TSARSEQ,0           IGNORE NON-BUY DATA                          
         BNE   PR40                                                             
*                                                                               
PR42     CLI   TSFLAG,0            TEST DELETED BUY                             
         BE    PR46                NO                                           
                                                                                
* SPOT COUNTERS REFLECT PREVIOUS ACTIVITY                                       
* SEE IF NEED TO PRINT THIS DELETED BUY FOR THIS PERIOD                         
                                                                                
         LA    R4,WEEKTAB          COUNT NUMBER OF ACTIVE WEEKS                 
         AH    R4,DOLDSPL          ADD 4 BYTE DSPL TO FIRST WEEK                
         SR    R0,R0                                                            
*                                                                               
PR42A    AHI   R4,4                NEXT WEEK IN TABLE                           
         CLC   0(2,R4),BPEREND     TEST STILL IN PERIOD                         
         BH    *+8                                                              
         BCT   R0,PR42A                                                         
*                                                                               
         BCTR  R0,0                ADD ONE TO COUNT                             
         LPR   R0,R0                                                            
*                                                                               
         LA    RE,TSSPTS           SPTS/WEEK TABLE                              
         AH    RE,SPTDSPL                                                       
*                                                                               
PR42B    CLI   0(RE),0                                                          
         BNE   PR42X                                                            
         AHI   RE,1                                                             
         BCT   R0,PR42B                                                         
         B     PR50                                                             
*                                                                               
PR42X    CLI   PASSACT,C'Y'        TEST FIRST ACTIVITY                          
         BE    PR44                                                             
         MVI   PASSACT,C'Y'        SET ACTIVITY FLAG                            
         CLI   FORCEMID,C'Y'       IF FORCEMID SET                              
         BE    PR44                SKIP FORCEHED                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PR44     MVI   ANYCHANG,C'Y'       SET CHANGED !                                
         MVC   P(3),QEST                                                        
         MVI   P+3,C'-'                                                         
         SR    R0,R0                                                            
         IC    R0,TSARLIN+1                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(3),DUB                                                       
         MVC   P+8(16),=C'==> DELETED <=='                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR50                                                             
*                                                                               
PR46     BRAS  RE,SETMYSPT         SET SPOT COUNTS ACROSS RATES                 
*                                                                               
         BRAS  RE,BUYSKED          FORMAT BUY DATA                              
         CLI   ANYSPOTS,C'Y'       TEST ANY ACTIVITY FOR THIS PASS              
         BNE   PR50                                                             
*                                                                               
         SR    R5,R5                                                            
         MVC   SVTSRNUM,T.TSRNUM   SAVE CURRENT TSAR REC NUMBER                 
         MVI   T.TSACTN,TSANXT     READ FOR ADDITIONAL RECORDS                  
         BRAS  RE,CALLTSAR                                                      
         BNE   PR48X                                                            
*                                                                               
         CLI   TSARSEQ,0           TEST BUY DATA                                
         BE    PR48X               YES - NO COMMENTS                            
*                                                                               
         LHI   R5,4                ASSUME 4 LINES OF BUY DATA                   
*                                                                               
PR48     AHI   R5,1                ADD ONE FOR THIS DATA LINE                   
         BRAS  RE,CALLTSAR                                                      
         BNZ   PR48X                                                            
         CLI   TSARSEQ,0           TEST ANOTHER SUPP REC                        
         BNE   PR48                                                             
*                                                                               
PR48X    LTR   R5,R5               TEST ANY SUPP RECS                           
         BZ    *+8                                                              
         STC   R5,ALLOWLIN                                                      
*                                                                               
         MVC   T.TSRNUM,SVTSRNUM   RESTORE TSAR REC NUMBER                      
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*=====================================================                          
* READ FOR ORDER HISTORY RECORD                                                 
*=====================================================                          
                                                                                
PR50     XC    KEY,KEY                                                          
K        USING OHISRECD,KEY                                                     
         MVI   K.OHISTYP,OHISTYQ                                                
         MVI   K.OHISSTYP,OHISSTYQ                                              
         MVC   K.OHISORD,BINORDER                                               
         MVC   K.OHISBUYK(10),SVBUYKEY                                          
         MVC   K.OHISBKST(3),TSARSTA     STATION !                              
         MVC   K.OHISBKLN,TSARLIN+1      BUYLINE NUMBER                         
*                                                                               
         MVC   KEYSAVE,KEY                                                      
* REMEMBER X'80' = READ FOR UPDATE (NOT RDUPDATE=Y)                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR',KEYSAVE,KEY           
         MVI   ANYHIST,C'N'              ASSUME NO HISTORY                      
         CLC   KEY(32),KEYSAVE                                                  
         BNE   PR60                                                             
         MVI   ANYHIST,C'Y'              SET HISTORY RECORD PRESENT             
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',K.OHISDDA,   X        
               AIO1,DMWORK                                                      
*                                                                               
         L     R7,AIO1                                                          
         USING OHISRECD,R7                                                      
         LA    R6,OBDELEM                                                       
         CLI   0(R6),5                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING OBDELEM,R6                                                       
*                                                                               
         CLI   TSFLAG,0            TEST DELETED                                 
         BNE   PR100                                                            
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BNE   PR60                                                             
         MVI   ELCDLO,X'15'        THEN POINT TO PREV VRSN DATA                 
         MVI   ELCDHI,X'15'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         SR    R6,R6               CLEAR ELEM POINTER                           
*                                                                               
PR60     CLI   ANYSPOTS,C'Y'       TEST TO PRINT                                
         BNE   PR100                                                            
         CLI   Q.RQ2CVRSN,0        TEST ORIGINAL ORDER                          
         BE    PR62                YES                                          
         CLI   OBDFLAG,0           TEST BUY WAS DELETED                         
         BNE   PR61                YES - SO NOW IT IS 'NEW' AGAIN               
*                                                                               
         CLI   SHOWCHGS,C'Y'                                                    
         BNE   PR62                                                             
         LTR   R6,R6               TEST NO PREVIOUS ELEM                        
         BZ    *+12                YES - SO MUST BE NEW                         
         CLI   ANYHIST,C'Y'        TEST FOUND HISTORY RECORD                    
         BE    PR62                YES - SO NOT NEW                             
PR61     MVC   P3(11),=C'==> NEW <=='                                           
*                                                                               
PR62     GOTO1 SPOOL,DMCB,(R8)     PRINT BUY DETAILS                            
                                                                                
* CHECK FOR SUPPLEMENTAL RECORDS                                                
                                                                                
         MVC   SVTSRNUM,T.TSRNUM   SAVE CURRENT TSAR REC NUMBER                 
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
PR64     BRAS  RE,CALLTSAR                                                      
         BNE   PR68                                                             
         CLI   TSARSEQ,0           TEST BUYLINE DATA                            
         BE    PR68                YES - NO (MORE) SUPP DATA                    
*                                                                               
         CLI   TSARSEQ,X'0F'       TEST COST OVRD REC (01-09)                   
         BNL   PR64X               NO                                           
*                                                                               
         BRAS  RE,BUYSKED          PRINT COST OVERRIDE AND SPOTS                
         MVC   SKDAYS-2(20),=C'** COST OVERRIDES **'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR64                                                             
*                                                                               
PR64X    CLI   TSARSEQ,X'0F'       TEST AN ORBIT RECORD                         
         BNE   PR65                NO                                           
*                                                                               
         BRAS  RE,PRTORB                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BRAS  RE,CALLTSAR         READ FOR COMMENT RECORD                      
         BNE   PR68                                                             
*                                                                               
PR65     CLI   TSARSEQ,X'10'       TEST A COMMENT RECORD                        
         BL    PR68                NO                                           
*                                                                               
         MVC   SKTIME(14),=C'** COMMENTS **'                                    
         LA    R4,SKGRID           COMMENTS PRINT UNDER GRID                    
*                                                                               
PR66     MVC   0(70,R4),TSCOM                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   PR68                                                             
         CLI   TSARSEQ,0           TEST A COMMENT                               
         BNE   PR66                YES                                          
*                                                                               
PR68     MVC   T.TSRNUM,SVTSRNUM   RESTORE TSAR BUYREC NUMBER                   
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ANYHIST,C'Y'        TEST ORDER HISTORY FOUND                     
         BNE   PR90                                                             
*                                                                               
         LTR   R6,R6               TEST NO PREV ELEMENT                         
         BZ    PR90                                                             
         CLI   OBDFLAG,0           TEST BUY WAS DELETED                         
         BNE   PR90                YES - SO IT WAS NEW AND THAT'S ALL           
*                                                                               
         CLI   SHOWCHGS,C'Y'                                                    
         BNE   PR90                                                             
*                                                                               
         LA    R4,P                                                             
         MVC   0(14,R4),=C'==> CHANGES TO'                                      
         AHI   R4,15                                                            
*                                                                               
         MVI   ANYCHANG,C'N'       ASSUME NO CHANGES                            
*                                                                               
         CLC   OBDDAY,TSBDDAY                                                   
         BE    PR70                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(5,R4),=C'DAYS,'                                                
         AHI   R4,5                                                             
*                                                                               
PR70     CLC   OBDTIME,TSBDTIME                                                 
         BE    PR72                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(5,R4),=C'TIME,'                                                
         AHI   R4,5                                                             
*                                                                               
PR72     CLC   OBDSEC,TSBDSEC                                                   
         BE    PR74                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(8,R4),=C'SPOTLEN,'                                             
         AHI   R4,8                                                             
*                                                                               
PR74     CLC   OBDCOST,TSBDCOST                                                 
         BE    PR76                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(5,R4),=C'COST,'                                                
         AHI   R4,5                                                             
*                                                                               
PR76     CLC   OBDPROG,TSBDPROG                                                 
         BE    PR78                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(8,R4),=C'PROGRAM,'                                             
         AHI   R4,8                                                             
*                                                                               
PR78     OC    NUMDEMS,NUMDEMS     TEST PRINTING DEMOS                          
         BZ    PR80                NO                                           
         CLC   OBDDEMS(32),TSDEMS  TEST DEMOS MATCH                             
         BE    PR80                                                             
         MVI   ANYCHANG,C'Y'                                                    
         MVC   0(6,R4),=C'DEMOS,'                                               
         AHI   R4,6                                                             
*                                                                               
PR80     LA    R1,TSSPTS                                                        
         AH    R1,SPTDSPL          POINT TO FIRST SCHEDULE WEEK                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         USING OHSPTEL,R6                                                       
*                                                                               
         XC    MYSPTS,MYSPTS                                                    
         SR    RF,RF                                                            
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         AHI   RF,-3               SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYSPTS(0),OHDSPTS   MOVE SCHEDULE TO 53 BYTE AREA                
*                                                                               
         LA    RE,MYSPTS                                                        
         AH    RE,SPTDSPL                                                       
         LH    RF,NUMWEEKS         GET NUMBER OF WEEKS THIS PASS                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(RE)                                                    
         BE    PR82                                                             
         MVC   0(8,R4),=C'SCHEDULE'                                             
         AHI   R4,8                                                             
         MVI   ANYCHANG,C'Y'       SET CHANGED                                  
*                                                                               
PR82     BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         AHI   R4,1                                                             
*                                                                               
PR84     CLI   ANYCHANG,C'Y'       TEST ANY CHANGES                             
         BE    PR90                                                             
         MVC   P,SPACES                                                         
         B     PR100                                                            
*                                                                               
PR90     GOTO1 SPOOL,DMCB,(R8)     PRINT CHANGE COMMENTS                        
*                                                                               
PR100    BRAS  RE,UPDORHIS         UPDATE ORDER HISTORY RECORD                  
*                                                                               
PR110    LA    R1,P                CLEAR PRINT LINES USED AS SAVE AREA          
         LHI   R0,4                                                             
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
         B     PR40                BACK FOR NEXT TSAR RECORD                    
         DROP  R7                                                               
         DROP  K                                                                
         EJECT                                                                  
*=================================================================              
* PRINT SCHEDULE TOTALS                                                         
*=================================================================              
                                                                                
PR200    OC    PASSSPTS,PASSSPTS   TEST ANYTHING TO PRINT                       
         BZ    PR230                                                            
         MVI   ALLOWLIN,4                                                       
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
         MVC   P+4(10),=C'TOTALS FOR'                                           
         SR    R4,R4                                                            
         IC    R4,PASS                                                          
         BCTR  R4,0                                                             
         MHI   R4,12                                                            
         LA    R4,PASSTAB(R4)      POINT TO DATES THIS PASS                     
         GOTO1 DATCON,DMCB,(R4),(8,P+15)                                        
         MVI   P+23,C'-'                                                        
         GOTO1 (RF),(R1),6(R4),(8,P+24)                                         
                                                                                
* SEE IF ANY WEEK HAS MORE THAN 100 SPOTS - IF SO, STAGGER TOTALS               
                                                                                
         MVI   TOOMANY,C'N'                                                     
         LHI   R4,14                                                            
         LA    R6,WEEKSPTS                                                      
*                                                                               
PR201A   CLC   0(2,R6),=H'99'                                                   
         BH    PR201B                                                           
         AHI   R6,2                                                             
         BCT   R4,PR201A                                                        
         B     PR201X                                                           
*                                                                               
PR201B   MVI   TOOMANY,C'Y'                                                     
*                                                                               
PR201X   LHI   R4,7                SET COUNTER                                  
         LA    R5,SKGRID           POINT TO FIRST PRINT POSN                    
         LA    R6,WEEKSPTS                                                      
*                                                                               
PR202    LH    R0,0(R6)                                                         
         LH    R1,2(R6)                                                         
*                                                                               
         LTR   R0,R0                                                            
         BZ    PR204                                                            
         EDIT  (R0),(3,(R5))                                                    
         MVI   3(R5),C'*'                                                       
*                                                                               
PR204    AHI   R5,4                                                             
         LTR   R1,R1                                                            
         BZ    PR206                                                            
         CLI   TOOMANY,C'Y'                                                     
         BE    PR205                                                            
         EDIT  (R1),(3,(R5))                                                    
         MVI   3(R5),C'*'                                                       
         B     PR206                                                            
*                                                                               
PR205    EDIT  (R1),(3,132(R5))                                                 
         MVI   135(R5),C'*'                                                     
*                                                                               
PR206    AHI   R5,4                NEXT OUTPUT WEEK                             
         AHI   R6,4                                                             
         BCT   R4,PR202                                                         
*                                                                               
         ICM   R0,15,PASSSPTS                                                   
         BZ    PR210                                                            
         EDIT  (R0),SKTOTSP                                                     
         MVI   SKTOTSP+4,C'*'                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR210    L     RE,PASSSPTS         ADD PASSTOTS TO RPTTOTS                      
         A     RE,RPTSPOTS                                                      
         ST    RE,RPTSPOTS                                                      
*                                                                               
         L     RE,PASSDOLS                                                      
         A     RE,RPTDOLS                                                       
         ST    RE,RPTDOLS                                                       
*                                                                               
         L     RE,PASSTAX                                                       
         A     RE,RPTTAX                                                        
         ST    RE,RPTTAX                                                        
*                                                                               
         OC    NUMDEMS,NUMDEMS     TEST PRINTING DEMOS                          
         BZ    PR220               NO - SKIP DEMO TOTALING                      
*                                                                               
         LHI   R0,8                ADD PASS DEMS/EQDEMS TO RPT DEMS             
         LA    R4,PASSDEMS                                                      
         LA    R5,RPTDEMS                                                       
*                                                                               
PR212    L     RE,0(R4)                                                         
         N     RE,=X'3FFFFFFF'                                                  
*                                                                               
         L     RF,0(R5)                                                         
         N     RF,=X'3FFFFFFF'                                                  
*                                                                               
         AR    RE,RF                                                            
         TM    0(R4),X'40'                                                      
         BZ    *+8                                                              
         O     RE,=X'40000000'                                                  
         ST    RE,0(R5)                                                         
*                                                                               
         AHI   R4,4                                                             
         AHI   R5,4                                                             
         BCT   R0,PR212                                                         
                                                                                
* PRINT TOTALS FOR THIS PERIOD                                                  
                                                                                
PR220    MVC   P(12),=C'123456 SPOTS'                                           
         L     R0,PASSSPTS                                                      
         EDIT  (R0),(6,P)                                                       
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   P+11,C' '                                                        
*                                                                               
         MVI   EXCLTAX,C'N'                                                     
         CLI   S00PROF+12,C'Y'     TEST TO EXCLUDE TAX                          
         BNE   PR220A                                                           
         OC    PASSTAX,PASSTAX     TEST ANY TAX THIS TIME                       
         BZ    *+8                                                              
         MVI   EXCLTAX,C'Y'                                                     
*                                                                               
PR220A   L     R0,PASSDOLS                                                      
         CLI   EXCLTAX,C'Y'                                                     
         BNE   *+8                                                              
         S     R0,PASSTAX                                                       
         EDIT  (R0),(12,P+13),2,ALIGN=LEFT,FLOAT=$                              
*                                                                               
         CLI   EXCLTAX,C'Y'        TEST TAX EXCLUDED                            
         BNE   PR221                                                            
         MVC   P2+7(16),=C'EXCLUDING TAX OF'                                    
         L     R0,PASSTAX                                                       
         EDIT  (R0),(8,P2+24),2,ALIGN=LEFT,FLOAT=$                              
*                                                                               
PR221    LA    R4,PASSDEMS                                                      
         SR    R5,R5                                                            
         ICM   R5,3,NUMDEMS                                                     
         BZ    PR230                                                            
*                                                                               
PR222    TM    0(R4),X'40'                                                      
         BZ    PR224                                                            
         L     R1,0(R4)            ROUND 2-DEC TO 1-DEC PRECISION               
         N     R1,=X'3FFFFFFF'                                                  
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,0(R4)                                                         
*                                                                               
PR224    AHI   R4,8                NEXT UNEQUIV DEMO                            
         BCT   R5,PR222                                                         
*                                                                               
         LA    R3,SVDEMNMS                                                      
         LA    R4,PASSDEMS                                                      
         LA    R5,P+30                                                          
         LH    R6,NUMDEMS                                                       
*                                                                               
PR226    MVC   0(6,R5),0(R3)       DEMO NAME                                    
         L     R0,0(R4)                                                         
         EDIT  (R0),(9,8(R5)),1,ALIGN=LEFT                                      
         AHI   R3,6                NEXT DEMO NAME                               
         AHI   R4,8                NEXT UNEQIV DEMO VALUE                       
         AHI   R5,20                                                            
         BCT   R6,PR226                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR230    MVC   P,SPACES                                                         
         SR    R0,R0                                                            
         IC    R0,PASS                                                          
         AHI   R0,1                                                             
         CLM   R0,1,MAXPASS                                                     
         BH    PR232                                                            
         STC   R0,PASS                                                          
         B     PR30                                                             
*                                                                               
PR232    MVI   P,0                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
         MVC   P(30),=C'*****  TOTALS FOR ORDER  *****'                         
         MVC   P2(14),=C'* 123456 SPOTS'                                        
         MVI   P2+29,C'*'                                                       
         L     R0,RPTSPOTS                                                      
         EDIT  (R0),(6,P2+2)                                                    
*                                                                               
         MVI   EXCLTAX,C'N'                                                     
         CLI   S00PROF+12,C'Y'     TEST TO EXCLUDE TAX                          
         BNE   PR234                                                            
         OC    RPTTAX,RPTTAX       TEST ANY TAX THIS TIME                       
         BZ    *+8                                                              
         MVI   EXCLTAX,C'Y'                                                     
*                                                                               
PR234    L     R0,RPTDOLS                                                       
         CLI   EXCLTAX,C'Y'                                                     
         BNE   *+8                                                              
         S     R0,RPTTAX                                                        
         EDIT  (R0),(12,P2+16),2,ALIGN=LEFT,FLOAT=$                             
*                                                                               
         CLI   EXCLTAX,C'Y'        TEST TAX EXCLUDED                            
         BNE   PR236                                                            
         MVC   P3(19),=C'*  EXCLUDING TAX OF'                                   
         L     R0,RPTTAX                                                        
         EDIT  (R0),(8,P3+20),2,ALIGN=LEFT,FLOAT=$                              
         MVI   P3+29,C'*'                                                       
*                                                                               
PR236    LA    R4,P3               SET CLOSING *'S                              
         CLI   EXCLTAX,C'Y'                                                     
         BNE   *+8                                                              
         LA    R4,P4                                                            
         MVI   0(R4),C'*'                                                       
         MVC   1(29,R4),0(R4)                                                   
*                                                                               
PR240    GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
* UPDATE TOTAL DOLLARS AND SPOTS IN ORDER RECORD                                
                                                                                
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BE    PRX                                                              
         CLI   COPYTYPE,C'X'       TEST FAX COPY                                
         BNE   PRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO ORDER RECORD                        
         AHI   R6,24                                                            
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DOSPELD,R6                                                       
         L     R0,RPTDOLS                                                       
         CVD   R0,DUB                                                           
         ZAP   DOSPTOTL,DUB                                                     
         MVC   DOSPSPTS,RPTSPOTS                                                
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
PRX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* COMMON CALL TO TSAR                                                           
*=================================================================              
                                                                                
CALLTSAR LR    R0,RE                                                            
         LA    RE,64                   TSAR BLOCK + TSARKEY                     
         STC   RE,TSARTRC              SET DATA LENGTH IN BYTE 0                
         GOTO1 DATAMGR,DMCB,QDMTRACE,QDMDATA,TSARTRC                            
         ORG   *-2                                                              
         DC    X'0700'                 0DEF TO ENABLE TRACE                     
*                                                                               
CALLTS2  GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         J     CALLTS4                                                          
         OC    SPOOLDM,SPOOLDM     MAKE SURE PRTQ OPEN                          
         JZ    CALLTS4                                                          
         BRAS  RE,CALLTRC          TRACE ROUTINE FOR DEBUGGING                  
*                                                                               
CALLTS4  CLI   T.TSERRS,0              SET CC ON EXIT                           
         LR    RE,R0                                                            
         BER   RE                                                               
* AN ERROR HAS OCCURRED                                                         
         CLI   T.TSACTN,TSAADD     TEST ACTION = ADD                            
         JNE   CALLTSX                                                          
         TM    T.TSERRS,X'80'      TEST EOF                                     
         JZ    CALLTSX                                                          
         DC    H'0'                                                             
*                                                                               
CALLTSX  LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
*                                                                               
CALLTRC  NTR1  BASE=*,LABEL=*                                                   
         S     R0,SPOM29RB                                                      
         ST    R0,WORD                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,WORD,P+2,4                                             
         MVC   P(1),T.TSACTN                                                    
         GOTO1 (RF),DMCB,TSARKEY,P+10,6                                         
         GOTO1 (RF),DMCB,TSARREC+6,P2,64                                        
         GOTO1 (RF),(R1),TSARREC+70,P3,64                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* UPDATE ORDER HISTORY RECORD AS REQUIRED                                       
*=================================================================              
                                                                                
K        USING OHISRECD,KEY                                                     
*                                                                               
UPDORHIS NTR1  BASE=*,LABEL=*                                                   
         XC    SVEL05,SVEL05                                                    
         XC    SVEL06,SVEL06                                                    
         XC    SVEL09,SVEL09                                                    
*                                                                               
         CLC   PASS,MAXPASS        TEST LAST PASS                               
         JNE   EXIT                NO - UPDATE LAST PASS ONLY !                 
         CLI   COPYTYPE,C'X'       ONLY UPDATE FOR FAX COPY                     
         JNE   EXIT                                                             
*                                                                               
         LA    R0,SVEL08           THIS OVERWRITES PRINT LINES                  
         LHI   R1,SVEL08X-SVEL08                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    FULL,FULL                                                        
         CLC   KEY(32),KEYSAVE     DID WE FIND THE RECORD                       
         BNE   UPD10               NO                                           
*                                                                               
         MVI   ELCDLO,X'99'        FIND OLD ACTIVITY ELEM                       
         MVI   ELCDHI,X'99'                                                     
         L     R6,AIO1                                                          
         AHI   R6,42                                                            
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING OHACTVEL,R6                                                      
         MVC   FULL(3),OHACTADD    SAVE ELEM ADD DATE                           
*                                                                               
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BNE   UPD2                                                             
         LA    R1,2(R6)                                                         
         OC    0(3,R1),0(R1)       TEST HAVE ADD DATE                           
         BZ    *+8                                                              
         AHI   R1,3                POINT TO CHANGE DATE                         
         MVC   0(3,R1),TODAYB      SET NEW ACTIVITY DATE                        
         B     UPD80               AND WRITE RECORD NOW                         
*                                                                               
UPD2     MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         L     R6,AIO1                                                          
         AHI   R6,42                                                            
         BRAS  RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVEL05,0(R6)                                                     
         MVI   SVEL05,X'15'        SET PREVIOUS VERSION ELCODE                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),6                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVEL06,0(R6)                                                     
         MVI   SVEL06,X'16'        SET PREVIOUS VERSION ELCODE                  
*                                                                               
         LA    R4,SVEL08                                                        
*                                                                               
UPD4     SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),8                                                          
         BL    UPD4                                                             
         BH    UPD6                                                             
         MVC   0(64,R4),0(R6)      MOVE ELEMENT AT MAX LEN                      
         MVI   0(R4),X'18'         SET PREVIOUS VERSION ELCODE                  
         AHI   R4,L'SVEL08                                                      
         B     UPD4                                                             
*                                                                               
UPD6     CLI   0(R6),9             TEST FOR TAX                                 
         BNE   UPD10                                                            
         MVC   SVEL09,0(R6)                                                     
         MVI   SVEL09,X'19'                                                     
*                                                                               
UPD10    L     R0,AIO1             CREATE A NEW RECORD                          
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R7,AIO1                                                          
         USING OHISRECD,R7                                                      
*                                                                               
         MVC   0(32,R7),KEYSAVE    MOVE IN RECORD KEY                           
         LHI   R0,OBDELEM-OHISRECD SET MIN LEN                                  
         STCM  R0,3,OHISLEN                                                     
*                                                                               
E        USING OBDELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   ELEM,OBDELEMQ                                                    
         MVI   ELEM+1,OBDELEMX-OBDELEM                                          
*                                                                               
         CLI   TSFLAG,0              TEST DELETED BUYREC                        
         BE    *+14                  NO                                         
         MVC   E.OBDFLAG,Q.RQ2CVRSN  ELSE SAVE VERSION NUMBER                   
         B     UPD20                                                            
*                                                                               
         MVC   E.OBDDAY,TSBDDAY                                                 
         MVC   E.OBDTIME,TSBDTIME                                               
         MVC   E.OBDSEC,TSBDSEC                                                 
         MVC   E.OBDCOST,TSBDCOST                                               
         MVC   E.OBDCIND,TSBDCIND                                               
         MVC   E.OBDCIND2,TSBDCIN2                                              
         MVC   E.OBDPROG,TSBDPROG                                               
         MVC   E.OBDDEMS(32),TSDEMS                                             
         DROP  E                                                                
*                                                                               
UPD20    SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,OHSPTELQ                                                    
         MVI   ELEM+1,3            SET MIN LENGTH                               
         CLI   TSFLAG,0            TEST DELETED                                 
         BNE   UPD30               YES - DO MIN ELEM                            
         OC    TSSPTS,TSSPTS       ANY SPOTS SCHEDULED                          
         BZ    UPD30                                                            
*                                                                               
         MVC   ELEM+2(53),TSSPTS                                                
         LA    RF,ELEM+54          POINT TO LAST POSSIBLE WEEK                  
         CLI   0(RF),0                                                          
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,ELEM                                                          
         SR    RF,R0                                                            
         AHI   RF,1                                                             
         STC   RF,ELEM+1           SET ELEM LEN                                 
*                                                                               
UPD30    L     R7,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,OHDOLELQ                                                    
         MVI   ELEM+1,3            SET MIN LENGTH                               
         CLI   TSFLAG,0            TEST DELETED RECORD                          
         BNE   UPD44                                                            
         OC    TSDOLS,TSDOLS       ANY DOLLARS ?                                
         BZ    UPD44                                                            
*                                                                               
         MVC   ELEM+2(212),TSDOLS                                               
         LA    RF,ELEM+2+208       POINT TO LAST POSSIBLE WEEK                  
*                                                                               
UPD40    OC    0(4,RF),0(RF)                                                    
         BNZ   UPD42                                                            
         AHI   RF,-4                                                            
         B     UPD40                                                            
*                                                                               
UPD42    LA    R0,ELEM                                                          
         SR    RF,R0                                                            
         AHI   RF,4                                                             
         STC   RF,ELEM+1           SET ELEM LEN                                 
*                                                                               
UPD44    L     R7,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
                                                                                
* ADD COST OVERRIDE ELEMENTS                                                    
                                                                                
         CLI   TSFLAG,0            TEST DELETED RECORD                          
         BNE   UPD60                                                            
         LA    R4,COVTAB                                                        
         LHI   R5,8                                                             
*                                                                               
UPD50    XC    ELEM,ELEM                                                        
         MVI   ELEM,OHCOVELQ                                                    
         OC    0(3,R4),0(R4)       TEST DATA                                    
         BZ    UPD52                                                            
*                                                                               
         MVC   ELEM+2(56),0(R4)    MOVE COST(3) AND SPOTS                       
         LA    RF,ELEM+57          POINT TO LAST POSSIBLE WEEK                  
         CLI   0(RF),0                                                          
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,ELEM                                                          
         SR    RF,R0                                                            
         AHI   RF,1                                                             
         STC   RF,ELEM+1           SET ELEM LEN                                 
*                                                                               
         L     R7,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
*                                                                               
         AHI   R4,L'COVTAB                                                      
         BCT   R5,UPD50                                                         
*                                                                               
UPD52    XC    ELEM,ELEM           TEST TO ADD TAX ELEM                         
         MVI   ELEM,OHTAXELQ                                                    
         MVI   ELEM+1,3            SET MIN LENGTH                               
         CLI   TSFLAG,0            TEST DELETED RECORD                          
         BNE   UPD60                                                            
         OC    TSTAX,TSTAX         ANY TAX ?                                    
         BZ    UPD60                                                            
*                                                                               
         MVC   ELEM+2(212),TSTAX                                                
         LA    RF,ELEM+2+208       POINT TO LAST POSSIBLE WEEK                  
*                                                                               
UPD54    OC    0(4,RF),0(RF)                                                    
         BNZ   UPD56                                                            
         AHI   RF,-4                                                            
         B     UPD54                                                            
*                                                                               
UPD56    LA    R0,ELEM                                                          
         SR    RF,R0                                                            
         AHI   RF,4                                                             
         STC   RF,ELEM+1           SET ELEM LEN                                 
*                                                                               
         L     R7,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
*                                                                               
UPD60    XC    ELEM,ELEM                                                        
         MVI   ELEM,X'99'                                                       
         MVI   ELEM+1,8                                                         
         MVC   ELEM+2(3),FULL      ELEM ADD DATE                                
         LA    R1,ELEM+2                                                        
         OC    0(3,R1),0(R1)       TEST HAVE ADD DATE                           
         BZ    *+8                                                              
         AHI   R1,3                POINT TO CHANGE DATE                         
         MVC   0(3,R1),TODAYB                                                   
*                                                                               
         L     R7,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,OHISLEN                                                     
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,(C'T',AIO1),ELEM,(R7)                                 
*                                                                               
         CLC   KEY(32),KEYSAVE     DID WE FIND THE RECORD                       
         BE    UPD70               YES                                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'ADDREC'),=C'XSPFIL',K.OHISDDA,   X        
               AIO1,DMWORK                                                      
         J     EXIT                                                             
*                                                                               
UPD70    SR    R0,R0               INSERT PREVIOUS VERSION ELEMENTS             
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,,SVEL05,(R7)                                          
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,,SVEL06,(R7)                                          
*                                                                               
         LA    R4,SVEL08                                                        
         LHI   R5,8                                                             
*                                                                               
UPD72    CLI   0(R4),X'18'         IS THERE ANOTHER ELEMENT                     
         BNE   UPD74                                                            
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,,(R4),(R7)                                            
         AHI   R4,L'SVEL08                                                      
         BCT   R5,UPD72                                                         
*                                                                               
UPD74    CLI   SVEL09,X'19'        TEST TAX ELEMENT                             
         BNE   UPD80                                                            
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         GOTO1 RECUP,DMCB,,SVEL09,(R7)                                          
*                                                                               
UPD80    GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),=C'XSPFIL',K.OHISDDA,   X        
               AIO1,DMWORK                                                      
         J     EXIT                                                             
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SET SPOT COUNTS ACROSS ALL RATE TYPES FOR THIS BUY                            
*==============================================================                 
                                                                                
SETMYSPT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYSPTS,TSSPTS       SET SPOTS FOR BDCOST                         
         MVC   SVTSRNUM,T.TSRNUM   SAVE CURRENT TSAR REC NUMBER                 
         MVI   T.TSACTN,TSANXT     READ FOR ADDITIONAL RECORDS                  
*                                                                               
         LA    R0,COVTAB                                                        
         LHI   R1,COVTABX-COVTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,COVTAB                                                        
*                                                                               
SETMYSP2 BRAS  RE,CALLTSAR                                                      
         BNE   SETMYSPX                                                         
*                                                                               
         CLI   TSARSEQ,1           TEST RATE DATA                               
         BL    SETMYSPX            NO - DONE                                    
         CLI   TSARSEQ,8                                                        
         BH    SETMYSPX                                                         
*                                                                               
         MVC   0(3,R6),TSBDCOST    MOVE COST OVERRIDE                           
         OC    0(3,R6),0(R6)                                                    
         BNZ   *+8                                                              
         OI    0(R6),X'80'                                                      
         MVC   3(53,R6),TSSPTS     AND SPOTS                                    
         AHI   R6,L'COVTAB                                                      
*                                                                               
         LA    RE,MYSPTS                                                        
         LA    RF,TSSPTS                                                        
         LHI   R0,53                                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
*                                                                               
SETMYSP4 IC    R4,0(RE)                                                         
         IC    R5,0(RF)                                                         
         AR    R4,R5                                                            
         STC   R4,0(RE)                                                         
*                                                                               
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,SETMYSP4                                                      
         B     SETMYSP2                                                         
*                                                                               
SETMYSPX MVC   T.TSRNUM,SVTSRNUM   RESTORE TSAR REC NUMBER                      
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PRINT THE BUY SCHEDULE                                                        
* MYSPTS HAS SPOT COUNTS BY WEEK FOR ALL RATES FOR THIS BUYLINE                 
* SO THAT IF ANY RATE IS ACTIVE, THE BUY DESC DATA WILL PRINT!                  
*===============================================================                
                                                                                
BUYSKED  NTR1  BASE=*,LABEL=*                                                   
                                                                                
* COUNT NUMBER OF DAYS (OR WEEKS) IN THIS PERIOD                                
                                                                                
         LA    R4,WEEKTAB                                                       
         AH    R4,DOLDSPL          ADD 4 BYTE DSPL TO FIRST WEEK                
         SR    RE,RE                                                            
*                                                                               
BUYSK2   AHI   R4,4                NEXT WEEK IN TABLE                           
         CLC   0(2,R4),BPEREND     TEST STILL IN PERIOD                         
         BH    *+8                                                              
         BCT   RE,BUYSK2                                                        
*                                                                               
         BCTR  RE,0                ADD ONE TO COUNT                             
         LPR   RE,RE                                                            
         STH   RE,NUMWEEKS         AND SAVE WEEKS IN THIS PERIOD                
                                                                                
         XC    BUYSPTS,BUYSPTS     GET BUY TOTAL SPOTS/DOLS THIS PER            
         XC    BUYDOLS,BUYDOLS                                                  
         XC    BUYTAX,BUYTAX                                                    
                                                                                
         LA    RE,TSSPTS           SPTS/WEEK TABLE                              
         AH    RE,SPTDSPL                                                       
         LA    RF,TSDOLS           DOLS/WEEK TABLE                              
         AH    RF,DOLDSPL                                                       
         LH    R0,NUMWEEKS                                                      
*                                                                               
BUYSK4   SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         A     R1,BUYSPTS                                                       
         ST    R1,BUYSPTS                                                       
         ICM   R1,15,0(RF)                                                      
         A     R1,BUYDOLS                                                       
         ST    R1,BUYDOLS                                                       
         ICM   R1,15,L'TSDOLS(RF)                                               
         A     R1,BUYTAX                                                        
         ST    R1,BUYTAX                                                        
*                                                                               
         AHI   RE,1                NEXT WEEK IN SPOT TABLE                      
         AHI   RF,4                NEXT WEEK IN DOLLAR TABLE                    
         BCT   R0,BUYSK4                                                        
*                                                                               
         L     R1,BUYSPTS                                                       
         A     R1,PASSSPTS                                                      
         ST    R1,PASSSPTS                                                      
*                                                                               
         L     R1,BUYDOLS                                                       
         A     R1,PASSDOLS                                                      
         ST    R1,PASSDOLS                                                      
*                                                                               
         L     R1,BUYTAX                                                        
         A     R1,PASSTAX                                                       
         ST    R1,PASSTAX                                                       
*                                                                               
         MVI   ANYSPOTS,C'N'                                                    
         CLI   TSARSEQ,0           TEST BUY DESC REC                            
         BNE   BUYSK6X                                                          
*                                                                               
         LA    R3,MYSPTS           POINT TO MY SPOT TABLE                       
         AH    R3,SPTDSPL          POINT TO FIRST WEEK THIS PASS                
*                                                                               
         LA    R4,WEEKTAB                                                       
         AH    R4,DOLDSPL          ADD 4 BYTE DSPL TO FIRST WEEK                
*                                                                               
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
BUYSK6   ICM   R0,1,0(R3)          GET SPOT COUNTER                             
         AR    RE,R0                                                            
         AHI   R3,1                                                             
         AHI   R4,4                                                             
         CLC   0(2,R4),BPEREND     TEST REACHED END OF THIS PASS                
         BNH   BUYSK6                                                           
         LTR   RE,RE                                                            
         BZ    BUYSKX              NO ACTIVITY THIS PASS                        
*                                                                               
         ST    RE,MYBUYSPT         SAVE SPOT COUNT FOR CPP CALC                 
         MVI   ANYSPOTS,C'Y'       SET FLAG TO FORCE PRINTING                   
*                                                                               
BUYSK6X  LA    R3,TSSPTS           POINT TO SCHEDULE                            
         AH    R3,SPTDSPL          POINT TO FIRST WEEK                          
*                                                                               
         LA    R4,WEEKTAB                                                       
         AH    R4,DOLDSPL          ADD 4 BYTE DSPL TO FIRST WEEK                
*                                                                               
         LA    R5,SKGRID           POINT TO FIRST PRINT POSN                    
         LA    R6,WEEKSPTS                                                      
*                                                                               
BUYSK8   SR    R0,R0                                                            
         ICM   R0,1,0(R3)          GET SPOT COUNT                               
*                                                                               
         CLI   PASSACT,C'Y'                                                     
         BE    BUYSK10                                                          
         MVI   PASSACT,C'Y'                                                     
         CLI   FORCEMID,C'Y'       IF MIDLINES PENDING                          
         BE    BUYSK10             DON'T FORCE HEADLINES AGAIN                  
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
BUYSK10  LH    R1,0(R6)                                                         
         AR    R1,R0                                                            
         STH   R1,0(R6)                                                         
*                                                                               
         MVI   ANYSPOTS,C'Y'                                                    
         LTR   R0,R0                                                            
         BZ    BUYSK12                                                          
         EDIT  (R0),(3,(R5))                                                    
*                                                                               
BUYSK12  AHI   R3,1                NEXT TSSPTS VALUE                            
         AHI   R4,4                NEXT WEEKTAB ENTRY                           
         AHI   R5,4                NEXT GRID POSITION                           
         AHI   R6,2                NEXT WEEK TOTAL                              
         CLC   0(2,R4),BPEREND     TEST REACHED END OF THIS PASS                
         BNH   BUYSK8                                                           
*                                                                               
         L     R0,BUYSPTS                                                       
         EDIT  (R0),(4,SKTOTSP)    PUT OUT TOTAL SPOTS THIS LINE                
         MVI   SKTOTSP+4,C'*'                                                   
*                                                                               
         CLI   ANYSPOTS,C'Y'                                                    
         BNE   BUYSKX                                                           
         CLI   TSARSEQ,0           TEST BUYLINE DATA                            
         BNE   BUYSK15                                                          
*                                                                               
         MVC   SKBUYLIN,QEST                                                    
         MVI   SKBUYLIN+3,C'-'                                                  
         SR    R0,R0                                                            
         IC    R0,TSARLIN+1        GET BUYLINE NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKBUYLIN+4(3),DUB                                                
*                                                                               
         LHI   R0,7                                                             
         LA    R1,SKDAYS                                                        
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,TSBDDAY                                                       
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
BUYSK14  LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,BUYSK14                                                       
*                                                                               
         GOTO1 UNTIME,DMCB,TSBDTIME,SKTIME                                      
*                                                                               
         MVC   SKPROG,TSBDPROG                                                  
*                                                                               
BUYSK15  CLI   SDXCOSTS,C'Y'       TEST TO PRINT COST                           
         BNE   BUYSK18                                                          
         SR    R0,R0                                                            
         ICM   R0,7,TSBDCOST                                                    
         TM    TSBDCIN2,X'10'      TEST COST IN DOLLARS                         
         BO    BUYSK16X                                                         
         C     R0,=F'99999999'      MAX COST IN 8 DIGITS                        
         BH    BUYSK16                                                          
         EDIT  (R0),(9,SKCOST),2                                                
         B     BUYSK18                                                          
*                                                                               
BUYSK16  AR    R0,R0               PRINT COST IN $                              
         SRDL  R0,32                                                            
         D     R0,=F'100'                                                       
         SRA   R1,1                                                             
         LR    R0,R1                                                            
BUYSK16X EDIT  (R0),(9,SKCOST),0,FLOAT=$                                        
*                                                                               
BUYSK18  CLI   TSARSEQ,0           TEST BUYLINE DETAIL                          
         BNE   BUYSKX                                                           
         SR    R0,R0                                                            
         IC    R0,TSBDSEC                                                       
         EDIT  (R0),(3,SKSLN)                                                   
         DROP  R6                                                               
         EJECT                                                                  
*=============================================================                  
* DEMOS                                                                         
*=============================================================                  
                                                                                
BUYSK20  LA    R3,SVDEMNMS                                                      
         LA    R4,TSDEMS                                                        
         LA    R5,SKDEMS                                                        
         SR    R6,R6                                                            
         ICM   R6,3,NUMDEMS                                                     
         BZ    BUYSKX                                                           
*                                                                               
BUYSK22  MVC   0(6,R5),0(R3)       MOVE DEMO NAME                               
         ICM   R1,15,4(R4)         DEMO FROM TSAR RECORD                        
         N     R1,=X'3FFFFFFF'     DROP FLAGS                                   
         TM    4(R4),X'40'         TEST 2 DECIMALS                              
         BZ    BUYSK24                                                          
         C     R1,=F'9999'         MAX VALUE TO 2 DEC                           
         BH    BUYSK23                                                          
         EDIT  (R1),(5,7(R5)),2                                                 
         B     BUYSK26                                                          
*                                                                               
BUYSK23  M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
BUYSK24  C     R1,=F'9999'         MAX VALUE TO 1 DEC                           
         BH    BUYSK25                                                          
         EDIT  (R1),(5,7(R5)),1                                                 
         B     BUYSK26                                                          
*                                                                               
BUYSK25  M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(5,7(R5))                                                   
*                                                                               
BUYSK26  TM    4(R4),X'80'         TEST OVERRIDE                                
         BZ    *+8                                                              
         MVI   12(R5),C'*'                                                      
*                                                                               
         CLI   SDXCPP,C'Y'         TEST TO PRINT CPP                            
         BNE   BUYSK30                                                          
                                                                                
*====================================================================           
* NOTE THAT BUYDOLS GOES ACROSS RATE TYPES BUT BUYSPOTS IS FOR CURRENT          
* RATE TYPE ONLY                                                                
* USE MYBUYSPT (COMPUTED ABOVE) WHICH IS TOTAL FOR THIS PASS ACROSS             
* ALL RATE TYPES                                                                
*====================================================================           
                                                                                
         L     R1,BUYDOLS          BUY DOLLARS IN PENNIES                       
*                                                                               
         CLI   S00PROF+12,C'Y'     TEST EXCLUDE TAX FOR CPP CALC                
         BNE   *+8                                                              
         S     R1,BUYTAX                                                        
*                                                                               
         LHI   R0,20               SET TO MULT X 10 X 2                         
         TM    4(R4),X'40'         TEST DEMO TO 2-DEC                           
         BZ    *+8                                                              
         LHI   R0,200              THEN MULT DOLS X 100 X 2                     
         MR    R0,R0                                                            
         L     RF,4(R4)            DEMO VALUE                                   
         N     RF,=X'3FFFFFFF'                                                  
         M     RE,MYBUYSPT         USE SPOT COUNT ACROSS RATES                  
         LTR   RF,RF                                                            
         BZ    BUYSK30                                                          
         DR    R0,RF                                                            
         AHI   R1,1                                                             
         SRL   R1,1                GIVES CPP TO 2-DEC                           
         C     R1,=F'99999'        MAX VALUE TO 2-DEC                           
         BH    BUYSK28                                                          
         EDIT  (R1),(6,16(R5)),2                                                
         B     BUYSK30                                                          
*                                                                               
BUYSK28  M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,16(R5)),0,FLOAT=$                                        
*                                                                               
BUYSK30  LA    R3,6(R3)            NEXT DEMO NAME                               
         AHI   R4,8                NEXT DEMO IN TSAR RECORD                     
         AHI   R5,132              NEXT DEMO GOES ON NEXT LINE                  
         BCT   R6,BUYSK22                                                       
*                                                                               
         BRAS  RE,EQUIV            GET EQUIVALENCED DEMO VALUES                 
*                                                                               
         LH    R0,NUMDEMS          AND ADD THEM TO PASS TOTALS                  
         LA    R1,PASSDEMS                                                      
         LA    R3,TSDEMS           UNEQUIV DEMO TYPES/VALUES                    
         LA    R4,EQUDEMS          EQUIV VALUES                                 
*                                                                               
BUYSK32  L     RF,4(R3)            UNEQUIV DEMO VALUE                           
         N     RF,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         M     RE,MYBUYSPT         USE SPOT COUNT ACROSS RATE TYPES             
         A     RF,0(R1)            ADD TO UNEQUIV TOT                           
         TM    4(R3),X'40'         TEST 2-DEC                                   
         BZ    *+8                                                              
         O     RF,=X'40000000'                                                  
         ST    RF,0(R1)                                                         
*                                                                               
         L     RF,0(R4)            GET EQUIV DEMO VALUE                         
         N     RF,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         M     RE,BUYSPTS          X SPOTS THIS BUYLINE                         
         A     RF,4(R1)                                                         
         TM    0(R4),X'40'         TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         O     RF,=X'40000000'                                                  
         ST    RF,4(R1)                                                         
*                                                                               
         AHI   R1,8                NEXT 00D9....                                
         AHI   R3,8                NEXT UNEQ/EQ PAIR                            
         AHI   R4,4                NEXT EQUIV DEMO                              
         BCT   R0,BUYSK32                                                       
*                                                                               
BUYSKX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* PRINT COVER PAGE AND ASSOCIATED COMMENTS NOT PART OF ORDER                    
*                                                                               
* -- COVER SHEET - RECEIVER DATA --                                             
*    STATION CALL LETTERS                                                       
*    STATION FAX NUMBER (FROM OM LAST METHOD)                                   
*    ATTENTION  (FROM OM LAST METHOD OR ORDER)                                  
*    TEL NUMBER (FROM OM DESTIN RECORD)                                         
*                                                                               
* -- COVER SHEET SENDER DATA --                                                 
*    BUYER NAME (FROM OM BUYER REC)/AGENCY/AGENCY ADDR                          
*    ORIGINATING ID                                                             
*    TEL NUMBER   "  "                                                          
*    FAX NUMBER   "  "                                                          
*    EMAIL ADDR   "  "                                                          
*    STD COMMENT  "  "                                                          
*    ORD COMMENT  "  "                                                          
*                                                                               
* -- DX REPORT --                                                               
*    OCOM COMMENTS (VIA SPGETOMCOM)                                             
*    ORDER STD COMMENTS                                                         
*    ORDER COMMENTS                                                             
*================================================================*              
                                                                                
PRTCOVER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'PQO',CONACT                                                   
         BNE   PC1                                                              
         MVC   P(26),=C'** NO FAX - PRTQUE ONLY **'                             
         MVC   P+27(26),P                                                       
         MVC   P+53(26),P                                                       
         MVC   P+80(26),P                                                       
         MVC   P2,P                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
D        USING DOWIGELD,SVWIGEL                                                 
                                                                                
PC1      LA    R4,P                                                             
         MVC   4(15,R4),=C'*HDR*EDICT=*BDE'                                     
         CLI   D.DOWIGMTH,C'E'          TEST EMAIL                              
         BE    PC2                                                              
         MVC   9(4,R4),=C'FAX '         OVERWRITE EDICT=                        
         MVC   13(16,R4),D.DOWIGFXN     AND THEN THE FAX NUMBER                 
         OC    13(16,R4),SPACES                                                 
*                                                                               
PC2      MVI   34(R4),C'W'              132 CHARS WIDE                          
         MVI   35(R4),C'P'              PAGE BREAKS                             
*                                                                               
         LA    R1,BMKTSTA+2             POINT TO REQUESTED STATION              
         BRAS  RE,GETSTA                                                        
         MVC   38(8,R4),BIGSTA          FORMATTED DESTINATION NAME              
         MVI   67(R4),C'D'              THE D FOR YI IF E                       
         MVI   70(R4),C'N'              SUPPRESS EDICT COVER PAGE               
*                                                                               
         LA    R4,54(R4)                ADD BILLING INFO                        
         MVC   0(1,R4),QMED                                                     
         MVC   1(3,R4),QCLT                                                     
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(14),DXTRAN                                                     
         LA    R1,P+15                                                          
         BAS   RE,DXFMTAPP                                                      
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   D.DOWIGMTH,C'E'     TEST EMAIL                                   
         BNE   PC10                                                             
         MVC   P(14),=C'++DDS      RCP'                                         
         MVC   P+15(40),D.DOWIGEML MOVE EMAIL ADDRESS                           
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,DXFMTORD                                                      
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EJECT                                                                  
PC10     MVI   SKIPSPEC,C'N'                                                    
         MVI   LINE,0                                                           
*                                                                               
         MVC   P1+33(10),=C'COVER PAGE'                                         
         MVC   P2+33(10),=C'----- ----'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,P1                                                            
         MVC   20(8,R4),=C'SENT TO:'                                            
         MVC   33(8,R4),BIGSTA                                                  
*                                                                               
         LA    R4,132(R4)                                                       
         CLI   D.DOWIGMTH,C'E'        TEST EMAIL                                
         BNE   PC12                                                             
         MVC   33(64,R4),D.DOWIGEML                                             
         OC    P,SPACES                                                         
         B     PC14                                                             
*                                                                               
PC12     MVC   33(12,R4),D.DOWIGFXN   FAX NUMBER                                
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   33(24,R4),D.DOWIGFXA   FAX ATTN NAME                             
         OC    0(132,R4),SPACES                                                 
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   33(12,R4),D.DOWIGFXT   TELEPHONE NUMBER                          
         OC    0(132,R4),SPACES                                                 
         DROP  D                                                                
*                                                                               
PC14     GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,0                                                           
         EJECT                                                                  
K        USING BYRKEY,KEY                                                       
*                                                                               
         L     R6,AIO3             POINT TO ORDER RECORD                        
         LA    R6,24(R6)           POINT TO ID ELEMENT                          
         USING DOIDELD,R6                                                       
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*=========================================================                      
* READ BUYER RECORD INTO AIO2                                                   
*=========================================================                      
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D31'                                                  
         MVC   K.BYRKAM,BAGYMD                                                  
         MVC   K.BYRKBYR,DOIDBYR                                                
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R7,AIO2                                                          
         ST    R7,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,24(R7)                                                        
         BRAS  RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BYRDSCD,R6                                                       
*                                                                               
         LA    R4,P1                                                            
         MVC   20(10,R4),=C'SENT FROM:'                                         
         MVC   33(24,R4),BYRFNAME                                               
*                                                                               
         BRAS  RE,GETUSRID                                                      
         MVC   60(4,R4),=C'(ID='                                                
         MVC   64(8,R4),DUB                                                     
*                                                                               
         LA    R1,74(R4)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   36(33,R4),USERNAME  INDENT AGENCY SLIGHTLY                       
         OC    0(132,R4),SPACES                                                 
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   36(33,R4),USERADDR                                               
         OC    0(132,R4),SPACES                                                 
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   20(10,R4),=C'TELEPHONE:'                                         
         MVC   33(12,R4),BYRPHONE   BUYER TELEPHONE                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R7,AIO3             POINT TO ORDER RECORD                        
         LA    R6,24(R7)                                                        
         MVI   ELCDLO,X'11'                                                     
         MVI   ELCDHI,X'11'                                                     
         BRAS  RE,NEXTEL           FIND TRANSMISSION ELEMENT                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DOXMTELD,R6                                                      
*                                                                               
         LA    R4,P1                                                            
         MVC   20(8,R4),=C'SENT ON:'                                            
         GOTO1 DATCON,DMCB,(8,DOXMTYMD),(8,33(R4))                              
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   20(8,R4),=C'SENT AT:'                                            
         GOTO1 HEXOUT,DMCB,DOXMTTIM,DUB,2,=C'TOG'                               
         MVC   33(2,R4),DUB                                                     
         MVI   35(R4),C'.'                                                      
         MVC   36(2,R4),DUB+2                                                   
         MVI   132(R4),0           FORCE A BLANK LINE AFTER                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,0                                                           
                                                                                
* SEE IF THERE IS PREVIOUS TRANSMISSION ELEMENT (NEXT ELEMENT)                  
                                                                                
         MVI   SHOWCHGS,C'N'       DEFAULT TO SHOWING CHANGES                   
         CLI   Q.RQ2CVRSN,0        TEST ORIGINAL                                
         BE    PC16                                                             
         MVI   SHOWCHGS,C'Y'                                                    
         XC    SVPRVDT,SVPRVDT                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   PC16                                                             
         MVC   SVPRVDT,DOXMTYMD    SAVE THE DATE (JULIAN PWOS)                  
         CLI   SDXSTYR,0           TEST ANY START DATE IN PROFILE               
         BE    PC16                NO                                           
         SR    R0,R0                                                            
         IC    R0,SDXSTYR                                                       
         AHI   R0,100                                                           
         STC   R0,DUB                                                           
         MVC   DUB+1(2),SDXSTMON                                                
         GOTO1 DATCON,DMCB,(3,DUB),(19,NEWFAXDT)  NEW FAX DATE PWOS             
         CLC   SVPRVDT,NEWFAXDT    TEST SENT BEFORE NEW FAX START               
         BH    PC16                NO                                           
         MVI   SHOWCHGS,C'N'       ELSE SUPPRESS CHANGES                        
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* NOW PRINT BUYER COMMENTS                                                      
*===========================================================                    
         SPACE 1                                                                
PC16     L     R7,AIO2             POINT TO BUYER RECORD                        
         MVI   ELCDLO,X'15'        FIND BYRCMTD                                 
         MVI   ELCDHI,X'15'        FIND BYRCMTD                                 
         LA    R6,24(R7)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PC30                                                             
*                                                                               
         USING BYRCMTD,R6                                                       
*                                                                               
         CLI   BYRSCMNT,C' '       TEST STANDARD COMMENT PRESENT                
         BNH   PC20                                                             
         LA    R1,BYRSCMNT                                                      
         BRAS  RE,DARSTD           READ STANDARD COMMENT                        
*                                                                               
         MVI   ELCDLO,X'10'        STANDARD COMMENT TEXT ELEM                   
         BRAS  RE,DARPRT           PRINT STANDARD COMMENT                       
*                                                                               
PC20     MVC   P(70),BYROCMT1                                                   
         CLC   P(70),SPACES        TEST FOR A BUYER COMMENT                     
         BNH   PC22                                                             
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PC22     MVC   P(70),BYROCMT2                                                   
         CLC   P(70),SPACES                                                     
         BNH   PC30                                                             
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
*                                                                               
PC30     OC    DARECON,DARECON     TEST CONTRACT PRESENT                        
         BZ    PCX                                                              
*                                                                               
         MVC   P(46),=C'*** ORDER PREVIOUSLY SET TO REP FIRM, CONTRACT'         
         MVC   P+48(8),DARECON                                                  
         MVC   P+57(3),=C'***'                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PCX      J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* READ STANDARD COMMENT RECORD                                                  
*================================================================               
                                                                                
DARSTD   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING COMRECD,R6                                                       
         MVI   COMKTYP,X'0D'                                                    
         MVI   COMKSUB,X'33'                                                    
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCOM,0(R1)                                                    
         OC    COMKCOM,=CL8' '     SHOULDN'T NEED THIS !                        
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PRINT A COMMENT RECORD. ELCDLO HAS TEXT ELEMENT ID                            
*===============================================================                
                                                                                
DARPRT   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1             READ COMMENT TO ADBUY+ 2000                  
         LA    R6,2000(R6)                                                      
         ST    R6,AIO                                                           
         MVI   ALLOWLIN,8          ALLOW 8 LINES WITHOUT A BREAK                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   ELCDHI,ELCDLO       COPY ELCODE                                  
         AHI   R6,24               POINT TO FIRST ELEMENT                       
         CLC   0(1,R6),ELCDLO                                                   
         BE    *+8                                                              
*                                                                               
DARPRT2  BRAS  RE,NEXTEL                                                        
         BNE   DARPRTX                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               ADJUST FOR OVERHEAD                          
         BM    DARPRT2                                                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),3(R6)                                                       
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     DARPRT2                                                          
*                                                                               
DARPRTX  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE AFTER COMMENT                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* FORMAT ++DDS   SUB                                                            
*        ++DDS   FIL                                                            
*=============================================================                  
                                                                                
DXFMTORD NTR1  BASE=*,LABEL=*                                                   
         MVC   P(14),=C'++DDS      SUB'                                         
         MVC   P+15(5),=C'ORDER'                                                
*                                                                               
         LA    RE,P+21                                                          
         MVC   0(3,RE),QCLT                                                     
         LA    RE,3(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVC   2(3,RE),QPRD                                                     
         LA    RE,5(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(3,RE),DUB                                                      
*                                                                               
         LA    RE,6(RE)                                                         
         ST    RE,DMCB+4           SET OUTPUT DATE POSN                         
         MVI   DMCB+4,11           SET TYPE=MMMDD/YY                            
         GOTO1 DATCON,DMCB,Q.RQSTART                                            
         L     RE,DMCB+4           POINT TO THE OUTPUT                          
         ICM   RF,3,6(RE)          GET THE YY                                   
         MVC   5(2,RE),=C'20'      OVERWRITE THE /                              
         STCM  RF,3,7(RE)                                                       
*                                                                               
         MVC   P2(14),=C'++DDS      FIL'                                        
*                                                                               
         MVC   P2+15(33),USERNAME                                               
         LA    RE,P2+15                                                         
         CLC   USERNAME,SPACES                                                  
         BE    DXFMT02                                                          
*                                                                               
         AHI   RE,33                                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
DXFMT02  MVI   1(RE),C'-'                                                       
         MVC   2(3,RE),Q.RQUESTOR                                               
*                                                                               
         MVI   5(RE),C'{'                                                       
         MVC   6(30,RE),P+21       PRINT CLT/PRD/EST/DATE AGAIN !               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* FORMAT APPL PART OF ++DDS REC RECORD                                          
*=============================================================                  
                                                                                
DXFMTAPP NTR1  BASE=*,LABEL=*      FORMAT APPLICATION PART OF ++DDS REC         
         USING SPEDICTD,R1                                                      
         MVI   SPCPTYPE,SPCPDATQ                                                
         MVC   SPCPMED,QMED                                                     
         MVC   SPCPCLT,QCLT                                                     
         MVC   SPCPPRD,QPRD                                                     
         MVC   SPCPEST,QEST                                                     
         MVC   SPCPMKT,Q.RQMKT                                                  
         MVC   SPCPSTA,Q.RQSTA                                                  
         MVC   SPCPRQST,Q.RQUESTOR                                              
         MVC   SPCDRORD,Q.RQ2DRORD                                              
         LA    R1,SPCDRUSR                                                      
         USING RTN2SNDR,R1                                                      
         MVC   RTNSYSID,Q.RQ2DRSID                                              
         MVC   RTNPWRCD,Q.RQAGY                                                 
         MVC   RTNAGYMD,Q.RQ2AGYMD                                              
         J     EXIT                                                             
         DROP  R1                                                               
*                                                                               
DXTRAN   DC    CL14'++DDS SPXDXTRN'                                             
DXEMAIL  DC    C'N'                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* HEADHOOK ROUTINE                                                              
*==================================================================             
                                                                                
GETHDHK  BASR  R1,RE               RETURN TO RE WITH A(HDHK) IN R1              
HDHK     NTR1  BASE=*,LABEL=*                                                   
         CLI   HEADHOOK,C'R'       TEST THIS IS A RETURN CALL                   
         BNE   HDHK2                                                            
         MVI   HEADHOOK,0          MAKE SURE TO RESET                           
         LHI   R0,14               BLANK HEADLINES                              
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
         BAS   RE,MYHDZEN                                                       
         J     EXIT                                                             
*                                                                               
HDHK2    MVC   H1(10),MEDNM                                                     
         OC    H1(10),SPACES                                                    
         MVC   H4+9(3),QCLT                                                     
         MVC   H4+13(20),CLTNM                                                  
         MVC   H5+9(3),QPRD                                                     
         MVC   H5+13(20),PRDNM                                                  
         LA    R4,H6                                                            
         CLC   Q.RQPTNR,SPACES                                                  
         BNH   HDHK10                                                           
         MVC   H6(8),=C'PARTNER '                                               
         MVC   H6+9(3),Q.RQPTNR                                                 
         MVC   H6+13(20),PRD2NM                                                 
*                                                                               
         LA    R4,H7                                                            
HDHK10   MVC   0(8,R4),=C'ESTIMATE'                                             
         MVC   9(3,R4),QEST                                                     
         MVC   13(20,R4),ESTNM                                                  
         AHI   R4,2*132            SKIP TWO LINES                               
         BAS   RE,PRTVRSN                                                       
*                                                                               
         GOTO1 DATCON,DMCB,SVQSTART,(8,H3+61)                                   
         GOTO1 (RF),(R1),SVQEND,(8,H3+73)                                       
*                                                                               
         CLC   =C'ES',Q.RQSTAUTO                                                
         BNE   *+8                                                              
         MVI   H3+81,C'*'          INDICATE ES DATES                            
*                                                                               
         MVC   H3+94(10),=C'REPORT DFX'                                         
*                                                                               
         MVC   H5+48(6),=C'MARKET'                                              
         MVC   H5+55(4),Q.RQMKT                                                 
         MVC   H5+60(24),MKTNM                                                  
         OC    H5+48(36),SPACES                                                 
         GOTO1 CENTER,DMCB,H5+48,36                                             
*                                                                               
         MVC   H7+48(7),=C'STATION'                                             
         LA    R1,TSARSTA          POINT TO TSAR STATION                        
         BRAS  RE,GETSTA                                                        
*                                                                               
         MVC   H7+56(8),BIGSTA                                                  
         MVC   H7+65(3),SVAFFL                                                  
         OC    H7+48(36),SPACES                                                 
         GOTO1 CENTER,DMCB,H7+48,36                                             
*                                                                               
         CLC   Q.RQAGY,=C'TH'      TEST ZENITH                                  
         BNE   *+8                                                              
         MVI   HEADHOOK,C'R'       REQUEST RETURN CALL                          
*                                                                               
HDHKX    XIT1                                                                   
*                                                                               
PRTVRSN  NTR1                                                                   
         MVC   0(37,R4),=CL37'** DARE ORDER 12345678 - ORIGINAL **'             
         MVC   14(8,R4),Q.RQ2DRORD                                              
         CLI   Q.RQ2CVRSN,0        TEST ORIGINAL                                
         BNE   PRTVRS2                                                          
         CLI   Q.RQ2RSND,C'Y'      TEST RESEND                                  
         BNE   PRTVRS10            NO                                           
         MVC   34(12,R4),=CL12'- RESEND **'                                     
         B     PRTVRS10                                                         
*                                                                               
PRTVRS2  MVC   25(14,R4),SPACES                                                 
         MVC   25(10,R4),=C'REV 001 **'                                         
         SR    R0,R0                                                            
         IC    R0,Q.RQ2CVRSN                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  29(3,R4),DUB                                                     
         CLI   Q.RQ2RSND,C'Y'     TEST RESEND                                   
         BNE   PRTVRS4            NO                                            
         MVC   33(12,R4),=CL12'- RESEND **'                                     
         B     PRTVRS10           DON'T PRINT REPLACES - DATE IS LOST           
*                                                                               
PRTVRS4  AHI   R4,132              ADVANCE TO NEXT LINE                         
         MVC   3(20,R4),=C'REPLACES REV 000  OF'                                
         SR    R0,R0                                                            
         IC    R0,Q.RQ2PVRSN                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  16(3,R4),DUB                                                     
         CLI   Q.RQ2PVRSN,0        TEST REPLACES ORIGINAL                       
         BNE   *+10                                                             
         MVC   12(8,R4),=C'ORIGINAL'                                            
         GOTO1 DATCON,DMCB,(2,Q.RQ2PVRSD),(5,24(R4))                            
*                                                                               
PRTVRS10 CLI   DRCSHTRD,0          TEST CASH OR TRADE ORDER                     
         BE    PRTVRSX                                                          
         AHI   R4,132                                                           
         MVC   0(30,R4),=CL30'*****  TRADE SPOTS ONLY  *****'                   
         CLI   DRCSHTRD,C'R'                                                    
         BE    PRTVRSX                                                          
         MVC   0(30,R4),=CL30'*****  CASH  SPOTS ONLY  *****'                   
*                                                                               
PRTVRSX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SPECIAL HEADLINES FOR ZENITH                                                  
*================================================================               
                                                                                
MYHDZEN  NTR1                                                                   
*                                                                               
         MVI   H1,C'*'                                                          
         MVC   H1+1(91),H1                                                      
                                                                                
         LA    R0,5                                                             
         LA    R1,H2                                                            
                                                                                
         MVI   0(R1),C'*'                                                       
         MVI   91(R1),C'*'                                                      
         AHI   R1,132                                                           
         BCT   R0,*-12                                                          
         MVC   0(92,R1),H1         COPY STARS FROM H1                           
*                                                                               
         MVC   H2+2(19),=C'STATION INVOICE TO:'                                 
*                                                                               
         MVC   H3+1(90),SPACES                                                  
         MVC   H4+1(90),SPACES                                                  
         MVC   H5+1(90),SPACES                                                  
         CLC   SVACCAGY,=C'DF'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCAGY,=C'DW'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCOFC,=C'3A'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCOFC,=C'3C'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCOFC,=C'3D'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCOFC,=C'3K'                                                  
         BE    MYHDZ10                                                          
         CLC   SVACCOFC,=C'3P'                                                  
         BNE   MYHDZ20                                                          
MYHDZ10  MVC   H3+2(18),=C'SAATCHI && SAATCHI,'                                 
         MVC   H3+21(42),=C'79 MADISON AVENUE, NEW YORK, NY 10016-7802'         
         MVC   H4+2(47),SSAGENT                                                 
         B     MYHDZ70                                                          
*                                                                               
MYHDZ20  CLC   SVACCAGY,=C'TH'                                                  
         BNE   MYHDZ30                                                          
         MVC   H3+2(28),=C'ZENITH MEDIA SERVICES, INC.,'                        
         MVC   H3+31(40),=C'79 MADISON AVE., NEW YORK, NY 10016-7802'           
         MVC   H3+73(12),=C'646-935-4700'                                       
         MVC   H4+2(19),=C'AS AGENT FOR CLIENT'                                 
         MVC   H6+2(24),=C'THIS ORDER IS MADE UNDER'                            
         MVC   H6+27(28),=C'AND IS SUBJECT TO AAAA TERMS'                       
         MVC   H6+56(34),=C'AND CONDITIONS FOR LOCAL BROADCAST'                 
         B     MYHDZ70                                                          
*                                                                               
MYHDZ30  CLC   SVACCAGY,=C'BS'                                                  
         BNE   MYHDZ60                                                          
         MVC   H3+1(70),SPACES                                                  
         MVC   H3+2(11),=C'BATES USA, '                                         
         MVC   H3+14(30),=C'498 SEVENTH AVENUE,NY NY 10018'                     
         B     MYHDZ70                                                          
*                                                                               
MYHDZ60  MVC   H3+2(31),USERNAME                                                
         MVI   H3+33,C','                                                       
         MVC   H3+34(31),USERADDR                                               
*                                                                               
MYHDZ70  MVC   H5+2(37),=C'--- MUST INCLUDE REFERENCE NUMBER ---'               
*                                                                               
MYHDZ90  DS    0H                                                               
         XIT1                                                                   
*                                                                               
SSAGENT  DC    C'AGENT FOR SAATCHI AND SAATCHI, AGENT FOR CLIENT'               
*                                                                               
         DS    0D                                                               
GETHDNG  BASR  R1,RE               RETURN TO RE WITH A(HEADING) IN R1           
HEADING  DS    0H                                                               
**NOP**  SSPEC H1,1,C'SPOT TV'                                                  
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,54,C'CONFIRMATION OF PURCHASE'                                
         SSPEC H2,54,C'------------------------'                                
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,50,C'PERIOD FROM JAN01/01 TO DEC31/01'                        
**NOP**  SSPEC H3,95,REPORT                                                     
         SSPEC H3,108,RUN                                                       
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,95,PAGE                                                       
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H7,95,C'------------------------------------'                    
         SSPEC H8,95,C'SALESPERSON''S SIGNATURE             '                   
         DC    X'00'                                                            
         EJECT                                                                  
*==================================================================             
* MIDHOOK ROUTINE                                                               
*==================================================================             
                                                                                
GETMIDHK BASR  R1,RE                                                            
*                                                                               
MIDHK    NTR1  BASE=*,LABEL=*                                                   
         CLI   MIDHOOK,C'R'        TEST THIS IS A RETURN CALL                   
         BNE   MIDHK2              PRINT THIRD MIDLINE                          
         MVI   MIDHOOK,0           INSURE NO RETURN                             
         MVC   MID1,SVMID3                                                      
         MVC   MID2,SPACES                                                      
         B     MIDHKX                                                           
*                                                                               
MIDHK2   CLC   PASS,MIDPASS        TEST NEED TO CHANGE DATES                    
         BE    MIDHK10             NO                                           
*                                                                               
         MVC   SVMID2(L'MYMID2),MYMID2                                          
         MVC   SVMID3(30),=C'      PROGRAMMING          LEN'                    
         MVC   SVMID3+124(7),=C'CPP/CPM'                                        
*                                                                               
         MVC   SVMID2+35(56),SPACES  BLANK OUT THE DATES                        
         MVC   SVMID3+35(56),SPACES                                             
*                                                                               
         MVC   MIDPASS,PASS          SAVE CURRENT PASS NUMBER                   
         SR    R4,R4                                                            
         IC    R4,PASS                                                          
         BCTR  R4,0                                                             
         MHI   R4,12               12 BYTES/ENTRY                               
         LA    R4,PASSTAB(R4)                                                   
         MVC   WORK(12),0(R4)      SET CURRENT PERIOD DATES YYMMDD              
*                                                                               
         LA    R4,SVMID2+35                                                     
         LHI   R5,14               MAX WEEK/DAY ENTRIES                         
*                                                                               
MIDHK4   GOTO1 DATCON,DMCB,WORK,(4,DUB)   GET MMMDD                             
         MVC   0(3,R4),DUB         MOVE MMM                                     
         MVC   133(2,R4),DUB+3     MOVE DD                                      
*                                                                               
         LHI   R0,7                                                             
         CLI   SVEDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LHI   R0,1                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R0)                                     
         MVC   WORK(6),WORK+12                                                  
         CLC   WORK(6),WORK+6                                                   
         BH    MIDHK10                                                          
         AHI   R4,4                                                             
         BCT   R5,MIDHK4                                                        
         DC    H'0'                                                             
*                                                                               
MIDHK10  MVC   MID1+35(L'MYMID1),MYMID1                                         
         MVC   MID2,SVMID2                                                      
         MVI   MIDHOOK,C'R'        REQUEST RETURN                               
*                                                                               
MIDHKX   J     EXIT                                                             
*                                                                               
MYMID1   DC    C'----------------- NUMBER OF TELECASTS ----------------X        
               --'                                                              
*                                                                               
MYMID2   DC    C'BUYLINE DAYS       TIME            JAN JAN JAN JAN JANX        
                FEB FEB FEB FEB MAR MAR MAR MAR MAR     TOT     COST  -X        
               --- DEMOGRAPHICS ----'                                           
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PRINT ORBIT DESCRIPTIONS                                                      
*===============================================================                
                                                                                
PRTORB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    VDAYUNPK,VDAYUNPK                                                
         BNZ   PRTORB2                                                          
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'    GET DAYUNPK ADDRESS                 
         MVC   VDAYUNPK,0(R1)                                                   
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'    GET UNTIME ADDRESS                  
         MVC   VUNTIME,0(R1)                                                    
*                                                                               
PRTORB2  MVC   SKGRID,SPACES                                                    
         MVC   SKORB(11),=C'** ORBIT **'                                        
*                                                                               
         LA    R3,ORBTAB                 POINT TO PRINT POSN TABLE              
         LA    R5,TSORBS                                                        
         LHI   R6,8                      MAX ORBITS TO PRINT                    
*                                                                               
PRTORB4  LH    R4,0(R3)                                                         
         LA    R4,SKORB(R4)                                                     
*                                                                               
         GOTO1 VDAYUNPK,DMCB,0(R5),(R4)                                         
         BAS   RE,ADDCOMMA                                                      
*                                                                               
         GOTO1 VUNTIME,DMCB,1(R5),(R4)                                          
         BAS   RE,ADDCOMMA                                                      
*                                                                               
         MVC   0(7,R4),5(R5)                PROGRAM                             
         BAS   RE,ADDCOMMA                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,12(R5)                                                      
         N     R0,=X'FFFFBFFF'                                                  
         TM    12(R5),X'40'        IS IT 2 PRECISION                            
         BO    PRTORB6                                                          
         EDIT  (R0),(5,(R4)),1,ALIGN=LEFT   DEMO                                
         B     PRTORB9                                                          
PRTORB6  EDIT  (R0),(5,(R4)),2,ALIGN=LEFT   DEMO                                
*                                                                               
PRTORB9  AHI   R3,2                                                             
         AHI   R5,16                                                            
         CLI   0(R5),0             TEST MORE DATA                               
         BE    *+8                 NO                                           
         BCT   R6,PRTORB4                                                       
         J     EXIT                                                             
*                                                                               
ADDCOMMA AHI   R4,8                POINT TO END (MAX LEN 8)                     
*                                                                               
ADDCOMM2 OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,ADDCOMM2                                                      
         MVI   1(R4),C','                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
*                                                                               
ORBTAB   DC    AL2(16,43,70,148,175,202,280,307,334) PRINT DSPLS                
         LTORG                                                                  
*===============================================================                
* READ DARE ORDER RECORD VIA PASSIVE KEY INTO AIO3                              
*===============================================================                
                                                                                
GETDRORD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    DARECON,DARECON                                                  
         XC    DRTRDDTA,DRTRDDTA                                                
         MVI   DRCSHTRD,0                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,WORD)   GET CURRENT DATE                   
         ZAP   FULL,WORD           CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,Q.RQ2DRORD,DUB,8 SAVE AS IF ENTRY WAS HEX             
         MVC   WORD,DUB            CONVERT IT TO PACK                           
         OI    WORD+3,X'0F'                                                     
         SRP   WORD,58,0           ISOLATE THE YEAR                             
         MVC   WORD+2(1),FULL+2    COPY TODAY'S CENTURY                         
*                                                                               
         CP    WORD+3(1),FULL+3(1) LESS 10 YEARS?                               
         BNH   *+10                                                             
         SP    WORD,=P'10'         YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    WORD,=P'90'         CALCULATE FROM 1990                          
         SRP   WORD,4,0                                                         
         OC    WORD+1(2),DUB       STICK IN DAYS IN YEAR                        
         SRP   WORD,63,0                                                        
*                                                                               
         ZAP   DUB,WORD            SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,Q.RQ2DRORD+4(4) SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
*********                                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER   ELIMINATES PROBLEMS WITH FLT & TRADE         
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   GETDRORX            EXIT WITH CC NEQ                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO ORDER RECORD                        
         AHI   R6,24                                                            
         USING DOIDELD,R6                                                       
*                                                                               
         MVC   DARECON,DOIDCON     SAVE CONTRACT NUMBER                         
*                                                                               
         XC    LASTSENT,LASTSENT                                                
         L     R6,AIO              POINT TO ORDER RECORD                        
         AHI   R6,24                                                            
         MVI   ELCDLO,X'11'        FIND TRANSMISSION ELEMENT                    
         MVI   ELCDHI,X'11'                                                     
*                                                                               
         USING DOXMTELD,R6                                                      
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   LASTSENT,DOXMTYMD   SAVE DATE LAST SENT                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              POINT TO ORDER RECORD                        
         AHI   R6,24                                                            
         MVI   ELCDLO,3            FIND DOSPELQ                                 
         MVI   ELCDHI,3                                                         
         USING DOSPELD,R6                                                       
         BRAS  RE,NEXTEL                                                        
         BNE   GETDROR5                                                         
*                                                                               
         CLI   DOSPTMTH,C'R'       UPPERCASE R                                  
         BE    *+12                                                             
         CLI   DOSPTMTH,C'R'-X'40' LOWERCASE R                                  
         BNE   GETDROR5                                                         
*                                                                               
         MVC   DRCSHTRD,DOSPTMTH   SAVE TRADE METHOD                            
         GOTO1 RCPACK,DMCB,(C'P',DOSPTDAT),DRTRDDTA  SAVE PACKED REP            
*                                                                               
GETDROR5 MVI   ELCDLO,X'50'        FIND DOWIGEL                                 
         MVI   ELCDHI,X'50'                                                     
         L     R6,AIO              POINT TO ORDER RECORD                        
         AHI   R6,24                                                            
         BRAS  RE,NEXTEL                                                        
         BNE   GETDRORX                                                         
         MVC   SVWIGEL,0(R6)                                                    
*                                                                               
GETDRORX XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* READ USERID RECORD AND RETURN ALPHA VALUE IN DUB                              
*=============================================================                  
                                                                                
GETUSRID NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),10(RA)    ID NUMBER IN TWA+10                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO1                      
*                                                                               
         L     R7,AIO1                                                          
         AHI   R7,28                                                            
*                                                                               
GETUSR2  CLI   0(R7),2                                                          
         BE    GETUSR4                                                          
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   GETUSR2                                                          
         DC    H'0'                                                             
*                                                                               
GETUSR4  MVC   DUB,2(R7)                                                        
         J     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=======================================================                        
* SET REPORT OPTIONS FROM DX PROFILE                                            
* NOTE THAT PROGPROF IS THE D2 PROFILE !                                        
*=======================================================                        
                                                                                
SETOPTS  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(12),=CL12'S0D2'                                             
         MVC   WORK+4(3),Q.RQAGY                                                
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
         MVC   WORK+24(12),WORK    SAVE PROFILE VALUES                          
         GOTO1 GETPROF,DMCB,(X'C0',WORK),PROGPROF,DATAMGR                       
*                                                                               
         MVC   WORK(16),WORK+24    RESTORE                                      
         MVC   WORK(4),=C'S0DX'                                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),SDXPROF,DATAMGR                        
*                                                                               
         CLC   SDXPROF+7(3),=X'040101' MAKE SURE THEY ENTERED A DATE            
         BH    SETOPT1                                                          
         MVC   GERROR(2),=Y(NODXSTDT)                                           
         MVI   GETMSYS,2           SET SYSTEM 2 ERROR                           
         GOTO1 MYERR                                                            
*                                                                               
SETOPT1  LA    R1,SDXPROF          GET DEFAULTS SET CORRECTLY !                 
         LA    RE,SDXDFLTS                                                      
         LHI   RF,SDXDFLTX-SDXDFLTS                                             
*                                                                               
SETOPT2  CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   0(1,R1),0(RE)                                                    
         AHI   R1,1                                                             
         AHI   RE,1                                                             
         BCT   RF,SETOPT2                                                       
         B     SETOPT4                                                          
SDXDFLTS DC    X'00'     +0        NUMBER OF DEMOS                              
         DC    C'N'      +1        NEW PAGE FOR CABLE NETWORK                   
         DC    C'N'      +2        NEW ORDER MANAGER FAXING                     
         DC    C'Y'      +3        FLAG DEMO OVERRIDES                          
         DC    C'Y'      +4        PRINT CPP                                    
         DC    C'Y'      +5        PRINT COSTS                                  
         DC    C'N'      +6        PUT COPY OF FAX TO PRTQUE                    
         DC    X'040101' +7/8/9    NEW FAX START DATE                           
SDXDFLTX EQU   *                                                                
*                                                                               
         DS    0H                                                               
SETOPT4  XC    WORK(16),WORK       RESTORE                                      
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),Q.RQAGY   AGENCY ONLY                                  
         GOTO1 GETPROF,DMCB,(X'C0',WORK),S00PROF,DATAMGR                        
*                                                                               
         MVC   WORK(16),WORK+24    RESTORE                                      
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'BF'          MAKE S LOWERCASE FOR 3 CHAR PROF             
         GOTO1 GETPROF,DMCB,(X'C0',WORK),SDARPROF,DATAMGR                       
                                                                                
* GET EQUIVALENCE HEADER                                                        
                                                                                
         MVC   DUB(2),Q.RQAGY      AGENCY CODE                                  
         MVC   DUB+2(1),Q.RQMED    MEDIA                                        
         MVC   DUB+3(2),BCLT       CLIENT                                       
         L     R0,DATAMGR                                                       
*                                                                               
         GOTO1 =V(EQVRD),DMCB,DUB,EQUDPT,EQUSECT1,(R0),RR=RELO                  
         CLI   DMCB+12,0           ERROR ?                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* GET STATION CALL LETTERS IN PRINT FORMAT IN BIGSTA                            
* ON ENTRY R1 POINTS TO INPUT VALUE                                             
*=============================================================                  
                                                                                
GETSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
S        USING STAPACKD,WORK                                                    
*                                                                               
         MVI   S.STAPACT,C'U'                                                   
         MVC   S.STAPAGY,AGENCY                                                 
         MVC   S.STAPCTRY,SVAPROF+7                                             
         MVC   S.STAPMED,QMED                                                   
         MVC   S.STAPACOM,ACOMFACS                                              
         MVC   S.STAPSTA,0(R1)                                                  
         OC    0(3,R1),0(R1)       STATION SHOULD NEVER BE 00                   
         BNZ   GETSTA2                                                          
*                                                                               
         MVC   S.STAPQSTA,QSTA     JUST USE REQUESTED STATION                   
         B     GETSTA2X                                                         
*                                                                               
GETSTA2  GOTO1 VSTAPACK,WORK                                                    
*                                                                               
GETSTA2X MVC   BIGSTA(4),S.STAPQSTA                                             
         LA    RE,BIGSTA+3                                                      
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         AHI   RE,1                POINT AFTER STATION                          
         MVI   0(RE),C'-'                                                       
*                                                                               
         CLI   S.STAPQSTA,C'0'     TEST CABLE                                   
         BL    GETSTA4                                                          
         MVI   0(RE),C'/'                                                       
         MVC   1(3,RE),S.STAPQNET  MOVE NETWORK                                 
         B     GETSTAX                                                          
*                                                                               
GETSTA4  MVI   1(RE),C'L'                                                       
         CLI   S.STAPQSTA+4,C'L'                                                
         BE    GETSTAX                                                          
*                                                                               
         MVC   1(2,RE),=C'TV'                                                   
         CLI   QMED,C'T'                                                        
         BE    GETSTAX                                                          
*                                                                               
         MVC   1(2,RE),=C'AM'                                                   
         CLI   S.STAPQSTA+4,C'A'                                                
         BE    GETSTAX                                                          
*                                                                               
         MVC   1(2,RE),=C'FM'                                                   
         CLI   S.STAPQSTA+4,C'F'                                                
         BE    GETSTAX                                                          
*                                                                               
         MVC   1(1,RE),QMED                                                     
         MVI   2(RE),C' '                                                       
*                                                                               
GETSTAX  J     EXIT                                                             
         DROP S                                                                 
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SUBROUTINE TO EQUIVALENCE DEMOS IN TSDEM1...4                                 
* REMEMBER TSDEMS ARE 8 BYTE DEMOS WITH HDR PRECEDING                           
*===============================================================                
                                                                                
EQUIV    NTR1  BASE=*,LABEL=*                                                   
         XC    EQUDEMS,EQUDEMS                                                  
         OC    NUMDEMS,NUMDEMS                                                  
         JZ    EXIT                                                             
* GET EQUIVALENCY FACTOR                                                        
         LHI   R0,C'R'                                                          
         CLI   Q.RQMED,C'R'                                                     
         BE    EQV2                                                             
         CLI   Q.RQMED,C'X'                                                     
         BE    EQV2                                                             
         LHI   R0,C'T'                                                          
         CLI   Q.RQMED,C'T'                                                     
         BE    EQV2                                                             
         CLI   Q.RQMED,C'N'                                                     
         BE    EQV2                                                             
         CLI   Q.RQMED,C'C'                                                     
         BE    EQV2                                                             
         DC    H'0'                                                             
*                                                                               
EQV2     GOTO1 CALLOV,DMCB,0,X'D9000A57'  GET SLNTAB ADDRESS                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,0(R1)            START OF PHASE                               
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            DSPL TO EOT                                  
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
EQV4     CLC   0(2,R1),=C'00'      TEST DEFAULT ENTRY                           
         BE    EQV6                                                             
         CLC   0(2,R1),Q.RQAGY     MATCH AGY CODE                               
         BNE   *+12                                                             
EQV6     CLM   R0,1,2(R1)          MATCH MEDIA CODE                             
         BE    EQV8                                                             
         BXLE  R1,RE,EQV4                                                       
         DC    H'0'                                                             
*                                                                               
EQV8     SR    RE,RE                                                            
         IC    RE,TSBDSEC                                                       
         AR    RE,RE               X 2                                          
         LA    RE,4(R1,RE)         POINT TO ENTRY FOR THIS SLN                  
         SR    R3,R3                                                            
         IC    R3,0(RE)            GET DSPL TO FACTOR                           
         LA    R3,EQUSECT1(R3)     POINT TO ENTRY FOR THIS SLN                  
                                                                                
* CALCULATE EQUIVALENCIES                                                       
                                                                                
         LH    R0,NUMDEMS          BCT LOOP                                     
         LA    R4,TSDEMS           INPUT                                        
         LA    R5,EQUDEMS          OUTPUT                                       
*                                                                               
EQV40    L     RF,4(R4)            INPUT                                        
         N     RF,=X'3FFFFFFF'     DROP FLAGS                                   
         AR    RF,RF               X 2                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)          FACTOR                                       
         BNZ   *+8                                                              
         LHI   RE,1000             USE 1000 IF FACTOR IS 0                      
*                                                                               
         MR    RE,RE                                                            
         D     RE,=F'1000'                                                      
         AHI   RF,1                ROUND                                        
         SRL   RF,1                                                             
*                                                                               
         TM    4(R4),X'40'         TEST INPUT TO 2-DEC                          
         BZ    *+8                                                              
         O     RF,=X'40000000'                                                  
*                                                                               
         ST    RF,0(R5)            EQUIVALENCED DATA                            
*                                                                               
         AHI   R4,8                INPUT                                        
         AHI   R5,4                OUTPUT                                       
         BCT   R0,EQV40                                                         
         J     EXIT                                                             
         EJECT                                                                  
PQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SPOOLQLK,SPOOLQLK                                                
         MVC   SPOOLID,=C'DFX'       SETUP PRINT QUEUE INFO                     
         CLI   COPYTYPE,C'X'         TEST THIS IS FAX COPY                      
         BE    *+10                                                             
         MVC   SPOOLID,Q.RQUESTOR    COPY GOES TO BUYER                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
*                                                                               
         MVC   PLDESC(3),=C'DFX'                                                
         MVC   PLDESC+3(8),Q.RQ2DRORD                                           
*                                                                               
         OI    SPOOLIND,SPUINIT      ALLOWS ME TO SET THE CLASS                 
         MVI   PLCLASS,C'G'          DARE CLASS G                               
         CLI   COPYTYPE,C'X'         TEST THIS IS FAX COPY                      
         BE    PQOP2                                                            
         MVI   PLCLASS,C'H'          THEN GO TO CLASS H                         
*                                                                               
PQOP2    MVI   USERLANG,0                                                       
         MVC   PLSUBID,SPOOLID                                                  
         MVC   PLUSER,10(RA)       TWAORIG                                      
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         MVC   SPOOLBUF,ATIA                                                    
         MVI   SPMODE,0                                                         
         XC    VPRINT,VPRINT         NOT OFF-LINE                               
         XC    ABOX,ABOX             NO BOXES                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SPOOLRPN,PLREPNO                                                 
         J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
PQCLOSE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
*                                                                               
         CLC   PLSUBID,=C'DFX'     IF DFX REPORT AND                            
         BNE   PQCLO10                                                          
         CLI   PLCLASS,C'H'        CLASS H                                      
         BNE   PQCLO10                                                          
         DC    H'0'                DIE, CUZ THIS IS BAD!!                       
*                                                                               
PQCLO10  MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*=======================================================                        
* PRINT RECORDS IN TSAR BUFFER                                                  
*=======================================================                        
                                                                                
PRINTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=H'1'                                                   
*                                                                               
PRINTSA2 BRAS  RE,CALLTSAR                                                      
         JNE   EXIT                                                             
         L     RF,ACOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,TSARREC,P,6                                            
         GOTO1 (RF),DMCB,TSARREC+6,P2,64                                        
         GOTO1 (RF),DMCB,TSARREC+70,P3,64                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   T.TSACTN,TSANXT                                                  
         B     PRINTSA2                                                         
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PROGRAM INITIALIZATION CODE                                                   
*===============================================================                
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'PQONLY',CONACT                                                
         BNE   *+8                                                              
         MVI   PQONLY,C'Y'                                                      
*                                                                               
         L     RE,AIO              SKIP SPOOK(80) + REQHDR (26)                 
         MVC   REQ,106(RE)         MOVE REQUEST CARDS TO MY STORAGE             
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5D'  GET TSAR ADDRESS                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
         MVC   QDMTRACE,=CL8'DMTRACE'  AVOID LITERALS IN CALLTSAR               
         MVC   QDMDATA,=CL8'DATA'                                               
*                                                                               
         MVI   T.TSACTN,TSAINI                                                  
         MVC   T.TSACOM,ACOMFACS                                                
         MVI   T.TSINDS,TSINODSK     CORE ONLY !                                
         MVI   T.TSKEYL,L'TSARKEY                                               
         LHI   R1,TSARRECX-TSARREC   RECORD LENGTH                              
         STH   R1,T.TSRECL                                                      
         MVI   T.TSPAGN,20           <<<< SET FOR BOTH BUFFERS                  
         LA    R1,TSARREC                                                       
         ST    R1,T.TSAREC                                                      
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XITSTADV    ON A TEST SYSTEM                             
         JO    EXIT                                                             
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF  TEST ABLE TO UPDATE           
         JZ    EXIT                                                             
*                                                                               
         MVC   GERROR(2),=Y(1260)  SET NO WRITE ERROR                           
         MVI   GETMSYS,2           SET SYSTEM 2 ERROR                           
         GOTO1 MYERR                                                            
         LTORG                                                                  
         EJECT                                                                  
MYWORKD  DSECT                                                                  
REQ      DS    CL160                                                            
QDMTRACE DS    CL8                                                              
QDMDATA  DS    CL8                                                              
DARECON  DS    CL8                                                              
DRTRDDTA DS    XL8                                                              
SDXPROF  DS    CL16                                                             
SDXDEMOV EQU   SDXPROF+3           FLAG DEMO OVERRIDES                          
SDXCPP   EQU   SDXPROF+4           PRINT CPP                                    
SDXCOSTS EQU   SDXPROF+5           PRINT COSTS                                  
SDXTOPQ  EQU   SDXPROF+6           PUT A COPY OF FAX TO PRTQUE                  
SDXSTYR  EQU   SDXPROF+7           NEW FAX START YEAR (200X)                    
SDXSTMON EQU   SDXPROF+8           NEW FAX START MONTH                          
SDXSTDAY EQU   SDXPROF+9           NEW FAX START DAY                            
SDARPROF DS    CL16                                                             
S00PROF  DS    CL16                                                             
PASSACT  DS    C                                                                
ANYHIST  DS    C                                                                
ANYSPOTS DS    C                                                                
ANYCOMS  DS    C                                                                
ANYCHANG DS    C                                                                
ANYBUYS  DS    C                                                                
TOOMANY  DS    C                   MORE THAN 100 SPOTS IN A WEEK                
DRCSHTRD DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
LASTSENT DS    XL3                 PWOS JULIAN                                  
SVPOL    DS    C                                                                
SHOWCHGS DS    C                                                                
COPYTYPE DS    X                   CURRENT OUTPUT TYPE Q=PQ,X=FAX               
PQONLY   DS    C                   C'Y' IF ACTION=PQONLY                        
*                                                                               
RELO     DS    A                                                                
VDAYUNPK DS    A                                                                
VUNTIME  DS    A                                                                
VTSAR    DS    A                                                                
SPOM29RB DS    A                                                                
WORD     DS    F                                                                
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
BINFLTNM DS    XL1                 BINARY FLIGHT NUMBER                         
         DS    0D                                                               
*                                                                               
SVXSPKEY DS    XL32                                                             
*                                                                               
         DS    0D                                                               
SVWIGEL  DS    CL80                                                             
*                                                                               
NUMDEMS  DS    H                   NUMBER OF DEMOS TO PRINT                     
SVDEMNMS DS    4CL6                                                             
*                                                                               
         DS    0D                                                               
PASSTAB  DS    XL96                                                             
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
SVQSTP   DS    XL2                                                              
SVQENDP  DS    XL2                                                              
SVQSTB   DS    XL3                                                              
SVQENDB  DS    XL3                                                              
TODAYB   DS    XL3                                                              
SVPRVDT  DS    XL3                                                              
NEWFAXDT DS    XL3                                                              
BPERST   DS    XL2                                                              
BPEREND  DS    XL2                                                              
SVOOWR   DS    XL1                                                              
EXCLTAX  DS    CL1                                                              
SVTSRNUM DS    H                                                                
*                                                                               
WEEKDSPL DS    H                   DISP TO FIRST WEEK ENTRY THIS PERIOD         
SPTDSPL  DS    H                   DISP TO FIRST SPOT ENTRY THIS PERIOD         
DOLDSPL  DS    H                   DISP TO FIRST DOL ENTRY THHIS PERIOD         
NUMWEEKS DS    H                   NUMBER OF WEEKS IN CURRENT PERIOD            
         DS    0D                                                               
WEEKTAB  DS    XL224               4 BYTES/WEEK + EOF                           
         DS    0D                                                               
WEEKSPTS DS    XL28                14X2 BYTE ACCUMS                             
WEEKDOLS DS    XL56                14X4 BYTE ACCUMS                             
         DS    0D                                                               
RPTTOTS  DS    0XL44                                                            
RPTSPOTS DS    F                                                                
RPTDOLS  DS    F                                                                
RPTTAX   DS    F                                                                
RPTDEMS  DS    0XL32                                                            
RPTDEM1  DS    F                                                                
RPTDEQ1  DS    F                                                                
RPTDEM2  DS    F                                                                
RPTDEQ2  DS    F                                                                
RPTDEM3  DS    F                                                                
RPTDEQ3  DS    F                                                                
RPTDEM4  DS    F                                                                
RPTDEQ4  DS    F                                                                
         DS    0D                                                               
PASSTOTS DS    0XL44                                                            
PASSSPTS DS    F                                                                
PASSDOLS DS    F                                                                
PASSTAX  DS    F                                                                
PASSDEMS DS    0XL32                                                            
PASSDEM1 DS    F                                                                
PASSDEQ1 DS    F                                                                
PASSDEM2 DS    F                                                                
PASSDEQ2 DS    F                                                                
PASSDEM3 DS    F                                                                
PASSDEQ3 DS    F                                                                
PASSDEM4 DS    F                                                                
PASSDEQ4 DS    F                                                                
*                                                                               
BUYSPTS  DS    F                                                                
BUYDOLS  DS    F                                                                
BUYTAX   DS    F                                                                
MYBUYSPT DS    F                   SPOTS THIS PASS FOR ALL RATES                
*                                                                               
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
TAX      DS    F                                                                
*                                                                               
EQUDEMS  DS    0XL16                                                            
EQUDEM1  DS    F                                                                
EQUDEM2  DS    F                                                                
EQUDEM3  DS    F                                                                
EQUDEM4  DS    F                                                                
*                                                                               
         DS    0D                                                               
MYSPTS   DS    XL53                                                             
         DS    0D                                                               
MYDOLS   DS    XL212               4BYTES/WEEK (53*4)                           
*                                                                               
BIGSTA   DS    CL8                                                              
PASS     DS    XL1                                                              
MAXPASS  DS    XL1                                                              
MIDPASS  DS    XL1                                                              
SVBUYKEY DS    XL10                                                             
*                                                                               
         DS    0D                                                               
COVTAB   DS    8XL56               COV(3)+WKS(53) X MAX 8 COST OVRDS            
COVTABX  EQU   *                                                                
*                                                                               
SVMID2   DS    CL132                                                            
SVMID3   DS    CL132                                                            
         ORG                                                                    
*                                                                               
         DS    0D                                                               
SVEL05   DS    XL128                                                            
SVEL06   DS    XL64                                                             
SVEL09   DS    XL128               TAX                                          
*                                                                               
         DS    0D                                                               
EQVREC   DS    XL256                                                            
*                                                                               
         DS    0D                                                               
         DS    XL7                                                              
TSARTRC  DS    X                   MUST BE ONE BYTE BEFORE TSARBLK !            
TSARBLK  DS    CL48                                                             
*                                                                               
TSARREC  DS    0D                                                               
*                                                                               
TSARKEY  DS    0XL6                                                             
TSARSTA  DS    XL3                 PACKED STATION                               
TSARLIN  DS    XL2                 LINE NUMBER                                  
TSARSEQ  DS    XL1                 SEQNUM 00    BUY DATA                        
*                                         01-08 COST OVRDS                      
*                                         0F    ORBITUIARY                      
*                                         11-15 COMMENT                         
TSFLAG   DS    XL1                 NON-ZERO = RECORD DELETED                    
TSBDDAY  DS    XL1                                                              
TSBDTIME DS    XL4                                                              
         DS    XL3                                                              
*                                                                               
TSDEMS   DS    4XL8                                                             
*                                                                               
TSBDCOST DS    XL3                                                              
TSBDCIND DS    XL1                                                              
TSBDCIN2 DS    XL1                                                              
TSBDSEC  DS    XL1                                                              
TSBDPROG DS    CL17                                                             
TSSPTS   DS    XL53                                                             
TSDOLS   DS    XL216                                                            
TSTAX    DS    XL216                                                            
*                                                                               
         ORG   TSBDDAY                                                          
TSORBS   DS    8XL16                                                            
*                                                                               
         ORG   TSBDDAY                                                          
TSCOM    DS    CL70                                                             
         ORG                                                                    
TSARRECX EQU   *                                                                
         ORG                                                                    
         ORG   EQVREC                                                           
       ++INCLUDE SPGENEQU                                                       
         ORG                                                                    
MYWORKX  EQU   *                                                                
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         PRINT ON                                                               
         ORG   P                                                                
SKBUYLIN DS    CL7                                                              
         DS    CL1                                                              
SKDAYS   DS    CL7                                                              
         DS    CL4                                                              
SKTIME   DS    CL11                                                             
         DS    CL5                                                              
SKGRID   DS    14CL4                                                            
         DS    CL2                                                              
SKTOTSP  DS    CL4                                                              
         DS    CL1                                                              
SKCOST   DS    CL9                                                              
         DS    CL2                                                              
SKDEMS   DS    CL21                                                             
         ORG   P+132                                                            
         DS    CL6                                                              
SKPROG   DS    CL17                                                             
         DS    CL4                                                              
SKSLN    DS    CL3                                                              
*                                                                               
SKORB    EQU   P+6                                                              
* AREA BELOW USED TO SAVE ORDER HISTORY ELEMENTS                                
         ORG   P                                                                
SVEL08   DS    8XL64                                                            
SVEL08X  EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
*                                                                               
       ++INCLUDE SPGENDRORD                                                     
*                                                                               
       ++INCLUDE SPGENORHIS                                                     
*                                                                               
       ++INCLUDE SPADBUYER                                                      
*                                                                               
       ++INCLUDE SPADAVCOM                                                      
*                                                                               
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPOMSFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPOMCOMD                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
REQD     DSECT 0D                                                               
REQHDR   DS    0XL26                                                            
       ++INCLUDE DMREQHDR                                                       
RQAREA   DS    0CL80   COLUMN                                                   
RQPROG   DS    0CL2    ------                                                   
RQCODE   DS    CL2        1        PROGRAM CODE                                 
RQAGY    DS    CL2        3        AGENCY CODE                                  
RQMED    DS    CL1        5        MEDIA CODE (R/T)                             
RQCLT    DS    CL3        6        CLIENT CODE                                  
RQPGR    DS    CL1        9        PROCESS BY DIVISION                          
RQMGR    DS    CL1       10        PROCESS BY DISTRICT                          
RQCLOFF DS     CL1       11        CLIENT OFFICE FILTER                         
RQPRD    DS    CL3       12        PRODUCT MNEMONIC                             
RQMKT    DS    CL4       15        MARKET NUMBER                                
RQSTA    DS    CL5       19        STATION CALL LETTERS                         
RQEST    DS    CL3       24        ESTIMATE NUMBER                              
RQESTEND DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
RQDEMOVR DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
RQCONT   DS    CL1       31        C'*' ==> DATA IN QAREA2                      
RQSTAUTO DS    CL3       32        AUTO REQUEST START DATE                      
RQENAUTO DS    CL3       35        AUTO REQUEST END DATE                        
RQSTART  DS    CL6       38        REQUEST START DATE                           
RQEND    DS    CL6       44        REQUEST END DATE                             
         DS    CL12      50                                                     
RQOPT1   DS    CL1       62        OPTION 1                                     
RQOPT2   DS    CL1       63        OPTION 2                                     
RQOPT3   DS    CL1       64        OPTION 3                                     
RQOPT4   DS    CL1       65        OPTION 4                                     
RQOPT5   DS    CL1       66        OPTION 5                                     
RQGRP    DS    CL2       67        GROUP                                        
RQUESTOR DS    CL12      69        REQUESTOR NAME                               
*                                                                               
RQAREA2  DS    0CL80               REQUEST CARD 2 DATA AREA                     
RQCRRNCY DS    CL1     COL 01      CURRENCY OVRD - U=US,C=CANADA                
RQLANG   DS    CL1         02      LANGUAGE (C'F' FOR FRENCH)                   
RQGST    DS    CL1         03      C'I OR Y' = INCLUDE INPUT GST                
*                                  C'O'      = INCLUDE OUTPUT GST               
RQDEMADJ DS    CL1         04      C'N' = SUPPRESS SPCL DEMO ADJ FCTR           
RQCLGID  DS    CL1         05      CLTGRP ID                                    
RQCLGRP  DS    CL4         06      CLTGRP NUMBER                                
RQPTNR   DS    CL3         10      PARTNER PRODUCT                              
RQCBLNET DS    CL3         13      CABLE NETWORK                                
RQCOST2  DS    CL1         16      REPORT COST2 DOLLARS                         
RQSGR    DS    CL1         17      C'Y' = STATION IS A STATION GROUP            
RQMGA    DS    CL1         18      PRINT MAKEGOOD ANALYSIS                      
RQPWCV   DS    CL1         19      PW CLIENT VERSION                            
         DS    CL1         20      THIS FIELD FOR COMMON USE                    
RQ2CVRSN DS    XL1         21      CURRENT VERSION NUMBER                       
RQ2PVRSN DS    XL1         22      PREVIOUS VERSION NUMBER                      
RQ2PVRSD DS    XL2         23-24   PREVIOUS VERSION DATE                        
RQ2RSND  DS    CL1         25      C'Y' = RESEND                                
         ORG   RQ2CVRSN                                                         
RQ2USER  DS    CL50        21      THESE ARE FOR USE BY PROGRAMS                
         ORG                                                                    
         DS    CL10                RESERVED FOR COMMON USE                      
         ORG   RQAREA2+38                                                       
RQ2DEMO  DS    CL1         39      INCL DEMOS                                   
RQ2DRORD DS    CL8         40      DARE ORDER NUMBER                            
RQ2DRSID DS    CL2         48      SYSTEM ID                                    
RQ2AGYMD DS    CL2         50      EBCDIC AGENCY/MEDIA                          
       ++INCLUDE FAXTRAINF                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPOMS29   03/24/09'                                      
         END                                                                    
