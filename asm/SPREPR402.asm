*          DATA SET SPREPR402  AT LEVEL 003 AS OF 05/01/02                      
*PHASE SPR402A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SPREPR402 (SPR402) - REP -> SPOT BUY FILE FIXER'                
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR402 (SPR402) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          SPOT BUY FIXER                            *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
* QOPT1   =   IF 'N' RUN IN SOFT MODE (NO UNPDATING)                 *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
* WHILE NOT EOF ON SPOT BUYS                                         *          
*     -FIX BROKEN TRANSFERED BUYS AND WRITE TO THE SPOT FILE         *          
* END WHILE                                                          *          
*                                                                    *          
* PRODUCE CONTROL REPORT                                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  FEB19/92 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
SPR402   CSECT                                                                  
         NMOD1 0,SPR402,R9,R8,RR=R2                                             
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,RA                                                    
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAINLINE                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE EQU   *                                                                
         MVC   P(24),=C'* * * START OF RUN * * *'                               
         GOTO1 REPORT                                                           
         BAS   RE,READIN                                                        
         BNZ   MAINERR                                                          
         BAS   RE,DOREPORT                                                      
         BNZ   MAINERR                                                          
*                                                                               
*        END MAINLINE / FINISH RUN                                              
*                                                                               
MAINGOOD EQU   *                                                                
         MVC   P(21),=C'*** END OF REPORT ***'                                  
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
MAINERR  EQU   *                                                                
         MVC   P(40),=C'* * * ERROR ENCOUNTERED DURING RUN * * *'               
         GOTO1 REPORT                                                           
         MVC   P(60),RUNERROR                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=C'* * * RUN TERMINATED * * *'                             
         GOTO1 REPORT                                                           
MAINEXIT EQU   *                                                                
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
*                                                                               
*        READIN --- READ AND PROCESS THE SPOT FILE                              
*                                                                               
READIN   NTR1                                                                   
*                                                                               
         L     RF,=V(HELLO)                                                     
         A     RF,RELO                                                          
         ST    RF,HELLO                                                         
         LA    RF,IOAREA1                                                       
         ST    RF,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'B2'                  INTEREP RADIO                         
         GOTO1 HIGH                                                             
         B     RRIN20                                                           
*                                                                               
RRIN10   EQU   *                                                                
         GOTO1 SEQ                        GET NEXT RECORD                       
RRIN20   EQU   *                                                                
         MVI   RECUPED,C'N'                                                     
         CLI   KEY,X'B2'                                                        
         BNE   RRINGOOD                   ALL DONE                              
         CLI   KEY+3,X'FF'                ONLY USE POL KEYS                     
         BNE   RRIN10                                                           
         TM    KEY+13,X'80'                                                     
         BO    RRIN10                                                           
         GOTO1 GET                                                              
         L     RF,INRECS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,INRECS                                                        
         CLI   QOPT2,C'N'                 SKIP REP/SPOT XFER TESTS              
         BE    RRIN25                                                           
         XC    DMCB,DMCB                                                        
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'98',AREC)  TRACE EL?                
         CLI   DM4,0                                                            
         BNE   RRIN10                     NO TRACE EL, CAN'T BE OURS            
         L     R4,DM4                                                           
         CLI   1(R4),X'16'                IS IT OUR LENGTH?                     
         BNE   RRIN10                     NO, GET NEXT                          
         XC    DMCB,DMCB                                                        
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'70',AREC)  ID EL?                   
         CLI   DM4,0                                                            
         BNE   RRIN10                     NO ID EL, CAN'T BE OURS               
         L     R4,DM4                                                           
         CLI   1(R4),X'15'                IS IT OUR LENGTH?                     
         BNE   RRIN10                     NO, GET NEXT                          
RRIN25   EQU   *                                                                
         XC    DMCB,DMCB                                                        
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'01',AREC)  DESC ELEMENT             
         CLI   DM4,0                                                            
         BNE   RRIN10                     NO DESCRIPTION EL??????               
         L     R4,DM4                                                           
         USING BDELEM,R4                                                        
         OC    BDSTART(3),BDSTART         START DATE MISSING?                   
         BNZ   RRIN50                                                           
         BAS   RE,PRINTB                                                        
         MVI   RECUPED,C'Y'                                                     
         MVC   P(L'DATEMISS),DATEMISS                                           
         GOTO1 REPORT                                                           
         XC    DMCB,DMCB                                                        
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'0B',AREC)  GET 1ST SPOT             
         CLI   DM4,0                                                            
         BE    RRIN30                                                           
         MVC   P(L'NOSPOTS),NOSPOTS                                             
         GOTO1 REPORT                                                           
         MVC   BDEND,BDSTART                                                    
         B     RRIN50                     NO SPOTS??????                        
RRIN30   EQU   *                                                                
         L     R3,DM4                                                           
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,BDSTART)                                
RRIN50   EQU   *                                                                
         CLI   BDSEDAY,0                                                        
         BE    RRIN70                     SKIP IF NOT LOADED                    
         MVC   BDSEEND(1),BDSEDAY                                               
         NI    BDSEEND,X'0F'                                                    
         ZIC   RF,BDSEDAY                                                       
         SRL   RF,4                                                             
         STC   RF,BDSESTA                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,WORK)                                 
         XC    DMCB(8),DMCB                                                     
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         MVC   STARTDAY(1),DMCB           GET START DAY NUMBER                  
         GOTO1 DATCON,DMCB,(3,BDEND),(0,WORK)                                   
         XC    DMCB(8),DMCB                                                     
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         MVC   ENDDAY(1),DMCB                                                   
         CLC   STARTDAY(1),BDSESTA                                              
         BNE   RRIN60                                                           
         CLC   ENDDAY(1),BDSEEND                                                
         BE    RRIN70                                                           
RRIN60   EQU   *                                                                
         CLI   RECUPED,C'Y'                                                     
         BE    RRIN62                                                           
         BAS   RE,PRINTB                                                        
         MVI   RECUPED,C'Y'                                                     
RRIN62   EQU   *                                                                
         MVC   P(L'SEDAYS),SEDAYS                                               
         GOTO1 REPORT                                                           
         ZIC   RF,STARTDAY                                                      
         SLL   RF,4                                                             
         STC   RF,BDSEDAY                                                       
         OC    BDSEDAY(1),ENDDAY                                                
RRIN70   EQU   *                                                                
         CLI   BDWKIND,C'0'                                                     
         BNE   RRIN80                                                           
         MVI   BDWKIND,C'O'                                                     
         CLI   RECUPED,C'Y'                                                     
         BE    RRIN72                                                           
         BAS   RE,PRINTB                                                        
         MVI   RECUPED,C'Y'                                                     
RRIN72   EQU   *                                                                
         MVC   P(L'WKIND),WKIND                                                 
         GOTO1 REPORT                                                           
RRIN80   EQU   *                                                                
         CLI   RECUPED,C'Y'                                                     
         BNE   RRIN10                        NO UPDATE, LOOP                    
         L     RF,OUTRECS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,OUTRECS                                                       
         CLI   QOPT1,C'N'                                                       
         BE    RRIN10                                                           
         GOTO1 PUT                                                              
         B     RRIN10                                                           
*                                                                               
RRINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
RRINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         SPACE 2                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        PRINTB --- PRINT THIS BUYLINE'S KEY                                    
*                                                                               
PRINTB   NTR1                                                                   
*                                                                               
         L     R4,AREC                                                          
         USING BUYREC,R4                                                        
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+00(08),=C'CLIENT ='                                            
         GOTO1 CLUNPK,DMCB,BUYKCLT,P+10                                         
         MVC   P+15(09),=C'STATION ='                                           
         GOTO1 MSUNPK,DMCB,BUYMSTA,WORK,P+25                                    
         MVC   P+32(10),=C'ESTIMATE ='                                          
         EDIT  (B1,BUYKEST),(3,P+44)                                            
         MVC   P+50(13),=C'LINE NUMBER ='                                       
         EDIT  (B1,BUYKBUY),(3,P+65)                                            
         GOTO1 REPORT                                                           
*                                                                               
PNTBGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
PNTBBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         SPACE 2                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        DOREPORT --- PRODUCE EXTRACT CONTROL REPORT                            
*                                                                               
DOREPORT NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(36),=CL36'* * * RUN TOTALS * * *'                              
         GOTO1 REPORT                                                           
         MVC   P(31),=CL31'NUMBER OF INTEREP BUYS READ =  '                     
         EDIT  (B4,INRECS),(6,P+32),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         MVC   P(27),=CL27'NUMBER OF BUYS UPDATED =   '                         
         EDIT  (B4,OUTRECS),(6,P+32),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
*                                                                               
*        DOREPORT EXIT                                                          
*                                                                               
DREPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
DREPBAD  EQU   *                                                                
         LA    R0,1                                                             
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        ERROR TEXTS                                                            
*                                                                               
DATEMISS DC    C'START DATE MISSING ON THIS BUYLINE'                            
NOSPOTS  DC    C'NO SPOTS ON THIS BUYLINE'                                      
SEDAYS   DC    C'THE START AND END DAYS ON THIS BUYLINE WERE WRONG'             
WKIND    DC    C'THE WEEK INDICATOR FIELD WAS SWITCH FROM 0 TO O'               
         EJECT                                                                  
*                                                                               
*        LITERAL POOL                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        IO AREA                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IOAREA*'                                                    
IOAREA1  DS    2008C                                                            
IOAREA1X EQU   *                                                                
IOAREA1L EQU   IOAREA1X-IOAREA1                                                 
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*LOCAL**'                                                    
HELLO    DS    A                                                                
INRECS   DS    F            NUMBER OF BUYS READ                                 
OUTRECS  DS    F            NUMBER OF BUYS UPDATED                              
RECUPED  DC    CL1'N'       THIS RECORD UPDATED FLAG                            
STARTDAY DS    CL1                                                              
ENDDAY   DS    CL1                                                              
BDSESTA  DS    CL1                                                              
BDSEEND  DS    CL1                                                              
RUNERROR DS    CL60                                                             
         EJECT                                                                  
SBUYRECD DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPR402 05/01/02'                                      
         END                                                                    
