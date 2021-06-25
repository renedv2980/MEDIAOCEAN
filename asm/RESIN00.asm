*          DATA SET RESIN00    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T80600C,*                                                                
*INCLUDE GETBROAD                                                               
         TITLE 'T80600 - REPPAK SINGLE INVOICE (SIN) - BASE'                    
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESIN00 --- REP SINGLE INVOICE PROGRAM                     *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG23/89 (MRR) --- HISTORY LOST                                   *           
*                                                                   *           
* 30OCT89  (EFJ) --- ADD SUPPORT FOR HOT-KEY SWITCHING FROM CNT00   *           
*                                                                   *           
* 06NOV95  (RHV) --- ADD CLEARING OF 'NET?' FIELD                   *           
*                                                                   *           
* 20MAY96  (WSB) --- CHANGE RESINGEN TO GIVE 600 BYTES OF LOCAL     *           
*                    WORKING STORAGE                                *           
*                                                                   *           
* 07JUN96  (WSB) --- STORE ADDRESSES OF HELLO AND GETFACT           *           
*                                                                   *           
* 25MAR99  (BU ) --- UPGRADE FOR ALTERNATE CALENDAR USE             *           
*                                                                   *           
* 12MAY99  (ROB) --- REPFACS ARRIVES                                *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GENOLDX-GENOLD,T80600,RR=R5,CLEAR=YES                            
         USING GENOLD,RC                                                        
         USING T806FFD,RA                                                       
         ST    R5,FULL             SAVE RELO FACTOR                             
         BAS   RE,INITL                                                         
         OI    SINSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         SPACE 1                                                                
         BAS   RE,CKGLOB                                                        
         SPACE 1                                                                
         ST    RB,BASERB                                                        
         LA    RE,GETMON                                                        
         ST    RE,VGETMON                                                       
         LA    RE,MOVEREC                                                       
         ST    RE,VMOVEREC                                                      
         L     RE,=V(GETBROAD)                                                  
         A     RE,FULL             RELOCATE VCON                                
         ST    RE,VGTBROAD                                                      
*                                                                               
         L     R5,16(R1)           A(COMFACS)                                   
         USING COMFACSD,R5                                                      
         L     RE,CHELLO                                                        
         ST    RE,VHELLO                                                        
         L     RE,CGETFACT                                                      
         ST    RE,VGETFACT                                                      
         L     RF,CCALLOV                                                       
         DROP  R5                                                               
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 (RF),DMCB,0,X'D9000AAC',0                                        
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
         STCM  R5,15,RFBLOCK       BUILD RFBLOCK                                
         MVC   RFBLOCK+4(2),REPALPHA                                            
*                                                                               
         LA    R3,ACTERR                                                        
         LA    R2,SINACTH                                                       
         MVC   SINMSG,SPACES                                                    
         SPACE 1                                                                
* TODAY'S DATE                                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,HALF)                                      
         GOTO1 (RF),(R1),(2,HALF),(3,TODAY)                                     
         SPACE 1                                                                
*              GET CURRENT ACCOUNTING MONTH                                     
*                                                                               
         XC    KEY,KEY             BUILD EOM KEY                                
         MVI   KEY,X'18'                                                        
         MVC   KEY+24(2),REPALPHA  REP                                          
         MVC   KEY+26(1),TODAY     YEAR                                         
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC           GET EOM RECORD                               
         SR    R7,R7                                                            
         LA    R8,REOMDATE                                                      
         CLC   SINACT(3),=C'CHA'   REGULAR $ CHANGE                             
         BE    GA010                                                            
         CLC   SINACT(3),=C'CHN'   ALTERNATE CALENDAR CHANGE                    
         BE    GA010                                                            
         XC    SINNETT,SINNETT     CLEAR 'NET?' FIELD                           
         OI    SINNETTH+6,X'80'                                                 
*                                                                               
GA010    CLC   HALF,0(R8)          SEARCH FOR ACCOUNTING MONTH                  
         BNH   GA020                                                            
         LA    R7,2(R7)                                                         
         CH    R7,=H'24'                                                        
         BH    GA040                                                            
         LA    R8,REOMDATE(R7)                                                  
         B     GA010                                                            
*                                                                               
GA020    LTR   R7,R7                                                            
         BZ    GA030                                                            
         SRA   R7,1                                                             
         STC   R7,ACCMON+1         MONTH                                        
         MVC   ACCMON(1),TODAY     YEAR                                         
         B     GA999                                                            
*                                                                               
GA030    MVI   ACCMON+1,12         DECEMBER                                     
         ZIC   R1,TODAY            LAST YEAR                                    
         BCTR  R1,0                                                             
         STC   R1,ACCMON                                                        
         B     GA999                                                            
*                                                                               
GA040    MVI   ACCMON+1,1          JANUARY                                      
         ZIC   R1,TODAY            NEXT YEAR                                    
         LA    R1,1(R1)                                                         
         STC   R1,ACCMON                                                        
*                                                                               
GA999    DS    0H                                                               
*                                                                               
* GET MONDAY DATE                                                               
         GOTO1 VDATCON,DMCB,(3,TODAY),DUB                                       
*                                                                               
         GOTO1 VGETDAY,(R1),DUB,FULL                                            
         CLC   FULL(3),=3C' '                                                   
         BNE   *+6                                                              
         DC    H'0'                COMRG DATE INVALID                           
*                                                                               
         SR    R6,R6                                                            
         IC    R6,DMCB             DAY OF WEEK                                  
         BCTR  R6,R0                                                            
         LNR   R6,R6                                                            
         GOTO1 VADDAY,(R1),DUB,DMCB+12,(R6)                                     
*                                                                               
         GOTO1 VDATCON,(R1),DMCB+12,(2,THISWEEK)                                
         LA    R2,SINACTH                                                       
*                                                                               
         CLC   SINACT(3),=C'DIS'   REGULAR $ DISPLAY?                           
         BE    CALLDIS             YES                                          
         CLC   SINACT(3),=C'DIN'   ALTERNATE CALENDAR $ DISPLAY                 
         BNE   B100                NO                                           
* DISPLAY CONTRACT                                                              
CALLDIS  GOTO1 VCALLOV,(R1),(X'20',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
B100     EQU   *                                                                
         CLC   SINACT(3),=C'CHA'   REGULAR $ CHANGE?                            
         BE    B120                YES                                          
         CLC   SINACT(3),=C'CHN'   ALTERNATE CALENDAR $ CHANGE?                 
         BNE   ERROR               NO                                           
* CALL CHANGE PROGRAM                                                           
B120     EQU   *                                                                
         OC    TWAKADDR,TWAKADDR                                                
         LA    R3,DISERR                                                        
         BZ    ERROR                                                            
         GOTO1 VCALLOV,(R1),(X'10',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   ERRAREA,X'FF'                                                    
         BE    EXXMOD                                                           
         B     CALLDIS                                                          
         EJECT                                                                  
* ROUTINE TO MOVE RECORD           P1=A(FROM REC)                               
*                                  P2=A(TO REC)                                 
*                                                                               
MOVEREC  NTR1  BASE=BASERB                                                      
*                                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         MVC   HALF,27(R2)                                                      
         LH    R5,HALF                                                          
MOVE100  LTR   R5,R5                                                            
         BNP   MOVEXIT                                                          
*                                                                               
         CH    R5,=H'256'                                                       
         BL    MOVEREST                                                         
         MVC   0(256,R3),0(R2)                                                  
         SH    R5,=H'256'                                                       
         LA    R2,256(R2)                                                       
         LA    R3,256(R3)                                                       
         B     MOVE100                                                          
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVAR                                                       
MOVEXIT  L     R6,4(R1)                                                         
         LH    R5,27(R6)                                                        
         AR    R6,R5                                                            
         MVI   0(R6),0                                                          
         XIT1                                                                   
MOVEVAR  MVC   0(0,R3),0(R2)                                                    
         EJECT                                                                  
* ROUTINE TO GET BUCKET AMOUNT     P1=A(CONREC)   BYTE 0=BUCKET EL-CODE         
* FOR 1 MONTH FROM CONTRACT        P2=A(2-BYTE YEAR-MONTH)                      
*                                  P3=A(4-BYTE OUTPUT AMOUNT)                   
GETMON   NTR1  BASE=BASERB                                                      
         L     R6,4(R1)            MONTH                                        
         L     R2,0(R1)            CONREC                                       
         SR    R5,R5               CTR                                          
         LA    R3,34(R2)                                                        
         SR    R4,R4                                                            
         B     *+12                                                             
GM100    IC    R4,1(R3)            ELEM LEN                                     
         LA    R3,0(R4,R3)         NEXT ELEM                                    
*                                                                               
         CLI   0(R3),0             LAST?                                        
         BNE   GM200                                                            
GMXIT    L     R4,8(R1)                                                         
         ST    R5,FULL                                                          
         MVC   0(4,R4),FULL        OUTPUT                                       
         XIT1                                                                   
GM200    CLC   0(1,R3),0(R1)       ELEM CODE (INV OR ORD)                       
         BNE   GM100                                                            
* CHECK MONTH                                                                   
         CLC   2(2,R3),0(R6)       SAME MONTH?                                  
         BNE   GM100                                                            
* ADD TO MONTH                                                                  
         MVC   FULL,6(R3)          AMT                                          
         A     R5,FULL                                                          
         MVI   4(R1),X'FF'         HIT INDICATOR                                
         B     GM100                                                            
         EJECT                                                                  
*                                                                               
* CKGLOB: CALL GLOBBER TO SEE IF WE CAME HERE VIA PFKEY                         
*                                                                               
CKGLOB   NTR1                                                                   
         L     RF,16(R1)           V(COMFACS)                                   
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',SINNUMH,,GLRCONNO                             
         CLI   DMCB+8,0                                                         
         BNE   CKGLGOOD                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',BYTE,L'BYTE,GLRPFKEY                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLRCONNO                                    
         CLI   BYTE,7                                                           
         BNE   CKGLGOOD                                                         
         MVC   SINACT(8),=C'DISPLAY '                                           
         OI    SINACTH+6,X'80'     XMIT                                         
         MVI   SINACTH+4,X'80'     TURN ON FIELD INPUT THIS TIME                
         MVI   SINACTH+5,7         SET INPUT LENGTH                             
*                                                                               
*        CKGLOB EXIT                                                            
*                                                                               
CKGLGOOD EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
       ++INCLUDE RESINGEN                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037RESIN00   05/01/02'                                      
         END                                                                    
