*          DATA SET SPLDEXTYK3 AT LEVEL 081 AS OF 04/19/00                      
*PHASE SPEXTYK3                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'FIND BRAND BUY CLIENTS'                                         
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         USING CLTHDRD,R3                                                       
         CLI   CKEYTYPE,0          CLT/PRD/EST/BILL?                            
         BNE   DMXKEEP             NO                                           
         CLI   CKEYAM,0                                                         
         BE    DMXKEEP                                                          
         OC    4(9,R3),4(R3)       CLT RECS END WITH ALL 0000S                  
         BNZ   CKPRD                                                            
         CLI   CPROF,C'0'          POSSIBLE BRD CLT ?                           
         BNE   DMXKEEP                                                          
         BAS   RE,PRTREC                                                        
DMXREC10 MVC   SVCLT,CKEYCLT       REMEMBER CLIENT                              
         MVC   SVAM,CKEYAM         AND AGY/MED                                  
         XC    ESTTBL(ESTTBLQ),ESTTBL                                           
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
CKPRD    DS    0H                                                               
         CLC   SVCLT,CKEYCLT       DID CLIENT CHANGE ?                          
         BNE   DMXKEEP                                                          
         OC    8(5,R3),8(R3)       IS IT EST REC?                               
         BNZ   DMXKEEP                                                          
         CLI   7(R3),0                                                          
         BNH   DMXKEEP                                                          
         USING ESTHDRD,R3                                                       
         ZIC   R2,EKEYEST          PUT EST VALUE IN R2                          
         LA    R5,ESTTBL(R2)                                                    
         CLI   0(R5),C'P'                                                       
         BE    DMXKEEP                                                          
         MVI   0(R5),C'E'          BY DEFAULT PUT E                             
         CLC   EKEYPRD,=C'POL'                                                  
         BNE   DMXKEEP                                                          
         MVI   0(R5),C'P'          IF POL, PUT 'P'                              
         B     DMXKEEP                                                          
*                                                                               
******** PRINT OUT REC                                                          
*                                                                               
PRTREC   NTR1                                                                   
*                                                                               
         SR    R2,R2                   EST COUNTER                              
         LA    R5,ESTTBL                                                        
         NI    FLAG,X'FF'-X'80'        RESET NO EST FLAG                        
*                                                                               
         CLC   ESTTBL(4),=X'FFFFFFFF'  TO AVOID PRTING 1ST TIME                 
         BE    PRTRX                                                            
*                                                                               
         OC    ESTTBL(ESTTBLQ),ESTTBL  CK IF EST REC EXISTS                     
         BNZ   PRTR10                                                           
         OI    FLAG,X'80'                                                       
         MVC   P+15(13),=C'NO EST RECORD'                                       
         B     PRTR12                                                           
*                                                                               
PRTR10   CLI   0(R5),C'E'                                                       
         BNE   PRTR30                  IF 'E' PRT INFO                          
         MVC   P+15(19),=C'*****ATTENTION*****'                                 
*                                                                               
PRTR12   GOTO1 VHEXOUT,DMCB,SVAM,P,1,0                                          
*                                                                               
         MVC   BYTE,SVAM               MEDIA                                    
         NI    BYTE,X'0F'                                                       
         MVI   P+3,C'T'                                                         
         CLI   BYTE,X'01'                                                       
         BE    PRTR20                                                           
         MVI   P+3,C'R'                                                         
         CLI   BYTE,X'02'                                                       
         BE    PRTR20                                                           
         MVI   P+3,C'N'                                                         
         CLI   BYTE,X'03'                                                       
         BE    PRTR20                                                           
         MVI   P+3,C'C'                                                         
         CLI   BYTE,X'08'                                                       
         BE    PRTR20                                                           
         MVI   P+3,C'X'                                                         
         CLI   BYTE,X'04'                                                       
         BE    PRTR20                                                           
         MVI   P+3,C'?'                                                         
*                                                                               
PRTR20   DS    0H                                                               
         GOTO1 VCLUNPK,DMCB,SVCLT,P+6        CLIENT                             
         EDIT  (R2),(3,P+11),FILL=0          EST                                
*                                                                               
         GOTO1 VPRINTER                                                         
         TM    FLAG,X'80'                                                       
         BO    PRTRX                                                            
*                                                                               
PRTR30   LA    R5,1(R5)                                                         
         AHI   R2,1                          GOTO NXT EST                       
         CHI   R2,256                                                           
         BL    PRTR10                                                           
*                                                                               
PRTRX    XIT1                                                                   
*                                                                               
DMXRX    B     DMXKEEP             KEEP IT                                      
         EJECT                                                                  
*                                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
*                                                                               
COUNT    DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
FLAG     DS    X                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
SVAM     DS    X                                                                
SVCLT    DS    CL2                                                              
ESTTBL   DC    256X'FF'                                                         
ESTTBLQ  EQU   *-ESTTBL                                                         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
VCLUNPK  DS    A                                                                
*                                                                               
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081SPLDEXTYK304/19/00'                                      
         END                                                                    
