*          DATA SET SPLDEXTKEV AT LEVEL 086 AS OF 03/17/98                      
*PHASE SPEXTKEV                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
SPEXTWHO TITLE 'SPLDEXTKEV - CHECK UNUSED CABLE NETWORKS'                       
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
SPLDEXT  CSECT                                                                  
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
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         L     RF,=V(RECUP)                                                     
         ST    RF,VRECUP                                                        
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
***********************************************************************         
* INITIALIZE LOGIC                                                    *         
***********************************************************************         
DMXINIT  DS    0H                                                               
                                                                                
         ZAP   PCKOF16B,=P'0'                                                   
         BAS   RE,UNUSECBL         BUILD LIST OF UNUSED CABLE NETWORKS          
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD LOGIC                                                *         
***********************************************************************         
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING BUYREC,R6                                                        
         CLI   0(R6),X'10'         BUY RECORDS?                                 
         BL    DMXKEEP             NOT BUY RECORDS                              
*                                                                               
         TM    BUYMSTA+2,X'F0'                                                  
         BNO   DMXKEEP             NOT CABLE SATIONS                            
*                                                                               
         CLI   BUYMSTA+4,X'00'     NOTHING IN THAT BYTE?                        
         BE    DMXKEEP                                                          
*                                                                               
         MVC   BYTE,BUYMSTA+4      LAST BYTE OF STATION                         
         NI    BYTE,X'7F'                                                       
*                                                                               
         CLI   BYTE,X'7F'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CABLETAB         POINT TO CABLE NETWORK TABLE                 
         ZICM  R5,BYTE,1           DIRECT ACCESS TO CABLE NETWORK TABLE         
         BCTR  R5,0                                                             
         MH    R5,=H'6'                                                         
         AR    R3,R5                                                            
*                                                                               
DMXREC20 CLI   5(R3),X'80'         CURRENT NETWORK IS UNUSED?                   
         BNE   DMXKEEP                                                          
         LA    R4,UNUSEDCB                                                      
         LA    R5,127              LOOP COUNTER                                 
*                                                                               
DMXREC30 CLC   BYTE,0(R4)                                                       
         BNE   *+12                                                             
         MVI   0(R4),X'00'                                                      
         B     DMXRECX                                                          
         LA    R4,1(R4)            NEXT ITEM IN UNUSED CABLE LIST               
         BCT   R5,DMXREC30                                                      
*                                                                               
DMXRECX  B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF UNUSED CABLE NETWORKS                               *         
***********************************************************************         
UNUSECBL NTR1                                                                   
         XC    UNUSEDCB,UNUSEDCB                                                
         LA    R3,CABLETAB                                                      
         LA    R4,UNUSEDCB                                                      
*                                                                               
UNUSE10  CLI   0(R3),X'FF'         END OF CABLE NETWORK TABLE?                  
         BE    UNUSEX                                                           
         CLI   5(R3),X'80'         UNUSED CABLE NETWORK?                        
         BNE   *+14                                                             
         MVC   0(1,R4),3(R3)       BUILT UNUSED CABLE NETWORK LIST              
         LA    R4,1(R4)                                                         
         LA    R3,L'CABLETAB(R3)                                                
         B     UNUSE10                                                          
*                                                                               
******** GOTO1 =V(HEXOUT),DMCB,UNUSEDCB,P,127    ****                           
******** GOTO1 VPRINTER                          ****                           
UNUSEX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* END OF FILE IS REACHED                                              *         
***********************************************************************         
DMXEOF   DS    0H                                                               
*                                                                               
******** GOTO1 =V(HEXOUT),DMCB,UNUSEDCB,P,127    ****                           
******** GOTO1 VPRINTER                          ****                           
         GOTO1 VPRINTER            SKIP SOME LINES                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(17),=CL17'UNUSED NETWORKS: '                                   
         LA    R2,P+17                                                          
         LA    R4,UNUSEDCB                                                      
         LA    R5,127              LOOP COUNTER                                 
         SR    R6,R6               NUMBER OF NETWORK PER LINE                   
*                                                                               
DMXEOF10 CLI   0(R4),X'00'                                                      
         BE    DMXEOF50                                                         
         LA    R3,CABLETAB                                                      
*                                                                               
         MVC   BYTE,0(R4)          DIRECT ACCESS TO CABLE NETWORK TABLE         
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'6'                                                         
         AR    R3,RE                                                            
*                                                                               
         CH    R6,=H'25'           25 NETWORK CODES PER LINE                    
         BL    DMXEOF30                                                         
         GOTO1 VPRINTER                                                         
         SR    R6,R6               RESET NETWORKS PER LINE COUNTER              
         LA    R2,P+17             RESET PRINT LINE                             
*                                                                               
DMXEOF30 SR    RE,RE                                                            
         IC    RE,4(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
         LA    R2,1(RE,R2)         NEXT POSITION ON PRINT LINE                  
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         LA    R6,1(R6)            NUMBER OF NETWORK PER LINE COUNTER           
*                                                                               
DMXEOF50 LA    R4,1(R4)                                                         
         BCT   R5,DMXEOF10                                                      
*                                                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER            SKIP SOME LINES                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT THE RECORD                                                              
***********************************************************************         
PRNTRECD NTR1                                                                   
         L     R6,AREC                                                          
         MVC   P(37),=CL37'     0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'               
         GOTO1 VPRINTER                                                         
***************                                                                 
* DISPLAY THE KEY TO BEFORE THE 1ST ELEMENT                                     
***************                                                                 
         BAS   RE,CALCDISP                                                      
         GOTO1 VHEXOUT,DMCB,FULL+2,P,2                                          
         GOTO1 VHEXOUT,DMCB,0(R6),P+5,16                                        
         GOTO1 VPRINTER                                                         
         LA    R6,16(R6)                                                        
         BAS   RE,CALCDISP                                                      
         GOTO1 VHEXOUT,DMCB,FULL+2,P,2                                          
         GOTO1 VHEXOUT,DMCB,0(R6),P+5,8                                         
         GOTO1 VPRINTER                                                         
         LA    R6,8(R6)                                                         
***************                                                                 
* DISPLAY THE ELEMENTS                                                          
***************                                                                 
PRTRC10  CLI   0(R6),0                                                          
         BE    PRTRC100                                                         
*                                                                               
         ZIC   R2,1(R6)            R2 = LENGTH OF THIS ELEMENT                  
PRTRC15  BAS   RE,CALCDISP                                                      
         GOTO1 VHEXOUT,DMCB,FULL+2,P,2                                          
         CH    R2,=H'16'                                                        
         BNH   PRTRC20                                                          
         GOTO1 VHEXOUT,DMCB,0(R6),P+5,16                                        
         GOTO1 VPRINTER                                                         
         LA    R6,16(R6)                                                        
         SH    R2,=H'16'                                                        
         B     PRTRC15                                                          
*                                                                               
PRTRC20  ST    R2,DMCB+8                                                        
         GOTO1 VHEXOUT,DMCB,0(R6),P+5                                           
         GOTO1 VPRINTER                                                         
         AR    R6,R2                                                            
         B     PRTRC10                                                          
*                                                                               
PRTRC100 MVC   P(37),=CL37'     0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'               
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
*                                                                               
CALCDISP DS    0H                                                               
         LR    R1,R6                                                            
         L     R0,AREC                                                          
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
PCKOF16B DC    PL16'0'                                                          
UNUSEDCB DC    XL127'00'           LIST OF UNUSED CABLE                         
         LTORG                                                                  
       ++INCLUDE SPCBLLST          CABLE NETWORK LIST TABLE                     
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
VRECUP   DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
BYTE     DS    X                                                                
ELEM     DS    CL256                                                            
OLDRLEN  DS    H                                                                
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY          BUY RECORD DSECT                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE DMLDDEFN          DMLDDEFN                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT          DDDPRINT                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086SPLDEXTKEV03/17/98'                                      
         END                                                                    
