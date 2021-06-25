*          DATA SET SPLDEXTCUT AT LEVEL 021 AS OF 04/05/06                      
*PHASE SP@XTCUT                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE RECUP                                                                  
SPLDXDEM TITLE 'UPDATE NTWK 68 ELS FOR CUTINS AND CLEAR COST OVRDS'             
*=====================================================================          
* READS A FILE OF NETWORK BUYS WITH CUTINS FOR EACH CLIENT                      
*  AND UPDATES NETWORK BUY 68 ELEMENTS FOR STATIONS WITH CUTINS                 
*=====================================================================          
                                                                                
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
         NMOD1 0,DMLDEXT                                                        
         LR    RC,RB                                                            
         AHI   RC,4096                                                          
         USING DMLDEXT+4096,RC                                                  
*                                                                               
         L     R9,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R9                                                       
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
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
*                                                                               
DMXEOF   DS    0H                                                               
         BAS   RE,DMXCNTS          PRINT RECORD COUNTS                          
         SPACE 2                                                                
DMXIT    XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* INITIALIZE LOGIC                                                              
*===============================================================                
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         L     RE,LSTXLST          ADD MYSELF TO DUMP LIST                      
         TM    4(RE),X'80'         TEST EOL                                     
         BO    *+12                                                             
         LA    RE,8(RE)                                                         
         B     *-12                                                             
         MVI   4(RE),0             RESET                                        
         LA    RE,8(RE)                                                         
         ST    RB,0(RE)                                                         
         L     R0,=A(ENDOFMOD)                                                  
         ST    R0,4(RE)                                                         
         OI    4(RE),X'80'         SET EOL FLAG                                 
         GOTO1 LSTXITER,DMCB,LSTXLST                                            
*                                                                               
         L     RE,=V(RECUP)                                                     
         ST    RE,RECUP                                                         
*                                                                               
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* PROCESS RECORD LOGIC                                                          
*===============================================================                
                                                                                
DMXREC   DS    0H                                                               
         L     R8,AREC                                                          
         USING BUYRECD,R8                                                       
*                                                                               
         CLI   0(R8),X'10'                                                      
         BL    DMXKEEP                                                          
*                                                                               
         OC    4(2,R8),4(R8)       TEST NETWORK BUY                             
         BNZ   DMXKEEP             NO - IGNORE                                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,13(R8)         GET RECORD LENGTH                            
         AR    RE,R8                                                            
         XC    0(2,RE),0(RE)       CLEAR TWO BYTES AT EOR                       
         MVI   CHGFLAG,C'N'                                                     
*                                                                               
         OI    BDSTAT3,BDST3_CNNEW                                              
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
DMX1     BRAS  RE,NEXTEL                                                        
         BNE   DMX2                                                             
         NI    6(R6),X'DF'         TURN OFF COST OVRD FLAG                      
         B     DMX1                                                             
*                                                                               
DMX2     CLI   MYREC,X'FF'         IF NO MORE EXTRACTED RECS                    
         BE    DMX30               KEEP EVERYTHING                              
         CLC   BUYKEY(12),MYNETKEY                                              
         BL    DMX30                                                            
         BE    DMX10                                                            
DMXGET   MVC   SVMYREC,MYREC       SAVE PREVIOUS TO PRINT                       
         GET   FILEIN,MYREC                                                     
         B     DMX2                                                             
*                                                                               
ENDIN    MVI   MYREC,X'FF'         EOF ON INPUT FILE                            
         MVC   MYREC+1(11),MYREC                                                
         B     DMXKEEP                                                          
*                                                                               
DMX10    MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
DMX12    BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NTWKELEM,R6                                                      
         CLC   NTWKMKST,MYBUYMKT                                                
         BNE   DMX12                                                            
*                                                                               
         MVI   CHGFLAG,C'Y'        SET NETWORK BUY CHANGED                      
         MVC   ELEM(11),0(R6)      THIS BUY HAS A CUTIN                         
         MVI   ELEM+1,12           SET NEW LEN                                  
         MVI   ELEM+11,NTWKFLG_CUTIN                                            
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',(R8)),(R6)                                      
         GOTO1 RECUP,DMCB,(C'S',(R8)),ELEM,(R6)                                 
*                                                                               
DMX20    B     DMXGET                                                           
         DROP  R6                                                               
*                                                                               
DMX30    CLI   CHGFLAG,C'Y'        NETWORK BUY CHANGED ?                        
         BNE   DMXKEEP                                                          
         AP    CHGRECS,=P'1'                                                    
         BRAS  RE,PRTIT            PRINT CHANGE INFO                            
         B     DMXKEEP                                                          
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
PRTIT    NTR1                                                                   
         MVC   P(2),SVMYAGY                                                     
         MVI   P+3,C'N'                                                         
         MVC   P+5(3),SVMYCLT                                                   
         MVC   P+9(4),SVMYSTA                                                   
         MVI   P+13,C'/'                                                        
         MVC   P+14(4),SVMYNET                                                  
         MVC   P+20(7),SVMYEST                                                  
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* PRINT RECORD COUNTS AT EOJ                                                    
*=============================================================                  
         SPACE 1                                                                
DMXCNTS  NTR1                                                                   
         ZAP   LINE,=P'99'                                                      
         LA    R4,COUNTS                                                        
         LHI   R5,(COUNTX-COUNTS)/L'COUNTS                                      
*                                                                               
DMXCNT2  MVC   P(20),4(R4)                                                      
         OI    3(R4),X'0F'                                                      
         UNPK  P+25(8),0(4,R4)                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         AHI   R4,L'COUNTS                                                      
         BCT   R5,DMXCNT2                                                       
         B     EXIT                                                             
*                                                                               
         DS    0D                                                               
COUNTS   DS    0XL24                                                            
CHGRECS  DC    PL4'0',CL20'BUY RECORDS CHANGED'                                 
COUNTX   EQU   *                                                                
                                                                                
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
CHGFLAG  DS    C                                                                
RECUP    DS    A                                                                
         LTORG                                                                  
         DS    0D                                                               
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,                        X        
               MACRF=GM,LRECL=48,EODAD=ENDIN                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*MYREC*'                                                     
MYREC    DS    CL48                                                             
         ORG   MYREC                                                            
MYNETKEY DS    XL12                                                             
MYBUYKEY DS    XL12                                                             
*                                                                               
MYBUYMKT EQU   MYBUYKEY+4                                                       
MYBUYSTA EQU   MYBUYKEY+6                                                       
*                                                                               
MYAGY    DS    CL2                                                              
MYCLT    DS    CL3                                                              
MYSTA    DS    CL4                                                              
MYNET    DS    CL4                                                              
MYEST    DS    CL3                                                              
         DS    CL1                                                              
MYLIN    DS    CL3                                                              
         ORG                                                                    
         DC    CL8'SVMYREC'                                                     
SVMYREC  DS    CL48                                                             
         ORG   SVMYREC                                                          
SVMYNKEY DS    XL12                                                             
SVMYBKEY DS    XL12                                                             
*                                                                               
SVMYAGY  DS    CL2                                                              
SVMYCLT  DS    CL3                                                              
SVMYSTA  DS    CL4                                                              
SVMYNET  DS    CL4                                                              
SVMYEST  DS    CL3                                                              
         DS    CL1                                                              
SVMYLIN  DS    CL3                                                              
         ORG                                                                    
         EJECT                                                                  
DUB      DS    D                                                                
ELEM     DS    XL256                                                            
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
*                                                                               
         DS    0D                                                               
APARM    DS    F                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
ENDOFMOD EQU   *                                                                
*                                                                               
       ++INCLUDE DDTSARD                                                        
                                                                                
       ++INCLUDE DMLDDEFN                                                       
         PRINT OFF                                                              
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPLDEXTCUT04/05/06'                                      
         END                                                                    
