*          DATA SET STEXTCPY   AT LEVEL 017 AS OF 10/24/00                      
*PHASE STEXTCPY                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SEP 20, 2000                                                                  
* DONE-COPY CML & S TEXT RECS NET3 SPOTA OMNY OM/06 TO SPOTC MSNY H7/02         
* COPY CML & SPEC TEXT RECS FROM SPOTE OMNY OM/04 TO SPOTF MSNY H7/03           
*                                                                               
* FOR CLTS                                                                      
* ABY/AIG/AP/ARC/BPA/EKN/GTJ/HSS/HU/IM0/KK/KRG/NPL/OND/OT/PEG/PLT/SAP           
* SAR/VZ/WMD/XBJ/YP                                                             
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
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
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
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R0,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMXI010  GOTO1 =V(CLPACK),DMCB,(R5),3(R5)                                       
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,L'CLTAB(,R5)                                                  
         BCT   R0,DMXI010                                                       
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R3,AREC                                                          
         AP    TOTRD,=P'1'                                                      
         CLC   =X'0A21',0(R3)     ONLY COMMLS                                   
         BE    DMX010                                                           
         CLC   =X'0A2D',0(R3)     & SPECIAL TEXT                                
         BNE   DMXPURGE                                                         
         SPACE                                                                  
DMX010   DS    0H                                                               
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'40'          THIS OMNY                                    
         BNE   DMXPURGE                                                         
         SPACE                                                                  
         LA    R0,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMX012   DS    0H                                                               
         CLC   3(2,R5),3(R3)       THIS CLIENT NEEDED                           
         BE    DMX014                                                           
         LA    R5,L'CLTAB(,R5)                                                  
         BCT   R0,DMX012                                                        
         B     DMXPURGE                                                         
DMX014   DS    0H                                                               
         CLC   =X'0A2D',0(R3)     & SPECIAL TEXT                                
         BNE   DMX016                                                           
         AP    TOTXT,=P'1'                                                      
         B     DMX040                                                           
DMX016   DS    0H                                                               
         CLC   =X'0A21',0(R3)     ONLY COMMLS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CMLKCML-CMLKEY(,R3),CMLKCML-CMLKEY(R3)                           
         BNZ   DMX019                                                           
         AP    TOTSEQ,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'44'                                                      
         BE    DMX018                                                           
         CLI   2(R3),X'42'                                                      
         BE    DMX017                                                           
         CLI   2(R3),X'41'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         OC    8(3,R5),8(R5)       SHOULD BE NO ENTRY                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   8(3,R5),CMLSEQ-CMLRECD(R3)                                       
         B     DMX040                                                           
DMX017   DS    0H                                                               
         OC    11(3,R5),11(R5)     SHOULD BE NO ENTRY                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   11(3,R5),CMLSEQ-CMLRECD(R3)                                      
         B     DMX040                                                           
DMX018   DS    0H                                                               
         OC    14(3,R5),14(R5)     SHOULD BE NO ENTRY                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   14(3,R5),CMLSEQ-CMLRECD(R3)                                      
         B     DMX040                                                           
DMX019   DS    0H                                                               
         CLC   CMLKCML-CMLKEY(,R3),=C'99999999'                                 
         BNE   DMX020                                                           
         AP    TOTPRD,=P'1'                                                     
         B     DMX040                                                           
         SPACE                                                                  
DMX020   DS    0H                                                               
         AP    5(3,R5),=P'1'                                                    
         AP    TOTCML,=P'1'                                                     
         LA    R6,24(,R3)          1ST ELEM                                     
DMX030   DS    0H                                                               
         CLI   0(R6),X'20'         PROD ELEM                                    
         BE    DMX034                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         LTR   R0,R0                                                            
         BNZ   DMX030                                                           
         DC    H'0'                                                             
DMX034   DS    0H                                                               
         CLI   1(R6),3                                                          
         BNE   DMX036                                                           
         MVI   2(R6),X'FF'                                                      
         B     DMX040                                                           
         SPACE                                                                  
DMX036   DS    0H                                                               
         LA    R4,=CL20'MULT PRD COMML-OMNY'                                    
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         ZIC   R0,1(R6)            GET LENGTH                                   
         AHI   R0,-3               GET DIFF                                     
         SPACE                                                                  
         MVI   1(R6),3             CHANGE LENGTH                                
         MVI   2(R6),X'FF'                                                      
         LA    RF,3(,R6)                                                        
         AR    RF,R0                                                            
         MVC   3(256,R6),0(RF)                                                  
         SR    RE,RE                                                            
         ICM   RE,3,13(R3)                                                      
         SR    RE,R0                                                            
         STCM  RE,3,13(R3)                                                      
         SPACE                                                                  
         LA    R4,=CL20'FIXED MULTI PRD CML'                                    
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
DMX040   DS    0H                                                               
         MVC   BYTE,2(R3)                    GET AGY/MED                        
         NI    BYTE,X'0F'          DROP AGY                                     
         OI    BYTE,X'30'          FIX IT                                       
         MVC   2(1,R3),BYTE        MUST HAVE ELEMENT                            
         SPACE                                                                  
         MVC   20(2,R3),=C'H7'                                                  
         SPACE                                                                  
         NOP   DMXKEEP                                                          
         MVI   *-3,X'F0'                                                        
         LA    R6,=CL20'MOVED REC FOR MIND'                                     
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
DMXEOF   DS    0H                                                               
         LA    R3,TOTRD                                                         
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTSEQ                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTPRD                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTCML                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R3,TOTXT                                                         
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R4,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMXE010  DS    0H                                                               
         MVC   P+5(7),=C'CLIENT='                                               
         MVC   P+12(3),0(R5)                                                    
         EDIT  (P3,5(R5)),(10,P+20),COMMAS=YES                                  
         MVC   P+32(07),=CL07'TV SEQ='                                          
         GOTO1 =V(HEXOUT),DMCB,8(R5),P+39,3                                     
         MVC   P+49(06),=CL06'R SEQ='                                           
         GOTO1 =V(HEXOUT),DMCB,11(R5),P+55,3                                    
         MVC   P+65(06),=CL06'X SEQ='                                           
         GOTO1 =V(HEXOUT),DMCB,11(R5),P+71,3                                    
         GOTO1 VPRINTER                                                         
         LA    R5,L'CLTAB(,R5)                                                  
         BCT   R4,DMXE010                                                       
         B     DMXIT                                                            
         SPACE 2                                                                
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ ='                                   
TOTSEQ   DC    PL4'0',CL28'TOTAL SEQ WRITTEN='                                  
TOTPRD   DC    PL4'0',CL28'TOTAL PRD WRITTEN='                                  
TOTCML   DC    PL4'0',CL28'TOTAL CMMLS WRITTEN='                                
TOTXT    DC    PL4'0',CL28'TOTAL TEXT WRITTEN='                                 
SVAGY    DS    XL3                                                              
FULL     DS    F                                                                
BADAGYCT DC    PL4'0'                                                           
         DS    0D                                                               
WORK     DS    CL64                                                             
* FORMAT IS 3 CLT, 2 PACKED CLT, 3 PACKED COMML CT, 3 BINARY HI SEQ TV          
*           3 BINARY SEQ RADIO 3 BINARY SEQ NET RADIO                           
CLTAB    DS   0CL17                                                             
         DC    CL5'ABY',PL3'0',XL9'00'                                          
         DC    CL5'AIG',PL3'0',XL9'00'                                          
         DC    CL5'AP ',PL3'0',XL9'00'                                          
         DC    CL5'ARC',PL3'0',XL9'00'                                          
         DC    CL5'BPA',PL3'0',XL9'00'                                          
         DC    CL5'EKN',PL3'0',XL9'00'                                          
         DC    CL5'GTJ',PL3'0',XL9'00'                                          
         DC    CL5'HSS',PL3'0',XL9'00'                                          
         DC    CL5'HU ',PL3'0',XL9'00'                                          
         DC    CL5'IM0',PL3'0',XL9'00'                                          
         DC    CL5'KK ',PL3'0',XL9'00'                                          
         DC    CL5'KRG',PL3'0',XL9'00'                                          
         DC    CL5'NPL',PL3'0',XL9'00'                                          
         DC    CL5'OND',PL3'0',XL9'00'                                          
         DC    CL5'OT ',PL3'0',XL9'00'                                          
         DC    CL5'PEG',PL3'0',XL9'00'                                          
         DC    CL5'PLT',PL3'0',XL9'00'                                          
         DC    CL5'SAP',PL3'0',XL9'00'                                          
         DC    CL5'SAR',PL3'0',XL9'00'                                          
         DC    CL5'VZ ',PL3'0',XL9'00'                                          
         DC    CL5'WMD',PL3'0',XL9'00'                                          
         DC    CL5'XBJ',PL3'0',XL9'00'                                          
         DC    CL5'YP ',PL3'0',XL9'00'                                          
CLTABCT  EQU   (*-CLTAB)/L'CLTAB                                                
BYTE     DS    CL1                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017STEXTCPY  10/24/00'                                      
         END                                                                    
