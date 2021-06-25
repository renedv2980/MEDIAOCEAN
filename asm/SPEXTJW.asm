*          DATA SET SPEXTJW    AT LEVEL 007 AS OF 09/26/94                      
*PHASE SPEXTJW                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* DEC 21, 1985 CHANGE ESTIMATES FOR J. WALTER THOMPSON                          
*                                                                               
RAGYMED  EQU   X'11'                                                            
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
         SPACE                                                                  
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE                                                                  
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE                                                                  
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         SPACE                                                                  
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLC   =X'B2E13FFF02435E4640CE',0(R3)                                   
         BNE   DMXKEEP                                                          
* PRINT THE RECORD                                                              
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)         GET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,0,(R3),C'DUMP',(R0),=C'1D'                       
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOTAL RECS READ'                                     
TBUYCT   DC    PL5'0',CL28'TOT BUYS JW/BK'                                      
BUYCT    DC    PL5'0',CL28'TOT BUYS CHANGED EST'                                
TBILLCT  DC    PL5'0',CL28'TOT STA BILLS JW/BK'                                 
BILLCT   DC    PL5'0',CL28'TOT STA BILLS CHANGED EST'                           
TESTCT   DC    PL5'0',CL28'TOT ESTS JW/BK'                                      
ESTCT    DC    PL5'0',CL28'TOT ESTS CHANGED EST'                                
TGOLCT   DC    PL5'0',CL28'TOT GOAL JW/BK'                                      
GOALCT   DC    PL5'0',CL28'TOT GOAL CHANGED EST'                                
TBILCT   DC    PL5'0',CL28'TOT BILLS JW/BK'                                     
BILCT    DC    PL5'0',CL28'TOT BILLS CHANGED EST'                               
TOTCTRS  EQU   (*-TOTRD)/33                                                     
WORK     DS    CL64                                                             
RTITLE   DC    CL100'RENUMBER ESTIMATES FOR JW CLT BK'                          
CLTABLE  DC    CL3'BK '                                                         
CLTPA    DC    XL2'855F'                                                        
CLTABEND EQU   *                                                                
         DC    X'FF'                                                            
TABADDRS DC    D'0'                                                             
DUPTABLE DC    XL256'00'                                                        
*                                                                               
* ESTIMATE TABLE - OLD TO NEW *                                                 
*                                                                               
ESTABLE  DC    AL1(100,98)                                                      
         DC    AL1(101,122)                                                     
         DC    AL1(102,146)                                                     
         DC    AL1(103,170)                                                     
         DC    AL1(104,100)                                                     
         DC    AL1(105,124)                                                     
         DC    AL1(106,148)                                                     
         DC    AL1(107,172)                                                     
         DC    AL1(108,102)                                                     
         DC    AL1(109,126)                                                     
         DC    AL1(110,150)                                                     
         DC    AL1(111,174)                                                     
         DC    AL1(112,104)                                                     
         DC    AL1(113,128)                                                     
         DC    AL1(114,152)                                                     
         DC    AL1(115,176)                                                     
         DC    AL1(116,106)                                                     
         DC    AL1(117,130)                                                     
         DC    AL1(118,154)                                                     
         DC    AL1(119,178)                                                     
         DC    AL1(120,108)                                                     
         DC    AL1(121,132)                                                     
         DC    AL1(122,156)                                                     
         DC    AL1(123,180)                                                     
         DC    AL1(124,110)                                                     
         DC    AL1(125,134)                                                     
         DC    AL1(126,158)                                                     
         DC    AL1(127,182)                                                     
         DC    AL1(128,112)                                                     
         DC    AL1(129,136)                                                     
         DC    AL1(130,160)                                                     
         DC    AL1(131,184)                                                     
         DC    AL1(132,114)                                                     
         DC    AL1(133,138)                                                     
         DC    AL1(134,162)                                                     
         DC    AL1(135,186)                                                     
         DC    AL1(136,116)                                                     
         DC    AL1(137,140)                                                     
         DC    AL1(138,164)                                                     
         DC    AL1(139,188)                                                     
         DC    AL1(140,118)                                                     
         DC    AL1(141,142)                                                     
         DC    AL1(142,166)                                                     
         DC    AL1(143,190)                                                     
         DC    AL1(144,120)                                                     
         DC    AL1(145,144)                                                     
         DC    AL1(146,168)                                                     
         DC    AL1(147,192)                                                     
         DC    AL1(148,194)                                                     
         DC    AL1(150,195)                                                     
         DC    AL1(152,196)                                                     
         DC    AL1(154,197)                                                     
         DC    AL1(155,198)                                                     
         DC    AL1(156,199)                                                     
         DC    AL1(157,200)                                                     
         DC    AL1(158,201)                                                     
         DC    AL1(159,202)                                                     
         DC    AL1(160,203)                                                     
         DC    AL1(161,204)                                                     
         DC    AL1(162,205)                                                     
ESTABCT  EQU   (*-ESTABLE)/2                                                    
         DC    AL1(255,255)                                                     
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
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPEXTJW   09/26/94'                                      
         END                                                                    
