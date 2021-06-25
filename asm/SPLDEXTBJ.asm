*          DATA SET SPLDEXTBJ  AT LEVEL 057 AS OF 05/01/02                      
*PHASE SPEXTBJ,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - FIX CHANGES IN RATING SERVICE MKT'                    
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         MVI   RECCHANG,C'N'                                                    
*                                                                               
         L     R3,AREC            POINT TO RECORD                               
         USING SPGENBUYD,R3                                                     
         CLI   0(R3),X'11'        BUY RECORDS ONLY                              
         BL    DMXKEEP                                                          
         CLI   0(R3),X'FF'                                                      
         BNL   DMXKEEP                                                          
*                                                                               
         CLI   0(R3),X'C1'        FOR BJ TELEVISION ONLY                        
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'BJ',BUYALPHA    FOR AGENCY ALPHA BJ ONLY                      
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =X'AD32',BUYKCLT   FOR LONG JOHN SILVER?                         
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,BDELEM                                                        
DMXREC5  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R6),6                                                          
         BL    DMXREC5                                                          
         CLI   0(R6),13                                                         
         BH    DMXREC5                                                          
         CLC   2(2,R6),=X'B4D9'   CHANGE BUY W/DT = JUN25/90                    
         BNE   DMXKEEP                                                          
*                                                                               
         BAS   RE,SAVIT           SAVE OLD IT INCASE IT CHANGES                 
*                                                                               
         MVI   ELCODE,3           FIND DEMO SPILL ELEMENT                       
         LA    R6,BDELEM                                                        
DMXREC10 BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
DMXREC12 BAS   RE,NEXTEL                                                        
         BNE   DMXREC50                                                         
*                                                                               
         LA    R8,RTGTBL          MKT/RTG TABLE                                 
DMXREC15 CLC   0(2,R8),6(R6)      MATCH ON OLD ARB RATING SERVICE NUM           
         BE    DMXREC20           YES - FIX IT                                  
         LA    R8,L'RTGTBL(R8)    NO - TRY THE NEXT BAD MKT                     
         CLI   0(R8),X'FF'        END OF TABLE?                                 
         BE    DMXREC12           YES - OLD RATING SRV NOT IN ELE               
         B     DMXREC15           TRY FOR MATCH ON NEXT NUM IN TABLE            
*                                                                               
DMXREC20 MVC   6(2,R6),2(R8)      MOVE IN NEW NSI RATING SERVICE NUMBER         
         MVI   RECCHANG,C'Y'                                                    
         B     DMXREC12                                                         
*                                                                               
DMXREC50 CLI   RECCHANG,C'Y'      HAS RECORD CHANGED?                           
         BNE   DMXKEEP                                                          
         AP    TOTCHG,=P'1'       YES - ADD TO RECORD TOTAL                     
         CP    TOTCHG,=P'100'     IF 1ST FEW OF RECS - PRINT IT                 
         BH    DMXKEEP                                                          
         LA    R4,=CL20'BUY RECORD BEFORE'                                      
         SR    R5,R5               PRINT OUT RECORD                             
         LA    R1,SAVEREC                                                       
         ICM   R5,3,13(R1)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),SAVEREC,C'DUMP',(R5),=C'2D'            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,=CL20'BUY RECORD AFTER'                                       
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         SPACE 2                                                                
*                                                                               
* COPIES AREC INTO SAVEREC                                                      
*                                                                               
SAVIT    NTR1                                                                   
         LR    R4,R3              POINT TO RECORD                               
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)        GET LENGTH                                    
         LA    RE,SAVEREC         POINT TO SAVED RECORD AREA                    
         LR    RF,R5              COPY FOR ORIGINAL LENGTH                      
         MVCL  RE,R4                                                            
         XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R2,TOTALS                                                        
         LA    R3,TOTCHG                                                        
DMXEOF10 MVC   P+5(23),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,28(R3)                                                        
         BCT   R2,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
*        OLD ARB, NEW NSI                                                       
RTGTBL   DS    0XL4                                                             
         DC    AL2(347),AL2(159)                                                
         DC    AL2(217),AL2(131)                                                
         DC    AL2(93),AL2(115)                                                 
         DC    AL2(261),AL2(198)                                                
         DC    AL2(121),AL2(135)                                                
         DC    AL2(95),AL2(142)                                                 
         DC    AL2(63),AL2(113)                                                 
         DC    AL2(91),AL2(109)                                                 
         DC    AL2(59),AL2(163)                                                 
         DC    AL2(287),AL2(169)                                                
         DC    AL2(33),AL2(174)                                                 
         DC    AL2(61),AL2(151)                                                 
         DC    AL2(211),AL2(141)                                                
         DC    AL2(209),AL2(129)                                                
         DC    AL2(283),AL2(144)                                                
         DC    AL2(29),AL2(108)                                                 
         DC    AL2(285),AL2(156)                                                
         DC    AL2(55),AL2(147)                                                 
         DC    AL2(103),AL2(154)                                                
         DC    AL2(31),AL2(136)                                                 
         DC    AL2(243),AL2(352)                                                
         DC    AL2(177),AL2(282)                                                
         DC    AL2(325),AL2(270)                                                
         DC    AL2(429),AL2(203)                                                
         DC    AL2(331),AL2(322)                                                
         DC    AL2(301),AL2(252)                                                
         DC    AL2(305),AL2(231)                                                
         DC    AL2(175),AL2(275)                                                
         DC    AL2(227),AL2(317)                                                
         DC    AL2(165),AL2(211)                                                
         DC    AL2(119),AL2(210)                                                
         DC    AL2(53),AL2(188)                                                 
         DC    AL2(77),AL2(248)                                                 
         DC    AL2(87),AL2(181)                                                 
         DC    AL2(313),AL2(205)                                                
         DC    AL2(419),AL2(125)                                                
         DC    AL2(603),AL2(246)                                                
         DC    AL2(195),AL2(336)                                                
         DC    AL2(409),AL2(122)                                                
         DC    AL2(183),AL2(239)                                                
         DC    AL2(219),AL2(103)                                                
         DC    AL2(179),AL2(240)                                                
         DC    AL2(231),AL2(220)                                                
         DC    AL2(441),AL2(262)                                                
         DC    AL2(265),AL2(257)                                                
         DC    AL2(73),AL2(400)                                                 
         DC    AL2(443),AL2(261)                                                
         DC    AL2(321),AL2(212)                                                
         DC    AL2(323),AL2(309)                                                
         DC    AL2(205),AL2(225)                                                
         DC    AL2(405),AL2(227)                                                
         DC    X'FF'                                                            
         SPACE 3                                                                
TOTCHG   DC    PL5'0',CL23'TOTAL 03 ELEMS CHANGED'                              
TOTALS   EQU   (*-TOTCHG)/28                                                    
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
RECCHANG DS    CL1                                                              
WORK     DS    CL64                                                             
         DS    0F                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'**SREC**'                                                    
SAVEREC  DS    2000C                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPLDEXTBJ 05/01/02'                                      
         END                                                                    
