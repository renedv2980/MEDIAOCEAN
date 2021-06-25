*          DATA SET SPLDEXTYK  AT LEVEL 105 AS OF 01/22/10                      
*PHASE SPEXTBIL                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXTBIL - BILL HEADERS: CHANGE LENGTH'                       
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
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
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
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC             LOOKING FOR CLT LHR BILL HEADER              
         USING BILLRECD,R6                                                      
         CLC   =X'0053AE27C7C340016E0101220B',0(R6)          GC                 
         BE    DMX10                                                            
         CLC   =X'0053AE27C8C6C5016E0101220C',0(R6)          HFE                
         BE    DMX10                                                            
         CLC   =X'0053AE27E3C5D4016E0101220D',0(R6)          TEM                
         BNE   DMXKEEP                                                          
*                                                                               
*                                                                               
DMX10    DS    0H                                                               
*&&DO                                                                           
         SR    R3,R3                                                            
         ICM   R3,3,BLEN                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'OLD LENGTH RECORD',(R6),C'DUMP',(R3),+        
               =C'1D',(C'P',LPRINT)                                             
*&&                                                                             
         LHI   R5,225              NEW LENGTH                                   
         STCM  R5,3,BLEN           PUT IN RIGHT LENGTH                          
         MVI   BILSMCOD,C'N'       SUB-MED 'N'                                  
         ZAP   BILSMGRS,=P'0'      INIT TO ZERO                                 
         ZAP   BILSMNET,=P'0'      INIT TO ZERO                                 
*                                                                               
*                                                                               
*&&DO                                                                           
         SR    R3,R3                                                            
         ICM   R3,3,BLEN                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'NEW LENDTH RECORD',(R6),C'DUMP',(R3),+        
               =C'1D',(C'P',LPRINT)                                             
*&&                                                                             
         AP    TOTCHG,=P'1'                                                     
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(34),=C'TOTAL NO. CHANGED BILL HEADERS:   '                     
         EDIT  TOTCHG,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
TOTCHG   DC    PL6'0'              TOTAL NO. RECS CHANGED                       
TRACE    DC    C'N'                'Y' = DO PRNTBL TRACE                        
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
         DS    0F                                                               
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
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         ORG   BRETACCT                                                         
BILSPARE DS    XL3                                                              
BILCTYPN DS    CL1                 COST TYPE (NETPAK- T,I,U,ETC)                
BLPKGNMN DS    CL5                 PKG NAME (NETPAK) UP TO 5 CHARS              
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105SPLDEXTYK 01/22/10'                                      
         END                                                                    
