*          DATA SET SPLDEXTAN3 AT LEVEL 155 AS OF 07/28/98                      
*PHASE SPEXTAN3                                                                 
SPEXTCBL TITLE 'DELETE INVALID CABLE STATIONS'                                  
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
*                                                                               
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
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
         MVC   VDATAMGR,LDATAMGR             SET IN PRETEND COMFACS             
*                                                                               
         GOTO1 LLOADER,DMCB,=CL8'T00A7A '    STATPACK   VERSION                 
         ICM   R0,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R0,VSTAPACK                                                      
         MVC   P+15(10),=CL10'GOT STAPACK'                                      
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLI   0(R3),X'10'         TEST BUYREC                                  
         BH    BUY                                                              
         B     DMXKEEP                                                          
         CLI   0(R3),X'0B'         OLD INVOICE RECORD                           
         BE    INV                                                              
         CLI   0(R3),X'0C'         NSID RECORDS                                 
         BE    SID                                                              
         CLC   =X'0D34',0(R3)      DARE ORDER                                   
         BE    DARE                                                             
         CLC   =X'0D7A',0(R3)      WIPW                                         
         BE    PW                                                               
         CLC   =X'0D7B',0(R3)      DOUBLE BOOK RECORDS                          
         BE    DBK                                                              
         CLC   =X'0D67',0(R3)      NEW BUYERS WORKSHEET                         
         BE    NBW                                                              
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*=================================================================              
* BUY RECORDS   R3=A(RECORD)                                                    
*=================================================================              
         SPACE 1                                                                
BUY      DS    0H                                                               
         USING BUYRECD,R3                                                       
         TM    6(R3),X'F0'         TEST CABLE                                   
         BNO   DMXKEEP             NO                                           
*                                                                               
         LA    R2,4(R3)            R2 POINTS TO MKT/STATION                     
         MVC   BAGYMED,0(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*==================================================================             
INV      DS    0H                                                               
         TM    2(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LR    R2,R3                                                            
         MVC   BAGYMED,1(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
SNV      DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
DARE     DS    0H                                                               
         TM    9(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,7(R3)                                                         
         MVC   BAGYMED,4(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
PW       DS    0H                                                               
         TM    9(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,7(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
SID      DS    0H                                                               
         TM    6(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,4(R3)                                                         
         MVC   BAGYMED,1(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
DBK      DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
NBW      DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         USING XMTELD,R6                                                        
         BNE   DMXKEEP                                                          
         TM    10(R6),X'F0'                                                     
         BNO   DMXKEEP                                                          
         LA    R2,12(R6)                                                        
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*==================================================================             
*                                                                               
GETSTA   NTR1                      R2 POINTS TO PACKED MKT/STATION              
*                                                                               
         CLI   BAGYMED,X'C1'       SJ/T                                         
         BNE   GSXYES                                                           
*                                                                               
         GOTO1 LHEXOUT,DMCB,0(R2),P+2,5,=C'TOG'                                 
         MVC   P+15(10),=CL10'B4 STAPACK'                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK                                       
         MVC   STAPAGY,=C'SJ'      ALPHA AGENCY                                 
         MVI   STAPMED,C'T'        TELEVISION                                   
         MVC   STAPACOM,=A(COMFACS)                                             
         MVC   STAPMKST,0(R2)      MKT/STA                                      
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    GSXYES                                                           
*                                                                               
         MVC   P+15(10),=CL10'ERROR UNPK'                                       
         GOTO1 VPRINTER                                                         
         GOTO1 LHEXOUT,DMCB,0(R2),P+2,5,=C'TOG'                                 
         GOTO1 LHEXOUT,DMCB,0(R3),P+15,21,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     GSXNO                                                            
*                                                                               
GSXYES   SR    RC,RC                                                            
GSXNO    LTR   RC,RC                                                            
         XIT1                                                                   
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
NETNUM   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
VSTAPACK DS    A                                                                
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
*                                                                               
COMFACS  DS    0F                  COMFACS FOR STAPACK                          
VDATAMGR DS    A                                                                
         DS    0D                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
WORK     DS    CL64                                                             
DUB      DS    D                                                                
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
POVER    DS    CL4                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL4                                                              
PERR     DS    CL25                                                             
         DS    CL1                                                              
PMYREC   DS    CL46                                                             
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
INVRECD   DSECT                                                                 
       ++INCLUDE SPGENINV                                                       
DARERECD  DSECT                                                                 
       ++INCLUDE SPGENDRORD                                                     
NBUYRECD  DSECT                                                                 
       ++INCLUDE SPNWSHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155SPLDEXTAN307/28/98'                                      
         END                                                                    
