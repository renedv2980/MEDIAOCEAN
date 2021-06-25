*          DATA SET SPLDEXTCAB AT LEVEL 001 AS OF 04/14/97                      
*PHASE SPEXTHSC                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'DMLDEXTHSC - BUILD TABLE FOR ALL USED CABLE STATIONS'           
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
         L     R2,VLDDEFN                                                       
         USING LDDEFND,R2                                                       
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
         MVC   VDATAMGR,LDATAMGR   SET DMGR ADDRESS IN COMFACS                  
         XC    COUNT,COUNT                                                      
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         USING BUYRECD,R3                                                       
         CLI   0(R3),X'11'         TEST BUYREC                                  
         BL    DMXKEEP                                                          
         CLI   0(R3),X'FE'         TEST BUYREC                                  
         BH    DMXEOF              DONE                                         
*                                                                               
         MVC   TMPAGMED,0(R3)      CHECK IF TV                                  
         NI    TMPAGMED,X'0F'                                                   
         CLI   TMPAGMED,X'01'      IS IT A TV BUY?                              
         BNE   REQFKEEP            NO - GO GET NEXT BUY REC                     
*                                                                               
         CLI   6(R3),X'F0'         IS IT A CABLE BUY?                           
         BNH   REQFKEEP            NO - GO GET NEXT BUY REC                     
*                                                                               
REQF10   DS    0H                  YES - IT IS A CABLE TV BUY!                  
         SR    RE,RE                                                            
         IC    RE,8(R3)            CALCULATE CABLE NUMBER                       
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,CABLTAB(RE)      GET POSITION IN TABLE                        
         L     RF,0(RE)                                                         
         A     RF,=F'1'            INCREMENT TOTAL FOR THAT CABLE NUM           
         ST    RF,0(RE)                                                         
         B     REQFKEEP            GO GET NEXT BUY REC                          
*                                                                               
REQFKEEP B     DMXKEEP             GO GET NEXT BUY RECORD                       
*                                                                               
DMXEOF   DS    0H                  PRINT RESULTS                                
         XC    COUNT,COUNT                                                      
         LA    R5,CABLTAB          TABLE OF CABLE NUMBERS                       
         LA    R6,127                                                           
*                                                                               
DMXE10   L     R4,COUNT            CABLE NUMBER                                 
         A     R4,=F'1'                                                         
         ST    R4,COUNT                                                         
*                                                                               
         EDIT  (4,COUNT),(8,P+2),ZERO=NOBLANK                                   
         EDIT  (4,(R5)),(8,P+12),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,4(R5)            NEXT TABLE ENTRY                             
         BCT   R6,DMXE10                                                        
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         DROP  R3                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DS    0D                                                               
         DS    0A                                                               
STAPARMS DS    XL32                                                             
*                                                                               
VCLUNPK  DS    A                                                                
VSTAPACK DS    A                                                                
VBINSRCH DS    A                                                                
*                                                                               
* COMFACS BELOW IS FOR CALL TO STAPACK                                          
*                                                                               
COMFACS  DS    0F                                                               
VDATAMGR DS    A                                                                
         DS    0D                                                               
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
CABLTAB  DC    127F'0'             TABLE FOR CABLE NUMBERS                      
CABLTABX EQU   *                                                                
TMPAGMED DS    XL1                 USED TO TEST IF TV BUY                       
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCABLNUM DS    CL8                                                              
         DS    CL1                                                              
         DC    CL1'-'                                                           
         DS    CL1                                                              
PCOUNTER DS    CL8                                                              
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPLDEXTCAB04/14/97'                                      
         END                                                                    
