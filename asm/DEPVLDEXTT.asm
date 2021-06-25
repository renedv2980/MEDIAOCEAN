*          DATA SET DEPVLDEXTT AT LEVEL 118 AS OF 05/01/02                      
*PHASE DEPVLETA PVLDEXTT                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'CREATING JCN KEYS USING QCN RECORDS'                            
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
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
PVXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    PVINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PVXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BNE   PVXIT                                                            
         CLOSE FILOUT                                                           
         B     PVXEOF              END-OF-FILE                                  
*                                                                               
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
*                                                                               
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
*                                                                               
         MVC   WORK,SPACES         KEEP RECORD                                  
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         BAS   RE,DUMPREC                                                       
*                                                                               
         B     PVXIT                                                            
*                                                                               
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         OPEN  (FILOUT,(OUTPUT))                                                
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),C'Q'          LOOKING FOR Q RECORDS                        
         BNE   PVXKEEP                                                          
*                                                                               
         USING PMKEY,R3                                                         
QREC     CLI   PMCODE,C'Q'         Q-RECS                                       
         BNE   PVXKEEP                                                          
         CLI   PMMEDIA,C'N'        N                                            
         BNE   PVXKEEP                                                          
         CLI   PMSRC,C'N'          N                                            
         BNE   PVXKEEP                                                          
         SPACE                                                                  
*                                                                               
         CLC   0(12,R3),SVKEY      SAME SET?                                    
         BE    PVXKEEP                                                          
*                                                                               
QREC05   MVC   SVKEY,0(R3)         SAVE THE KEY FOR NEXT TIME                   
         L     R2,=A(OREC)         RETRIEVE THE INFO FOR OUTPUT                 
         USING PLINE,R2                                                         
         MVC   PSTA,PMSTAT         STATION                                      
         MVC   PBTY,PMBTYP         BOOK TYPE                                    
         MVC   PBOK,PMBOOK         BOOK                                         
         LA    R4,PMDATA                                                        
         USING MARELEM,R4          X'01' ELEM                                   
         MVC   PCDT,MARDATE        CREATION DATE                                
*                                                                               
         LA    R4,PMDATA                                                        
         USING PHTELEM,R4                                                       
QREC10   CLI   0(R4),X'10'                                                      
         BE    QREC20                                                           
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               POINT TO THE NEXT ELEMENT                    
         B     QREC10                                                           
*                                                                               
QREC20   MVC   PDDS,PHTDDS                                                      
         MVC   PNTI,PHTNTI                                                      
         PUT   FILOUT,(R2)                                                      
         GOTO1 =V(HEXOUT),DMCB,0(R2),P,25                                       
         GOTO1 VPRINTER                                                         
         B     PVXKEEP                                                          
*                                                                               
PVXEOF   MVC   P(21),=CL21'***RECORDS SAVED***'                                 
         GOTO1 VPRINTER                                                         
         CLOSE FILOUT                                                           
         LA    R2,COUNTS                                                        
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)                                                       
         EDIT  (R9),(8,P+10)                                                    
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)                                                     
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)                                                     
*        GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
*        MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
*        LH    R5,HALF                                                          
*        CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
*        BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D'              
         XIT1                                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,LRECL=2000,BLKSIZE=8200,*        
               MACRF=PM                                                         
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
KEEPS    DC    F'0'                                                             
SVKEY    DS    CL12                                                             
         DC    H'0'                                                             
NEXT     DS    XL1                 FLICK FOR NEXT SET OF QCN                    
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
OREC     DC    H'27'                                                            
         DC    H'0'                                                             
OUTREC   DC    2000X'00'                                                        
         EJECT                                                                  
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
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
PSTA     DS    CL5                                                              
         DS    CL1                                                              
PBTY     DS    CL1                                                              
         DS    CL1                                                              
PBOK     DS    CL2                                                              
         DS    CL1                                                              
PCDT     DS    CL2                                                              
         DS    CL1                                                              
PDDS     DS    CL2                                                              
         DS    CL1                                                              
PNTI     DS    CL5                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118DEPVLDEXTT05/01/02'                                      
         END                                                                    
